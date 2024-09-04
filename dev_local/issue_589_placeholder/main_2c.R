devtools::load_all()

# dir_layout$set("public", "reload", \(package_dir, master_metadata, master_xfrm) {
#   super$initialize(package_dir, slide_layout$new("ppt/slideLayouts"))
#   private$master_metadata <- master_metadata
#   private$xfrm_data <- xfrmize(self$xfrm(), master_xfrm)
# })

# reload_slidelayouts <- function(x) {
#   # x$slideLayouts$reload(x$package_dir,
#   #   master_metadata = x$masterLayouts$get_metadata(),
#   #   master_xfrm = x$masterLayouts$xfrm()
#   # )
#   x$slideLayouts$initialize(x$package_dir,
#                         master_metadata = x$masterLayouts$get_metadata(),
#                         master_xfrm = x$masterLayouts$xfrm()
#   )
# }


# reload slideLayouts (if layout XML in package_dir has changed)
reload_slidelayouts <- function(x) {
  x$slideLayouts$initialize(x$package_dir,
    master_metadata = x$masterLayouts$get_metadata(),
    master_xfrm = x$masterLayouts$xfrm()
  )
  x
}


# Create unique string by appending a sepatator and a number
# make_strings_unique(c("A", "B", "B", "C", "A"))
make_strings_unique <- function(x, sep = ".") {
  ii <- ave(x, x, FUN = seq_along)
  paste0(x, sep, ii)
}


# handle placeholder labels in ONE layout
#
# layout_file: layout filename (e.g. "slideLayout1.xml").
# x: An `rpptx` object
#
# returns: Dataframe with placeholder info. Only needed for .print_dedupe_info()
.dedupe_phs_in_layout <- function(layout_file, x, action = "rename") {
  if (!grepl("\\.xml$", layout_file, ignore.case = TRUE)) {
    stop("'layout_file' must be an .xml file", call. = FALSE)
  }
  action <- match.arg(action, c("detect", "rename", "delete"))
  layout <- x$slideLayouts$collection_get(layout_file)
  xfrm <- layout$xfrm()
  xfrm <- subset(xfrm, duplicated(ph_label) | duplicated(ph_label, fromLast = TRUE))
  if (nrow(xfrm) == 0) {
    return()
  }
  xfrm <- transform(xfrm, ph_label_new = make_strings_unique(ph_label), delete_flag = duplicated(ph_label)) # prepare once for all action types
  if (action == "detect") {
    return(xfrm) # no further action required
  } else if (action == "rename") {
    xfrm$delete_flag <- FALSE
  } else if (action == "delete") {
    xfrm$ph_label_new <- xfrm$ph_label
  }

  # rename label or delete ph shape
  layout_xml <- layout$get()
  for (i in 1L:nrow(xfrm)) {
    shape <- xml2::xml_find_first(layout_xml, sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]", xfrm$id[i]))
    if (xfrm$delete_flag[i]) {
      xml2::xml_remove(shape)
    } else {
      xml2::xml_find_first(shape, ".//p:cNvPr") |> xml2::xml_set_attr("name", xfrm$ph_label_new[i])
    }
  }
  layout$save() # persist changes in slideout xml file
  xfrm
}


# print info on what was done (if print_info = TRUE)
.print_dedupe_info <- function(xfrm_list, action) {
  .df_1 <- do.call(rbind, xfrm_list)
  .df_2 <- x$slideLayouts$get_xfrm_data()
  .df_2 <- .df_2[, c("master_file", "master_name"), drop = FALSE] |> unique()
  df <- merge(.df_1, .df_2, sort = FALSE)
  rownames(df) <- NULL
  df <- subset(df, select = c(master_name, name, ph_label, ph_label_new, delete_flag))
  colnames(df)[2] <- "layout_name"
  if (action == "detect") {
    cat("Placeholders with duplicated labels:\n")
    cat("\033[90m * 'ph_label_new' = new placeholder label if action = 'rename' \033[39m")
    cat("\n\033[90m * 'delete_flag' = delete placeholder if action = 'delete'? \033[39m\n")
  } else if (action == "rename") {
    df$delete_flag <- NULL
    cat("Renamed duplicated placeholder labels:\n")
    cat("\033[90m * 'ph_label_new' = new placeholder label \033[39m\n")
  } else if (action == "delete") {
    df <- subset(df, delete_flag)
    df$ph_label_new <- NULL
    cat("Removed placeholders with duplicated labels:\n")
  }
  print(df)
}


#' Detect and handle duplicated placeholder labels
#'
#' It may happen that a placeholder label is used more than once in a layout.
#' This may cause errors when selecting a placecholder. It is recommended to fix
#' placeholder labels. `layout_dedupe_ph` helps to detect, rename, or delete duplicated placholder labels.
#'
#' @param x  description
#' @param action Action to perform on duplicated placeholder labels. `detect` (default) = show dupes,
#'  `rename` = create unique labels, `delete` = only keep one of the placeholders with a duplicated label.
#' @param print_info Print information on action (e.g. which placholders were changed or deleted) to console?
#'  (default is `FALSE`). Info with `action = "detect"` is always printed.
#' @return A `rpptx` object (with adjusted placeholders).
#' @export
layout_dedupe_ph <- function(x, action = "detect", print_info = FALSE) {
  if (!inherits(x, "rpptx")) {
    stop("'x' must be an 'rpptx' object", call. = FALSE)
  }
  action <- match.arg(action, c("detect", "rename", "delete"))
  layout_names <- x$slideLayouts$get_metadata()$filename
  xfrm_list <- lapply(layout_names, .dedupe_phs_in_layout, x = x, action = action)
  x <- reload_slidelayouts(x) # reinit slideLayouts to get processed ph labels [e.g. when calling x$slideLayouts$get_xfrm_data()]
  if (print_info | action == "detect") {
    .print_dedupe_info(xfrm_list, action)
  }
  invisible(x)
}


# Tests
# import attached presentation via officer
file_in <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
x <- read_pptx(file_in)
before <- x$slideLayouts$get_xfrm_data()$ph_label
layout_dedupe_ph(x, action = "detect", print_info = T)

# import attached presentation via officer
file_in <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
file_out <- "dev_local/issue_589_placeholder/Prasentation1_repair.pptx"
x <- read_pptx(file_in)
before <- x$slideLayouts$get_xfrm_data()$ph_label
layout_dedupe_ph(x, action = "rename", print_info = T)
after <- x$slideLayouts$get_xfrm_data()$ph_label
assertthat::assert_that(any(before != after))
print(x, target = file_out)
x <- read_pptx(file_out)
x$slideLayouts$get_xfrm_data()$ph_label
annotate_base(file_out)


file_in <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
file_out <- "dev_local/issue_589_placeholder/Prasentation1_delete.pptx"
x <- read_pptx(file_in)
before <- x$slideLayouts$get_xfrm_data()$ph_label
layout_dedupe_ph(x, action = "delete", print_info = T)
after <- x$slideLayouts$get_xfrm_data()$ph_label
assertthat::assert_that(length(before) > length(after))
print(x, target = file_out)
x <- read_pptx(file_out)
x$slideLayouts$get_xfrm_data()$ph_label
annotate_base(file_out)


## helpers ---

.count_dupe_occurences <- function(x) {
  counts <- table(x)
  count_dupes <- counts[counts > 1]
  if (length(count_dupes) == 0) {
    return(NA_character_)
  }
  paste0(names(count_dupes), " [", count_dupes, "]", collapse = ", ")
}


ph_dupes <- \(x, all_layouts = TRUE) {
  xfrm <- x$slideLayouts$get_xfrm_data()
  dupes <- aggregate(ph_label ~ master_name + name, data = xfrm, FUN = .count_dupe_occurences)
  names(dupes)[c(2,3)] <- c("layout_name", "ph_label_dupes")
  dupes$has_dupes <- !is.na(dupes$ph_label_dupes)
  dupes <- subset(dupes, select = c(master_name, layout_name, has_dupes, ph_label_dupes))
  if (all_layouts) {
    return(dupes)
  }
  subset(dupes, has_dupes)
}


has_ph_dupes <- \(x) {
  nrow(ph_dupes(x, all_layouts = FALSE)) > 0
}
