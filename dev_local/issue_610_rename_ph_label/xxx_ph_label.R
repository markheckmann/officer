# change ph labels in a layout
# layout_rename_ph_labels(x, layout, master, 1 = "Hello")
# ... key value pairs. Key can either be an id = new label,
#
# layout_rename_ph_labels(x, layout, "this label" = 1)
layout_rename_ph_labels <- function(x, layout, master = NULL, ...) {
  dots <- list(...)
  l <- get_layout(x, layout, master)
  lp <- layout_properties(x, l$layout_name, l$master_name)
  if (length(dots) == 0) {
    return(lp$ph_label)
  }
  # CAVEAT: ph order in layout_properties() (i.e. get_xfrm_data()) is reference for the user.
  # Using the 'slide_layout' object's xfrm() method does not yield the same ph order!
  # We need to guarantee a proper match by ph id here.
  df_renames <- .rename_df_from_dots(lp, dots)
  .set_ph_labels(l, df_renames)
  reload_slidelayouts(x)

  lp <- layout_properties(x, l$layout_name, l$master_name)
  invisible(lp$ph_label)
}


`layout_rename_ph_labels<-` <- function(x, layout, master = NULL, id = NULL, value) {
  l <- get_layout(x, layout, master)
  lp <- layout_properties(x, l$layout_name, l$master_name)
  # if (!is.null(i)) {
  #
  #   ii_allowed <- seq_len(nrow(lp))
  #   out_of_bound <- !i %in% ii_allowed
  #   if (any(out_of_bound)) {
  #     ii <- i[out_of_bound]
  #     cli::cli_abort(c(
  #       "Index {.arg i} out of bounds for {.val ii}",
  #       "x" = "Choose a value in the range [{min(ii_allowed)}; {max(ii_allowed)}]",
  #       "i" = cli::col_grey("See rowname indexes in {.code layout_properties(x, '{layout}', '{master}')}")
  #     ))
  #   }
  #   # if (length(i) != length(value)) {
  #   #   cli::cli_warn(
  #   #     c("Length of indexes {.arg i} and length of values differ.",
  #   #       "x"= "This may cause unwanted effect."))
  #   # }
  #   lp$ph_label[i] <- value
  #   value <- lp$ph_label
  # }
  if (!is.null(id)) {
    if (length(id) != length(value)) {
      cli::cli_abort(
        c("Number of ids ({.val {length(id)}}) and values ({.val {length(value)}}) differ",
          "x" = "{.arg id} and rhs values must have the same length"
        )
      )
    }
    wrong_ids <- setdiff(id, lp$id)
    if (length(wrong_ids) > 0) {
      cli::cli_abort(c(
        "These {.arg id} do not exist: {.val {wrong_ids}}",
        "x" = "Choose one of: {.val ph_id}",
        "i" = cli::col_grey("Also see {.code plot_layout_properties(..., '{layout}', '{master}')} ")
      ))
    }
    .idx <- match(id, lp$id) # user may enter ids in arbitrary order
    lp$ph_label[.idx] <- value
    value <- lp$ph_label
  }
  names(value) <- lp$id
  df_renames <- .rename_df_from_dots(lp, value)
  .set_ph_labels(l, df_renames)
  reload_slidelayouts(x)
}



# heuristic: if one number, then treat as ph_id
.detect_ph_id <- function(x) {
  suppressWarnings({ # avoid character to NA warning
    nchar(x) == 1 & !is.na(as.numeric(x))
  })
}


# create data frame with: ph_id, ph_label, ph_label_new
# as a basis for subsequent renaming actions
.rename_df_from_dots <- function(lp, dots) {
  lp <- lp[, c("id", "ph_label")]

  label_old <- names(dots)
  label_new <- as.character(dots)
  is_id <- .detect_ph_id(label_old)
  is_label <- !is_id

  # match by label or id
  row_idx_label <- match(label_old[is_label], table = lp$ph_label)
  row_idx_id <- match(label_old[is_id], table = lp$id)

  lp$ph_label_new <- NA
  lp$ph_label_new[row_idx_label] <- label_new[is_label]
  lp$ph_label_new[row_idx_id] <- label_new[is_id]
  lp[!is.na(lp$ph_label_new), , drop = FALSE]
}


.set_ph_labels <- function(l, df_renames) {
  if (!inherits(l, "layout_info")) {
    cli::cli_abort(
      c("{.arg l} must a a {.cls layout_info} object",
        "x" = "Got {.cls {class(l)[1]}} instead"
      )
    )
  }
  layout_xml <- l$slide_layout$get()
  for (i in seq_len(nrow(df_renames))) {
    cnvpr_node <- xml2::xml_find_first(layout_xml, sprintf("p:cSld/p:spTree/*/p:nvSpPr/p:cNvPr[@id='%s']", df_renames$id[i]))
    xml2::xml_set_attr(cnvpr_node, "name", df_renames$ph_label_new[i])
  }
  l$slide_layout$save() # persist changes in slideout xml file
}




# find similar name
find_similar_names <- function(name, names, n = 3) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return(NULL)
  }
  l <- stringdist::afind(names, name) # search for similar names
  d <- l$distance |> as.vector()
  ii <- order(d)
  ii <- utils::head(ii, n = n)
  # paste(ii, "=", names[ii])
  names[ii]
}



x <- read_pptx()
plot_layout_properties(x, "Comparison")

layout_rename_ph_labels(x, "Comparison")[1:3] <- paste("CHANGED", 1:3)
plot_layout_properties(x, "Comparison")

layout_rename_ph_labels(x, "Comparison", id = c(2:4)) <- paste("ID", 2:4)
plot_layout_properties(x, "Comparison")


# EXAMPLES ------

x <- read_pptx()

# Returns ph_labels by default. Same order as in layout_properties().
layout_rename_ph_labels(x, "Comparison")
layout_properties(x, "Comparison")[, c("id", "ph_label")]

# Hint: run `plot_layout_properties(x, "Comparison")` now and then to see what has changed.

# rename using key-value pairs: 'old label' = 'new label' or 'id' = 'new label'
layout_rename_ph_labels(x, "Comparison", "Title 1" = "LABEL MATCHED") # label matching
layout_rename_ph_labels(x, "Comparison", "3" = "ID MATCHED") # id matching
plot_layout_properties(x, "Comparison")

# rename via assignment style
layout_rename_ph_labels(x, "Comparison") <- LETTERS[1:8]
layout_rename_ph_labels(x, "Comparison")[1:3] <- paste("CHANGED", 1:3)

layout_rename_ph_labels(x, "Comparison", i = 1:4) <- paste("AGAIN", 1:3)

labels <- layout_rename_ph_labels(x, "Comparison")
layout_rename_ph_labels(x, "Comparison") <- tolower(labels)

layout_rename_ph_labels(x, "Comparison")[-1] <- 1
plot_layout_properties(x, "Comparison")
