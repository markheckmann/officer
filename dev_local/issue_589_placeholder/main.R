devtools::load_all()

# make_labels_unique(c("A", "B", "B", "C", "A"))
make_labels_unique <- function(x, sep = ".") {
  ii <- ave(x, x, FUN = seq_along) |> as.numeric()
  paste0(x, sep, ii)
}


# keep dupes and create new labels for dupes
prepare_xrfm <- function(x, action = "repair") {
  action <- match.arg(action, c("repair", "delete"))
  ii <- duplicated(x$ph_label) | duplicated(x$ph_label, fromLast = TRUE)
  x <- x[ii, , drop = FALSE]
  if (nrow(x) == 0) {
    return(x)
  }
  if (action == "repair") {
    x <- transform(x, ph_label_new = make_labels_unique(ph_label), delete_flag = FALSE)
  } else {
    x <- transform(x, ph_label_new = ph_label, delete_flag = duplicated(ph_label))
  }
  x
}


ph_handle_labels <- function(x, xfrm) {
  if (nrow(xfrm) == 0) {
    return(NULL)
  }
  layout_file <- unique(xfrm$file)
  layout <- x$slideLayouts$collection_get(layout_file)
  layout_xml <- layout$get()
  for (i in 1L:nrow(xfrm)) {
    shape <- xml2::xml_find_first(layout_xml, sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]", xfrm$id[i]))
    if (xfrm$delete_flag[i]) {
      xml2::xml_remove(shape)
    } else {
      xml2::xml_find_first(shape, ".//p:cNvPr")  |> xml2::xml_set_attr("name", xfrm$ph_label_new[i])
    }
  }
  layout$save()
  NULL
}


.info_ph_fortify_layout_labels <- function(xfrm_list, action) {
  df <- do.call(rbind, xfrm_list)[, c("ph_label", "ph_label_new", "name", "master_name", "delete_flag"), drop = FALSE]
  rownames(df) <- NULL
  df <- subset(df, delete_flag)
  colnames(df)[3] <- "layout_name"
  if (action == "repair") {
    df$delete_flag <- NULL
    cat("Found duplicate pg labels. Replacing labels as follows:\n")
    print(df)
  } else {
    df$ph_label_new <- NULL
    cat("Found duplicate ph labels. Deleting following dupes:\n")
    print(df)
  }
}


ph_fortify_layout_labels <- function(x, action = "repair", verbose = FALSE) {
  xfrm <- x$slideLayouts$get_xfrm_data()
  xfrm_list <- split(xfrm, xfrm$file)
  xfrm_list <- xfrm_list |> lapply(prepare_xrfm, action = action)
  xfrm_list <- xfrm_list[sapply(xfrm_list, nrow) > 0]
  if (length(xfrm_list) == 0) {
    return(x)
  }
  .l <- lapply(xfrm_list, \(xfrm) ph_handle_labels(x, xfrm))
  if (verbose) {
    .info_ph_fortify_layout_labels(xfrm_list, action)
  }
  x
}


library(xml2)
pptx_corrupted <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
pptx_repaired <- "dev_local/issue_589_placeholder/tmp.pptx"
x <- read_pptx(pptx_corrupted)
x <- ph_fortify_layout_labels(x, action = "repair", verbose = T)
x$slideLayouts$get_xfrm_data()$ph_label
print(x, target = pptx_repaired)
x_repaired <- read_pptx(pptx_repaired)
x_repaired$slideLayouts$get_xfrm_data()$ph_label
annotate_base(pptx_repaired)

library(xml2)
pptx_corrupted <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
pptx_repaired <- "dev_local/issue_589_placeholder/tmp.pptx"
x <- read_pptx(pptx_corrupted)
x <- ph_fortify_layout_labels(x, action = "delete")
x$slideLayouts$get_xfrm_data()$ph_label
print(x, target = pptx_repaired)
x_repaired <- read_pptx(pptx_repaired)
x_repaired$slideLayouts$get_xfrm_data()$ph_label
annotate_base(pptx_repaired)

