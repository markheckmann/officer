devtools::load_all()


# make_labels_unique(c("A", "B", "B", "C", "A"))
make_labels_unique <- function(x, sep = ".") {
  ii <- ave(x, x, FUN = seq_along) |> as.numeric()
  paste0(x, sep, ii)
}

fix_layout_ph_labels <- function(x, action = "repair") {
  action <- match.arg(action, c("repair", "delete"))
  xfrm <- x$slideLayouts$get_xfrm_data()
  xfrm_list <- split(xfrm, xfrm$file)
  xfrm_list <- xfrm_list[sapply(xfrm_list, nrow) > 0]

  for (xfrm in xfrm_list) {
    xfrm <- subset(xfrm, duplicated(ph_label) | duplicated(ph_label, fromLast = TRUE))
    if (nrow(xfrm) == 0) {
      next
    }
    if (action == "repair") {
      xfrm <- transform(xfrm, ph_label_new = make_labels_unique(ph_label), delete_flag = FALSE)
    } else {
      xfrm <- transform(xfrm, ph_label_new = ph_label, delete_flag = duplicated(ph_label))
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
  }
  x
}



.fix_layout <- function(layout_file, x, action = "repair") {
  if (!grepl("\\.xml$", layout_file, ignore.case = TRUE)) {
    stop("'layout_file' must be the name of an xml file", call. = FALSE)
  }
  action <- match.arg(action, c("repair", "remove"))
  layout <- x$slideLayouts$collection_get(layout_file)
  xfrm <- layout$xfrm()
  xfrm <- subset(xfrm, duplicated(ph_label) | duplicated(ph_label, fromLast = TRUE))
  if (nrow(xfrm) == 0) {
    return()
  }
  if (action == "repair") {
    xfrm <- transform(xfrm, ph_label_new = make_labels_unique(ph_label), delete_flag = FALSE)
  } else {
    xfrm <- transform(xfrm, ph_label_new = ph_label, delete_flag = duplicated(ph_label))
  }

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
  xfrm
}


.info_fix_layout_ph_labels <- function(xfrm_list, action) {
  df_1 <- do.call(rbind, xfrm_list)
  df_2 <- x$slideLayouts$get_xfrm_data()
  df_2 <- df_2[, c("master_file", "master_name"), drop=FALSE] |> unique()
  df <- merge(df_1, df_2)
  rownames(df) <- NULL
  df <- subset(df, select = c(ph_label, ph_label_new, name, master_name, delete_flag))
  colnames(df)[3] <- "layout_name"
  if (action == "repair") {
    df$delete_flag <- NULL
    cat("Repaired these duplicated placeholder labels:\n")
  } else {
    df <- subset(df, delete_flag)
    df$ph_label_new <- NULL
    cat("Removed these placeholders with duplicate label:\n")
  }
  print(df)
}


fix_layout_ph_labels <- function(x, action = "repair", verbose = FALSE) {
  layout_names <- x$slideLayouts$get_metadata()$filename
  xfrm_list <- lapply(layout_names, .fix_layout, x = x, action = action)
  if (verbose) {
    .info_fix_layout_ph_labels(xfrm_list, action)
  }
  x
}



# import attached presentation via officer
file_in <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
file_out <- "dev_local/issue_589_placeholder/Prasentation1_repair.pptx"
x <- read_pptx(file_in)
x$slideLayouts$get_xfrm_data()$ph_label
fix_layout_ph_labels(x, verbose = TRUE)
print(x, target = file_out)
x <- read_pptx(file_out)
x$slideLayouts$get_xfrm_data()$ph_label
annotate_base(file_out)


file_in <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
file_out <- "dev_local/issue_589_placeholder/Prasentation1_delete.pptx"
x <- read_pptx(file_in)
x$slideLayouts$get_xfrm_data()$ph_label
fix_layout_ph_labels(x, action = "remove", verbose = T)
print(x, target = file_out)
x <- read_pptx(file_out)
x$slideLayouts$get_xfrm_data()$ph_label
annotate_base(file_out)
