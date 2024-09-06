devtools::load_all()

# rename function
layout_duplicate_rename <- function(x) {
  for (slide_layout in split(x$slideLayouts$get_xfrm_data(), factor(x$slideLayouts$get_xfrm_data()$file))) {
    for (duplicated_layout in unique(slide_layout$ph_label[duplicated(slide_layout$ph_label)])) {
      duplicated_ids <- as.numeric(
        slide_layout[
          slide_layout$ph_label %in% slide_layout$ph_label[duplicated(slide_layout$ph_label)],
        ]$id
      )

      rename_ids <-
        setdiff(duplicated_ids, min(duplicated_ids))

      layout_file <-
        list.files(x$package_dir, paste0(slide_layout$file[1], "$"), recursive = T, full.names = T)

      layout_xml <-
        xml2::read_xml(layout_file)

      for (rename_index in 1:length(rename_ids)) {
        layout_delete_nodes <-
          xml2::xml_find_all(layout_xml, sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]", rename_ids[rename_index]))

        xml2::xml_set_attr(
          xml2::xml_find_all(
            layout_xml,
            sprintf("//p:cNvPr[@id='%s']", rename_ids[rename_index])
          ),
          "name", paste0(duplicated_layout, " ", rename_index)
        )
      }

      xml2::write_xml(layout_xml, file = layout_file)
    }
  }
}
# delete function
layout_duplicate_delete <- function(x, keep_max_id = TRUE) {
  for (slide_layout in split(x$slideLayouts$get_xfrm_data(), factor(x$slideLayouts$get_xfrm_data()$file))) {
    for (duplicated_layout in unique(slide_layout$ph_label[duplicated(slide_layout$ph_label)])) {
      duplicated_ids <- as.numeric(
        slide_layout[
          slide_layout$ph_label %in% slide_layout$ph_label[duplicated(slide_layout$ph_label)],
        ]$id
      )

      delete_ids <-
        setdiff(duplicated_ids, ifelse(keep_max_id, max(duplicated_ids), min(duplicated_ids)))

      layout_file <-
        list.files(x$package_dir, paste0(slide_layout$file[1], "$"), recursive = T, full.names = T)

      layout_xml <-
        xml2::read_xml(layout_file)

      for (delete_id in delete_ids) {
        layout_delete_node <-
          xml2::xml_find_all(layout_xml, sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]", delete_id))

        xml2::xml_remove(layout_delete_node)
      }

      xml2::write_xml(layout_xml, file = layout_file)
    }
  }
}

# import attached presentation via officer
file_in <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
file_out <- "dev_local/issue_589_placeholder/Präsentation1_rename.pptx"
ppt_rename <- read_pptx(file_in)
ppt_rename$slideLayouts$get_xfrm_data()$ph_label
layout_duplicate_rename(ppt_rename)
ppt_rename$slideLayouts$get_xfrm_data()$ph_label
print(ppt_rename, target = file_out)
ppt_rename <- read_pptx(file_out)
ppt_rename$slideLayouts$get_xfrm_data()$ph_label


# export cleaned pptx files


print(ppt_delete, target = "dev_local/issue_589_placeholder/Präsentation1_delete.pptx")

# annotate
annotate_base("dev_local/issue_589_placeholder/Präsentation1_rename.pptx")
annotate_base("dev_local/issue_589_placeholder/Präsentation1_delete.pptx")

# vs error via original file
annotate_base(file_in)

# ph_replace_label <- function(xml, shape_id, ph_label_new) {
#   cnvpr_node <- xml2::xml_find_first(xml, sprintf("/p:cSld/p:spTree/*/p:nvSpPr/p:cNvPr[@id='%s']", shape_id))
#   xml2::xml_set_attr(cnvpr_node, "name", ph_label_new)
#   xml
# }


# get_layout_xfrm_list <- function(x) {
#   xfrm <- x$slideLayouts$get_xfrm_data()
#   split(xfrm, xfrm$file)
# }


# make_labels_unique(c("A", "B", "B", "C", "A"))
make_labels_unique <- function(x, sep = ".") {
  if (length(x) == 0) {
    return(NULL)
  }
  ii <- ave(x, x, FUN = seq_along) |> as.numeric()
  paste0(x, sep, ii)
}


# keep dupes and create new labels for dupes
prepare_xrfm <- function(x) {
  ii <- duplicated(x$ph_label) | duplicated(x$ph_label, fromLast = TRUE)
  x <- x[ii, , drop = FALSE]
  transform(x, ph_label_new = make_labels_unique(ph_label), delete = duplicated(ph_label))
}


ph_labels_rename <- function(x, xfrm) {
  if (nrow(xfrm) == 0) {
    return(x)
  }
  layout_file <- unique(xfrm$file)
  layout <- x$slideLayouts$collection_get(layout_file)
  layout_xml <- layout$get()
    for (i in 1L:nrow(xfrm)) {
    cnvpr_node <- xml2::xml_find_first(layout_xml, sprintf("//p:cNvPr[@id='%s']", xfrm$id[i]))
    xml2::xml_set_attr(cnvpr_node, "name", xfrm$ph_label_new[i])
  }
  layout$save()
}


ph_repair_names <- function(x, action = "repair", verbose = FALSE) {
  xfrm <- x$slideLayouts$get_xfrm_data()
  xfrm_list <- split(xfrm, xfrm$file)
  xfrm_list <- xfrm_list |> lapply(prepare_xrfm)
  .x <- lapply(xfrm_list, \(xfrm) ph_labels_rename(x, xfrm))
  if (verbose) {
    df <- do.call(rbind, xfrm_list)[, c("ph_label", "ph_label_new", "name", "master_name"), drop = FALSE]
    rownames(df) <- NULL
    colnames(df)[3] <- "layout_name"
    cat("Found", nrow(df), "duplicates. Replacing ph label as follows:\n")
    print(df)
  }
  x
}



library(xml2)
pptx_corrupted <- "dev_local/issue_589_placeholder/Prasentation1.pptx"
pptx_repaired <- "dev_local/issue_589_placeholder/tmp.pptx"
x <- read_pptx(pptx_corrupted)
x <- ph_repair_names(x)
x$slideLayouts$get_xfrm_data()$ph_label
print(x, target = pptx_repaired)
x_repaired <- read_pptx(pptx_repaired)
x_repaired$slideLayouts$get_xfrm_data()$ph_label

# annotate_base(pptx_corrupted)
annotate_base(pptx_repaired)

