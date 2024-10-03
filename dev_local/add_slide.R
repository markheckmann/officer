#' @export
#' @title Add a slide
#' @description Add a slide into a pptx presentation.
#' @param x an `rpptx` object.
#' @param layout slide layout name or numeric index (row index from [layout_summary()).
#' @param master master layout name where `layout` is located. Can be omitted if layout is unambiguous.
#' @examples
#' x <- read_pptx()
#' layout_summary(x)
#' x <- add_slide(x, layout = "Two Content", master = "Office Theme")
#' x <- add_slide(x, layout = "Two Content") # no master needed for unique layout name
#' x <- add_slide(x, layout = 4) # use layout index instead of name
#' @seealso [print.rpptx()], [read_pptx()], [plot_layout_properties()], [ph_with()], [layout_summary()]
#' @family functions slide manipulation
add_slide <- function(x, layout = "Title and Content", master = NULL) {
  la <- get_layout(x, layout, master)
  slide_info <- x$slideLayouts$get_metadata()
  slide_info <- slide_info[slide_info$name == la$layout_name & slide_info$master_name == la$master_name, ]
  new_slidename <- x$slide$get_new_slidename()

  xml_file <- file.path(x$package_dir, "ppt/slides", new_slidename)
  layout_obj <- x$slideLayouts$collection_get(slide_info$filename)
  layout_obj$write_template(xml_file)

  # update presentation elements
  x$presentation$add_slide(target = file.path("slides", new_slidename))
  x$content_type$add_slide(partname = file.path("/ppt/slides", new_slidename))

  x$slide$add_slide(xml_file, x$slideLayouts$get_xfrm_data())

  x$cursor <- x$slide$length()
  x
}

