wml_str <- function(str) {
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n",
    "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">",
    str,
    "</w:document>"
  )
}

pml_str <- function(str) {
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n",
    "<a:document xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:p=\"http://schemas.openxmlformats.org/presentationml/2006/main\">",
    str,
    "</a:document>"
  )
}

has_css_color <- function(x, atname, color) {
  css <- format(x, type = "html")
  reg <- sprintf("%s:%s", atname, color)
  grepl(reg, css)
}

has_css_attr <- function(x, atname, value) {
  css <- format(x, type = "html")
  reg <- sprintf("%s:%s", atname, value)
  grepl(reg, css)
}



# helpers to compare rpptx object against a target

get_shapetree <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% x$cursor
  xml_node <- x$slide$get_slide(slide_idx)$get()
  xml2::xml_child(xml_node, "*/p:spTree")
}


get_shapetrees <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% seq_len(length(x))
  lapply(slide_idx, function(idx) get_shapetree(x, idx))
}


# all slide's shapetrees as a string and shape's UUIDs removed
# used to check if created slides are identical.
get_shapetrees_string <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  sp_tree <- get_shapetrees(x, slide_idx = slide_idx)
  sp_tree_chr <- vapply(sp_tree, paste, character(1))
  s <- paste(sp_tree_chr, collapse = " ")
  gsub(
    "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
    "xxx",
    s
  ) # delete shape's UUIDs
}



# read pptx if x is a file path. If x is an rpptx object, save it to disk and reload it
# for coherent shapetree structure
get_saved_or_reload_rpptx <- function(x) {
  if (is_rpptx(x)) {
    x <- print(x, tempfile(fileext = ".pptx"))
  }
  if (is.character(x)) {
    x <- read_pptx(x)
    return(x)
  }
  cli::cli_abort("{.arg x} must be a {.cls rpptx} object or a file path.")
}


# compares two presentations
# x,y rpptx object or path to .pptx file
is_identical_shapetree <- function(x, y, slide_idx = NULL) {
  x <- get_saved_or_reload_rpptx(x)
  y <- get_saved_or_reload_rpptx(y)
  st_x <- get_shapetrees_string(x)
  st_y <- get_shapetrees_string(y)
  identical(st_x, st_y)
}

