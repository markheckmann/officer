url <- "https://github.com/user-attachments/files/16825504/many_layouts.pptx"
file <- tempfile(fileext = ".pptx")
download.file(url, file)
x <- read_pptx(file)
x$slideLayouts$get_metadata()
layout_summary() calls the x$slideLayouts$get_metadata() method


# slideLayout1.xml -> 1
# slideLayout12.xml -> 12
# slideMaster1.xml -> 1
get_file_index <- function(file) {
  sub(pattern = ".+?(\\d+).xml$", replacement = "\\1", x = file, ignore.case = TRUE) |> as.numeric()
}

sort_by_index <- function(x) {
  indexes <- get_file_index(x)
  x[order(indexes)]
}

# order of layouts is not the same as in PPTX presentation
file <- system.file("doc_examples", "many_layouts.pptx", package = "officer")
x <- read_pptx(file)
x$slideLayouts$get_metadata()
