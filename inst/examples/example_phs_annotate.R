# Annotate all placeholders on current slide
x <- read_pptx()
x <- add_slide(x, "Title Slide")
x <- phs_annotate(x)
\dontrun{
print(x, preview = TRUE)}


# Annotate all placeholders on all slides
x <- read_pptx()
x <- add_slide(x, "Title and Content")
x <- add_slide(x, "Two Content")
x <- phs_annotate(x, .slide_idx = "all") # annotate all slides
\dontrun{
print(x, preview = TRUE)}


# Annotate some placeholders only
x <- read_pptx()
x <- add_slide(x, "Title Slide")
x <- phs_annotate(x, "Title 1") # annotate phs with label "Title 1" only
x <- add_slide(x, "Title and Content")
x <- add_slide(x, "Two Content")
x <- phs_annotate(x, "dt", "ftr", .slide_idx = 2:3) # only types "dt" and "ftr" on slides 2 and 3
\dontrun{
print(x, preview = TRUE)}


# Modify ph appearance (usually not needed) -------

## Show label only
x <- read_pptx()
x <- add_slide(x, "Title Slide")
x <- phs_annotate(x, .font_size = c(type = 0, id = 0), .keys = FALSE)
\dontrun{
print(x, preview = TRUE)}

## Modify colors and box
x <- read_pptx()
x <- add_slide(x, "Title Slide")
x <- phs_annotate(x, .font_color = c("red", "blue", "darkgreen"), .bg = "#ff000010", .line = NA)
\dontrun{
print(x, preview = TRUE)}
