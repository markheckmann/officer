# this name will be used to print the file
# change it to "youfile.pptx" to write the pptx
# file in your working directory.
fileout <- tempfile(fileext = ".pptx")

doc_1 <- read_pptx()
sz <- slide_size(doc_1)

# add text and a table ----
doc_1 <- add_slide(doc_1, layout = "Two Content", master = "Office Theme")
doc_1 <- ph_with(
  x = doc_1,
  value = c("Table cars"),
  location = ph_location_type(type = "title")
)
doc_1 <- ph_with(
  x = doc_1,
  value = names(cars),
  location = ph_location_left()
)
doc_1 <- ph_with(
  x = doc_1,
  value = cars,
  location = ph_location_right()
)
doc_1 <- ph_with(
  x = doc_1,
  value = Sys.Date(),
  location = ph_location_type("dt")
)

# add a base plot ----
anyplot <- plot_instr(code = {
  col <- c(
    "#440154FF",
    "#443A83FF",
    "#31688EFF",
    "#21908CFF",
    "#35B779FF",
    "#8FD744FF",
    "#FDE725FF"
  )
  barplot(1:7, col = col, yaxt = "n")
})

doc_1 <- add_slide(doc_1, "Title and Content")
doc_1 <- ph_with(
  doc_1,
  anyplot,
  location = ph_location_fullsize(),
  bg = "#006699"
)

# add a ggplot2 plot ----
if (require("ggplot2")) {
  doc_1 <- add_slide(doc_1, "Title and Content")
  gg_plot <- ggplot(data = iris) +
    geom_point(
      mapping = aes(Sepal.Length, Petal.Length),
      size = 3
    ) +
    theme_minimal()
  doc_1 <- ph_with(
    x = doc_1,
    value = gg_plot,
    location = ph_location_type(type = "body"),
    bg = "transparent"
  )
  doc_1 <- ph_with(
    x = doc_1,
    value = "graphic title",
    location = ph_location_type(type = "title")
  )
}

# add a external images ----
doc_1 <- add_slide(doc_1, layout = "Title and Content", master = "Office Theme")
doc_1 <- ph_with(
  x = doc_1,
  value = empty_content(),
  location = ph_location(
    left = 0,
    top = 0,
    width = sz$width,
    height = sz$height,
    bg = "black"
  )
)

svg_file <- file.path(R.home(component = "doc"), "html/Rlogo.svg")
if (require("rsvg")) {
  doc_1 <- ph_with(
    x = doc_1,
    value = "External images",
    location = ph_location_type(type = "title")
  )
  doc_1 <- ph_with(
    x = doc_1,
    external_img(svg_file, 100 / 72, 76 / 72),
    location = ph_location_right(),
    use_loc_size = FALSE
  )
  doc_1 <- ph_with(
    x = doc_1,
    external_img(svg_file),
    location = ph_location_left(),
    use_loc_size = TRUE
  )
}

# add a block_list ----
dummy_text <- readLines(system.file(
  package = "officer",
  "doc_examples/text.txt"
))
fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = TRUE, font.size = 0)
fp_3 <- fp_text(italic = TRUE, color = "red", font.size = 0)
bl <- block_list(
  fpar(ftext("hello world", fp_1)),
  fpar(
    ftext("hello", fp_2),
    ftext("hello", fp_3)
  ),
  dummy_text
)
doc_1 <- add_slide(doc_1, "Title and Content")
doc_1 <- ph_with(
  x = doc_1,
  value = bl,
  location = ph_location_type(type = "body")
)


# fpar ------
fpt <- fp_text(
  bold = TRUE,
  font.family = "Bradley Hand",
  font.size = 150,
  color = "#F5595B"
)
hw <- fpar(
  ftext("hello ", fpt),
  hyperlink_ftext(
    href = "https://cran.r-project.org/index.html",
    text = "cran",
    prop = fpt
  )
)
doc_1 <- add_slide(doc_1, "Title and Content")
doc_1 <- ph_with(
  x = doc_1,
  value = hw,
  location = ph_location_type(type = "body")
)
# unordered_list ----
ul <- unordered_list(
  level_list = c(1, 2, 2, 3, 3, 1),
  str_list = c("Level1", "Level2", "Level2", "Level3", "Level3", "Level1"),
  style = fp_text(color = "red", font.size = 0)
)
doc_1 <- add_slide(doc_1, "Title and Content")
doc_1 <- ph_with(
  x = doc_1,
  value = ul,
  location = ph_location_type()
)

print(doc_1, target = fileout)


# Example using short-form locations ----
x <- read_pptx()
x <- add_slide(x, "Title Slide")
x <- ph_with(x, "A title", "Title 1") # label
x <- ph_with(x, "A subtitle", 3) # id
x <- ph_with(x, "A left text", "left") # keyword
x <- ph_with(x, Sys.Date(), "dt[1]") # type + index
x <- ph_with(x, "More content", c(5, .5, 5, 2)) # numeric vector (left, top, width, heigh)
\dontrun{
print(x, preview = TRUE)}


# Example passing ph modifiers via dots (...) ----
colored_text <- fpar(ftext("some colored text  ...", prop = fp_text_lite(color = "red")))
ext_img <- external_img(system.file("img/dog.png", package = "officer"), guess_size = TRUE)
my_line <- sp_line("red", lty = "dash", lwd = 2)
bg <- "#ff000030"

x <- read_pptx()
# slide with text
x <- add_slide(x, "Two Content")
x <- ph_with(x, "The Title", "title", bg = "#0000ff30", ln = "blue", geom = "horizontalScroll")
x <- ph_with(x, "I am a star", "body[1]", geom = "star32", bg = "orange")
x <- ph_with(x, colored_text, "body[2]", geom = "roundRect", rotation = 3, bg = bg, ln = "red")
# slide with images
x <- add_slide(x, "Two Content")
x <- ph_with(x, "Doggies", "title", bg = "#0000ff30", geom = "downArrow", ln = "blue")
x <- ph_with(x, ext_img, "body[1]", use_loc_size = FALSE, bg = bg, rotation = -3, ln = my_line)
x <- ph_with(x, ext_img, "body[2]", use_loc_size = FALSE, bg = "#00ff0030", rotation = 5)
\dontrun{
print(x, preview = TRUE)}
