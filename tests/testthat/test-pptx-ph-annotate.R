test_that("phs_annotate annotates current slide by default", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    phs_annotate()
  expect_s3_class(x, "rpptx")
  slide <- x$slide$get_slide(x$cursor)
  nodes <- xml2::xml_find_all(slide$get(), "//p:spTree/p:sp")
  expect_gt(length(nodes), 0)
})

test_that("phs_annotate with .slide_idx = 'all' processes all slides", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    add_slide("Title and Content") |>
    phs_annotate(.slide_idx = "all")
  expect_s3_class(x, "rpptx")
  expect_equal(length(x), 2)
  nodes_1 <- xml2::xml_find_all(x$slide$get_slide(1)$get(), "//p:spTree/p:sp")
  nodes_2 <- xml2::xml_find_all(x$slide$get_slide(2)$get(), "//p:spTree/p:sp")
  expect_gt(length(nodes_1), 0)
  expect_gt(length(nodes_2), 0)
})

test_that("phs_annotate restores cursor position", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    add_slide("Title and Content")
  x$cursor <- 1L
  x <- phs_annotate(x, .slide_idx = 2)
  expect_equal(x$cursor, 1L)
})

test_that("phs_annotate with subset of phs via ...", {
  x <- read_pptx() |>
    add_slide("Title and Content") |>
    phs_annotate("body")
  expect_s3_class(x, "rpptx")
})

test_that("phs_annotate skips non-existent ph gracefully", {
  x <- read_pptx() |>
    add_slide("Title Slide")
  expect_no_error(phs_annotate(x, "nonexistent_label_xyz"))
})

test_that("phs_annotate respects .font_size argument", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    phs_annotate(.font_size = c(label = 10, type = 12, id = 8))
  expect_s3_class(x, "rpptx")
})

test_that("phs_annotate respects .font_color argument", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    phs_annotate(.font_color = c(label = "red", type = "blue", id = "darkgreen"))
  expect_s3_class(x, "rpptx")
})

test_that("phs_annotate respects .bg argument", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    phs_annotate(.bg = "#FF000020")
  expect_s3_class(x, "rpptx")
})

test_that("phs_annotate respects .line argument as color string", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    phs_annotate(.line = "red")
  expect_s3_class(x, "rpptx")
})

test_that("phs_annotate respects .keys = FALSE", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    phs_annotate(.keys = FALSE)
  expect_s3_class(x, "rpptx")
})

test_that("add_annotated_layouts adds slides for all layouts", {
  x <- read_pptx() |>
    add_annotated_layouts()
  n_layouts <- nrow(layout_summary(read_pptx()))
  expect_equal(length(x), n_layouts)
})

test_that("add_annotated_layouts with subset by name", {
  x <- read_pptx() |>
    add_annotated_layouts(layouts = c("Title Slide", "Title and Content"))
  expect_equal(length(x), 2)
})

test_that("add_annotated_layouts with subset by index", {
  x <- read_pptx() |>
    add_annotated_layouts(layouts = 1:2)
  expect_equal(length(x), 2)
})

test_that("add_annotated_layouts errors on non-existent layout name", {
  expect_error(
    read_pptx() |> add_annotated_layouts(layouts = "NonExistentLayout"),
    "does not exist"
  )
})

test_that("add_annotated_layouts errors on out-of-range index", {
  expect_error(
    read_pptx() |> add_annotated_layouts(layouts = 999),
    "out of range"
  )
})

test_that("add_annotated_layouts with remove_slides = TRUE", {
  x <- read_pptx() |>
    add_slide("Title Slide") |>
    add_slide("Title and Content") |>
    add_annotated_layouts(remove_slides = TRUE)
  n_layouts <- nrow(layout_summary(read_pptx()))
  expect_equal(length(x), n_layouts)
})

test_that("annotate_base returns rpptx with output_file = NULL", {
  x <- annotate_base(output_file = NULL)
  expect_s3_class(x, "rpptx")
  n_layouts <- nrow(layout_summary(read_pptx()))
  expect_equal(length(x), n_layouts)
})

test_that("annotate_base writes file and returns invisibly", {
  f <- tempfile(fileext = ".pptx")
  x <- annotate_base(output_file = f)
  expect_true(file.exists(f))
  expect_s3_class(x, "rpptx")
  unlink(f)
})

test_that("annotate_base with layouts subset", {
  x <- annotate_base(output_file = NULL, layouts = c("Title Slide"))
  expect_equal(length(x), 1)
})

test_that("annotate_base passes ... to phs_annotate", {
  x <- annotate_base(output_file = NULL, .bg = "#00FF0010")
  expect_s3_class(x, "rpptx")
})
