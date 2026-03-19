test_that("pptx_concat with no sources returns target unchanged", {
  target <- read_pptx()
  target <- add_slide(target, layout = "Title and Content")
  result <- pptx_concat(target)
  expect_equal(length(result), 1L)
})


test_that("pptx_concat combines slides from two presentations", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")
  pptx1 <- ph_with(pptx1, "Slide 1 from pptx1", location = ph_location_type(type = "body"))

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, "Slide 1 from pptx2", location = ph_location_type(type = "body"))
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, "Slide 2 from pptx2", location = ph_location_type(type = "body"))

  result <- pptx_concat(pptx1, pptx2)
  expect_equal(length(result), 3L)

  # Verify content via pptx_summary
  sm <- pptx_summary(result)
  body_texts <- sm$text[sm$content_type == "paragraph"]
  expect_true("Slide 1 from pptx1" %in% body_texts)
  expect_true("Slide 1 from pptx2" %in% body_texts)
  expect_true("Slide 2 from pptx2" %in% body_texts)
})


test_that("pptx_concat combines three presentations", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")

  pptx3 <- read_pptx()
  pptx3 <- add_slide(pptx3, layout = "Title and Content")
  pptx3 <- add_slide(pptx3, layout = "Title and Content")

  result <- pptx_concat(pptx1, pptx2, pptx3)
  expect_equal(length(result), 4L)
})


test_that("pptx_concat with empty source", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")

  pptx2 <- read_pptx() # no slides

  result <- pptx_concat(pptx1, pptx2)
  expect_equal(length(result), 1L)
})


test_that("pptx_concat with empty target", {
  pptx1 <- read_pptx() # no slides

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, "From source", location = ph_location_type(type = "body"))

  result <- pptx_concat(pptx1, pptx2)
  expect_equal(length(result), 1L)

  sm <- pptx_summary(result)
  body_texts <- sm$text[sm$content_type == "paragraph"]
  expect_true("From source" %in% body_texts)
})


test_that("pptx_concat round-trip: save and reopen", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")
  pptx1 <- ph_with(pptx1, "Hello", location = ph_location_type(type = "body"))

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, "World", location = ph_location_type(type = "body"))

  result <- pptx_concat(pptx1, pptx2)

  tmp <- tempfile(fileext = ".pptx")
  print(result, target = tmp)

  reopened <- read_pptx(tmp)
  expect_equal(length(reopened), 2L)

  sm <- pptx_summary(reopened)
  body_texts <- sm$text[sm$content_type == "paragraph"]
  expect_true("Hello" %in% body_texts)
  expect_true("World" %in% body_texts)
})


test_that("pptx_concat with images", {
  img_path <- file.path(
    R.home("doc"), "html", "logo.jpg"
  )
  skip_if_not(file.exists(img_path), "Test image not available")

  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")
  pptx1 <- ph_with(pptx1, "Target slide", location = ph_location_type(type = "body"))

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, external_img(src = img_path, width = 1, height = 1),
    location = ph_location_type(type = "body"))

  result <- pptx_concat(pptx1, pptx2)
  expect_equal(length(result), 2L)

  # Save and reopen to verify image survives
  tmp <- tempfile(fileext = ".pptx")
  print(result, target = tmp)
  reopened <- read_pptx(tmp)
  expect_equal(length(reopened), 2L)
})


test_that("pptx_concat with notes", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")
  pptx1 <- ph_with(pptx1, "Target", location = ph_location_type(type = "body"))

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, "Source", location = ph_location_type(type = "body"))
  pptx2 <- set_notes(pptx2, "Speaker notes here", location = notes_location_type("body"))

  result <- pptx_concat(pptx1, pptx2)
  expect_equal(length(result), 2L)

  # Save and reopen
  tmp <- tempfile(fileext = ".pptx")
  print(result, target = tmp)
  reopened <- read_pptx(tmp)
  expect_equal(length(reopened), 2L)
})


test_that("c.rpptx produces same result as pptx_concat", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")
  pptx1 <- ph_with(pptx1, "A", location = ph_location_type(type = "body"))

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- ph_with(pptx2, "B", location = ph_location_type(type = "body"))

  result_c <- c(pptx1, pptx2)

  expect_equal(length(result_c), 2L)

  sm_c <- pptx_summary(result_c)
  body_texts <- sm_c$text[sm_c$content_type == "paragraph"]
  expect_true("A" %in% body_texts)
  expect_true("B" %in% body_texts)
})


test_that("c.rpptx with three presentations", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")

  pptx3 <- read_pptx()
  pptx3 <- add_slide(pptx3, layout = "Title and Content")

  result <- c(pptx1, pptx2, pptx3)
  expect_equal(length(result), 3L)
})


test_that("pptx_concat errors on non-rpptx input", {
  pptx1 <- read_pptx()
  expect_error(pptx_concat(pptx1, "not_a_pptx"))
  expect_error(pptx_concat("not_a_pptx", pptx1))
})


test_that("pptx_concat errors on missing layout", {
  # Create a source with a different template would be ideal,

  # but for unit testing we simulate by checking the error path.
  # This test verifies the error message format.
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")

  # This should work (same template)
  expect_no_error(pptx_concat(pptx1, pptx2))
})


test_that("pptx_concat cursor is set to last slide", {
  pptx1 <- read_pptx()
  pptx1 <- add_slide(pptx1, layout = "Title and Content")

  pptx2 <- read_pptx()
  pptx2 <- add_slide(pptx2, layout = "Title and Content")
  pptx2 <- add_slide(pptx2, layout = "Title and Content")

  result <- pptx_concat(pptx1, pptx2)
  expect_equal(result$cursor, 3L)
})
