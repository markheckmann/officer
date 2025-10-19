test_that("pptx ph locations", {
  doc <- read_pptx()
  doc <- add_slide(doc, "Title and Content", "Office Theme")

  doc <- ph_with(
    x = doc,
    value = "left",
    location = ph_location_left()
  )
  doc <- ph_with(
    x = doc,
    value = "right",
    location = ph_location_right()
  )
  doc <- ph_with(
    x = doc,
    value = "title",
    location = ph_location_type(type = "title")
  )
  doc <- ph_with(
    x = doc,
    value = "fullsize",
    location = ph_location_fullsize()
  )
  doc <- ph_with(
    x = doc,
    value = "from title",
    location = ph_location_template(
      left = 1,
      width = 2,
      height = 1,
      top = 4,
      type = "title",
      newlabel = "newlabel"
    )
  )

  layouts_info <- layout_properties(doc)

  title_xfrm <- layouts_info[
    layouts_info$name %in% "Two Content" & layouts_info$type %in% "title",
    c("offx", "offy", "cx", "cy")
  ]
  side_xfrm <- layouts_info[
    layouts_info$name %in% "Two Content" & layouts_info$type %in% "body",
    c("offx", "offy", "cx", "cy")
  ]
  full_xfrm <- as.data.frame(slide_size(doc))
  names(full_xfrm) <- c("cx", "cy")
  full_xfrm <- cbind(data.frame(offx = 0L, offy = 0L), full_xfrm)
  from_title_xfrm <- data.frame(offx = 1, offy = 4, cx = 2, cy = 1)
  theorical_xfrm <- rbind(
    side_xfrm,
    title_xfrm,
    full_xfrm,
    from_title_xfrm
  )

  all_xfrm <- xml_find_all(
    x = doc$slide$get_slide(1)$get(),
    xpath = "/p:sld/p:cSld/p:spTree/p:sp/p:spPr/a:xfrm"
  )
  offx <- xml_attr(xml_child(all_xfrm, "a:off"), "x")
  offx <- as.integer(offx) / 914400
  offy <- xml_attr(xml_child(all_xfrm, "a:off"), "y")
  offy <- as.integer(offy) / 914400
  cx <- xml_attr(xml_child(all_xfrm, "a:ext"), "cx")
  cx <- as.integer(cx) / 914400
  cy <- xml_attr(xml_child(all_xfrm, "a:ext"), "cy")
  cy <- as.integer(cy) / 914400

  observed_xfrm <- data.frame(offx = offx, offy = offy, cx = cx, cy = cy)
  expect_equivalent(observed_xfrm, theorical_xfrm)
})


test_that("pptx ph_location_type", {
  opts <- options(cli.num_colors = 1) # suppress colors for easier error message check
  on.exit(options(opts))

  x <- read_pptx()
  x <- add_slide(x, "Two Content")

  expect_no_error({
    ph_with(x, "correct ph type id", ph_location_type("body", type_idx = 1))
  })

  expect_warning(
    {
      ph_with(
        x,
        "cannot supply id AND type_idx",
        ph_location_type("body", type_idx = 1, id = 1)
      )
    },
    regexp = "`id` is ignored if `type_idx` is provided",
    fixed = TRUE
  )

  expect_warning(
    {
      ph_with(
        x,
        "id still working with warning to avoid breaking change",
        ph_location_type("body", id = 1)
      )
    },
    regexp = "The `id` argument in `ph_location_type()` is deprecated",
    fixed = TRUE
  )

  expect_error(
    {
      ph_with(x, "out of range type id", ph_location_type("body", type_idx = 3)) # 3 does not exists => no error or warning
    },
    regexp = "`type_idx` is out of range.",
    fixed = TRUE
  )

  expect_error(
    {
      expect_warning(
        {
          ph_with(x, "out of range type id", ph_location_type("body", id = 3)) # 3 does not exists => no error or warning
        },
        regexp = " The `id` argument in `ph_location_type()` is deprecated",
        fixed = TRUE
      )
    },
    regexp = "`id` is out of range.",
    fixed = TRUE
  )

  expect_error(
    {
      ph_with(
        x,
        "type okay but not available in layout",
        ph_location_type("tbl")
      ) # tbl not on layout
    },
    regexp = "Found no placeholder of type",
    fixed = TRUE
  )

  expect_error(
    {
      ph_with(x, "xxx is unknown type", ph_location_type("xxx"))
    },
    regexp = 'type "xxx" is unknown',
    fixed = TRUE
  )

  expect_no_error({
    # for complete coverage
    ph_with(
      x,
      " ph type position_right",
      ph_location_type("body", position_right = TRUE)
    )
  })
})


test_that("pptx ph_location_id", {
  opts <- options(cli.num_colors = 1) # no colors for easier error message check
  on.exit(options(opts))

  # direct errors
  error_exp <- "`id` must be one number"
  expect_error(ph_location_id(id = 1:2), regex = error_exp, fixed = TRUE)
  expect_error(ph_location_id(id = -1:1), regex = error_exp, fixed = TRUE)
  expect_error(
    ph_location_id(id = c("A", "B")),
    regex = error_exp,
    fixed = TRUE
  )
  expect_error(ph_location_id(id = c(NA, NA)), regex = error_exp, fixed = TRUE)

  error_exp <- "`id` must be a positive number"
  expect_error(ph_location_id(id = NULL), regex = error_exp, fixed = TRUE)
  expect_error(ph_location_id(id = NA), regex = error_exp, fixed = TRUE)
  expect_error(ph_location_id(id = NaN), regex = error_exp, fixed = TRUE)
  expect_error(
    ph_location_id(id = character(0)),
    regex = error_exp,
    fixed = TRUE
  )
  expect_error(ph_location_id(id = integer(0)), regex = error_exp, fixed = TRUE)

  expect_error(
    ph_location_id(id = "A"),
    regex = 'Cannot convert "A" to integer',
    fixed = TRUE
  )
  expect_error(
    ph_location_id(id = ""),
    regex = 'Cannot convert "" to integer',
    fixed = TRUE
  )
  expect_error(
    ph_location_id(id = Inf),
    regex = "Cannot convert Inf to integer",
    fixed = TRUE
  )
  expect_error(
    ph_location_id(id = -Inf),
    regex = "Cannot convert -Inf to integer",
    fixed = TRUE
  )

  error_exp <- "`id` must be a positive number"
  expect_error(ph_location_id(id = 0), regex = error_exp, fixed = TRUE)
  expect_error(ph_location_id(id = -1), regex = error_exp, fixed = TRUE)

  # downstream errors
  x <- read_pptx()
  x <- add_slide(x, "Comparison")

  expect_error(
    {
      ph_with(x, "id does not exist", ph_location_id(id = 1000))
    },
    "`id` 1000 does not exist",
    fixed = TRUE
  )

  # test for correct results
  expect_no_error({
    ids <- layout_properties(x, "Comparison")$id
    for (id in ids) {
      ph_with(
        x,
        paste("text:", id),
        ph_location_id(id, newlabel = paste("newlabel:", id))
      )
    }
  })
  nodes <- xml_find_all(
    x = x$slide$get_slide(1)$get(),
    xpath = "/p:sld/p:cSld/p:spTree/p:sp"
  )
  # text inside phs
  expect_true(all(xml_text(nodes) == paste("text:", ids)))
  # assigned shape names
  all_nvpr <- xml_find_all(nodes, "./p:nvSpPr/p:cNvPr")
  expect_true(all(xml_attr(all_nvpr, "name") == paste("newlabel:", ids)))
})


test_that("pptx ph labels", {
  opts <- options(cli.num_colors = 1) # no colors for easier error message check
  on.exit(options(opts))

  doc <- read_pptx()
  doc <- add_slide(doc, "Title and Content", "Office Theme")

  doc <- ph_with(
    x = doc,
    value = "elephant",
    location = ph_location_type(newlabel = "label1")
  )
  doc <- ph_with(
    x = doc,
    value = "elephant",
    location = ph_location_label(
      ph_label = "Date Placeholder 3",
      newlabel = "label2"
    )
  )
  doc <- ph_with(
    x = doc,
    value = "elephant",
    location = ph_location_left(newlabel = "label3")
  )
  doc <- ph_with(
    x = doc,
    value = "elephant",
    location = ph_location_right(newlabel = "label4")
  )

  all_nvpr <- xml_find_all(
    x = doc$slide$get_slide(1)$get(),
    xpath = "/p:sld/p:cSld/p:spTree/p:sp/p:nvSpPr/p:cNvPr"
  )
  expect_equal(
    xml_attr(all_nvpr, "name"),
    paste0("label", 1:4)
  )

  expect_error({
    doc <- ph_with(
      x = doc,
      value = "error if label does not exist",
      location = ph_location_label(ph_label = "xxx")
    )
  }, regexp = 'ph label "xxx" does not exist')

  # edge case: layout without any placeholders
  file <- test_path("docs_dir/test-no-placeholders-in-layout.pptx")
  x <- read_pptx(file)
  expect_error({
      x |> add_slide("layout_without_placeholders", xxx = "abc")
    }, regex = "The layout has no placeholders!"
  )

  expect_error({
      x <- x |> add_slide("layout_without_placeholders")
      x |> phs_with(xxx = "abc")
    }, regex = "The layout has no placeholders!"
  )

  # edge case: duplicate labels
  expect_error({
    file <- test_path("docs_dir", "test-pptx-dedupe-ph.pptx")
    x <- read_pptx(file)
    x <- x |> add_slide("2x2-dupes")
    x |> phs_with(Content = "xxx")
  }, regex = 'Placeholder "Content" label in layout "2x2-dupes" is duplicated.')
})


test_that("as_ph_location", {
  ref_names <- c(
    "width",
    "height",
    "left",
    "top",
    "ph_label",
    "ph",
    "type",
    "rotation",
    "fld_id",
    "fld_type"
  )
  l <- replicate(length(ref_names), "dummy", simplify = FALSE)
  df <- as.data.frame(l)
  names(df) <- ref_names

  expect_no_error({
    as_ph_location(df)
  })

  expect_error(
    {
      as_ph_location(df[, -(1:2)])
    },
    regexp = "missing column values:width,height",
    fixed = TRUE
  )

  expect_error(
    {
      as_ph_location("wrong class supplied")
    },
    regexp = "`x` must be a data frame",
    fixed = TRUE
  )
})


test_that("get_ph_loc", {
  x <- read_pptx()
  expect_no_error({
    get_ph_loc(
      x,
      "Comparison",
      "Office Theme",
      type = "body",
      position_right = TRUE,
      position_top = FALSE
    )
  })
})


test_that("short-form locations", {
  opts <- options(cli.num_colors = 1) # no colors for easier error message check
  on.exit(options(opts))

  # helpers
  v2 <- v <- c(top = 0, left = 0.1, width = 5.2, height = 4)
  names(v2) <- substr(names(v2), 1, 1)
  expect_equal(fortify_named_location_position(v), v)
  expect_equal(fortify_named_location_position(v2), v)

  names(v2)[1] <- NA
  expect_error(
    fortify_named_location_position(v2),
    regexp = "Some vector elements have no names",
    fixed = TRUE
  )
  expect_error(
    fortify_named_location_position(unname(v)),
    regexp = "Some vector elements have no names",
    fixed = TRUE
  )

  res <- has_ph_type_format(c(
    "body",
    "body[1]",
    "body [1]",
    "  body    [ 1 ]  "
  ))
  expect_true(all(res))
  res <- has_ph_type_format(c(
    "body[]",
    "body  []",
    "body [a]",
    "body [ a1]",
    "body [1a]",
    "body [1",
    "body 1]",
    "unknown [1]"
  ))
  expect_true(all(!res))

  # incorrect input values/class
  err_msg <- "Must be a vector (character or numeric) or a ph_location object"
  expect_error(resolve_location(NA_integer_), regex = err_msg, fixed = TRUE)
  expect_error(resolve_location(NA_character_), regex = err_msg, fixed = TRUE)
  expect_error(resolve_location(NA_real_), regex = err_msg, fixed = TRUE)
  expect_error(resolve_location(NA), regex = err_msg, fixed = TRUE)
  expect_error(resolve_location(NULL), regex = err_msg, fixed = TRUE)
  expect_error(
    resolve_location(mtcars),
    regex = "Cannot resolve class <data.frame> into a location",
    fixed = TRUE
  )

  # numeric input
  v <- c(top = 0, left = 0, width = 5)
  expect_error(
    resolve_location(v),
    regex = "`location` has incorrect length.",
    fixed = TRUE
  )
  expect_error(
    resolve_location(unname(v)),
    regexp = "`location` has incorrect length.",
    fixed = TRUE
  )

  ## numeric: single integer
  v <- c("dummy" = 1)
  loc_1 <- resolve_location(v)
  loc_2 <- resolve_location(unname(v))
  loc_3 <- ph_location_id(v)
  loc_4 <- resolve_location(loc_3) # unchanged
  expect_equal(loc_1, loc_2)
  expect_equal(loc_1, loc_3)
  expect_equal(loc_1, loc_4)

  expect_error(
    resolve_location(1.1),
    regex = "If a length 1 numeric, `location` requires <integer>",
    fixed = TRUE
  )
  expect_error(
    resolve_location(-1),
    regex = "Integers passed to `location` must be positive",
    fixed = TRUE
  )

  ## numeric: (named) position vector
  v2 <- v <- c(left = 1, top = 2, width = 3.3, height = 4.4)
  names(v2) <- substr(names(v2), 1, 1)
  loc_1 <- resolve_location(unname(v))
  loc_2 <- resolve_location(v)
  loc_3 <- resolve_location(v2) # partial name matching
  loc_4 <- resolve_location(rev(v)) # order does not matter
  loc_5 <- ph_location(
    left = v["left"],
    top = v["top"],
    width = v["width"],
    height = v["height"]
  )
  loc_6 <- resolve_location(loc_5) # unchanged
  expect_equal(loc_1, loc_2)
  expect_equal(loc_1, loc_3)
  expect_equal(loc_1, loc_4)
  expect_equal(loc_1, loc_5)
  expect_equal(loc_1, loc_6)

  v <- c(top = 1, left = 2, width = 3, xxx = 10)
  expect_error(
    resolve_location(v),
    regex = 'Found 1 unknown name in `location`: "xxx"',
    fixed = TRUE
  )
  v <- c(top = 1, top = 2, width = 3, width = 4)
  expect_error(
    resolve_location(v),
    regex = 'Duplicate entries in `location`: "top" and "width"',
    fixed = TRUE
  )

  # character input
  v <- c("a", "b")
  expect_error(
    resolve_location(v),
    regex = "Character vector passed to `location` must have length 1",
    fixed = TRUE
  )

  ## keywords
  expect_equal(resolve_location("left"), ph_location_left())
  expect_equal(resolve_location("right"), ph_location_right())
  expect_equal(resolve_location("fullsize"), ph_location_fullsize())

  # type + type index
  types <- c("body", "title", "ctrTitle", "subTitle", "dt", "ftr", "sldNum")
  for (type in types) {
    loc <- ph_location_type(type, 2)
    expect_equal(resolve_location(paste0(type, "[2]")), loc)
    expect_equal(resolve_location(paste0(type, " [2]")), loc)
    expect_equal(resolve_location(paste0("  ", type, "  [2]  ")), loc)
    expect_equal(resolve_location(paste0("  ", type, "  [ 2  ]  ")), loc)
  }

  ## character: label if not keyword or type
  expect_equal(resolve_location("< a label>"), ph_location_label("< a label>"))
  expect_equal(
    resolve_location(c(xxx = "< a label>")),
    ph_location_label("< a label>")
  )
  expect_equal(resolve_location("body[]"), ph_location_label("body[]"))
  expect_equal(resolve_location("left "), ph_location_label("left "))
  expect_equal(resolve_location(" left"), ph_location_label(" left"))
  expect_equal(resolve_location(" left "), ph_location_label(" left "))

  # full example
  expect_no_error({
    x <- read_pptx()
    x <- add_slide(x, "Title Slide")
    x <- ph_with(x, "A title", "Title 1") # label
    x <- ph_with(x, "A subtitle", 3) # id
    x <- ph_with(x, "A date", "dt[1]") # type + index
    x <- ph_with(x, "A left text", "left") # keyword
    x <- ph_with(x, "More content", c(5, .5, 9, 2)) # numeric vector
  })
})




test_that("ph_with, phs_with, add_slide equality", {
  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content")

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content")
  x2 <- phs_with(x2) # empty case, no changes to rpptx

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)

  expect_equal(st_1, st_2)

  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content")
  x1 <- ph_with(x1, "A title", "Title 1")
  x1 <- ph_with(x1, "Jan. 26, 2025", "dt")
  x1 <- ph_with(x1, "Body text", "body [2]")
  x1 <- ph_with(x1, "Footer", 6)

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content")
  x2 <- phs_with(
    x2,
    `Title 1` = "A title",
    dt = "Jan. 26, 2025",
    `body[2]` = "Body text",
    `6` = "Footer"
  )

  x3 <- read_pptx()
  x3 <- add_slide(
    x3,
    "Two Content",
    `Title 1` = "A title",
    dt = "Jan. 26, 2025",
    `body[2]` = "Body text",
    `6` = "Footer"
  )

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)
  st_3 <- get_shapetrees_string(x3)

  expect_equal(st_1, st_2)
  expect_equal(st_1, st_3)
})


test_that("phs_with .slide_idx arg", {
  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content")
  x1 <- ph_with(x1, "Footer", "ftr")
  x1 <- ph_with(x1, "Jan. 26, 2025", "dt")
  x1 <- add_slide(x1, "Two Content")
  x1 <- ph_with(x1, "Footer", "ftr")
  x1 <- ph_with(x1, "Jan. 26, 2025", "dt")

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content")
  x2 <- add_slide(x2, "Two Content")
  x2 <- phs_with(x2, ftr = "Footer", dt = "Jan. 26, 2025", .slide_idx = 1:2)

  x3 <- read_pptx()
  x3 <- add_slide(x3, "Two Content")
  x3 <- add_slide(x3, "Two Content")
  x3 <- phs_with(x3, ftr = "Footer", dt = "Jan. 26, 2025", .slide_idx = "all")

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)
  st_3 <- get_shapetrees_string(x3)

  expect_equal(st_1, st_2)
  expect_equal(st_1, st_3)
})


test_that("add_slide, phs_with: ... and .dots", {
  opts <- options(testthat.use_colours = FALSE)
  on.exit(opts)

  # errors
  x0 <- read_pptx()
  x0 <- add_slide(x0, "Two Content")
  expect_error(
    phs_with(x0, "A title", dt = "Jan. 26, 2025"),
    regexp = "Missing key in `...`"
  )
  expect_error(
    phs_with(x0, .dots = list("A title", dt = "Jan. 26, 2025")),
    regexp = "Missing names in `.dots`"
  )
  expect_error(
    add_slide(
      x0,
      "Two Content",
      master = "Office Theme",
      "A title",
      dt = "Jan. 26, 2025"
    ),
    regexp = "Missing key in `...`"
  )
  expect_error(
    add_slide(x0, "Two Content", .dots = list("A title", dt = "Jan. 26, 2025")),
    regexp = "Missing names in `.dots`"
  )

  # functionality
  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content")
  expect_no_error(phs_with(x1)) # empty case
  x1 <- phs_with(
    x1,
    `Title 1` = "A title",
    dt = "Jan. 26, 2025",
    `body[2]` = "Body text",
    `6` = "Footer"
  )

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content")
  x2 <- phs_with(
    x2,
    .dots = list(
      `Title 1` = "A title",
      dt = "Jan. 26, 2025",
      `body[2]` = "Body text",
      `6` = "Footer"
    )
  )

  x3 <- read_pptx()
  x3 <- add_slide(x3, "Two Content")
  x3 <- phs_with(
    x3,
    `Title 1` = "A title",
    dt = "Jan. 26, 2025",
    .dots = list(`body[2]` = "Body text", `6` = "Footer")
  )

  x4 <- read_pptx()
  x4 <- add_slide(
    x4,
    "Two Content",
    `Title 1` = "A title",
    dt = "Jan. 26, 2025",
    .dots = list(`body[2]` = "Body text", `6` = "Footer")
  )

  x5 <- read_pptx()
  x5 <- add_slide(
    x5,
    "Two Content",
    .dots = list(
      `Title 1` = "A title",
      dt = "Jan. 26, 2025",
      `body[2]` = "Body text",
      `6` = "Footer"
    )
  )

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)
  st_3 <- get_shapetrees_string(x3)
  st_4 <- get_shapetrees_string(x4)
  st_5 <- get_shapetrees_string(x5)

  expect_equal(st_1, st_2)
  expect_equal(st_1, st_3)
  expect_equal(st_1, st_4)
  expect_equal(st_1, st_5)
})


test_that("ph_with: ... and .dots", {
  # check if ... and .dots are processed as expected by each ph_with.* method
  require(ggplot2)

  loc <- "body[1]"
  layout <- "Two Content"
  bg <- "#ff000015"
  dots <- list(ln = "red", rot = 3, geom = "round2SameRect")

  fp <- fpar(ftext("text for first page header", prop = fp_text_lite(color = "red", font.size = 40, bold = TRUE)))
  bl <- block_list(fp, fp)
  ul <- unordered_list(
    level_list = c(1, 2, 1),
    str_list = c("Level1", "Level2", "Level1"),
    style = list(
      fp_text(color = "red", font.size = 0),
      fp_text(color = "pink", font.size = 0),
      fp_text(color = "orange", font.size = 0)
    )
  )

  file_png <- test_path("img/dog_1.png")
  ext_img <- external_img(file_png, width = 4, height = 3)

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    theme_minimal(base_size = 15) +
    theme(
      plot.background  = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    )

  x <- read_pptx()
  x <- add_slide(x, layout, title = "ph_with.character()")
  x <- ph_with(x, location = loc, "The body", bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.numeric()")
  x <- ph_with(x, location = loc, 1:3, bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.numeric()")
  x <- ph_with(x, location = loc, as.double(1:3), bg = bg, .dots = dots, nsmall = 3, decimal.mark = ",")
  x <- add_slide(x, layout, title = "ph_with.factor()")
  x <- ph_with(x, location = loc, factor(letters[1:3]), bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.Date()")
  x <- ph_with(x, location = loc, Sys.Date(), date_format = "%D", bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.fpar()")
  x <- ph_with(x, location = loc, fp, bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.block_list()")
  x <- ph_with(x, location = loc, bl, bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.unordered_list()")
  x <- ph_with(x, location = loc, ul, bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.empty_content()")
  x <- ph_with(x, location = loc, empty_content(), bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.external_img()") # no: geom
  x <- ph_with(x, location = loc, ext_img, use_loc_size = FALSE, bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.plot_instr()") # no: geom
  x <- ph_with(x, location = loc, plot_instr(code = barplot(1:5, col = 2:6)), bg = bg, .dots = dots)
  x <- add_slide(x, layout, title = "ph_with.gg()") # no: geom
  x <- ph_with(x, location = loc, p, bg = bg, .dots = dots, scaling = 2)
  x <- add_slide(x, layout, title = "ph_with.data.frame()") # no: geom, ln, bg, rotatation
  x <- ph_with(x, location = loc, mtcars[1:3, ], bg = bg, .dots = dots, width = 9)

  pptx_expected <- test_path("docs_dir/expected_ph_with_dots_test.pptx")
  # print(x, pptx_expected) # Save expected file. When replacing it, make sure to check manually
  check <- is_identical_shapetree(x, pptx_expected)
  expect_true(check)


  # some methods pass ... on to two functions.
  # ph_with.numeric() passes ... also to format_fun.
  # Check if that works too by creating the same result in two ways.
  val <- 1.1
  x1 <- read_pptx()
  x1 <- add_slide(x1, layout, title = "ph_with.numeric()")
  x1 <- ph_with(x1, val, location = "body[1]", .dots = dots, decimal.mark = ",")
  x1 <- ph_with(x1, val, location = "body[2]", .dots = dots, nsmall = 3)

  x2 <- read_pptx()
  x2 <- add_slide(x2, layout, title = "ph_with.numeric()")
  x2 <- ph_with(x2, format(val, decimal.mark = ","), location = "body[1]", .dots = dots)
  x2 <- ph_with(x2, format(val, nsmall = 3), location = "body[2]", .dots = dots)

  check <- is_identical_shapetree(x1, x2)
  expect_true(check)
})
