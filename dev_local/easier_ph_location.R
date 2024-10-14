x <- read_pptx()

x <- add_slide(x, "Title Slide")

# current version
x <- ph_with(x, "A title", location = ph_location_label("Title 1"))
x <- ph_with(x, "A subtitle", location = ph_location_id(3))
x <- ph_with(x, "A date", location = ph_location_type("dt", 1))
x <- ph_with(x, "A left text", location = ph_location_left())
x <- ph_with(x, "A right text", location = ph_location_right())
x <- ph_with(x, "Full size", location = ph_location_fullsize())

x <- ph_with(x, "A title", location = "Title 1") # use ph_label
x <- ph_with(x, "A subtitle", location = 3) # use ph id
x <- ph_with(x, "A date", location = "dt [1]")  # use type[type_idx]
x <- ph_with(x, "A left text", location = "left")
x <- ph_with(x, "A right text", location = "right")
x <- ph_with(x, "Full size", location = "fullsize")
