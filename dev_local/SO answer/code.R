library(officer)

file <- system.file("doc_examples", "ph_dupes.pptx", package = "officer")
x <- read_pptx(file)

layout_properties(x, "2-dupes")[, 3:6] # relevant columns only

plot_layout_properties(x, "2-dupes")

layout_dedupe_ph_labels(x, action = "rename")
layout_properties(x, "2-dupes")[, 3:6]

layout_rename_ph_labels(x, "2-dupes", "8" = "New 1", "10" = "New 2") # using id = value pairs
layout_rename_ph_labels(x, "2-dupes", id = c(8, 10)) <- c("New 1", "New 2") # ids + rhs assignment
layout_properties(x, "2-dupes")[, 3:6]

x <- x |>
  add_slide("2-dupes", "Master1") |>
  ph_with("Text 1", ph_location_id(id = 8)) |>
  ph_with("Text 2", ph_location_id(id = 10))

x <- x |>
  add_slide("2-dupes", "Master1") |>
  ph_with("Text New 1", ph_location_type(type = "body", type_idx = 1)) |>
  ph_with("Text New 1", ph_location_type(type = "body", type_idx = 2))
