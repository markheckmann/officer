###
### 606 suggestions 4: add ph_location_type()
###


x <- read_pptx()
x <- x |> add_slide("Comparison")
plot_layout_properties(x, "Comparison")
x <- ph_with(x, "My Ttitle", location = ph_location_id(id = 2)) # title ph
x <- ph_with(x, "Text 2", location = ph_location_id(id = 3))
x <- ph_with(x, "Text 4", location = ph_location_id(id = 5))
x <- ph_with(x, 1, location = ph_location_id(id = 9))
file <- tempfile(fileext = ".pptx")
print(x, file)
file.show(file)


###
### 606 suggestions 1-3
###

# type indexing of id and type_idx is different

x <- read_pptx()
# x |> plot_layout_properties("Comparison", labels=F)

x <- x |> add_slide("Comparison")

x |> ph_with("NEW: type_idx", ph_location_label("Title 1"))
for (type_idx in 1:4) {
  x |> ph_with(paste("type_idx:", type_idx), ph_location_type(type_idx = type_idx))
}

x <- x |> add_slide("Comparison")
x |> ph_with("OLD: id", ph_location_label("Title 1"))
for (id in 1:4) {
  x |> ph_with(paste("id:", id), ph_location_type(id = id))
}

file <- tempfile(fileext = ".pptx")
print(x, file)
file.show(file)



x <- read_pptx()
x <- x |> add_slide("Comparison")
x |> ph_with("Title 1", ph_location_label("Title 1"))
x |> ph_with("Text Placeholder 2", ph_location_label("Text Placeholder 2"))
x |> ph_with("body", ph_location_type("body"))
file <- tempfile(fileext = ".pptx")
print(x, file)
file.show(file)



# PR 607
library(officer)
x <- read_pptx()

layout_properties(x, "Comparison")

x |> plot_layout_properties("Comparison", labels = T)

x <- x |> add_slide("Comparison")




#
# #
# #
# .id_to_type_idx <- function(x, layout, master, type, id) {
#   props <- layout_properties(x, layout = layout, master = master)
#   props <- props[props$type %in% type, , drop = FALSE]
#   nr <- nrow(props)
#   if (!id %in% 1L:nr) {
#     cli::cli_abort(
#       c(
#         "{.arg id} is out of range.",
#         "x" = "Must be between {.val {1L}} and {.val {nr}} for ph type {.val {type}}.",
#         "i" = cli::col_grey("see {.code layout_properties(x, '{layout}', '{master}')} for all phs with type '{type}'")
#       ),
#       call = NULL
#     )
#   }
#   props <- props[id, , drop = FALSE]
#   props$type_idx
# }
#
#
# # # to avoid breaking change, the deprecated id is passed along.
# # As type_idx uses a different index order than id, so this is necessary until the id arg
# # is removed. Note that id in get_ph_loc
# if (!is.null(x$id) && is.null(x$type_idx)) {
#   x$type_idx <- .id_to_type_idx(doc, layout = layout, master = master, type = x$type, id = x$id)
#   x$id <- NULL
# }
