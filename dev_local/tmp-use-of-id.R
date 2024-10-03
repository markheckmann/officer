#
# use of id in officer
#
# Currently, the notion of id in phs is slightly ambiguous. There are at least two places
# where id is used with different meanings.
#
# 1. layout_properties(): returns an id column, with a unique id for each placeholder.
#  This id is extracted from the p:cNvPr node's id attribute. The placeholder node and p:cNvPr node
#  both belong to a common ancestor. This can be a shape (most common), group shape, connection
#  shape, graphic frame or a pic node. According to the OOXML specs (ECMA-376, Part 1, p. 2570),
#  id must be unique  on a single slide to be conformant. (NB: id = 1 is reserved for the main
#  slides background shape, so added objects start at id=2). Hence, id is unique and refers to
#  exactly one node (which may a ph) according to the by OOCML standards.
#
# 2. ph_location_type(): the argument id used for disambiguation between placeholders of same type.
#   Here, the id is a generated sequential number along phs of the same type. It starts at 1. The id
#   is only unique in combination with a certain type. The id has no further meaning in the context
#   of the OOXML specs.
#
# For disambiguation, I would suggest to make the following changes:
#
# 1. ph_location_type(): changing the arg name from id to index to make clear its a running
#    index and not the id used in layout_properties().
#
# 2. Introduce a new function ph_location_id(): This one takes the unique id as returned by
#    layout_properties() to reference a placeholder.
#


devtools::load_all()
#library(officer)
x <- read_pptx()
x <- x |> add_slide()
x |> ph_with("test", ph_location_label("Titel 1"))

x <- x |> add_slide()
x |> ph_with("test", ph_location_type("title"))


plot_layout_properties(x, "Two Content", labels = F)
layout_properties(x, "Two Content")


library(officer)

x <- read_pptx()
x <- x |> add_slide("Two Content")
x |> ph_with(c("first body?"), ph_location_type("body"))
f <- print(x, tempfile(fileext = ".pptx"))
file_open(f)


x <- read_pptx()
x <- x |> add_slide("Two Content")
x |> ph_with("unknown id", ph_location_type("body", id = 3))
print(x, "corrupted.pptx")


x <- read_pptx()
x <- x |> add_slide("Two Content")
x |> ph_with("unknown id", ph_location_type("title", id = 2))
print(x, "corrupted.pptx")

x <- read_pptx()
x <- x |> add_slide("Two Content")
x |> ph_with(c("left", "id = 1"), ph_location_type("body", id = 3))
print(x, "corrupted.pptx")



