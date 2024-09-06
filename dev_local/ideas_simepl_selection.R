# Ideas

library(officer)

x <- read_pptx()

x |> add_slide("Title Slide", "Office Theme")

# equally
x |> add_slide("Title Slide")
x |> add_slide("title sl")
x |> add_slide(1)


# visualize placeholders on current slide
x |> plot_layout_properties("Title Slide")#, "Office Theme")
# without parameters, plot_layout_properties() plots layout of the current slide

# also, it may be helpful to add the ids (column id from layout_properties())
# to the layout plot. This mykes it easier to find the cirrect id, for example,
# using id arg in ph_location_type()
x |> plot_layout_properties("Title Slide", ids = TRUE)


# placeholders

# Placeholdes can currently be selected on the basis of type and label
# ph_location_type() has an id arg for disambiguation in cae there are 2
# phs of the same type.
# The id refers to the placeholders shape id. This is always unique on a slide,
# as per OOXML specficiation

ph_location_id(x, id = 1)

ph_location_any(x, id = 1, type = "", label = "", master = "")


# The function layout_properties() alsways
layout_properties(x, layout = "Title Slide", master = NULL)


x <- read_pptx() |> add_slide("Title Slide", "Office Theme")
x |> plot_layout_properties("Title Slide")
x |> plot_layout_properties()

