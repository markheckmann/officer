devtools::load_all()

# EXAMPLES ------

x <- read_pptx()

# Returns ph_labels by default. Same order as in layout_properties().
layout_rename_ph_labels(x, "Comparison")
layout_properties(x, "Comparison")[, c("id", "ph_label")]

# Hint: run `plot_layout_properties(x, "Comparison")` now and then to see what has changed.

# rename using key-value pairs: 'old label' = 'new label' or 'id' = 'new label'
layout_rename_ph_labels(x, "Comparison", "Title 1" = "LABEL MATCHED") # label matching
layout_rename_ph_labels(x, "Comparison", "3" = "ID MATCHED") # id matching
plot_layout_properties(x, "Comparison")

# rename via assignment style
layout_rename_ph_labels(x, "Comparison") <- LETTERS[1:8]
layout_rename_ph_labels(x, "Comparison")[1:3] <- paste("CHANGED", 1:3)

layout_rename_ph_labels(x, "Comparison", id = c(2,4)) <- paste("ID =", c(2,4))

labels <- layout_rename_ph_labels(x, "Comparison")
layout_rename_ph_labels(x, "Comparison") <- tolower(labels)

# layout_rename_ph_labels(x, "Comparison")[-1] <- 1
# plot_layout_properties(x, "Comparison")
