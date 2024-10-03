source("dev_local/issue_595_plot_layout_properties_improved/layout_helpers.R")

x <- read_pptx()

get_layout(x, layout = "Comparison")
get_layout(x, 1)

