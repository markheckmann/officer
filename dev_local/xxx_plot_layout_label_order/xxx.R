devtools::load_all()
x <- read_pptx("dev_local/xxx_plot_layout_label_order/four_content (1).pptx")
x <- x |> add_slide("Four Content")
plot_layout_properties(x, "Four Content", labels = F)




url <- "https://github.com/user-attachments/files/16922311/four_content.pptx"
file <- tempfile(fileext = ".pptx")
download.file(url, file)

x <- read_pptx(file)

library(officer)
x <- read_pptx("dev_local/xxx_plot_layout_label_order/four_content (1).pptx")
x <- x |> add_slide("Four Content")

layout_properties(x, "Four Content")

plot_layout_properties(x, "Four Content", labels = F)

# assignment

for (id in 1:4) {
  x <- ph_with(x, paste("type = body, id =", id), ph_location_type("body", id = id))
}
print(x, target = "dev_local/xxx_plot_layout_label_order/four_content_filled.pptx")
plot_layout_properties(x, "Four Content", labels = F)



# The labeling in plot_layout_properties is incorrect
#
# **Analysis**
#
# The reason appears to lie here: The labels are reordered without reordering
# the other rows of dat, resulting in a mismatch between ph and label
#
# labels <- dat$type[order(as.integer(dat$id))]
# rle_ <- rle(labels)
# labels <- sprintf("type: '%s' - id: %.0f", labels, unlist(lapply(rle_$lengths, seq_len)))
#
# **Suggestion**
#
# Since PR the output of

