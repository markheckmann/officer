
devtools::load_all()

# plot layout with type label, (type_idx), id,

x <- read_pptx()
plot_layout_properties(x, "Title Slide", type =T, id = T)

url <- "https://github.com/user-attachments/files/16829213/three_identical_masters.pptx"
file <- tempfile(fileext = ".pptx")
download.file(url, file)
x <- read_pptx(file)
df <- layout_properties(x)
df[, c("master_name", "name", "type", "ph_label", "ph")]
