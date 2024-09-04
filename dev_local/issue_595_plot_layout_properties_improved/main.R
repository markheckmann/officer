devtools::load_all()

url <- "https://github.com/user-attachments/files/16829213/three_identical_masters.pptx"
file <- tempfile(fileext = ".pptx")
download.file(url, file)
x <- read_pptx(file)
df <- layout_properties(x)
df[, c("master_name", "name", "type", "ph_label", "ph")]


# sorting


file <- system.file("doc_examples", "three_identical_masters.pptx", package = "officer")
x <- read_pptx(file)
layout_properties(x)
# x <- read_pptx()
layout <- "Title and Content"
master <- "Master_2"

layout_properties(x)#, layout = layout, master = master)
layout_summary(x)

# works
la <- get_layout(x, layout = layout, master = master)
la <- get_layout(x, layout = 9)

# get_xfrm_data() does not list all placeholders across master
# here: slide has same placeholders in each master
data <- x$slideLayouts$get_xfrm_data()


plot_layout_properties(x, "Title and Content", "Master_2")

plot_layout_properties(x, 14)  # fails

plot_layout_properties(x, 3, "Master_1")
plot_layout_properties(x, 3, "Master_2")


# order of layouts is not the same as in PPTX presentation
file <- system.file("doc_examples", "three_masters.pptx", package = "officer")
x <- read_pptx(file)
dat <- layout_properties(x, layout = la$layout, master = la$master)

# selection fails for same layout names across multiple masters
file <- system.file("doc_examples", "three_masters.pptx", package = "officer")
x <- read_pptx(file)
layout_ <- "Title and Content"
x$slideLayouts$get_metadata() |> subset(name == layout_)

plot_layout_properties(x, layout = layout_, master = "Master_1")
plot_layout_properties(x, layout = layout_, master = "Master_2")
plot_layout_properties(x, layout = layout_, master = "Master_3")


# layout_summary: layout order different from layout order in PPTX

x <- read_pptx(file)
layout_summary(x)
df <- x$slideLayouts$get_metadata()

grep("\\d+", df$filename)


# slideLayout1.xml -> 1
# slideLayout12.xml -> 12
# slideMaster1.xml -> 1
get_file_index <- function(file) {
  sub(pattern = ".+?(\\d+).xml$", replacement = "\\1", x = file, ignore.case = TRUE) |> as.numeric()
}

files <- c("slideLayout1.xml", "slideLayout3.xml", "slideLayout10.xml")
get_file_index(files)

df[order(get_file_index(df$master_file), get_file_index(df$filename)), , drop=FALSE]


# order of layouts is not the same as in PPTX presentation
file <- system.file("doc_examples", "many_layouts.pptx", package = "officer")
x <- read_pptx(file)
layout_summary(x)

dat <- layout_properties(x, layout = la$layout, master = la$master)


url <- "https://github.com/user-attachments/files/16825504/many_layouts.pptx"
file <- tempfile(fileext = ".pptx")
download.file(url, file)
x <- read_pptx(file)
layout_summary(x)

