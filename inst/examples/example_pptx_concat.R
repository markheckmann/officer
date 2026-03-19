# create presentations from the same template and join them
p1 <- read_pptx()
p1 <- add_slide(p1, layout = "Title Slide", ctrTitle = "Presentation 1")

p2 <- read_pptx()
p2 <- add_slide(p2, layout = "Title and Content", title = "Presentation 2")

p3 <- read_pptx()
p3 <- add_slide(p3, layout = "Two Content", title = "Presentation 3")

# concatenate: creates new object. p1, p2, and p3 are not modified
pp <- pptx_concat(p1, p2, p3)
length(pp) # 2
length(p1)  # 1, unchanged

# same using c()
pp <- c(p1, p2, p3)
length(pp) # 2
