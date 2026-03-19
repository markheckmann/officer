# create two presentations from the same template and join them
p1 <- read_pptx()
p1 <- add_slide(p1, layout = "Title Slide", ctrTitle = "Presentation 1")

p2 <- read_pptx()
p2 <- add_slide(p2, layout = "Title and Content", title = "Presentation 2")

pp <- pptx_concat(p1, p2) # append p1 to p2, same as c(p1, p2)
length(pp)

# print(pp, preview = TRUE)
