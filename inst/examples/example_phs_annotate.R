file_pptx <- tempfile(fileext = ".pptx")

x <- read_pptx()

x <- add_slide(x, "Title Slide", ctrTitle = "My Title")
x <- phs_annotate(x) # annotate all phs on current slide

x <- add_slide(x, "Title and Content", body = plot_instr(plot(0:10)))
x <- phs_annotate(x, "dt", "Title 1") # annotate phs with tyope dt and label "Title 1

x <- add_slide(x, "Title and Content")
x <- add_slide(x, "Title and Content")
x <- phs_annotate(x, "sldNum", "body", .slide_idx = 3:4, .font_color = NA) # default color and size

print(x, file_pptx)
# browseUR(file_pptx)  # may not work on all systems

x <- annotate_layouts()
print(x, file_pptx)
# browseURL(file_pptx)  # may not work on all systems
