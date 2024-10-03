library(officer)

pr <- read_pptx()
pr <- add_slide(pr, layout = "Blank", master = "Office Theme")
pr <- ph_with(x=pr, value = external_img(src = 'dev_local/613_adding_pdf/slide1.pdf'), location = ph_location_fullsize() )
# pr <- ph_with(x=pr, value = external_img(src = 'dev_local/613_adding_pdf/slide1.jpg'), location = ph_location_fullsize() )

f <- tempfile(fileext = ".pptx")
print(pr, f)
file.show(f)
