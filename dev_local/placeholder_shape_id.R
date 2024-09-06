# tmp
devtools::load_all()
x <- read_pptx()

print(x, "dev_local/tmp.pptx")
x$slideLayouts$get_xfrm_data()
