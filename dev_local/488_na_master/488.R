library(tidyverse)
library(officer)

# dir_layout -> get_xfrm_data -> xfrmize(self$xfrm(), master_xfrm)

# master_xfrm passed to xfrmize(self$xfrm(), master_xfrm) in dir_layout initializer
# has zero rows
# line 50 read_pptx calls dir_layout(package_dir, )
#
# obj$slideLayouts <- dir_layout$new( package_dir,
#    master_metadata = obj$masterLayouts$get_metadata(),
#    master_xfrm = obj$masterLayouts$xfrm() )
#

# XML of /ppt/slideMasters/slideMaster1.xml"
# p:cSld -> p:spTree>

# There are no shapes, piuc or images on the slide master ("/ppt/slideMasters/slideMaster1.xml")

# <p:sldMaster ...>
#   <p:cSld>
#   ...
# <p:spTree>
#   <p:nvGrpSpPr>
#   <p:cNvPr id="1" name=""/>
#     ...
#   </p:nvGrpSpPr>
#     <p:grpSpPr>
#     ...
#   </p:grpSpPr>
#     </p:spTree>
#     <p:extLst>
#     ...
#   </p:extLst>
#     </p:cSld>

# master_xfrm = obj$masterLayouts$xfrm() searches
# The slide_layout xfrm() method searches for all these nodes defined as xPath
# to "p:cSld/p:spTree/*[self::p:cxnSp or self::p:sp or self::p:graphicFrame or self::p:grpSp or self::p:pic]"
# i.e. alls shapes, pics etc in all shapetrees.

# xml_children(xml_find_all( self$get(), "p:cSld/p:spTree")) |> xml2::xml_structure()

xfrm = function(){
  nodeset <- xml_find_all( self$get(), as_xpath_content_sel("p:cSld/p:spTree/") )
  read_xfrm(nodeset, self$file_name(), self$name())
}

there is oinly one shaüpetree with none of these

cat(as.character(xml_children(xml_find_all( self$get(), "p:cSld/p:spTree"))))

x <- officer::read_pptx("dev_local/488_na_master/Slide.Template_R.pptx")
df <- x %>%
  officer::layout_properties() %>% select(c('master_name','name', 'type', 'id', 'ph_label'))

x <- x %>%
  remove_slide(index = 1) %>%
  add_slide(master='Custom Design', layout='Style_leftright_a') %>%
  ph_with(value= 'Corridors', location = ph_location_label(ph_label = "Big Title"))

file <- "dev_local/488_na_master/Custom Design.pptx"
print(x, file)
file.show(file)
