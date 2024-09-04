devtools::load_all()
source("dev_local/text_replace/replace_funs.R")

library(dplyr)
library(tidyr)

file_in <- "dev_local/text_replace/sample.docx"
file_out <- stringr::str_replace(file_in, ".docx", "_OUT.docx")

# mh::file_open(file_in)
x <- read_docx(file_in)

pattern <- "Tag1"
replacement <- "XXXXX"

# todo at: cursor context only or whole doc

doc <- x$doc_obj$get()
paras <- find_paragraphs(doc, "Tag")
for (p in paras) {
  xml_replace_all_text_in_par(p, pattern = pattern, replacement = replacement)
}

print(x, target = file_out)
mh::file_open(file_out)
