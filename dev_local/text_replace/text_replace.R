
# replacing text in officer using the `*_replace_all_text()` functions only works properly
# if the text to be replaced is conatined in the same run. For programatically generated docs
# this is usually not an issue. However, if the goal is to replace text in a doc that was edited
# by hand, it is often the case that the to be replaced is distribuuted across several
# runs. There a tricks to ensure that the text you insert if contained in a single run using
# `Paste Special -> Unformatted Text` (see https://stackoverflow.com/questions/51047772/doesnt-work-body-replace-all-text-method-in-package-officer).
# Yet, this is tedious and does not always work as expected.
#
# Related issues: https://github.com/davidgohel/officer/issues/439
#
# In the following, we fix this problem.
# One design quuestion is, what should happen, if the replacement is longer or shorter than the text to be replaced.
# We chose to take the same approach as in the case of a bookmark. When replacing a bookmark, the format of the the
# first character of a bookmark appears to be used for thr entire replacement.
#
# One could try to capatilze on the body_replace_text_at_bkm by insering a temporary bookmark at the
# text to be replaced. However, this might be difficult, as one my want to start a replacemtn in the middle
# of a run. Hence, one would need to split the run before inserting a <w:bookmarkStart> node. This appears
# to be quite difficult to do.
#
# It appears easier, to identify the run where the search text appears and 1) remove the text to be replaced from this
# and consecutive runs, 2) insert the text to be replaced at the position wherte the search text started.
#
# Code below translated from: https://github.com/markheckmann/officer.pptx/blob/main/R/text_replace.R

library(officer)

file_in <- "dev_local/text_replace/sample.docx"
# mh::file_open(file_in)
file_out <- stringr::str_replace(file_in, ".docx", "_OUT.docx")

x <- read_docx(file_in)
officer::docx_bookmarks(x)

bkm_start <- xml_find_all(x$doc_obj$get(), "//w:bookmarkStart[@w:name]")
blm_end <- xml_find_all(x$doc_obj$get(), "//w:bookmarkEnd[@w:name]")


x <- officer::body_replace_text_at_bkm(x, "Tag1a", "NEW TEXT")
x <- officer::body_replace_text_at_bkm(x, "Tag1b", "SOME MORE NEW TEXT")
x <- officer::body_replace_text_at_bkm(x, "Tag2a", "NEW TEXT 2")
x <- officer::body_replace_text_at_bkm(x, "Tag2b", "SOME MORE NEW TEXT 2")

# img_file <- file.path( R.home("doc"), "html", "logo.jpg" )
# ext_img <- external_img(src = img_file, guess_size = TRUE)
# x <- officer::body_replace_img_at_bkm(x, "Img1", ext_img)

img_paragraph_1 <- fpar(
  external_img(img_file, guess_size = T),
  fp_p = fp_par(text.align = "left")
)
d <- docx_dim(x)
w <- d$page["width"] - d$margins["left"] - d$margins["right"]
x <- x %>%
  cursor_bookmark(id = "Img1") %>%
  body_add_fpar(img_paragraph_1, pos = "on")

img_file2 <- "dev_local/text_replace/sample.png"
img <- png::readPNG(img_file2)
h <- w * nrow(img) / ncol(img)
img_paragraph_2 <- fpar(
  external_img(img_file2, width = w, height = h),
  fp_p = fp_par(text.align = "center")
)
x <- x %>%
  cursor_bookmark(id = "Img2") %>%
  body_add_fpar(img_paragraph_2, pos = "on")


print(x, target = file_out)
mh::file_open(file_out)



## approach replace at first occurence ----

doc_ <- xml_find_all(x$doc_obj$get(), "//w:bookmarkStart[@w:name]")



string <- "The quick brown fox jumps over the lazy fox and another fox"
pattern <- "fox"
str_locate_start_end(string, pattern)
stringr::str_locate_all(string, pattern)

# get texts from all runs in paragraph and concatenate
pattern <- "<Tag1>"
doc <- x$doc_obj$get()
pars <- find_paragraphs(doc, pattern)
par <- pars[[1]]
runs <- xml_get_runs_in_par(par)
runs_text <- runs |> xml2::xml_text() |> stats::setNames(paste0("r_", seq_along(runs)))
text_old <- paste0(runs_text, collapse = "")
pattern_loc <- str_locate_start_end(text_old, pattern, fixed = TRUE)|> as.list() |>
  stats::setNames(c("from", "to"))

# split up text into chars
df_runs <- data.frame(run_idx = seq_along(runs_text), text = runs_text)
lapply(df_runs$text, \(x) strsplit(x, "") |> un)




tidyr::unnest(original)
chars_new <- strsplit(replacement, "") |> unlist()
n_new <- length(chars_new)
if (replacement == "") { # handle edge case of zero length replacement
  chars_new <- ""
  n_new <- 1 # keep this row
}


