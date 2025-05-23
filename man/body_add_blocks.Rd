% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docx_add.R
\name{body_add_blocks}
\alias{body_add_blocks}
\title{Add a list of blocks into a 'Word' document}
\usage{
body_add_blocks(x, blocks, pos = "after")
}
\arguments{
\item{x}{an rdocx object}

\item{blocks}{set of blocks to be used as footnote content returned by
function \code{\link[=block_list]{block_list()}}.}

\item{pos}{where to add the new element relative to the cursor,
one of "after", "before", "on".}
}
\description{
add a list of blocks produced by \code{\link[=block_list]{block_list()}} into
into an rdocx object.
}
\examples{
library(officer)

img.file <- file.path(R.home("doc"), "html", "logo.jpg")

bl <- block_list(
  fpar(ftext("hello", shortcuts$fp_bold(color = "red"))),
  fpar(
    ftext("hello world", shortcuts$fp_bold()),
    external_img(src = img.file, height = 1.06, width = 1.39),
    fp_p = fp_par(text.align = "center")
  )
)

doc_1 <- read_docx()
doc_1 <- body_add_blocks(doc_1, blocks = bl)
print(doc_1, target = tempfile(fileext = ".docx"))
}
\seealso{
Other functions for adding content: 
\code{\link{body_add_break}()},
\code{\link{body_add_caption}()},
\code{\link{body_add_docx}()},
\code{\link{body_add_fpar}()},
\code{\link{body_add_gg}()},
\code{\link{body_add_img}()},
\code{\link{body_add_par}()},
\code{\link{body_add_plot}()},
\code{\link{body_add_table}()},
\code{\link{body_add_toc}()},
\code{\link{body_append_start_context}()}
}
\concept{functions for adding content}
