# Annotate all layouts in officer's default presentation
\dontrun{
annotate_base() # creates local file `annotated_layouts.pptx`
}

# without file creation
x <- annotate_base(output_file = NULL)
\dontrun{
print(x, preview = TRUE)}

# annotate selected layouts only (if there are too many)
x <- annotate_base(layouts = c("Title Slide", "Two Content"), output_file = NULL)
x <- annotate_base(layouts = c(1,4), output_file = NULL)  # same using layout index
\dontrun{
print(x, preview = TRUE)}

# save annotated layouts directly to file
file_out <- tempfile(fileext = ".pptx")
annotate_base(output_file = file_out)
\dontrun{
file_open(file_out)}
