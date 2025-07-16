file_format <- "png"
dpi <- 300L
device <- ifelse(file_format == "pdf", cairo_pdf, file_format)
font <- "SourceSansPro"
