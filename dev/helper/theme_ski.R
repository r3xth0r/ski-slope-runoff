ski_col <- "#33ccff"
ref_col <- "#A27146"

theme_ski <- function(...) {
  th <- theme_bw(...) +
    theme(
      text = element_text(size = 16),
      axis.title.x = element_text(vjust = 0.5, size = 16),
      axis.title.y = element_text(vjust = 0.5, size = 16),
      plot.title = element_text(
        size = rel(1.5), hjust = 0.5,
        margin = margin(t = 10, b = 20, unit = "pt")
      ),
      legend.position = "none"
    )
  return(th)
}

theme_pdp <- function(...) {
  th <- theme_bw(...) +
    theme(
      text = element_text(size = 9),
      axis.title.x = element_text(vjust = 0.5, size = 9),
      axis.title.y = element_text(vjust = 0.5, size = 9),
      plot.title = element_text(
        size = rel(1), hjust = 0.5,
        margin = margin(t = 10, b = 20, unit = "pt")
      ),
      legend.position = "none",
      panel.grid.major = element_line(size = 0.3),
      panel.grid.minor = element_line(size = 0.15)
    )
  return(th)
}
