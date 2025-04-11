theme_ski <- function(...) {
  th <- theme_bw(...) +
    theme(
      text = element_text(size = 16),
      axis.title.x = element_text(vjust = 0.5, size = 16),
      axis.title.y = element_text(vjust = 0.5, size = 16),
      plot.title = element_text(
        size = rel(1.5), hjust = 0.5, # face = "bold",
        margin = margin(t = 10, b = 20, unit = "pt")
      )
    ) +
    theme(legend.position = "none")
  return(th)
}
