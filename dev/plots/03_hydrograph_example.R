# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Conceptual hydrograph (Fig. 03)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

suppressPackageStartupMessages({
  library("tidyverse")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

# Load data ----
dat <- read_csv("dat/interim/plt_irrigation_example.csv", show_col_types = FALSE)

p <- ggplot(data = dat, aes(x = t, y = AK, group = type)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15, color = "black", formula = "y ~ x") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", linewidth = 0.5) +
  geom_segment(aes(x = 50, xend = 60, y = 0.77, yend = 0.77), linetype = "solid", color = "red", linewidth = 0.7) +
  geom_segment(aes(x = 50, y = 0.70, xend = 50, yend = 1), linewidth = 0.5, color = "black", linetype = "dotted") +
  geom_segment(aes(x = 60, y = 0.70, xend = 60, yend = 1), linewidth = 0.5, color = "black", linetype = "dotted") +
  geom_segment(aes(x = 32, y = 0.92, xend = 29, yend = 0.95), linewidth = 0.2, arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 36, y = 0.90, label = expression(italic(C[peak])), size = 4) +
  geom_segment(aes(x = 55, y = 0.845, xend = 57.5, yend = 0.78), linewidth = 0.2, color = "red", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 55, y = 0.89, label = expression(italic(C[const.])), size = 4, color = "red") +
  geom_segment(aes(x = 3.4, y = 0.20, xend = 3.8, yend = 0.03), linewidth = 0.2, color = "grey50", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 3.5, y = 0.30, label = bquote(italic(atop("start", "time"))), size = 3.5, color = "grey50") +
  geom_segment(aes(x = 0.5, y = 0.98, xend = 27.5, yend = 0.98), linewidth = 0.4, color = "grey50", arrow = arrow(length = unit(1.7, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 13.5, y = 0.95, label = bquote(italic("accumulation time")), size = 3.5, color = "grey50") +
  geom_segment(aes(x = 50.5, y = 0.73, xend = 59.5, yend = 0.73), linewidth = 0.4, color = "grey50", arrow = arrow(length = unit(1.7, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 55, y = 0.58, label = bquote(italic(atop("time of constant", "discharge"))), size = 3.5, color = "grey50") +
  geom_segment(aes(x = 11, y = 0.4, xend = 8.3, yend = 0.43), linewidth = 0.2, color = "grey50", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 14, y = 0.35, label = bquote(italic(atop("rising", "limb"))), size = 3.5, color = "grey50") +
  xlab(expression(italic(t) ~ "[min]")) +
  ylab(expression(italic(C) ~ " [-]")) +
  scale_y_continuous(limits = c(0, 1.2), breaks = scales::breaks_extended(n = 7), expand = c(0, 0)) +
  scale_size_manual(values = c(0.7, 0.7, 0.7)) +
  coord_cartesian(xlim = c(0, 62), ylim = c(0, 1)) +
  theme_ski()
ggsave(glue::glue("plt/fig_03.{file_format}"),
  plot = p, device = file_format,
  height = 80, width = 140, units = "mm", dpi = dpi
)
