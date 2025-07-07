# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Conceptual hydrograph (Fig. 03)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")

source("dev/helper/theme_ski.R")

# Load data ----
dat <- read_csv("dat/interim/plt_irrigation_example.csv")

# Basic line plot with points
p <- ggplot(data = dat, aes(x = t, y = AK, group = type)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15, color = "black") +
  geom_segment(aes(x = 50, xend = 60, y = 0.77, yend = 0.77), linetype = "solid", color = "red", size = 0.7) +
  geom_segment(aes(x = 50, y = 0.70, xend = 50, yend = 1), size = 0.5, color = "black", linetype = "dotted") +
  geom_segment(aes(x = 60, y = 0.70, xend = 60, yend = 1), size = 0.5, color = "black", linetype = "dotted") +
  geom_segment(aes(x = 32, y = 0.92, xend = 29, yend = 0.95), size = 0.2, arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 36, y = 0.90, label = expression(italic(C[peak])), size = 4) +
  geom_segment(aes(x = 55, y = 0.845, xend = 57.5, yend = 0.78), size = 0.2, color = "red", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 55, y = 0.89, label = expression(italic(C[const.])), size = 4, color = "red") +
  geom_segment(aes(x = 3.4, y = 0.20, xend = 3.8, yend = 0.03), size = 0.2, color = "grey54", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 3.5, y = 0.30, label = bquote(italic(atop("start", "time"))), size = 3.5, color = "grey54") +
  geom_segment(aes(x = 0.7, y = 0.82, xend = 17, yend = 0.82), size = 0.4, color = "grey54", arrow = arrow(length = unit(1.7, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 8.3, y = 0.82, label = bquote(italic(atop("accumulation", "time"))), size = 3.5, color = "grey54") +
  geom_segment(aes(x = 50.5, y = 0.73, xend = 59.5, yend = 0.73), size = 0.4, color = "grey54", arrow = arrow(length = unit(1.7, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 55, y = 0.58, label = bquote(italic(atop("time of constant", "discharge"))), size = 3.5, color = "grey54") +
  annotate("text", x = 13, y = 0.35, label = bquote(italic(atop("rising", "limb"))), size = 3.5, color = "grey54") +
  geom_segment(aes(x = 10, y = 0.4, xend = 8.1, yend = 0.44), size = 0.2, color = "grey54", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  xlab(expression(italic(t) ~ "[min]")) +
  ylab(expression(italic(C)~ " [-]")) +
  scale_y_continuous(limits = c(0, 1.2), breaks = scales::breaks_extended(n = 7), expand=c(0,0)) +
  scale_size_manual(values = c(0.7, 0.7, 0.7)) +
  coord_cartesian(xlim = c(0, 62), ylim = c(0, 1)) +
  theme_ski() 
ggsave("plt/fig_03.png", p, device = png, height = 78.9, width = 140, dpi = 300, units = "mm")
