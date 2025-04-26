# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Conceptual hydrograph (Fig. 03)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")

source("dev/helper/theme_ski.R")

# Load data ----
dat <- read_csv("dat/interim/plt_irrigation_example.csv")

# Basic line plot with points
p <- ggplot(data = dat, aes(x = t, y = AK, group = type)) +
  geom_smooth(aes(size = type, linetype = type), method = "loess", se = FALSE, span = 0.15, color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_segment(aes(x = 50, xend = 60, y = 0.77, yend = 0.77), linetype = "solid", color = "red", size = 0.7) +
  geom_segment(aes(x = 50, y = 0.70, xend = 50, yend = 1), size = 0.5, color = "black", linetype = "dotted") +
  geom_segment(aes(x = 60, y = 0.70, xend = 60, yend = 1), size = 0.5, color = "black", linetype = "dotted") +
  geom_segment(aes(x = 32, y = 0.92, xend = 29, yend = 0.95), size = 0.2, arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 36, y = 0.90, label = expression(italic(C[peak])), size = 6) +
  geom_segment(aes(x = 55, y = 0.845, xend = 57.5, yend = 0.78), size = 0.2, color = "red", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 55, y = 0.89, label = expression(italic(C[const.])), size = 6, color = "red") +
  geom_segment(aes(x = 3.4, y = 0.20, xend = 4.5, yend = 0.01), size = 0.2, color = "grey54", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 3.5, y = 0.30, label = bquote(italic(atop("start", "time"))), size = 4.5, color = "grey54") +
  geom_segment(aes(x = 0.7, y = 0.82, xend = 17, yend = 0.82), size = 0.4, color = "grey54", arrow = arrow(length = unit(2, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 8, y = 0.82, label = bquote(italic(atop("accumulation", "time"))), size = 4.5, color = "grey54") +
  geom_segment(aes(x = 50.5, y = 0.73, xend = 59.5, yend = 0.73), size = 0.4, color = "grey54", arrow = arrow(length = unit(2, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 55, y = 0.61, label = bquote(italic(atop("time of constant", "discharge"))), size = 4.5, color = "grey54") +
  annotate("text", x = 13, y = 0.35, label = bquote(italic(atop("rising", "limb"))), size = 4.5, color = "grey54") +
  geom_segment(aes(x = 10, y = 0.4, xend = 8.1, yend = 0.44), size = 0.2, color = "grey54", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  xlab(expression(Time ~ "[min]")) +
  ylab(expression(Psi ~ " [-]")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_size_manual(values = c(0.7, 0.7, 0.7)) +
  coord_cartesian(xlim = c(0, 71), ylim = c(0, 1)) +
  theme_ski() +
  theme(
    text = element_text(size = 18),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue"),
    axis.ticks.y.right = element_line(color = "blue")
  )
ggsave("plt/fig_03.png", p1, device = png, height = 10, width = 19, dpi = 300, units = "cm")
