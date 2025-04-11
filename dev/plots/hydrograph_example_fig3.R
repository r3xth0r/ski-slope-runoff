library("tidyverse")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat <- read_csv("dat/interim/plt_irrigation_example.csv") 

# Basic line plot with points
p1 <- ggplot(data = dat, aes(x = t, y = AK, group = type)) +
  theme_bw() 
  p1 <- ggplot(data = dat, aes(x = t, y = AK, group = type)) +
  theme_bw() +
  theme(
    text = element_text(size = 18),
    legend.position = "none",
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue"),
    axis.ticks.y.right = element_line(color = "blue")
  ) +
  xlab(expression(Time ~ "[min]")) +
  ylab(expression(Psi ~ " [-]")) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 71), ylim = c(0, 1)) +
    geom_segment(aes(x = 32, xend = 64, y = 0.77, yend = 0.77), linetype = "solid", color = "red", size = 0.7) +
  geom_smooth(aes(color = type, size = type, linetype = type), method = "loess", se = FALSE, span = 0.15) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_color_manual(values = c("black", "blue", "royalblue1")) +
  scale_size_manual(values = c(0.7, 0.7, 0.7)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_vline(xintercept = 17.5, linetype = "dotted", color = "black", size = 0.5) +
  geom_vline(xintercept = 62, linetype = "dotted", color = "black", size = 0.5) +
  geom_segment(aes(x = 32, y = 0.92, xend = 29, yend = 0.95), size = 0.2, arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 36, y = 0.90, label = expression(Psi[italic(peak)]), size = 6) +
  geom_segment(aes(x = 65, y = 0.845, xend = 62.5, yend = 0.78), size = 0.2, color = "red", arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("text", x = 69, y = 0.85, label = expression(Psi[italic(const.)]), size = 6, color = "red") +
  #geom_segment(aes(x = 0.7, y = 0.2, xend = 6.1, yend = 0.2), size = 0.4, color = "grey54", arrow = arrow(length = unit(2, "mm"), ends = "both", type = "closed")) +
  geom_segment(aes(x = 3.4, y = 0.20, xend = 4.5, yend = 0.01), size = 0.2, color = "grey54", arrow = arrow(length = unit(1.5, "mm"), type = "closed"))+
  annotate("text", x = 3.5, y = 0.30, label = bquote(italic(atop("start", "time"))), size = 4.5, color = "grey54") +
  geom_segment(aes(x = 0.7, y = 0.82, xend = 17, yend = 0.82), size = 0.4, color = "grey54", arrow = arrow(length = unit(2, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 8, y = 0.82, label = bquote(italic(atop("accumulation", "time"))), size = 4.5, color = "grey54") +
    geom_segment(aes(x = 17.8, y = 0.5, xend = 61.6, yend = 0.5), size = 0.4, color = "grey54", arrow = arrow(length = unit(2, "mm"), ends = "both", type = "closed")) +
    annotate("text", x = 45, y = 0.5, label = bquote(italic(atop("time of constant", "discharge"))), size = 4.5, color = "grey54") +
  geom_segment(aes(x = 62.7, y = 0.15, xend = 69.2, yend = 0.15), size = 0.4, color = "grey54", arrow = arrow(length = unit(2, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = 69.5, y = 0.25, label = bquote(italic(atop("follow-up", "time"))), size = 4.5, color = "grey54") +
  annotate("text", x = 13, y = 0.35, label = bquote(italic(atop("rising", "limb"))), size = 4.5, color = "grey54") +
  geom_segment(aes(x = 10, y = 0.4, xend = 8.1, yend = 0.44), size = 0.2, color = "grey54", arrow = arrow(length = unit(1.5, "mm"), type = "closed"))
ggsave("plt/fig_3.png", p1, device = png, height = 10, width = 19, dpi = 300, units = "cm")
#p1