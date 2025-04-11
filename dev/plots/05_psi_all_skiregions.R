# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Individual boxplots for for ski and reference slopes of each ski region (Fig. 05)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")


all_dat <- read_csv("dat/raw/all_data.csv") |>
  mutate(
    toponym = recode_factor(
      project_area,
      "Dobratsch" = "D",
      "Ehrwald" = "E",
      "Golm" = "G",
      "Hauser Kaibling" = "H",
      "Ischgl" = "I",
      "Meran2000" = "M",
      "Nassfeld" = "N",
      "Patscherkofel" = "P",
      "Schladming" = "S",
      "Sankt Anton" = "STA",
      "See in Paznaun" = "SP",
      "Wartschenbach" = "Z"
    ),
    ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B")
  ) |>
  select("toponym", "ski_slope", "psi_intervall")

p <- ggplot(all_dat, aes(x = toponym, y = psi_intervall, color = as.factor(ski_slope), fill = as.factor(ski_slope))) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.8) +
  stat_summary(aes(group = as.factor(ski_slope), color = as.factor(ski_slope)), fun = median, geom = "point", shape = 20, size = 3, position = position_dodge2(width = 0.8)) +
  theme_bw() +
  theme(
    legend.position = "top", legend.text = element_text(size = 16), text = element_text(size = 16), axis.title.y = element_text(angle = 90, vjust = 0.5, size = 16), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, size = 16), axis.title.y.right = element_text(angle = 0, vjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 7)) +
  labs(x = "Ski regions", y = expression(Psi[italic(constant)])) +
  scale_color_manual(values = c("#33ccff", "#A27146"), labels = c("ski slope", "reference"), name = "") +
  scale_fill_manual(values = c("#33ccff", "#A27146"), labels = c("ski slope", "reference"), name = "") +
  geom_vline(aes(xintercept = 1.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 2.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 3.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 4.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 5.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 6.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 7.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 8.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 9.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 10.5), linetype = "dashed", colour = "black", size = 0.7) +
  geom_vline(aes(xintercept = 11.5), linetype = "dashed", colour = "black", size = 0.7)

ggsave("plt/fig5.png", plot = p1, device = png, height = 10, width = 19, dpi = 300, units = "cm")
