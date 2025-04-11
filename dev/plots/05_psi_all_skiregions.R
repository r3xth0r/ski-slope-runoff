# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Individual boxplots for for ski and reference slopes of each ski region (Fig. 05)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")

source("dev/helper/theme_ski.R")

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
  theme_ski() +
  scale_y_continuous(breaks = scales::breaks_extended(n = 7)) +
  labs(x = "Ski regions", y = expression(Psi[italic(constant)])) +
  scale_color_manual(values = c(ski_col, ref_col), labels = c("ski slope", "reference"), name = "") +
  scale_fill_manual(values = c(ski_col, ref_col), labels = c("ski slope", "reference"), name = "") +
  geom_vline(xintercept = seq(1.5, 11.5, 1), linetype = "dashed", colour = "black", size = 0.7)

ggsave("plt/fig_05.png", plot = p, device = png, height = 10, width = 19, dpi = 300, units = "cm")
