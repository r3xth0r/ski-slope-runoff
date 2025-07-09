# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Individual boxplots for for ski and reference slopes of each ski region (Fig. 05)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

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
      "See in Paznaun" = "SP",
      "Sankt Anton" = "STA",
      "Wartschenbach" = "Z"
    ),
    ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B")
  ) |>
  select("toponym", "ski_slope", "psi_intervall")

p <- ggplot(all_dat, aes(x = toponym, y = psi_intervall, color = ski_slope, fill = ski_slope)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.8, position = position_dodge(preserve = "single")) +
  theme_ski() +
  scale_y_continuous(breaks = scales::breaks_extended(n = 7)) +
  labs(x = "Ski region", y = expression(italic(C[const.]))) +
  scale_color_manual(values = c(ski_col, ref_col), labels = c("ski slopes", "reference areas"), name = "") +
  scale_fill_manual(values = c(ski_col, ref_col), labels = c("ski slopes", "reference areas"), name = "") +
  geom_vline(xintercept = seq(1.5, 11.5, 1), linetype = "dashed", colour = "black", linewidth = 0.7)
ggsave(
  glue::glue("plt/fig_05.{file_format}"),
  plot = p, device = file_format,
  height = 80, width = 140, units = "mm", dpi = dpi
)
