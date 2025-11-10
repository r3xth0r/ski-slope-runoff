# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Individual boxplots for for ski and reference slopes of each ski region (Fig. 05)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

suppressPackageStartupMessages({
  library("tidyverse")
  library("ggbeeswarm")
  library("patchwork")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

all_dat <- read_csv("dat/raw/all_data.csv", show_col_types = FALSE) |>
  mutate(
    ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B")
  ) |>
  select("toponym", "ski_slope", "psi_intervall")

counts_template <- expand_grid(
  toponym = unique(all_dat$toponym),
  ski_slope = unique(all_dat$ski_slope)
)
counts <- all_dat |>
  group_by(toponym, ski_slope) |>
  summarize(n = n(), psi_min = min(psi_intervall), psi_max = max(psi_intervall), .groups = "drop") |>
  right_join(counts_template, by = join_by(toponym, ski_slope)) |>
  mutate(n = replace_na(n, 0))
ymax <- ceiling(max(all_dat$psi_intervall) * 10) / 10

p <- ggplot(all_dat, aes(x = toponym, color = ski_slope)) +
  # geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.8, position = position_dodge(preserve = "single")) +
  geom_linerange(
    data = counts, aes(x = toponym, ymin = psi_min, ymax = psi_max, color = ski_slope),
    position = position_dodge(width = 0.8)
  ) +
  geom_beeswarm(aes(y = psi_intervall, fill = ski_slope), dodge.width = 0.8, size = 1.5, alpha = 0.7) +
  theme_ski() +
  scale_y_continuous(breaks = scales::breaks_extended(n = 7)) +
  labs(x = "Ski region", y = expression(italic(C[const.]))) +
  scale_color_manual(values = c(ski_col, ref_col), labels = c("ski slopes", "reference areas"), name = "") +
  scale_fill_manual(values = c(ski_col, ref_col), labels = c("ski slopes", "reference areas"), name = "") +
  geom_vline(xintercept = seq(1.5, 11.5, 1), linetype = "dashed", colour = "black", linewidth = 0.7) +
  geom_text(
    data = counts, aes(x = toponym, y = ymax, label = n, group = ski_slope),
    position = position_dodge(width = 0.8), vjust = -0.5, size = 2,
    family = "SourceSansPro"
  )

ggsave(
  glue::glue("plt/fig_05.{file_format}"),
  plot = p, device = device,
  height = 80, width = 140, units = "mm", dpi = dpi
)
