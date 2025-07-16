# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Boxplots of runoff coefficients (Fig. 04)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

suppressPackageStartupMessages({
  library("tidyverse")
  library("patchwork")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

all_dat <- read_csv("dat/raw/all_data.csv", show_col_types = FALSE) |>
  mutate(ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B"))

dat_ski <- all_dat |>
  filter(ski_slope == "A") |>
  select(-ski_slope)
dat_ref <- all_dat |>
  filter(ski_slope == "B") |>
  select(-ski_slope)

psi_ski <- dat_ski |>
  select(psi_intervall) |>
  mutate(type = "A")
psi_ref <- dat_ref |>
  select(psi_intervall) |>
  mutate(type = "B")
dat_psi <- bind_rows(psi_ski, psi_ref)

p <- ggplot(dat_psi, aes(x = type, y = psi_intervall, fill = as.factor(type))) +
  geom_violin(alpha = 0.35, color = NA, width = 0.5) +
  geom_boxplot(width = 0.1, fill = NA, color = "black") +
  geom_point(aes(color = as.factor(type)), alpha = 1.3, shape = 1, position = position_jitter(width = 0.1, height = 0, seed = 42), size = 1.5) +
  scale_x_discrete(labels = c("ski slopes", "reference areas")) +
  scale_y_continuous(limits = c(0, 1.2), breaks = scales::breaks_extended(n = 7)) +
  scale_fill_manual(values = c(ski_col, ref_col), labels = c("ski slopes", "reference areas"), name = "") +
  scale_color_manual(values = c(ski_col, ref_col), labels = c("ski slopes", "reference areas"), name = "") +
  labs(x = "", y = bquote(italic(C[const.]))) +
  # coord_flip() +
  theme_ski()
ggsave(
  glue::glue("plt/fig_04.{file_format}"),
  plot = p, device = file_format,
  height = 80, width = 140, units = "mm", dpi = dpi
)
