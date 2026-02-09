# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Surface runoff hydrographs (Fig. A.01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

suppressPackageStartupMessages({
  library("tidyverse")
  library("mgcv")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

df_ski_hydro <- read_rds("dat/interim/df_ski_hydro.rds")

df_rel_psi <- df_ski_hydro |>
  group_by(combi_id) |>
  arrange(timestamp) |>
  mutate(time_rel = as.numeric(difftime(timestamp, min(timestamp), units = "mins"))) |>
  ungroup() |>
  filter(time_rel <= 60)

annotation_pos_y <- max(df_rel_psi$psi_int, na.rm = TRUE) * 0.95

p <- ggplot(
  df_rel_psi, aes(x = time_rel, y = psi_int, group = combi_id, color = ski_slope)
) +
  # geom_line() +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "ts"),
    method.args = list(family = mgcv::tw(link = "log")),
    se = FALSE,
    linewidth = 0.35,
    alpha = 0.45
  ) +
  geom_vline(xintercept = c(50, 60), linetype = "dashed", color = "black", linewidth = 0.3) +
  annotate("text",
    x = 55,
    y = annotation_pos_y,
    label = "Time Cconst",
    size = 3.5,
    fontface = "italic",
    color = "black"
  ) +
  scale_color_manual(
    values = c("yes" = ski_col, "no" = ref_col),
    labels = c("yes" = "Ski Slope", "no" = "Reference Area"),
    name = NULL
  ) +
  labs(
    x = "t [min]",
    y = expression(italic(C) * " [-]"),
    title = "ARS Hydrographs"
  ) +
  theme_ski() +
  theme(
    legend.position = "bottom"
  )
