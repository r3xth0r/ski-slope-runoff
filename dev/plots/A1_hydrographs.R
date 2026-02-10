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
  select(timestamp, id = combi_id, ski_slope, psi_int) |>
  group_by(id) |>
  arrange(timestamp) |>
  mutate(time_rel = as.numeric(difftime(timestamp, min(timestamp), units = "mins"))) |>
  tidyr::complete(time_rel = c(time_rel, 60)) |>
  arrange(time_rel) |>
  tidyr::fill(ski_slope, .direction = "downup") |>
  mutate(
    psi_int = if_else(
      time_rel == 60 & is.na(psi_int),
      dplyr::coalesce(dplyr::last(psi_int[time_rel < 60 & !is.na(psi_int)]), NA_real_),
      psi_int
    )
  ) |>
  ungroup() |>
  mutate(psi_int = replace_na(psi_int, 0)) |>
  filter(time_rel <= 90)

annotation_pos_y <- max(df_rel_psi$psi_int, na.rm = TRUE) * 0.95

p <- ggplot(
  df_rel_psi, aes(x = time_rel, y = psi_int, color = ski_slope)
) +
  geom_smooth(method = "loess", span = 0.5) +
  stat_smooth(aes(group = id),
    method = "gam",
    formula = y ~ s(x, bs = "ts"),
    method.args = list(family = mgcv::tw(link = "log")),
    se = FALSE,
    linewidth = 0.35,
    alpha = 0.45
  ) +
  ggalt::geom_xspline(aes(group = id), spline_shape = 0.75, alpha = 0.5) +
  geom_vline(xintercept = c(50, 60), linetype = "dashed", color = "black", linewidth = 0.3) +
  annotate("text",
    x = 55,
    y = annotation_pos_y,
    label = "C[const]",
    parse = TRUE,
    size = 3.5,
    family = "SourceSansPro",
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
  ) +
  facet_wrap(~ski_slope, nrow = 1, labeller = labeller(ski_slope = c("yes" = "Ski slope", "no" = "Reference area"))) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1.25))
