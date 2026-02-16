# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Surface runoff hydrographs (Fig. A.01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

suppressPackageStartupMessages({
  library("tidyverse")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")
source("dev/helper/geom_xspline.R")

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
  geom_xspline(aes(group = id), spline_shape = 0.75, alpha = 0.7, show.legend = FALSE) +
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
    labels = c("yes" = "ski slopes", "no" = "reference areas"),
    name = NULL
  ) +
  xlab(expression(italic(t) ~ "[min]")) +
  ylab(expression(italic(C) ~ " [-]")) +
  theme_ski() +
  facet_wrap(~ski_slope, nrow = 1, labeller = labeller(ski_slope = c("yes" = "ski slopes", "no" = "reference areas"))) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1.25))
ggsave(
  glue::glue("plt/fig_A1.{file_format}"),
  plot = p, device = device,
  height = 80, width = 160, units = "mm", dpi = dpi
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
