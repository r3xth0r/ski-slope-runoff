# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# performance assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

suppressPackageStartupMessages({
  library("mlr3")
  library("data.table")
  library("dplyr")
  library("ggplot2")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# nested resampling
r_ski <- readRDS("dat/interim/random_forest/ranger_nested_resampling_ski.rds")
r_ref <- readRDS("dat/interim/random_forest/ranger_nested_resampling_noski.rds")

# final trained models
mod_ski <- readRDS("dat/interim/random_forest/ranger_trained_ski.rds")
mod_ref <- readRDS("dat/interim/random_forest/ranger_trained_noski.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_score <- function(rr, id) {
  scores <- rr$obs_loss() |>
    as_tibble() |>
    mutate(id = id)
  scores |>
    # group_by(iteration) |>
    summarize(mse = mean(regr.mse)) |>
    print()
  scores
}

s_ski <- get_score(r_ski, "ski slopes")
s_ref <- get_score(r_ref, "reference areas")

metrics <- bind_rows(s_ski, s_ref) |>
  mutate(id = forcats::fct_relevel(id, "ski slopes", "reference areas"))

p <- ggplot(metrics, aes(x = response, y = truth, color = id)) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~id) +
  scale_x_continuous(name = expression(predicted ~ italic(C[const.]))) +
  scale_y_continuous(name = expression(observed ~ italic(C[const.]))) +
  scale_color_manual(values = c(ski_col, ref_col)) +
  coord_fixed(xlim = c(0, 1.15), ylim = c(0, 1.15)) +
  theme_ski()
ggsave(
  glue::glue("plt/fig_mod_obs_rr.{file_format}"),
  plot = p, device = device,
  height = 80, width = 140, units = "mm", dpi = dpi
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
