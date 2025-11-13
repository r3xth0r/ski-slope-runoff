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
source("dev/helper/get_score.R")

# config for mod vs obs plot
TYPE <- "mod" # rr

if (!(TYPE %in% c("mod", "rr"))) {
  stop("TYPE should be one of\n - `mod` (full final model)\n - `rr` (resampling result)")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# observed psi data data
dat_ski <- read.csv("dat/processed/dat_sd_delta_ski.csv") |>
  pull(psi_intervall)
dat_ref <- read.csv("dat/processed/dat_sd_delta_noski.csv") |>
  pull(psi_intervall)

# nested resampling
r_ski <- readRDS("dat/interim/random_forest/ranger_nested_resampling_ski.rds")
r_ref <- readRDS("dat/interim/random_forest/ranger_nested_resampling_noski.rds")

# final trained models
mod_ski <- readRDS("dat/interim/random_forest/ranger_trained_ski.rds")
mod_ref <- readRDS("dat/interim/random_forest/ranger_trained_noski.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

s_ski <- get_score(r_ski, "ski slopes")
s_ref <- get_score(r_ref, "reference areas")

metrics_rr <- bind_rows(s_ski, s_ref) |>
  mutate(id = forcats::fct_relevel(id, "ski slopes", "reference areas"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

metrics_mod <- bind_rows(
  tibble(truth = dat_ski, response = mod_ski$model$predictions, id = "ski slopes"),
  tibble(truth = dat_ref, response = mod_ref$model$predictions, id = "reference areas")
) |>
  mutate(id = forcats::fct_relevel(id, "ski slopes", "reference areas"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

if (TYPE == "rr") {
  metrics <- metrics_rr
} else if (TYPE == "mod") {
  metrics <- metrics_mod
}

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
  glue::glue("plt/fig_mod_obs_{TYPE}.{file_format}"),
  plot = p, device = device,
  height = 80, width = 140, units = "mm", dpi = dpi
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
