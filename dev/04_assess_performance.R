# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# performance assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

suppressPackageStartupMessages({
  library("mlr3")
  library("data.table")
  library("dplyr")
})

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
  mutate(residual = truth - response) |>
  mutate(id = forcats::fct_relevel(id, "ski slopes", "reference areas"))
saveRDS(metrics_rr, "dat/processed/metrics_rr.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

metrics_mod <- bind_rows(
  tibble(truth = dat_ski, response = mod_ski$model$predictions, id = "ski slopes"),
  tibble(truth = dat_ref, response = mod_ref$model$predictions, id = "reference areas")
) |>
  mutate(residual = truth - response) |>
  mutate(id = forcats::fct_relevel(id, "ski slopes", "reference areas"))
saveRDS(metrics_mod, "dat/processed/metrics_mod.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
