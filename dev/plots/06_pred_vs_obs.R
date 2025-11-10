# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# performance assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

suppressPackageStartupMessages({
  library("mlr3")
  library("data.table")
  library("dplyr")
})

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
