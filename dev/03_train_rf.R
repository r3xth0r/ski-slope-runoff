suppressPackageStartupMessages({
  library("mlr3")
  library("mlr3learners")
  library("mlr3tuning")
  library("mlr3mbo")
  library("rgenoud")
  library("DiceKriging")
  library("ranger")
  library("dplyr")
  library("glue")
  library("tictoc")
  library("ggplot2")
})

args <- commandArgs(trailingOnly = TRUE)

# stop the script if command line argument is not "ski" or "noski"
if (length(args) != 1) {
  cat("Argument must be 'ski' or 'noski'\n")
  stop("Please provide exactly one argument")
}

print(glue("» {format(Sys.time())} -- Working on use case '{args[1]}'\n"))

use_case <- args[1]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# helper functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

get_importance <- function(ranger_model) {
  ranger_model$importance() |>
    tibble::enframe() |>
    rename(index = name, importance = value) |>
    arrange(-importance) |>
    mutate(index = forcats::fct_reorder(index, -desc(importance)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# read data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

dat <- readRDS("dat/interim/model_data_imputed.rds") |>
  mutate(
    # sd_ratio = saturation_deficit_intervall / saturation_deficit,
    sd_delta = saturation_deficit_intervall - saturation_deficit,
    # sd_mad = sd_delta - mean(saturation_deficit)
  ) |>
  select(
    -id, -toponym, -pre_moisture, -moisture_intervall,
    -coarse_pore_volume_10, -total_pore_volume_10,
    -saturation_deficit, -saturation_deficit_intervall,
  )

if (use_case == "noski") {
  dat <- subset(dat, ski_slope == "no") |>
    select(-ski_slope)
  readr::write_csv(dat, "dat/processed/dat_sd_delta_noski.csv")
} else if (use_case == "ski") {
  dat <- subset(dat, ski_slope == "yes") |>
    select(-ski_slope)
  readr::write_csv(dat, "dat/processed/dat_sd_delta_ski.csv")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# setup mlr3 task
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

set.seed(2718L)

task <- as_task_regr(dat, target = "psi_intervall", id = "psi_intervall")
task_ski <- task
learner <- lrn("regr.ranger",
  num.trees = 2000,
  mtry = to_tune(1, length(task$feature_names)),
  min.node.size = to_tune(p_int(1, 10)),
  sample.fraction = to_tune(0.2, 0.9),
  respect.unordered.factors = "order",
  importance = "permutation",
  num.threads = 16L
)

tuner <- tnr("mbo")

resampling <- rsmp("cv", folds = 4)

measure <- msr("regr.mse")

terminator <- trm("evals", n_evals = 2000)

# nested resampling
at <- auto_tuner(
  tuner = tuner,
  learner = learner,
  resampling = resampling,
  measure = measure,
  terminator = trm("evals", n_evals = 1000)
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# performance estimation
print(glue("» {format(Sys.time())} -- Estimating performance'"))
tic()
rr <- resample(task = task, learner = at, resampling = rsmp("cv", folds = 4), store_models = TRUE)
toc()
saveRDS(rr, glue("dat/interim/random_forest/ranger_nested_resampling_{use_case}.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# tune rf
print(glue("» {format(Sys.time())} -- Tuning model'"))
tic()
instance <- tune(
  tuner = tuner,
  task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  terminator = terminator
)
toc()

# set optimal hyperparameter configuration to learner
learner$param_set$values <- instance$result_learner_param_vals

# train the learner on the full data set
print(glue("» {format(Sys.time())} -- Training full model w/optimal hyperparameters'"))
learner$train(task)

saveRDS(learner, glue("dat/interim/random_forest/ranger_trained_{use_case}.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# permutation feature importance
imp <- get_importance(learner) |>
  mutate(importance = if_else(importance < 0, 0, importance))

p <- ggplot(imp, aes(x = importance, y = index)) +
  geom_point() +
  theme_bw() +
  scale_x_log10()
ggsave(glue("plt/importance_{use_case}.png"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
