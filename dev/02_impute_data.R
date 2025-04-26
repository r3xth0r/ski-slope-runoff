# Select features to include in model
# Impute missing values using MissForest and PMM
# - Stekhoven & Bühlmann (2012): https://doi.org/10.1093/bioinformatics/btr597
# - van Buuren & Groothuis-Oudshoorn (2011): https://doi.org/10.18637/jss.v045.i03

library("tidyverse")
library("missRanger")
library("tictoc")

dat_na <- read_csv("./dat/raw/all_data.csv") |>
  select(
    # metadata
    id,
    toponym = project_name, ski_slope,
    # target
    psi_intervall,
    # features
    geol_class, soil_class, humus_type,
    vegetation_class, pasture, ground_cover,
    slope = slope_calc, geomorphon = topography,
    soil_depth, skeleton, embedded_rock_type, soiltexture,
    bulk_density, total_pore_volume_10, coarse_pore_volume_10, pre_moisture, moisture_intervall,
    saturation_deficit, saturation_deficit_intervall,
    # only for imputation, remove
    RKL, abstraction
  ) |>
  mutate(pasture = if_else(pasture == "yes", NA, pasture)) |>
  mutate(pasture = if_else(pasture == "", NA, pasture)) |>
  mutate(across(where(is.character), ~ as.factor(.x))) |>
  mutate(
    pasture = as.ordered(fct_relevel(pasture, "no", "low", "medium", "intensive")),
    soil_depth = as.ordered(fct_relevel(soil_depth, "sehr flachgründig", "flachgründig", "mittelgründig", "tiefgründig"))
  )

# missing values
sapply(dat_na, \(x) sum(is.na(x))) |>
  enframe(name = "feature", value = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(-n_missing) |>
  mutate(p_missing = n_missing / nrow(dat_na) * 100)

# multiple imputation
tic()
dat_imputed <- dat_na %>%
  select(-c(id, toponym)) %>%
  missRanger(
    formula = . ~ .,
    verbose = 1,
    pmm.k = 5L,
    maxiter = 10L,
    seed = 2718L,
    num.trees = 5000,
    respect.unordered.factors = "order",
    # weight contribution of rows according to number of missing values
    case.weights = rowSums(!is.na(dat_na))
  )
toc()

res <- dat_na |>
  as_tibble() |>
  select(id, toponym) |>
  bind_cols(dat_imputed) |>
  select(-RKL, -abstraction)
# res$sat_deficit_ratio <- res$saturation_deficit_intervall/res$saturation_deficit
# res$sat_deficit_difference <- res$saturation_deficit-res$saturation_deficit_intervall
write_csv(res, "dat/interim/model_data_imputed.csv")
write_rds(res, "dat/interim/model_data_imputed.rds")
