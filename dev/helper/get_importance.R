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
