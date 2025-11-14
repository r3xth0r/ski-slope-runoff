#' Summarize scores from a resample object
#'
#' This function takes a resample object from the `mlr3` package and an identifier for the slope type,
#' calculates the mean squared error (MSE) for the observations, and returns a tibble with the scores.
#'
#' @param rr A `ResampleResult` object from the `mlr3` package.
#' @param id A character string representing the label to be added
#'   to the scores.
#' @param bygroup A logical flag indicating whether to group by resampling iteration.
#'   Defaults to `FALSE`.
#'
#' @return A tibble containing the scores per iteration. Also prints the overall
#'   mean squared error across all iterations.
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
