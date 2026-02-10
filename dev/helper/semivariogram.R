# Semivariogram functions
gamma_gaussian <- function(x, nugget, sill, range) {
  nugget + (sill - nugget) * (1 - exp(-(x^2) / (range^2)))
}

gamma_spherical <- function(x, nugget, sill, range) {
  out <- numeric(length(x))
  idx <- x <= range
  xr <- x[idx] / range
  out[idx] <- nugget + (sill - nugget) * (1.5 * xr - 0.5 * xr^3)
  out[!idx] <- sill
  out
}

gamma_exponential <- function(x, nugget, sill, range) {
  nugget + (sill - nugget) * (1 - exp(-x / range))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

fit_semivariogram <- function(
  df, x = "x", y = "y", use_abs_x = TRUE,
  start = NULL
) {
  df <- df |>
    dplyr::rename(x = !!x, y = !!y) |>
    dplyr::mutate(x = if (use_abs_x) abs(x) else x)

  # default initial guesses from data
  if (is.null(start)) {
    y_min <- min(df$y, na.rm = TRUE)
    y_max <- max(df$y, na.rm = TRUE)
    x_max <- max(df$x, na.rm = TRUE)
    start <- list(
      nugget = y_min,
      sill   = y_max,
      range  = x_max / 3
    )
  }

  fit <- nls(
    # TODO: hard coded Gaussian
    y ~ nugget + (sill - nugget) * (1 - exp(-(x^2) / (range^2))),
    data = df,
    start = start,
    control = nls.control(maxiter = 200, warnOnly = TRUE)
  )

  broom::tidy(fit)
}

predict_semivariogram <- function(df, params, x = "x", use_abs_x = TRUE) {
  df <- df |>
    dplyr::rename(x = !!x) |>
    dplyr::mutate(x = if (use_abs_x) abs(x) else x)

  with(as.list(setNames(params$estimate, params$term)), {
    df |>
      # TODO: hard coded Gaussian
      dplyr::mutate(y_hat = gamma_gaussian(x, nugget, sill, range))
  })
}
