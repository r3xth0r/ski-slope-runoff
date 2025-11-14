# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# predicted vs observed plot assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

suppressPackageStartupMessages({
  library("mlr3")
  library("data.table")
  library("dplyr")
  library("ggplot2")
})

source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

# config for mod vs obs plot
TYPE <- "mod" # rr

if (!(TYPE %in% c("mod", "rr"))) {
  stop("TYPE should be one of\n - `mod` (full final model)\n - `rr` (resampling result)")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

metrics <- readRDS(glue::glue("dat/processed/metrics_{TYPE}.rds"))

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
  glue::glue("plt/fig_06_{TYPE}.{file_format}"),
  plot = p, device = device,
  height = 80, width = 140, units = "mm", dpi = dpi
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
