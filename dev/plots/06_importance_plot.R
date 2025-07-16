# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Feature importance plot (Fig. 06)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

suppressPackageStartupMessages({
  library("tidyverse")
  library("patchwork")
})

source("dev/helper/get_importance.R")
source("dev/helper/theme_ski.R")
source("dev/helper/config.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

process_importance <- function(learner, top_n_features = 6L) {
  imp <- learner |>
    get_importance() |>
    mutate(importance = if_else(importance < 0, 0, importance)) |>
    mutate(index = gsub("_", " ", index)) |>
    mutate(index = fct_recode(index,
      "coarse fraction embedment" = "embedded rock type",
      "saturation deficit difference" = "sd delta",
      "geological class" = "geol class",
      "texture" = "soiltexture",
      "humustype" = "humus type",
      "pasture intensity" = "pasture"
    ))
  top_n <- imp |>
    top_n(top_n_features, importance) |>
    pull(index)
  imp |>
    mutate(color = if_else(index %in% top_n, "highlight", "normal")) |>
    mutate(point_type = case_when(
      index %in% c("pasture intensity", "vegetation class", "ground cover") ~ "land use",
      index %in% c("geomorphon", "slope") ~ "topography",
      index %in% c("skeleton", "geological class", "texture") ~ "geology",
      index %in% c(
        "coarse fraction embedment", "humustype", "bulk density",
        "soil depth", "saturation deficit difference", "soil class"
      ) ~ "soil"
    ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

plot_importance <- function(imp, title, highlight_color) {
  ggplot(
    imp,
    aes(
      x = reorder(index, importance), y = importance * 100,
      label = round(100 * importance, 0),
      fill = color,
      shape = point_type
    )
  ) +
    geom_col(width = 0.1) +
    geom_point(aes(color = point_type, fill = color), size = 3, stroke = 1) +
    scale_fill_manual(values = c("highlight" = highlight_color, "normal" = "#D3D3D3"), guide = "none") +
    scale_color_manual(
      values = c("land use" = "black", "topography" = "black", "geology" = "black", "soil" = "black"),
      guide = "none"
    ) +
    scale_shape_manual(
      values = c("land use" = 21, "topography" = 22, "geology" = 23, "soil" = 24),
      breaks = c("land use", "topography", "geology", "soil")
    ) +
    coord_flip() +
    xlab("") +
    scale_y_continuous(
      name = expression(Importance ~ on ~ italic(C[const.])),
      breaks = seq(0, 4, by = 1), limits = c(0, 4)
    ) +
    labs(title = title) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = font),
      legend.position = "inside",
      legend.justification.inside = c(1, 0.1),
      legend.margin = margin(0, 0, 0, 0),
      legend.background = element_rect(color = "white"),
      legend.key.size = unit(7, "mm"),
      legend.spacing.y = unit(3, "mm")
    ) +
    guides(shape = guide_legend(title = NULL))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Reference slopes ----
learner_ref <- read_rds("dat/interim/random_forest/ranger_trained_noski.rds")
imp_ref <- process_importance(learner_ref, 5)
p_ref <- plot_importance(imp_ref, "reference areas", ref_col)

# Ski slopes ----
learner_ski <- read_rds("dat/interim/random_forest/ranger_trained_ski.rds")
imp_ski <- process_importance(learner_ski, 6)
p_ski <- plot_importance(imp_ski, "ski slopes", ski_col)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Compose plot ----
patchwork <- (p_ski) / (p_ref)
ggsave(
  glue::glue("plt/fig_06.{file_format}"),
  plot = patchwork, device = device,
  height = 160, width = 140, units = "mm", dpi = dpi
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
