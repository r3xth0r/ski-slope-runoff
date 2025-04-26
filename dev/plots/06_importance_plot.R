# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Feature importance plot (Fig. 06)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")

source("dev/helper/get_importance.R")
source("dev/helper/theme_ski.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Reference slopes ----
learner_ref <- read_rds("dat/interim/random_forest/ranger_trained_noski.rds")

imp <- get_importance(learner_ref) |>
  mutate(importance = if_else(importance < 0, 0, importance)) |>
  mutate(index = gsub("_", " ", index)) |>
  mutate(index = fct_recode(index, "coarse fraction embedment" = "embedded rock type")) |>
  mutate(index = fct_recode(index, "saturation deficit difference" = "sd delta")) |>
  mutate(index = fct_recode(index, "geological class" = "geol class")) |>
  mutate(index = fct_recode(index, "texture" = "soiltexture")) |>
  mutate(index = fct_recode(index, "humustype" = "humus type")) |>
  mutate(index = fct_recode(index, "vegetation" = "vegetation class"))

# Highlight top 6 features
top6_ref <- imp |>
  top_n(5, importance) |>
  pull(index)
imp <- imp |>
  mutate(color = if_else(index %in% top6_ref, "highlight", "normal"))

# Classify point types
imp <- imp |>
  mutate(point_type = case_when(
    index %in% c("pasture", "vegetation", "ground cover") ~ "land use",
    index %in% c("geomorphon", "slope") ~ "topography",
    index %in% c("skeleton", "geological class", "texture") ~ "geology",
    index %in% c("coarse fraction embedment", "humustype", "bulk density", "soil depth", "saturation deficit difference", "soil class") ~ "soil"
  ))

p_ref <- ggplot(
  imp,
  aes(
    x = reorder(index, importance), y = importance * 100,
    label = round(100 * importance, 0),
    fill = color,
    shape = point_type
  )
) +
  geom_col(width = 0.1) +
  geom_point(aes(color = point_type, fill = color), size = 6, stroke = 2) +
  scale_fill_manual(values = c("highlight" = ref_col, "normal" = "#D3D3D3"), guide = "none") +
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
    name = expression(Importance ~ on ~ italic(C[constant])),
    breaks = seq(0, 4, by = 1), limits = c(0, 4)
  ) +
  labs(title = paste0("reference")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(angle = 90, vjust = 0.5, size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, size = 20),
    axis.title.y.right = element_text(angle = 0, vjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1, "cm"), # Adjust the size of the legend keys
    legend.spacing.y = unit(0.5, "cm") # Adjust the spacing between legend entries
  ) +
  guides(shape = guide_legend(title = NULL))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Ski slopes ----
learner_ski <- read_rds("dat/interim/random_forest/ranger_trained_ski.rds")

imp <- get_importance(learner_ski) |>
  mutate(importance = if_else(importance < 0, 0, importance)) |>
  mutate(index = gsub("_", " ", index)) |>
  mutate(index = fct_recode(index, "coarse fraction embedment" = "embedded rock type")) |>
  mutate(index = fct_recode(index, "saturation deficit difference" = "sd delta")) |>
  mutate(index = fct_recode(index, "geological class" = "geol class")) |>
  mutate(index = fct_recode(index, "texture" = "soiltexture")) |>
  mutate(index = fct_recode(index, "humustype" = "humus type")) |>
  mutate(index = fct_recode(index, "vegetation" = "vegetation class"))

# Highlight top 6 features
top6_ski <- imp |>
  top_n(6, importance) |>
  pull(index)
imp <- imp |>
  mutate(color = if_else(index %in% top6_ski, "highlight", "normal"))

# Classify point types
imp <- imp |>
  mutate(point_type = case_when(
    index %in% c("pasture", "vegetation", "ground cover") ~ "land use",
    index %in% c("geomorphon", "slope") ~ "topography",
    index %in% c("skeleton", "geological class", "texture") ~ "geology",
    index %in% c("coarse fraction embedment", "humustype", "bulk density", "soil depth", "saturation deficit difference", "soil class") ~ "soil"
  ))

p_ski <- ggplot(
  imp,
  aes(
    x = reorder(index, importance), y = importance * 100,
    label = round(100 * importance, 0),
    fill = color,
    shape = point_type
  )
) +
  geom_col(width = 0.1) +
  geom_point(aes(color = point_type, fill = color), size = 6, stroke = 2) +
  scale_fill_manual(values = c("highlight" = ski_col, "normal" = "#D3D3D3"), guide = "none") +
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
    name = expression(Importance ~ on ~ italic(C[constant])),
    breaks = seq(0, 4, by = 1), limits = c(0, 4)
  ) +
  labs(title = paste0("ski slope")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(angle = 90, vjust = 0.5, size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, size = 20),
    axis.title.y.right = element_text(angle = 0, vjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1, "cm"), # Adjust the size of the legend keys
    legend.spacing.y = unit(0.5, "cm") # Adjust the spacing between legend entries
  ) +
  guides(shape = guide_legend(title = NULL))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Compose plot ----
patchwork <- (p_ski) / (p_ref)
ggsave("plt/fig_06.png", patchwork, device = png, height = 28, width = 30, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
