library("tidyverse")
library("patchwork")

source("dev/helper/get_importance.R")

# non-ski-slopes ----
learner_ski <- read_rds("dat/interim/random_forest/ranger_trained_noski.rds")
imp <- get_importance(learner_ski) |>
  mutate(importance = if_else(importance < 0, 0, importance)) |>
  mutate(index = gsub("_", " ", index))

# Rename factor level
imp <- imp %>%
  mutate(index = fct_recode(index, "soil structure" = "embedded rock type"))

# Highlight top 6 features
top6_noski <- imp %>% top_n(5, importance) %>% pull(index)
imp <- imp %>%
  mutate(color = if_else(index %in% top6_noski, "highlight", "normal"))

# Classify point types
imp <- imp %>%
  mutate(point_type = case_when(
    index %in% c("pasture", "vegetation class", "ground cover") ~ "land use",
    index %in% c("geomorphon", "slope") ~ "topography",
    index %in% c("skeleton", "geol class", "soiltexture") ~ "geology",
    index %in% c("soil structure", "humus type", "bulk density", "soil depth", "sd delta", "soil class") ~ "soil"
  ))

p_noski <- ggplot(
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
  scale_fill_manual(values = c("highlight" = "#A27146", "normal" = "#D3D3D3"), guide = "none") +
  scale_color_manual(values = c("land use" = "black", "topography" = "black", "geology" = "black", "soil" = "black"), guide = "none") +
  scale_shape_manual(values = c("land use" = 21, "topography" = 22, "geology" = 23, "soil" = 24), 
                     breaks = c("land use", "topography", "geology", "soil")) +
  coord_flip() +
  xlab("") +
  ylab(expression(Importance~on~italic(C[constant]))) +
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) +  # Set y-axis to integer values from 0 to 4
  labs(title = paste0("reference")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(angle = 90, vjust = 0.5, size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, size = 20),
    axis.title.y.right = element_text(angle = 0, vjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),  # Adjust the size of the legend keys
    legend.spacing.y = unit(0.5, "cm")  # Adjust the spacing between legend entries
  ) +
  guides(shape = guide_legend(title = NULL))

# ski-slopes ----
learner_ski <- readRDS("dat/interim/random_forest/ranger_trained_ski.rds")
imp <- get_importance(learner_ski) |>
  mutate(importance = if_else(importance < 0, 0, importance)) |>
  mutate(index = gsub("_", " ", index))

# Rename factor level
imp <- imp %>%
  mutate(index = fct_recode(index, "soil structure" = "embedded rock type"))

# Highlight top 6 features
top6_ski <- imp %>% top_n(6, importance) %>% pull(index)
imp <- imp %>%
  mutate(color = if_else(index %in% top6_ski, "highlight", "normal"))

# Classify point types
imp <- imp %>%
  mutate(point_type = case_when(
    index %in% c("pasture", "vegetation class", "ground cover") ~ "land use",
    index %in% c("geomorphon", "slope") ~ "topography",
    index %in% c("skeleton", "geol class", "soiltexture") ~ "geology",
    index %in% c("soil structure","humus type", "bulk density", "soil depth", "sd delta", "soil class") ~ "soil"
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
  scale_fill_manual(values = c("highlight" = "#33ccff", "normal" = "#D3D3D3"), guide = "none") +
  scale_color_manual(values = c("land use" = "black", "topography" = "black", "geology" = "black", "soil" = "black"), guide = "none") +
  scale_shape_manual(values = c("land use" = 21, "topography" = 22, "geology" = 23, "soil" = 24), 
                     breaks = c("land use", "topography", "geology", "soil")) +
  coord_flip() +
  xlab("") +
  ylab(expression(Importance~on~italic(C[constant]))) +
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) +  # Set y-axis to integer values from 0 to 4
  labs(title = paste0("ski slope")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(angle = 90, vjust = 0.5, size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, size = 20),
    axis.title.y.right = element_text(angle = 0, vjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),  # Adjust the size of the legend keys
    legend.spacing.y = unit(0.5, "cm")  # Adjust the spacing between legend entries
  ) +
  guides(shape = guide_legend(title = NULL))

# patchwork ----

patchwork <- (p_ski) / (p_noski)
patchwork
ggsave("plt/fig6.png", patchwork, device = png, height = 28, width = 30, dpi = 300, units = "cm")
