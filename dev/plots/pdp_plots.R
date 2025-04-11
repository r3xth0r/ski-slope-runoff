# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Partial dependence plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("mlr3")
library("ggplot2")
library("patchwork")
library(tidyverse)

# Get data for ski slopes and reference slopes
dat_ski <- readRDS("dat/processed/dat_sd_delta_ski.rds")
dat_nonski <- readRDS("dat/processed/dat_sd_delta_noski.rds")

# Load trained learners
learner_ski <- readRDS("dat/interim/random_forest/ranger_trained_ski.rds")
learner_nonski <- readRDS("dat/interim/random_forest/ranger_trained_noski.rds")

# Source helper functions
source("dev/helper/construct_effects.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Land use
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Pasture ----

# Construct effects
pasture <- construct_effects(learner_nonski, learner_ski, feature = "pasture")

# Extract the data for plotting
pasture_nonski_data <- pasture$nonski$results
pasture_ski_data <- pasture$ski$results
# Define custom colors
custom_colors_ski <- c("no" = "#33ccff40", "low" = "#33ccff80", "medium" = "#33ccffBF", "intensive" = "#33ccff")
custom_colors_ref <- c("no" = "#A2714640", "low" = "#A2714680", "medium" = "#A27146BF", "intensive" = "#A27146")


# Create a boxplot with different colors for each factor level
p1 <- ggplot(pasture_nonski_data, aes(x = pasture, y = .value, fill = pasture)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(title = "reference", x = "pasture") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.2), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

p2 <- ggplot(pasture_ski_data, aes(x = pasture, y = .value, fill = pasture)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(title = "ski slope", x = "pasture") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.2), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Vegetation ----

vc <- construct_effects(learner_nonski, learner_ski, feature = "vegetation_class")

# Extract the data for plotting
vc_nonski_data <- vc$nonski$results
vc_ski_data <- vc$ski$results
vc_nonski_data$vegetation_class <- factor(vc_nonski_data$vegetation_class, levels = c("forest", "shrubs", "grassland"))
vc_ski_data$vegetation_class <- factor(vc_ski_data$vegetation_class, levels = c("forest", "shrubs", "grassland"))
# Define custom colors
custom_colors_ski <- c("forest" = "#33ccff", "grassland" = "#33ccff", "shrubs" = "#33ccff")
custom_colors_ref <- c("forest" = "#A27146", "grassland" = "#A27146", "shrubs" = "#A27146")

p3 <- ggplot(vc_nonski_data, aes(x = vegetation_class, y = .value, fill = vegetation_class)) +
  geom_boxplot() +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(x = "vegetation class") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.5), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

p4 <- ggplot(vc_ski_data, aes(x = vegetation_class, y = .value, fill = vegetation_class)) +
  geom_boxplot() +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  theme_bw() +
  # scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(x = "vegetation class") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.5), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

# patchwork1 <- (p2 + p1) / (p4 + p3) / (p6 + p5) +
patchwork1 <- (p2 + p1) / (p4 + p3) +

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Ground cover ----
# grcov <- construct_effects(learner_nonski, learner_ski, feature = "ground_cover")
# # Extract data for plotting
# nonski_data <- grcov$nonski$results
# ski_data <- grcov$ski$results
# # Calculate mean and median values
# summary_data <- nonski_data %>%
#   group_by(ground_cover) %>%
#   summarize(mean_value = mean(.value), median_value = median(.value))
#
# # Plot the results for non-ski slopes with customized mean and median lines
# p5 <- ggplot(nonski_data, aes(x = ground_cover, y = .value)) +
#   theme_bw()+
#   geom_line(aes(group = .id), alpha = 0.3) +  # ICE lines
#   geom_line(data = summary_data, aes(y = mean_value), color = "#A27146", size = 1.2) +  # Mean line
#   geom_line(data = summary_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") +  # Median line
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   labs(x = "ground cover")
#
# summary_data <- ski_data %>%
#   group_by(ground_cover) %>%
#   summarize(mean_value = mean(.value), median_value = median(.value))
#
# p6 <- ggplot(ski_data, aes(x = ground_cover, y = .value)) +
#   theme_bw()+
#   geom_line(aes(group = .id), alpha = 0.3) +  # ICE lines
#   geom_line(data = summary_data, aes(y = mean_value), color = "#33ccff", size = 1.2) +  # Mean line
#   geom_line(data = summary_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") +  # Median line
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   labs(x = "ground cover")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Topography
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Geomorphon ----
# geomorphon <- construct_effects(learner_nonski, learner_ski, feature = "geomorphon")
#
# geo_nonski_data <- geomorphon$nonski$results
# geo_ski_data <- geomorphon$ski$results
# # Define custom colors
# custom_colors_ski <- c("trench"= "#33ccff", "plateau"= "#33ccff", "escarpment"= "#33ccff", "mid-slope"= "#33ccff", "ridge"= "#33ccff", "upper-slope"= "#33ccff", "slope-flattening"= "#33ccff", "lower-slope"= "#33ccff")
# custom_colors_ref <- c("trench"= "#A27146", "plateau"= "#A27146", "escarpment"= "#A27146", "mid-slope"= "#A27146", "ridge"= "#A27146", "upper-slope"= "#A27146", "slope-flattening"= "#A27146", "lower-slope"= "#A27146")
#
# p1 <- ggplot(geo_nonski_data, aes(x = geomorphon, y = .value, fill = geomorphon)) +
#   geom_boxplot() +
#   theme_bw()+
#   scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ref) +
#   labs(title = "reference", x = "geomorphon") +
#   theme(text = element_text(size=16),axis.title.y = element_text(vjust = 0.5,size = 16),axis.title.x = element_text(vjust = 0.5,size = 16),plot.title = element_text(
#     size = rel(1.1), hjust = 0.5,# face = "bold",
#     margin = margin(t = 10, b = 20, unit = "pt")
#   )) +
#   theme(legend.position = "none")
#
# p2 <- ggplot(geo_ski_data, aes(x = geomorphon, y = .value, fill = geomorphon)) +
#   geom_boxplot() +
#   theme_bw()+
#   #scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
#   scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ski) +
#   labs(title = "ski slopes", x = "geomorphon") +
#   theme(text = element_text(size=16),axis.title.y = element_text(vjust = 0.5,size = 16),axis.title.x = element_text(vjust = 0.5,size = 16),plot.title = element_text(
#     size = rel(1.1), hjust = 0.5,# face = "bold",
#     margin = margin(t = 10, b = 20, unit = "pt")
#   )) +
#   theme(legend.position = "none")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Slope ----
slope <- construct_effects(learner_nonski, learner_ski, feature = "slope")
nonski_data <- slope$nonski$results
ski_data <- slope$ski$results
# Calculate mean and median values
summary_data <- nonski_data %>%
  group_by(slope) %>%
  summarize(mean_value = mean(.value), median_value = median(.value))

p3 <- ggplot(nonski_data, aes(x = slope, y = .value)) +
  theme_bw() +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = summary_data, aes(y = mean_value), color = "#A27146", size = 1.2) + # Mean line
  geom_line(data = summary_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "reference", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

summary_data <- ski_data %>%
  group_by(slope) %>%
  summarize(mean_value = mean(.value), median_value = median(.value))

p4 <- ggplot(ski_data, aes(x = slope, y = .value)) +
  theme_bw() +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = summary_data, aes(y = mean_value), color = "#33ccff", size = 1.2) + # Mean line
  geom_line(data = summary_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "ski slope", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")


patchwork2 <- (p4 + p3) +
  plot_annotation("Topographic feature variable", theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
ggsave("plt/figski10.png", patchwork2, device = png, height = 10, width = 22.5, dpi = 300, units = "cm")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Geology
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Geological class ----
geol_class <- construct_effects(learner_nonski, learner_ski, feature = "geol_class")
geol_nonski_data <- geol_class$nonski$results
geol_ski_data <- geol_class$ski$results
# Define custom colors
custom_colors_ski <- c("Cover layer on metamorphic bedrock" = "#33ccff", "Glacial sediment" = "#33ccff", "Glacial sediment, potentially very loose" = "#33ccff", "Cover layer on carbonate sediment" = "#33ccff", "Cover layer on carbonate bedrock" = "#33ccff", "Talus/Scree" = "#33ccff", "Anthropogenic debris" = "#33ccff", "Glacial sediment, possibly solidified" = "#33ccff")
custom_colors_ref <- c("Cover layer on metamorphic bedrock" = "#A27146", "Glacial sediment" = "#A27146", "Glacial sediment, potentially very loose" = "#A27146", "Cover layer on carbonate sediment" = "#A27146", "Cover layer on carbonate bedrock" = "#A27146", "Talus/Scree" = "#A27146", "Anthropogenic debris" = "#A27146", "Glacial sediment, possibly solidified" = "#A27146")

p1 <- ggplot(geol_nonski_data, aes(x = geol_class, y = .value, fill = geol_class)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_x_discrete(labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(title = "reference", x = "geological class") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

p2 <- ggplot(geol_ski_data, aes(x = geol_class, y = .value, fill = geol_class)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(title = "ski slope", x = "geological class") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Skeleton ----
skeleton <- construct_effects(learner_nonski, learner_ski, feature = "skeleton")
nonski_data <- skeleton$nonski$results
ski_data <- skeleton$ski$results
# Calculate mean and median values
summary_data <- nonski_data %>%
  group_by(skeleton) %>%
  summarize(mean_value = mean(.value), median_value = median(.value))

p3 <- ggplot(nonski_data, aes(x = skeleton, y = .value)) +
  theme_bw() +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = summary_data, aes(y = mean_value), color = "#A27146", size = 1.2) + # Mean line
  geom_line(data = summary_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")
summary_data <- ski_data %>%
  group_by(skeleton) %>%
  summarize(mean_value = mean(.value), median_value = median(.value))

p4 <- ggplot(ski_data, aes(x = skeleton, y = .value)) +
  theme_bw() +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = summary_data, aes(y = mean_value), color = "#33ccff", size = 1.2) + # Mean line
  geom_line(data = summary_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Soil texture ----
soiltext <- construct_effects(learner_nonski, learner_ski, feature = "soiltexture")
soilt_nonski_data <- soiltext$nonski$results
soilt_ski_data <- soiltext$ski$results
soilt_nonski_data$soiltexture <- factor(soilt_nonski_data$soiltexture, levels = c("S", "uS", "lS", "sU", "U", "lU", "sL", "uL"))
soilt_ski_data$soiltexture <- factor(soilt_ski_data$soiltexture, levels = c("S", "uS", "lS", "sU", "U", "lU", "sL", "uL"))
# Define custom colors
custom_colors_ski <- c("lS" = "#33ccff", "lU" = "#33ccff", "S" = "#33ccff", "sL" = "#33ccff", "sU" = "#33ccff", "uL" = "#33ccff", "uS" = "#33ccff")
custom_colors_ref <- c("lS" = "#A27146", "lU" = "#A27146", "S" = "#A27146", "sL" = "#A27146", "sU" = "#A27146", "uL" = "#A27146", "uS" = "#A27146")
p5 <- ggplot(soilt_nonski_data, aes(x = soiltexture, y = .value, fill = soiltexture)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(x = "texture") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

p6 <- ggplot(soilt_ski_data, aes(x = soiltexture, y = .value, fill = soiltexture)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(x = "texture") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")
patchwork3 <- (p2 + p1) / (p4 + p3) / (p6 + p5) +
  plot_annotation("Geological feature variables", theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
ggsave("plt/figski8.png", patchwork3, device = png, height = 30, width = 25, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# GROUP: Soil
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Soil structure ----
erd <- construct_effects(learner_nonski, learner_ski, feature = "embedded_rock_type")
erd_nonski_data <- erd$nonski$results
erd_ski_data <- erd$ski$results
# Define custom colors
custom_colors_ski <- c("LOC" = "#33ccff", "EHO" = "#33ccff", "EGE" = "#33ccff", "GS" = "#33ccff")
custom_colors_ref <- c("LOC" = "#A27146", "EHO" = "#A27146", "EGE" = "#A27146", "GS" = "#A27146")


p1 <- ggplot(erd_nonski_data, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  scale_x_discrete(labels = c("cohesive", "intermediate", "loose")) +
  labs(title = "reference", x = "soil structure") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

p2 <- ggplot(erd_ski_data, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("cohesive", "intermediate", "loose")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(title = "ski slope", x = "soil structure") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

# Soil class ----

soilclass <- construct_effects(learner_nonski, learner_ski, feature = "soil_class")
soilcl_nonski_data <- soilclass$nonski$results
soilcl_ski_data <- soilclass$ski$results
# Define custom colors
custom_colors_ski <- c(
  "regosol_leptosol" = "#33ccff",
  "regosol_phaeozem_umbrisol_folic-histosol" = "#33ccff",
  "cambisol_cambic-phaeozem_cambic-umbrisol" = "#33ccff",
  "albic-podzol_entic-podzol" = "#33ccff",
  "cambisol_cambic-phaeozem" = "#33ccff",
  "regosol" = "#33ccff",
  "disturbed_soil" = "#33ccff",
  "planosol-stagnosol" = "#33ccff",
  "fluvisol" = "#33ccff",
  "gleysol" = "#33ccff",
  "solonchak" = "#33ccff",
  "rheic_histosol" = "#33ccff",
  "subaquatic-fluvisol" = "#33ccff", "unknown" = "#33ccff"
)

custom_colors_ref <- c(
  "regosol_leptosol" = "#A27146",
  "regosol_phaeozem_umbrisol_folic-histosol" = "#A27146",
  "cambisol_cambic-phaeozem_cambic-umbrisol" = "#A27146",
  "albic-podzol_entic-podzol" = "#A27146",
  "cambisol_cambic-phaeozem" = "#A27146",
  "regosol" = "#A27146",
  "disturbed_soil" = "#A27146",
  "planosol-stagnosol" = "#A27146",
  "fluvisol" = "#A27146",
  "gleysol" = "#A27146",
  "solonchak" = "#A27146",
  "rheic_histosol" = "#A27146",
  "subaquatic-fluvisol" = "#A27146", "unknown" = "#A27146"
)

p3 <- ggplot(soilcl_nonski_data, aes(x = soil_class, y = .value, fill = soil_class)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("APE", "CCP", "CCU", "DS", "PS", "RPUF")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(x = "soil class") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

p4 <- ggplot(soilcl_ski_data, aes(x = soil_class, y = .value, fill = soil_class)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("APE", "CCU", "DS")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(x = "soil class") +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Saturation ----
saturation <- construct_effects(learner_nonski, learner_ski, feature = "sd_delta")
nonski_data <- saturation$nonski$results
ski_data <- saturation$ski$results
# Calculate mean and median values
summary_data <- nonski_data %>%
  group_by(sd_delta) %>%
  summarize(mean_value = mean(.value), median_value = median(.value))

p5 <- ggplot(nonski_data, aes(x = sd_delta, y = .value)) +
  theme_bw() +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = summary_data, aes(y = mean_value), color = "#A27146", size = 1.2) + # Mean line
  geom_line(data = summary_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "saturation deficit difference [Vol.%]") +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")
summary_data <- ski_data %>%
  group_by(sd_delta) %>%
  summarize(mean_value = mean(.value), median_value = median(.value))

p6 <- ggplot(ski_data, aes(x = sd_delta, y = .value)) +
  theme_bw() +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = summary_data, aes(y = mean_value), color = "#33ccff", size = 1.2) + # Mean line
  geom_line(data = summary_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") + # Median line
  # geom_rug(sides = "b", position="jitter", alpha = 0.5) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "saturation deficit difference [Vol.%]") +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme(text = element_text(size = 16), axis.title.y = element_text(vjust = 0.5, size = 16), axis.title.x = element_text(vjust = 0.5, size = 16), plot.title = element_text(
    size = rel(1.1), hjust = 0.5, # face = "bold",
    margin = margin(t = 10, b = 20, unit = "pt")
  )) +
  theme(legend.position = "none")
patchwork5 <- (p2 + p1) / (p4 + p3) / (p6 + p5) +
  plot_annotation("Soil feature variables", theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
ggsave("plt/figski9.png", patchwork5, device = png, height = 30, width = 25, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Humus Type ----
# humus <- construct_effects(learner_nonski, learner_ski, feature = "humus_type")
#
# humus_nonski_data <- humus$nonski$results
# humus_ski_data <- humus$ski$results
# # Define custom colors
# custom_colors_ski <-c("moder"= "#33ccff", "mull"= "#33ccff", "mor"= "#33ccff", "none"= "#33ccff")
#
# custom_colors_ref <-c("moder"= "#A27146", "mull"= "#A27146", "mor"= "#A27146", "none"= "#A27146")
#
# p5 <- ggplot(humus_nonski_data, aes(x = humus_type, y = .value, fill = humus_type)) +
#   geom_boxplot() +
#   theme_bw()+
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ref) +
#   labs(x = "humus type") +
#   theme(plot.title = element_text(
#     size = rel(1.2), face = "bold", hjust = 0.5,
#     margin = margin(t = 10, b = 20, unit = "pt")
#   )) +
#   theme(legend.position = "none")
#
# p6 <- ggplot(humus_ski_data, aes(x = humus_type, y = .value, fill = humus_type)) +
#   geom_boxplot() +
#   theme_bw()+
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ski) +
#   labs(x = "humus type") +
#   theme(plot.title = element_text(
#     size = rel(1.2), face = "bold", hjust = 0.5,
#     margin = margin(t = 10, b = 20, unit = "pt")
#   )) +
#   theme(legend.position = "none")
#
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# # Soil depth ----
#
# soil_depth <- construct_effects(learner_nonski, learner_ski, feature = "soil_depth")
# soild_nonski_data <- soil_depth$nonski$results
# soild_ski_data <- soil_depth$ski$results
# # Define custom colors
# custom_colors_ski <-c("flachgründig"= "#33ccff80", "mittelgründig"= "#33ccffBF", "sehr flachgründig"= "#33ccff40", "tiefgründig"= "#33ccff")
# custom_colors_ref <-c("flachgründig"= "#A2714680", "mittelgründig"= "#A27146BF", "sehr flachgründig"= "#A2714640", "tiefgründig"= "#A27146")
#
# p1 <- ggplot(soild_nonski_data, aes(x = soil_depth, y = .value, fill = soil_depth)) +
#   geom_boxplot() +
#   theme_bw()+
#   scale_x_discrete(labels = c("very shallow soil", "shallow soil", "moderately deep soil", "deep soil")) +
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ref) +
#   labs(title = "reference slopes",x = "soil depth") +
#   theme(plot.title = element_text(
#     size = rel(1.2), face = "bold", hjust = 0.5,
#     margin = margin(t = 10, b = 20, unit = "pt")
#   )) +
#   theme(legend.position = "none")
#
# p2 <- ggplot(soild_ski_data, aes(x = soil_depth, y = .value, fill = soil_depth)) +
#   geom_boxplot() +
#   theme_bw()+
#   scale_x_discrete(labels = c("very shallow soil", "shallow soil", "moderately deep soil", "deep soil")) +
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ski) +
#   labs(title = "ski slopes",x = "soil depth") +
#   theme(plot.title = element_text(
#     size = rel(1.2), face = "bold", hjust = 0.5,
#     margin = margin(t = 10, b = 20, unit = "pt")
#   )) +
#   theme(legend.position = "none")
#
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# # Bulk density ----
#
# bd <- construct_effects(learner_nonski, learner_ski, feature = "bulk_density")
# nonski_data <- bd$nonski$results
# ski_data <- bd$ski$results
# # Calculate mean and median values
# summary_data <- nonski_data %>%
#   group_by(bulk_density) %>%
#   summarize(mean_value = mean(.value), median_value = median(.value))
#
# p3 <- ggplot(nonski_data, aes(x = bulk_density, y = .value)) +
#   theme_bw()+
#   geom_line(aes(group = .id), alpha = 0.3) +  # ICE lines
#   geom_line(data = summary_data, aes(y = mean_value), color = "#A27146", size = 1.2) +  # Mean line
#   geom_line(data = summary_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") +  # Median line
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   labs(x = "bulk density")
#
#
# summary_data <- ski_data %>%
#   group_by(bulk_density) %>%
#   summarize(mean_value = mean(.value), median_value = median(.value))
#
# p4 <- ggplot(ski_data, aes(x = bulk_density, y = .value)) +
#   theme_bw()+
#   geom_line(aes(group = .id), alpha = 0.3) +  # ICE lines
#   geom_line(data = summary_data, aes(y = mean_value), color = "#33ccff", size = 1.2) +  # Mean line
#   geom_line(data = summary_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") +  # Median line
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   labs(x = "bulk density")
#
# patchwork6 <- (p2 + p1) / (p4 + p3) +
#   plot_annotation("soil parameters (2)", theme = theme(plot.title = element_text(hjust = 0.5)))
# ggsave("plt/figski6.png", patchwork6, device = png, height = 20, width = 25, dpi = 300, units = "cm")
