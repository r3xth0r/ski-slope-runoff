# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Partial dependence plots (Fig. 07-09)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("mlr3")
library("iml")
library("dplyr")
library("ggplot2")
library("patchwork")

# Get data for ski slopes and reference slopes
dat_ski <- readRDS("dat/processed/dat_sd_delta_ski.rds")
dat_reference <- readRDS("dat/processed/dat_sd_delta_noski.rds")

# Load trained learners
learner_ski <- readRDS("dat/interim/random_forest/ranger_trained_ski.rds")
learner_reference <- readRDS("dat/interim/random_forest/ranger_trained_noski.rds")

# Source helper functions
source("dev/helper/construct_effects.R")
source("dev/helper/theme_ski.R")

# Flag to construct all plots or only the ones included in the manuscript
construct_all_plots <- FALSE

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Land use
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Pasture ----

# Construct effects
pasture <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "pasture")

# Define custom colors
custom_colors_ski <- c("no" = "#33ccff40", "low" = "#33ccff80", "medium" = "#33ccffBF", "intensive" = ski_col)
custom_colors_ref <- c("no" = "#A2714640", "low" = "#A2714680", "medium" = "#A27146BF", "intensive" = ref_col)

# Create a boxplot with different colors for each factor level
p1 <- ggplot(pasture$reference, aes(x = pasture, y = .value, fill = pasture)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(
    name = bquote(Psi[constant]), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(title = "reference", x = "pasture") +
  theme_ski()

p2 <- ggplot(pasture$ski, aes(x = pasture, y = .value, fill = pasture)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(
    name = bquote(Psi[constant]), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(title = "ski slope", x = "pasture") +
  theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Vegetation ----

vc <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "vegetation_class")

# Extract the data for plotting
vc_reference_data <- vc$reference |>
  as_tibble() |>
  mutate(vegetation_class = factor(vegetation_class, levels = c("forest", "shrubs", "grassland")))
vc_ski_data <- vc$ski |>
  as_tibble() |>
  mutate(vegetation_class = factor(vegetation_class, levels = c("forest", "shrubs", "grassland")))

p3 <- ggplot(vc_reference_data, aes(x = vegetation_class, y = .value, fill = vegetation_class)) +
  geom_boxplot(fill = ref_col) +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  scale_y_continuous(
    name = bquote(Psi[constant]), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  labs(x = "vegetation class") +
  theme_ski()

p4 <- ggplot(vc_ski_data, aes(x = vegetation_class, y = .value, fill = vegetation_class)) +
  geom_boxplot(fill = ski_col) +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  # scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(
    name = bquote(Psi[constant]), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  labs(x = "vegetation class") +
  theme_ski()

patchwork1 <- (p2 + p1) / (p4 + p3) +
  plot_annotation("Landuse feature variables",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )
ggsave("plt/fig_07.png", patchwork1, device = png, height = 20, width = 25, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Ground cover ----

if (construct_all_plots) {
  grcov <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "ground_cover")

  # Calculate mean and median values
  ref_summary <- grcov$reference |>
    group_by(ground_cover) |>
    summarize(mean_value = mean(.value), median_value = median(.value))
  ski_summary <- grcov$ski |>
    group_by(ground_cover) |>
    summarize(mean_value = mean(.value), median_value = median(.value))

  # Plot the results for reference slopes with customized mean and median lines
  po1 <- ggplot(grcov$reference, aes(x = ground_cover, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3) +
    geom_line(data = ref_summary, aes(y = mean_value), color = ref_col, linewidth = 1.2) +
    geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", linewidth = 1.2, linetype = "dashed") + # Median line
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "ground cover") +
    theme_ski()

  po2 <- ggplot(grcov$ski, aes(x = ground_cover, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3) +
    geom_line(data = ski_summary, aes(y = mean_value), color = ski_col, linewidth = 1.2) +
    geom_line(data = ski_summary, aes(y = median_value), color = "#33ccff80", linewidth = 1.2, linetype = "dashed") + # Median line
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "ground cover") +
    theme_ski()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Topography
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Geomorphon ----

if (construct_all_plots) {
  geomorphon <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "geomorphon")

  po3 <- ggplot(geomorphon$reference, aes(x = geomorphon, y = .value, fill = geomorphon)) +
    geom_boxplot(fill = ref_col) +
    scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(title = "reference", x = "geomorphon") +
    theme_ski()

  po4 <- ggplot(geomorphon$ski, aes(x = geomorphon, y = .value, fill = geomorphon)) +
    geom_boxplot(fill = ski_col) +
    # scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
    scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(title = "ski slopes", x = "geomorphon") +
    theme_ski()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Slope ----

slope <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "slope")

# Calculate mean and median values
ref_summary <- slope$reference |>
  group_by(slope) |>
  summarize(mean_value = mean(.value), median_value = median(.value))
ski_summary <- slope$ski |>
  group_by(slope) |>
  summarize(mean_value = mean(.value), median_value = median(.value))

p5 <- ggplot(slope$reference, aes(x = slope, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ref_summary, aes(y = mean_value), color = ref_col, linewidth = 1.2) + # Mean line
  geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", linewidth = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "reference", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme_ski()

p6 <- ggplot(slope$ski, aes(x = slope, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ski_summary, aes(y = mean_value), color = ski_col, linewidth = 1.2) + # Mean line
  geom_line(data = ski_summary, aes(y = median_value), color = "#33ccff80", linewidth = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "ski slope", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme_ski()

patchwork2 <- (p6 + p5) +
  plot_annotation("Topographic feature variables", theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
ggsave("plt/fig_10.png", patchwork2, device = png, height = 10, width = 22.5, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Geology
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Geological class ----
geol_class <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "geol_class")

p7 <- ggplot(geol_class$reference, aes(x = geol_class, y = .value, fill = geol_class)) +
  geom_boxplot(fill = ref_col) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_x_discrete(labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(title = "reference", x = "geological class") +
  theme_ski()

p8 <- ggplot(geol_class$ski, aes(x = geol_class, y = .value, fill = geol_class)) +
  geom_boxplot(fill = ski_col) +
  scale_x_discrete(labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "ski slope", x = "geological class") +
  theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Skeleton ----

skeleton <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "skeleton")

# Calculate mean and median values
ref_summary <- skeleton$reference |>
  group_by(skeleton) |>
  summarize(mean_value = mean(.value), median_value = median(.value))
ski_summary <- skeleton$ski |>
  group_by(skeleton) |>
  summarize(mean_value = mean(.value), median_value = median(.value))

p9 <- ggplot(skeleton$reference, aes(x = skeleton, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ref_summary, aes(y = mean_value), color = ref_col, size = 1.2) + # Mean line
  geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_ski()


p10 <- ggplot(skeleton$ski, aes(x = skeleton, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ski_summary, aes(y = mean_value), color = ski_col, size = 1.2) + # Mean line
  geom_line(data = ski_summary, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil texture ----

soiltext <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soiltexture")

soilt_reference <- soiltext$reference |>
  mutate(soiltexture = factor(soiltexture, levels = c("S", "uS", "lS", "sU", "U", "lU", "sL", "uL")))

soilt_ski <- soiltext$ski |>
  mutate(soiltexture = factor(soiltexture, levels = c("S", "uS", "lS", "sU", "U", "lU", "sL", "uL")))

p11 <- ggplot(soilt_reference, aes(x = soiltexture, y = .value, fill = soiltexture)) +
  geom_boxplot(fill = ref_col) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "texture") +
  theme_ski()

p12 <- ggplot(soilt_ski, aes(x = soiltexture, y = .value, fill = soiltexture)) +
  geom_boxplot(fill = ski_col) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "texture") +
  theme_ski()

patchwork3 <- (p8 + p7) / (p10 + p9) / (p12 + p11) +
  plot_annotation("Geological feature variables", theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
ggsave("plt/fig_08.png", patchwork3, device = png, height = 30, width = 25, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# GROUP: Soil
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil structure ----
erd <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "embedded_rock_type")

p13 <- ggplot(erd$reference, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot(fill = ref_col) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_x_discrete(labels = c("cohesive", "intermediate", "loose")) +
  labs(title = "reference", x = "soil structure") +
  theme_ski()

p14 <- ggplot(erd$ski, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot(fill = ski_col) +
  scale_x_discrete(labels = c("cohesive", "intermediate", "loose")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "ski slope", x = "soil structure") +
  theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil class ----

soilclass <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soil_class")

p15 <- ggplot(soilclass$reference, aes(x = soil_class, y = .value, fill = soil_class)) +
  geom_boxplot(fill = ref_col) +
  scale_x_discrete(labels = c("APE", "CCP", "CCU", "DS", "PS", "RPUF")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "soil class") +
  theme_ski()

p16 <- ggplot(soilclass$ski, aes(x = soil_class, y = .value, fill = soil_class)) +
  geom_boxplot(fill = ski_col) +
  scale_x_discrete(labels = c("APE", "CCU", "DS")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "soil class") +
  theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Saturation ----
saturation <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "sd_delta")

# Calculate mean and median values
ref_summary <- saturation$reference |>
  group_by(sd_delta) |>
  summarize(mean_value = mean(.value), median_value = median(.value))
ski_summary <- saturation$ski |>
  group_by(sd_delta) |>
  summarize(mean_value = mean(.value), median_value = median(.value))

p17 <- ggplot(saturation$reference, aes(x = sd_delta, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ref_summary, aes(y = mean_value), color = ref_col, size = 1.2) + # Mean line
  geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "saturation deficit difference [Vol.%]") +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme_ski()

p18 <- ggplot(saturation$ski, aes(x = sd_delta, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ski_summary, aes(y = mean_value), color = ski_col, size = 1.2) + # Mean line
  geom_line(data = ski_summary, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") + # Median line
  # geom_rug(sides = "b", position="jitter", alpha = 0.5) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "saturation deficit difference [Vol.%]") +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme_ski()

patchwork5 <- (p14 + p13) / (p16 + p15) / (p18 + p17) +
  plot_annotation("Soil feature variables", theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
ggsave("plt/fig_09.png", patchwork5, device = png, height = 30, width = 25, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Humus Type ----

if (construct_all_plots) {
  humus <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "humus_type")

  po5 <- ggplot(humus$reference, aes(x = humus_type, y = .value, fill = humus_type)) +
    geom_boxplot(fill = ref_col) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = custom_colors_ref) +
    labs(x = "humus type") +
    theme_ski()

  po6 <- ggplot(humus$ski, aes(x = humus_type, y = .value, fill = humus_type)) +
    geom_boxplot(fill = ski_col) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = custom_colors_ski) +
    labs(x = "humus type") +
    theme_ski()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil depth ----

if (construct_all_plots) {
  soil_depth <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soil_depth")

  # Define custom colors
  custom_colors_ski <- c(
    "flachgründig" = "#33ccff80", "mittelgründig" = "#33ccffBF",
    "sehr flachgründig" = "#33ccff40", "tiefgründig" = ski_col
  )
  custom_colors_ref <- c(
    "flachgründig" = "#A2714680", "mittelgründig" = "#A27146BF",
    "sehr flachgründig" = "#A2714640", "tiefgründig" = ref_col
  )

  po7 <- ggplot(soil_depth$reference, aes(x = soil_depth, y = .value, fill = soil_depth)) +
    geom_boxplot() +
    scale_x_discrete(labels = c("very shallow soil", "shallow soil", "moderately deep soil", "deep soil")) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = custom_colors_ref) +
    labs(title = "reference slopes", x = "soil depth") +
    theme_ski()

  po8 <- ggplot(soil_depth$ski, aes(x = soil_depth, y = .value, fill = soil_depth)) +
    geom_boxplot() +
    scale_x_discrete(labels = c("very shallow soil", "shallow soil", "moderately deep soil", "deep soil")) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = custom_colors_ski) +
    labs(title = "ski slopes", x = "soil depth") +
    theme_ski()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Bulk density ----

if (construct_all_plots) {
  bd <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "bulk_density")

  # Calculate mean and median values
  ref_data <- bd$reference |>
    group_by(bulk_density) |>
    summarize(mean_value = mean(.value), median_value = median(.value))
  ski_data <- bd$ski |>
    group_by(bulk_density) |>
    summarize(mean_value = mean(.value), median_value = median(.value))

  po9 <- ggplot(bd$reference, aes(x = bulk_density, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
    geom_line(data = ref_data, aes(y = mean_value), color = ref_col, size = 1.2) + # Mean line
    geom_line(data = ref_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "bulk density") +
    theme_ski()

  po0 <- ggplot(bd$ski, aes(x = bulk_density, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
    geom_line(data = ski_data, aes(y = mean_value), color = ski_col, size = 1.2) + # Mean line
    geom_line(data = ski_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") + # Median line
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "bulk density") +
    theme_ski()

  patchwork_o2 <- (po6 + po5) / (po8 + po7) / (po0 + po9) +
    plot_annotation("additional soil  parameters", theme = theme(plot.title = element_text(hjust = 0.5)))
  ggsave("plt/fig_effects_soil.png", patchwork_o2, device = png, height = 30, width = 25, dpi = 300, units = "cm")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
