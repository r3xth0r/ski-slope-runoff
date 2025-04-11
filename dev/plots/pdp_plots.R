# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Partial dependence plots
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Land use
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Pasture ----

# Construct effects
pasture <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "pasture")

# Define custom colors
custom_colors_ski <- c("no" = "#33ccff40", "low" = "#33ccff80", "medium" = "#33ccffBF", "intensive" = "#33ccff")
custom_colors_ref <- c("no" = "#A2714640", "low" = "#A2714680", "medium" = "#A27146BF", "intensive" = "#A27146")

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

# Define custom colors
custom_colors_ski <- c("forest" = "#33ccff", "grassland" = "#33ccff", "shrubs" = "#33ccff")
custom_colors_ref <- c("forest" = "#A27146", "grassland" = "#A27146", "shrubs" = "#A27146")

p3 <- ggplot(vc_reference_data, aes(x = vegetation_class, y = .value, fill = vegetation_class)) +
  geom_boxplot() +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  scale_y_continuous(
    name = bquote(Psi[constant]), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(x = "vegetation class") +
  theme_ski()

p4 <- ggplot(vc_ski_data, aes(x = vegetation_class, y = .value, fill = vegetation_class)) +
  geom_boxplot() +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  # scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(
    name = bquote(Psi[constant]), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(x = "vegetation class") +
  theme_ski()

patchwork1 <- (p2 + p1) / (p4 + p3) +
  plot_annotation("Landuse feature variables",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )
ggsave("plt/fig_07.png", patchwork1, device = png, height = 20, width = 25, dpi = 300, units = "cm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Ground cover ----
# grcov <- construct_effects(learner_reference, learner_ski, feature = "ground_cover")
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
#   geom_line(aes(group = .id), alpha = 0.3) +  # ICE lines
#   geom_line(data = summary_data, aes(y = mean_value), color = "#A27146", size = 1.2) +  # Mean line
#   geom_line(data = summary_data, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") +  # Median line
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   labs(x = "ground cover") +
#   theme_ski()
#
# summary_data <- ski_data %>%
#   group_by(ground_cover) %>%
#   summarize(mean_value = mean(.value), median_value = median(.value))
#
# p6 <- ggplot(ski_data, aes(x = ground_cover, y = .value)) +
#   geom_line(aes(group = .id), alpha = 0.3) +  # ICE lines
#   geom_line(data = summary_data, aes(y = mean_value), color = "#33ccff", size = 1.2) +  # Mean line
#   geom_line(data = summary_data, aes(y = median_value), color = "#33ccff80", size = 1.2, linetype = "dashed") +  # Median line
#   scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   labs(x = "ground cover") +
#   theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Topography
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Geomorphon ----
# geomorphon <- construct_effects(learner_reference, learner_ski, feature = "geomorphon")
#
# geo_nonski_data <- geomorphon$nonski$results
# geo_ski_data <- geomorphon$ski$results
# # Define custom colors
# custom_colors_ski <- c("trench"= "#33ccff", "plateau"= "#33ccff", "escarpment"= "#33ccff", "mid-slope"= "#33ccff", "ridge"= "#33ccff", "upper-slope"= "#33ccff", "slope-flattening"= "#33ccff", "lower-slope"= "#33ccff")
# custom_colors_ref <- c("trench"= "#A27146", "plateau"= "#A27146", "escarpment"= "#A27146", "mid-slope"= "#A27146", "ridge"= "#A27146", "upper-slope"= "#A27146", "slope-flattening"= "#A27146", "lower-slope"= "#A27146")
#
# p1 <- ggplot(geo_nonski_data, aes(x = geomorphon, y = .value, fill = geomorphon)) +
#   geom_boxplot() +
#   scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ref) +
#   labs(title = "reference", x = "geomorphon") +
#   theme_ski()
#
# p2 <- ggplot(geo_ski_data, aes(x = geomorphon, y = .value, fill = geomorphon)) +
#   geom_boxplot() +
#   #scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
#   scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
#   scale_fill_manual(values = custom_colors_ski) +
#   labs(title = "ski slopes", x = "geomorphon") +
#   theme_ski()
#
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
  geom_line(data = ref_summary, aes(y = mean_value), color = "#A27146", linewidth = 1.2) + # Mean line
  geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", linewidth = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "reference", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme_ski()

p6 <- ggplot(slope$ski, aes(x = slope, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ski_summary, aes(y = mean_value), color = "#33ccff", linewidth = 1.2) + # Mean line
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

# Define custom colors
custom_colors_ski <- c("Cover layer on metamorphic bedrock" = "#33ccff", "Glacial sediment" = "#33ccff", "Glacial sediment, potentially very loose" = "#33ccff", "Cover layer on carbonate sediment" = "#33ccff", "Cover layer on carbonate bedrock" = "#33ccff", "Talus/Scree" = "#33ccff", "Anthropogenic debris" = "#33ccff", "Glacial sediment, possibly solidified" = "#33ccff")
custom_colors_ref <- c("Cover layer on metamorphic bedrock" = "#A27146", "Glacial sediment" = "#A27146", "Glacial sediment, potentially very loose" = "#A27146", "Cover layer on carbonate sediment" = "#A27146", "Cover layer on carbonate bedrock" = "#A27146", "Talus/Scree" = "#A27146", "Anthropogenic debris" = "#A27146", "Glacial sediment, possibly solidified" = "#A27146")

p7 <- ggplot(geol_class$reference, aes(x = geol_class, y = .value, fill = geol_class)) +
  geom_boxplot() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_x_discrete(labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(title = "reference", x = "geological class") +
  theme_ski()

p8 <- ggplot(geol_class$ski, aes(x = geol_class, y = .value, fill = geol_class)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
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
  geom_line(data = ref_summary, aes(y = mean_value), color = "#A27146", size = 1.2) + # Mean line
  geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_ski()


p10 <- ggplot(skeleton$ski, aes(x = skeleton, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ski_summary, aes(y = mean_value), color = "#33ccff", size = 1.2) + # Mean line
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

# Define custom colors
custom_colors_ski <- c("lS" = "#33ccff", "lU" = "#33ccff", "S" = "#33ccff", "sL" = "#33ccff", "sU" = "#33ccff", "uL" = "#33ccff", "uS" = "#33ccff")
custom_colors_ref <- c("lS" = "#A27146", "lU" = "#A27146", "S" = "#A27146", "sL" = "#A27146", "sU" = "#A27146", "uL" = "#A27146", "uS" = "#A27146")

p11 <- ggplot(soilt_reference, aes(x = soiltexture, y = .value, fill = soiltexture)) +
  geom_boxplot() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(x = "texture") +
  theme_ski()

p12 <- ggplot(soilt_ski, aes(x = soiltexture, y = .value, fill = soiltexture)) +
  geom_boxplot() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
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

# Define custom colors
custom_colors_ski <- c("LOC" = "#33ccff", "EHO" = "#33ccff", "EGE" = "#33ccff", "GS" = "#33ccff")
custom_colors_ref <- c("LOC" = "#A27146", "EHO" = "#A27146", "EGE" = "#A27146", "GS" = "#A27146")

p13 <- ggplot(erd$reference, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot() +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  scale_x_discrete(labels = c("cohesive", "intermediate", "loose")) +
  labs(title = "reference", x = "soil structure") +
  theme_ski()

p14 <- ggplot(erd$ski, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("cohesive", "intermediate", "loose")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
  labs(title = "ski slope", x = "soil structure") +
  theme_ski()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil class ----

soilclass <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soil_class")

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

p15 <- ggplot(soilclass$reference, aes(x = soil_class, y = .value, fill = soil_class)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("APE", "CCP", "CCU", "DS", "PS", "RPUF")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ref) +
  labs(x = "soil class") +
  theme_ski()

p16 <- ggplot(soilclass$ski, aes(x = soil_class, y = .value, fill = soil_class)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("APE", "CCU", "DS")) +
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = custom_colors_ski) +
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
  geom_line(data = ref_summary, aes(y = mean_value), color = "#A27146", size = 1.2) + # Mean line
  geom_line(data = ref_summary, aes(y = median_value), color = "#A2714680", size = 1.2, linetype = "dashed") + # Median line
  scale_y_continuous(name = bquote(Psi[constant]), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "saturation deficit difference [Vol.%]") +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme_ski()

p18 <- ggplot(saturation$ski, aes(x = sd_delta, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3) + # ICE lines
  geom_line(data = ski_summary, aes(y = mean_value), color = "#33ccff", size = 1.2) + # Mean line
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil depth ----
