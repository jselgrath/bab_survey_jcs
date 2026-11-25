# Jennifer Selgrath
# California Marine Sanctuary Foundation
# California Ocean Access: Benefits and Barriers (bab)

# Goal : map weighting variable
# -----------------------------------------

library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(viridis)
library(colorspace)
library(ggspatial)
library(ggnewscale)
# -----------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# ----------------------------------------------------------

# 1. Load spatial layers (all transformed to EPSG:3310) -----
d_weight <- st_read("./gis_results/california.gpkg", layer = "weighting") %>%
  st_transform(3310) %>%
  st_make_valid()

# Dissolve weighting polygons so internal county borders disappear within
# multi-county groups ("other coastal", "Inland - Central & Northern Regions", etc.),
# while focal counties remain individual polygons
d_weight_dissolved <- d_weight %>%
  group_by(weighting, co_survey, co_coastal) %>%
  summarise(.groups = "drop")

# Surveyed (focal) counties for dashed outline
d_survey_counties <- d_weight %>%
  filter(co_survey == 1)

# State MPAs
d_mpa <- st_read("./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp") %>%
  select(name = NAME, ccr = CCR, full_name = FULLNAME, type = Type) %>%
  st_transform(3310)

# National Marine Sanctuaries
d_nms <- st_read("./gis_data/national_marine_sanctuaries_2025/national_marine_sanctuaries.shp") %>%
  select(name = sitename, siteurl) %>%
  st_transform(3310) %>%
  filter(name %in% c(
    "Channel Islands National Marine Sanctuary",
    "Cordell Bank National Marine Sanctuary",
    "Greater Farallones National Marine Sanctuary",
    "Monterey Bay National Marine Sanctuary",
    "Chumash Heritage National Marine Sanctuary"
  ))

# ---------------------------------------------------------------------------
# 2. Set Viridis (focal) + Greyscale (non-focal) colors ------
# ---------------------------------------------------------------------------

# Order focal coastal counties north-to-south (by latitude)
focal_levels <- d_weight_dissolved %>%
  filter(co_survey == 1) %>%
  mutate(lat = st_coordinates(st_centroid(geom))[, 2]) %>%
  arrange(desc(lat)) %>%
  pull(weighting)

other_coastal_levels <- d_weight_dissolved %>%
  filter(co_survey == 0, co_coastal == "coastal") %>%
  pull(weighting)

inland_levels <- d_weight_dissolved %>%
  filter(co_coastal == "inland") %>%
  arrange(weighting) %>%
  pull(weighting)

non_focal_levels <- c(other_coastal_levels, inland_levels)
all_levels <- c(focal_levels, non_focal_levels)

# Coastal Focal Counties: Viridis mako teal-blue shades
focal_cols <- setNames(
  viridis(length(focal_levels), option = "mako", begin = 0.28, end = 0.62),
  focal_levels
)

# Non-Focal Strata (Other Coastal + Inland): Distinct shades of light-to-medium grey
# Ranges from grey88 (lightest, assigned to "other coastal") to grey62 (darkest inland)
non_focal_cols <- setNames(
  grey.colors(length(non_focal_levels), start = 0.62, end = 0.88),
  non_focal_levels
)

weight_palette <- c(focal_cols, non_focal_cols)

# Apply factor order for clean legend grouping
d_weight_dissolved <- d_weight_dissolved %>%
  mutate(weighting = factor(weighting, levels = all_levels))

# 3. Map Generation ----------------------------------------
ggplot() +
  # Base Layer: Dissolved Weighting Regions
  geom_sf(
    data = d_weight_dissolved,
    aes(fill = weighting),
    color = "grey35",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = weight_palette,
    name = "Weighting Stratum",
    guide = guide_legend(order = 1, ncol = 1)
  ) +

  # Outlined Focal Counties (dashed border)
  geom_sf(
    data = d_survey_counties,
    aes(linetype = "Focal County"),
    fill = NA,
    color = "grey20",
    linewidth = 0.45
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Focal County" = "twodash"),
    guide = guide_legend(
      order = 4,
      override.aes = list(color = "grey20", linewidth = 0.7, fill = NA)
    )
  ) +

  # National Marine Sanctuaries (NMS)
  # new_scale_fill() +
  # new_scale_color() +
  # geom_sf(
  #   data = d_nms,
  #   aes(fill = "National Marine Sanctuaries", color = "National Marine Sanctuaries"),
  #   linewidth = 0.35,
  #   alpha = 0.55
  # ) +
  # scale_fill_manual(
  #   values = c("National Marine Sanctuaries" = "#5C8AB4"),
  #   name = NULL,
  #   guide = guide_legend(
  #     order = 2,
  #     override.aes = list(color = "#5C8AB4", alpha = 0.55, linewidth = 0.5)
  #   )
  # ) +
  # scale_color_manual(
  #   values = c("National Marine Sanctuaries" = "#5C8AB4"),
  #   guide = "none"
  # ) +

  # State Marine Protected Areas (MPAs)
  # new_scale_fill() +
  # geom_sf(
  #   data = d_mpa,
  #   aes(fill = "State MPAs"),
  #   color = alpha("#073763", 0.6),
  #   linewidth = 0.2
  # ) +
  # scale_fill_manual(
  #   values = c("State MPAs" = alpha("#073763", 0.6)),
  #   name = NULL,
  #   guide = guide_legend(order = 3)
  # ) +

  # Scale Bar
  annotation_scale(
    location = "bl",
    pad_x = unit(2.8, "in"),
    pad_y = unit(0.2, "in"),
    unit_category = "imperial",
    width_hint = 0.18,
    style = "ticks",
    text_cex = 0.8
  ) +

  # North Arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(3.2, "in"),
    pad_y = unit(0.4, "in"),
    height = unit(0.5, "in"),
    width = unit(0.3, "in"),
    style = north_arrow_orienteering(text_size = 9)
  ) +

  # Map Title Annotation
  annotate(
    "text",
    x = I(0.46), y = I(0.82),
    label = "Strata for \nweighting",
    size = 11,
    fontface = "bold",
    color = "grey30",
    hjust = 0
  ) +

  # Theme Settings
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.05, 0.01),
    legend.justification = c(0, 0),
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8.5),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.04, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("./doc/bab_map_weighting.png", width = 9, height = 9, dpi = 300)
