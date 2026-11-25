# Jennifer Selgrath
# California Marine Sanctuary Foundation
# California Ocean Access: Benefits and Barriers (bab)

# Goal : white map of CA for WSN 2026 presentation
# -----------------------------------------
library(tidyverse)
library(sf)
# -----------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# Load counties and dissolve internal borders into a single CA state polygon
ca_outline <- st_read("./gis_results/ca_outline.gpkg") 

plot(st_geometry(ca_outline), col = "grey", border = "darkgrey")

# Shared transparent theme (no scale bar, north arrow, or legend)
theme_ca_transparent <- theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

# 1. Map A: White Outline Only (Transparent Interior)
p_ca_outline <- ggplot() +
  geom_sf(
    data = ca_outline,
    fill = NA,
    color = "white",
    linewidth = 0.8
  ) +
  theme_ca_transparent

ggsave(
  "./doc/ca_outline_white.png",
  plot = p_ca_outline,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "transparent"
)

# 2. Map B: Solid White Fill (White Silhouette)
p_ca_filled <- ggplot() +
  geom_sf(
    data = ca_outline,
    fill = "white",
    color = "white",
    linewidth = 0.8
  ) +
  theme_ca_transparent

ggsave(
  "./doc/ca_filled_white.png",
  plot = p_ca_filled,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "transparent"
)




# 3. Map A: dark gray Outline Only (Transparent Interior)
p_ca_outline2 <- ggplot() +
  geom_sf(
    data = ca_outline,
    fill = NA,
    color = "#4E4E4E",
    linewidth = 0.8
  ) +
  theme_ca_transparent

ggsave(
  "./doc/ca_outline_gray.png",
  plot = p_ca_outline2,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "transparent"
)

# 4. Map B: Solid Grey Fill 
p_ca_filled2 <- ggplot() +
  geom_sf(
    data = ca_outline,
    fill = "#4E4E4E",
    color = "#4E4E4E",
    linewidth = 0.8
  ) +
  theme_ca_transparent

ggsave(
  "./doc/ca_filled_grey.png",
  plot = p_ca_filled2,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "transparent"
)

# 5. # map e: solid grey close to white - same are arc basemap

p_ca_filled3 <- ggplot() +
  geom_sf(
    data = ca_outline,
    fill = "#f7f7f7",
    color = "#f7f7f7",
    linewidth = 0.8
  ) +
  theme_ca_transparent

ggsave(
  "./doc/ca_filled_grey2.png",
  plot = p_ca_filled3,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "transparent"
)