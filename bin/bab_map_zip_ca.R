# Jennifer Selgrath
# California Marine Sanctuary Foundation
# California Ocean Access: Benefits and Barriers (bab)

# Goal : to map zip codes where California respondents lived

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)
library(colorspace)
library(sf)
library(scales)
library(ggspatial) #  north arrow and scale bar
library(ggnewscale)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

#zip codes in CA ------
d0<-st_read("./gis_data/zip_codes_ca/california_zip_codes4.shp")%>%
  mutate(zip_code=as.numeric(zip_code))%>%
  glimpse()

plot(d0[1])

# mpas -----------
d00<-st_read("./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp")%>%
  select(name=NAME,ccr=CCR,full_name=FULLNAME,type=Type,ccr_int=CCR_Int,short_name=SHORTNAME,acres=Acres,hectares=Hectares)%>%
  st_transform(crs = st_crs(d0))%>%
  glimpse()
st_crs(d00)

# sanctuaries ------------
d000<-st_read("./gis_data/national_marine_sanctuaries_2025/national_marine_sanctuaries.shp")%>%
  select(name=sitename,siteurl)%>%
  st_transform(crs = st_crs(d0))%>%
  filter(name=="Channel Islands National Marine Sanctuary" | name=="Cordell Bank National Marine Sanctuary" |name=="Greater Farallones National Marine Sanctuary" |name=="Monterey Bay National Marine Sanctuary" |name=="Chumash Heritage National Marine Sanctuary" )%>%
  glimpse()
# st_crs(d000)
# plot(d000[1])

unique(d000$name)

# counties ----------
d0000 <- st_read("./gis_data/ca_counties/ca_counties.shp") %>%
  st_transform(crs = st_crs(d0)) %>%
  mutate(
    county = NAME, 
    co_long = NAMELSAD,
    co_survey = if_else(county %in% c("San Diego", "Los Angeles", "Ventura", 
                                      "Santa Barbara", "Monterey", "Santa Cruz", 
                                      "San Francisco", "Sonoma", "Humboldt", "Del Norte"), 1, 0),
    co_phase = if_else(county %in% c("San Diego", "Los Angeles", "Sonoma", "Humboldt", "Del Norte"), "Phase II", "Phase I"),
    co_phase = if_else(co_survey == 1, co_phase, NA_character_)
  ) %>%
  select(county, co_long, co_survey, co_phase) %>%
  glimpse()
# st_crs(d000)
plot(d0000[4])

# surveyed counties only ------
d_survey_counties <- d0000 %>% 
  filter(co_survey == 1)


# survey data ----------------------------
d1<-read_csv("./results/data_long11.csv")%>%
  glimpse()

# summarize zip codes > counts ------------
zip_counts <- d1 %>% 
  filter(!is.na(QDemographic_PrimaryZip)) %>% 
  mutate(zip_clean = as.numeric(str_pad(as.character(QDemographic_PrimaryZip), width = 5, side = "left", pad = "0"))) %>% 
  count(zip_clean, name = "count") %>%
  glimpse()

# Join spatial data with counts & bin
map_data_binned <- d0 %>% 
  left_join(zip_counts, by = c("zip_code" = "zip_clean")) %>%
  mutate(
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count == 1                ~ "1",
      count == 2                ~ "2",
      count == 3                ~ "3",
      count >= 4  & count <= 10 ~ "4–10",
      count >= 11 & count <= 50 ~ "11–50",
      count > 50                ~ ">50"
    ),
    count_cat = factor(
      count_cat, 
      levels = c("0", "1", "2", "3", "4–10", "11–50", ">50")
    )
  )

# Palette
sunset_colors <- sequential_hcl(6, "SunsetDark", rev = TRUE)

# ---------------------------------------------------------
# Map Generation
# ---------------------------------------------------------
ggplot() +
  # 1. Base Layer: ZIP Code Polygons
  geom_sf(
    data = map_data_binned, 
    aes(fill = count_cat), 
    color = "white", 
    linewidth = 0.05
  ) +
  scale_fill_manual(
    values = c(
      ">50"   = sunset_colors[6],
      "11–50" = sunset_colors[5],
      "4–10"  = sunset_colors[4],
      "3"     = sunset_colors[3],
      "2"     = sunset_colors[2],
      "1"     = sunset_colors[1],
      "0"     = "grey92"
    ),
    name = "No. Responses",
    breaks = c(">50", "11–50", "4–10", "3", "2", "1", "0"),
    guide = guide_legend(order = 1)
  ) +
  
  # 2. Outlined Survey Counties (mid-grey dashed line, no fill)
  geom_sf(
    data = d_survey_counties,
    fill = NA,
    color = "grey50",
    linetype = "dashed",
    linewidth = 0.55
  ) +
  
  # 3. Marine Protected Areas (MPAs)
  new_scale_fill() +
  geom_sf(
    data = d00, 
    aes(fill = "State MPAs"), 
    color = alpha("#00588B", 0.6),
    linewidth = 0.2,
    alpha = 0.5
  ) +
  scale_fill_manual(
    values = c("State MPAs" = alpha("#009B95", 0.5)),
    name = NULL,
    guide = guide_legend(order = 2)
  ) +
  
 # 4. National Marine Sanctuaries (NMS)
  new_scale_fill() +
  new_scale_color() +
  geom_sf(
    data = d000,
    aes(fill = "National Marine\nSanctuaries", color = "National Marine\nSanctuaries"),
    linewidth = 0.35,
    alpha = 0.25
  ) +
  scale_fill_manual(
    values = c("National Marine\nSanctuaries" = "#1F78B4"),
    name = NULL,
    guide = guide_legend(
      order = 3,
      override.aes = list(color = "#084594", alpha = 0.4, linewidth = 0.5)
    )
  ) +
  scale_color_manual(
    values = c("National Marine\nSanctuaries" = "#084594"),
    guide = "none" # <-- Suppresses duplicate color legend
  ) +
  
  # 5. Scale Bar
  annotation_scale(
    location = "bl", 
    pad_x = unit(2.5, "in"),   # dist from left margin
    pad_y = unit(0.2, "in"),   # Baseline height
    unit_category = "imperial",
    width_hint = 0.18,
    style = "ticks",
    text_cex = 0.8
  ) +
  
  # 6. North Arrow (Same height as scale bar, shifted right)
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_x = unit(2.9, "in"),   # Offset to the right of the scale bar
    pad_y = unit(0.4, "in"),   # y location
    height = unit(0.5, "in"), 
    width = unit(0.33, "in"),
    style = north_arrow_orienteering(text_size = 9)
  ) +
  
  # 7. Map Title Annotation
  annotate(
    "text", 
    x = I(0.46), y = I(0.82), 
    label = "Ocean\nAccess\nSurvey", 
    size = 11, 
    fontface = "bold", 
    color = "grey30", 
    hjust = 0
  ) +
  
  # 8. Theme Settings
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.089, 0.035),  #
    legend.justification = c(0, 0),    # Anchors bottom-left of legend
    legend.box = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.y = unit(0.05, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("./doc/bab_map_zip_codes_ca.png", width = 9, height = 9, dpi = 300)


# save county shapefile
st_write(d0000,"./gis_results/ca_counties2.shp",delete_layer = TRUE)

st_write(  d0000, dsn = "./gis_results/counties.gdb",  layer = "ca_counties2",   driver = "OpenFileGDB",   delete_layer = TRUE)
