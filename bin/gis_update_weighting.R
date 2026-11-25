# Jennifer Selgrath
# California Marine Sanctuary Foundation
# California Ocean Access: Benefits and Barriers (bab)

# Goal : make weighting variable for maps etc
# -----------------------------------------
library(tidyverse)
library(sf)
library(dplyr)
library(lwgeom)
# -----------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# 1. Read datasets and ensure consistent CRS (EPSG:3310) & valid geometries
ca_counties <- st_read("./gis_results/california.gpkg", layer = "ca_counties2") %>%
  st_transform(3310) %>%
  st_make_valid()%>%
  glimpse()

plot(ca_counties)

cdfw <- st_read("./gis_results/california.gpkg", layer = "cdfw_regions2") %>%
  st_transform(3310) %>%
  st_make_valid()%>%
  glimpse()

ca_outline <- st_read("./gis_results/california.gpkg", layer = "ca_outline") %>%
  st_transform(3310) %>%
  st_make_valid()

# 2. Clip counties to ca_outline
# Using st_intersection removes offshore marine buffers while preserving all county attributes
ca_counties_clipped <- ca_counties %>%
  st_intersection(st_geometry(ca_outline)) %>%
  st_collection_extract("POLYGON") %>%
  st_make_valid()

plot(ca_counties_clipped)

# 3. Match each county to its primary CDFW region (by largest area overlap)
# Using largest = TRUE avoids misclassifying counties due to tiny border overlaps
county_cdfw_join <- st_join(
  ca_counties_clipped,
  cdfw %>% select(region_name, region),
  join = st_intersects,
  largest = TRUE
)

unique(county_cdfw_join$region_name)

# 4. assign counties to weighting regions
ca_counties_final <- county_cdfw_join %>%
  mutate(
    weighting = case_when(
      co_survey == 1 ~ as.character(county),
      co_survey == 0 & co_coastal == "coastal" ~ "other coastal",
      co_survey == 0 & co_coastal == "inland" &
        region_name %in% c("Central Region", "North Central Region", "Northern Region") ~ "Inland - Central & Northern Regions",
      co_survey == 0 & co_coastal == "inland" ~ as.character(region_name),
      TRUE ~ NA_character_
    )
  )

# check ----
plot(ca_counties_final)

ca_counties_final%>%
  filter(co_coastal == "coastal")%>%
  select(county,weighting,co_phase)%>%
  arrange(weighting)%>%
  print(n=15)


# save -----
st_write(ca_counties_final,"./gis_results/california.gpkg", layer = "weighting", delete_layer = TRUE)