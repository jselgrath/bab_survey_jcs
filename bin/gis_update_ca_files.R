# Jennifer Selgrath
# California Marine Sanctuary Foundation
# California Ocean Access: Benefits and Barriers (bab)

# Goal : process gis files for mapping etc
# -----------------------------------------
library(tidyverse)
library(sf)
library(dplyr)
library(lwgeom)
# -----------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# list of coastal counties (15 Pacific coast + 6 SF Bay shoreline counties)
# Remove the 6 SF Bay counties if you only want open-ocean coast
coastal_list <- c(
  # Pacific Ocean coast (15)
  "Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin",
  "San Francisco", "San Mateo", "Santa Cruz", "Monterey",
  "San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles",
  "Orange", "San Diego"
  # San Francisco / San Pablo Bay shoreline (6)
  # "Contra Costa", "Alameda", "Santa Clara", "Solano", "Napa", "Sacramento"
)

# 1. Read datasets

# high resolution west coasta coastline from cusp
west_coastline <- st_read("./gis_data/coastline_west_cusp_20260518/coastline_west_cusp.shp")

# ca zip codes
ca_zips <- st_read("./gis_data/zip_codes_ca/california_zip_codes4.shp")%>%
  mutate(zip_code=as.numeric(zip_code))%>%
  glimpse()

# cdfw regions
cdfw<-st_read("./gis_data/cdfw_regions/cdfw_regions.shp")%>%
  st_transform(3310) %>%
  select(region=REGION, region_name=NAME)%>%
  filter(region!=7)%>%  # remove marine region because no one lives there - in ocean
  glimpse()

# counties ----------
ca_counties <- st_read("./gis_data/ca_counties/ca_counties.shp") %>%
  st_transform(3310) %>%
  mutate(
    county = NAME, 
    co_long = NAMELSAD,
    co_survey = if_else(
      county %in% c("San Diego", "Los Angeles", "Ventura", 
                    "Santa Barbara", "Monterey", "Santa Cruz", 
                    "San Francisco", "Sonoma", "Humboldt", "Del Norte"), 1, 0),
    co_phase = case_when(
      co_survey == 0 ~ "Not Focal County",
      county %in% c("San Diego", "Los Angeles", "Sonoma", "Humboldt", "Del Norte") ~ "Phase II", TRUE ~ "Phase I" )) %>%
  select(county, co_long, co_survey, co_phase) %>%
  mutate(co_coastal = ifelse(county %in% coastal_list, "coastal", "inland"))%>%
  glimpse()
plot(ca_counties[4])
plot(ca_counties)


# plot(cdfw)

# 2. Reproject both to NAD83 / California Albers (EPSG:3310) and fix any topology issues
ca_outline <- ca_zips %>%
  st_transform(3310) %>%
  st_make_valid() %>%
  st_buffer(500) %>%   # Expands polygons by 500m to overlap across the gap
  st_union() %>%       # Dissolves them into one seamless shape
  st_buffer(-500)      # Shrinks the outer coastline back to its original position

# plot(st_geometry(ca_outline), col = "grey", border = "darkgrey")

coast_lines <- west_coastline %>%
  st_transform(3310) %>%
  st_crop(st_bbox(ca_outline)) %>% # Drop Oregon/Washington segments
  st_union()                       # Combine thousands of CUSP segments into one geometry

# plot(coast_lines)



# save -----
st_write(cdfw,"./gis_results/cdfw_regions2.shp",append=FALSE)
st_write(ca_outline,  "./gis_results/ca_outline.shp",append=FALSE)
st_write(coast_lines, "./gis_results/ca_coast_line.shp",append=FALSE) 
st_write(ca_counties,"./gis_results/ca_counties2.shp",delete_layer = TRUE)

st_write(cdfw,"./gis_results/california.gpkg",  layer = "cdfw_regions2", delete_layer = TRUE)
st_write(ca_outline,"./gis_results/california.gpkg",  layer = "ca_outline", delete_layer = TRUE)
st_write(coast_lines,"./gis_results/california.gpkg",  layer = "ca_coast_line", delete_layer = TRUE)
st_write(ca_counties, "./gis_results/california.gpkg",  layer = "ca_counties2",   delete_layer = TRUE)
