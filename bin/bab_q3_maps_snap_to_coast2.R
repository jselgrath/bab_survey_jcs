# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: snap beach access points to coast if within 5km of coast. for points on land and at sea.

# ----------------
library(sf)
library(units)
library(tidyverse)
# ------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
# setwd("G:/My Drive/research/r_projects/bab_survey_jcs/")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
# ----------------------------
st_layers("./gis_results/q3_coordinates.gpkg")

d1 <- st_read(dsn = "./gis_results/q3_coordinates.gpkg", layer = "beach_access")%>%
  glimpse()
st_crs(d1) #3310

d2<-st_read("./gis_data/ca_eez/ca_eez.shp")%>%
  st_transform(3310)%>%
  glimpse()
st_crs(d2)

d3<-st_read("./gis_data/west_coast_coastline_cusp_20260518/West.shp")%>% #coastline_ca/Coastline_CA.shp")%>%
  st_transform(3310)%>%
  filter(FIPS_ALPHA=="CA")%>%
  glimpse()
st_crs(d3)

# Union the coastline if it's multiple segments
# This ensures we calculate distance to the nearest part of the whole network
coast_union <- st_union(d3)

# Calculate distances from each point in d1 to d3
# st_distance returns a matrix; take the first column
dist_to_coast <- st_distance(d1, coast_union)%>%
  glimpse()

# Filter d1 to keep only points within 5000m (5km)/10km
d4 <- d1[as.numeric(dist_to_coast) <= 10000, ]%>%
  glimpse

#Snap Points within 5km (but not intersecting) to the Edge
# Identify points that are NOT intersecting (distance > 0) # Note:  already filtered points > 5km 
# to_snap_idx <- which(as.numeric(st_distance(d4, d2_union)) > 0)

if(nrow(d4) > 0) {
  # Get the nearest points on the polygon boundary for these specific points
  # st_nearest_points creates a line between the point and the polygon
  lines <- st_nearest_points(d4, coast_union)
  
  # Extract the destination point (the point on the coast)
  snapped_geoms <- st_cast(lines, "POINT")[seq(2, 2 * nrow(d4), by = 2)]
  
  # Update the geometry
  st_geometry(d4) <- snapped_geoms
}

d4
plot(d4)

# save ---------------------
st_write(d4, "./gis_results/q3_coordinates.gpkg", 
  layer = "beach_access_3", 
  delete_layer = TRUE)

st_write(d4, "./gis_results/q3_coordinates_3.shp", delete_layer = TRUE)

