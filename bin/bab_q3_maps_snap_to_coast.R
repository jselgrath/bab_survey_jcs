# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: 

# ----------------
library(sf)
library(units)
library(tidyverse)
# ------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
# ----------------------------
st_layers("./gis_results/q3_coordinates.gpkg")

d1 <- st_read("./gis_results/q3_coordinates.gpkg", layer = "beach_access")%>%
  glimpse()

d2<-st_read("./gis_data/ca_eez/ca_eez.shp")%>%
  st_transform(3310)%>%
  glimpse()
st_crs(d2)

# confirm single geometry
d2_union <- st_union(d2)

# Calculate distances from each point in d1 to d2
# st_distance returns a matrix; take the first column
dist_to_d2 <- st_distance(d1, d2_union)

# Filter d1 to keep only points within 5000m (5km)/10km
d3 <- d1[as.numeric(dist_to_d2) <= 10000, ]%>%
  glimpse

#Snap Points within 5km (but not intersecting) to the Edge
# Identify points that are NOT intersecting (distance > 0) # Note:  already filtered points > 5km 
to_snap_idx <- which(as.numeric(st_distance(d3, d2_union)) > 0)

if(length(to_snap_idx) > 0) {
  # Get the nearest points on the polygon boundary for these specific points
  # st_nearest_points creates a line between the point and the polygon
  lines <- st_nearest_points(d3[to_snap_idx, ], d2_union)
  
  # Extract the "end" of those lines (the point on the polygon edge)
  snapped_points <- st_cast(lines, "POINT")[seq(2, 2 * length(lines), by = 2)]
  
  # Replace the old geometries with the snapped ones
  st_geometry(d3[to_snap_idx, ]) <- snapped_points
}

d3

# save ---------------------
st_write(d3,"./gis_results/q3_coordinates.gpkg", 
  layer = "beach_access_2", 
  delete_layer = TRUE)      # overwrites 

st_write(d3, "./gis_results/q3_coordinates_2.shp", delete_layer = TRUE)

