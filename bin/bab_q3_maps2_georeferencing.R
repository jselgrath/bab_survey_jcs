# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath
# California Marine Sanctuary Foundation/ CINMS
#-------------------------------------

# goal: check georectifying against survey maps

# notes on codes-----------
# hdn - humboldt del norte
# sn - sonoma
# sb -  santa barbara, pilot
# la - los angeles
# sd - san diego
# op - online prize
# tp - tribal pilot
# ippsc - in person prize southern calfornia

# maps to check against here: https://docs.google.com/spreadsheets/d/1SCQfjCVaj0fk0LSarMIVoUtfmWAvB2THA5O6BIG_91g/edit?usp=sharing


# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(sf)
library(janitor)
#-------------------------------------

rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
# setwd("G:/My Drive/research/r_projects/bab_survey_jcs/")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d0<-read_csv("./data/bab_q3_map_calculations_20260227.csv")%>%
  dplyr::select(-checked_against )%>%
  glimpse()

unique(d0$map)

d1<-read_csv("./results/data_long8.csv")%>%# data_long8.csv
  glimpse()

d2<-d1%>%
  full_join(d0)%>%
  unique()%>%
  mutate(x_long=x1+(Final_X*x2_y2_cell_size))%>% # x=((x2_y2_cell_size*sb_s_x)+x1),
  mutate(y_lat=y1-(Final_Y*x2_y2_cell_size))%>% #y=(-(x2_y2_cell_size*sb_s_y)+y1) - same results from both
  dplyr::select(-x_calc,-y_calc,-x1,-y1,-x2_y2_cell_size,-map_name,-map_link)%>%
  glimpse()

names(d2)

# subset for Emily's project about coastal access locations
d2b<-d2%>%
  dplyr::select(response_id,UserLanguage,q_mapping_county,map,QDemographic_PrimaryZip:QDemographic_Swimming, q_demographic_race:q_demographic_education_clean ,EJ_Score:Distance_Binned,Mechanism, Phase,Version, City, State, Primary_County,EJ_Score,EJ_Bin,Distance,Distance_Binned,influencer_any_b,x_long,y_lat)%>%
  filter(!is.na(x_long)) %>% #remove surveys with no value
  rename_with(~ str_remove(., "^QDemographic_")) %>%
  rename_with(~ str_remove(., "^q_demographic_")) %>%
  clean_names()%>%
  glimpse()  

# make spatial files --------------------
d2_sf <- st_as_sf(d2b, coords = c("x_long", "y_lat"), crs = 3310)

# d3_sf <-  d2_sf%>%
#   select(QDemographic_CA_Years:q_demographic_education_clean)%>%
#   glimpse()

plot(d2_sf)

d3 <- d2_sf %>%
  # Shorten names to 10 characters manually
  rename_with(~ substr(.x, 1, 10)) %>%
  # Ensure names are unique after shortening (e.g., col_1, col_2)
  clean_names()%>%
  glimpse()

d3$comment_cl[d3$response_i=="R_6f1mTg4VnCQrhm6"]

# save --------------------------
write_csv(d2,"./results/data_long9.csv")
write_csv(d2b,"./results/q3_coordinates_all.csv")

st_write(d3,"./gis_results/q3_coordinates.gpkg", 
  layer = "beach_access", 
  delete_layer = TRUE)      # overwrites

st_layers("./gis_results/q3_coordinates.gpkg")

st_write(d3, "./gis_results/q3_coordinates.shp", delete_layer = TRUE)



