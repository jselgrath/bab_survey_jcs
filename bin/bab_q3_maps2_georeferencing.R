# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

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

rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
# setwd("G:/My Drive/research/r_projects/bab_survey_jcs/")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d0<-read_csv("./data/bab_q3_map_calculations_20260227.csv")%>%
  select(-checked_against )%>%
  glimpse()

unique(d0$map)

d1<-read_csv("./doc/undergrad_projects_20260428.csv")%>%# 
  glimpse()

d2<-d1%>%
  full_join(d0)%>%
  unique()%>%
  mutate(x_long=x1+(Final_X*x2_y2_cell_size))%>% # x=((x2_y2_cell_size*sb_s_x)+x1),
  mutate(y_lat=y1-(Final_Y*x2_y2_cell_size))%>% #y=(-(x2_y2_cell_size*sb_s_y)+y1) - same results from both
  filter(!is.na(Final_X)) %>% #remove surveys with no value
  glimpse()

names(d2)



# make spatial files --------------------
d2_sf <- st_as_sf(d2, coords = c("x_long", "y_lat"), crs = 3310)

d3_sf<-  d2_sf%>%
  select(QDemographic_CA_Years:QDemographic_Education_clean)%>%
  glimpse()

plot(d3_sf)

# save --------------------------
write_csv(d2,"./results/q3_coordinates_all.csv")

st_write(d3_sf, "./gis/q3_coordinates.shp", delete_layer = TRUE)

st_write(
  obj = d3_sf, 
  dsn = "./gis/q3_coordinates.gdb", 
  layer = "beach_access", 
  driver = "OpenFileGDB", # Use this driver for modern GDB support
  delete_dsn = TRUE       # Optional: overwrites the GDB if it already exists
)

