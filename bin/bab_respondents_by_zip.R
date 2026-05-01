# Jennifer Selgrath
# NOAA CINMS / CMSF
#
# GOAL: summarize counts of survey respondents by zip code
# ---------------------------------------
library(tidyverse)
library(sf)
# ---------------------------------------
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
# ----------------------------
rm(list = ls(all = TRUE))

setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/mec_travel/gis_data/zip_codes_ca")
d1<-st_read("california_zip_codes3.shp")%>%
  mutate(zip_code=as.numeric(ZIP_CODE))%>%
  select(-OBJECTID_1,-STATE,-Shape_Leng,- Shape_Area,-ZIP_CODE)%>%
  filter(zip_code>=90001&zip_code<=96162)%>%
  glimpse()
# plot(d1)


setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
d2<- read_csv("./results/undergrad_projects_20260428c.csv")%>%
  mutate(QDemographic_PrimaryZip=as.numeric(QDemographic_PrimaryZip))%>%
  filter(QDemographic_PrimaryZip>=90001&QDemographic_PrimaryZip<=96162)%>% # california zips
  group_by(QDemographic_PrimaryZip)%>%
  summarize(respondent_n=length(QDemographic_PrimaryZip))%>%
  select(zip_code=QDemographic_PrimaryZip,respondent_n)%>%
  # st_as_sf(d2, coords = c("x_long", "y_lat"), crs = 3310)%>% # make spatial files
  glimpse()

d3<-d1%>%
  merge(d2)%>%
  glimpse()



# save --------------------
st_write(d3, "./gis_results/bab_zip_sample_size.shp", delete_layer = TRUE)

st_write(
  obj = d3, 
  dsn = "./gis_results/bab_zip_sample_size.gdb", 
  layer = "zip_sample_size", 
  driver = "OpenFileGDB", # Use this driver for modern GDB support
  delete_dsn = TRUE       # Optional: overwrites the GDB if it already exists
)
