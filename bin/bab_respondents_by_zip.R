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

d1<-st_read("./gis_data/zip_codes_ca/california_zip_codes4.shp")%>%
  # mutate(zip_code=as.numeric(ZIP_CODE))%>%
  # select(-OBJECTID_1,-STATE,-Shape_Leng,- Shape_Area,-ZIP_CODE)%>%
  dplyr::select(-notes)%>%
  # filter(zip_code>=90001&zip_code<=96162)%>%
  glimpse()
# plot(d1)
range(d1$zip_code)


d2 <- read_csv("./results/data_long9.csv") %>%
  mutate(zip_code = as.numeric(QDemographic_PrimaryZip)) %>%
  group_by(zip_code) %>%
  summarize(
    respondent_n = n(),
    # Use names(which.max(table(x))) but only if the table isn't empty
    most_common_time      = if(all(is.na(QActual_Time))) NA else names(which.max(table(QActual_Time))),
    most_common_time_des  = if(all(is.na(QDesired_Time))) NA else names(which.max(table(QDesired_Time))),
    most_common_race      = if(all(is.na(q_demographic_race))) NA else names(which.max(table(q_demographic_race))),
    most_common_income    = if(all(is.na(QDemographic_Income))) NA else names(which.max(table(QDemographic_Income))),
    most_common_swimming  = if(all(is.na(QDemographic_Swimming))) NA else names(which.max(table(QDemographic_Swimming))),
    most_common_house_sz  = if(all(is.na(QDemographic_Family))) NA else names(which.max(table(QDemographic_Family))),
    most_common_fished_sp = if(all(is.na(QFishing_Species_A))) NA else names(which.max(table(QFishing_Species_A))),
    most_common_impt_sp   = if(all(is.na(QOI_Species_A))) NA else names(which.max(table(QOI_Species_A))),
    most_common_mpa       = if(all(is.na(QMPA_Aware))) NA else names(which.max(table(QMPA_Aware))),
    most_common_nms       = if(all(is.na(QSanct_Aware))) NA else names(which.max(table(QSanct_Aware))),
    # Mean needs the na.rm inside the function
    mean_kids             = mean(QNumber_Childern, na.rm = TRUE)
  ) %>%
  glimpse()


d3<-d1%>%
  left_join(d2)%>%
  glimpse()



# save --------------------
st_write(d3, "./gis_results/bab_zip_sample_size.shp", delete_layer = TRUE)

st_write(d3, "./gis_results/bab_zip_sample_size.gpkg", 
  layer = "zip_sample_size", 
  delete_layer = TRUE )
