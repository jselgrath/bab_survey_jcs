# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset data for bab 2026 focus groups in San Diego, LA, Sonoma, Humboldt, and Del Norte Counties
# methods for selecting zip codes is documented in this link: https://docs.google.com/document/d/1gUCYapCdspr0dT5fPkegix3JoaSnRdqK4va2yuX32Yo/edit?usp=drive_link 
# and here: https://docs.google.com/document/d/1gUCYapCdspr0dT5fPkegix3JoaSnRdqK4va2yuX32Yo/edit?usp=sharing
# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

###Load in Data file
d0<-read.csv("./data/bab_fg_zip_codes_ALL_ZIP_20260723.csv")%>%  # used for most fg: bab_fg_zip_codes_ALL_ZIP_20260527.csv
  mutate(zip_code=ZIP)%>%
  dplyr::select(zip_code,Focus_Group,Approximate_Location)%>%
  unique()%>%
  glimpse()
d0

d1<-read_csv("./results/data_long9.csv")%>%
  mutate(zip_code=as.numeric(QDemographic_PrimaryZip))%>%
  dplyr::select(ResponseId,UserLanguage,QDesired_Time:QE_Overnight_Transportation,QMedia1:QPriority_10,QPriority_1:y_lat,zip_code)%>%
  glimpse()


# -----------------------------
# Subset data to focus groups

unique(d0$Focus_Group) #"SD_Downtown"     "SD_South_County" "Santa_Rosa"      "Eureka"          "Crescent_City"  "Torrance_LA"     "East_LA"         "South_LA"
# did Tribal via demographics and county 

# focus groups by low income zip ----------------
d2<-d1%>%
  inner_join(d0,relationship = "many-to-many")%>%
  glimpse()

fg<-unique(d2$Focus_Group)

# make a list
focus_list <- d2 %>%
  filter(Focus_Group %in% fg) %>%
  group_split(Focus_Group) %>%
  set_names(., map_chr(., ~unique(.x$Focus_Group)))

# Glimpse every data frame in the list to inspect them
# walk(focus_list, glimpse)


# fg for north coast with all county residents
# d2_h<-d1%>%
#   filter(Primary_County=="Humboldt")%>%
#   mutate(Focus_Group="Eureka2")%>%
#   glimpse()
# 
# d2_dn<-d1%>%
#   filter(Primary_County=="Del Norte")%>%
#   mutate(Focus_Group="Crescent_City2")%>%
#   glimpse()
# 
# d2_b<-d2%>%
#   filter(Focus_Group!="Crescent_City"&Focus_Group!="Eureka")%>%
#   select(-"Approximate_Location")%>%
#   rbind(d2_h)%>%
#   rbind(d2_dn)%>%
#   glimpse





# save fg data only and list as a .rds file
write_csv(d2, "./results/data_long9_fg.csv")
# write_csv(d2_b, "./results/data_long9_fg2.csv")
# write_rds(focus_list, "./results/data_long9_fg.rds")




# ------------------------------------------


# email lists for fg in Humboldt and Del NOrte (to recruit more participants)------------
# d3<-d1%>%
#   # filter(Focus_Group=="Eureka")%>%
#   filter(Primary_County=="Humboldt")%>%
#   filter(!is.na(Email))%>%
#   dplyr::select(ResponseId,UserLanguage,Email,zip_code, Update:Mechanism )%>%
#   glimpse()
# 
# write_csv(d3,"./doc/eureka_fg_all_humboldt.csv")
# 
# # ------
# d4<-d1%>%
#   filter(Primary_County=="Del Norte")%>%
#   filter(!is.na(Email))%>%
#   # select(QDemographic_Home :Mechanism )%>%
#   dplyr::select(ResponseId,UserLanguage,Email,zip_code , Update:Mechanism )%>%
#   glimpse()
# 
# write_csv(d4,"./doc/dn_fg_all_cc.csv")
# 
