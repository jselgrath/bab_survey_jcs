# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: temp version of new responses - for undergrad projects

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long5.csv")%>%
  # select(ResponseId,UserLanguage,"QMapping_North_County":"Final_Y",Comments,QDemographic_PrimaryZip:QDemographic_Swimming, Mechanism, Version)%>%
  glimpse

unique(d1$Mechanism) #"Online"    "In_Person"
unique(d1$QUse_Area)


# checking coordinate column names -----------
unique(d1$QMapping_North_County)
unique(d1$QMapping_Central_County)
unique(d1$QMapping_South_County)

# make one map column, add map abbreviations
d2<-d1%>%
  mutate(QMapping_County = coalesce(
    QMapping_North_County, 
    QMapping_Central_County, 
    QMapping_South_County
  )) %>%
  mutate(map = case_when(
    tolower(QMapping_County) == "san diego"     ~ "san diego",
    tolower(QMapping_County) == "orange"        ~ "orange",
    tolower(QMapping_County) == "los angeles"   ~ "la",
    tolower(QMapping_County) == "ventura"       ~ "ventura",
    tolower(QMapping_County) == "santa barbara" ~ "sb",
    tolower(QMapping_County) == "san luis obispo" ~ "slo",
    tolower(QMapping_County) == "monterey"      ~ "monterey",
    tolower(QMapping_County) == "santa cruz"    ~ "santa cruz",
    tolower(QMapping_County) == "san mateo"     ~ "san mateo",
    tolower(QMapping_County) == "san francisco" ~ "san francisco",
    tolower(QMapping_County) == "alameda"       ~ "alameda",
    tolower(QMapping_County) == "marin"         ~ "marin",
    tolower(QMapping_County) == "sonoma"        ~ "sonoma",
    tolower(QMapping_County) == "mendicino"     ~ "mendocino", # Fixed spelling
    tolower(QMapping_County) == "humboldt"      ~ "humboldt",
    tolower(QMapping_County) == "del norte"     ~ "del norte",
    TRUE ~ NA_character_
  ))%>%
  glimpse()

# check - # NAs that are left are real NAs
filter(d2,!is.na(Final_X))%>%
  select(QMapping_County,map,QMapping_North_County,QMapping_Central_County,QMapping_South_County,Final_X,Final_Y)


# undergrad version
d3<-d2%>%
  select(ResponseId,UserLanguage,QMapping_County,map,QMapping_North_County:Final_Y,Comments,QDemographic_PrimaryZip:QDemographic_Swimming, q_demographic_education_clean,EJ_Score:Distance_Binned,Mechanism, Phase,Version)%>%
  glimpse()

# save
write_csv(d2,"./results/data_long6.csv")
write_csv(d3,"./results/undergrad_projects_20260509.csv")


# currently no alameda, but is in survey
# filter(d1,QMapping_North_County=="Del Norte")%>%glimpse()
# filter(d1,QMapping_North_County=="Humboldt")%>%glimpse()
# filter(d1,QMapping_North_County=="Mendicino")%>%glimpse()
# filter(d1,QMapping_North_County=="Sonoma")%>%glimpse()
# filter(d1,QMapping_North_County=="Marin")%>%glimpse()
# filter(d1,QMapping_North_County=="San Francisco")%>%glimpse()
# filter(d1,QMapping_North_County=="Alameda")%>%glimpse()
# filter(d1,QMapping_Central_County=="San Francisco")%>%glimpse()
# filter(d1,QMapping_Central_County=="San Mateo")%>%glimpse()
# filter(d1,QMapping_Central_County=="Santa Cruz")%>%glimpse()
# filter(d1,QMapping_Central_County=="Monterey")%>%glimpse()
# filter(d1,QMapping_Central_County=="San Luis Obispo")%>%glimpse()
# filter(d1,QMapping_Central_County=="Santa Barbara")%>%glimpse()
# 
# filter(d1,QMapping_South_County=="Santa Barbara")%>%glimpse()
# filter(d1,QMapping_South_County=="Ventura")%>%glimpse()
# filter(d1,QMapping_South_County=="Los Angeles")%>%glimpse()
# filter(d1,QMapping_South_County=="Orange")%>%glimpse()
# filter(d1,QMapping_South_County=="San Diego")%>%glimpse()



# vector of empty values -------------
glimpse(d1)

# d1$q_use_area_final_x <-0
# d1$q_use_area_final_y<-0

# assign coordinates -----------------
# d2 <- d1 %>%
#   mutate(
#     q_use_area_final_x = case_when(
#       QMapping_North_County == "Del Norte"         ~ as.numeric(LocationLongitude),
#       QMapping_North_County == "Humboldt"          ~ as.numeric(LocationLongitude),
#       QMapping_North_County == "Mendicino"         ~ as.numeric(LocationLongitude),
#       QMapping_North_County == "Sonoma"            ~ as.numeric(LocationLongitude),
#       QMapping_Central_County == "Santa Cruz"      ~ as.numeric(LocationLongitude),
#       QMapping_Central_County == "San Luis Obispo" ~ as.numeric(LocationLongitude),
#       QMapping_South_County == "Ventura"           ~ as.numeric(LocationLongitude),
#       QMapping_South_County == "Los Angeles"       ~ as.numeric(LocationLongitude),
#       QMapping_South_County == "San Diego"         ~ as.numeric(LocationLongitude),
#       TRUE ~ NA_real_ # This catches anything else as a numeric NA
#     ),
#     q_use_area_final_y = case_when(
#       QMapping_North_County == "Del Norte"       ~ as.numeric(LocationLatitude),
#       QMapping_North_County == "Humboldt"        ~ as.numeric(LocationLatitude),
#       QMapping_North_County == "Mendicino"       ~ as.numeric(LocationLatitude),
#       QMapping_North_County == "Sonoma"          ~ as.numeric(LocationLatitude),
#       QMapping_Central_County == "Santa Cruz"    ~ as.numeric(LocationLatitude),
#       QMapping_Central_County == "San Luis Obispo" ~ as.numeric(LocationLatitude),
#       QMapping_South_County == "Ventura"         ~ as.numeric(LocationLatitude),
#       QMapping_South_County == "Los Angeles"     ~ as.numeric(LocationLatitude),
#       QMapping_South_County == "San Diego"       ~ as.numeric(LocationLatitude),
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   glimpse()
# glimpse(d2)

# check - # NAs that are left are real NAs
# filter(d2,is.na(q_use_area_final_x ))%>%
#   select(QMapping_North_County,QMapping_Central_County,QMapping_South_County,Final_X,Final_Y,q_use_area_final_x,q_use_area_final_y)
# 
# d9<-d8%>%
#   select(-Q3,-Q3a1,-Q3a2,-Q3a3)%>%
#   select(ResponseId,UserLanguage,QMapping_North_County:Final_Y,Comments,Q37b:Q49,Mechanism,Version)%>%
#   glimpse()
# 
# colnames(d9)<-colnames(d1)
#   glimpse(d9)
# 
# d10<-rbind(d1,d9)




