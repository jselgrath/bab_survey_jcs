# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean county names for maps


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
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long5.csv")%>%
  glimpse

unique(d1$Mechanism) #"Online"    "In_Person"
unique(d1$QUse_Area)


# checking coordinate column names -----------
unique(d1$QMapping_North_County)
unique(d1$QMapping_Central_County)
unique(d1$QMapping_South_County)

# make one map column, add map abbreviations
d2<-d1%>%
  mutate(q_mapping_county = coalesce(
    QMapping_North_County, 
    QMapping_Central_County, 
    QMapping_South_County
  )) %>%
  mutate(map = case_when(
    tolower(q_mapping_county) == "san diego"     ~ "san diego",
    tolower(q_mapping_county) == "orange"        ~ "orange",
    tolower(q_mapping_county) == "los angeles"   ~ "la",
    tolower(q_mapping_county) == "ventura"       ~ "ventura",
    tolower(q_mapping_county) == "santa barbara" ~ "sb",
    tolower(q_mapping_county) == "san luis obispo" ~ "slo",
    tolower(q_mapping_county) == "monterey"      ~ "monterey",
    tolower(q_mapping_county) == "santa cruz"    ~ "santa cruz",
    tolower(q_mapping_county) == "san mateo"     ~ "san mateo",
    tolower(q_mapping_county) == "san francisco" ~ "san francisco",
    tolower(q_mapping_county) == "alameda"       ~ "alameda",
    tolower(q_mapping_county) == "marin"         ~ "marin",
    tolower(q_mapping_county) == "sonoma"        ~ "sonoma",
    tolower(q_mapping_county) == "mendicino"     ~ "mendocino", # Fixed spelling
    tolower(q_mapping_county) == "humboldt"      ~ "humboldt",
    tolower(q_mapping_county) == "del norte"     ~ "del norte",
    TRUE ~ NA_character_
  ))%>%
  select(-QMapping_North_County, -QMapping_Central_County, -QMapping_South_County)%>%
  glimpse()

# check - # NAs that are left are real NAs
filter(d2,!is.na(Final_X))%>%
  select(q_mapping_county,map,Final_X,Final_Y) #QMapping_North_County,QMapping_Central_County,QMapping_South_County,



# save
write_csv(d2,"./results/data_long6.csv")
