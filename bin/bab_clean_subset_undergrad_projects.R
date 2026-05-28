# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath
# California Marine Sanctuary Foundation/ CINMS
#-------------------------------------

# goal:  version of new responses - for undergrad projects
# --------------------------------------------------------------------------

# load libraries -------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1<-read_csv("./results/data_long8.csv")%>%
  # select(response_id,QDesired_Time:QImportant_Activities_Most_TEXT,QDemographic_Home:QDemographic_Swimming,Mechanism,Version,Phase,State:fishing_most_b)%>% #quest_comb,
  glimpse()
d1

# undergrad version
d2<-d1%>%
  select(response_id,UserLanguage,q_mapping_county,map,QMapping_North_County:Final_Y,Comments,comment_clean,QDemographic_PrimaryZip:QDemographic_Swimming, q_demographic_race:q_demographic_education_clean ,EJ_Score:Distance_Binned,Mechanism, Phase,Version, City, State, Primary_County,EJ_Score,EJ_Bin,Distance,Distance_Binned,influencer_any_b)%>%
  glimpse()


# save
write_csv(d2,"./results/undergrad_projects_20260513.csv")