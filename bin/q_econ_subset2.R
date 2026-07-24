# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset data for economic analysis via implan

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(googledrive)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long9.csv")%>%
  mutate(zip_code=as.numeric(QDemographic_PrimaryZip))%>%
  filter(Phase=="Phase_2")%>%
  mutate(QE_TimeSpent=if_else(QE_TimeSpent=="I went for the day","single_day",QE_TimeSpent))%>%
  mutate(QE_TimeSpent=if_else(QE_TimeSpent=="I stayed overnight for ____ days (count partial days as whole days)","overnight",QE_TimeSpent))%>%
  dplyr::select(response_id,QE_TimeSpent:QE_Overnight_Transportation,QE_Grocery, LocationLatitude, LocationLongitude, QActual_Time,
                QDemographic_Home:QDemographic_CA_Years,
                q_demographic_race:q_demographic_education_clean,
                QDemographic_Birth:QDemographic_Swimming,
                Primary_County,EJ_Score:Distance_Binned,
                Version,Mechanism,
                Phase :Primary_County,
                QImportant_Activities_Most2,q_mapping_county,
                county_coastal:map,
                zip_code)%>%
  tibble()%>%
  glimpse()

names(d1)


unique(d1$QE_TimeSpent)

# save
write_csv(d1,"./results/bab_data_econ_subset2.csv")


