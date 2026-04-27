# Equity in Ocean Access (MPAs Equity and Climate (mec))
# Jennifer Selgrath - base code from Tim Frawley
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


d2<-read_csv("./data/bab_survey_2026_tribal_20260427_20_new.csv")%>%
  select(UserLanguage,Q3:Q3b18_1_y,Q50,Q37b:Q49)%>%
  mutate(Version="P2_tribal_new")%>%
  glimpse()

d3<-read_csv("./data/bab_survey_2025_in_person_prize - norcal_20260427_23_new.csv")%>%
  select(UserLanguage,Q3:Q3b18_1_y,Q50,Q37b:Q49)%>%
  mutate(Version="P2_norcal_ipp_new")%>%
  glimpse()

names(d2)
names(d3)

# new responses -----------

# combine
d3a<-d3[3:nrow(d3),]

d5<-rbind(d2,d3a)%>%
  glimpse()
