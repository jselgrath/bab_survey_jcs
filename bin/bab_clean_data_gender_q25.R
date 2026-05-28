# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean gender data q_demographic_gender for long data (Q25)

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d0<-read_csv("./results/data_long2.csv")%>%
  mutate(q_demographic_gender = as.character(QDemographic_Gender))%>%
  glimpse()
d0

d1<-d0

unique(d1$q_demographic_gender)

# simplify responses if gender + choose not to answer
d1$q_demographic_gender<-gsub("Female,Choose not to answer","Female",d1$q_demographic_gender)
d1$q_demographic_gender<-gsub("Male,Choose not to answer","Male", d1$q_demographic_gender)# combine
d1$q_demographic_gender<-gsub("Transgender, non-binary, or another gender,Choose not to answer","Transgender, non-binary, or another gender",d1$q_demographic_gender) 

# from different survey versions??
d1$q_demographic_gender<-gsub("Prefer to self-describe","Transgender, non-binary, or another gender",d1$q_demographic_gender) 
d1$q_demographic_gender<-gsub("Non-binary or gender non-conforming","Transgender, non-binary, or another gender",d1$q_demographic_gender) 
d1$q_demographic_gender<-gsub("Transgender, non-binary, or another gender,Transgender, non-binary, or another gender","Transgender, non-binary, or another gender",d1$q_demographic_gender) 


unique(d1$q_demographic_gender)


write_csv(d1,"./results/data_long3.csv")
