# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean gender data Q25 for long data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d0<-read_csv("./results/data_long2.csv")%>%
  mutate(Q25 = as.character(Q25))%>%
  glimpse()
d0

d1<-d0

unique(d1$Q25)

# simplify responses if gender + choose not to answer
d1$Q25<-gsub("Female,Choose not to answer","Female",d1$Q25)
d1$Q25<-gsub("Male,Choose not to answer","Male", d1$Q25)# combine
d1$Q25<-gsub("Transgender, non-binary, or another gender,Choose not to answer","Transgender, non-binary, or another gender",d1$Q25) 

# from different survey versions??
d1$Q25<-gsub("Prefer to self-describe","Transgender, non-binary, or another gender",d1$Q25) 
d1$Q25<-gsub("Non-binary or gender non-conforming","Transgender, non-binary, or another gender",d1$Q25) 
d1$Q25<-gsub("Transgender, non-binary, or another gender,Transgender, non-binary, or another gender","Transgender, non-binary, or another gender",d1$Q25) 


unique(d1$Q25)


write_csv(d1,"./results/data_long3.csv")
