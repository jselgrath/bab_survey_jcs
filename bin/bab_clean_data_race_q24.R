# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean race data QDemographic_Race for long data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long.csv")%>%
  mutate(QDemographic_Race = as.character(QDemographic_Race))%>%
  glimpse()


unique(d1$QDemographic_Race)

d1$QDemographic_Race<-gsub("Native Hawaiian or Other Pacific Islander", "Native Hawaiian or Pacific Islander", d1$QDemographic_Race)

# sync year options
d1$QDemographic_Race<-gsub("Other","Another race or ethnicity",d1$QDemographic_Race)# combine verious options

# survey specific response
d1$QDemographic_Race<-gsub("Another race or ethnicity, please specify:","Another race or ethnicity",d1$QDemographic_Race)

# if people wrote a race and "choose not to answer" we assigned them the race they chose
d1$QDemographic_Race<-gsub("White,Choose not to answer","White",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("Hispanic or Latino,Native Hawaiian or Pacific Islander,White,Choose not to answer","Hispanic or Latino,Native Hawaiian or Pacific Islander,White",d1$QDemographic_Race)  # check this one
d1$QDemographic_Race<-gsub("Hispanic or Latino,Choose not to answer" ,"Hispanic or Latino",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("Black or African American,Choose not to answer","Black or African American",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("Asian,Choose not to answer","Asian",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("Another race or ethnicity,Choose not to answer","Another race or ethnicity",d1$QDemographic_Race)



# if people selected and also choose not to answer or all responses
d1$QDemographic_Race<-gsub("American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity,Choose not to answer","Choose not to answer",d1$QDemographic_Race) # if people picked all
d1$QDemographic_Race<-gsub("American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity" ,"Choose not to answer",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("Asian,Black or African American,Hispanic or Latino,Native Hawaiian or Pacific Islander,White,Choose not to answer" , "Choose not to answer" ,d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity" , "Choose not to answer" ,d1$QDemographic_Race)







# d1$QDemographic_Race<-gsub("Another race or ethnicity,Choose not to answer" ,"Choose not to answer",d1$QDemographic_Race)


# consider grouping with "white because they are such a small community
d1$QDemographic_Race<-gsub("Middle Eastern or North African","White",d1$QDemographic_Race)


d1$QDemographic_Race<-gsub("White,White","White",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("White,Native Hawaiian or Pacific Islander,White","Native Hawaiian or Pacific Islander,White",d1$QDemographic_Race)
d1$QDemographic_Race<-gsub("White,Choose not to answer","White",d1$QDemographic_Race)

# simplify name of Hawaiian and PI
d1$QDemographic_Race<-gsub("Native Hawaiian or Pacific Islander","Pacific Islander",d1$QDemographic_Race)

unique(d1$QDemographic_Race)




write_csv(d1,"./results/data_long2.csv")
