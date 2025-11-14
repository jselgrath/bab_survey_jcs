# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean race data Q24 for long data

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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long.csv")%>%
  mutate(Q24 = as.character(Q24))%>%
  glimpse()
d1

unique(d1$Q24)

# sync year options
d1$Q24<-gsub("Other","Another race or ethnicity",d1$Q24)# combine verious options

# survey specific response
d1$Q24<-gsub("Another race or ethnicity, please specify:","Another race or ethnicity",d1$Q24)

# if people wrote a race and "choose not to answer" we assigned them the race they chose
d1$Q24<-gsub("White,Choose not to answer","White",d1$Q24)
d1$Q24<-gsub("Hispanic or Latino,Native Hawaiian or Pacific Islander,White,Choose not to answer","Hispanic or Latino,Native Hawaiian or Pacific Islander,White",d1$Q24)  # check this one
d1$Q24<-gsub("Hispanic or Latino,Choose not to answer" ,"Hispanic or Latino",d1$Q24)
d1$Q24<-gsub("Black or African American,Choose not to answer","Black or African American",d1$Q24)
d1$Q24<-gsub("Asian,Choose not to answer","Asian",d1$Q24)
d1$Q24<-gsub("Another race or ethnicity,Choose not to answer","Another race or ethnicity",d1$Q24)



# if people selected and also choose not to answer or all responses
d1$Q24<-gsub("American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity,Choose not to answer","Choose not to answer",d1$Q24) # if people picked all
d1$Q24<-gsub("American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity" ,"Choose not to answer",d1$Q24)
d1$Q24<-gsub("Asian,Black or African American,Hispanic or Latino,Native Hawaiian or Pacific Islander,White,Choose not to answer" , "Choose not to answer" ,d1$Q24)




# d1$Q24<-gsub("Another race or ethnicity,Choose not to answer" ,"Choose not to answer",d1$Q24)


# consider grouping with "white because they are such a small community
# d1$Q24<-gsub("Middle Eastern or North African","White",d1$Q24)


d1$Q24<-gsub("White,White","White",d1$Q24)
d1$Q24<-gsub("White,Native Hawaiian or Pacific Islander,White","Native Hawaiian or Pacific Islander,White",d1$Q24)

unique(d1$Q24)


write_csv(d1,"./results/data_long2.csv")
