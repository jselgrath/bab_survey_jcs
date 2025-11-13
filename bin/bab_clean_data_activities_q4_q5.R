# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean activity data Q4 & Q5 for long data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d0<-read_csv("./results/data_long3.csv")%>%
  mutate(Q4 = as.character(Q4))%>%  # all activities
  mutate(Q5 = as.character(Q5))%>%  # important activity
  glimpse()
d0

d1<-d0

unique(d1$Q4)
unique(d1$Q5)

# simplify responses if gender + choose not to answer

d1$Q4<-str_replace_all(d1$Q4,"with music and/or food (e.g., earth day)","")
d1$Q5<-str_replace_all(d1$Q5,"with music and/or food (e.g., earth day)","")

# d1$Q4<-gsub("Festivals with music and/or food  (e.g., earth day)","Festivals")
# d1$Q5<-str_replace_all(d1$Q5,"Festivals with music and/or food  (e.g., earth day)","Festivals")

d1$Q4<-str_replace_all(d1$Q4,"Other, please specify:","Another activity")
d1$Q5<-str_replace_all(d1$Q5,"Other, please specify:","Another activity")

d1$Q4<-str_replace_all(d1$Q4,"None of the above","Another activity")
d1$Q5<-str_replace_all(d1$Q5,"None of the above","Another activity")

d1$Q4<-str_replace_all(d1$Q4,"Other","Another activity")
d1$Q5<-str_replace_all(d1$Q5,"Other","Another activity")

d1$Q4<-str_replace_all(d1$Q4,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding,")
d1$Q5<-str_replace_all(d1$Q5,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding")
d1$Q5<-str_replace_all(d1$Q5,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding")

d1$Q4<-str_replace_all(d1$Q4,"Bicycling, roller skating, skateboarding, etc","Bicycling/Roller skating/Skateboarding,")
d1$Q5<-str_replace_all(d1$Q5,"Bicycling, roller skating, skateboarding, etc","Bicycling/Roller skating/Skateboarding")


d1$Q4<-str_replace_all(d1$Q4,"Festivals with music and/or food  (e.g., earth day)","Festivals")
d1$Q5<-str_replace_all(d1$Q5,"Festivals with music and/or food  (e.g., earth day)","Festivals")

d1$Q4<-str_replace_all(d1$Q4,"Festivals with music and/or food (e.g., earth day)","Festivals")
d1$Q5<-str_replace_all(d1$Q5,"Festivals with music and/or food (e.g., earth day)","Festivals")

d1$Q4<-str_replace_all(d1$Q4,"Volunteering (e.g., beach clean-ups)","Volunteering ")
d1$Q5<-str_replace_all(d1$Q5,"Volunteering (e.g., beach clean-ups)","Volunteering ")

d1$Q4<-str_replace_all(d1$Q4,"Sailing/Boating (engine powered)","Sailing/Boating")
d1$Q5<-str_replace_all(d1$Q5,"Sailing/Boating (engine powered)","Sailing/Boating")

d1$Q4<-str_replace_all(d1$Q4,"Beach games or sports (e.g., frisbee, volleyball, yoga)","Beach games or sports")
d1$Q5<-str_replace_all(d1$Q5,"Beach games or sports (e.g., frisbee, volleyball, yoga)","Beach games or sports")

d1$Q4<-str_replace_all(d1$Q4,"Group/Family gatherings or activities","Group or family gatherings or activities")
d1$Q5<-str_replace_all(d1$Q5,"Group/Family gatherings or activities","Group or family gatherings or activities")

d1$Q4<-str_replace_all(d1$Q4,"Meditation, reading, and/or relaxing","Meditation/ Reading/Relaxing")
d1$Q5<-str_replace_all(d1$Q5,"Meditation, reading, and/or relaxing","Meditation/ Reading/Relaxing")



# eliminate sticky ()
d1$Q4<-str_replace_all(d1$Q4, "\\s*\\([^)]*\\)", "")
d1$Q5<-str_replace_all(d1$Q5, "\\s*\\([^)]*\\)", "")

write_csv(d1,"./results/data_long4.csv")
