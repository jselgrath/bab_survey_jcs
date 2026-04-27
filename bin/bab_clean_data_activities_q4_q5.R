# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean activity data QImportant_Activities & QImportant_Activities_Most for long data

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

d0<-read_csv("./results/data_long3.csv")%>%
  mutate(QImportant_Activities = as.character(QImportant_Activities))%>%  # all activities
  mutate(QImportant_Activities_TEXT  = as.character(QImportant_Activities_TEXT ))%>%  # all activities
  mutate(QImportant_Activities_Most = as.character(QImportant_Activities_Most))%>%  # important activity
  mutate(QImportant_Activities_Most_TEXT = as.character(QImportant_Activities_Most_TEXT))%>%  # important activity
  glimpse()
d0

d1<-d0

unique(d1$QImportant_Activities)
unique(d1$QImportant_Activities_Most)

# simplify responses if gender + choose not to answer

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"with music and/or food (e.g., earth day)","")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"with music and/or food (e.g., earth day)","")

# d1$QImportant_Activities<-gsub("Festivals with music and/or food  (e.g., earth day)","Festivals")
# d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Festivals with music and/or food  (e.g., earth day)","Festivals")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Other, please specify:","Another activity")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Other, please specify:","Another activity")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"None of the above","Another activity")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"None of the above","Another activity")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Other","Another activity")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Other","Another activity")

# put commas as / so separate correctly below
d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding,")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Bicycling, roller skating, skateboarding, etc","Bicycling/Roller skating/Skateboarding,")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Bicycling, roller skating, skateboarding, etc","Bicycling/Roller skating/Skateboarding")


d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Festivals with music and/or food  (e.g., earth day)","Festivals")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Festivals with music and/or food  (e.g., earth day)","Festivals")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Festivals with music and/or food (e.g., earth day)","Festivals")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Festivals with music and/or food (e.g., earth day)","Festivals")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Festivals with music and/or food","Festivals")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Festivals with music and/or food","Festivals")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Volunteering (e.g., beach clean-ups)","Volunteering ")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Volunteering (e.g., beach clean-ups)","Volunteering ")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Sailing/Boating (engine powered)","Sailing/Boating")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Sailing/Boating (engine powered)","Sailing/Boating")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Beach games or sports (e.g., frisbee, volleyball, yoga)","Beach games or sports")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Beach games or sports (e.g., frisbee, volleyball, yoga)","Beach games or sports")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Group or family gatherings or activities (e.g., family outing, bbq)","Group/Family gatherings")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Group or family gatherings or activities (e.g., family outing, bbq)","Group/Family gatherings")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Group/Family gatherings or activities","Group/Family gatherings")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Group/Family gatherings or activities","Group/Family gatherings")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Meditation, reading, and/or relaxing","Meditation/Reading/Relaxing")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Meditation, reading, and/or relaxing","Meditation/Reading/Relaxing")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Observing or photographing nature or wildlife, outdoor education","Observing or photographing nature") #Observing or photographing nature/Outdoor education
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Observing or photographing nature or wildlife, outdoor education","Observing or photographing nature")


d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Driving or sitting in your car to enjoy the views/sunsets","Enjoy the views/sunsets from car")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Driving or sitting in your car to enjoy the views/sunsets","Enjoy the views/sunsets from car")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Cultural or religious practices or ceremonies","Cultural or religious ceremonies")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Cultural or religious practices or ceremonies","Cultural or religious ceremonies")

d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities,"Stand up paddleboarding/Kite or sailboarding/Kayaking","Paddleboarding/Kiteboarding/Kayaking")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most,"Stand up paddleboarding/Kite or sailboarding/Kayaking","Paddleboarding/Kiteboarding/Kayaking")



# eliminate sticky ()
d1$QImportant_Activities<-str_replace_all(d1$QImportant_Activities, "\\s*\\([^)]*\\)", "")
d1$QImportant_Activities_Most<-str_replace_all(d1$QImportant_Activities_Most, "\\s*\\([^)]*\\)", "")

unique(d1$QImportant_Activities)
unique(d1$QImportant_Activities_Most)


# check NAs for most important activiites - none - in other text from first prompt
d1%>%
  filter(is.na(QImportant_Activities_Most))%>%
  select(QImportant_Activities_Most_TEXT)%>%
  view()


# other activities
d2<-d1%>%
  filter(QImportant_Activities_Most=="Another activity")%>%
  select(ResponseId,QImportant_Activities_TEXT)%>%
  filter(!is.na(QImportant_Activities_TEXT))%>%
  view()


write_csv(d1,"./results/data_long4.csv")
write_csv(d2,"./results/other_activities.csv")
