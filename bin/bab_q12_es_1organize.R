# Equity in Ocean Access (benefits and barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset cleaned data for graphing and making long version - ecosystem services

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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

###Load in Data file
d1<-read_csv("./results/data_long3.csv")%>%
  # select(Q1:Format,ResponseId)%>%  # remove metadata from surveys
  glimpse()

##transpose Data so that each column is a survey response and each row is a prompt
d0<-as.data.frame(t(d1))%>%
  glimpse()
d0[1:10,]


# Question list ##### -------------------------------------------
# Q1: time desired
# Q2: time spent
# Q3: location
# Q4: activities
# Q5: one activity
# Q6: transportation
# Q7: travel time
# Q8: mentorship
# Q9: companionship
# 5a: fishing method
# 5b: species: fishing use
# 5b.2 species: cultural 
# 5b.3: species: observing photo. education
# 5b.4: species: snorkling/scuba
# Q10: digital
# Q11: wellbeing
# Q12: ecosystem services
# Q13: barriers
# Q14: microaggressions
# Q15: observed changes
# Q16: anticipated changes
# Q17: state MPAs
# Q18: sanctuaries
# Q19: ranking management
# Q20: climate causes
# Q20a: climate perceptions
# Q21: confidence
# Q22: zip code
# Q23: years in CA
# Q24: race
# 24a: Asian
# Q25: gender
# Q26: year born
# Q27: income
# Q28: education
# Q29: household size
# Q30: household age
# Q31: swimming
# Q32: thoughts
# Q33: management


# -------------------------------------------------------
##Retain Columns w/ Data you Want to Work With
# ecosytem services -----------------------------------

# list from questions  
es<-c("support a diversity of marine life",
"inspire artistic and creative expression",
"help produce and renew clean air and water",
"are important for future generations",
"provide fisheries, recreation, or tourism opportunities that support economic benefit",
"provide opportunities for education learning and science",
"help define our heritage culture and identity",
"are a place for our favorite outdoor recreation activities",
"help us feel connected to the natural world",
"care for us when we care for them")%>%
  glimpse()
glimpse(es)

# shorter version
es2<-c("biodiversity",
                "artistic_inspiration",
                "clean_air_water",
                "future_generations",
                "economic_benefit",
                "education_learning_science",
                "heritage_culture_identity",
                "recreation",
                "connected_to_nature",
                "care")%>%
  glimpse()


#select ecosystem services question --------------------------
d12<-d1%>%
  select(starts_with("Q12"))%>%
  glimpse()

##Transform Data so that each column is a survey response and each row is a prompt
d12a<-as.data.frame(t(d12))%>%
  mutate(services=es2,services2=es)%>%
  pivot_longer(cols = !c(services,services2))%>%
  select(-name)%>% # keep to link to demographics
  glimpse()
d12a



# repeat for LOW income ------------------
unique(d1$Q27)

d12_low<-d1%>%
  filter(Q27=="Less than $59,999")%>%
  select(starts_with("Q12"))%>%
  glimpse()

##Transform Data so that each column is a survey response and each row is a prompt
d12a_low<-as.data.frame(t(d12_low))%>%
  mutate(services=es2,services2=es)%>%
  pivot_longer(cols = !c(services,services2))%>%
  select(-name)%>% # keep to link to demographics
  glimpse()
d12a

# save -----------------------------
write_csv(d12a,"./results/q12_es_long.csv")
write_csv(d12a_low,"./results/q12_es_long_low.csv")

