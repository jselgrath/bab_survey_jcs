# Equity in Ocean Access (Benefits and Barriers (bab))
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
# LOAD DATA
# --------------------------------------------------------------------------
rm(list = ls()) 
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d0 <- read_csv("./results/data_long9.csv")%>%
  glimpse()


###Load in Data  -----------------------------
d0<-read_csv("./results/data_long9.csv")%>%
  glimpse()

# focus group version
d1<-read_csv("./results/data_long9_fg2.csv")%>%
  dplyr::select(QES_1:QES_10,ResponseId,zip_code,Focus_Group,Phase)%>%  # reduce data
  glimpse()
names(d1)
d1

# focus group list version
l1<- read_rds("./results/data_long9_fg.rds")
l1$Santa_Rosa

# ----------------------------------------------
# es list from questions  
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



# es - shorter version
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
# ----------------------------------------------



# join codes and question
es3 <- tibble(
  question = paste0("QES_", 1:length(es2)),
  ecosystem_service   = es2,
  ecosystem_service_l   = es
)

# ----------------------------------






# wide to long - fg
d2 <- d1 %>%
  pivot_longer(
    cols = QES_1:QES_10,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(es3, by = "question")%>%
  glimpse()


# wide to long - all
d3 <- d0 %>%
  pivot_longer(
    cols = QES_1:QES_10,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(es3, by = "question")%>%
  glimpse()


# save -----------------------------
write_csv(d2,"./results/q_es_long_fg.csv")
write_csv(d3,"./results/q_es_long.csv")



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
