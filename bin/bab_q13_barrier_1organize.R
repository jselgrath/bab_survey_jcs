# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation


# goal: organize focus group data for barriers question 

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 

# ---------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

# create barrier question codes to join with data --------------------------------
barrier1<-c(
  "I am interested in ocean experiences or activities", 
  "I have enough time to go to ocean and coastal areas",
  "I feel welcome at beaches and coastal areas",
  "I have access to and can afford  transportation to the beach or coasts (e.g., bus, car)",
  "The amenities/infrastructure I need or want are available",
  "I possess the required skills or knowledge to participate in ocean activities (i.e., swimming, fishing, surfing)",
  "I can afford or have access to necessary tools, gear, or equipment (e.g., fishing pole, surf board)",
  "I feel safe from environmental factors (i.e. sharks, waves, wind, pollution, etc.)",
  "I understand the associated rules and regulations (e.g., fishing rules, parking rules)",
  "I feel protected by law or rule enforcers (e.g. police, rangers, security guards)")

barrier2 <- c(
  "Interested in ocean activities",
  "Enough time",
  "Feel welcome at coastal areas",
  "Affordable/accessible transportation",
  "Amenities & infrastructure available",
  "Possess required skills/knowledge",
  "Access to necessary tools/gear",
  "Safe from environmental factors",
  "Understand rules & regulations",
  "Feel protected by law & rule enforcers"
)


# join codes and question
barrier3 <- tibble(
  question = paste0("QBarriers_", 1:length(barrier2)),
  barrier   = barrier2
)





###Load in Data  -----------------------------
d0<-read_csv("./results/data_long9.csv")%>%
  glimpse()

# focus group version
d1<-read_csv("./results/data_long9_fg.csv")%>%
  dplyr::select(QBarriers_1:QBarriers_10,ResponseId,zip_code,Focus_Group,Phase)%>%  # reduce data
  glimpse()
names(d1)
d1

# focus group list version
l1<- read_rds("./results/data_long9_fg.rds")
l1$Santa_Rosa


# wide to long - fg
d2 <- d1 %>%
    pivot_longer(
    cols = QBarriers_1:QBarriers_10,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(barrier3, by = "question")%>%
  glimpse()


# wide to long - all
d3 <- d0 %>%
  pivot_longer(
    cols = QBarriers_1:QBarriers_10,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(barrier3, by = "question")%>%
  glimpse()


# save -----------------------------
write_csv(d2,"./results/q13_barrier_long_fg.csv")
write_csv(d3,"./results/q13_barrier_long.csv")





# Question list (2025)##### -------------------------------------------
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



