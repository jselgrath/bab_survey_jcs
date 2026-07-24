# California Ocean Access: Benefits and wellbeings (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: subset cleaned data for graphing and making long version - wellbeing

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

# focus group version - fg2 has all h and dn counties not just fg areas
d1<-read_csv("./results/data_long9_fg2.csv")%>%
  dplyr::select(QWB_1:QWB_9,ResponseId,zip_code,Focus_Group,Phase)%>%  # reduce data
  glimpse()
names(d1)
d1

# ----------------------------------------------
# wb list from questions  
wb<-c("important source of food for myself and/or my family",
      "provide me with income or a job",
      "help me to feel a part of community",
      "help me to build or maintain relationships with my friends or family",
      "support plants, animals, and/or activiites that are important to my culture and heritage",
      "allow me to participate in adventerous and exciting activities",
      "improve and/or maintain my physical health and/or mental wellbeing",
      "help me feel more spiritual and/or connected to a higher power",
      "expose me to beauty and/or sensory experiences (e.g., wind, light, smell, sound) that are important to me")%>%
  glimpse()




# wb - shorter version
wb2<-c("food",
       "income",
       "community",
       "relationships",
       "culture/heritage",
       "adventure",
       "physical/mental health",
       "spirituality",
       "beauty/sensory experiences")%>%
  glimpse()
# ----------------------------------------------



# join codes and question
wb3 <- tibble(
  question = paste0("QWB_", 1:length(wb2)),
  wellbeing   = wb2,
  wellbeing_l   = wb
)

# ----------------------------------






# wide to long - fg
d2 <- d1 %>%
  pivot_longer(
    cols = QWB_1:QWB_9,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(wb3, by = "question")%>%
  glimpse()


# wide to long - all
d3 <- d0 %>%
  pivot_longer(
    cols = QWB_1:QWB_9,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(wb3, by = "question")%>%
  glimpse()


# save -----------------------------
write_csv(d2,"./results/q_wb_long_fg.csv")
write_csv(d3,"./results/q_wb_long.csv")



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
