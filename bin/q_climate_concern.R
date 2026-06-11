# California Ocean Access: Benefits and climate_concerns (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: subset cleaned data for graphing and making long version - climate observations and concerns
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
  dplyr::select(QClimate_Obs_S_1:QClimate_Conc_E_7,ResponseId,zip_code,Focus_Group,Phase)%>%  # reduce data
  glimpse()
names(d1)
d1

# ----------------------------------------------
# climate concerns list from questions  
# social
cc_s<-c("Costs to visit ocean and/or participate in ocean activities", 
      "Existence of working coastal infrastructure (e.g., piers, seawalls)", 
      "Rules about how I use the ocean (e.g., fishing restrictions, swimming closures)", 
      "Local people being displaced (i.e., moving away) from ocean and coastal neighborhoods")%>%
  glimpse()

# environmental
cc_e<-c("Wildfires", "Storms, storm surge, and/or floods", "Air temperature", 
        "Abundance and/or diversity of marine plants and animals", 
        "Ocean temperature", "Ocean water quality", "Sea level rise")%>%
  glimpse()


# cc - shorter version

# social
cc2_s<-c("Ocean visit & activity costs", 
       "Coastal infrastructure", 
       "Ocean usage rules", 
       "Local displacement")%>%
  glimpse()

# environmental
cc2_e<-c("Wildfires", 
       "Storms & flooding", "Air temperature", 
       "Marine life diversity", 
       "Ocean temperature", "Water quality", 
       "Sea level rise")%>%
  glimpse()
# ----------------------------------------------



# join codes and question
#social
# observations
cc3_s <- tibble(
  question = paste0("QClimate_Obs_S_", 1:length(cc2_s)),
  climate_concern   = cc2_s,
  climate_concern_l   = cc_s,
  climate_concern_type="social",
  climate_concern_type2="observation"
)%>%
  glimpse()
cc3_s

# concern
cc3_s2 <- tibble(
  question = paste0("QClimate_Conc_S_", 1:length(cc2_s)),
  climate_concern   = cc2_s,
  climate_concern_l   = cc_s,
  climate_concern_type="social",
  climate_concern_type2="concern"
)%>%
  glimpse()
cc3_s



#environental
# observations
cc3_e <- tibble(
  question = paste0("QClimate_Obs_E_", 1:length(cc2_e)),
  climate_concern   = cc2_e,
  climate_concern_l   = cc_e,
  climate_concern_type="environmental",
  climate_concern_type2="observation"
)
cc3_e

# concerns
cc3_e2 <- tibble(
  question = paste0("QClimate_Conc_E_", 1:length(cc2_e)),
  climate_concern   = cc2_e,
  climate_concern_l   = cc_e,
  climate_concern_type="environmental",
  climate_concern_type2="concern"
)%>%
  glimpse()
cc3_e2



cc3<-tibble(rbind(cc3_s,cc3_e,cc3_s2,cc3_e2))%>%
  glimpse()
  
cc3

# ----------------------------------






# wide to long - fg
d2 <- d1 %>%
  pivot_longer(
    cols = QClimate_Obs_S_1:QClimate_Conc_E_7,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(cc3, by = "question")%>%
  glimpse()


# wide to long - all
d3 <- d0 %>%
  pivot_longer(
    cols = QClimate_Obs_S_1:QClimate_Conc_E_7,
    names_to = "question",
    values_to = "response"
  ) %>%
  left_join(cc3, by = "question")%>%
  glimpse()


# save -----------------------------
write_csv(d2,"./results/q_cc_long_fg.csv")
write_csv(d3,"./results/q_cc_long.csv")



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
# Q11: climate_concern
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
