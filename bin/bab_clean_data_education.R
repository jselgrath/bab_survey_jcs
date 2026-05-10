# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean education data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long4.csv")%>%
  # select(ResponseId,UserLanguage,"QMapping_North_County":"Final_Y",Comments,QDemographic_PrimaryZip:QDemographic_Swimming, Mechanism, Version)%>%
  glimpse


# clean education
d2<- d1 %>%
  mutate(
    # normalize punctuation/spacing
    QDemographic_Education = stringr::str_replace_all(QDemographic_Education, "’", "'"),
    QDemographic_Education = stringr::str_squish(QDemographic_Education),
    
    # map by pattern (robust to small text differences)
    q_demographic_education_clean = dplyr::case_when(
      stringr::str_detect(QDemographic_Education, regex("^some high school|GED", ignore_case = TRUE)) ~ "HS or GED",
      stringr::str_detect(QDemographic_Education, regex("^vocational", ignore_case = TRUE))           ~ "Vocational",
      stringr::str_detect(QDemographic_Education, regex("^some college|associate", ignore_case = TRUE)) ~ "Some college/Associate",
      stringr::str_detect(QDemographic_Education, regex("bachelor", ignore_case = TRUE))              ~ "Bachelor’s",
      stringr::str_detect(QDemographic_Education, regex("^graduate|masters|master's|phd|professional", ignore_case = TRUE)) ~ "Graduate",
      stringr::str_detect(QDemographic_Education, regex("^other$", ignore_case = TRUE))               ~ "Other",
      TRUE ~ NA_character_   # anything unmatched becomes NA so we can drop it
    )
  ) %>%
  select(-QDemographic_Country_State,-QDemographic_Education)%>%
  glimpse()

#save
write_csv(d2,"./results/data_long5.csv")
