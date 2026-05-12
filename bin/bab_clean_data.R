# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath - base code from Tim Frawley
# California Marine Sanctuary Foundation/ CINMS

# goal: organize and clean data from qualtrics

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
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./data/Merged_Cleaned_QC_Final_5.8.2026.csv")%>% #Merged_Cleaned_QC_Final_v1.csv
  glimpse()

d1
colnames(d1)

##Save row with question Specific Prompts for later
# view(d1[1,]) # prompts
tail(d1[2,]) # I think this row is not needed

# questions
d_questions<-d1[1,]

#misc, not needed row
d1[2,]

#remove questions and misc row  
d4<-d1[3:nrow(d1),]
glimpse(d4)  

# remove people who took survey 2xby email
d5 <- d4 %>%
  # Convert emails to lowercase to ensure "User@me.com" matches "user@me.com"
  mutate(Email = tolower(Email)) %>% 
  # keeps only the first occurrence of each email
  filter(!duplicated(Email) | is.na(Email))%>%
  glimpse()

# export formatted data --------------------------------
write_csv(d4,"./results/data_long_duplicate_emails.csv")
write_csv(d5,"./results/data_long.csv")
write_csv(d_questions,"./results/data_questions.csv")
