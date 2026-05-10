# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: graph activity data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(lubridate)

# assign binary variable to fishing as most important activity from online surveys 2024-09-05 to 2024-11-04 based on influencer

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# read file
d1<-read_csv("./results/data_long7.csv")%>%
  
  # note if likely aw influencer respondent
  mutate(response_id=ResponseId)%>%
  mutate(
    influencer_aw_b = if_else(
      # 1. Date check: Is RecordedDate between Sept 15 and Sept 18, 2024? - MPA collaborative meeting on Sept 4
        date(RecordedDate) >= as.Date("2024-09-05") & 
        date(RecordedDate) <= as.Date("2024-11-04") &
        
        # 2. Mechanism check
        Mechanism == "Online" &
        
        #3. Activity check (using your original checkbox column)
        QImportant_Activities_Most == "Fishing or collecting food",
      
      # Results
      1, 0,
      
      # Handle NAs (if any value is missing, default to 0)
      missing = 0
    )
  )%>%
  glimpse()

influencer_t<-tibble(table(d1$influencer_aw_b))
influencer_t

# save ---------------
write_csv(d1,"./results/data_long8.csv")



