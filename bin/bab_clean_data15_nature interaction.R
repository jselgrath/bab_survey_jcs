# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: add columns for nature interaction - broad and strict

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# read file
d1 <- read_csv("./results/data_long14.csv") %>%
  glimpse()


# make var for nature activities ------------
d1 %>%
  count(q_activity_most3, sort = TRUE)

d2 <- d1 %>%
  mutate(
    # Strict definition: Direct interaction with marine species, tidepools, or ecosystems
    nature_interact_strict = case_when(
      is.na(q_activity_most3) ~ NA_real_,
      str_detect(
        q_activity_most3,
        regex("Nature Observing|Fishing|Collecting|Tidepool|Snorkeling|Scuba|Kayak|Paddle|Boating|Cultural|Volunteering", ignore_case = TRUE)
      ) ~ 1,
      TRUE ~ 0
    ),
    
    # Broad definition: Adds ocean immersion & shoreline movement (swimming, surfing, walking)
    nature_interact_broad = case_when(
      is.na(q_activity_most3) ~ NA_real_,
      nature_interact_strict == 1 ~ 1,
      str_detect(
        q_activity_most3,
        regex("Swimming|Bodysurfing|Surfing|Walking|Running|Hiking|Paid Work", ignore_case = TRUE)
      ) ~ 1,
      TRUE ~ 0
    ),
    
    # Labeled factor version for clean plots and tables
    nature_interact_strict_l = factor(
      nature_interact_strict,
      levels = c(1, 0),
      labels = c("Direct Nature Interaction", "Other Coastal Activity")
    ),
      nature_interact_broad_l = factor(
      nature_interact_broad,
      levels = c(1, 0),
      labels = c("Direct Nature Interaction", "Other Coastal Activity")
    ) )%>%
  glimpse()

# 2. Cross-tabulate to verify every category mapped as expected
d2 %>%
  count(q_activity_most3, nature_interact_strict, nature_interact_broad)


# save----
write_csv(d2,"./results/data_long15.csv")

