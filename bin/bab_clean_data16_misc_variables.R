# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: misc variables needed for analysis

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# read file
d1 <- read_csv("./results/data_long15.csv") %>%
  glimpse()


d2 <- d1 %>%
  mutate(
    age_group = case_when(
      age >= 18 & age <= 24 ~ "18-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65             ~ "65+",
      TRUE                  ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    )
  )

write_csv(d2,"./results/data_long16.csv")