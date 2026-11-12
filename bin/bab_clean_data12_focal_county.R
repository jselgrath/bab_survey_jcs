# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: make a column for focal counties

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(stringr)
library(utils)

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# read file
d1 <- read_csv("./results/data_long11.csv") %>%
  glimpse()

focal_counties <- c(
  "San Diego", "Los Angeles", "Ventura", "Santa Barbara",
  "Monterey", "Santa Cruz", "San Francisco", "Sonoma", 
  "Humboldt", "Del Norte")

d2<- d1 %>%
  mutate(
    county_focal_b = if_else(County_Name_1 %in% focal_counties, 1, 0),
    county_focal_yr = case_when(
      Phase == "Phase_1" ~ 2024,
      Phase == "Phase_2" ~ 2025,
      TRUE ~ NA_real_    )  )%>%
  glimpse()

d2%>%
  filter(county_focal_b!=1)%>%
  glimpse()

# save
write_csv(d2,"./results/data_long12.csv")