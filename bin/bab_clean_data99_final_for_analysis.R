# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: turn cleaning waterfall into final dataset for analysis

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)


# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# update this input to be the most cleaned version 
d1 <- read_csv("./results/data_long14.csv") %>%
  glimpse()

# save this as a file that can be input to post-cleaning analysis
write_csv(d1,"./results/data_final.csv")
