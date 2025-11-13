# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: driver file for code to analyze surveys about ocean access done in 2024 and 2025

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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

# import joined data from qualitrics, remove low quality data, and organize headers
source("./bin/bab_clean_data.R")
# input:        ./data/Combined_Data_8.27.24.csv
# output:       ./results/data_wide.csv
# output:       ./results/data_long.csv

source("./bin/bab_clean_data_race_q24.R")
# input:       ./results/data_long.csv
# output:      ./results/data_long2.csv

source("./bin/bab_clean_data_gender_q25.R")
# input:       ./results/data_long2.csv
# output:      ./results/data_long3.csv

source("./bin/bab_clean_data_activities.R")
# input:       ./results/data_long3.csv
# output:      ./results/data_long4.csv

source("./bin/bab_clean_data_remove_headers.R")
# input:       ./results/data_long4.csv
# output:      ./results/data_long5.csv

source("./bin/bab_q17_q18_mpa_q29_q30.R")
# input:       ./results/data_long5.csv
# output:      ./doc/q17_mpa_famil.png
#              ./doc/q18_nms_famil.png
#              ./doc/q29_mpa_purpose.png
#              ./doc/Q30_mpa_science.png

source("./bin/bab_q17_race2.R")
# input:       ./results/data_long5.csv
# output:      ./doc/q17_mpa_famil_race3.png

source("./bin/bab_q17_education2.R")
# input:       ./results/data_long5.csv
# output:      ./doc/q17_mpa_famil_education3.png

source("./bin/bab_q17_income2.R")
# input:       ./results/data_long5.csv
# output:      ./doc/q17_mpa_famil_income3.png

source("./bin/bab_q17_gender.R")
# input:       ./results/data_long5.csv
# output:      ./doc/q17_mpa_famil_gender3.png

source("./bin/bab_q17_activity.R")
# input:       ./results/data_long5.csv
# output:      ./doc/q17_mpa_famil_activity.png