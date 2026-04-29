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
# setwd("C:/Users/jennifer.selgrath/Documents/research/r_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# -------------------------------
# -- data cleaning --



# combine honorarium versions of survey
# source("./bin/bab_combine_honorarium.R")
# input: folder with raw data from qualtrics 2025
# output: ./results/data_honorarium_versions.csv


# import joined and cleaned data from qualitrics
source("./bin/bab_clean_data.R")
# input:        ./data/Merged_Cleaned_QC_Final_v1.csv
# output:       ./results/data_questions.csv
# output:       ./results/data_long.csv


source("./bin/bab_clean_data_race_q24.R")
# input:       ./results/data_long.csv
# output:      ./results/data_long2.csv

source("./bin/bab_clean_data_gender_q25.R")
# input:       ./results/data_long2.csv
# output:      ./results/data_long3.csv

source("./bin/bab_clean_data_activities_q4_q5.R")
# input:       ./results/data_long3.csv
# output:      ./results/data_long4.csv

# source("./bin/bab_clean_data_remove_headers.R")
# input:       ./results/data_long4.csv
# output:      ./results/data_long5.csv

# undergrad projects ----------------------------

# new spring 2026 - will get merged later
source("./bin/bab_new_temp_for_undergrads.R")
# input:       ./results/data_long4.csv
#              ./data/bab_survey_2026_tribal_20260427_20_new.csv
#              ./data/bab_survey_2025_in_person_prize - norcal_20260427_23_new.csv
# output:     ./doc/undergrad_projects_20260428.csv 


# q3 maps - transform pixel coordinates to lat long
source("./bin/bab_q3_maps2_georeferencing.R")
# input:      ./data/bab_q3_map_calculations_20260227.csv
#             ./doc/undergrad_projects_20260428.csv 
# output:     ./results/q3_coordinates_all.csv
#             ./results/q3_coordinates.shp
#             ./results/q3_coordinates.gdb  layer = "beach_access"

# q3 maps - delete points >5km from coast and snap points <5km from coast to coast
source("./bin/bab_q3_maps_snap_to_coast.R")
# input:      ./results/q3_coordinates.shp
#             ./results/q3_coordinates.gdb  layer = "beach_access"    
# output:     ./gis/q3_coordinates_2.shp 
#             ./results/q3_coordinates.gdb  layer = "beach_access_2"

# nadia - zip codes
source("./bin/bab_respondents_by_zip.R")


# pull comments for analysis
source("bab_pull_comments_q32.R")
# input: ./results/data_long5.csv
# output: ./results/q32_mec_survey_2024_comments_online2.csv # repeated for each survey version

# --------------------------------


# -- activity questions --
source("./bin/bab_q4_q5.R")
# input:       ./results/data_long5.csv
# output:      

source("./bin/bab_q4_q5_race.R")
# input:       ./results/data_long5.csv
# output:      

# -- mpa questions --
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