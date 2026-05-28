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
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))

# setwd("C:/Users/jennifer.selgrath/Documents/research/r_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# ------------------------------
# undergrad project 1: cleaned comments - q32, q50 in two phases of data collection
# ------------------------------
# pull comments for analysis
# done to allow CSUSM students to clean comment data
# ------------------------------
# source("./bin/bab_pull_comments_q32.R")
# input:  ./data/Merged_Cleaned_QC_Final_5.8.2026.csv
# output: ./results/q32_mec_survey_2024_comments_online2.csv # repeated for each survey version

# convert cleaned comment files to .csv
# Note: this version is missing the surveys closed in April 2026. Will need to rerun and update when final coding is completed.
source("./bin/bab_comments_cleaned_xlsx_to_csv.R") # good but overwrites files (there is a backup)
# input:        folder_path <- "./data/comments/"
# output:       ./data/comments/ # for list of .csv files with comments > have been manually cleaned - do not overwrite! (I think they were overwritten - use backup)

# merge comments with full dataset
# Note: this version is missing the surveys closed in April 2026. Will need to rerun and update when final coding is completed.
source("./bin/bab_comments_cleaned_joining_coded.R")
# input:        folder_path <- "./data/comments/" . csv files
# output:       ./results/q32_bab_comments_cleaned_not_all_versions.csv"




# ------------------------------
# -- data cleaning --
# ------------------------------
# import joined and cleaned data from qualitrics
# remove people who took survey 2x using email
source("./bin/bab_clean_data.R")
# input:        ./data/Merged_Cleaned_QC_Final_5.8.2026.csv
# output:       ./results/data_questions.csv
# output:       ./results/data_long.csv

# clean race categories - currently keeps north african separately, but has code to merge with white
source("./bin/bab_clean_data_race_q24.R")
# input:       ./results/data_long.csv
# output:      ./results/data_long2.csv

# clean gender
source("./bin/bab_clean_data_gender_q25.R") 
# input:       ./results/data_long2.csv
# output:      ./results/data_long3.csv

# clean activities
source("./bin/bab_clean_data_activities_q4_q5.R") 
# input:       ./results/data_long3.csv
# output:      ./results/data_long4.csv

# clean education
source("./bin/bab_clean_data_education.R") # updating this...
# input:       ./results/data_long4.csv
# output:      ./results/data_long5.csv


# clean county names for maps and q3 ----------------
source("./bin/bab_clean_counties.R")
# input:       ./results/data_long5.csv
# output:      ./results/data_long6.csv


# clean other activity text and make activity columns for some variables (e.g., consumptive/non-consumptive; dog walking)
source("./bin/bab_clean_data_activities_text_comments.R")
# input:       ./results/data_long6.csv
#              ./results/q32_bab_comments_cleaned_not_all_versions.csv  # comments - for activities and dogs
# Note: this version is missing the surveys closed in April 2026. Will need to rerun and update when final coding is completed.
# output:      ./results/data_long7.csv

# make column for influencer fishing responses
source("./bin/bab_clean_data_fishing_influencer.R")
# input:       ./results/data_long7.csv
# output:      ./results/data_long8.csv
#              ./doc/fishing_influencer_summary.csv


# chart relative influence of influencer vs other online responses - all data
# source("./bin/bab_fishing_most_table_all2.R")
# input:       ./results/data_long8.csv
# output:      ./doc/activity_fishing_any_monthly_all.csv
#              ./doc/activity_fishing_any_summaries_all.csv
#              ./doc/activity_fishing_most_monthly_all.csv
#              ./doc/activity_fishing_most_summaries_all.csv

# chart relative influence of influencer vs other online responses - online data only
source("./bin/bab_fishing_most_table_online.R")
# input:       ./results/data_long8.csv
# output:      ./doc/activity_online_fishing_most_monthly.csv
#              ./doc/activity_online_fishing_most_summaries.csv
#              ./doc/activity_online_fishing_any_monthly.csv
#              ./doc/activity_online_fishing_any_summaries.csv

# graph effect of influencer on total data and data by month
# source("./bin/bab_fishing_influencer_graph.R")
# input:       ./doc/activity_fishing_any_monthly_all.csv
#              ./doc/activity_fishing_any_summaries_all.csv
#              ./doc/activity_fishing_most_monthly_all.csv
#              ./doc/activity_fishing_most_summaries_all.csv
#              ./doc/activity_online_fishing_most_monthly.csv
#              ./doc/activity_online_fishing_most_summaries.csv
#              ./doc/activity_online_fishing_any_monthly.csv
#              ./doc/activity_online_fishing_any_summaries.csv
# output:      ./doc/influencer_pct.png
#              ./doc/influencer_pctP_time.png

# graph effect of influencer on total data and data by month
source("./bin/bab_fishing_influencer_graph2.R")
# input:       ./results/data_long8.csv
# output:      ./doc/fishing_influencer_time.png



# ------------------------------
# undergrad project 2:  location on the coastline where you spend the most time - q3
# student: emily lombardi, ucsb
# ------------------------------
# goal: pull map codes for each survey for figuring out conversions from pixel space to geographic space
# source("./bin/bab_q3_maps.R")
# input:      ./results/data_long8.csv # was data long 5 when version we used was made
# output:     ./doc/q3_bab_survey_2025_summary_report.csv
#             ./results/q3_bab_survey_2025_maps_hdn_georect.csv # for all survey versions


# q3 maps - transform pixel coordinates to lat long
source("./bin/bab_q3_maps2_georeferencing.R")
# input:      ./data/bab_q3_map_calculations_20260227.csv # alamdea is missing from calculation,but no alameda maps from survey
#             ./results/data_long8.csv 
# output:     ./results/data_long9.csv
#             ./results/q3_coordinates_all.csv
#             ./gis/q3_coordinates.shp
#             ./gis/q3_coordinates.gdb  layer = "beach_access"


# q3 maps - delete points >5km from coast and snap points <10km from coast to coast. for land points only.
source("./bin/bab_q3_maps_snap_to_coast.R")
# input:      ./gis/q3_coordinates.shp
#             ./gis/q3_coordinates.gdb  layer = "beach_access"    
# output:     ./gis/q3_coordinates_2.shp 
#             ./gis/q3_coordinates.gdb  layer = "beach_access_2"


# q3 maps - delete points >5km from coast and snap points <10km from coast to coast. for ocean and land points,
source("./bin/bab_q3_maps_snap_to_coast2.R")
# input:      ./gis_data/coastline_ca/Coastline_CA.shp
#             ./gis/q3_coordinates.gdb  layer = "beach_access"    
# output:     ./gis/q3_coordinates_4.shp 
#             ./gis/q3_coordinates.gdb  layer = "beach_access_3"


# gis map: bab_q3_responses.aprx


# ------------------------------
# undergrad project 3: mapping zip codes where respondents live
# student: nadia garcia, csusm
# ------------------------------
# bab_clean_subset_undergrad_projects.R

# zip codes
source("./bin/bab_respondents_by_zip.R")
# input:    ./doc/undergrad_projects_20260428.csv  
#             california_zip_codes3.shp
# output:   ./gis/bab_zip_sample_size.shp  
#           ./gis/bab_zip_sample_size.gdb", layer = "zip_sample_size"



# --------------------------------


# -- activity questions --
source("./bin/bab_q_activities.R")
# input:       ./results/data_long9.csv
# output:      ./doc/QImportant_Activities_activity_w2.png
#              ./doc/QImportant_Activities_Most_activity_w.png

source("./bin/bab_q_activity_q4_q5_race.R")
# input:       ./results/data_long9.csv
# output:      ./results/data_long9_race.csv
#              ./doc/QImportant_Activities_Most2_activity_race_pct_influencer_vs_noinfluencer.png
#              ./doc/QImportant_Activities_Most2_activity_race_count_alldata.png
#              ./doc/QImportant_Activities_Most2_activity_race_count_noinfluencer.png
#              ./doc/QImportant_Activities_Most2_activity_race_p_within_activity.png
#              ./doc/QImportant_Activities_Most2_activity_race_p_within_race.png
#              ./doc/QImportant_Activities_Most2_activity_race_difference.png

# -- mpa questions --
source("./bin/bab_q17_q18_mpa_q29_q30.R")
# input:       ./results/data_long8.csv
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