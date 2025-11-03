# Equity in Ocean Access (MPAs Equity and Climate Change (mec))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: driver file for code to analyze surveys about ocean access done in the summer and fall of 2024 

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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")

# import joined data from qualitrics, remove low quality data, and organize headers
source("./bin/mec_clean_data.R")
# input:        ./data/Combined_Data_8.27.24.csv
# output:       ./results/data_wide.csv

# Ecosystem services ------------------------------
# organize ecosystem services data 
source("./bin/mec_q12_es_1organize.R")
# input:      ./results/data_wide.csv
# output:     ./results/q12_es_long.csv
#             ./results/q12_es_long_low.csv

# summarize and graph ecosystem services data & separate code for low income only
source("./bin/mec_q12_es_1summarize.R")
# input:       ./results/q12_es_long.csv
# output:  

# Barriers ------------------------------
# organize barrier data
source("./bin/mec_q13_barrier_1organize.R")
# input:      ./results/data_wide.csv
# output:     ./results/q12_barrier_long.csv
#             ./results/q12_barrier_long_low.csv

# summarize and graph barrier data & separate code for low income only
source("./bin/mec_q13_barrier_2summarize.R")
# input:       ./results/q12_barrier_long.csv
# output:      ./doc/q13_barrier_low.png
#              ./doc/q13_barrier.png 


# Activities ------------------------------
# select activities where fishing is important, clean species names
source("./bin/mec_q4_5_activity_1organize.R")
# input:    ./results/data_wide.csv    
# output:   ./results/species_caught.csv
#           ./results/fishing_culture.csv

# make wordcloud of important fished species
source("./bin/mec_q4_5_activity_2fished_species_wordcloud.R")
# input:     
# output:  

# select and organize climate related questions (q15,q16,q20)
source("./bin/mec_q20_15_16_change_climate.R")
# input:    ./results/data_wide.csv    
# output:   ./results/q15_change_observed_long.csv
#           ./results/q16_concern_long.csv
#           ./results/q20_climate_belief_long.csv
#           ./results/q20a_climate_concern_long.csv

# rec fishers only version
source("./bin/mec_q20_15_16_change_climate_fishing.R")
# input:    ./results/data_wide.csv    
# output:   ./results/q15_change_observed_long_fishing.csv
#           ./results/q16_concern_long_fishing.csv
#           ./results/q20_climate_belief_long_fishing.csv
#           ./results/q20a_climate_concern_long_fishing.csv


# organize Q15 - observed changes
source("./bin/mec_q15_observations_2summarize.R")
source("./bin/mec_q15_observations_2summarize_fishing.R")
# input:    ./results/q15_change_observed_long.csv    
#           ./results/q15_change_observed_long_fishing.csv
# output:   ./doc/q15_changes_obsv_climate_only.png #or _fishing
#           ./doc/q15_changes_obsvg.png             #or _fishing

# organize Q16 - anticipated changes
source("./bin/mec_q16_observations_2summarize.R")
source("./bin/mec_q16_observations_2summarize_fishing.R")
# input:    ./results/q16_concern_long.csv    
#           ./results/q16_concern_long_fishing.csv
# output:   ./doc/q16_changes_obsv.png                #or _fishing
#           ./doc/q16_changes_obsv_climate_only.png   #or _fishing

# organize Q20a - climate 
source("./bin/mec_q20_climate_2summarize.R")
source("./bin/mec_q20_climate_2summarize_fishing.R")
# input:    ./results/q20_climate_belief_long         #or _fishing
# output:   ./doc/q20_belief.png                      #or _fishing


# organize Q20a - climate 
source("./bin/mec_q20a_climate_2summarize.R")
source("./bin/mec_q20a_climate_2summarize_fishing.R")
# input:    ./results/q20a_climate_concern_long.csv    
#           ./results/q20a_climate_concern_long_fishing.csv
# output:   ./doc/q20a_opinion_all.png                #or _fishing
#           ./doc/q20a_knowledge.png                  #or _fishing
#           ./doc/q20a_opinion_personal.png           #or _fishing

# organize Q20a - climate 
source("./bin/mec_q20_climate_2summarize.R")
source("./bin/mec_q20_climate_2summarize_fishing.R")
# input:    ./results/q20a_climate_concern_long.csv    
#           ./results/q20a_climate_concern_long_fishing.csv
# output:   ./doc/q20a_opinion_all.png                #or _fishing
#           ./doc/q20a_knowledge.png                  #or _fishing
#           ./doc/q20a_opinion_personal.png           #or _fishing
