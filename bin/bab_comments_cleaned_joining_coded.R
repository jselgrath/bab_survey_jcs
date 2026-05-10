# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

# goal: separate open comment question with respondent ID from other data for cleaning
# updated function - done after most surveys were cleaned except last three

# notes on codes-----------
# hdn - humboldt del norte
# sn - sonoma
# sb -  santa barbara, pilot
# la - los angeles
# sd - san diego
# op - online prize
# tp - tribal pilot
# ippsc - in person prize southern calfornia


# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(textclean)
library(stringi)
library(readxl)
library(janitor)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# demographics etc
d0<-read_csv("./results/data_long6.csv")%>%
  mutate(response_id=ResponseId)%>%
  glimpse()

folder_path <- "./data/comments/" # for files with comments


# list .csv files
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# 2. Read them into a list and clean column names
# This ensures "Survey Name" becomes "survey_name" across all files
temp_list <- csv_files %>%
  set_names(nm = basename(.) %>% tools::file_path_sans_ext()) %>%
  map(~read_csv(.x) %>% clean_names())


# better names
colnames(temp_list[1])
temp_list
names(temp_list)<-c("hdn","ippsc","la","op","sb","sd","sn","tp","online2","online3","sf","ucsb","ucsc")


# This creates a summary of all columns and their types across all files
structure_comp <- compare_df_cols(temp_list)

# View the results
# Columns that are missing in a specific file will show up as NA
print(structure_comp)

# To see ONLY the columns that aren't the same everywhere:
mismatched_cols <- structure_comp %>%
  filter(apply(., 1, function(x) length(unique(na.omit(x))) > 1))

print(mismatched_cols)

# make all characters
temp_list2 <- map(temp_list, ~ mutate(.x, across(everything(), as.character)))

final_df <- bind_rows(temp_list2, .id = "file_origin")%>%
  filter(!is.na(comment_clean))%>%
  select(-x23)%>%
  mutate(across(
    .cols = barriers:appreciation_for_survey, 
    .fns = ~ as.numeric(as.character(.))))%>%
  mutate(response_id=response)%>%
  select(-response)%>%
  glimpse()
final_df 

# set NA to 0 for coding ------------------------------------
names(final_df)

# List of coded columns
cols_to_fix <- c("barriers", "climate_change", "concerns", "joy", "management", "marine_life", 
                 "mobility", "mpa_sanctuary", "ocean_values", "places", "relationship", 
                 "social_change", "solutions", "use_activity", "wellbeing", 
                 "appreciation_for_survey")

final_df2 <- final_df %>%
  mutate(across(all_of(cols_to_fix), ~replace_na(., 0)))%>%
  mutate(comments_notes=notes)%>%
  select(-notes)%>%
  glimpse()

# merge cleaned comments with full dataset
d1<- d0%>%
  right_join(final_df2)%>%
  glimpse()


# save ---------------------
write_csv(d1,"./results/q32_bab_comments_cleaned_demographics_not_all_versions.csv")
