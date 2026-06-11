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
# d0<-read_csv("./results/data_long6.csv")%>%
#   mutate(response_id=ResponseId)%>%
#   glimpse()

folder_path <- "./data/comments/" # for files with comments


# list .csv files
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
csv_files

#  list and clean column names
temp_list <- csv_files %>%
  set_names(nm = basename(.) %>% tools::file_path_sans_ext()) %>%
  map(~read_csv(.x) %>% clean_names())


# better names
original_names <- names(temp_list)
original_names

# Remove the prefix, remove the suffix
clean_names <- gsub("q32_bab_survey_2025_comments_", "", original_names)
clean_names <- gsub("q32_mec_survey_2024_comments_", "", clean_names)
clean_names <- gsub("_CODED", "", clean_names)

# Assign them back to your list (or a new list)
names(temp_list) <- clean_names
names(temp_list)

# summary of all columns and their types across all files
structure_comp <- compare_df_cols(temp_list)

# results -----------------------------
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
  select(-x1,-x1,-x2,-x3,-x4,-x5,-x6,-x7,-coding,-x9,-x10,-x11,-x12,-x12,-x13,-x14,-x15,-x16,-x17,-x18,-x19,-x20,-x21,-x22,-x24,-x25)%>%
  glimpse()


# set NA to 0 for coding ------------------------------------
names(final_df)

# List of coded columns
cols_to_fix <- c("barriers", "climate_change", "concerns", "joy", "management", "marine_life", "mobility", "mpa_sanctuary", 
                 "ocean_values", "places", "relationship", "social_change", "solutions", "use_activity", "wellbeing", "appreciation_for_survey")

final_df2 <- final_df %>%
  mutate(across(all_of(cols_to_fix), ~replace_na(., 0)))%>%
  mutate(comments_notes=notes)%>%
  select(-notes)%>%
  unique()%>%
  glimpse()


names(final_df2) <- c("file_origin","com_version","comment","comment_clean","com_barriers", "com_climate_change", "com_concerns", "com_joy", "com_management", "com_marine_life", "com_mobility", "com_mpa_sanctuary", 
                 "com_ocean_values", "com_places", "com_relationship", "com_social_change", "com_solutions", "com_use_activity", "com_wellbeing", "com_appreciation_for_survey","com_other_fill_in","response_id","comment_notes")
glimpse(final_df2)


# Pull out all rows where response_id is duplicated
duplicated_rows <- final_df2 %>%
  group_by(response_id) %>%
  filter(n() > 1) %>%
  ungroup()%>%
  glimpse()
# no duplicates


# merge cleaned comments with full dataset
# d1<- d0%>%
#   right_join(final_df2)%>%
#   glimpse()

# view(final_df2)


final_df3<-final_df2%>%
  mutate(comment_clean=if_else(comment_clean=="No response",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="no response",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="Not really.",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="no",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="No",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="No thoughts.",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="NO",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="None.",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="none.",NA,comment_clean))%>%
  mutate(comment_clean=if_else(comment_clean=="See Jenny's notes.",NA,comment_clean))%>%
  
  
  filter(!is.na(comment_clean))%>%
  
  view()
  
  

# save ---------------------
write_csv(final_df3,"./results/q_comments_cleaned.csv") 
