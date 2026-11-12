# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: clean race data q_demographic_race for long data

# ----------------------------------------------------------
# load libraries ----------
library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)

# --------------------------------------------------------------------------
# load data --------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long1.csv")%>%
  mutate(q_demographic_race = as.character(QDemographic_Race))%>%
  glimpse()

writeClipboard(d1$QDemographic_Race)

# I want to organize the race data from my survey (data = d1) column = $QDemographic_Race

# Task 1: I want to clean and update the race column (old column:   new column: $q_demographic_race. I want to do general cleaning including removing duplicates of a single race (e.g., white, white), standardize "Native Hawaiian or Other Pacific Islander" to "Native Hawaiian or Pacific Islander", standardize "Other", "Another race or ethnicity, please specify:", and "Another race or ethnicity" to "Another race". when people select both specific races and also "Another race" or "Choose not to answer" I want to drop those non-specific categories. when people select all of the options or almost all I want to group them as "Another race". If values are NA then I want to classify them as "Choose not to answer". Because they have low samples I want to group Middle Eastern/North African as White (based on Historical OMB Directive 15 (1977–2024)) and Native Hawaiian/Pacific Islander as Asian (then that category can be updated to "Asian American/Pacific Islander"). Historical Federal Standards (1977–1997): Under the original Office of Management and Budget (OMB) Directive 15 established in 1977, the official federal reporting category was explicitly "Asian or Pacific Islander" (API). I want to keep native Americans distinct. Be sure to drop duplicates (e.g., if the were native hawaiian and asian). 

# Task 2: I would like to make binary categories for all original races ($race_white_b, $race_black_b, $race_latinx_b,$race_asian_b, $race_indigenous_b, $race_nhpi_b,$race_meha_b,$race_another_b,$race_na). 

# Task 3: I would like to make four new race columns: 
# 1) $q_demographic_race1_minority - follow OMB Bulletin 00-02 Allocation and prioritize classification of non-white races
# 2) $q_demographic_race2_indig - indigenous first (native americans and alaska natives) combined with with OMB Bulletin 00-02
# 3) $q_demographic_race3_equal - omb 2024 guidelines (co-equal multirace)
# 4) $q_demographic_race4 -  The Standard U.S. Census / Polling Benchmark with Hispanic Prioritized
# --------------------------------------


# -----------------------------------------------------------------------------
# functions ------
# -----------------------------------------------------------------------------

# clean a single race response into standardized original tokens
f_clean_original_race <- function(val, straightline_thresh = 6) {
  # 1. Reclassify missing/NA values as "Choose not to answer"
  if (is.na(val) || str_trim(val) == "" || str_trim(val) == "NA") {
    return("Choose not to answer")
  }
  
  # Split and trim
  tokens <- str_trim(unlist(str_split(val, ",")))
  tokens <- tokens[tokens != ""]
  
  # Standardize naming variations
  tokens <- case_when(
    tokens %in% c("Native Hawaiian or Other Pacific Islander", "Native Hawaiian or Pacific Islander") ~ "Native Hawaiian or Pacific Islander",
    tokens %in% c("Other", "Another race or ethnicity, please specify:", "Another race or ethnicity", "Another race") ~ "Another race",
    TRUE ~ tokens
  )
  
  # Deduplicate exact repeats
  tokens <- unique(tokens)
  
  # Straightliner rule: all or almost all options selected (>= 6 out of 9)
  if (length(tokens) >= straightline_thresh) {
    return("Another race")
  }
  
  # Drop non-specific options if specific races are also selected
  non_specific <- c("Another race", "Choose not to answer")
  specific <- setdiff(tokens, non_specific)
  
  if (length(specific) > 0) {
    return(specific)
  } else if ("Another race" %in% tokens) {
    # Keeps "Another race" distinct even if paired with "Choose not to answer"
    return("Another race")
  } else {
    return("Choose not to answer")
  }
}

# Helper function to collapse MENA -> White and NHPI + Asian -> Asian American/Pacific Islander
f_group_historical_omb <- function(tokens) {
  grouped <- case_when(
    tokens == "Middle Eastern or North African" ~ "White",
    tokens %in% c("Asian", "Native Hawaiian or Pacific Islander") ~ "Asian American/Pacific Islander",
    TRUE ~ tokens
  )
  
  # Deduplicate after collapsing
  unique(grouped)
}

# Execute Tasks 1, 2, and 3  ------------------
d2 <- d1 %>%
  mutate(
    # Intermediate list-column of cleaned original race selections
    .orig_tokens = map(QDemographic_Race, f_clean_original_race, straightline_thresh = 6),
    
    # -------------------------------------------------------------------------
    # TASK 2: Binary indicators for all original race categories
    # ("Another race" and "Choose not to answer" / NA kept distinct)
    # -------------------------------------------------------------------------
    race_white_b      = as.integer(map_lgl(.orig_tokens, ~ "White" %in% .x)),
    race_black_b      = as.integer(map_lgl(.orig_tokens, ~ "Black or African American" %in% .x)),
    race_latinx_b     = as.integer(map_lgl(.orig_tokens, ~ "Hispanic or Latino" %in% .x)),
    race_asian_b      = as.integer(map_lgl(.orig_tokens, ~ "Asian" %in% .x)),
    race_indigenous_b = as.integer(map_lgl(.orig_tokens, ~ "American Indian or Alaska Native" %in% .x)),
    race_nhpi_b       = as.integer(map_lgl(.orig_tokens, ~ "Native Hawaiian or Pacific Islander" %in% .x)),
    race_meha_b       = as.integer(map_lgl(.orig_tokens, ~ "Middle Eastern or North African" %in% .x)),
    race_another_b    = as.integer(map_lgl(.orig_tokens, ~ "Another race" %in% .x)),
    race_na           = as.integer(map_lgl(.orig_tokens, ~ "Choose not to answer" %in% .x)),
    
    # -------------------------------------------------------------------------
    # TASK 1: Cleaned & grouped multi-select string column ($q_demographic_race)
    # -------------------------------------------------------------------------
    .grouped_tokens = map(.orig_tokens, f_group_historical_omb),
    q_demographic_race = map_chr(.grouped_tokens, ~ paste(.x, collapse = ", ")),
    
    # -------------------------------------------------------------------------
    # TASK 3: Four mutually exclusive weighting schemes
    # ("Another race" and "Choose not to answer" remain distinct)
    # -------------------------------------------------------------------------
    # 1) OMB Bulletin 00-02 Allocation (prioritize non-white over white)
    q_demographic_race1_minority = map_chr(.grouped_tokens, function(t) {
      if (length(t) == 1 && t %in% c("Another race", "Choose not to answer")) return(t)
      
      non_white <- setdiff(t, "White")
      if (length(non_white) == 0) return("White")
      if (length(non_white) == 1) return(non_white)
      return("Multiracial")
    }),
    
    # 2) Indigenous First (AIAN) + OMB Bulletin 00-02
    q_demographic_race2_indig = map_chr(.grouped_tokens, function(t) {
      if (length(t) == 1 && t %in% c("Another race", "Choose not to answer")) return(t)
      if ("American Indian or Alaska Native" %in% t) return("American Indian or Alaska Native")
      
      non_white <- setdiff(t, "White")
      if (length(non_white) == 0) return("White")
      if (length(non_white) == 1) return(non_white)
      return("Multiracial")
    }),
    
    # 3) OMB 2024 Guidelines (co-equal multi-race)
    q_demographic_race3_equal = map_chr(.grouped_tokens, function(t) {
      if (length(t) == 1) return(t)
      return("Multiracial")
    }),
    
    # 4) Standard U.S. Census / Polling Benchmark (Hispanic prioritized)
    q_demographic_race4_hispanic = map_chr(.grouped_tokens, function(t) {
      if (length(t) == 1) return(t)
      if ("Hispanic or Latino" %in% t) return("Hispanic or Latino")
      return("Multiracial")
    })
  ) %>%
  select(-.orig_tokens, -.grouped_tokens)



# save -------------------------------------
write_csv(d2,"./results/data_long2.csv")
