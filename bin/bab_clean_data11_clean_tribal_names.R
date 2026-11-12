# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: clean tribal names

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
d1 <- read_csv("./results/data_long10.csv") %>%
  glimpse()

# copy text
writeClipboard(d1$QDemographic_Tribal)
writeClipboard(d1$QDemographic_Tribal_TEXT)

# check out responses
unique(d1$QDemographic_Tribal)
unique(na.omit(d1$QDemographic_Tribal_TEXT))

# 2. Clean and standardize to primary tribal/nation groupings
library(dplyr)
library(stringr)

# Clean, categorize, and flag tribal affiliations in a single pipeline
d2 <- d1 %>%
  mutate(
    # Normalize whitespace and curly apostrophes
    .raw = str_replace_all(str_squish(QDemographic_Tribal_TEXT), "[’‘]", "'"),

    # 1. Standardized single-label identification
    tribe_clean = case_when(
      is.na(.raw) | .raw %in% c("No", "Sentinal") ~ NA_character_,
      
      # Combined entries
      str_detect(.raw, regex("Tolowa and Yurok", ignore_case = TRUE)) ~ "Tolowa / Yurok",
      str_detect(.raw, regex("Tongva.*Chumash|Gabrielino.*Chumash", ignore_case = TRUE)) ~ "Tongva / Chumash",
      str_detect(.raw, regex("tongva-gabrielino/yaqui", ignore_case = TRUE)) ~ "Tongva-Gabrielino / Yaqui",
      
      # California tribes / bands
      str_detect(.raw, regex("Tolowa", ignore_case = TRUE)) ~ "Tolowa Dee-ni' Nation",
      str_detect(.raw, regex("Coastal Band of Chumash|^CBCN$", ignore_case = TRUE)) ~ "Coastal Band of Chumash",
      str_detect(.raw, regex("Santa Ynez", ignore_case = TRUE)) ~ "Santa Ynez Band of Chumash",
      str_detect(.raw, regex("Chumash", ignore_case = TRUE)) ~ "Chumash (Unspecified)",
      str_detect(.raw, regex("La Jolla", ignore_case = TRUE)) ~ "La Jolla Band of Luiseño Indians",
      str_detect(.raw, regex("Pala Band", ignore_case = TRUE)) ~ "Pala Band of Mission Indians",
      str_detect(.raw, regex("Pechanga", ignore_case = TRUE)) ~ "Pechanga Band of Indians",
      str_detect(.raw, regex("Luiseno", ignore_case = TRUE)) ~ "Luiseño (Unspecified)",
      str_detect(.raw, regex("iipay", ignore_case = TRUE)) ~ "Iipay Nation of Santa Ysabel",
      str_detect(.raw, regex("San Pasqual", ignore_case = TRUE)) ~ "San Pasqual Band of Mission Indians",
      str_detect(.raw, regex("Kashia", ignore_case = TRUE)) ~ "Kashia Band of Pomo Indians",
      str_detect(.raw, regex("Sherwood Valley", ignore_case = TRUE)) ~ "Sherwood Valley Band of Pomo",
      str_detect(.raw, regex("los coyotes", ignore_case = TRUE)) ~ "Los Coyotes Band of Cahuilla and Cupeño Indians",
      str_detect(.raw, regex("Amah Mutsun", ignore_case = TRUE)) ~ "Amah Mutsun Tribal Band",
      str_detect(.raw, regex("Pulikla", ignore_case = TRUE)) ~ "Pulikla Tribe of Yurok People",
      str_detect(.raw, regex("Round valley", ignore_case = TRUE)) ~ "Round Valley Indian Tribes",
      
      # Non-California tribes
      str_detect(.raw, regex("Sonora Yaqui", ignore_case = TRUE)) ~ "Pascua Yaqui / Sonora Yaqui",
      str_detect(.raw, regex("Oglala", ignore_case = TRUE)) ~ "Oglala Lakota (Pine Ridge)",
      str_detect(.raw, regex("Lakota", ignore_case = TRUE)) ~ "Lakota",
      str_detect(.raw, regex("Maya mam", ignore_case = TRUE)) ~ "Maya Mam",
      str_detect(.raw, regex("^Apache$", ignore_case = TRUE)) ~ "Apache",
      str_detect(.raw, regex("^Choctaw$", ignore_case = TRUE)) ~ "Choctaw",
      str_detect(.raw, regex("^Seminole$", ignore_case = TRUE)) ~ "Seminole",
      str_detect(.raw, regex("^Potawatomi$", ignore_case = TRUE)) ~ "Potawatomi",
      str_detect(.raw, regex("^Tewa$", ignore_case = TRUE)) ~ "Tewa",
      str_detect(.raw, regex("^Seneca-Cayuga$", ignore_case = TRUE)) ~ "Seneca-Cayuga Nation",
      str_detect(.raw, regex("^Algonquin$", ignore_case = TRUE)) ~ "Algonquin",
      TRUE ~ .raw
    ),

    # 2. Comma-delimited California tribe names
    tribe_ca = case_when(
      is.na(.raw) | .raw %in% c("No", "Sentinal") ~ NA_character_,
      
      # Multi-tribe entries containing CA nations
      str_detect(.raw, regex("Tolowa and Yurok", ignore_case = TRUE)) ~ "Tolowa Dee-ni' Nation, Yurok Tribe",
      str_detect(.raw, regex("Tongva.*Chumash|Gabrielino.*Chumash", ignore_case = TRUE)) ~ "Gabrielino/Tongva, Chumash",
      str_detect(.raw, regex("tongva-gabrielino/yaqui", ignore_case = TRUE)) ~ "Gabrielino/Tongva",
      
      # Single CA nations
      str_detect(.raw, regex("Tolowa", ignore_case = TRUE)) ~ "Tolowa Dee-ni' Nation",
      str_detect(.raw, regex("Coastal Band of Chumash|^CBCN$", ignore_case = TRUE)) ~ "Coastal Band of the Chumash Nation",
      str_detect(.raw, regex("Santa Ynez", ignore_case = TRUE)) ~ "Santa Ynez Band of Chumash",
      str_detect(.raw, regex("Chumash", ignore_case = TRUE)) ~ "Chumash",
      str_detect(.raw, regex("La Jolla", ignore_case = TRUE)) ~ "La Jolla Band of Luiseño Indians",
      str_detect(.raw, regex("Pala Band", ignore_case = TRUE)) ~ "Pala Band of Mission Indians",
      str_detect(.raw, regex("Pechanga", ignore_case = TRUE)) ~ "Pechanga Band of Indians",
      str_detect(.raw, regex("Luiseno", ignore_case = TRUE)) ~ "Luiseño",
      str_detect(.raw, regex("iipay", ignore_case = TRUE)) ~ "Iipay Nation of Santa Ysabel",
      str_detect(.raw, regex("San Pasqual", ignore_case = TRUE)) ~ "San Pasqual Band of Mission Indians",
      str_detect(.raw, regex("Kashia", ignore_case = TRUE)) ~ "Kashia Band of Pomo Indians",
      str_detect(.raw, regex("Sherwood Valley", ignore_case = TRUE)) ~ "Sherwood Valley Band of Pomo",
      str_detect(.raw, regex("los coyotes", ignore_case = TRUE)) ~ "Los Coyotes Band of Cahuilla and Cupeño Indians",
      str_detect(.raw, regex("Amah Mutsun", ignore_case = TRUE)) ~ "Amah Mutsun Tribal Band",
      str_detect(.raw, regex("Pulikla", ignore_case = TRUE)) ~ "Pulikla Tribe of Yurok People",
      str_detect(.raw, regex("Round valley", ignore_case = TRUE)) ~ "Round Valley Indian Tribes",
      TRUE ~ NA_character_
    ),

    # 3. Boolean California indicator: TRUE if CA, FALSE if non-CA, NA if blank/invalid
    tribe_ca_b1 = case_when(
      is.na(tribe_clean) ~ NA,
      !is.na(tribe_ca)   ~ TRUE,
      TRUE                ~ FALSE
    )
  ) %>%
  mutate(tribe_ca_b=if_else(tribe_ca_b1 ==TRUE,"California Tribe",as.character(tribe_ca_b1)))%>%
  select(-.raw)%>%
  glimpse()

# Verify summary table
table(d2$tribe_clean, useNA = "ifany")

# Check multi-tribe and single entries in the new data frame
d2 %>%
  filter(!is.na(tribe_ca)) %>%
  select(QDemographic_Tribal_TEXT, tribe_ca,tribe_ca_b) %>%
  distinct()%>%
  print(n=30)

# save
write_csv(d2,"./results/data_long11.csv")
