# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: organize zip codes into counties

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

# order of coastal counties
coastal_south2north <- c(
  "San Diego","Orange","Los Angeles","Ventura","Santa Barbara",
  "San Luis Obispo","Monterey","Santa Cruz","San Mateo","San Francisco",
  "Marin","Sonoma","Mendocino","Humboldt","Del Norte"
  
  
crosswalk<-read_csv("./data/hud_ZIP_COUNTY_062025.csv")%>%
  glimpse()

# -- filter for CA
ca_zip_county <- crosswalk %>%
  # Keep only California rows
  filter(usps_zip_pref_state == "CA") %>%
  # Clean up and keep the columns you care about
  transmute(
    zip        = str_pad(zip, 5, pad = "0"),  # ensure 5-char string
    county_fips = str_pad(county, 5, pad = "0"),
    county_name = county_name,
    # optional: keep the HUD ratio if you want to deal with multi-county ZIPs
    tot_ratio
  )

# Check the result
head(ca_zip_county)
nrow(ca_zip_county)  # number of ZIP–county pairs (will be > # of unique ZIPs)
length(unique(ca_zip_county$zip))  # number of unique California ZIP codes




# -- deal with zip spanning counties --------------------
ca_zip_to_one_county <- ca_zip_county %>%
  group_by(zip) %>%
  slice_max(order_by = tot_ratio, n = 1, with_ties = FALSE) %>%
  ungroup()

# One county per ZIP
head(ca_zip_to_one_county)
