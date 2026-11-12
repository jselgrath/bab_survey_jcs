# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: weighting responses
# Stratum 'Inland - North Central' has n = 26 (< 30). 
# Stratum 'Inland - Northern' has n = 6 (< 30). 

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(dplyr)
library(stringr)
library(tidycensus)
library(survey)
library(readr)

# --------------------------------------------------------------------------
# load data ----
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# ceusus key ------------
# make sure census key is installed # # Run this once 
# census_api_key("SEE EMAIL CMSF", install = TRUE,overwrite=TRUE)
# # Reload your environment without restarting R
# readRenviron("~/.Renviron")



# update this input to be the most cleaned version 
d1 <- read_csv("./results/data_final.csv")%>%
  glimpse()

# unique(d1$q_demographic_race2_indig)
# writeClipboard(d1$q_demographic_race)
# N=length(d1$q_demographic_race)
# 
# check<-d1%>%
#   group_by(q_demographic_race)%>%
#   summarize(
#     n=n(),
#     pct=round(n/N,4))%>%
#   glimpse()
#   
# check%>%
#   print(n=50)


# writeClipboard(check)


# prompt------------------------
# I have ocean access survey data with thousands of samples (Data = d1). We focused on coastal counties (specific countyies $county_focal_b=1), but also have respondents from other coastal counties and inland counties. In the 10 coastal counties where we focused  ($county_focal_b=1) we have a much higher sample of the population. those are: san diego, LA, Ventura, Santa Barbara, Monterey, Santa Cruz, San Francisco, Sonoma, Humboldt, and Del Norte. 

#We did puprosive sampling and worked to balance samples by race so that the race of respondents roughly matched that of the county, but because of online respondents and chance its not perfectly matched - which is to be expected. I'd like to design a method for weighting samples within counties by age, race, and gender so that the sample of the counties matches is weighted to match the population. Note that the race variable people can select more than one race. relevant variables: $q_demographic_race2_indig (levels: "Choose not to answer","White","Asian American/Pacific Islander"  "Hispanic or Latino","Black or African American","Multiracial", "American Indian or Alaska Native", "Another race", NA ) (version of race variable which "AIAN / Indigenous-First" Priority Hierarchy, with minority over white rule for the remaining race classes based on OMB Bulletin 00-02 Allocation), q_demographic_gender (levels: NA, "Male", "Female","Transgender or Non-binary"), QDemographic_Birth (year of birth) and Primary_County (county is based on zip code: QDemographic_PrimaryZip).    

#other respondents live in areas with smaller sample sizes because we did not target them: a) the five other (non-sampled) coastal counties or b) inland counties (relvant variable $county_coastal (levels: NA,"Coastal", "Inland"). I'd like to include them but its less clear how to weight these. Because there is so much geographic and cultural variability across the state, one idea is to group them by CDFW regions (variable = $county_cdfw_region (levels="Unknown/Other","South Coast","Inland Deserts", "Central", "Bay Delta", "North Central","Northern",NA)

# What do you suggest and how would you code it in R. 


# -----------------
# recommendation:
# 1) Weighting Design & Geographic StratificationTo balance within-county demographic precision against small sample sizes in non-targeted areas, use a Hybrid Geographic Stratum + Iterative Proportional Fitting (Raking) design (anesrake or survey::calibrate()).
# 2) Geographic Weighting Strata (weight_stratum)Group your California respondents into 17 mutually exclusive geographic strata:The 10 Focal Coastal Counties (10 individual strata): Keep San Diego, Los Angeles, Ventura, Santa Barbara, Monterey, Santa Cruz, San Francisco, Sonoma, Humboldt, and Del Norte as standalone county strata since they have large sample sizes.Non-Focal Coastal Counties (1–2 strata): Combine the remaining 5 coastal counties (Orange, San Luis Obispo, San Mateo, Marin, Mendocino) either into a single "Other Coastal" stratum or split by North/Central vs. South Coast depending on sample count ($n 75-100 per stratum).Inland Counties by CDFW Region (5 strata): Group inland respondents (county_coastal == "Inland") into their CDFW management regions (Bay Delta, Central, Inland Deserts, North Central, Northern). If any inland CDFW region has fewer than ~75–100 respondents, merge it with an adjacent inland region before raking.
# 3) Handling Non-Standard Census CategoriesCensus ACS marginals only provide binary sex (Male / Female) and lack "Choose not to answer" or "Another race" targets that match survey non-response:Gender (Transgender or Non-binary & NA): Do not drop these respondents. Assign them a neutral baseline weight of 1.0 on the gender margin (so they retain their full geographic and age/race weights without distorting binary Census targets), or randomly impute binary raking targets proportionally while preserving q_demographic_gender for subgroup analysis.Race (Choose not to answer & Another race): Leave "Choose not to answer" unweighted on the race margin (effective target = sample share, so raking does not force artificial inflation) and collapse "Another race" with "Multiracial" if your Census benchmark lacks an "Other" cell.Benchmark Alignment: Because q_demographic_race2_indig uses an AIAN-first + Minority-over-White allocation, pull your ACS targets from ACS 5-Year PUMS microdata (using the tidycensus package) and apply the exact same AIAN-first recoding function to the PUMS person records so your sample and target definitions match 100%.
# --------------------------------


# ---------------------------------------------------------------------------
# STEP 1: Organize & Standardize Survey Data (d1 -> d2)
# ---------------------------------------------------------------------------
survey_year <- 2025

other_coastal_south <- c("Orange", "San Luis Obispo")
other_coastal_north <- c("San Mateo", "Marin", "Mendocino")

d2 <- d1 %>%
  mutate(
    age = survey_year - as.numeric(QDemographic_Birth),
    age_cat = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65             ~ "65+",
      TRUE                  ~ "Missing"
    ),
    county_clean = str_remove(str_to_title(str_trim(Primary_County)), " County$"),
    
    weight_stratum = case_when(
      county_focal_b == 1 ~ county_clean,
      county_clean %in% other_coastal_south ~ "Other Coastal - South",
      county_clean %in% other_coastal_north ~ "Other Coastal - North",
      county_coastal == "Inland" & county_cdfw_region %in% c("North Central", "Northern") ~ "Inland - Northern & North Central",
      county_coastal == "Inland" & !is.na(county_cdfw_region) & county_cdfw_region != "Unknown/Other" ~ paste0("Inland - ", county_cdfw_region),
      TRUE ~ "Out of State / Unknown"
    ),
    
    gender_rake = case_when(
      q_demographic_gender %in% c("Male", "Female") ~ q_demographic_gender,
      TRUE ~ "Non-binary / Missing"
    ),
    race_rake = case_when(
      is.na(q_demographic_race2_indig) | q_demographic_race2_indig == "Choose not to answer" ~ "Missing",
      q_demographic_race2_indig %in% c("Multiracial", "Two or more races", "Another race") ~ "Multiracial / Other",
      TRUE ~ q_demographic_race2_indig
    )
  ) %>%
  filter(weight_stratum != "Out of State / Unknown", age_cat != "Missing")

# Quick sample size check before raking
d2 %>% count(weight_stratum)




# ---------------------------------------------------------------------------
# STEP 2: Download ACS PUMS & Map PUMAs to Your Exact Strata (ca_pums_with_strata)
# ---------------------------------------------------------------------------
# 2A. Download official Census 2020 Tract-to-PUMA relationship file & map to CA counties
xwalk_url <- "https://www2.census.gov/geo/docs/reference/puma2020/2020_Census_Tract_to_2020_PUMA.txt"

ca_counties <- tidycensus::fips_codes %>%
  filter(state == "CA") %>%
  transmute(
    COUNTYFP = county_code,
    county_clean = str_remove(county, " County$")
  )

puma_xwalk <- read_csv(xwalk_url, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  filter(STATEFP == "06") %>%
  mutate(PUMA = str_pad(PUMA5CE, width = 5, pad = "0")) %>%
  left_join(ca_counties, by = "COUNTYFP") %>%
  count(PUMA, county_clean, name = "n_tracts") %>%
  group_by(PUMA) %>%
  mutate(tract_share = n_tracts / sum(n_tracts)) %>%
  ungroup() %>%
  select(PUMA, county_clean, tract_share)


# 2B. Lookup tables for California counties -> Focal / Coastal / Inland CDFW Region
focal_counties <- c(
  "San Diego", "Los Angeles", "Ventura", "Santa Barbara", "Monterey",
  "Santa Cruz", "San Francisco", "Sonoma", "Humboldt", "Del Norte"
)

other_coastal_counties <- c(
  "Orange", "San Luis Obispo", "San Mateo", "Marin", "Mendocino"
)

cdfw_inland_lookup <- tibble(
  county_clean = c(
    "Alameda", "Contra Costa", "Napa", "Sacramento", "San Joaquin", "Solano", "Yolo",
    "Calaveras", "Fresno", "Kern", "Kings", "Madera", "Mariposa", "Merced",
    "San Benito", "Stanislaus", "Tulare", "Tuolumne",
    "Imperial", "Inyo", "Mono", "Riverside", "San Bernardino",
    "Alpine", "Amador", "Butte", "Colusa", "El Dorado", "Glenn", "Lake",
    "Nevada", "Placer", "Plumas", "Sierra", "Sutter", "Yuba",
    "Lassen", "Modoc", "Shasta", "Siskiyou", "Tehama", "Trinity"
  ),
  cdfw_region = c(
    rep("Bay Delta", 7),
    rep("Central", 11),
    rep("Inland Deserts", 5),
    rep("North Central", 13),
    rep("Northern", 6)
  )
)

# 2C. Pull 2023 1-Year ACS California PUMS (uses 100% 2020 PUMAs)
# Note: Make sure you have run census_api_key("YOUR_KEY", install = TRUE) once beforehand
ca_pums <- get_pums(
  variables = c("PUMA", "AGEP", "SEX", "HISP", "RACAIAN", "RACASN", "RACBLK", "RACNH", "RACPI", "RACWHT", "RACSOR"),
  state = "CA",
  survey = "acs1",
  year = 2023
) %>%
  filter(AGEP >= 18)

# ---------------------------------------------------------------------------
# 2D. Join crosswalk and classify PUMS targets to match d2
# ---------------------------------------------------------------------------
# Build county -> weight_stratum lookup directly from your d2 data where possible,
# supplemented by the complete 58-county California table so every county maps cleanly
# updated to combine north central and north due to small sample sizes 

ca_county_strata <- tibble(
  county_clean = c(
    # 10 Focal Coastal Counties
    "San Diego", "Los Angeles", "Ventura", "Santa Barbara", "Monterey",
    "Santa Cruz", "San Francisco", "Sonoma", "Humboldt", "Del Norte",
    # Other Coastal - South (2 counties)
    "Orange", "San Luis Obispo",
    # Other Coastal - North (3 counties)
    "San Mateo", "Marin", "Mendocino",
    # Inland - Bay Delta (7 counties)
    "Alameda", "Contra Costa", "Napa", "Sacramento", "San Joaquin", "Solano", "Yolo",
    # Inland - Central (11 counties)
    "Calaveras", "Fresno", "Kern", "Kings", "Madera", "Mariposa", "Merced",
    "San Benito", "Stanislaus", "Tulare", "Tuolumne",
    # Inland - Inland Deserts (5 counties)
    "Imperial", "Inyo", "Mono", "Riverside", "San Bernardino",
    # Combined Inland - Northern & North Central (19 counties)
    "Alpine", "Amador", "Butte", "Colusa", "El Dorado", "Glenn", "Lake",
    "Nevada", "Placer", "Plumas", "Sierra", "Sutter", "Yuba",
    "Lassen", "Modoc", "Shasta", "Siskiyou", "Tehama", "Trinity"
  ),
  weight_stratum = c(
    "San Diego", "Los Angeles", "Ventura", "Santa Barbara", "Monterey",
    "Santa Cruz", "San Francisco", "Sonoma", "Humboldt", "Del Norte",
    rep("Other Coastal - South", 2),
    rep("Other Coastal - North", 3),
    rep("Inland - Bay Delta", 7),
    rep("Inland - Central", 11),
    rep("Inland - Inland Deserts", 5),
    rep("Inland - Northern & North Central", 19)
  )
)

# check orange vs slo -------------
table(d2$county_clean[d2$weight_stratum == "Other Coastal - South"])

# -----------------------------
ca_pums_with_strata <- ca_pums %>%
  mutate(PUMA = str_pad(as.character(PUMA), width = 5, pad = "0")) %>%
  # Many-to-many join splits multi-county PUMAs across their constituent counties
  inner_join(puma_xwalk, by = "PUMA", relationship = "many-to-many") %>%
  left_join(ca_county_strata, by = "county_clean") %>%
  mutate(
    # Scale person weight by the county's share of the PUMA
    PWGTP_adj = PWGTP * tract_share,
    race_target = case_when(
      RACAIAN == "1" ~ "American Indian or Alaska Native",
      HISP != "01" & (RACBLK == "0" & RACASN == "0" & RACNH == "0" & RACPI == "0") ~ "Hispanic or Latino",
      RACBLK == "1" & (HISP == "01" & RACASN == "0" & RACNH == "0" & RACPI == "0") ~ "Black or African American",
      (RACASN == "1" | RACNH == "1" | RACPI == "1") & (HISP == "01" & RACBLK == "0") ~ "Asian American/Pacific Islander",
      RACWHT == "1" & HISP == "01" & RACBLK == "0" & RACASN == "0" & RACNH == "0" & RACPI == "0" & RACSOR == "0" ~ "White",
      TRUE ~ "Multiracial / Other"
    ),
    age_target = case_when(
      AGEP <= 29 ~ "18-29",
      AGEP <= 44 ~ "30-44",
      AGEP <= 64 ~ "45-64",
      TRUE       ~ "65+"
    ),
    gender_target = if_else(SEX == "1", "Male", "Female")
  )

# DIAGNOSTIC CHECK: Verify 100% of strata in d2 exist in ca_pums_with_strata
missing_strata <- setdiff(unique(d2$weight_stratum), unique(ca_pums_with_strata$weight_stratum))
if (length(missing_strata) > 0) {
  stop("These strata in d2 do not match ca_pums_with_strata: ", paste(missing_strata, collapse = ", "))
} else {
  message("All strata in d2 matched successfully!")
}

# ---------------------------------------------------------------------------
# STEP 3: Run Within-Stratum Raking & Trim Extreme Weights (d2 -> d3_weighted)
# ---------------------------------------------------------------------------
f_rake_by_stratum <- function(sample_df, target_pums_df, max_weight = 5.0) {
  
  strata_list <- unique(sample_df$weight_stratum)
  weighted_list <- list()
  
  for (st in strata_list) {
    sub_samp <- sample_df %>% filter(weight_stratum == st)
    sub_pop  <- target_pums_df %>% filter(weight_stratum == st)
    n_samp   <- nrow(sub_samp)
    
    # Safety check: If a stratum has very few respondents (< 30), assign neutral weights
    if (n_samp < 30) {
      message(sprintf("Stratum '%s' has n = %d (< 30). Assigning uniform within-stratum weight = 1.", st, n_samp))
      stratum_pop_total <- sum(sub_pop$PWGTP)
      sub_samp$weight_within_stratum <- 1.0
      sub_samp$weight_statewide      <- stratum_pop_total / n_samp
      weighted_list[[st]] <- sub_samp
      next
    }
    
    # --- Safety Collapse for Zero-Count Race Cells in Small Strata ---
    # survey::rake() fails if a target category has 0 respondents in the sample.
    # Any race category with < 3 sample respondents in this stratum is merged into "Multiracial / Other"
    small_races <- sub_samp %>%
      filter(race_rake != "Missing") %>%
      count(race_rake) %>%
      right_join(tibble(race_rake = unique(sub_pop$race_target)), by = "race_rake") %>%
      mutate(n = replace_na(n, 0)) %>%
      filter(n < 3, race_rake != "Multiracial / Other") %>%
      pull(race_rake)
    
    sub_samp <- sub_samp %>%
      mutate(race_rake_adj = if_else(race_rake %in% small_races, "Multiracial / Other", race_rake))
    
    sub_pop <- sub_pop %>%
      mutate(race_target_adj = if_else(race_target %in% small_races, "Multiracial / Other", race_target))
    
    # --- 1. Build Marginal Target Tables ---
    pop_age <- sub_pop %>%
      count(age_cat = age_target, wt = PWGTP_adj) %>%
      mutate(Freq = n / sum(n) * n_samp) %>%
      select(age_cat, Freq)
    
    # Gender: Hold "Non-binary / Missing" at exact sample count (weight = 1.0)
    n_nonbinary <- sum(sub_samp$gender_rake == "Non-binary / Missing")
    n_binary    <- n_samp - n_nonbinary
    
    pop_gender <- sub_pop %>%
      count(gender_rake = gender_target, wt = PWGTP) %>%
      mutate(Freq = n / sum(n) * n_binary) %>%
      select(gender_rake, Freq)
    
    if (n_nonbinary > 0) {
      pop_gender <- bind_rows(pop_gender, tibble(gender_rake = "Non-binary / Missing", Freq = n_nonbinary))
    }
    
    # Race: Hold "Missing" at exact sample count (weight = 1.0)
    n_race_miss  <- sum(sub_samp$race_rake_adj == "Missing")
    n_race_valid <- n_samp - n_race_miss
    
    pop_race <- sub_pop %>%
      count(race_rake_adj = race_target_adj, wt = PWGTP) %>%
      mutate(Freq = n / sum(n) * n_race_valid) %>%
      select(race_rake_adj, Freq)
    
    if (n_race_miss > 0) {
      pop_race <- bind_rows(pop_race, tibble(race_rake_adj = "Missing", Freq = n_race_miss))
    }
    
    # --- 2. Initialize Design & Rake ---
    des <- svydesign(ids = ~1, data = sub_samp)
    
    des_raked <- rake(
      design = des,
      sample.margins = list(~age_cat, ~gender_rake, ~race_rake_adj),
      population.margins = list(pop_age, pop_gender, pop_race),
      control = list(maxit = 100, epsilon = 1e-5)
    )
    
    # --- 3. Trim Extreme Weights ---
    des_trimmed <- trimWeights(des_raked, lower = 0.2, upper = max_weight, strict = TRUE)
    
    # --- 4. Attach Final Weights ---
    stratum_pop_total <- sum(sub_pop$PWGTP_adj)
    raw_w <- weights(des_trimmed)
    
    sub_samp$weight_within_stratum <- raw_w / mean(raw_w)
    sub_samp$weight_statewide      <- (raw_w / sum(raw_w)) * stratum_pop_total
    
    weighted_list[[st]] <- sub_samp %>% select(-race_rake_adj)
  }
  
  bind_rows(weighted_list)
}

# Run weighting to produce final dataset
d3_weighted <- f_rake_by_stratum(d2, ca_pums_with_strata)

d3_weighted %>%
  group_by(weight_stratum) %>%
  summarise(
    n = n(),
    min_w = min(weight_within_stratum),
    max_w = max(weight_within_stratum),
    deff = 1 + var(weight_within_stratum) / (mean(weight_within_stratum)^2),
    eff_n = round(n / deff, 1)
  )

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------