# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: weighting responses

# raking ~ Iterative Proportional Fitting (W. Edwards Deming and Frederick Stephan 1940)

# Stratum Notes -----
# Combining Inland - North Central (n = 26), Inland - Northern (n = 6), and Inland - Central (n = 58)
# combining four non-focal coastal counties, but leaving OC separate. Keeping Del Norte separate for now, but small sample so could combine with humboldt. ok for state data since small population.

#  adds new columns------
# weight_stratum: The geographic unit inside which each respondent's demographics were calibrated to match Census ACS targets. Contains 15 levels: 10 focal counties, plus Orange County; non-focal coastal county region (SLO, San Mateo, Marin, Mendocino); 3 inland CDFW regions (Bay Delta, Inland Deserts, Central & Northern)

# Because Census ACS benchmarks only publish binary sex and do not match survey non-response categories, store the cleaned versions of  demographic variables used inside rake(). 
# gender_rake: Respondents in "Non-binary / Missing" were held at a neutral gender adjustment (1.0) during raking so they remain in dataset without distorting binary Census benchmarks.
# race_rake: Derived from AIAN-first + OMB Bulletin 00-02 variable (q_demographic_race2_indig). # Contains "White", "Hispanic or Latino", "Asian American/Pacific Islander", "Black or African American", "American Indian or Alaska Native", "Multiracial / Other" (combining "Multiracial", "Two or more races", and "Another race"), and "Missing" ("Choose not to answer" or NA, held neutral during raking).

# Two distinct weight columns because purposive oversampling gave  10 focal coastal counties far more respondents per capita than inland counties.

# weight_within_stratum: Relative Within-County/Region Weight. Scale: Centered around a mean of 1.0 inside every single weight_stratum (meaning the sum of weight_within_stratum in Los Angeles equals $n = 871$, in San Diego equals $n = 502$, etc.). What the numbers mean (examples): # 0.440 (Row 1): This respondent belongs to an over-sampled age/gender/race group in Los Angeles, so they count as 0.44 of an average LA respondent. # 1.272 (Row 5): This respondent belongs to an under-sampled group in Los Angeles, so they count as 1.27 of an average LA respondent. Use when: Comparing counties or regions against each other (e.g., "How do ocean access barriers in Humboldt compare to Los Angeles?"). Running regressions with county/stratum fixed effects, where each county should contribute proportionally to its sample size rather than letting Los Angeles overwhelm smaller coastal counties.


# weight_statewide: Population-Scaled Statewide Weight. sums to the total adult population of california. Scale: Expands each respondent to the actual number of California adults (18+) they represent in the 2023 ACS, so the sum of weight_statewide across the entire dataset equals California's total adult population (~30.6 million).What the numbers mean: 3897.4 (Row 1): After demographic adjustment, this respondent represents 3,897 adults living in Los Angeles County. 11257.4 (Row 5): This respondent represents 11,257 adults living in Los Angeles County. Note that respondents in inland regions will have larger weight_statewide values (e.g., 25,000–60,000) because fewer respondents were sampled per resident in inland California than in coastal counties. Use when: Reporting statewide California totals or percentages (e.g., "X% of all California adults visited the coast in the past year"). Comparing Coastal California vs. Inland California as a whole.

# for coastal and focal area subsets-------
# weight_coastal_pop = weight_statewide for coastal_strata. sums to the total adult population of the 15 coastal counties
# weight_focal_pop = weight statewide for focal_strata. sums to the total adult population of the 10 focal counties

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
# census_api_key("SEE EMAIL CMSF FOR CODE", install = TRUE,overwrite=TRUE)
# # Reload  environment without restarting R
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
# 2) Geographic Weighting Strata (weight_stratum)Group  California respondents into 17 mutually exclusive geographic strata:The 10 Focal Coastal Counties (10 individual strata): Keep San Diego, Los Angeles, Ventura, Santa Barbara, Monterey, Santa Cruz, San Francisco, Sonoma, Humboldt, and Del Norte as standalone county strata since they have large sample sizes.Non-Focal Coastal Counties (1–2 strata): Combine the remaining 5 coastal counties (Orange, San Luis Obispo, San Mateo, Marin, Mendocino) either into a single "Other Coastal" stratum or split by North/Central vs. South Coast depending on sample count ($n 75-100 per stratum).Inland Counties by CDFW Region (5 strata): Group inland respondents (county_coastal == "Inland") into their CDFW management regions (Bay Delta, Central, Inland Deserts, North Central, Northern). If any inland CDFW region has fewer than ~75–100 respondents, merge it with an adjacent inland region before raking.
# 3) Handling Non-Standard Census CategoriesCensus ACS marginals only provide binary sex (Male / Female) and lack "Choose not to answer" or "Another race" targets that match survey non-response:Gender (Transgender or Non-binary & NA): Do not drop these respondents. Assign them a neutral baseline weight of 1.0 on the gender margin (so they retain their full geographic and age/race weights without distorting binary Census targets), or randomly impute binary raking targets proportionally while preserving q_demographic_gender for subgroup analysis.Race (Choose not to answer & Another race): Leave "Choose not to answer" unweighted on the race margin (effective target = sample share, so raking does not force artificial inflation) and collapse "Another race" with "Multiracial" if  Census benchmark lacks an "Other" cell.Benchmark Alignment: Because q_demographic_race2_indig uses an AIAN-first + Minority-over-White allocation, pull  ACS targets from ACS 5-Year PUMS microdata (using the tidycensus package) and apply the exact same AIAN-first recoding function to the PUMS person records so  sample and target definitions match 100%.
# --------------------------------

# -------------------------------------------------------------------------
# STEP 1: Organize & Standardize Survey Data (d1 -> d2)
# -------------------------------------------------------------------------
survey_year <- 2025

other_coastal_central_north <- c("San Luis Obispo", "San Mateo", "Marin", "Mendocino")

d2 <- d1 %>%
  mutate(
    # Calculate age and adult age brackets (keep Missing as held-constant category)
    age = survey_year - as.numeric(QDemographic_Birth),
    age_cat = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65             ~ "65+",
      TRUE                  ~ "Missing"
    ),
    
    # Clean county name
    county_clean = str_remove(str_to_title(str_trim(Primary_County)), " County$"),
    
    # Define 15 hybrid geographic strata
    weight_stratum = case_when(
      county_focal_b == 1 | county_clean == "Orange" ~ county_clean,
      county_clean %in% other_coastal_central_north ~ "Other Coastal - Central & North",
      county_coastal == "Inland" & county_cdfw_region %in% c("Central", "North Central", "Northern") ~ "Inland - Central & Northern",
      county_coastal == "Inland" & !is.na(county_cdfw_region) & county_cdfw_region != "Unknown/Other" ~ paste0("Inland - ", county_cdfw_region),
      TRUE ~ "Out of State / Unknown"
    ),
    
    # Clean Gender for Raking (hold Transgender/Non-binary & NA neutral at weight = 1.0)
    gender_rake = case_when(
      q_demographic_gender %in% c("Male", "Female") ~ q_demographic_gender,
      TRUE ~ "Non-binary / Missing"
    ),
    
    # Clean Race (map MENA -> White as safety net; hold Choose not to answer neutral)
    race_rake = case_when(
      is.na(q_demographic_race2_indig) | q_demographic_race2_indig == "Choose not to answer" ~ "Missing",
      q_demographic_race2_indig == "Middle Eastern or North African" ~ "White",
      q_demographic_race2_indig %in% c("Multiracial", "Two or more races", "Another race") ~ "Multiracial / Other",
      TRUE ~ q_demographic_race2_indig
    )
  ) %>%
  filter(weight_stratum != "Out of State / Unknown")

# Quick sample size check before raking (15 total strata)
d2 %>% count(weight_stratum)


# -------------------------------------------------------------------------
# STEP 2: Download ACS PUMS & Map PUMAs to  Exact Strata (ca_pums_with_strata)
# -------------------------------------------------------------------------
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

# Pull 2023 1-Year ACS California PUMS (uses 100% 2020 PUMAs)
ca_pums <- get_pums(
  variables = c("PUMA", "AGEP", "SEX", "HISP", "RACAIAN", "RACASN", "RACBLK", "RACNH", "RACPI", "RACWHT", "RACSOR"),
  state = "CA",
  survey = "acs1",
  year = 2023
) %>%
  filter(AGEP >= 18)

# Complete 58-county California strata lookup matching d2
ca_county_strata <- tibble(
  county_clean = c(
    # 11 Individual Coastal Counties (10 Focal + Orange)
    "San Diego", "Orange", "Los Angeles", "Ventura", "Santa Barbara", "Monterey",
    "Santa Cruz", "San Francisco", "Sonoma", "Humboldt", "Del Norte",
    # Other Coastal - Central & North (4 counties)
    "San Luis Obispo", "San Mateo", "Marin", "Mendocino",
    # Inland - Bay Delta (7 counties)
    "Alameda", "Contra Costa", "Napa", "Sacramento", "San Joaquin", "Solano", "Yolo",
    # Inland - Inland Deserts (5 counties)
    "Imperial", "Inyo", "Mono", "Riverside", "San Bernardino",
    # Combined Inland - Central & Northern (30 counties)
    "Calaveras", "Fresno", "Kern", "Kings", "Madera", "Mariposa", "Merced",
    "San Benito", "Stanislaus", "Tulare", "Tuolumne",
    "Alpine", "Amador", "Butte", "Colusa", "El Dorado", "Glenn", "Lake",
    "Nevada", "Placer", "Plumas", "Sierra", "Sutter", "Yuba",
    "Lassen", "Modoc", "Shasta", "Siskiyou", "Tehama", "Trinity"
  ),
  weight_stratum = c(
    "San Diego", "Orange", "Los Angeles", "Ventura", "Santa Barbara", "Monterey",
    "Santa Cruz", "San Francisco", "Sonoma", "Humboldt", "Del Norte",
    rep("Other Coastal - Central & North", 4),
    rep("Inland - Bay Delta", 7),
    rep("Inland - Inland Deserts", 5),
    rep("Inland - Central & Northern", 30)
  )
)

ca_pums_with_strata <- ca_pums %>%
  mutate(
    PUMA = str_pad(as.character(PUMA), width = 5, pad = "0"),
    HISP = str_pad(as.character(HISP), width = 2, pad = "0")
  ) %>%
  inner_join(puma_xwalk, by = "PUMA", relationship = "many-to-many") %>%
  left_join(ca_county_strata, by = "county_clean") %>%
  mutate(
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


# -------------------------------------------------------------------------
# STEP 3: Run Within-Stratum Raking & Trim Extreme Weights (d2 -> d3_weighted)
# -------------------------------------------------------------------------
f_rake_by_stratum <- function(sample_df, target_pums_df, max_weight = 5.0) {
  
  strata_list <- unique(sample_df$weight_stratum)
  weighted_list <- list()
  
  for (st in strata_list) {
    sub_samp <- sample_df %>% filter(weight_stratum == st)
    sub_pop  <- target_pums_df %>% filter(weight_stratum == st)
    n_samp   <- nrow(sub_samp)
    
    # Tighter weight cap (3.5) for small strata (e.g., Del Norte n = 37)
    cap <- if (n_samp < 50) 3.5 else max_weight
    
    # --- Safety Collapse 1: Zero-count age brackets in small strata ---
    zero_ages <- setdiff(unique(sub_pop$age_target), unique(sub_samp$age_cat))
    
    if (length(zero_ages) > 0) {
      message(sprintf(
        "Stratum '%s' has 0 sample cases in age bracket(s): %s. Collapsing adjacent brackets.",
        st, paste(zero_ages, collapse = ", ")
      ))
      
      collapse_age_fn <- function(x) {
        case_when(
          x %in% c("18-29", "30-44") & ("18-29" %in% zero_ages | "30-44" %in% zero_ages) ~ "18-44",
          x %in% c("45-64", "65+")   & ("45-64" %in% zero_ages | "65+" %in% zero_ages)   ~ "45+",
          TRUE ~ x
        )
      }
      
      sub_samp <- sub_samp %>% mutate(age_cat_adj = collapse_age_fn(age_cat))
      sub_pop  <- sub_pop  %>% mutate(age_target_adj = collapse_age_fn(age_target))
    } else {
      sub_samp <- sub_samp %>% mutate(age_cat_adj = age_cat)
      sub_pop  <- sub_pop  %>% mutate(age_target_adj = age_target)
    }
    
    # --- Safety Collapse 2: Race categories with < 3 sample respondents ---
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
    
# --- 1. Build Marginal Target Tables (using PWGTP_adj across all 3 margins) ---
    # Age (hold "Missing" at exact sample count so weight = 1.0)
    n_age_miss  <- sum(sub_samp$age_cat_adj == "Missing")
    n_age_valid <- n_samp - n_age_miss
    
    pop_age <- sub_pop %>%
      count(age_cat_adj = age_target_adj, wt = PWGTP_adj) %>%
      mutate(Freq = n / sum(n) * n_age_valid) %>%
      select(age_cat_adj, Freq)
    
    if (n_age_miss > 0) {
      pop_age <- bind_rows(pop_age, tibble(age_cat_adj = "Missing", Freq = n_age_miss))
    }
    
    # Gender (hold "Non-binary / Missing" at exact sample count so weight = 1.0)
    n_nonbinary <- sum(sub_samp$gender_rake == "Non-binary / Missing")
    n_binary    <- n_samp - n_nonbinary
    
    pop_gender <- sub_pop %>%
      count(gender_rake = gender_target, wt = PWGTP_adj) %>%
      mutate(Freq = n / sum(n) * n_binary) %>%
      select(gender_rake, Freq)
    
    if (n_nonbinary > 0) {
      pop_gender <- bind_rows(pop_gender, tibble(gender_rake = "Non-binary / Missing", Freq = n_nonbinary))
    }
    
    # Race (hold "Missing" at exact sample count so weight = 1.0)
    n_race_miss  <- sum(sub_samp$race_rake_adj == "Missing")
    n_race_valid <- n_samp - n_race_miss
    
    pop_race <- sub_pop %>%
      count(race_rake_adj = race_target_adj, wt = PWGTP_adj) %>%
      mutate(Freq = n / sum(n) * n_race_valid) %>%
      select(race_rake_adj, Freq)
    
    if (n_race_miss > 0) {
      pop_race <- bind_rows(pop_race, tibble(race_rake_adj = "Missing", Freq = n_race_miss))
    }
    
    # --- 2. Initialize Design & Rake ---
    des <- svydesign(ids = ~1, weights = ~1, data = sub_samp)
    
    des_raked <- rake(
      design = des,
      sample.margins = list(~age_cat_adj, ~gender_rake, ~race_rake_adj),
      population.margins = list(pop_age, pop_gender, pop_race),
      control = list(maxit = 100, epsilon = 1e-5)
    )
    
    # --- 3. Trim Extreme Weights ---
    des_trimmed <- trimWeights(
      des_raked,
      lower = 0.2,
      upper = cap,
      strict = (n_samp >= 50)
    )
    
    # --- 4. Attach Final Weights ---
    stratum_pop_total <- sum(sub_pop$PWGTP_adj)
    raw_w <- weights(des_trimmed)
    
    sub_samp$weight_within_stratum <- raw_w / mean(raw_w)
    sub_samp$weight_statewide      <- (raw_w / sum(raw_w)) * stratum_pop_total
    
    weighted_list[[st]] <- sub_samp %>% select(-age_cat_adj, -race_rake_adj)
  }
  
  bind_rows(weighted_list)
}

# Run weighting to produce final dataset
d3_weighted <- f_rake_by_stratum(d2, ca_pums_with_strata)


# -------------------------------------------------------------------------
# STEP 4: Verification & Diagnostics
# -------------------------------------------------------------------------
# 1. Confirm zero rows were lost during raking
stopifnot(nrow(d3_weighted) == nrow(d2))

# 2. Check weight spread, Design Effect (DEFF), and Effective Sample Size across all 15 strata
d3_weighted %>%
  group_by(weight_stratum) %>%
  summarise(
    n     = n(),
    min_w = round(min(weight_within_stratum), 2),
    max_w = round(max(weight_within_stratum), 2),
    deff  = round(1 + var(weight_within_stratum) / (mean(weight_within_stratum)^2), 2),
    eff_n = round(n / deff, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(deff))

# 3. Spot-check weighted sample vs. Census PUMS target in Los Angeles
d3_weighted %>%
  filter(weight_stratum == "Los Angeles", race_rake != "Missing") %>%
  count(race_rake, wt = weight_within_stratum) %>%
  mutate(sample_weighted_pct = round(n / sum(n) * 100, 1)) %>%
  left_join(
    ca_pums_with_strata %>%
      filter(weight_stratum == "Los Angeles") %>%
      count(race_rake = race_target, wt = PWGTP_adj) %>%
      mutate(census_target_pct = round(n / sum(n) * 100, 1)) %>%
      select(race_rake, census_target_pct),
    by = "race_rake"
  )

# Check LA and Ventura caps
d3_weighted %>%
  filter(weight_stratum %in% c("Los Angeles", "Ventura"), weight_within_stratum >= 4.9) %>%
  count(weight_stratum, age_cat, gender_rake, race_rake)
# -------------------------------------------------------------------------

# check
glimpse(d3_weighted)

# ---------------------------------------------------------------------------
# STEP 5: Create Dedicated Coastal & Focal County Datasets
# ---------------------------------------------------------------------------
# Define the exact strata belonging to each analytical scope
focal_strata <- c(
  "San Diego", "Los Angeles", "Ventura", "Santa Barbara", "Monterey",
  "Santa Cruz", "San Francisco", "Sonoma", "Humboldt", "Del Norte"
)

coastal_strata <- c(
  focal_strata,
  "Orange",
  "Other Coastal - Central & North"
)

# 1. Coastal Counties Dataset (All 15 coastal counties across 12 strata)
d3_coastal <- d3_weighted %>%
  filter(weight_stratum %in% coastal_strata) %>%
  rename(weight_coastal_pop = weight_statewide)

# 2. Focal Counties Dataset (Only the 10 purposively sampled focal counties)
d3_focal <- d3_weighted %>%
  filter(weight_stratum %in% focal_strata) %>%
  rename(weight_focal_pop = weight_statewide)

# --- Verification & Summary Check ---
message(sprintf(
  "Dataset sizes -> Statewide: n = %d | Coastal: n = %d | Focal (10 counties): n = %d",
  nrow(d3_weighted), nrow(d3_coastal), nrow(d3_focal)
))

# Confirm each subset's population weight sums to its exact Census adult population
tibble(
  dataset = c("Statewide (All CA)", "Coastal Counties (15)", "Focal Counties (10)"),
  n_respondents = c(nrow(d3_weighted), nrow(d3_coastal), nrow(d3_focal)),
  n_strata = c(
    n_distinct(d3_weighted$weight_stratum),
    n_distinct(d3_coastal$weight_stratum),
    n_distinct(d3_focal$weight_stratum)
  ),
  represented_adult_pop = round(c(
    sum(d3_weighted$weight_statewide),
    sum(d3_coastal$weight_coastal_pop),
    sum(d3_focal$weight_focal_pop)
  ))
)

# -------------------------------------------------------------------------
# save
write_csv(d3_weighted,"./results/data_final_weighted.csv")
write_csv(d3_coastal,"./results/data_final_weighted_coastal.csv")
write_csv(d3_focal,"./results/data_final_weighted_focal.csv")