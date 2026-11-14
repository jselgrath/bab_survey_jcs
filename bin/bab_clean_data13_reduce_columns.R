# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: remove redundant columns

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(lubridate)
library(utils)

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# read file
d1 <- read_csv("./results/data_long12.csv") %>%
  glimpse()

writeClipboard(names(d1))


# --------------------------------------------------------------------------
# 1. Define ordered column list (excluding anything starting with *)
# -------------------------------------------------------------------------
desired_cols <- c(
  "StartDate", "Progress", "Durationinseconds", "Finished", "response_id",
  "LocationLatitude", "LocationLongitude", "UserLanguage", "Consent",
  "QRepeat", "QDesired_Time", "QActual_Time", "QUse_Area", "QUse_Area_TEXT",
  "q_mapping_county", "map", "x_long", "y_lat", 
  "q_activities2", "q_activities_most2", "q_activity_all3",
  "q_activity_most3", "q_activities_text_2", "q_activities_most_text_2",
  "q_activity_restoration","q_activity_consumptive_most_b",  "QImportant_Activities", "QImportant_Activities_TEXT",   "QImportant_Activities_Most", 
  
  "QTransport_Mode", "QTransport_Time", "QActivity_Mentor",
  "QActivity_Mentor_TEXT", "QActivity_Companion", "QFishing", "QFishing_Type",
  "QFishing_Species_A", "QFishing_Species_B", "QFishing_Species_C", "QCatch_Type",
  "QOI_Species_A", "QOI_Species_B", "QOI_Species_C", "QE_TimeSpent",
  "QE_TimeSpent_TEXT", "QE_GroupSize", "QE_Activities", "QE_Entry",
  "QE_Parking", "QE_PublicT", "QE_CarRental", "QE_Gas", "QE_Restuarant",
  "QE_Grocery", "QE_Rental", "QE_Gear", "QE_Other", "QE_Overnight_Lodging",
  "QE_Overnight_Camping", "QE_Overnight_Transportation", "QMedia1", "QMedia2",
  "QWB_1", "QWB_2", "QWB_3", "QWB_4", "QWB_5", "QWB_6", "QWB_7", "QWB_8",
  "QWB_9", "QES_1", "QES_2", "QES_3", "QES_4", "QES_5", "QES_6", "QES_7",
  "QES_8", "QES_9", "QES_10", "QBarriers_1", "QBarriers_2", "QBarriers_3",
  "QBarriers_4", "QBarriers_5", "QBarriers_6", "QBarriers_7", "QBarriers_8",
  "QBarriers_9", "QBarriers_10", "QMicroA_1", "QMicroA_2", "QMicroA_3",
  "QMicroA_4", "QClimate_Obs_S_1", "QClimate_Obs_S_2", "QClimate_Obs_S_3",
  "QClimate_Obs_S_4", "QClimate_Conc_S_1", "QClimate_Conc_S_2",
  "QClimate_Conc_S_3", "QClimate_Conc_S_4", "QClimate_Obs_E_1",
  "QClimate_Obs_E_2", "QClimate_Obs_E_3", "QClimate_Obs_E_4",
  "QClimate_Obs_E_5", "QClimate_Obs_E_6", "QClimate_Obs_E_7",
  "QClimate_Conc_E_1", "QClimate_Conc_E_2", "QClimate_Conc_E_3",
  "QClimate_Conc_E_4", "QClimate_Conc_E_5", "QClimate_Conc_E_6",
  "QClimate_Conc_E_7", "QMPA_Aware", "QSanct_Aware", "QMPA_Purpose",
  "QMPA_Science", "QMPA_Visit", "QMPA_Eval1", "QMPA_Eval2", "QMPA_Eval3",
  "QMPA_Eval4", "QPriority_1", "QPriority_2", "QPriority_3", "QPriority_4",
  "QPriority_5", "QPriority_6", "QPriority_7", "QPriority_8", "QPriority_9",
  "QPriority_10", "QClimate_Belief", "QClimate_Attitude1", "QClimate_Attitude2",
  "QClimate_Attitude3", "QClimate_Attitude4", "QClimate_Attitude5", "QFutures1",
  "QFutures2", "QFutures3", "QFutures4", "QDemographic_Home",
  "QDemographic_PrimaryZip", "QDemographic_Country_State", "QDemographic_CA_Years",
  "QDemographic_Race_TEXT", "QDemographic_Asian", "QDemographic_Asian_TEXT",
  "QDemographic_Tribal", "QDemographic_Tribal_TEXT", "QDemographic_Birth",
  "QDemographic_Income", "QDemographic_Income_Collapsed", "QDemographic_Family",
  "q_demographic_race", 
  
"race_white_b", "race_black_b", "race_latinx_b", "race_asian_b", "race_indigenous_b", "race_nhpi_b", "race_meha_b", "race_another_b", "race_na", "q_demographic_race1_minority", "q_demographic_race2_indig", "q_demographic_race3_equal", "q_demographic_race4_hispanic", "q_demographic_gender", "tribe_clean", "tribe_ca", "tribe_ca_b1", "tribe_ca_b", "q_demographic_gender", "q_demographic_education",
  "QNumber_Children", "QDemographic_Swimming", "comment2", "comment_dog_b",
  "comment_dog_walking", "comment_dog_positive", "comment_activity", "Update",
  "Email", "Version", "Mechanism", "Phase", "Format", "State", "City",
  "Primary_County", "County_Name_1", "County_Name_2", "County_Name_3",
  "County_Name_4", "county_coastal",   "county_cdfw_region", "county_focal_b", "county_focal_yr",
  
  "RES_RATIO_1", "RES_RATIO_2", "RES_RATIO_3", "RES_RATIO_4",
  "EJ_Score", "EJ_Bin", "Distance", "Distance_Binned", "influencer_any_b",
  "fishing_any_b", "fishing_most_b")

# --------------------------------------------------------------------------
# 2. Reorder columns and keep only desired ones existing in d1
# --------------------------------------------------------------------------
d2 <- d1 %>%
  dplyr::select(any_of(desired_cols))%>%
  glimpse()

# --------------------------------------------------------------------------
# 3. Check for any requested columns missing in d1
# --------------------------------------------------------------------------
missing_cols <- setdiff(desired_cols, names(d1))
if (length(missing_cols) > 0) {
  message("Columns from the list not found in d1: ", paste(missing_cols, collapse = ", "))
}

glimpse(d2)

# --------------------------------------------------
# make var for nature activities ------------
d2 %>%
  count(q_activity_most3, sort = TRUE)

d3 <- d2 %>%
  mutate(
    # Strict definition: Direct interaction with marine species, tidepools, or ecosystems
    nature_interact_strict = case_when(
      is.na(q_activity_most3) ~ NA_real_,
      str_detect(
        q_activity_most3,
        regex("Nature Observing|Fishing|Collecting|Tidepool|Snorkeling|Scuba|Kayak|Paddle|Boating|Cultural", ignore_case = TRUE)
      ) ~ 1,
      TRUE ~ 0
    ),
    
    # Broad definition: Adds ocean immersion & shoreline movement (swimming, surfing, walking/beachcombing)
    nature_interact_broad = case_when(
      is.na(q_activity_most3) ~ NA_real_,
      nature_interact_strict == 1 ~ 1,
      str_detect(
        q_activity_most3,
        regex("Swimming|Bodysurfing|Surfing|Walking|Running|Hiking", ignore_case = TRUE)
      ) ~ 1,
      TRUE ~ 0
    ),
    
    # Labeled factor version for clean plots and tables
    nature_interact_cat = factor(
      nature_interact_strict,
      levels = c(1, 0),
      labels = c("Direct Nature Interaction", "Other Coastal Activity")
    )
  )%>%
  glimpse()

# 2. Cross-tabulate to verify every category mapped as expected
d3 %>%
  count(q_activity_most3, nature_interact_strict, nature_interact_broad)


write_csv(d3,"./results/data_long13.csv")
