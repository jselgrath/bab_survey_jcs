# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: make of table of fishing as most important activity vs all surveys by month for online data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(lubridate)

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# read file
d1<-read_csv("./results/data_long6.csv")%>%
  glimpse()

# start_date
sd<-20240901

# end_date
ed<-20241101

# ---------------------------------------------------------
# ONLINE: ANYONE WHO FISHES
# ---------------------------------------------------------

# 2. Create the Monthly Summary Table
monthly_fishing_any_summary <- d1 %>%
  filter(Mechanism == "Online") %>%
  # Ensure RecordedDate is valid
  filter(!is.na(RecordedDate)) %>%
  mutate(
    # Get the first day of the month
    start_date_obj = floor_date(as_date(RecordedDate), unit = "month"),
    # Calculate the last day of the month
    end_date_obj = ceiling_date(start_date_obj, unit = "month") - days(1)
  ) %>%
  group_by(start_date_obj, end_date_obj,Mechanism) %>%
  summarize(
    responses_n = n(),
    # Use str_detect to find "Fishing or collecting food" anywhere in the comma-separated string
    fishing_n = sum(
      str_detect(QImportant_Activities2, "Fishing or collecting food"), 
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  # 3. Reformat to YYYYMMDD
  mutate(
    start_date = format(start_date_obj, "%Y%m%d"),
    end_date = format(end_date_obj, "%Y%m%d")
  ) %>%
  # 4. Clean up and calculate percentage
  select(start_date, end_date, responses_n, fishing_n,Mechanism) %>%
  mutate(fishing_pct = fishing_n / responses_n,
         fishing_type="any_activity",
         mechanism=Mechanism) %>%
  select(-Mechanism)%>%
  arrange(start_date)

# View the monthly result
print(monthly_fishing_any_summary)


# ---------------------------------------------------------
# Grand Means (Summary Versions)
# ---------------------------------------------------------

# grand mean - all dates
summary_all <- monthly_fishing_any_summary %>%
  summarize(
    responses_n_all = sum(responses_n),
    fishing_n_all = sum(fishing_n),
    fishing_pct_all = fishing_n_all / responses_n_all
  )

# grand mean - without sept and oct 2024
summary_no_aw <- monthly_fishing_any_summary %>%
  filter(start_date != sd & start_date != ed) %>%
  summarize(
    responses_n_no_aw = sum(responses_n),
    fishing_n_no_aw = sum(fishing_n),
    fishing_pct_no_aw = fishing_n_no_aw / responses_n_no_aw
  )

# grand mean - only sept and oct 2024
summary_only_aw <- monthly_fishing_any_summary %>%
  filter(start_date == sd | start_date == ed) %>%
  summarize(
    responses_n_only_aw = sum(responses_n),
    fishing_n_only_aw = sum(fishing_n),
    fishing_pct_only_aw = fishing_n_only_aw / responses_n_only_aw
  )

# Combine summaries into one wide table
monthly_fishing_any_versions <- cbind(summary_all, summary_no_aw, summary_only_aw) %>%
  mutate(mechanism="Online",
         fishing_type="any_activity")%>%
  glimpse()

# ---------------------------------------------------------
# ONLINE: MOST IMPORTANT FISHING
# ---------------------------------------------------------




# All surveys online: all vs fishing is most important activity  ------------------------

# 1. Create the Monthly Summary Table
monthly_fishing_summary_most <- d1 %>%
  filter(Mechanism=="Online")%>%
  # Ensure RecordedDate is valid
  filter(!is.na(RecordedDate)) %>%
  mutate(
    # Get the first day of the month
    start_date_obj = floor_date(as_date(RecordedDate), unit = "month"),
    # Calculate the last day of the month
    end_date_obj = ceiling_date(start_date_obj, unit = "month") - days(1)
  ) %>%
  group_by(start_date_obj, end_date_obj,Mechanism) %>%
  summarize(
    responses_n = n(),
    fishing_n = sum(
      Mechanism == "Online" & 
        QImportant_Activities_Most2 == "Fishing or collecting food", 
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  # 2. Reformat to YYYYMMDD
  mutate(
    start_date = format(start_date_obj, "%Y%m%d"),
    end_date = format(end_date_obj, "%Y%m%d")
  ) %>%
  # 3. Clean up and organize
  select(start_date, end_date, responses_n, fishing_n,Mechanism) %>%
  mutate(fishing_pct=fishing_n/responses_n,
         fishing_type="most_important_activity",
         mechanism="Online")%>%
  select(-Mechanism)%>%
  arrange((start_date))

# View the final result
print(monthly_fishing_summary_most )

# grand mean -----
sum(monthly_fishing_summary_most$fishing_n)/sum(monthly_fishing_summary_most $responses_n)



# grand mean - all dates
monthly_fishing_summary_most1<-monthly_fishing_summary_most %>%
  # filter(start_date!=sd&start_date!=20241001)%>%
  summarize(
    responses_n_all = sum(responses_n),
    fishing_n_all = sum(fishing_n),
    fishing_pct_all = fishing_n_all/responses_n_all
  )%>%
  glimpse()


# grand mean - without sept and oct 2024
monthly_fishing_summary_most2<-monthly_fishing_summary_most %>%
  filter(start_date!=sd &start_date!=20241001 &start_date!=ed)%>%
  summarize(
    responses_n_no_aw = sum(responses_n),
    fishing_n_no_aw = sum(fishing_n),
    fishing_pct_no_aw = fishing_n_no_aw/responses_n_no_aw
  )%>%
  glimpse()


# grand mean - only sept and oct 2024
monthly_fishing_summary_most3<-monthly_fishing_summary_most %>%
  filter(start_date==sd|start_date==20241001|start_date==ed)%>%
  summarize(
    responses_n_only_aw = sum(responses_n),
    fishing_n_only_aw = sum(fishing_n),
    fishing_pct_only_aw = fishing_n_only_aw/responses_n_only_aw
  )%>%
  glimpse()



# review tables
print(monthly_fishing_summary_most )
print(monthly_fishing_summary_most1)
print(monthly_fishing_summary_most2)
print(monthly_fishing_summary_most3)

monthly_fishing_summary_most_versions<-cbind(monthly_fishing_summary_most1,monthly_fishing_summary_most2,monthly_fishing_summary_most3)%>%
  mutate(mechanism="Online",
         fishing_type="most_important_activity")%>%
  glimpse()



# ---------------------------------------------------------
# Save 
# ---------------------------------------------------------
write_csv(monthly_fishing_any_summary, "./doc/activity_fishing_any_monthly_online.csv")
write_csv(monthly_fishing_any_versions, "./doc/activity_fishing_any_summaries_online.csv")

write_csv(monthly_fishing_summary_most,"./doc/activity_fishing_most_monthly_online.csv")
write_csv(monthly_fishing_summary_most_versions,"./doc/activity_fishing_most_summaries_online.csv")

