# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: make of table of fishing as most important activity vs all surveys by month

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
  group_by(start_date_obj, end_date_obj) %>%
  summarize(
    online_responses = n(),
    # Use str_detect to find "Fishing or collecting food" anywhere in the comma-separated string
    fishing_any_online = sum(
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
  select(start_date, end_date, online_responses, fishing_any_online) %>%
  mutate(pct_fishing_any = fishing_any_online / online_responses) %>%
  arrange(start_date)

# View the monthly result
print(monthly_fishing_any_summary)


# ---------------------------------------------------------
# Grand Means (Summary Versions)
# ---------------------------------------------------------

# grand mean - all dates
summary_all <- monthly_fishing_any_summary %>%
  summarize(
    online_responses_all = sum(online_responses),
    fishing_any_online_all = sum(fishing_any_online),
    fishing_pct_all = fishing_any_online_all / online_responses_all
  )

# grand mean - without sept and oct 2024
summary_no_aw <- monthly_fishing_any_summary %>%
  filter(start_date != "20240901" & start_date != "20241001") %>%
  summarize(
    online_responses_no_aw = sum(online_responses),
    fishing_any_online_no_aw = sum(fishing_any_online),
    fishing_pct_no_aw = fishing_any_online_no_aw / online_responses_no_aw
  )

# grand mean - only sept and oct 2024
summary_only_aw <- monthly_fishing_any_summary %>%
  filter(start_date == "20240901" | start_date == "20241001") %>%
  summarize(
    online_responses_only_aw = sum(online_responses),
    fishing_any_online_only_aw = sum(fishing_any_online),
    fishing_pct_only_aw = fishing_any_online_only_aw / online_responses_only_aw
  )

# Combine summaries into one wide table
monthly_fishing_any_versions <- cbind(summary_all, summary_no_aw, summary_only_aw) %>%
  glimpse()

# ---------------------------------------------------------
# ONLINE: MOST IMPORTANT FISHING
# ---------------------------------------------------------




# All surveys online: all vs fishing is most important activity  ------------------------

# 1. Create the Monthly Summary Table
monthly_fishing_summary <- d1 %>%
  filter(Mechanism=="Online")%>%
  # Ensure RecordedDate is valid
  filter(!is.na(RecordedDate)) %>%
  mutate(
    # Get the first day of the month
    start_date_obj = floor_date(as_date(RecordedDate), unit = "month"),
    # Calculate the last day of the month
    end_date_obj = ceiling_date(start_date_obj, unit = "month") - days(1)
  ) %>%
  group_by(start_date_obj, end_date_obj) %>%
  summarize(
    online_responses = n(),
    fishing_most_online = sum(
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
  select(start_date, end_date, online_responses, fishing_most_online) %>%
  mutate(pct_fishing_most=fishing_most_online/online_responses)%>%
  arrange((start_date))

# View the final result
print(monthly_fishing_summary)

# grand mean -----
sum(monthly_fishing_summary$fishing_most_online)/sum(monthly_fishing_summary$online_responses)



# grand mean - all dates
monthly_fishing_summary1<-monthly_fishing_summary%>%
  # filter(start_date!=20240901&start_date!=20241001)%>%
  summarize(
    online_responses_all = sum(online_responses),
    fishing_most_online_all = sum(fishing_most_online),
    fishing_pct_all = fishing_most_online_all/online_responses_all
  )%>%
  glimpse()


# grand mean - without sept and oct 2024
monthly_fishing_summary2<-monthly_fishing_summary%>%
  filter(start_date!=20240901&start_date!=20241001)%>%
  summarize(
    online_responses_no_aw = sum(online_responses),
    fishing_most_online_no_aw = sum(fishing_most_online),
    fishing_pct_no_aw = fishing_most_online_no_aw/online_responses_no_aw
  )%>%
  glimpse()


# grand mean - only sept and oct 2024
monthly_fishing_summary3<-monthly_fishing_summary%>%
  filter(start_date==20240901|start_date==20241001)%>%
  summarize(
    online_responses_only_aw = sum(online_responses),
    fishing_most_online_only_aw = sum(fishing_most_online),
    fishing_pct_only_aw = fishing_most_online_only_aw/online_responses_only_aw
  )%>%
  glimpse()



# review tables
print(monthly_fishing_summary)
print(monthly_fishing_summary1)
print(monthly_fishing_summary2)
print(monthly_fishing_summary3)

monthly_fishing_summary_versions<-cbind(monthly_fishing_summary1,monthly_fishing_summary2,monthly_fishing_summary3)%>%
  glimpse()



# ---------------------------------------------------------
# Save 
# ---------------------------------------------------------
write_csv(monthly_fishing_any_summary, "./doc/activity_online_fishing_any_monthly.csv")
write_csv(monthly_fishing_any_versions, "./doc/activity_online_fishing_any_summaries.csv")

write_csv(monthly_fishing_summary,"./doc/activity_online_fishing_most_monthly.csv")
write_csv(monthly_fishing_summary_versions,"./doc/activity_online_fishing_most_summaries.csv")
