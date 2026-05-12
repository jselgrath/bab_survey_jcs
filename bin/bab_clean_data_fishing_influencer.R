# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: graph activity data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(lubridate)

# assign binary variable to fishing as most important activity from online surveys 2024-09-05 to 2024-11-04 based on influencer
# assign binary variable to fishing as any important activity from online surveys 2024-09-05 to 2024-11-04 based on influencer

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")


# read file
d1 <- read_csv("./results/data_long7.csv") %>%
  mutate(response_id = ResponseId) %>%
  
  # during influencer period, fishing is most important activity - online only
  mutate(
    influencer_most_b = if_else(
      date(RecordedDate) >= as.Date("2024-09-05") & 
        date(RecordedDate) <= as.Date("2024-11-04") &
        Mechanism == "Online" &
        QImportant_Activities_Most2 == "Fishing or collecting food",
      1, 0, missing = 0
    )
  ) %>%
  
  # during influencer period, fishing is most important activity  - all responses
  mutate(
    influencer_most_b2 = if_else(
      date(RecordedDate) >= as.Date("2024-09-05") & 
        date(RecordedDate) <= as.Date("2024-11-04") &
        QImportant_Activities_Most2 == "Fishing or collecting food",
      1, 0, missing = 0
    )
  ) %>%
  
  # during influencer period, fishing is any important activity  - online only
  mutate(
    influencer_any_b = if_else(
      date(RecordedDate) >= as.Date("2024-09-05") & 
        date(RecordedDate) <= as.Date("2024-11-04") &
        Mechanism == "Online" & (
          str_detect(QImportant_Activities2, fixed("fishing", ignore_case = TRUE)) | 
            str_detect(QImportant_Activities_TEXT, fixed("fishing", ignore_case = TRUE))
        ),
      1, 0, missing = 0
    )
  ) %>%
  
  # during influencer period, fishing is any important activity  - all responses
  mutate(
    influencer_any_b2 = if_else(
      date(RecordedDate) >= as.Date("2024-09-05") & 
        date(RecordedDate) <= as.Date("2024-11-04") &
        (
          str_detect(QImportant_Activities2, fixed("fishing", ignore_case = TRUE)) | 
            str_detect(QImportant_Activities_TEXT, fixed("fishing", ignore_case = TRUE))
        ),
      1, 0, missing = 0
    )
  ) %>%
  
  # 3. fishing is activity during any time period
  mutate(
    fishing_any_b = if_else(
      str_detect(QImportant_Activities2, fixed("fishing", ignore_case = TRUE)) | 
        str_detect(QImportant_Activities_TEXT, fixed("fishing", ignore_case = TRUE)),
      1, 0, missing = 0
    )
  ) %>%
  
  # 4. fishing is most important activity during any time period
  mutate(
    fishing_most_b = if_else(
      str_detect(QImportant_Activities_Most, fixed("fishing", ignore_case = TRUE)),
      1, 0, missing = 0
    )
  ) %>%
  glimpse()



# # Summary table ----------------
# influencer_t <- d1 %>% 
#   count(influencer_any_b)
# 
# print(influencer_t)
# 
# influencer_t<-tibble(table(d1$influencer_most_b))
# influencer_t
# 
# 



# summarize as a table------------
fishing_summary_table <- bind_rows(
  
  # All Respondents
  d1 %>%
    select(fishing_any_b, fishing_most_b) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "All Responses - Any Mechanism"),
  
  # removed influencer data - any fishing - online only
    d1 %>%
    filter(influencer_any_b!=1) %>%
    select(fishing_any_b, fishing_most_b) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Removed Influencer - Any Fishing - Online"),
  
  # removed influencer data - most important fishing - online only
  d1 %>%
    filter(influencer_most_b!=1) %>%
    select(fishing_any_b, fishing_most_b) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Removed Influencer - Most important fishing - Online"),
  
  
  # Influencer Period Only - online
  d1 %>%
    filter(influencer_any_b==1) %>%
    select(influencer_any_b, influencer_most_b) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Influencer Period Only - Any Fishing - Online"),
  
  # # Influencer Period Most Important - any mechanism (online and in person)
  d1 %>%
    filter(influencer_most_b2==1) %>%
    select(influencer_most_b) %>% #influencer_any_b, 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Influencer Period Only - Most Important Fishing - Any Mechanism"),
  
  # # Influencer Period Most Important - online 
  d1 %>%
    filter(influencer_most_b2==1) %>%
    select(influencer_most_b2) %>% #influencer_any_b, 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Influencer Period Only - Most Important Fishing - Online"), 
  
  # Influencer Period Any mechanism (online and in person)
  d1 %>%
    filter(influencer_any_b2==1) %>%
    select(influencer_any_b, influencer_most_b) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    group_by(Variable) %>%
    
    summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Influencer Period Only - Any Fishing - Any Mechanism")
  
  # # # removed Influencer Period Most Important - Any Mechanism
  # d1 %>%
  #   filter(influencer_most_b2!=1) %>%
  #   select(fishing_most_b) %>%
  #   pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
  #   group_by(Variable) %>%
  #   summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Removed Influencer - Most important fishing - Any Mechanism"),
  # 
  # # # removed Influencer Period Most Important - Any Mechanism
  # d1 %>%
  #   filter(influencer_most_b2!=1) %>%
  #   select(fishing_any_b) %>%
  #   pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
  #   group_by(Variable) %>%
  #   summarise(n = sum(Status == 1, na.rm = TRUE), total = n(), Group = "Removed Influencer - Any fishing - Any Mechanism")
  # 

)%>%

  


  
  
  # 3. Final formatting
  mutate(
    percent = round((n / total) * 100, 1),
    Variable_Clean = case_when(
      Variable == "influencer_most_b" ~ "Fishing: Most Important Activity",
      Variable == "influencer_any_b"  ~ "Fishing: Important Activity",
      Variable == "fishing_any_b"     ~ "Fishing: Important Activity",
      Variable == "fishing_most_b"    ~ "Fishing: Most Important Activity",
      Variable == "influencer_most_b2"    ~ "Fishing: Most Important Activity",     
      
      TRUE ~ Variable
    )
  ) %>%
  select(Group, Variable_Clean, n, total, percent) %>%
  arrange(Group, desc(n))%>%
  glimpse()

print(fishing_summary_table) # checked - numbers match


# save ---------------
write_csv(d1,"./results/data_long8.csv")
write_csv(fishing_summary_table,"./doc/fishing_influencer_summary.csv")


