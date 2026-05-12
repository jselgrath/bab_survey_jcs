# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: make of table of fishing as most important activity vs all surveys by month for all data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(lubridate)
library(janitor)

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# read files
d1<-read_csv("./results/data_long8.csv")%>%
  glimpse()

unique(d1$Mechanism)


monthly_breakdown <- d1 %>%
  mutate(survey_month = floor_date(date(RecordedDate), "month")) %>%
  filter(!is.na(survey_month)) %>%
  
  # 1. Count by Month and Mechanism
  group_by(survey_month, Mechanism) %>%
  summarise(
    n = n(),
    fishing_any_n = sum(fishing_any_b == 1, na.rm = TRUE),
    fishing_most_n = sum(fishing_most_b == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    names_from = Mechanism, 
    values_from = c(n, fishing_any_n, fishing_most_n),
    values_fill = 0
  ) %>%
  
  # Calculate Totals and Percentages
  mutate(
    # Fix the date format
    survey_month = as.Date(survey_month),
    
    total_n = n_Online + n_In_Person,
    fishing_most_pct_all = ((fishing_most_n_Online + fishing_most_n_In_Person) / total_n) * 100,
    fishing_most_pct_online = if_else(n_Online > 0, (fishing_most_n_Online / n_Online) * 100, 0),
    fishing_most_pct_inperson = if_else(n_In_Person > 0, (fishing_most_n_In_Person / n_In_Person) * 100, 0)
  ) %>%
  
  select(survey_month, total_n, n_In_Person,n_Online,fishing_any_n_In_Person:fishing_most_pct_inperson) %>%
  arrange(survey_month)

glimpse(monthly_breakdown)

print(monthly_breakdown)




# Plotting
ggplot(monthly_breakdown, aes(x = survey_month, y = fishing_most_pct_all)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0("n=", total_n)), vjust = -1.5, fontface = "bold") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Monthly Fishing Preference (%)", x = "Month", y = "Percent Most Important") +
  theme_minimal()

# 1. Prepare data for plotting
plot_data <- monthly_breakdown %>%
  # Select the columns we want to plot plus the label column
  select(survey_month, total_n, 
         fishing_most_pct_all, 
         fishing_most_pct_online, 
         fishing_most_pct_inperson) %>%
  # Pivot percentages so they can be colored by a "Type" variable
  pivot_longer(
    cols = starts_with("fishing_most_pct"),
    names_to = "pct_type",
    values_to = "percentage"
  ) %>%
  # Clean up labels for the legend
  mutate(pct_type = case_when(
    pct_type == "fishing_most_pct_all" ~ "Total (All)",
    pct_type == "fishing_most_pct_online" ~ "Online",
    pct_type == "fishing_most_pct_inperson" ~ "In Person"
  ))

# 2. Create the graph
ggplot(plot_data, aes(x = survey_month, y = percentage, color = pct_type, group = pct_type)) +
  # Lines and points
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  
  # Add total_n labels (only once per month, so we filter the text data)
  geom_text(data = filter(plot_data, pct_type == "Total (All)"),
            aes(label = paste0("n = ",total_n)), #pct_
            vjust = -2.5,  # Push text higher above the points
            color = "black",
            fontface = "bold",
            size = 3.5) +
  
  # Formatting
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.3))) + # Adds headroom for labels
  scale_color_manual(values = c("Total (All)" = "black", 
                                "Online" = "#3498db", 
                                "In Person" = "#e67e22")) +
  labs(
    title = "Fishing as Most Important Activity Over Time",
    subtitle = "Monthly percentages by survey mechanism",
    x = "Survey Month",
    y = "Percent (%)",
    color = "Sampling"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )

ggsave("./doc/fishing_influencer_time.png", width = 8, height = 5)