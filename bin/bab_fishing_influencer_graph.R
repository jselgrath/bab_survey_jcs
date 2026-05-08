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



d1<-read_csv("./doc/activity_fishing_any_monthly_online.csv")%>%
  # select(-Mechanism)%>%
  glimpse()

d2<-read_csv("./doc/activity_fishing_any_summaries_online.csv")%>%  glimpse()

d3<-read_csv("./doc/activity_fishing_most_monthly_online.csv")%>%
  glimpse()
d4<-read_csv("./doc/activity_fishing_most_summaries_online.csv")%>%
  glimpse()

d5<-read_csv("./doc/activity_fishing_any_monthly_all.csv")%>%
  glimpse()
d6<-read_csv("./doc/activity_fishing_any_summaries_all.csv")%>%
  glimpse()

d7<-read_csv("./doc/activity_fishing_most_monthly_all.csv")%>%
  glimpse()
d8<-read_csv("./doc/activity_fishing_most_summaries_all.csv")%>%
  glimpse()

# combine
d9<-rbind(d1,d3,d5,d7)%>%
  mutate(date = ymd(start_date))%>%
  glimpse()

d10<-rbind(d2,d4,d6,d8)%>%
  glimpse()


# summarized ------------------------------
# wide to long format
d11 <- d10 %>%
  pivot_longer(
    cols = starts_with("fishing_pct"), 
    names_to = "group", 
    values_to = "percentage"
  ) %>%
  # Clean up the labels for a prettier legend
  mutate(group = str_replace_all(group, "fishing_pct_", ""),
         group = str_replace_all(group, "_", " "),
         group = str_to_title(group))

# graph
ggplot(d11, aes(x = fishing_type, y = percentage, fill = group)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~mechanism) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "mako") + # Professional color palette
  theme_minimal() +
  labs(
    title = "Fishing Activity Percentages",
    subtitle = "Comparison across groups by mechanism and activity type",
    x = "Fishing Activity Type",
    y = "Percent",
    fill = "Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./doc/influencer_pct.png",  width = 10, height = 6, dpi = 300)


# by time -----------------------
# 2. Convert numeric date to Date class

#  Plot
ggplot(d9, 
       aes(x = date, 
           y = fishing_pct, 
           # interaction() creates the 4 joint levels
           color = interaction(mechanism, fishing_type, sep = " - "))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  # Using a discrete palette with 4 distinct colors
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Fishing as an Activity by Sampling Mechanism",
    # subtitle = "Four distinct colors for each Mechanism/Type combination",
    x = "Month",
    y = "Percent",
    color = "Sampling Mechanism & Activity Type"
  ) +
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))

# Save 
ggsave("./doc/influencer_pctP_time.png", width = 8, height = 5)


