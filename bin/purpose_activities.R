# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: graph activities
# ------------------

library(tidyverse)
library(colorspace)
library(viridis)

# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean4_maindata_responses.csv")%>%
  glimpse()

unique(d1$q3_purpose)

# -------------------------------------------------------------------------
# 1. Define clean label lookup mapping
# -------------------------------------------------------------------------
activity_labels <- c(
  "Fishing or collecting food"                        = "Fishing/Collecting Food",
  "Group/Family gatherings or activities"             = "Family/Group Activities",
  "Meditation/Reading/Relaxing/Art"                   = "Meditation/Relaxing/Art",
  "Swimming/Bodysurfing"                              = "Swimming/Bodysurfing",
  "Festivals"                                         = "Festivals",
  "Bicycling/Roller skating/Skateboarding"            = "Cycling/Skating",
  "Paid work"                                         = "Paid Work",
  "Beach games/Sports/Yoga"                           = "Beach Games/Sports/Yoga",
  "Walking/Running/Hiking"                            = "Walking/Running/Hiking",
  "Enjoying views/sunsets"                            = "Enjoying Views/Sunsets",
  "Surfing"                                           = "Surfing",
  "Cultural or religious ceremonies"                  = "Cultural/Religious Ceremonies",
  "Nature Observing/Photographing/Education/Research" = "Nature Observation",
  "Paddleboarding/Kiteboarding/Kayaking/Canoeing"     = "Paddle/Kayak/Canoe",
  "Snorkeling/Scuba Diving"                           = "Snorkeling/Scuba",
  "Volunteering"                                      = "Volunteering",
  "Sailing/Boating"                                   = "Sailing/Boating",
  "Another activity"                                  = "Another Activity"
)

# function ----------------------
clean_raw_activities <- function(x) {
  x %>%
    # A. Protect phrases containing commas by converting to slashes
    str_replace_all("Bicycling,\\s*roller skating,\\s*skateboarding,\\s*etc\\.?", "Bicycling/Roller skating/Skateboarding") %>%
    str_replace_all("Relaxing,\\s*reading,\\s*sun-bathing,\\s*meditation", "Meditation/Reading/Relaxing/Art") %>%
    str_replace_all("Observing or photographing nature or wildlife,\\s*outdoor education", "Nature Observing/Photographing/Education/Research") %>%
    
    # B. Standardize variations and trailing spaces
    str_replace_all("Festivals with music and(/or)? food(\\s*\\(e\\.g\\., earth day\\))?", "Festivals") %>%
    str_replace_all("Beach games or sports(\\s*\\(e\\.g\\., frisbee, volleyball, yoga\\))?", "Beach games/Sports/Yoga") %>%
    str_replace_all("Group or family gatherings or activities(\\s*\\(e\\.g\\., family outing, bbq\\))?", "Group/Family gatherings or activities") %>%
    str_replace_all("Volunteering(\\s*\\(e\\.g\\., beach clean-ups\\))?", "Volunteering") %>%
    str_replace_all("Sailing/Boating(\\s*\\(engine powered\\))?", "Sailing/Boating") %>%
    str_replace_all("Stand up paddleboarding/kite or sailboarding/kayaking", "Paddleboarding/Kiteboarding/Kayaking/Canoeing") %>%
    str_replace_all("Cultural or religious practices or ceremonies", "Cultural or religious ceremonies") %>%
    str_replace_all("Driving or sitting in your car to enjoy the views/sunsets", "Enjoying views/sunsets") %>%
    str_replace_all("Walking or running", "Walking/Running/Hiking") %>%
    str_replace_all("Swimming or bodysurfing", "Swimming/Bodysurfing") %>%
    str_replace_all("Fishing or collecting food\\s+", "Fishing or collecting food") %>%
    
    # C. Standardize "Other" / "None" options
    str_replace_all("Other,\\s*please specify:?", "Another activity") %>%
    str_replace_all("None of the above", "Another activity") %>%
    
    # D. Remove any remaining explanatory parentheses
    str_replace_all("\\s*\\([^)]*\\)", "") %>%
    str_trim()
}

# -------------------------------------------------------------------------
# 3. Apply cleaning to the MWIS dataset
# -------------------------------------------------------------------------
d2 <- d1 %>%
  mutate(
    # Clean the multi-selection activities
    q3_purpose_clean = clean_raw_activities(q3_purpose),
    
    # Clean the single "most important" activity
    q4_purpose_most_important_clean = clean_raw_activities(q4_purpose_most_important)
  )

# Verify the cleaned strings
head(d2$q3_purpose_clean, 20)


# long format and apply short labels: one row per respondent per activity
activities_long <- d2 %>%
  filter(!is.na(q3_purpose_clean), q3_purpose_clean != "") %>%
  select(response_id, m3_program, q3_purpose_clean) %>%
  separate_rows(q3_purpose_clean, sep = ",") %>%
  mutate(
    activity_raw = str_trim(q3_purpose_clean),
    # Map to standardized short display names
    activity_label = recode(activity_raw, !!!activity_labels)
  ) %>%
  filter(activity_raw != "")

# Verification: inspect all unique mapped categories
activities_long %>%
  count(activity_raw, activity_label, sort = TRUE) %>%
  print(n = 25)



# -------------------------------------------------------------------------
# 1. Prepare & Count Cleaned Activities
# -------------------------------------------------------------------------
# Total number of unique respondents answering the activity question
n_total_respondents <- d2 %>%
  filter(!is.na(q3_purpose_clean), q3_purpose_clean != "") %>%
  pull(response_id) %>%
  n_distinct()

# Unnest, clean, map to short labels, and order by frequency
activities_summary <- d2 %>%
  filter(!is.na(q3_purpose_clean), q3_purpose_clean != "") %>%
  select(response_id, q3_purpose_clean) %>%
  separate_rows(q3_purpose_clean, sep = ",") %>%
  mutate(
    activity_raw = str_trim(q3_purpose_clean),
    activity_label = recode(activity_raw, !!!activity_labels)
  ) %>%
  filter(activity_label != "") %>%
  # Count each activity across respondents
  count(activity_label, name = "Count") %>%
  mutate(
    # Percentage of total respondents engaging in this activity
    pct = Count / n_total_respondents,
    label = paste0(comma(Count), "\n(", percent(pct, accuracy = 0.1), ")"),
    # Reorder factor from most common to least common (left to right)
    activity_label = fct_reorder(activity_label, Count, .desc = TRUE)
  )

# -------------------------------------------------------------------------
# 2. Plot: Vertical Bars Ordered by Frequency
# -------------------------------------------------------------------------
plt_activities <- ggplot(activities_summary, aes(x = activity_label, y = Count, fill = Count)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(
    aes(label = label),
    vjust = -0.25,
    size = 2.8,
    lineheight = 0.85,
    color = "grey20"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = comma
  ) +
  # Continuous mako gradient highlighting highest volume categories
  scale_fill_viridis_c(option = "mako", begin = 0.35, end = 0.85, direction = 1) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Activities Engaged in by Survey Respondents (N = ", comma(n_total_respondents), ")"),
    subtitle = "Percentages reflect proportion of total respondents (multi-selection allowed)",
    x = "Activity",
    y = "No. Respondents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 14)),
    axis.text.x = element_text(size = 9,       face = "bold",  color = "black",  angle = 90,  hjust = 1,vjust = 1),
    axis.text.y = element_text(size = 8.5, color = "grey30"),
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, face = "bold", color = "grey30", margin = margin(r = 8)),
    axis.ticks.y = element_line(color = "grey80"),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(15, 15, 10, 15)
  )

plt_activities

# -------------------------------------------------------------------------
# 3. Save Output
# -------------------------------------------------------------------------
ggsave("./doc/activities.png",  plot = plt_activities,  width = 11,  height = 5.5,  dpi = 300)