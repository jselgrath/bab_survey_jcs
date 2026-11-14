# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# Goal: compare most important activity by frequency of use and mentor

# ----------------------------------------------------------
# Load libraries -------------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(ggpattern)
library(stringr)
library(ggalluvial)

# --------------------------------------------------------------------------
# Load data ----------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d0 <- read_csv("./results/data_final_weighted_focal.csv", guess_max = Inf) %>%   glimpse()

d1<-d0%>%
    filter(nature_interact_strict==1)%>%
      # filter(nature_interact_broad==1)%>%
  glimpse()

unique(d1$QActivity_Mentor)
unique(d1$QActual_Time)

# --------------------------------------------------------------------------
# organize data -----
# --------------------------------------------------------------------------
# Prep base weighted dataset
base_df <- d1 %>%
  filter(!is.na(weight_focal_pop), !is.na(q_activity_most3)) %>%
  mutate(
    freq_ordered = factor(
      QActual_Time,
      levels = c(
        "Less than once a year",
        "Once a year",
        "Once every couple months",
        "Several times per month",
        "Several times per week"
      )
    ),
    mentor_clean = case_when(
      str_detect(QActivity_Mentor, "^Family") ~ "Family Member",
      str_detect(QActivity_Mentor, "^Friends") ~ "Friends / Colleagues",
      str_detect(QActivity_Mentor, "^I taught") ~ "Self-Taught",
      str_detect(QActivity_Mentor, "^School") ~ "School / Club / Group",
      str_detect(QActivity_Mentor, "^Cultural") ~ "Cultural Practitioner",
      !is.na(QActivity_Mentor) ~ "Other",
      TRUE ~ NA_character_
    ),
    mentor_clean = factor(
      mentor_clean,
      levels = c(
        "Family Member",
        "Friends / Colleagues",
        "Self-Taught",
        "School / Club / Group",
        "Cultural Practitioner",
        "Other"
      )
    )
  )

# # Summarize Activity x Mentorship
mentor_summary1 <- base_df %>%
  filter(!is.na(mentor_clean)) %>%
  group_by(q_activity_most3, mentor_clean) %>%
  summarise(weighted_n = sum(weight_focal_pop), .groups = "drop") %>%
  group_by(q_activity_most3) %>%
  mutate(
    act_total = sum(weighted_n),
    pct = weighted_n / act_total
  ) %>%
  ungroup()

# Order activities by overall weighted popularity
act_order <- mentor_summary1 %>%
  distinct(q_activity_most3, act_total) %>%
  arrange(act_total) %>%
  pull(q_activity_most3)

mentor_summary <- mentor_summary1 %>%
  mutate(q_activity_most3 = factor(q_activity_most3, levels = act_order))

# Plot: Activity x Mentorship
# ggplot(mentor_summary, aes(x = pct, y = q_activity_most3, fill = mentor_clean)) +
#   geom_col(width = 0.75, color = "white", linewidth = 0.25) +
#   geom_text(
#     aes(label = ifelse(pct >= 0.08, percent(pct, accuracy = 1), "")),
#     position = position_stack(vjust = 0.5),
#     size = 3,
#     color = "white",
#     fontface = "bold"
#   ) +
#   scale_x_continuous(labels = percent_format(), expand = c(0, 0)) +
#   scale_fill_discrete_qualitative(palette = "Dark 3", name = "Learned From") +
#   labs(
#     title = "How Coastal Activities Are Learned",
#     subtitle = "Weighted share of focal-area respondents by primary activity",
#     x = "Weighted Percentage",
#     y = NULL
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     legend.position = "bottom",
#     panel.grid.major.y = element_blank()
#   ) +
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE))


# ----------------------------------------
# Calculate overall baseline share of each mentor type
mentor_baseline <- mentor_summary %>%
  group_by(mentor_clean) %>%
  summarise(baseline_pct = sum(weighted_n) / sum(mentor_summary$weighted_n))

mentor_diff <- mentor_summary %>%
  left_join(mentor_baseline, by = "mentor_clean") %>%
  mutate(diff_pp = (pct - baseline_pct) * 100) # Difference in percentage points

ggplot(mentor_diff, aes(x = mentor_clean, y = q_activity_most3, fill = diff_pp)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+.0f pp", diff_pp)),
    size = 3,
    color = ifelse(abs(mentor_diff$diff_pp) > 12, "white", "grey20")
  ) +
  scale_fill_continuous_diverging(
    palette = "Blue-Red 3",
    mid = 0,
    name = "Diff vs. Overall\nAverage (pp)"
  ) +
  labs(
    title = "Mentorship Pathways to Activities",
    subtitle = "Percentage-point difference relative to the focal-area average across nature-based activities",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )

ggsave("./doc/bab_activity_mentor_wide_focal_ninteraction.png",  width = 18, height = 8, units = "in", dpi = 300, bg = "white")

# --------------------------------
freq_summary <- base_df %>%
  filter(!is.na(freq_ordered)) %>%
  group_by(q_activity_most3, freq_ordered) %>%
  summarise(weighted_n = sum(weight_focal_pop), .groups = "drop") %>%
  group_by(q_activity_most3) %>%
  mutate(pct = weighted_n / sum(weighted_n)) %>%
  ungroup()

# Order activities by share of high-frequency users (Several times/month or week)
high_freq_order <- freq_summary %>%
  filter(freq_ordered %in% c("Several times per month", "Several times per week")) %>%
  group_by(q_activity_most3) %>%
  summarise(high_pct = sum(pct)) %>%
  arrange(high_pct) %>%
  pull(q_activity_most3)

freq_diverging <- freq_summary %>%
  mutate(
    q_activity_most3 = factor(q_activity_most3, levels = high_freq_order),
    # Make lower frequencies negative so they plot to the left of 0%
    pct_signed = if_else(
      freq_ordered %in% c("Less than once a year", "Once a year", "Once every couple months"),
      -pct,
      pct
    )
  )

# Custom diverging palette (warm/amber for infrequent, deep teal/blue for frequent)
freq_colors <- c(
  "Less than once a year"    = "#D95F02",
  "Once a year"              = "#FDB863",
  "Once every couple months" = "#FEE0B6",
  "Several times per month"  = "#41B6C4",
  "Several times per week"   = "#08519C"
)

ggplot(freq_diverging, aes(x = pct_signed, y = q_activity_most3, fill = freq_ordered)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.6) +
  scale_x_continuous(
    labels = function(x) percent(abs(x)),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = freq_colors, name = "Frequency of Use") +
  labs(
    title = "Ocean Use Frequency by Primary Activity",
    subtitle = "Left: Every couple months or less  |  Right: Several times per month or week",
    x = "Weighted Percentage of Respondents",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave("./doc/bab_activity_time_wide_focal_ninteraction.png",  width = 18, height = 8, units = "in", dpi = 300, bg = "white")
