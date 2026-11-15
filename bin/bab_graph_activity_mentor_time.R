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

# --------------------------------------------------------------------------
# Load data ----------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# Short activity labels (matching previous plots)
activity_labels <- c(
  "Fishing or collecting food"                        = "Fishing/Collecting Food",
  "Group/Family gatherings or activities"             = "Family/Group Activities",
  "Meditation/Reading/Relaxing/Art/Health"            = "Meditation/Relaxing/Art/Health",
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
  "Beachcombing"                                      = "Beachcombing",
  "Camping"                                           = "Camping",
  "Habitat restoration / Conservation action"         = "Habitat Restoration/Conservation",
  "Another activity"                                  = "Another Activity"
)

# whole dataset ----------
d0 <- read_csv("./results/data_final_weighted_focal.csv", guess_max = Inf) %>%   
  glimpse()

# filter if want to use strict or broad definitions of nature interaction -------------
d1 <- d0 %>%
  filter(nature_interact_strict == 1) %>%
  # filter(nature_interact_broad == 1) %>%
  glimpse()

# --------------------------------------------------------------------------
# organize data -----
# --------------------------------------------------------------------------
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
      str_detect(QActivity_Mentor, "^Family")   ~ "Family Member",
      str_detect(QActivity_Mentor, "^Friends")  ~ "Friends / Colleagues",
      str_detect(QActivity_Mentor, "^I taught") ~ "Self-Taught",
      str_detect(QActivity_Mentor, "^School")   ~ "School / Club / Group",
      str_detect(QActivity_Mentor, "^Cultural") ~ "Cultural Practitioner",
      !is.na(QActivity_Mentor)                  ~ "Other",
      TRUE                                      ~ NA_character_
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

# ==========================================================================
# PLOT A: Activity x Mentorship (Diverging Heatmap)
# ==========================================================================
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

act_order <- mentor_summary1 %>%
  distinct(q_activity_most3, act_total) %>%
  arrange(act_total) %>%
  pull(q_activity_most3)

mentor_summary <- mentor_summary1 %>%
  mutate(q_activity_most3 = factor(q_activity_most3, levels = act_order))

mentor_baseline <- mentor_summary %>%
  group_by(mentor_clean) %>%
  summarise(baseline_pct = sum(weighted_n) / sum(mentor_summary$weighted_n))

mentor_diff <- mentor_summary %>%
  left_join(mentor_baseline, by = "mentor_clean") %>%
  mutate(diff_pp = (pct - baseline_pct) * 100)

p_mentor <- ggplot(mentor_diff, aes(x = mentor_clean, y = q_activity_most3, fill = diff_pp)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = sprintf("%+.0f pp", diff_pp)),
    size = 5,
    fontface = "bold",
    color = ifelse(abs(mentor_diff$diff_pp) > 12, "white", "grey20")
  ) +
  scale_y_discrete(labels = activity_labels) +
  scale_fill_continuous_diverging(
    palette = "Blue-Red 3",
    mid = 0,
    name = "Diff vs. Overall\nAverage (pp)"
  ) +
  labs(
    # title = "Mentorship Pathways to Activities",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.x   = element_text(angle = 90, hjust = 1, vjust = 1),
    legend.text   = element_text(size = 15),
    legend.title  = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid    = element_blank()
  )

p_mentor

ggsave("./doc/bab_activity_mentor_wide_focal_n_interaction.png", p_mentor, width = 18, height = 8, units = "in", dpi = 300, bg = "white")

# ==========================================================================
# PLOT B: Activity x Frequency of Ocean Use (Diverging Stacked Bars)
# ==========================================================================
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
    ),
    # Put outer edges FIRST in levels so position_stack() pushes them furthest from 0
    freq_stack = factor(
      freq_ordered,
      levels = c(
        "Several times per week",   # Outer right (furthest right)
        "Several times per month",  # Inner right (closest to 0)
        "Less than once a year",    # Outer left (furthest left)
        "Once a year",              # Middle left
        "Once every couple months"  # Inner left (closest to 0)
      )    )  )

freq_colors <- c(
  "Less than once a year"    = "#D95F02",
  "Once a year"              = "#FDB863",
  "Once every couple months" = "#FEE0B6",
  "Several times per month"  = "#41B6C4",
  "Several times per week"   = "#08519C"
)

p_freq <- ggplot(freq_diverging, aes(x = pct_signed, y = q_activity_most3, fill = freq_stack)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.8) +
  scale_x_continuous(
    labels = function(x) percent(abs(x)),
    limits = c(-1, 1)
  ) +
  scale_y_discrete(labels = activity_labels) +
  scale_fill_manual(
    values = freq_colors,
    breaks = levels(freq_diverging$freq_ordered), # Keeps legend in chronological order
    name   = "Frequency of Use"
  ) +
  labs(
    # title    = "Ocean Use by Primary Activity",
    x        = "Weighted Percentage of Respondents",
    y        = NULL
  ) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.y        = element_text(size = 22, color = "grey15"),
    axis.text.x        = element_text(size = 20, color = "grey15"),
    axis.title.x       = element_text(size = 22, face = "bold", margin = margin(t = 12)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 20),
    legend.title       = element_text(size = 21, face = "bold"),
    legend.key.size    = unit(1.1, "cm"),
    plot.title         = element_text(size = 26, face = "bold"),
    plot.subtitle      = element_text(size = 20, color = "grey40"),
    panel.grid.major.y = element_blank(),
    plot.margin        = margin(15, 20, 15, 15)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top"))

p_freq

ggsave("./doc/bab_activity_time_wide_focal_n_interaction.png", p_freq, width = 18, height = 8, units = "in", dpi = 300, bg = "white")