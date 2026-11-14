# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# Goal: Graph activity data (Focal Counties, raw data)
# note: 10 agate/shell collectors represent < 0.1% of total activities

# ----------------------------------------------------------
# Load libraries -------------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(ggpattern)
library(stringr)

# --------------------------------------------------------------------------
# Load data ----------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1 <- read_csv("./results/data_final_weighted_focal.csv", guess_max = Inf) %>%
  glimpse()

# --------------------------------------------------------------------------
# Check & Define Labels for Plotting ---------------------------------------
all_unique_acts <- unique(c(
  str_trim(unlist(str_split(d1$q_activity_most3, ","))),
  str_trim(unlist(str_split(d1$q_activity_all3, ",")))
))

all_unique_acts <- all_unique_acts[!is.na(all_unique_acts) & !all_unique_acts %in% c("", "Choose not to answer")]

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

# Diagnostic: Verify 100% of categories in your dataset have clean plot labels
missing_labels <- setdiff(all_unique_acts, names(activity_labels))
if (length(missing_labels) > 0) {
  message("WARNING: Update 'activity_labels' to include these categories:")
  print(missing_labels)
} else {
  message("All activity categories matched cleanly!")
}

# ==========================================================================
# GRAPH 1: Most Important Activity (q_activity_most3)
# ==========================================================================

# -- Organize and reorder (preserving consumptive vs. non-consumptive splits) --
props_activity_most <- d1 %>%
  mutate(q_activity_most3 = as.character(q_activity_most3)) %>%
  tidyr::separate_rows(q_activity_most3, sep = ",") %>%
  mutate(q_activity_most3 = str_trim(q_activity_most3)) %>%
  filter(
    !is.na(q_activity_most3), 
    q_activity_most3 != "",
    q_activity_most3 != "Choose not to answer"
  ) %>%
  distinct(response_id, q_activity_most3, .keep_all = TRUE) %>%
  mutate(
    consumptive_label = if_else(
      q_activity_consumptive_most_b == 1,
      "Consumptive", "Non-consumptive",
      missing = "Non-consumptive"
    )
  ) %>%
  group_by(q_activity_most3, consumptive_label) %>%
  summarise(n = n_distinct(response_id), .groups = "drop") %>%
  mutate(
    pct = n / sum(n),
    # Order bars by TOTAL category share (summing consumptive + non-consumptive slices)
    q_activity_most3 = fct_reorder(q_activity_most3, pct, .fun = sum, .desc = TRUE),
    consumptive_label = factor(consumptive_label, levels = c("Non-consumptive", "Consumptive"))
  )

# Keep "Another activity" at the far right if present
if ("Another activity" %in% levels(props_activity_most$q_activity_most3)) {
  props_activity_most <- props_activity_most %>%
    mutate(q_activity_most3 = fct_relevel(q_activity_most3, "Another activity", after = Inf))
}

# -- Category totals (for clean bar-top labels and Chi-Squared test) --
totals_activity_most <- props_activity_most %>%
  group_by(q_activity_most3) %>%
  summarise(
    total_n   = sum(n),
    total_pct = sum(pct),
    .groups   = "drop"
  )

# -- Chi-Squared Test (on category totals) --
chi1 <- chisq.test(totals_activity_most$total_n) 
chi_stat1 <- unname(chi1$statistic)
chi_df1   <- unname(chi1$parameter)
chi_p1    <- chi1$p.value
fmt_p1    <- ifelse(chi_p1 < .001, "< 0.001", scales::number(chi_p1, accuracy = 0.001))

residuals_most <- data.frame(
  Activity = totals_activity_most$q_activity_most3, 
  Residual = chi1$stdres
)

# -- Pattern setup for "Another activity" (safe even if absent) --
all_acts_most <- unique(props_activity_most$q_activity_most3)
pattern_values_most <- setNames(rep("none", length(all_acts_most)), all_acts_most)
if ("Another activity" %in% names(pattern_values_most)) {
  pattern_values_most["Another activity"] <- "stripe"
}

# -- Build Plot 1 --
p1 <- ggplot(props_activity_most, 
             aes(x = q_activity_most3, y = pct, 
                 fill = consumptive_label,
                 pattern = q_activity_most3)) +
  geom_col_pattern(
    position = "stack",
    show.legend = TRUE,
    pattern_color = "transparent", 
    pattern_fill = "#d43d51",      
    pattern_density = 0.4,         
    pattern_spacing = 0.02,
    color = NA                     
  ) + 
  # Place one total percentage label above each bar
  geom_text(
    data = totals_activity_most,
    aes(x = q_activity_most3, y = total_pct, label = scales::percent(total_pct, accuracy = 0.1)),
    inherit.aes = FALSE,
    vjust = -0.4, size = 3.3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(totals_activity_most$total_pct) * 1.2)) +
  scale_x_discrete(labels = activity_labels) +
  scale_fill_manual(
    values = c("Non-consumptive" = "#79ABE2", "Consumptive" = "#d43d51"),
    name = "Activity Type"
  ) +
  scale_pattern_manual(
    values = pattern_values_most,
    guide = "none"
  ) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
  labs(
    x = "Most Important Activity",
    y = "Percent of Respondents",
    subtitle = paste0("\u03C7\u00B2(", chi_df1, ") = ", round(chi_stat1, 2), ", p ", fmt_p1)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = c(0.95, 0.95),        
    legend.justification = c("right", "top"), 
    legend.text = element_text(size = 15), 
    legend.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

p1

ggsave("./doc/bab_activities_most_focal.png", p1, width = 12, height = 10, units = "in", dpi = 300, bg = "white")
ggsave("./doc/bab_activities_most_wide_focal.png", p1, width = 14, height = 10, units = "in", dpi = 300, bg = "white")


# -------------------------------------------------------------------------------
# GRAPH 2: All Important Activities (q_activity_all3 - multiple activities) -----
# -------------------------------------------------------------------------------
names(d1)[str_detect(names(d1), "text|TEXT|consumptive")]

consumptive_writein_regex <- regex(
  "beach ?combing|collect(ing)? shell|finding shells|shell hunting|finding glass|sea ?glass|glass|rock hunt(ing)?|rock exploring|agetes?|agates?|渔|钓",
  ignore_case = TRUE
)

# -- Explode multi-select and assign row-level consumptive flag -----
d_activities <- d1 %>%
  mutate(q_activity_all3 = as.character(q_activity_all3)) %>%
  tidyr::separate_rows(q_activity_all3, sep = ",") %>%
  mutate(q_activity_all3 = stringr::str_trim(q_activity_all3)) %>%
  filter(
    !is.na(q_activity_all3), 
    q_activity_all3 != "",
    q_activity_all3 != "Choose not to answer"
  ) %>%
  distinct(response_id, q_activity_all3, .keep_all = TRUE) %>%
  mutate(
    # Check if either write-in text box contains consumptive collecting terms
    has_consumptive_writein = (
      (!is.na(QImportant_Activities_TEXT) & str_detect(QImportant_Activities_TEXT, consumptive_writein_regex)) |
      (if ("QImportant_Activities_Most_TEXT" %in% names(.)) {
        !is.na(QImportant_Activities_Most_TEXT) & str_detect(QImportant_Activities_Most_TEXT, consumptive_writein_regex)
      } else {
        FALSE
      })
    ),
    
    # Assign row-level consumptive flag (1 = consumptive, 0 = non-consumptive)
    # Only tags Fishing/collecting food, Beachcombing, or Nature Observation rows backed by collecting write-ins
    q_activity_consumptive_b = if_else(
      q_activity_all3 %in% c("Fishing or collecting food") |
        (q_activity_all3 == "Nature Observing/Photographing/Education/Research" & has_consumptive_writein),
      1L, 0L
    ),
    
    consumptive_label = factor(
      if_else(q_activity_consumptive_b == 1L, "Consumptive", "Non-consumptive"),
      levels = c("Non-consumptive", "Consumptive")
    )
  ) %>%
  select(-has_consumptive_writein)

# -- Summarize by activity and consumptive status --
props_activity_all <- d_activities %>%
  group_by(q_activity_all3, consumptive_label) %>%
  summarise(
    n = n_distinct(response_id),
    .groups = "drop"
  ) %>%
  mutate(
    pct = n / sum(n),
    # Order bars by TOTAL activity share (summing consumptive + non-consumptive slices)
    q_activity_all3 = fct_reorder(q_activity_all3, pct, .fun = sum, .desc = TRUE)
  )

if ("Another activity" %in% levels(props_activity_all$q_activity_all3)) {
  props_activity_all <- props_activity_all %>%
    mutate(q_activity_all3 = fct_relevel(q_activity_all3, "Another activity", after = Inf))
}

# -- Category totals (for clean bar-top labels and Chi-Squared test) --
totals_activity_all <- props_activity_all %>%
  group_by(q_activity_all3) %>%
  summarise(
    total_n   = sum(n),
    total_pct = sum(pct),
    .groups   = "drop"
  )

# -- Chi-Squared Test (on category totals) --
chi2 <- chisq.test(totals_activity_all$total_n)
chi_stat2 <- unname(chi2$statistic)
chi_df2   <- unname(chi2$parameter)
chi_p2    <- chi2$p.value
fmt_p2    <- ifelse(chi_p2 < .001, "< 0.001", scales::number(chi_p2, accuracy = 0.001))

# -- Pattern setup for "Another activity" --
all_activities <- unique(props_activity_all$q_activity_all3)
pattern_values <- setNames(rep("none", length(all_activities)), all_activities)
if ("Another activity" %in% names(pattern_values)) {
  pattern_values["Another activity"] <- "stripe"
}

# -- Build Plot 2 --
p2 <- ggplot(props_activity_all, 
             aes(x = q_activity_all3, y = pct, 
                 fill = consumptive_label,
                 pattern = q_activity_all3)) +
  geom_col_pattern(
    position = "stack",
    show.legend = TRUE,
    pattern_color = "transparent", 
    pattern_fill = "#d43d51",      
    pattern_density = 0.4,         
    pattern_spacing = 0.02,
    color = NA                     
  ) + 
  scale_fill_manual(
    values = c("Non-consumptive" = "#007EB3", "Consumptive" = "#d43d51"), 
    name = "Activity Type"
  ) +
  scale_pattern_manual(
    values = pattern_values,
    guide = "none"
  ) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
  geom_text(
    data = totals_activity_all,
    aes(x = q_activity_all3, y = total_pct, label = scales::percent(total_pct, accuracy = 0.1)),
    inherit.aes = FALSE,
    vjust = -0.4, size = 3.3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(totals_activity_all$total_pct) * 1.15)) +
  scale_x_discrete(labels = activity_labels) +
  labs(
    x = "Activity",
    y = "Percent of Total Activities",
    subtitle = paste0("\u03C7\u00B2(", chi_df2, ") = ", round(chi_stat2, 2), ", p ", fmt_p2)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = c(0.95, 0.95),        
    legend.justification = c("right", "top"), 
    legend.text = element_text(size = 15), 
    legend.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

p2

ggsave("./doc/bab_activity_focal.png", p2, width = 12, height = 10, units = "in", dpi = 300, bg = "white")
ggsave("./doc/bab_activity_wide_focal.png", p2, width = 18, height = 8, units = "in", dpi = 300, bg = "white")
