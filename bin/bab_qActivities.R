# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: graph activity data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(ggpattern)
library(stringr)


# note in cases where write in text fits two categories, then it is assigned the first one that matches

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d00<-read_csv("./doc/fishing_influencer_summary.csv")%>%
  glimpse()
d00


d0<-read_csv("./results/data_long8.csv")%>%
  mutate(response_id=ResponseId)%>%
  #fix swimming 
  mutate(
    QImportant_Activities2 = str_replace_all(
      QImportant_Activities2, 
      "Swimming or bodysurfing", 
      "Swimming/Bodysurfing"))%>%
  # select(response_id,QDesired_Time,QActual_Time,QImportant_Activities2,QImportant_Activities2_TEXT,QImportant_Activities2_Most,QImportant_Activities_Most_TEXT,QTransport_Time,QActivity_Mentor,QActivity_Mentor_TEXT,
  # QActivity_Companion,QFishing,QFishing_Type,QDemographic_Home:QDemographic_Swimming,Version,Mechanism,EJ_Bin,Distance,Distance_Binned)%>%
  # mutate(QActual_Time4 = as.character(QActual_Time4), QImportant_Activities = as.character(QImportant_Activities), QImportant_Activities_Most = as.character(QImportant_Activities_Most)) %>%
  # mutate(QActual_Time=if_else(QActual_Time=="Less than once a year (i.e., rarely or never)","Less than once a year",QActual_Time))%>%
  glimpse()
names(d0)

d0

# dealing with influencer bias ---------------
# weighting people who said fishing was an important activity during the two month influencer period
# not enough variation to use fishing as most important activity
# ---------------------------------------------------

# natural rate of fishing as most important activity
# Removed Influencer Period - Any Fishing - Online
nrm<-d00$percent[10]/100 #5.9
nrm

# influencer rate of fishing as most important activity
# Influencer Period Only - Any Fishing - Online
nim<-d00$percent[6]/100 #82.7
nim

# natural rate of fishing as any important activity
# Removed Influencer Period - Any Fishing - Online 
nra<-d00$percent[9]/100 #17.3
nra

# influencer rate of fishing as any important activity
# Influencer Period Only - Any Fishing - Online
nia<-d00$percent[5]/100 #100
nia


# weight influencer samples: dataset = most important activity ------------------------------------

# ratio for most anyone who fishes = no influencer (online)/influencer only (online)
target_ratio_any <- nra / nia

# ratio for people who stated fishing is most important activity = no influencer/influencer
target_ratio_most <- nrm / nim

# calculate weights for all respondents vs fishing as the most important activity
d1<-d0%>%
  mutate(
    weight_influencer_most = if_else(
      influencer_any_b == 1, 
      target_ratio_most, 
      1.0, 
      missing = 1.0
  ),
  
# calculate weights for all respondents vs fishing any important activity
weight_influencer_all = if_else(
  influencer_any_b == 1, 
  target_ratio_any, 
  1.0, 
  missing = 1.0
)
) %>%
  glimpse()
  

# remove influencer samples -------------------
d2<-d0%>%
  filter(influencer_any_b!=1)%>%
  glimpse()


# ----------------------------------
# -- Activity graphs - QImportant_Activities_Most ---------------------------


# VERSION 1: ALL DATA --------------------------------
# -- organize --
counts_activity <- d0 %>%
  filter(!is.na(QImportant_Activities_Most2))%>% # most 2 assigns Another Activity to categories
  filter(QImportant_Activities_Most2!="Choose not to answer")%>%
  mutate(
    QImportant_Activities_Most2 = fct_relevel(QImportant_Activities_Most2,
                                              sort(unique(QImportant_Activities_Most2[QImportant_Activities_Most2 != "Another activity"])),
                                              "Another activity"))%>%
  group_by(QImportant_Activities_Most2) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")


# ----------------------------------------------------------------
# ACCOUNTING FOR INFLUENCER --------------- --------------------------------------

# -- organize and reorder --
props_activity_weight <- d1 %>%
  filter(!is.na(QImportant_Activities_Most2), 
         QImportant_Activities_Most2 != "Choose not to answer") %>%
  group_by(QImportant_Activities_Most2) %>%
  summarise(n_weighted = sum(weight_influencer_most, na.rm = TRUE), .groups = "drop") %>%
  
  mutate(pct = n_weighted / sum(n_weighted)) %>%
  # Reorder by percentage
  mutate(QImportant_Activities_Most2 = fct_reorder(QImportant_Activities_Most2, pct, .desc = TRUE)) %>%
  # Keep "Another activity" at the end
  mutate(QImportant_Activities_Most2 = fct_relevel(QImportant_Activities_Most2, "Another activity", after = Inf))


# -- compare expected frequencies to observed weighted values --
chi1_w <- chisq.test(props_activity_weight$n_weighted) 

chi_stat1_w <- unname(chi1_w$statistic)
chi_df1_w   <- unname(chi1_w$parameter)
chi_p1_w    <- chi1_w$p.value
fmt_p1_w <- ifelse(chi_p1_w < .001, "< 0.001", scales::number(chi_p1_w, accuracy = 0.001))

# Residuals based on weighted expectations
resids1_w <- chi1_w$stdres
weighted_residuals <- data.frame(
  Activity = props_activity_weight$QImportant_Activities_Most2, 
  Residual = resids1_w
)



# # -- graph --
# for labeling
props_activity_weight$consumptive_b<-"Non-consumptive"
props_activity_weight$consumptive_b[props_activity_weight$QImportant_Activities_Most2=="Fishing or collecting food"]<-"Consumptive"
props_activity_weight$consumptive_b[props_activity_weight$QImportant_Activities_Most2=="Another Activity"]<-"Consumptive"

# 
# ggplot(props_activity_weight, aes(x = QImportant_Activities_Most2, y = pct, fill = consumptive_b)) +
#   # 1. Enable the legend here
#   geom_col(show.legend = TRUE) + 
#   geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
#             vjust = -0.4, size = 3.3) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
#                      limits = c(0, max(props_activity_weight$pct) * 1.2)) +
#   labs(
#     x = "Most Important Activity",
#     y = "Percent of Respondents",
#     subtitle = paste0("\u03C7\u00B2(", chi_df1_w, ") = ", round(chi_stat1_w, 2), ", p ", fmt_p1_w)
#   ) +
#   theme_minimal(base_size = 18) +
#   scale_fill_manual(
#     values = c("Non-consumptive" = "#206161", "Consumptive" = "#d43d51"), 
#     labels = c("Non-Consumptive" = "Non-Consumptive", "Consumptive" = "Consumptive"),
#     name = "Activity Type"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
#     legend.position = "right",
#     # Shrink the legend text and title
#     legend.text = element_text(size = 12), 
#     legend.title = element_text(size = 12, face = "bold"),
#     # Keep your subtitle styling
#     plot.subtitle = element_text(color = "grey40", hjust = 1)
#   )
# 
#   
# --------------------------------
# 1. Create the pattern vector for this specific dataframe
all_acts_w <- unique(props_activity_weight$QImportant_Activities_Most2)
pattern_values_w <- setNames(rep("none", length(all_acts_w)), all_acts_w)
pattern_values_w["Another activity"] <- "stripe"

# 2. Build the plot with patterns
ggplot(props_activity_weight, 
       aes(x = QImportant_Activities_Most2, y = pct, 
           fill = consumptive_b,
           pattern = QImportant_Activities_Most2)) + # Map pattern to the activity
  
  geom_col_pattern(
    show.legend = TRUE,
    pattern_color = "transparent", 
    pattern_fill = "#d43d51",      # The red stripe color
    pattern_density = 0.4,         
    pattern_spacing = 0.02,
    color = NA                     # No black outlines
  ) + 
  
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.3) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(props_activity_weight$pct) * 1.2)) +
  
  scale_fill_manual(
    values = c("Non-consumptive" = "#79ABE2", "Consumptive" = "#d43d51"), ##206161
    name = "Activity Type"
  ) +
  
  scale_pattern_manual(
    values = pattern_values_w,
    guide = "none"
  ) +
  
  # Keep the legend solid (no stripes in the legend boxes)
  guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
  
  labs(
    x = "Most Important Activity",
    y = "Percent of Respondents",
    subtitle = paste0("\u03C7\u00B2(", chi_df1_w, ") = ", round(chi_stat1_w, 2), ", p ", fmt_p1_w)
  ) +
  
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

ggsave("./doc/QImportant_Activities_Most_activity_w.png",   width = 12, height = 10,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)







# QImportant_Activities --------------------
# Explode multi-select activity (QImportant_Activities) so multi-activity respondents count in each chosen activity ----
d4 <- d1 %>%
  mutate(QImportant_Activities2 = as.character(QImportant_Activities2)) %>%
  tidyr::separate_rows(QImportant_Activities2, sep = ",") %>%
  mutate(QImportant_Activities2 = stringr::str_trim(QImportant_Activities2)) %>%
  filter(
    !is.na(QImportant_Activities2), QImportant_Activities2 != "",
    !QImportant_Activities2 %in% c("Choose not to answer") 
  ) %>%
  # --- ADD IS_CONSUMPTIVE LOGIC HERE ---
  mutate(
    is_consumptive = if_else(
      QImportant_Activities2 == "Fishing or collecting food" | 
        str_detect(str_to_lower(QImportant_Activities_TEXT), "collect shell|glass|rock hunt"), 
      1, 0, missing = 0
    )
  ) %>%
  # -------------------------------------
mutate(
  QImportant_Activities2 = fct_relevel(QImportant_Activities2,
                                      sort(unique(QImportant_Activities2[QImportant_Activities2 != "Another activity"])),
                                      "Another activity")) %>%
  distinct(response_id, QImportant_Activities2, .keep_all = TRUE) %>% # Keep the new flag
  
  # calculate categories
  # add columns for sub-categories ----------
# need to check these before use them 
mutate(
is_spiritual_cultural = if_else(
  QImportant_Activities_Cleaned == "Cultural or religious ceremonies" | 
    str_detect(QImportant_Activities2, fixed("Cultural or religious ceremonies")) |
    str_detect(str_to_lower(QImportant_Activities_TEXT), "spirit|religious|meditation|drum circle|singing|yoga|lakota|prayer|offering|ancestor|dance|ceremon|indigenous|native"),
  "Spiritual/Cultural Use", "Non-spiritual use", missing = "Non-spiritual use"
),

# B. SCIENTIFIC RESEARCH FLAG
is_research = if_else(
  QImportant_Activities_Cleaned == "Scientific Research" | 
    str_detect(str_to_lower(QImportant_Activities_TEXT), "research|monitor|science|field|condor|salmon|whale|clam survey|intertidal work|transect|ecology|marine mammal"),
  "Research/Monitoring", "Non-research use", missing = "Non-research"
),

# C. DOG FLAG
is_dog_activity = if_else(
  str_detect(str_to_lower(QImportant_Activities_TEXT), "dog|puppy|dig|perr|犬|狗") | 
    str_detect(str_to_lower(comment_clean), "dog|puppy|canine|pet|perr|犬|狗"), 
  "Dog related", "No dog mentioned", missing = "No dog mentioned"
),

# D. CIVIC ENGAGEMENT FLAG
is_civic_engagement = if_else(
  QImportant_Activities_Cleaned == "Volunteering" |
    str_detect(QImportant_Activities2, fixed("Volunteering")) |
    str_detect(str_to_lower(QImportant_Activities_TEXT), "activism|restoration|clean up|stewardship|land back"),
  "Civic/Stewardship use", "Non-civic use", missing = "Non-civic use"
),

# E. CONSUMPTIVE FLAG
is_consumptive = if_else(
  QImportant_Activities_Cleaned == "Fishing or collecting food" | 
    str_detect(QImportant_Activities2, fixed("Fishing or collecting food")) |
    str_detect(str_to_lower(QImportant_Activities_TEXT), "collect shell|glass|rock hunt|agetes|渔|钓"), 
  "Consumptive", "Non-consumptive", missing = "Non-consumptive")
    ) %>%
  glimpse()
glimpse(d4)

# ------------------------------------------------------------------------
# --- Weighted -----------------------------------------------------------

# 1. Organize and calculate weighted counts for all activities
props_activity_QImportant_Activities2 <- d4 %>%
  filter(!is.na(QImportant_Activities2)) %>%
  filter(QImportant_Activities2 != "Choose not to answer") %>%
  group_by(QImportant_Activities2) %>%
  summarise(
    # Sum weights instead of n() to maintain bias correction
    n_weighted = sum(weight_influencer_all, na.rm = TRUE),
    # Carry over the consumptive flag (use max or first since it's the same per activity)
    is_consumptive = max(is_consumptive), 
    .groups = "drop"
  ) %>%
  mutate(
    pct = n_weighted / sum(n_weighted),
    # Reorder Activity by percentage (highest to lowest)
    QImportant_Activities2 = fct_reorder(QImportant_Activities2, pct, .desc = TRUE),
    # Move "Another activity" to the end if present
    QImportant_Activities2 = fct_relevel(QImportant_Activities2, "Another activity", after = Inf),
    # Factor for discrete coloring
    is_consumptive = as.factor(is_consumptive)
  )

# 2. Chi-Squared Test (using weighted counts)
chi1_QImportant_Activities2 <- chisq.test(props_activity_QImportant_Activities2$n_weighted)
chi_stat1_QImportant_Activities2 <- unname(chi1_QImportant_Activities2$statistic)
chi_df1_QImportant_Activities2   <- unname(chi1_QImportant_Activities2$parameter)
chi_p1_QImportant_Activities2    <- chi1_QImportant_Activities2$p.value
fmt_p1_QImportant_Activities2    <- ifelse(chi_p1_QImportant_Activities2 < .001, "< 0.001", 
                                          scales::number(chi_p1_QImportant_Activities2, accuracy = 0.001))

# ----------------------
# calc values for striped "Another activity" -------------

# unique activities 
all_activities <- unique(props_activity_QImportant_Activities2$QImportant_Activities2)

# vector where everything is "none"
pattern_values <- setNames(rep("none", length(all_activities)), all_activities)

# set "Another activity" to "stripe"
pattern_values["Another activity"] <- "stripe"



# check --------------
"Another activity" %in% props_activity_QImportant_Activities2$QImportant_Activities2
  
# graph ----------------
ggplot(props_activity_QImportant_Activities2, 
       aes(x = QImportant_Activities2, y = pct, 
           fill = is_consumptive,
           pattern = QImportant_Activities2)) + # pattern allows for stripes
  
  geom_col_pattern(
    show.legend = TRUE,
    pattern_color = "transparent", # Change 'none' to 'transparent'
    pattern_fill = "#d43d51",      # Red stripes
    pattern_density = 0.4,         
    pattern_spacing = 0.02,
    color = NA                # Keeps a clean border around the bar
  ) + 
  
  scale_fill_manual(
    values = c("Non-consumptive" = "#007EB3", "Consumptive" = "#d43d51"), 
    name = "Activity Type"
  ) +
  
  scale_pattern_manual(
    values = pattern_values,
    guide = "none"
  ) +
  
  # This keeps your legend icons solid (no stripes in the legend)
  guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
  
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.3) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(props_activity_QImportant_Activities2$pct) * 1.15)) +
  
  labs(
    x = "Activity",
    y = "Percent of Activities",
    subtitle = paste0("\u03C7\u00B2(", chi_df1_QImportant_Activities2, ") = ", 
                      round(chi_stat1_QImportant_Activities2, 2), ", p ", fmt_p1_QImportant_Activities2)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

ggsave("./doc/QImportant_Activities2_activity_w2.png",  width = 12, height = 10,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)

