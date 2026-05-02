# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: graph activity data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# note in cases where write in text fits two categories, then it is assigned the first one that matches

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d0<-read_csv("./results/data_long6.csv")%>%
  mutate(response_id=ResponseId)%>%
  # select(response_id,QDesired_Time,QActual_Time,QImportant_Activities,QImportant_Activities_TEXT,QImportant_Activities_Most,QImportant_Activities_Most_TEXT,QTransport_Time,QActivity_Mentor,QActivity_Mentor_TEXT,
  # QActivity_Companion,QFishing,QFishing_Type,QDemographic_Home:QDemographic_Swimming,Version,Mechanism,EJ_Bin,Distance,Distance_Binned)%>%
  # mutate(QActual_Time4 = as.character(QActual_Time4), QImportant_Activities = as.character(QImportant_Activities), QImportant_Activities_Most = as.character(QImportant_Activities_Most)) %>%
  # mutate(QActual_Time=if_else(QActual_Time=="Less than once a year (i.e., rarely or never)","Less than once a year",QActual_Time))%>%
  glimpse()
names(d0)

# dealing with influencer bias ---------------

# method 1: weight influencer samples: ------------------------------------
# natural rate/influencer rate (see calcs in bab_fishing_most_table)
# Natural Rate (Rn): 0.04903678
# Influencer-Period Rate (Ri): 0.5882353
# weight<-0.04903678 / 0.5882353 
# 0.08336


# ratio
target_ratio <- 0.04903678 / 0.5882353

# calculate weights for all respondents
d1<-d0%>%
  mutate(
    weight_influencer = if_else(
      influencer_aw_b == 1, 
      target_ratio, 
      1.0, 
      missing = 1.0
    )
  )
  

# method 2: remove influencer samples -------------------
d2<-d0%>%
  filter(influencer_aw_b!=1)%>%
  glimpse()


# ----------------------------------
# -- Activity graphs - QImportant_Activities_Most ---------------------------


# VERSION 1: ALL DATA --------------------------------

# -- organize --
counts_activity <- d0 %>%
  filter(!is.na(QImportant_Activities_Most2))%>%
  filter(QImportant_Activities_Most2!="Choose not to answer")%>%
  mutate(
    QImportant_Activities_Most = fct_relevel(QImportant_Activities_Most2,
                     sort(unique(QImportant_Activities_Most2[QImportant_Activities_Most2 != "Another activity"])),
                     "Another activity"))%>%
  group_by(QImportant_Activities_Most2) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")



# all data
props_activity <- counts_activity %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()


# -- compare expected frequencies to observed values --
chi1 <- chisq.test(props_activity$n) # default = equal proportions
chi_stat1 <- unname(chi1$statistic)
chi_df1   <- unname(chi1$parameter)
chi_p1   <- chi1$p.value
fmt_p1 <- ifelse(chi_p1 < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001)) #  formatted p

resids1 <- chi1$stdres
data.frame(QImportant_Activities_Most2 = props_activity$QImportant_Activities_Most2, Residual = resids1)



# reorder based on percentage-------------
props_activity <- props_activity %>%
  mutate(
    # Reorder Activity by pct (descending)
    QImportant_Activities_Most2 = fct_reorder(QImportant_Activities_Most2, pct, .desc = TRUE),
    
    # Optional: If you want "Another activity" to always be last regardless of its %
    QImportant_Activities_Most2 = fct_relevel(QImportant_Activities_Most2, "Another activity", after = Inf)
  )


# -- graph --
ggplot(props_activity, aes(x = QImportant_Activities_Most2, y = pct, fill = QImportant_Activities_Most2)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_activity$pct) * 1.15)) +
  labs(
    x = "Most Important Activity",
    y = "Percent of Respondents",
    # title = "Distribution of MPA familiarity",
    subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
                      chi_df1, ") = ", round(chi_stat1, 2), ", p ", fmt_p1)) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),  # top margin (move x-label down)
    axis.title.y = element_text(margin = margin(r = 10)),   # right margin (move y-label left)
    plot.subtitle = element_text(
      color = "grey40",    
      hjust = 1 )         
  )+
  scale_fill_discrete_sequential(palette = "Teal")


ggsave("./doc/QImportant_Activities_Most_activity_biased.png",   width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)






# ----------------------------------------------------------------
# Version 2: weighted by influencer: --------------------------------------

# -- organize and reorder --
props_activity_weight <- d1 %>%
  filter(!is.na(QImportant_Activities_Most2), 
         QImportant_Activities_Most2 != "Choose not to answer") %>%
  group_by(QImportant_Activities_Most2) %>%
  summarise(n_weighted = sum(weight_influencer, na.rm = TRUE), .groups = "drop") %>%
  
  mutate(pct = n_weighted / sum(n_weighted)) %>%
  # NEW: Reorder by percentage
  mutate(QImportant_Activities_Most2 = fct_reorder(QImportant_Activities_Most2, pct, .desc = TRUE)) %>%
  # NEW: Keep "Another activity" at the end if it exists
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
  Residual = resids1
)



# -- graph --
ggplot(props_activity_weight, aes(x = QImportant_Activities_Most2, y = pct, fill = QImportant_Activities_Most2)) +
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(props_activity_weight$pct) * 1.2)) +
  labs(
    x = "Most Important Activity",
    y = "Percent of Respondents",
    subtitle = paste0("\u03C7\u00B2(", chi_df1_w, ") = ", round(chi_stat1_w, 2), ", p ", fmt_p1_w)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  ) +
  colorspace::scale_fill_discrete_sequential(palette = "Teal")


ggsave("./doc/QImportant_Activities_Most_activity_weights.png",   width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)







# QImportant_Activities --------------------
# Explode multi-select activity (QImportant_Activities) so multi-activity respondents count in each chosen activity ----
d4 <- d1 %>%
  mutate(QImportant_Activities = as.character(QImportant_Activities)) %>%
  tidyr::separate_rows(QImportant_Activities, sep = ",") %>%
  mutate(QImportant_Activities = stringr::str_trim(QImportant_Activities)) %>%
  filter(
    !is.na(QImportant_Activities), QImportant_Activities != "",
    !QImportant_Activities %in% c("Choose not to answer") 
  ) %>%
  # --- ADD IS_CONSUMPTIVE LOGIC HERE ---
  mutate(
    is_consumptive = if_else(
      QImportant_Activities == "Fishing or collecting food" | 
        str_detect(str_to_lower(QImportant_Activities_TEXT), "collect shell|glass|rock hunt"), 
      1, 0, missing = 0
    )
  ) %>%
  # -------------------------------------
mutate(
  QImportant_Activities = fct_relevel(QImportant_Activities,
                                      sort(unique(QImportant_Activities[QImportant_Activities != "Another activity"])),
                                      "Another activity")) %>%
  distinct(response_id, QImportant_Activities, .keep_all = TRUE) %>% # Keep the new flag
  glimpse()


# -- Activity graphs - QImportant_Activities ---------------------------

# -- organize --
# 1. Organize and calculate raw counts (Unweighted)
props_activity_raw <- d4 %>%
  filter(!is.na(QImportant_Activities)) %>%
  filter(QImportant_Activities != "Choose not to answer") %>%
  group_by(QImportant_Activities) %>%
  summarise(
    # Use standard n_distinct to count each person once per activity
    n_raw = n_distinct(response_id),
    is_consumptive = max(is_consumptive), 
    .groups = "drop"
  ) %>%
  mutate(
    # Percentages based on raw total mentions
    pct = n_raw / sum(n_raw),
    # Reorder by raw frequency
    QImportant_Activities = fct_reorder(QImportant_Activities, pct, .desc = TRUE),
    QImportant_Activities = fct_relevel(QImportant_Activities, "Another activity", after = Inf),
    is_consumptive = as.factor(is_consumptive)
  )

# 2. Chi-Squared Test (using raw counts)
chi_raw <- chisq.test(props_activity_raw$n_raw)
chi_stat_raw <- unname(chi_raw$statistic)
chi_df_raw   <- unname(chi_raw$parameter)
chi_p_raw    <- chi_raw$p.value
fmt_p_raw    <- ifelse(chi_p_raw < .001, "< 0.001", 
                       scales::number(chi_p_raw, accuracy = 0.001))

# 3. Create the Graph
ggplot(props_activity_raw, 
       aes(x = QImportant_Activities, y = pct, fill = is_consumptive)) +
  geom_col(show.legend = TRUE) + 
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(props_activity_raw$pct) * 1.15)) +
  scale_fill_manual(
    values = c("0" = "#7fb8b4", "1" = "#d43d51"), 
    labels = c("0" = "Non-Consumptive", "1" = "Consumptive"),
    name = "Activity Type"
  ) +
  labs(
    x = "Activities",
    y = "Percent of Total Mentions",
    subtitle = paste0("\u03C7\u00B2(", chi_df_raw, ") = ", 
                      round(chi_stat_raw, 2), ", p ", fmt_p_raw)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "top",
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

ggsave("./doc/QImportant_Activities_activity.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)




# ------------------------------------------------------------------------
# --- Weighted -----------------------------------------------------------

# 1. Organize and calculate weighted counts for all activities
props_activity_QImportant_Activities <- d4 %>%
  filter(!is.na(QImportant_Activities)) %>%
  filter(QImportant_Activities != "Choose not to answer") %>%
  group_by(QImportant_Activities) %>%
  summarise(
    # Sum weights instead of n() to maintain bias correction
    n_weighted = sum(weight_influencer, na.rm = TRUE),
    # Carry over the consumptive flag (use max or first since it's the same per activity)
    is_consumptive = max(is_consumptive), 
    .groups = "drop"
  ) %>%
  mutate(
    pct = n_weighted / sum(n_weighted),
    # Reorder Activity by percentage (highest to lowest)
    QImportant_Activities = fct_reorder(QImportant_Activities, pct, .desc = TRUE),
    # Move "Another activity" to the end if present
    QImportant_Activities = fct_relevel(QImportant_Activities, "Another activity", after = Inf),
    # Factor for discrete coloring
    is_consumptive = as.factor(is_consumptive)
  )

# 2. Chi-Squared Test (using weighted counts)
chi1_QImportant_Activities <- chisq.test(props_activity_QImportant_Activities$n_weighted)
chi_stat1_QImportant_Activities <- unname(chi1_QImportant_Activities$statistic)
chi_df1_QImportant_Activities   <- unname(chi1_QImportant_Activities$parameter)
chi_p1_QImportant_Activities    <- chi1_QImportant_Activities$p.value
fmt_p1_QImportant_Activities    <- ifelse(chi_p1_QImportant_Activities < .001, "< 0.001", 
                                          scales::number(chi_p1_QImportant_Activities, accuracy = 0.001))

# 3. Create the Graph
ggplot(props_activity_QImportant_Activities, 
       aes(x = QImportant_Activities, y = pct, fill = is_consumptive)) +
  geom_col(show.legend = TRUE) + 
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, max(props_activity_QImportant_Activities$pct) * 1.15)) +
  # Custom Manual Scale: Teal for Non-Consumptive, Red for Consumptive
  scale_fill_manual(
    values = c("0" = "#7fb8b4", "1" = "#d43d51"), 
    labels = c("0" = "Non-Consumptive", "1" = "Consumptive"),
    name = "Activity Type"
  ) +
  labs(
    x = "Activities",
    y = "Percent of Total Mention",
    subtitle = paste0("\u03C7\u00B2(", chi_df1_QImportant_Activities, ") = ", 
                      round(chi_stat1_QImportant_Activities, 2), ", p ", fmt_p1_QImportant_Activities)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "top",
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

# Save
# ggsave("./doc/QImportant_Activities_activity.png", width = 15, height = 8, dpi = 300, bg = "white")