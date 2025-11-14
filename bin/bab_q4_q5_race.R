# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset cleaned data for graphing and making long version - ecosystem services


# consider:  Using X2 - consider Spearman rank correlation coefficient values that were the basis for comparison, with only values corresponding to highly significant comparisons ( p < 0.01, following the application of a Bonferroni adjustment to correct for the family-wise error rate) displayed.
# ----------------------------------------------------------
# load libraries ######-------------------------------------


library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long5.csv")%>%
  select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,Q17:Q32_4,Q20a_5,Q24,Q25,Q27,Q28,Q31b,YEAR)%>%
  mutate(Q24 = as.character(Q24), Q4 = as.character(Q4), Q5 = as.character(Q5)) %>%
  mutate(Q2=if_else(Q2=="Less than once a year (i.e., rarely or never)","Less than once a year",Q2))%>%
  glimpse()
d1


levels(factor(d1$Q24)) # check race categories


# organizing activity categories
activities <- c(
  "Swimming or bodysurfing",
  "Group or family gatherings or activities",
  "Meditation/Reading/Relaxing",
  "Enjoy the views/sunsets from car",
  "Fishing or collecting food",
  "Beach games or sports",
  "Surfing",
  "Observing or photographing nature",
  "Bicycling/Roller skating/Skateboarding",
  "Snorkeling/Scuba Diving",
  "Walking or running",
  "Sailing/Boating",
  "Paddleboarding/Kiteboarding/Kayaking",
  "Paid work",
  "Festivals",
  "Cultural or religious ceremonies",
  "Another activity",
  "Volunteering"
)

activities_ordered <- c(
  sort(activities[activities != "Another activity"]),
  "Another activity"
)

activities_ordered




# -- select Activity and Frequency questions --
d3<-d1%>%
  # filter(Q17!=4) %>% # error - unsure of source
  select(response_id,Q1,Q2,Q4,Q5)%>%
  glimpse()

unique(d3$Q1)
unique(d3$Q2)
unique(d3$Q4)
unique(d3$Q5)





# ----------------------------------
# -- Activity graphs - Q5 ---------------------------

# # -- organize --
# counts_activity <- d3 %>%
#   filter(!is.na(Q5))%>%
#   filter(Q5!="Choose not to answer")%>%
#   group_by(Q5) %>%
#   summarise(n = n_distinct(response_id), .groups = "drop")
# 
# props_activity <- counts_activity %>%
#   mutate(pct = n / sum(n)) %>%
#   ungroup()
# 
# 
# # -- compare expected frequencies to observed values --
# chi1 <- chisq.test(props_activity$n) # default = equal proportions
# chi_stat1 <- unname(chi1$statistic)
# chi_df1   <- unname(chi1$parameter)
# chi_p1   <- chi1$p.value
# fmt_p1 <- ifelse(chi_p1 < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001)) #  formatted p
# 
# resids1 <- chi1$stdres
# data.frame(Q5 = props_activity$Q5, Residual = resids1)
# 
# 
# # -- graph --
# ggplot(props_activity, aes(x = Q5, y = pct, fill = Q5)) +
#   geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
#   geom_text(aes(label = percent(pct, accuracy = 0.1)),
#             vjust = -0.4, size = 3.8) +
#   ylim(0,.35)+
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_activity$pct) * 1.15)) +
#   labs(
#     x = "Most Important Activity",
#     y = "Percent of Respondents",
#     # title = "Distribution of MPA familiarity",
#     subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
#                       chi_df1, ") = ", round(chi_stat1, 2), ", p ", fmt_p1)) +
#   theme_minimal(base_size = 20) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
#     strip.text = element_text(face = "bold"),
#     axis.title.x = element_text(margin = margin(t = 10)),  # top margin (move x-label down)
#     axis.title.y = element_text(margin = margin(r = 10)),   # right margin (move y-label left)
#     plot.subtitle = element_text(
#       color = "grey40",    
#       hjust = 1 )         
#   )+
#   scale_fill_discrete_sequential(palette = "Teal")
# 
# 
# ggsave("./doc/Q5_activity.png",   width = 15, height = 7,     # size in inches
#   units = "in",              # "in", "cm", or "mm"
#   dpi = 300,                 # resolution (300+ for publication quality)
#   bg = "white"               # background color (use "transparent" if needed)
# )
# 
# 
# 
# # Q4 --------------------
# # Explode multi-select activity (Q4) so multi-activity respondents count in each chosen activity ----
# d4 <- d3 %>%
#   mutate(Q4 = as.character(Q4)) %>%
#   tidyr::separate_rows(Q4, sep = ",") %>%
#   mutate(Q4 = stringr::str_trim(Q4)) %>%
#   filter(
#     !is.na(Q4), Q4 != "",
#     !Q4 %in% c("Choose not to answer") #, "Another activity"
#   ) %>%
#   distinct(response_id, Q4) %>%               # avoid duplicates
#   glimpse()
# 
# 
# # -- Activity graphs - Q4 ---------------------------
# 
# # -- organize --
# counts_activity_q4 <- d4 %>%
#   filter(!is.na(Q4))%>%
#   filter(Q4!="Choose not to answer")%>%
#   group_by(Q4) %>%
#   summarise(n = n_distinct(response_id), .groups = "drop")
# 
# props_activity_q4 <- counts_activity_q4 %>%
#   mutate(pct = n / sum(n)) %>%
#   ungroup()
# 
# 
# # -- compare expected frequencies to observed values --
# chi1_q4 <- chisq.test(props_activity_q4$n) # default = equal proportions
# chi_stat1_q4 <- unname(chi1_q4$statistic)
# chi_df1_q4   <- unname(chi1_q4$parameter)
# chi_p1_q4   <- chi1_q4$p.value
# fmt_p1_q4 <- ifelse(chi_p1_q4 < .001, "< 0.001", scales::number(chi_p_q4, accuracy = 0.001)) #  formatted p
# 
# resids1_q4 <- chi1_q4$stdres
# data.frame(Q4 = props_activity_q4$Q4, Residual = resids1_q4)
# 
# 
# # -- graph --
# ggplot(props_activity_q4, aes(x = Q4, y = pct, fill = Q4)) +
#   geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
#   geom_text(aes(label = percent(pct, accuracy = 0.1)),
#             vjust = -0.4, size = 3.8) +
#   ylim(0,.35)+
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_activity$pct) * 1.15)) +
#   labs(
#     x = "Activities",
#     y = "Percent of Respondents",
#     subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
#                       chi_df1_q4, ") = ", round(chi_stat1_q4, 2), ", p ", fmt_p1_q4)) +
#   theme_minimal(base_size = 20) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
#     strip.text = element_text(face = "bold"),
#     axis.title.x = element_text(margin = margin(t = 10)),  # top margin (move x-label down)
#     axis.title.y = element_text(margin = margin(r = 10)),   # right margin (move y-label left)
#     plot.subtitle = element_text(
#       color = "grey40",    
#       hjust = 1 )         
#   )+
#   scale_fill_discrete_sequential(palette = "Teal")
# 
# 
# ggsave("./doc/Q4_activity.png",   width = 15, height = 7,     # size in inches
#        units = "in",              # "in", "cm", or "mm"
#        dpi = 300,                 # resolution (300+ for publication quality)
#        bg = "white"               # background color (use "transparent" if needed)
# )



# heatmap for activities from Q5 -------------------

# -- select Activity and Frequency questions, plus demographics --
d5<-d1%>%
  select(response_id,Q1,Q2,Q5,Q24,Q25,Q27,Q28,Q31b)%>%
  mutate(Q2=if_else(Q2=="Less than once a year (i.e., rarely or never)","Less than once a year",Q2))%>%
  filter(Q5!="Choose not to answer") %>%
  glimpse()

unique(d5$Q5)

# Explode multi-select race (Q24) so multi-race respondents count in each chosen race ----
d6 <- d5 %>%
  filter(Q5!="Choose not to answer") %>%
  filter(!is.na(Q5))%>%
  mutate(Q24 = as.character(Q24)) %>%
  tidyr::separate_rows(Q24, sep = ",") %>%
  mutate(Q24 = stringr::str_trim(Q24)) %>%
  filter(
  !is.na(Q24), Q24 != "", # loose about 300 with this
  !Q24 %in% c("Choose not to answer"))%>% # loose about 500 with both race filters
  distinct(response_id, Q24, Q5) %>%               # avoid duplicates
  # mutate(Q5 = factor(Q5))%>%
  filter(Q24!="Another race or ethnicity") %>%
  mutate(
    Q5 = fct_relevel(
      Q5,
      "Another activity",
      sort(unique(Q5[Q5 != "Another activity"]), decreasing = TRUE)
    ))


unique(d6$Q24)


# create column n = count
d_heat <- d6 %>%
  count(Q5, Q24)   




# -- plot raw numbers ---
ggplot(d_heat, aes(x = Q24, y = Q5, fill = n)) +
  geom_tile() +
  scale_fill_continuous_sequential(
    palette = "Blues3",
    name    = "# respondents"
  ) +
  labs(
    x = "Race", #Q24
    y = "Most Important Activity",
    title = "Most Important Activity by Race"
  ) +
  # scale_fill_continuous_diverging(palette = "Green-Brown") +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggsave("./doc/Q5_activity_race_count.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)

# -- percent --
# set n respondents based on this dataset. 

n_resp<-nrow(d3) # total samples, only one for multi-racial individuals. this one makes multiracial people count % of total races per race.

n_resp_races<-nrow(d6) # each person-race combo counted once. this one makes multiracial people count once per race.





# -- plot % within activity --
# --- influenced by sampling across races --
d_heat_pct <- d6 %>%
  count(Q5, Q24) %>%
  mutate(pct_total_sample = n/n_resp)%>% # 
  group_by(Q5) %>%  # percent within activity
  mutate(pct_total_races = n / sum(n)) %>%
  
  # mutate(pct_total_sample = n/n_resp)%>% # multiracial people count % of total races per race
  # mutate(pct_total_races = n /n_resp_races) %>% #multiracial people count once per race.
  ungroup()




ggplot(d_heat_pct, aes(x = Q24, y = Q5, fill = pct_total_races)) +
  geom_tile() +
  scale_fill_continuous_sequential(palette = "Purples3",name = "Percent", labels = scales::percent) +
  # scale_fill_gradient(name = "Percent", labels = scales::percent) +
  labs(
    x = "Race",
    y = "Most Important Activity",
    title = "Most Important Activity by Race (percent within activity)"
  ) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("./doc/Q5_activity_race_p_within_activity.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)





# ----------------------------
# v2: P(activity | race) = among people of a given race, what percent did each activity ----------------

# race likelihood
d_within <- d6 %>%
  count(Q24, Q5, name = "n") %>%              # counts by race × activity
  tidyr::complete(Q24, Q5, fill = list(n = 0))%>%
  group_by(Q24) %>%               # percent within each race
  mutate(p_within_race = n / sum(n)) %>%  # percent within race #multiracial people count once per race
  ungroup()



ggplot(d_within, aes(x = Q24, y = Q5, fill = p_within_race)) +
  geom_tile() +
  scale_fill_continuous_sequential(
    palette = "Blues3",
    labels  = percent_format(accuracy = 1),
    name    = "% within race"
  ) +
  labs(
    x = "Race",
    y = "Most Important Activity"
    # title = "Most Important Activity within Race "
  ) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(
    aes(label = ifelse(p_within_race > 0.10,
                       scales::percent(p_within_race, accuracy = 0.1),
                       "")),  size = 3)

ggsave("./doc/Q5_activity_race_p_within_race.png",  width = 16, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)




# --------------------------------------
# -- v3. scale from less likely to have an activity vs more likely to have an activity based on race --

# 1. Overall probability of each activity (across all races)
overall_activity <- d6 %>%
  count(Q5) %>%
  mutate(p_overall = n / n_resp) %>% #sum(n)) %>%
    select(Q5, p_overall)

# 2. Within-race probability of each activity
d_rel <- d6 %>%
  count(Q24, Q5) %>%
  group_by(Q24) %>%
  mutate(p_within_race = n / sum(n)) %>%
  ungroup() %>%
  
  # 3. Join overall activity probabilities (not within race)
  left_join(overall_activity, by = "Q5") %>%
  
  # 4. Difference from overall: over/under representation
  mutate(diff_from_overall = p_within_race - p_overall)

# 5. Plot: negative = less likely than overall, positive = more likely
ggplot(d_rel, aes(x = Q24, y = Q5, fill = diff_from_overall)) +
  geom_tile() +
  scale_fill_continuous_diverging(
    palette  = "Green-Brown",
    # palette  = "Blue-Red3",
    # midpoint = 0,  # 0 = same as overall
    labels   = percent_format(accuracy = 1),
    name     = "More / less than overall sample"
  ) +
  labs(
    x = "Race",
    y = "Most Important Activity"
    # title = "Most Important Activity by Race:\nOver- or under-representation relative to overall sample"
  ) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(
    aes(label = ifelse(diff_from_overall > 0.04 | diff_from_overall < -0.04,
                       scales::percent(diff_from_overall, accuracy = 0.1),
                       "")),  size = 3)

ggsave("./doc/Q5_activity_race_difference.png",  width = 16, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)

# -- interpretation
# Dark teal (large negative diff) → this race is less likely than the overall sample to report that activity

# Dark orange (large positive diff) → this race is more likely than the overall sample to report that activity

# Near zero / white → about what you’d expect given overall activity rates
