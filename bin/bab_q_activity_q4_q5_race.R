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
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1<-read_csv("./results/data_long9.csv")%>%
  select(response_id,QDesired_Time:QImportant_Activities_Most_TEXT,QDemographic_Home:QDemographic_Swimming,Mechanism,Version,Phase,State:fishing_most_b)%>% #quest_comb,
  glimpse()
d1
unique(d1$QActual_Time)

names(d1)

levels(factor(d1$q_demographic_race)) # check race categories


# organizing activity categories
activities <- c(
  "Swimming/Bodysurfing",
  "Group/Family gatherings or activities",
  "Meditation/Reading/Relaxing/Art",
  "Enjoy views/sunsets",
  "Fishing or collecting food",
  "Beach games/Sports/Yoga",
  "Surfing",
  "Nature observing/Photographing/Education/Research",
  "Bicycling/Roller skating/Skateboarding",
  "Snorkeling/Scuba Diving",
  "Walking/Running/Hiking",
  "Sailing/Boating",
  "Paddleboarding/Kiteboarding/Kayaking/Canoeing",
  "Paid work",
  "Festivals",
  "Cultural or religious ceremonies",
  "Volunteering",
  "Another activity"
)

activities_ordered <- c(
  sort(activities[activities != "Another activity"]),
  "Another activity"
)

activities_ordered




# -- select Activity and Frequency questions --
d3<-d1%>%
  # filter(QDesired_Time7!=4) %>% # error - unsure of source
  select(response_id,QDesired_Time,QActual_Time,QImportant_Activities2,QImportant_Activities_Most2,influencer_most_b,influencer_any_b,fishing_any_b,fishing_most_b)%>%
  glimpse()

unique(d3$QDesired_Time)
unique(d3$QActual_Time)
unique(d3$QImportant_Activities2)
unique(d3$QImportant_Activities_Most2)




# heatmap for activities from QImportant_Activities_Most2 -------------------

# -- select Activity and Frequency questions, plus demographics --
d5<-d1%>%
  select(response_id,QDesired_Time,QActual_Time,QImportant_Activities_Most2, q_demographic_race,QDemographic_Birth,QDemographic_Income,QDemographic_Swimming,q_demographic_gender,QDemographic_PrimaryZip,q_demographic_education_clean,EJ_Bin,influencer_most_b:fishing_most_b)%>%
  filter(QImportant_Activities_Most2!="Choose not to answer") %>%
  glimpse()

unique(d5$QImportant_Activities_Most2)

# Explode multi-select race ( q_demographic_race) so multi-race respondents count in each chosen race ----
d6 <- d5 %>%
  filter(QImportant_Activities_Most2!="Choose not to answer") %>%
  filter(!is.na(QImportant_Activities_Most2))%>%
  mutate( q_demographic_race = as.character( q_demographic_race)) %>%
  tidyr::separate_rows( q_demographic_race, sep = ",") %>%
  mutate( q_demographic_race = stringr::str_trim( q_demographic_race)) %>%
  filter(
  !is.na(q_demographic_race),  q_demographic_race != "", # loose about 300 with this
  ! q_demographic_race %in% c("Choose not to answer"))%>% # loose about 500 with both race filters
  
  # avoid duplicates # add any columns to keep here:
  distinct(response_id, q_demographic_race, QImportant_Activities_Most2,influencer_any_b,influencer_most_b) %>%
  
  mutate(QImportant_Activities_Most2 = factor(QImportant_Activities_Most2))%>%
  # filter( q_demographic_race!="Another race or ethnicity") %>%
  glimpse()
  
# Get all unique activities except "Another activity" and sort them
levels_to_sort <- d6$QImportant_Activities_Most2[d6$QImportant_Activities_Most2 != "Another activity"]
sorted_alphabetical <- sort(unique(as.character(levels_to_sort)), decreasing = TRUE)

# 2. Combine them into the final desired order
final_order <- c(sorted_alphabetical,"Another activity")


# save exploded by race
write_csv(d6,"./results/data_long9_race.csv")

# 3. Apply the order to the dataframe
d7 <- d6 %>%
  mutate(QImportant_Activities_Most2 = fct_relevel(QImportant_Activities_Most2, final_order)) %>%
  glimpse()
  
  
# check where loose responses
# Diagnostic Check
stats <- list(
  original_d5 = nrow(d5),
  
  after_NA_filter = d5 %>% 
    filter(!is.na(q_demographic_race), q_demographic_race != "Choose not to answer") %>% 
    nrow(),
  
  after_explosion = d5 %>% 
    tidyr::separate_rows(q_demographic_race, sep = ",") %>% 
    nrow(),
  
  after_another_race_filter = d5 %>% 
    tidyr::separate_rows(q_demographic_race, sep = ",") %>% 
    filter(q_demographic_race != "Another race or ethnicity") %>% 
    nrow()
)

print(stats)




# evaluate how influencer changed results --------------

# Create a column that labels the group
d_within_comparison <- d7 %>%
  mutate(Source = ifelse(influencer_any_b == 1, "Influencer Recruit", "General Public")) %>%
  count(Source,  q_demographic_race, QImportant_Activities_Most2, name = "n") %>%
  group_by(Source,  q_demographic_race) %>%
  mutate(p_within_race = n / sum(n)) %>%
  ungroup()

# Plot with facets
ggplot(d_within_comparison, aes(x =  q_demographic_race, y = QImportant_Activities_Most2, fill = p_within_race)) +
  geom_tile() +
  facet_wrap(~Source) + # This splits the chart into two: Influencers vs Public
  scale_fill_continuous_sequential(palette = "Blues3", labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Activity by Race: Influencer vs. General Public")


ggsave("./doc/QImportant_Activities_Most2_activity_race_pct_influencer_vs_noinfluencer.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)


# heat maps --------------------

# create column n = count
d_heat <- d7 %>%
  count(QImportant_Activities_Most2,  q_demographic_race)   

# no influencer
d_heat2 <- d7 %>%
  filter(influencer_any_b == 0) %>% # ONLY include non-influencer respondents
  count(QImportant_Activities_Most2,  q_demographic_race) 



# -- plot raw numbers ---

# all data --------------
ggplot(d_heat, aes(x =  q_demographic_race, y = QImportant_Activities_Most2, fill = n)) +
  geom_tile() +
  scale_fill_continuous_sequential(
    palette = "Blues3",
    name    = "# respondents"
  ) +
  labs(
    x = "Race", # q_demographic_race
    y = "Most Important Activity",
    title = "Most Important Activity by Race - all data"
  ) +
  # scale_fill_continuous_diverging(palette = "Green-Brown") +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("./doc/QImportant_Activities_Most2_activity_race_count_alldata.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)

# no influencer -----------
ggplot(d_heat2, aes(x =  q_demographic_race, y = QImportant_Activities_Most2, fill = n)) +
  geom_tile() +
  scale_fill_continuous_sequential(
    palette = "Blues3",
    name    = "# respondents"
  ) +
  labs(
    x = "Race", # q_demographic_race
    y = "Most Important Activity",
    title = "Most Important Activity by Race - no influencer"
  ) +
  # scale_fill_continuous_diverging(palette = "Green-Brown") +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggsave("./doc/QImportant_Activities_Most2_activity_race_count_noinfluencer.png",  width = 15, height = 7,     # size in inches
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
d_heat_pct <- d7 %>%
  count(QImportant_Activities_Most2,  q_demographic_race) %>%
  mutate(pct_total_sample = n/n_resp)%>% # 
  group_by(QImportant_Activities_Most2) %>%  # percent within activity
  mutate(pct_total_races = n / sum(n)) %>%
  
  # mutate(pct_total_sample = n/n_resp)%>% # multiracial people count % of total races per race
  # mutate(pct_total_races = n /n_resp_races) %>% #multiracial people count once per race.
  ungroup()




ggplot(d_heat_pct, aes(x =  q_demographic_race, y = QImportant_Activities_Most2, fill = pct_total_races)) +
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

ggsave("./doc/QImportant_Activities_Most2_activity_race_p_within_activity.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)





# ----------------------------
# v2: P(activity | race) = among people of a given race, what percent did each activity ----------------

# race likelihood
d_within <- d7 %>%
  count( q_demographic_race, QImportant_Activities_Most2, name = "n") %>%              # counts by race × activity
  tidyr::complete( q_demographic_race, QImportant_Activities_Most2, fill = list(n = 0))%>%
  group_by( q_demographic_race) %>%               # percent within each race
  mutate(p_within_race = n / sum(n)) %>%  # percent within race #multiracial people count once per race
  ungroup()



ggplot(d_within, aes(x =  q_demographic_race, y = QImportant_Activities_Most2, fill = p_within_race)) +
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

ggsave("./doc/QImportant_Activities_Most2_activity_race_p_within_race.png",  width = 16, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)




# --------------------------------------
# -- v3. scale from less likely to have an activity vs more likely to have an activity based on race --

# 1. Overall probability of each activity (across all races)
overall_activity <- d7 %>%
  count(QImportant_Activities_Most2) %>%
  mutate(p_overall = n / n_resp) %>% #sum(n)) %>%
    select(QImportant_Activities_Most2, p_overall)

# 2. Within-race probability of each activity
d_rel <- d7 %>%
  count( q_demographic_race, QImportant_Activities_Most2) %>%
  group_by( q_demographic_race) %>%
  mutate(p_within_race = n / sum(n)) %>%
  ungroup() %>%
  
  # 3. Join overall activity probabilities (not within race)
  left_join(overall_activity, by = "QImportant_Activities_Most2") %>%
  
  # 4. Difference from overall: over/under representation
  mutate(diff_from_overall = p_within_race - p_overall)

# 5. Plot: negative = less likely than overall, positive = more likely
ggplot(d_rel, aes(x =  q_demographic_race, y = QImportant_Activities_Most2, fill = diff_from_overall)) +
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

ggsave("./doc/QImportant_Activities_Most2_activity_race_difference.png",  width = 16, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)

# -- interpretation
# Dark teal (large negative diff) → this race is less likely than the overall sample to report that activity

# Dark orange (large positive diff) → this race is more likely than the overall sample to report that activity

# Near zero / white → about what you’d expect given overall activity rates
