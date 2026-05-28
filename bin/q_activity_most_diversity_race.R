# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath
# California Marine Sanctuary Foundation/ CINMS
#-------------------------------------

# goal - compare distribution of activities by race using diversity metrics

# Richness️: The total number of unique activities reported by a group. 
# Shannon Index: This uses a logarithmic formula to account for both richness and "abundance" (how many people are in each category).
# Evenness: if the "diversity" is coming from having many activities or from people being spread out equally. 


# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(vegan)
library(ggplot2)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1<-read_csv("./results/data_long9_race.csv")%>%
  # select(response_id,QDesired_Time:QImportant_Activities_Most_TEXT,QDemographic_Home:QDemographic_Swimming,Mechanism,Version,Phase,State:fishing_most_b)%>% #quest_comb,
  unique()%>%
  glimpse()
d1

activity_counts <- d1 %>%
  count(q_demographic_race, QImportant_Activities_Most2) %>%
  tidyr::pivot_wider(names_from = QImportant_Activities_Most2, 
                     values_from = n, 
                     values_fill = 0)%>%
  glimpse()

diversity_results <- activity_counts %>%
  mutate(
    # 1. Richness (Number of non-zero categories)
    Richness = specnumber(select(., -q_demographic_race)),
    
    # 2. Shannon Index (H)
    Shannon = diversity(select(., -q_demographic_race), index = "shannon"),
    
    # 3. Pielou's Evenness (J = H / log(S))
    Evenness = Shannon / log(Richness)
  )%>%
  glimpse()


# graphs ------------------
ggplot(diversity_results, aes(y = q_demographic_race, x = Shannon)) +
  geom_col(position = "dodge") +
  coord_flip() + # Makes the race labels easier to read
    labs(
    title = "Shannon Diversity",
    y = "Race",
    x = "Shannon Index (H)"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))

ggplot(diversity_results, aes(y = q_demographic_race, x = Richness)) +
  geom_col(position = "dodge") +
  coord_flip() + # Makes the race labels easier to read
  labs(
    title = "Richness Diversity",
    y = "Race",
    x = "Richness"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))

ggplot(diversity_results, aes(y = q_demographic_race, x = Evenness)) +
  geom_col(position = "dodge") +
  coord_flip() + # Makes the race labels easier to read
  labs(
    title = "Evenness",
    y = "Race",
    x = "Evenness"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))



# resutls by influencer ------------------------------
activity_counts_grouped <- d1 %>%
  count(q_demographic_race, influencer_any_b, QImportant_Activities_Most2) %>%
  tidyr::pivot_wider(names_from = QImportant_Activities_Most2, 
                     values_from = n, 
                     values_fill = 0)%>%
  glimpse()

# Calculate metrics for the comparison table
diversity_comparison <- activity_counts_grouped %>%
  mutate(
    Richness = specnumber(select(., -q_demographic_race, -influencer_any_b)),
    Shannon = diversity(select(., -q_demographic_race, -influencer_any_b), index = "shannon"),
    Evenness = Shannon / log(Richness))%>%
  glimpse()

# graphs ------------------
ggplot(diversity_comparison, aes(y = q_demographic_race, x = Shannon, fill = factor(influencer_any_b))) +
  geom_col(position = "dodge") +
  coord_flip() + # Makes the race labels easier to read
  scale_fill_manual(values = c("0" = "#56B4E9", "1" = "#E69F00"), 
                    labels = c("General Public", "Influencer Recruit")) +
  labs(
    title = "Shannon Diversity: General Public vs. Influencers",
    y = "Race",
    x = "Shannon Index (H)",
    fill = "Recruitment Source"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))

ggplot(diversity_comparison, aes(y = q_demographic_race, x = Richness, fill = factor(influencer_any_b))) +
  geom_col(position = "dodge") +
  coord_flip() + # Makes the race labels easier to read
  scale_fill_manual(values = c("0" = "#56B4E9", "1" = "#E69F00"), 
                    labels = c("General Public", "Influencer Recruit")) +
  labs(
    title = "Richness Diversity: General Public vs. Influencers",
    y = "Race",
    x = "Richness Index (H)",
    fill = "Recruitment Source"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))

ggplot(diversity_comparison, aes(y = q_demographic_race, x = Evenness, fill = factor(influencer_any_b))) +
  geom_col(position = "dodge") +
  coord_flip() + # Makes the race labels easier to read
  scale_fill_manual(values = c("0" = "#56B4E9", "1" = "#E69F00"), 
                    labels = c("General Public", "Influencer Recruit")) +
  labs(
    title = "Evenness Diversity: General Public vs. Influencers",
    y = "Race",
    x = "Evenness Index (H)",
    fill = "Recruitment Source"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))
