# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing q13 barrier question for entire state

# "centering" for a Mean/SEM plot usually means converting the categories to numeric scores (e.g., 1 to 5), calculating the grand mean of the entire dataset (d0), 
# and subtracting that grand mean from your focus group (d1) means. This centers the focus group graphs perfectly around the global average baseline.

# NOTE NEED TO CHECK OUTLIERS ETC

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 
library(colorspace)
library(purrr)
library(readr)
library(lme4)
library(lmerTest) # p-values
library(emmeans)
library(multcomp)
library(multcompView)

# ---------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

###Load in Data file -------------------
d0<-read_csv("./results/q13_barrier_long.csv")%>%
  # filter(influencer_any_b!=1)%>%                          # select if include influencer or not - this changes top barriers
  glimpse()

# focus group version
d1<-read_csv("./results/q13_barrier_long_fg.csv")%>%
  glimpse()
names(d1)

# focus group list version - have not made this yet
# l1<- read_rds("./results/q13_barrier_long_fg.rds")
# l1$Santa_Rosa

# ---------------------------------------------------------------
# convert likert to numerical
likert_lookup <- c(
  "Strongly disagree" = 5,
  "Disagree"          = 4,
  "Neutral"           = 3,
  "Agree"             = 2,
  "Strongly agree"    = 1
)

# Calculate the Grand Mean from the WHOLE dataset (d0) ------------------------------
global_mean <- d0 %>%
  filter(!is.na(response)) %>%
  mutate(score = likert_lookup[response]) %>%
  summarize(grand_mean = mean(score, na.rm = TRUE)) %>%
  pull(grand_mean)


# calculate mean/SEM, and center them
d00 <- d0 %>%
  filter(!is.na(response)) %>%
  mutate(score = likert_lookup[response]) %>%
  group_by(barrier) %>%
  summarize(
    n = n(),
    mean_score = mean(score),
    sem = sd(score) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    centered_mean = mean_score - global_mean, # Center the mean on the global baseline
    ymin = centered_mean - sem, # Error bars remain proportional, just shifted
    ymax = centered_mean + sem
  )


# STATS - WHOLE DATASET ---------------------
stats_data <- d0 %>%
  filter(!is.na(response)) %>%
  mutate(score = likert_lookup[response])

# Run mixed model: barrier as fixed effect, respondent as random effect
m1 <- lmer(score ~ barrier + (1 | ResponseId), data = stats_data)
anova(m1)
anova_res <- anova(m1)

# Extract statistics 
f_val  <- round(anova_res$`F value`[1], 2)
df_num <- round(anova_res$NumDF[1], 1)
df_den <- round(anova_res$DenDF[1], 1)
p_val  <- anova_res$`Pr(>F)`[1]

# Format p-value cleanly for presentation
p_text <- if(p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))



# posthoc test -------------------------------
pairwise_comps <- emmeans(m1, pairwise ~ barrier,lmerTest.limit = 36000,pbkrtest.limit = 36000)
summary(pairwise_comps$contrasts)

# groups from posthoc test --------------------

# Note: 'Reversed = TRUE' assigns 'a' to the highest mean score
cld_result <- emmeans:::cld.emmGrid(pairwise_comps$emmeans, Letters = letters, Reversed = TRUE) %>% 
  as.data.frame() %>% 
  mutate(barrier = as.character(barrier)) %>% 
  mutate(cld_group = trimws(.group)) # remove spaces

# Join the letters back to your summary dataframe d00
d00b <- d00 %>%
  mutate(barrier = as.character(barrier)) %>%
  left_join(cld_result %>% 
              dplyr:: select(barrier, cld_group), by = "barrier")


# graph -----------------------
stats_subtitle <- paste0(
  "Centered on Mean Likert Score: ", round(global_mean, 2), 
  "  |  Repeated Measures ANOVA: F(", df_num, ", ", df_den, ") = ", f_val, ", ", p_text
)

ggplot(d00b, aes(y = reorder(barrier, centered_mean), x = centered_mean)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey64", linewidth = 0.8) + 
  
  geom_errorbar(
    aes(xmin = ymin, xmax = ymax), 
    orientation = "y", 
    width = 0.2, 
    color = "#002F70",
    linewidth = 0.7
  ) +
  
  geom_point(size = 3.5, color = "#002F70") +
  
  # ADD THE CLD LETTERS HERE
  # nudge_x moves the letters slightly to the right of the error bars
  geom_text(
    aes(label = cld_group, x = ymax), 
    nudge_x = 0.05, 
    hjust = 0, 
    color = "grey30", 
    fontface = "bold",
    size = 3.5
  ) +
  
  labs(
    title = "Barriers to Ocean Access in California",
    subtitle = paste0(stats_subtitle, "\nShared letters indicate no significant difference (Tukey HSD, p < 0.05)"),
    x = "Deviation from Mean Likert Score",
    y = ""
  ) +
  # Expand x-axis slightly so letters don't get clipped off the edge
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_bw() +
  deets9


# Save ---------
# ggsave("./doc/q_barrier_california_centered_no_influencer.png", width = 12, height = 4.5, units = "in")
ggsave("./doc/q_barrier_california_centered_influencer.png", width = 12, height = 4.5, units = "in")
