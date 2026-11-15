# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# Goal: ranking management priorities (Focal Area Weighted)

# --------------------------------------------------------------------------
# LOAD LIBRARIES
# --------------------------------------------------------------------------
library(tidyverse)
library(multcomp)  # For glht and cld (statistical lettering)
library(survey)    # For survey-weighted means, SEs, and regression
library(car)       # For Anova()

# --------------------------------------------------------------------------
# SETUP & LOAD DATA
# --------------------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1 <- read_csv("./results/data_final_weighted_focal.csv", guess_max = Inf, show_col_types = FALSE)

# Keep ID, focal weight, and the 10 priority ranking columns
Overall_Rankings_wide <- d1 %>%
  dplyr::select(response_id, weight_focal_pop, QPriority_1:QPriority_10) %>%
  mutate(across(QPriority_1:QPriority_10, ~ as.numeric(na_if(as.character(.x), "")))) %>%
  filter(!is.na(weight_focal_pop)) %>%
  drop_na(QPriority_1:QPriority_10)

# Rename priority columns to clean labels
priority_names <- c(
  "Preventing Commercial Activity",
  "Promoting Ocean Education",
  "Jobs/Income",
  "Community Decisions/Rule-making",
  "Local Seafood",
  "Local Culture/Practices",
  "Community Connections",
  "Protecting Species",
  "Reducing Coastal Erosion",
  "Improved Ocean Access"
)
names(Overall_Rankings_wide)[3:12] <- priority_names

# Reshape to long format while keeping response_id and weight_focal_pop
Overall_Rankings <- Overall_Rankings_wide %>%
  pivot_longer(
    cols      = all_of(priority_names),
    names_to  = "Prompt",
    values_to = "value"
  )

# --------------------------------------------------------------------------
# STEP 1: CALCULATE WEIGHTED MEANS FIRST & REORDER THE DATA
# --------------------------------------------------------------------------
Mean_Order <- Overall_Rankings %>%
  group_by(Prompt) %>%
  summarise(
    mean_val = weighted.mean(value, w = weight_focal_pop, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  arrange(desc(mean_val)) # Highest weighted mean to lowest weighted mean

# Re-level Prompt based on descending weighted mean order
Overall_Rankings$Prompt <- factor(Overall_Rankings$Prompt, levels = Mean_Order$Prompt)

# --------------------------------------------------------------------------
# STEP 2: WEIGHTED STATISTICAL ANALYSIS (Clustered by respondent)
# --------------------------------------------------------------------------
# Survey design accounting for focal population weights & repeated ranks per respondent
des_focal <- svydesign(
  ids     = ~response_id,
  weights = ~weight_focal_pop,
  data    = Overall_Rankings
)

# Weighted model across priorities
m1 <- svyglm(value ~ Prompt, design = des_focal)
summary(m1)
car::Anova(m1, test.statistic = "F")

# Pairwise Tukey comparisons & compact letter display (CLD)
tukey_glht <- glht(m1, linfct = mcp(Prompt = "Tukey"))
tukey_letters <- suppressWarnings(cld(tukey_glht, level = 0.05, decreasing = TRUE))

letters_df <- data.frame(letters = tukey_letters$mcletters$Letters) %>%
  rownames_to_column("Prompt")

# --------------------------------------------------------------------------
# STEP 3: AGGREGATE WEIGHTED MEANS, SEs, & JOIN LETTERS
# --------------------------------------------------------------------------
svy_means <- svyby(~value, ~Prompt, design = des_focal, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  rename(mean = value, se = se)

Overall_Means <- Overall_Rankings %>%
  group_by(Prompt) %>%
  summarise(
    n_unweighted = n(),
    n_weighted   = sum(weight_focal_pop, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  left_join(svy_means, by = "Prompt") %>%
  left_join(letters_df, by = "Prompt")

print(Overall_Means)

# --------------------------------------------------------------------------
# STEP 4: GRAPH (Smallest rank score at the top)
# --------------------------------------------------------------------------
p_priority <- ggplot(Overall_Means, aes(x = fct_reorder(Prompt, mean, .desc = TRUE), y = mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_text(
    aes(y = mean + se, label = letters),
    hjust = -0.4,
    vjust = 0.4,
    size  = 5
  ) +
  labs(
    y = "Weighted Mean Rank Score",
    x = "Management Priority\n"
  ) +
  coord_flip() +
  theme_bw() +
  expand_limits(y = max(Overall_Means$mean + Overall_Means$se) * 1.12) +
  theme(
    axis.title = element_text(size = 17),
    axis.text  = element_text(size = 14)
  )

p_priority



# --------------------------------------------------------------------------
# SAVE PLOT
# --------------------------------------------------------------------------
ggsave(
  "./doc/bab_graph_mgt_priority.png",
  plot   = p_priority,
  width  = 8,
  height = 6,
  units  = "in",
  dpi    = 300,
  bg     = "white"
)