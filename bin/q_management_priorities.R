# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: ranking management priorities

# --------------------------------------------------------------------------
# LOAD LIBRARIES
# --------------------------------------------------------------------------
library(tidyverse) # Loads ggplot2, dplyr, tidyr, readr, forcats, stringr
library(multcomp)  # For glht and cld (statistical lettering)


# --------------------------------------------------------------------------
# INFLUENCER
# --------------------------------------------------------------------------
include_influencers <- FALSE # Set to TRUE or FALSE

# --------------------------------------------------------------------------
# LOAD DATA & PRE-PROCESS
# --------------------------------------------------------------------------
rm(list = setdiff(ls(all = TRUE), "include_influencers")) 
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1 <- read_csv("./results/data_long9.csv")

if (include_influencers == FALSE) {
  d1 <- d1 %>% filter(influencer_any_b != 1)
}

Overall_Rankings <- d1 %>% 
  dplyr::select(QPriority_1:QPriority_10)

### REMOVE INSTANCES WITH MISSING DATA
Overall_Rankings[Overall_Rankings == ""] <- NA
Overall_Rankings <- Overall_Rankings[complete.cases(Overall_Rankings),]

names(Overall_Rankings) <- c("Preventing Commercial Activity", "Promoting Ocean Education", "Jobs/Income",
                             "Community Decisions/Rule-making", "Local Seafood", "Local Culture/Practices",
                             "Community Connections", "Protecting Species", "Reducing Coastal Erosion",
                             "Improved Ocean Access")

Overall_Rankings <- pivot_longer(Overall_Rankings, cols=(1:10), names_to="Prompt")
Overall_Rankings$value <- as.numeric(Overall_Rankings$value)


# --------------------------------------------------------------------------
# STEP 1: CALCULATE THE MEANS FIRST & REORDER THE DATA
# --------------------------------------------------------------------------
# We calculate means first so we can force the raw data to match the mean order.
Mean_Order <- Overall_Rankings %>%
  group_by(Prompt) %>%
  summarise(mean_val = mean(value, na.rm = TRUE)) %>%
  arrange(desc(mean_val)) # Highest mean to lowest mean

# Re-level the Prompt column in the raw data based on this mathematical order
Overall_Rankings$Prompt <- factor(Overall_Rankings$Prompt, levels = Mean_Order$Prompt)


# --------------------------------------------------------------------------
# STEP 2: STATISTICAL ANALYSIS (Letters will now follow the means!)
# --------------------------------------------------------------------------
m1 <- aov(value ~ Prompt, data = Overall_Rankings)
Anova(m1)
summary(m1)

tukey_glht <- glht(m1, linfct = mcp(Prompt = "Tukey"))

# Because the data factor levels are ordered by mean, 'a' will reliably go to the largest mean
tukey_letters <- suppressWarnings(cld(tukey_glht, level = 0.05, decreasing = TRUE))

letters_df <- data.frame(letters = tukey_letters$mcletters$Letters) %>% 
  rownames_to_column("Prompt")


# --------------------------------------------------------------------------
# STEP 3: AGGREGATE FINAL MEANS & JOIN CORRECT LETTERS
# --------------------------------------------------------------------------
Overall_Means <- Overall_Rankings %>% 
  group_by(Prompt) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            n = n(),
            se = sd/sqrt(n)) %>%
  left_join(letters_df, by = "Prompt")

# Set filename dynamically
output_filename <- if(include_influencers) "./doc/q_priority_influencer.png" else "./doc/q_priority_no_influencer.png"


# --------------------------------------------------------------------------
# STEP 4: GRAPH (Smallest values at the top)
# --------------------------------------------------------------------------
# Adding '.desc = TRUE' inside fct_reorder reverses the axis layout,
# pushing the smallest mean value to the top of your horizontal bar chart.
ggplot(Overall_Means, aes(x = fct_reorder(Prompt, mean, .desc = TRUE), y = mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  
  # Adds significance letters cleanly tracking each bar
  geom_text(
    aes(y = mean + se, label = letters), 
    hjust = -0.5, 
    vjust = 0.4,  
    size = 5
  ) + 
  
  labs(y = "Mean Rank Score", x = "Management Priority\n") +
  coord_flip() + 
  theme_bw() +
  
  expand_limits(y = max(Overall_Means$mean + Overall_Means$se) * 1.1) +
  
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 14)
  )

# --------------------------------------------------------------------------
# SAVE PLOT
# --------------------------------------------------------------------------
ggsave(output_filename, width = 8, height = 6,
       units = "in",        
       dpi = 300,           
       bg = "white"         
)

