# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: barriers by county

# ===========================================================================
# load data and setups
# ===========================================================================
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

library(tidyverse)
library(lmerTest) 
library(emmeans)

### Load in Data files -------------------

d0 <- read_csv("./results/q13_barrier_long.csv") %>% 
  filter(influencer_any_b!=1)%>%
  glimpse()

# Convert likert to numerical (Standardized)
likert_lookup <- c(
  "Strongly disagree" = 5,
  "Disagree"          = 4,
  "Neutral"           = 3,
  "Agree"             = 2,
  "Strongly agree"    = 1
)

# 1. Calculate the Grand Mean from the WHOLE dataset (d0)
global_mean <- d0 %>%
  filter(!is.na(response)) %>%
  mutate(score = likert_lookup[response]) %>%
  summarize(grand_mean = mean(score, na.rm = TRUE)) %>%
  pull(grand_mean)

# 2. Split data by County into list l_county -----------------------------
# Note: Change "County" to match the exact column name in your d0 file (e.g., "county", "q_county")
county_col <- "Primary_County" 

l_county <- d0 %>%
  filter(!is.na(get(county_col))) %>%
  group_split(get(county_col)) %>%
  set_names(., map_chr(., ~unique(.x[[county_col]])))

# ===========================================================================
# Core Function: Calculate Deviation, Run Statistics, and Plot Subsets
# ===========================================================================
plot_deviation_subset <- function(df, global_baseline, is_list_element = TRUE) {
  
  # 1. Determine group name for titles and file paths
  if (is_list_element) {
    # Dynamically grab the county name
    group_name <- unique(df[[county_col]])[1]
    if (is.na(group_name)) group_name <- "Unknown_County"
    title_text <- paste0("Ocean Barriers: ", group_name, " County Deviation")
    file_suffix <- paste0("_county_", tolower(gsub(" ", "_", group_name)))
  } else {
    title_text <- "Barriers to Ocean Access in California (Statewide Baseline)"
    file_suffix <- "_statewide"
  }
  
  # 2. STATS - Filter and Convert responses to numeric format
  stats_data <- df %>%
    filter(!is.na(response)) %>%
    mutate(score = likert_lookup[response]) %>%
    filter(!is.na(score))
  
  # 3. Run mixed model for this data subset
  m1 <- lmer(score ~ barrier + (1 | ResponseId), data = stats_data)
  
  # Use standard base anova wrapper (lmerTest overloads this automatically)
  anova_res <- anova(m1)
  
  # Extract statistics 
  f_val  <- round(anova_res$`F value`[1], 2)
  df_num <- round(anova_res$NumDF[1], 1)
  df_den <- round(anova_res$DenDF[1], 1)
  p_val  <- anova_res$`Pr(>F)`[1]
  
  # Format p-value cleanly for presentation
  p_text <- if(p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
  
  # 4. posthoc test
  pairwise_comps <- emmeans(m1, pairwise ~ barrier, lmerTest.limit = 36000, pbkrtest.limit = 36000)
  
  # groups from posthoc test (CLD)
  cld_result <- emmeans:::cld.emmGrid(pairwise_comps$emmeans, Letters = letters, Reversed = TRUE) %>% 
    as.data.frame() %>% 
    mutate(barrier = as.character(barrier)) %>% 
    mutate(cld_group = trimws(.group)) # remove spaces
  
  # 5. Calculate base summary numbers & Center them on Global Mean
  df_summary <- stats_data %>%
    group_by(barrier) %>%
    summarize(
      n = n(),
      mean_score = mean(score),
      sem = sd(score) / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(
      centered_mean = mean_score - global_baseline, # Center the mean on the global baseline
      ymin = centered_mean - sem,                  # Shift error bar calculations
      ymax = centered_mean + sem,
      barrier = as.character(barrier)
    )
  
  # Join the CLD letters back to your summary dataframe
  df_final <- df_summary %>%
    left_join(cld_result %>% dplyr::select(barrier, cld_group), by = "barrier") %>%
    mutate(barrier = reorder(barrier, centered_mean)) # Reorder dynamically by magnitude
  
  # 6. Build the Subtitle string matching your original format
  stats_subtitle <- paste0(
    "Centered on Statewide Mean Likert Score: ", round(global_baseline, 2), 
    "  |  Repeated Measures ANOVA: F(", df_num, ", ", df_den, ") = ", f_val, ", ", p_text
  )
  
  # 7. Generate the Point + Deviation Plot
  p <- ggplot(df_final, aes(x = centered_mean, y = barrier)) +
    # Red baseline representing overall State Mean
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey64", linewidth = 0.8) + 
    
    # Horizontal error bar syntax using orientation = "y"
    geom_errorbar(
      aes(xmin = ymin, xmax = ymax), 
      orientation = "y",
      width = 0.2, 
      color = "#002F70", 
      linewidth = 0.7
    ) +
    
    geom_point(size = 3.5, color = "#002F70") +
    
    # Add the Compact Letter Display (CLD) letters
    geom_text(
      aes(label = cld_group, x = ymax), 
      nudge_x = 0.05, 
      hjust = 0, 
      color = "grey30", 
      fontface = "bold",
      size = 3.5
    ) +
    
    labs(
      title = title_text,
      subtitle = paste0(stats_subtitle, "\nShared letters indicate no significant difference (Tukey HSD, p < 0.05)"),
      x = "County Deviation from Statewide Mean Likert Score",
      y = ""
    ) +
    # Expand x-axis slightly so letters don't get clipped off the edge
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    theme_bw() +
    deets9 
  
  # 8. Save the output plot
  clean_filename <- paste0("./doc/q_barrier_county_", file_suffix, "_deviation_with_stats_no_infl.png")
  # clean_filename <- paste0("./doc/q_barrier_county_", file_suffix, "_deviation_with_stats_infl.png")
  ggsave(clean_filename, plot = p, width = 11, height = 5, units = "in")
  
  return(p)
}

# ===========================================================================
# Execution
# ===========================================================================

# 1. Process and save the Statewide dataset with its statistics for reference
plot_deviation_subset(d0, global_baseline = global_mean, is_list_element = FALSE)

# 2. Automatically loop through every individual County split and generate graphs
walk(l_county, ~plot_deviation_subset(.x, global_baseline = global_mean, is_list_element = TRUE))