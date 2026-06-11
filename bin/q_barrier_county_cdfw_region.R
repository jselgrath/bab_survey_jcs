# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# GOAL: Combined Coastal Counties & Inland CDFW Regions Analysis (2026)

# -------------------------

library(tidyverse)
library(lmerTest) 
library(emmeans)

# ===========================================================================
# load data and setups
# ===========================================================================
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")


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

# ===========================================================================
# 2. Data Wrangling: Classify Coastal vs. Inland CDFW Groups
# ===========================================================================
# Ensure variable names match your exact dataset configuration
county_col     <- "Primary_County"          # Your primary county name column
coastal_col    <- "coastal_county"   # Name for your coastal output column
cdfw_col       <- "county_cdfw_region" # Name for your CDFW output column

# Hardcoded vector of official CA open-ocean coastal counties
coastal_list <- c(
  "Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin", 
  "San Francisco", "San Mateo", "Santa Cruz", "Monterey", 
  "San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", 
  "Orange", "San Diego"
)

d0_processed <- d0 %>%
  filter(!is.na(get(county_col))) %>%
  mutate(
    # Create the standard coastal logical check
    !!coastal_col := if_else(get(county_col) %in% coastal_list, "Coastal", "Non-Coastal"),
    
    # Create the standard full CDFW Region mapping block
    !!cdfw_col := case_when(
      get(county_col) %in% c("Del Norte", "Humboldt", "Lassen", "Mendocino", "Modoc", "Shasta", "Siskiyou", "Tehama", "Trinity") ~ "1 - Northern Region",
      get(county_col) %in% c("Alpine", "Amador", "Butte", "Calaveras", "Colusa", "El Dorado", "Glenn", "Lake", "Nevada", "Placer", "Plumas", "Sierra", "Sutter", "Yolo", "Yuba") ~ "2 - North Central Region",
      get(county_col) %in% c("Alameda", "Contra Costa", "Marin", "Napa", "Sacramento", "San Mateo", "Santa Clara", "Santa Cruz", "San Francisco", "Solano", "Sonoma") ~ "3 - Bay Delta Region",
      get(county_col) %in% c("Fresno", "Kern", "Kings", "Madera", "Mariposa", "Merced", "Monterey", "San Benito", "San Luis Obispo", "Stanislaus", "Tulare", "Tuolumne") ~ "4 - Central Region",
      get(county_col) %in% c("Los Angeles", "Orange", "San Diego", "Santa Barbara", "Ventura") ~ "5 - South Coast Region",
      get(county_col) %in% c("Imperial", "Inyo", "Mono", "Riverside", "San Bernardino") ~ "6 - Inland Deserts Region",
      TRUE ~ "Unknown/Other"
    ),
    
    # CREATE THE STRATEGIC COMBINED PLOTTING VARIABLE:
    # If Coastal -> Keep county name. If Non-Coastal -> Label with its CDFW Region pool.
    plot_group = if_else(get(coastal_col) == "Coastal", 
                         paste0(get(county_col), " County"), 
                         get(cdfw_col))
  ) %>%
  # Filter out any unresolved counties to clean up iterations
  filter(plot_group != "Unknown/Other")

# Split processed data into the list for iteration
l_hybrid <- d0_processed %>%
  group_split(plot_group) %>%
  set_names(., map_chr(., ~unique(.x$plot_group)))


# ===========================================================================
# Core Function: Run Statistics and Plot Subsets with Top-Left Sample Size
# ===========================================================================
plot_deviation_subset <- function(df, global_baseline, is_list_element = TRUE) {
  
  # 1. Determine group name for titles and file paths
  if (is_list_element) {
    group_name <- unique(df$plot_group)[1]
    title_text <- paste0("Ocean Barriers: ", group_name, " Deviation")
    file_suffix <- paste0("_group_", tolower(gsub(" ", "_", group_name)))
  } else {
    title_text <- "Barriers to Ocean Access in California (Statewide Baseline)"
    file_suffix <- "_statewide"
  }
  
  # 2. STATS - Filter and Convert responses to numeric format
  stats_data <- df %>%
    filter(!is.na(response)) %>%
    mutate(score = likert_lookup[response]) %>%
    filter(!is.na(score))
  
  # 3. Fallback Mechanism: Skip or catch errors if a region still has extreme low counts
  if (length(unique(stats_data$ResponseId)) < 3 || nrow(stats_data) < 10) {
    message(paste("Skipping statistics for:", group_name, "- Sample size too small."))
    return(NULL)
  }
  
  # Run mixed model
  m1 <- lmer(score ~ barrier + (1 | ResponseId), data = stats_data)
  anova_res <- anova(m1)
  
  # Extract statistics 
  f_val  <- round(anova_res$`F value`[1], 2)
  df_num <- round(anova_res$NumDF[1], 1)
  df_den <- round(anova_res$DenDF[1], 1)
  p_val  <- anova_res$`Pr(>F)`[1]
  p_text <- if(p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
  
  # 4. posthoc test
  pairwise_comps <- emmeans(m1, pairwise ~ barrier, lmerTest.limit = 36000, pbkrtest.limit = 36000)
  
  # groups from posthoc test (CLD)
  cld_result <- emmeans:::cld.emmGrid(pairwise_comps$emmeans, Letters = letters, Reversed = TRUE) %>% 
    as.data.frame() %>% 
    mutate(barrier = as.character(barrier)) %>% 
    mutate(cld_group = trimws(.group))
  
  # 5. Calculate base summary numbers & sample size
  # Extract total unique respondents contributing to this subset
  total_respondents <- length(unique(stats_data$ResponseId))
  
  df_summary <- stats_data %>%
    group_by(barrier) %>%
    summarize(
      n = n(),
      mean_score = mean(score),
      sem = sd(score) / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(
      centered_mean = mean_score - global_baseline, 
      ymin = centered_mean - sem,                  
      ymax = centered_mean + sem,
      barrier = as.character(barrier)
    )
  
  # Join the CLD letters back to your summary dataframe
  df_final <- df_summary %>%
    left_join(cld_result %>% dplyr::select(barrier, cld_group), by = "barrier") %>%
    mutate(barrier = reorder(barrier, centered_mean))
  
  # 6. Build the Subtitle string
  stats_subtitle <- paste0(
    "Centered on Statewide Mean Likert Score: ", round(global_baseline, 2), 
    "  |  Repeated Measures ANOVA: F(", df_num, ", ", df_den, ") = ", f_val, ", ", p_text
  )
  
  # 7. Generate the Point + Deviation Plot
  p <- ggplot(df_final, aes(x = centered_mean, y = barrier)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey64", linewidth = 0.8) + 
    
    geom_errorbar(
      aes(xmin = ymin, xmax = ymax), 
      orientation = "y",
      width = 0.2, 
      color = "#002F70", 
      linewidth = 0.7
    ) +
    
    geom_point(size = 3.5, color = "#002F70") +
    
    geom_text(
      aes(label = cld_group, x = ymax), 
      nudge_x = 0.05, 
      hjust = 0, 
      color = "grey30", 
      fontface = "bold",
      size = 3.5
    ) +
    
    # Dynamic top-left Sample Size Annotation
    annotate(
      geom = "text",
      x = -Inf, y = Inf,             # Forces placement to absolute top-left boundary
      label = paste0("n = ", total_respondents),
      hjust = -0.2, vjust = 1.5,     # Nudges the label slightly inward so it doesn't clip the borders
      fontface = "bold",
      color = "black",
      size = 4.5
    ) +
    
    labs(
      title = title_text,
      subtitle = paste0(stats_subtitle, "\nShared letters indicate no significant difference (Tukey HSD, p < 0.05)"),
      x = "Deviation from Mean Likert Score",
      y = ""
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    theme_bw() +
    deets9 
  
  # 8. Save the output plot
  clean_filename <- paste0("./doc/q13_barrier", file_suffix, "_deviation_with_stats.png")
  ggsave(clean_filename, plot = p, width = 11, height = 5, units = "in")
  
  return(p)
}
# ===========================================================================
# Execution
# ===========================================================================

# 1. Process and save the Statewide control baseline
plot_deviation_subset(d0_processed, global_baseline = global_mean, is_list_element = FALSE)

# 2. Loop through all 15 Coastal Counties + 6 Inland CDFW Pooled Regions automatically
walk(l_hybrid, ~plot_deviation_subset(.x, global_baseline = global_mean, is_list_element = TRUE))