# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# Goal: Summarizing q13 barrier question with Means and SEM (no stats)

# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)

# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

### Load in Data files -------------------
d0 <- read_csv("./results/q13_barrier_long.csv") %>% glimpse()
d1 <- read_csv("./results/q13_barrier_long_fg.csv") %>% glimpse()

# Split community focus groups into list l1
fg <- unique(d1$Focus_Group)
l1 <- d1 %>%
  filter(Focus_Group %in% fg) %>%
  group_split(Focus_Group) %>%
  set_names(., map_chr(., ~unique(.x$Focus_Group)))

# ---------------------------------------------------------------------------
# Core Function: Calculate Mean/SEM and Plot
# ---------------------------------------------------------------------------
plot_mean_sem <- function(df, is_list_element = TRUE) {
  
  # Determine group name for titles and file paths
  if (is_list_element) {
    group_name <- unique(df$Focus_Group)[1]
    if (is.na(group_name)) group_name <- "Unknown_Group"
    title_text <- paste0("Ocean Barriers: Mean Scores in ", group_name)
    file_suffix <- paste0("_fg_", tolower(group_name))
  } else {
    title_text <- "California Ocean Barriers"
    file_suffix <- "_statewide"
  }
  
  # Convert Likert scale responses to numeric values (1 to 5)
  # Adjust the mapping: 5 to mean "High Barrier" or "Strongly Agree"
  df_numeric <- df %>%
    filter(!is.na(response)) %>%
    mutate(
      score = case_when(
        response == "Strongly agree"    ~ 1,
        response == "Agree"             ~ 2,
        response == "Neutral"           ~ 3,
        response == "Disagree"          ~ 4,
        response == "Strongly disagree" ~ 5,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(score))
  
  # Calculate Mean, SD, N, and SEM per barrier
  df_stats <- df_numeric %>%
    group_by(barrier) %>%
    summarize(
      n = n(),
      mean_val = mean(score),
      sd_val = sd(score),
      sem_val = sd(score) / sqrt(n()),
      .groups = "drop"
    ) %>%
    # Order barriers so the highest mean score sits at the top of the y-axis
    mutate(barrier = reorder(barrier, mean_val))
  
  # Generate the Point + Error Bar Plot
  p <- ggplot(df_stats, aes(x = mean_val, y = barrier)) +
    # Draw a subtle reference line at the neutral score (3)
    geom_vline(xintercept = 3, linetype = "dashed", color = "grey60") +
    
    # Error bars for SEM
    geom_errorbar(aes(xmin = mean_val - sem_val, xmax = mean_val + sem_val), 
                   height = 0.2, color = "#002F70", linewidth = 0.8) +
    
    # Point markers for the mean
    geom_point(color = "#002F70", size = 3) +
    
    # Formatting axes and labels
    scale_x_continuous(limits = c(1, 5), breaks = 1:5,
                       labels = c("Strongly\nagree (1)", "Agree (2)", 
                                  "Neutral (3)", "Disgree (4)", "Strongly\ndisagree (5)")) +
    labs(
      title = title_text,
      subtitle = "Error bars: ±1 SEM",
      x = "Mean",
      y = ""
    ) +
    theme_bw() +
    deets9 # Keeps your custom theme components
  
  # Save the output plot
  clean_filename <- paste0("./doc/q_barrier", file_suffix, "_mean_sem.png")
  ggsave(clean_filename, plot = p, width = 10, height = 5, units = "in")
  
  return(p)
}

# ---------------------------------------------------------------------------
# Execution
# ---------------------------------------------------------------------------

# Process and save the Statewide data (d0)
plot_mean_sem(d0, is_list_element = FALSE)

# Automatically loop through and save plots for all community subsets (l1)
walk(l1, ~plot_mean_sem(.x, is_list_element = TRUE))