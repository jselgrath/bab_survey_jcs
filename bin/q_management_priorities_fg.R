# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: basic analysis and graphs for MPA and NMS questions

# --------------------------------------------------------------------------
# LOAD LIBRARIES
# --------------------------------------------------------------------------
library(tidyverse) 
library(multcomp)  


# --------------------------------------------------------------------------
# LOAD DATA
# --------------------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# --------------------------------------------------------------------------
# USER SETTING: CHOOSE YOUR ANALYSIS RUN HERE
# --------------------------------------------------------------------------
# include_influencers <- FALSE # Set to TRUE or FALSE


### Load in Data files -------------------
d1 <- read_csv("./results/data_long9_fg.csv") %>% 
  glimpse()

# if (include_influencers == FALSE) {
#   d1 <- d1 %>% filter(influencer_any_b != 1)
# }

# Split community focus groups into list l1
fg <- unique(d1$Focus_Group)
l1 <- d1 %>%
  filter(Focus_Group %in% fg) %>%
  group_split(Focus_Group) %>%
  set_names(., map_chr(., ~unique(.x$Focus_Group)))


# --------------------------------------------------------------------------
# THE AUTOMATED FOCUS GROUP FUNCTION
# --------------------------------------------------------------------------


# This function handles the entire pipeline for one single focus group
analyze_and_plot_group <- function(group_data, group_name) {
  
  # 1. Clean and Subset Data
  Overall_Rankings <- group_data %>% dplyr::select(QPriority_1:QPriority_10)
  
  Overall_Rankings[Overall_Rankings == ""] <- NA
  Overall_Rankings <- Overall_Rankings[complete.cases(Overall_Rankings),]
  
  # Apply text names
  names(Overall_Rankings) <- c("Preventing Commercial Activity", "Promoting Ocean Education", "Jobs/Income",
                               "Community Decisions/Rule-making", "Local Seafood", "Local Culture/Practices",
                               "Community Connections", "Protecting Species", "Reducing Coastal Erosion",
                               "Improved Ocean Access")
  
  Overall_Rankings <- pivot_longer(Overall_Rankings, cols=(1:10), names_to="Prompt")
  Overall_Rankings$value <- as.numeric(Overall_Rankings$value)
  
  # 2. Determine Mean Order to Fix Letter Sequence Bugs
  Mean_Order <- Overall_Rankings %>%
    group_by(Prompt) %>%
    summarise(mean_val = mean(value, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(mean_val))
  
  Overall_Rankings$Prompt <- factor(Overall_Rankings$Prompt, levels = Mean_Order$Prompt)
  
  # 3. Statistical Analysis
  m1 <- aov(value ~ Prompt, data = Overall_Rankings)
  tukey_glht <- glht(m1, linfct = mcp(Prompt = "Tukey"))
  tukey_letters <- suppressWarnings(cld(tukey_glht, level = 0.05, decreasing = TRUE))
  
  letters_df <- data.frame(letters = tukey_letters$mcletters$Letters) %>% 
    rownames_to_column("Prompt")
  
  # 4. Aggregate Means & Merge Letters
  Overall_Means <- Overall_Rankings %>% 
    group_by(Prompt) %>% 
    summarise(mean = mean(value),
              sd = sd(value),
              n = n(),
              se = sd/sqrt(n), .groups = 'drop') %>%
    left_join(letters_df, by = "Prompt")
  
  # 5. Generate the Plot (Smallest values at the top)
  p <- ggplot(Overall_Means, aes(x = fct_reorder(Prompt, mean, .desc = TRUE), y = mean)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.6) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    geom_text(
      aes(y = mean + se, label = letters), 
      hjust = -0.5, 
      vjust = 0.4,  
      size = 5
    ) + 
    # Dynamic title changes based on which group is processing
    labs(y = "Mean Rank Score", x = "Management Priority\n", title = paste("Focus Group:", group_name)) +
    coord_flip() + 
    theme_bw() +
    expand_limits(y = max(Overall_Means$mean + Overall_Means$se) * 1.1) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 14)
    )
  
  # 6. Save the Output dynamically using the group name
  # e.g., "./doc/q_priority_Region1.png"
  clean_name <- str_replace_all(group_name, " ", "_") # replaces spaces with underscores for filenames
  output_filename <- paste0("./doc/q_priority_fg_", clean_name, ".png")
  
  ggsave(output_filename, plot = p, width = 9, height = 6,
         units = "in", dpi = 300, bg = "white")
  
  message(paste("Successfully processed and saved plot for:", group_name))
}

# --------------------------------------------------------------------------
# EXECUTE LOOP ACROSS YOUR LIST `l1`
# --------------------------------------------------------------------------
# Assumes l1 is loaded and has named elements (e.g., l1$RegionA, l1$RegionB)
group_names <- names(l1)

for (name in group_names) {
  # Extract the single group data frame from your list
  single_group_data <- l1[[name]]
  
  # Run the pipeline function
  analyze_and_plot_group(group_data = single_group_data, group_name = name)
}