# California Ocean Access: Benefits and barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing climate questions for entire state

# "centering" for a Mean/SEM plot usually means converting the categories to numeric scores (e.g., 1 to 5), calculating the grand mean of the entire dataset (d0), 
# and subtracting that grand mean from your focus group (d1) means. This centers the focus group graphs perfectly around the global average baseline.


# --------------------------------------------------------------------------
# LOAD LIBRARIES
# --------------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(grid)
library(colorspace)

# library(multcomp)
# library(multcompView)

# --------------------------------------------------------------------------
# LOAD DATA
# --------------------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

###Load in Data file -------------------
d0<-read_csv("./results/q_cc_long.csv")%>%
  filter(influencer_any_b!=1)%>%                          # select if include influencer or not 
  filter(climate_concern_type2=="observation")%>%
  # filter(climate_concern_type2=="environmental")%>%       # if only want social or environmental
  mutate(climate_obsv=climate_concern,
         climate_obsv_l=climate_concern_l)%>%
  glimpse()

# focus group version
d1<-read_csv("./results/q_cc_long_fg.csv")%>%
  filter(climate_concern_type2=="observation")%>%
  mutate(climate_obsv=climate_concern,
         climate_obsv_l=climate_concern_l)%>%
  glimpse()
names(d1)

# ---------------------------------------------------------------
# convert likert to numerical
# observations
likert_lookup_o <- c(
  "Strong decrease"    = -2,
  "Decrease"           = -1,
  "No change observed" = 0,
  "Increase"           = 1,
  "Strong increase"  = 2
)





# concerns
likert_lookup_c <- c(
  "Not at all concerned" = 1,
  "Somewhat concerned"   = 2,
  "Very concerned"       = 3
)




# Calculate the Grand Mean from the WHOLE dataset (d0) ------------------------------
global_mean <- d0 %>%
  filter(!is.na(response_cc)) %>%
  mutate(score = likert_lookup_o[response_cc]) %>%
  summarize(grand_mean = mean(score, na.rm = TRUE)) %>%
  pull(grand_mean)%>%
  glimpse()


# calculate mean/SEM, and center them
d00 <- d0 %>%
  filter(!is.na(response_cc)) %>%
  mutate(response_cc = trimws(response_cc)) %>%
  mutate(score = likert_lookup_o[response_cc]) %>%
  group_by(climate_obsv,climate_concern_type) %>%
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
  )%>%
  glimpse()


# STATS - WHOLE DATASET ---------------------
stats_data <- d0 %>%
  filter(!is.na(response_cc)) %>%
  mutate(response_cc = trimws(response_cc)) %>%
  mutate(score = likert_lookup_o[response_cc])

# Run mixed model: climate_obsv as fixed effect, respondent as random effect
m1 <- lmer(score ~ climate_obsv + (1 | ResponseId), data = stats_data)
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
pairwise_comps <- emmeans(m1, pairwise ~ climate_obsv,lmerTest.limit = 36000,pbkrtest.limit = 36000)
summary(pairwise_comps$contrasts)

# groups from posthoc test --------------------

# # Note: 'Reversed = TRUE' assigns 'a' to the highest mean score
# Clean the emmeans output and force it to be a raw character vector
cld_result_clean <- emmeans:::cld.emmGrid(pairwise_comps$emmeans, Letters = letters, Reversed = TRUE) %>% 
  as.data.frame() %>% 
  # as.character(as.matrix(...)) completely strips any hidden factor attributes
  mutate(climate_obsv = trimws(as.character(as.matrix(climate_obsv)))) %>% 
  dplyr::select(climate_obsv, cld_group = .group) %>% 
  mutate(cld_group = trimws(cld_group))

# raw character & join
d00b <- d00 %>%
  mutate(climate_obsv = trimws(as.character(as.matrix(climate_obsv))))%>%
  left_join(cld_result_clean, by = "climate_obsv")

# check
print(head(d00b))


# graph -----------------------
stats_subtitle <- paste0(
  "Centered on Mean Likert Score: ", round(global_mean, 2), 
  "  |  Repeated Measures ANOVA: F(", df_num, ", ", df_den, ") = ", f_val, ", ", p_text
)

ggplot(d00b, aes(y = reorder(climate_obsv, centered_mean), x = centered_mean, color = climate_concern_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey64", linewidth = 0.8) + 
  
  # Error bars now dynamically color code by type
  geom_errorbar(
    aes(xmin = ymin, xmax = ymax), 
    width = 0.2, 
    linewidth = 0.7
  ) +
  
  # Points dynamically color code by type
  geom_point(size = 3.5) +
  
  # Text labels for CLD
  geom_text(
    aes(label = cld_group, x = ymax), 
    nudge_x = 0.04, 
    hjust = 0, 
    fontface = "bold",
    size = 3.5,
    show.legend = FALSE # Hides 'a', 'b', 'c' from cluttering your color legend box
  ) +
  
  # Choose your own color palette (Example: ColorBrewer 'Set1' or custom hex codes)
  scale_color_brewer(palette = "Set1") + 
  # OR manually specify colors using:
  # scale_color_manual(values = c("observation" = "#002F70", "other_type" = "#E66101")) +
  
  labs(
    title = "Environmental & Social Observations in California",
    subtitle = paste0(stats_subtitle, "\nShared letters indicate no significant difference (Tukey HSD, p < 0.05)"),
    x = "Deviation from Mean Likert Score",
    y = "",
    color = "Type" # Changes the title of your color legend
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_bw() +
  deets9 +
  
  # Annotations at bottom
  annotation_custom(
    grob = textGrob("← Strongly Disagree", x = unit(0, "npc"), hjust = 0, 
                    gp = gpar(fontface = "italic", col = "grey30", fontsize = 11)),
    ymin = -0.6, ymax = -0.6, xmin = -Inf, xmax = Inf
  ) +
  annotation_custom(
    grob = textGrob("Strongly Agree →", x = unit(1, "npc"), hjust = 1, 
                    gp = gpar(fontface = "italic", col = "grey30", fontsize = 11)),
    ymin = -0.6, ymax = -0.6, xmin = -Inf, xmax = Inf
  ) +
  
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(t = 10, r = 15, b = 25, l = 15, unit = "pt"),
    legend.position = "right") # Ensures the color key shows up on the right margin


# Save ---------
# ggsave("./doc/q_climate_obs_env_california_centered_no_influencer.png", width = 12, height = 6, units = "in")
#
ggsave("./doc/q_climate_obs_california_centered_no_influencer.png", width = 12, height = 6, units = "in")
# ggsave("./doc/q_climate_obs_california_centered_influencer.png", width = 12, height = 4.5, units = "in")
# 