# California Ocean Access: Benefits and wellbeings (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing wellbeing question for entire state

# "centering" for a Mean/SEM plot usually means converting the categories to numeric scores (e.g., 1 to 5), calculating the grand mean of the entire dataset (d0), 
# and subtracting that grand mean from your focus group (d1) means. This centers the focus group graphs perfectly around the global average baseline.

# NOTE NEED TO CHECK OUTLIERS ETC

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
d0<-read_csv("./results/q_wb_long.csv")%>%
  filter(influencer_any_b!=1)%>%                          # select if include influencer or not - this changes top wellbeings
  glimpse()

# focus group version
d1<-read_csv("./results/q_wb_long_fg.csv")%>%
  glimpse()
names(d1)

# ---------------------------------------------------------------
# convert likert to numerical
likert_lookup <- c(
  "Strongly disagree" = 1,
  "Disagree"          = 2,
  "Neutral"           = 3,
  "Agree"             = 4,
  "Strongly agree"    = 5
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
  group_by(wellbeing) %>%
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

# Run mixed model: wellbeing as fixed effect, respondent as random effect
m1 <- lmer(score ~ wellbeing + (1 | ResponseId), data = stats_data)
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
pairwise_comps <- emmeans(m1, pairwise ~ wellbeing,lmerTest.limit = 36000,pbkrtest.limit = 36000)
summary(pairwise_comps$contrasts)

# groups from posthoc test --------------------

# Note: 'Reversed = TRUE' assigns 'a' to the highest mean score
cld_result <- emmeans:::cld.emmGrid(pairwise_comps$emmeans, Letters = letters, Reversed = TRUE) %>% 
  as.data.frame() %>% 
  mutate(wellbeing = as.character(wellbeing)) %>% 
  mutate(cld_group = trimws(.group)) # remove spaces

# Join the letters back to your summary dataframe d00
d00b <- d00 %>%
  mutate(wellbeing = as.character(wellbeing)) %>%
  left_join(cld_result %>% 
              dplyr:: select(wellbeing, cld_group), by = "wellbeing")


# graph -----------------------
stats_subtitle <- paste0(
  "Centered on Mean Likert Score: ", round(global_mean, 2), 
  "  |  Repeated Measures ANOVA: F(", df_num, ", ", df_den, ") = ", f_val, ", ", p_text
)

ggplot(d00b, aes(y = reorder(wellbeing, centered_mean), x = centered_mean)) +
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
    nudge_x = 0.005, 
    hjust = 0, 
    color = "grey40", 
    fontface = "bold",
    size = 3.5
  ) +
  
  labs(
    title = "Wellbeing in California",
    subtitle = paste0(stats_subtitle, "\nShared letters indicate no significant difference (Tukey HSD, p < 0.05)"),
    x = "Deviation from Mean Likert Score",
    y = ""
  ) +
  # Expand x-axis slightly so letters don't get clipped off the edge
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_bw() +
  deets9+
  
  # annotation at bottom
  annotation_custom(
    grob = textGrob("← Strongly Disagree", x = unit(0, "npc"), hjust = 0,     # anchor positions relative to the native layout box (0 = Left, 1 = Right)
                    gp = gpar(fontface = "italic", col = "grey30", fontsize = 11)),
    ymin = -0.6, ymax = -0.6, xmin = -Inf, xmax = Inf
  ) +
  annotation_custom(
    grob = textGrob("Strongly Agree →", x = unit(1, "npc"), hjust = 1, 
                    gp = gpar(fontface = "italic", col = "grey30", fontsize = 11)),
    ymin = -0.6, ymax = -0.6, xmin = -Inf, xmax = Inf
  ) +
  
  # Allow annotations to draw inside margins instead of truncating
  coord_cartesian(clip = "off") +
  
  # Extra margin layout room on the bottom for text anchors
  theme(plot.margin = margin(t = 10, r = 15, b = 25, l = 15, unit = "pt"))


# Save ---------
ggsave("./doc/q_wb_california_centered_no_influencer.png", width = 12, height = 6, units = "in")
# ggsave("./doc/q_wb_california_centered_influencer.png", width = 12, height = 4.5, units = "in")
