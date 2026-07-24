# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing q13 barrier question

# "centering" for a Mean/SEM plot usually means converting the categories to numeric scores (e.g., 1 to 5), calculating the grand mean of the entire dataset (d0), 
# and subtracting that grand mean from your focus group (d1) means. This centers the focus group graphs perfectly around the global average baseline.

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
  glimpse()

# focus group version
d1<-read_csv("./results/q13_barrier_long_fg.csv")%>%
  glimpse()
names(d1)

# Split d1 into a named list of data frames by Focus_Group
l1 <- d1 %>% 
  group_split(Focus_Group) %>% 
  set_names(unique(d1$Focus_Group))

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
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) + 
  
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
    subtitle = paste0(stats_subtitle, "\nNote: Shared letters indicate no significant difference (Tukey HSD, p < 0.05)"),
    x = "Deviation from Mean Likert Score",
    y = ""
  ) +
  # Expand x-axis slightly so letters don't get clipped off the edge
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_bw() +
  deets9


# Save ---------
ggsave("./doc/q_barrier_all_centered.png", width = 12, height = 4.5, units = "in")





# FOCUS GROUP ANALYSES -----------------------

# make lists for each fg ------------------------------
fg<-unique(d1$Focus_Group)

# Process Focus Group data (d1), calculate mean/SEM, and center them
d2 <- d1 %>%
  filter(!is.na(response)) %>%
  mutate(score = likert_lookup[response]) %>%
  group_by(Focus_Group, barrier) %>%
  summarize(
    n = n(),
    mean_score = mean(score),
    sem = sd(score) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    # Center the mean on the global baseline
    centered_mean = mean_score - global_mean,
    # Error bars remain proportional, just shifted
    ymin = centered_mean - sem,
    ymax = centered_mean + sem
  )






# list of focus groups
l1_summarized <- map(l1, function(df) {
  df %>%
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
      centered_mean = mean_score - global_mean,
      ymin = centered_mean - sem,
      ymax = centered_mean + sem
    )
})



# map(l1, your_function)

# f1<-function(list){

# summarize -------------------

# n respondents - includes NA --------------
d0c<-d0%>%
  group_by(barrier)%>%
  summarize(
    n=n())%>%
  glimpse() 


# responses - includes NA --------------
d0d<-d0%>%
  group_by(barrier,response)%>%
  summarize(
    n_val=n())%>%
  mutate(pct=round(n_val/d0c$n[1],3))%>%
  glimpse()

# n respondents - no NA --------------
d0e<-d0%>%
  filter(!is.na(response))%>%
  group_by(barrier)%>%
  summarize(
    n_tot=n())%>%
  glimpse() 

# responses - no NA --------------
d0f<-d0%>%
  filter(!is.na(response))%>%
  group_by(barrier,response)%>%
  summarize(
    n_val=n())%>%
  full_join(d0e)%>%
  mutate(pct=round(n_val/n_tot,3))%>%
  glimpse()

unique(d0f$response)

# -----------------------------------------------
# prep for graphing ------------------------------


# # order factors ---------------------------
d0f$response <- factor(d0f$response, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))

d0f
# view(d0f)

#make Disagree and Strongly disagree percentages negative show up of the left side of the y-axis
d0g0<-d0f%>%
  mutate(
    pct2 = case_when(
      response %in% c("Strongly agree", "Agree") ~ pct,
      response == "Neutral"                      ~ pct / 2,
      response %in% c("Disagree", "Strongly disagree") ~ -pct,
      TRUE ~ pct
    )
  )
neutral_rows <- d0g0 %>% filter(response == "Neutral") %>% mutate(pct2 = -pct2)
d0g2 <- bind_rows(d0g0, neutral_rows)

d0g <- d0g2 %>%
  group_by(barrier) %>%
  mutate(
    # Sum only the positive agreement sides to order your chart cleanly
    overall = sum(pct2[pct2 > 0]) 
  ) %>%
  ungroup() %>%
  mutate(barrier = reorder(barrier, overall)) %>%
  glimpse()

# order
d0g$response<-ordered(d0g$response, levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree","Neutral"))


##Order Prompts so the highest level of "Strongly agree" is at the top -------------
Factor_Order<-d0g[which(d0g$response=="Strongly agree"),]
Factor_Order<-Factor_Order[order(Factor_Order$pct2),]
Order<-Factor_Order$barrier
d0g$barrier <- ordered(d0g$barrier, levels=Order)

# graph -------------------------------------
ggplot(d0g, aes(y = barrier, x = pct2, fill = response)) + 
  # 1. Kept only geom_col (removed duplicate geom_bar)
  geom_col(orientation = 'y', width = 0.6) +
  
  # 2. Fixed 'response =' to 'values =' 
  scale_fill_manual(
    values = c(
      "Strongly agree"    = "#002F70", 
      "Agree"             = "#879FDB", 
      "Neutral"           = "grey50", 
      "Disagree"          = "#DA8A8B", 
      "Strongly disagree" = "#5F1415"
    ),
    breaks = c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
    name = ""
  ) +
  geom_vline(xintercept = 0) +
  xlab("% of Respondents") + 
  ylab("") +
  xlim(c(-1, 1)) +
  theme_bw() + 
  ggtitle("Use and/or experience within ocean and \ncoastal areas:") +
  deets9

ggsave("./doc/q_barrier_state_raw.png", width=12, height=4.5, units="in")










# as a function for focus groups --------------------
f1 <- function(df) {
  # 0. Extract the group name for dynamic plot titles and file saving
  # (Assumes all rows in the current df belong to the same Focus_Group)
  group_name <- unique(df$Focus_Group)[1]
  if (is.na(group_name)) group_name <- "Unknown_Group"
  
  # Summarize total respondents (no NA)
  d1e <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier) %>%
    summarize(n_tot = n(), .groups = "drop")
  
  # Summarize response percentages (no NA)
  d1f <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier, response) %>%
    summarize(n_val = n(), .groups = "drop") %>%
    full_join(d1e, by = "barrier") %>%
    mutate(pct = round(n_val / n_tot, 3))
  
  # Ensure response is a factor with explicit levels
  d1f$response <- factor(
    d1f$response, 
    levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree")
  )
  
  # Prep for Likert divergent graphing (handling the split Neutral)
  d1g0 <- d1f %>%
    mutate(
      pct2 = case_when(
        response %in% c("Strongly agree", "Agree") ~ pct,
        response == "Neutral"                      ~ pct / 2,
        response %in% c("Disagree", "Strongly disagree") ~ -pct,
        TRUE ~ pct
      )
    )
  
  neutral_rows <- d1g0 %>% filter(response == "Neutral") %>% mutate(pct2 = -pct2)
  d1g2 <- bind_rows(d1g0, neutral_rows)
  
  # Calculate overall agreement for standard sorting
  d1g <- d1g2 %>%
    group_by(barrier) %>%
    mutate(overall = sum(pct2[pct2 > 0])) %>%
    ungroup()
  
  # Factor order for plot fill logic
  d1g$response <- ordered(
    d1g$response, 
    levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree", "Neutral")
  )
  
  # Order items so highest "Strongly agree" is at the top
  factor_order <- d1g %>% 
    filter(response == "Strongly agree") %>% 
    arrange(pct2)
  
  d1g$barrier <- ordered(d1g$barrier, levels = factor_order$barrier)
  
  # Graph 
  # Dynamically including the Group Name in the title
  p <- ggplot(d1g, aes(y = barrier, x = pct2, fill = response)) +
    geom_col(orientation = 'y', width = 0.6) +
    scale_fill_manual(
      values = c(
        "Strongly agree"    = "#002F70",
        "Agree"             = "#879FDB",
        "Neutral"           = "grey50",
        "Disagree"          = "#DA8A8B",
        "Strongly disagree" = "#5F1415"
      ),
      breaks = c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
      name = ""
    ) +
    geom_vline(xintercept = 0) +
    xlab("% of Respondents") +
    ylab("") +
    xlim(c(-1, 1)) +
    theme_bw() +
    ggtitle(paste0("Use and/or experience within ocean and \ncoastal areas in ", group_name, ":")) +
    deets9

  # Dynamic File Saving
  # Converts "SD_South_County" to a clean filename component
  clean_filename <- paste0("./doc/q13_barrier_fg_", tolower(group_name), ".png")
  ggsave(clean_filename, plot = p, width = 12, height = 4.5, units = "in")

  # Return the plot object silently in case you want to view it in R
  return(p)
}


# 2. Run the function automatically across every data frame in your list `l1`
# walk(l1, f1)











# NEGATIVE FUNCTION

f2 <- function(df) {
  # 0. Extract the group name for dynamic plot titles and file saving
  group_name <- unique(df$Focus_Group)[1]
  if (is.na(group_name)) group_name <- "Unknown_Group"
  
  # 1. Summarize total respondents (no NA)
  d1e <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier) %>%
    summarize(n_tot = n(), .groups = "drop")
  
  # 2. Summarize response percentages (no NA)
  d1f <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier, response) %>%
    summarize(n_val = n(), .groups = "drop") %>%
    full_join(d1e, by = "barrier") %>%
    mutate(pct = round(n_val / n_tot, 3))
  
  d1f$response <- factor(
    d1f$response, 
    levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree")
  )
  
  # 3. Prep for Likert divergent graphing (handling the split Neutral)
  d1g0 <- d1f %>%
    mutate(
      pct2 = case_when(
        response %in% c("Strongly agree", "Agree") ~ pct,
        response == "Neutral"                      ~ pct / 2,
        response %in% c("Disagree", "Strongly disagree") ~ -pct,
        TRUE ~ pct
      )
    )
  
  neutral_rows <- d1g0 %>% filter(response == "Neutral") %>% mutate(pct2 = -pct2)
  d1g2 <- bind_rows(d1g0, neutral_rows)
  
  # 4. Calculate overall agreement for standard sorting
  d1g <- d1g2 %>%
    group_by(barrier) %>%
    mutate(overall = sum(pct2[pct2 > 0])) %>%
    ungroup()
  
  d1g$response <- ordered(
    d1g$response, 
    levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree", "Neutral")
  )
  
  # 5. NEW ORDER LOGIC: Sort by highest "Strongly disagree"
  # Since Strongly Disagree pct2 values are negative (e.g., -0.40 vs -0.10), 
  # arranging them in ascending order puts the biggest disagreement at the top.
  factor_order_disagree <- d1g %>% 
    filter(response %in% c("Strongly disagree", "Disagree")) %>% 
    group_by(barrier) %>% 
    summarize(total_disagreement = sum(pct2), .groups = "drop") %>% 
    arrange(desc(total_disagreement))
  
  # Apply the flipped order to your barrier factor
  d1g$barrier <- ordered(d1g$barrier, levels = factor_order_disagree$barrier)
  
  # 6. Graph Generation
  p <- ggplot(d1g, aes(y = barrier, x = pct2, fill = response)) + 
    geom_col(orientation = 'y', width = 0.6) +
    scale_fill_manual(
      values = c(
        "Strongly agree"    = "#002F70", 
        "Agree"             = "#879FDB", 
        "Neutral"           = "grey50", 
        "Disagree"          = "#DA8A8B", 
        "Strongly disagree" = "#5F1415"
      ),
      breaks = c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
      name = ""
    ) +
    geom_vline(xintercept = 0) +
    xlab("% of Respondents") + 
    ylab("") +
    coord_cartesian(xlim = c(-1, 1)) +
    theme_bw() + 
    ggtitle(paste0("Use and/or experience within ocean and \ncoastal areas in ", group_name, ":")) +
    deets9
  
  # 7. Dynamic File Saving (Appends '_disagreement' to the filename)
  clean_filename <- paste0("./doc/q_barrier_fg_raw_", tolower(group_name), "_disagreement.png")
  ggsave(clean_filename, plot = p, width = 12, height = 4.5, units = "in")
  
  return(p)
}


walk(l1, f2)
