# California Ocean Access: Benefits and ecosystem_services (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing q13 ecosystem_service question for entire state - diverging likert scale



# --------------------------------------------------------------------------
# LOAD LIBRARIES
# --------------------------------------------------------------------------
library(tidyverse)
library(plyr) # load before other processing
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(likert)   # CRITICAL: For native diverging bar chart assembly

# --------------------------------------------------------------------------
# LOAD DATA
# --------------------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# Load raw long data
d0 <- read_csv("./results/q_es_long.csv") %>%
  # filter(influencer_any_b != 1)%>%   # upodate graph name at bottom based on this
  glimpse()

likert_lookup <- c(
  "Strongly disagree" = 1,
  "Disagree"          = 2,
  "Neutral"           = 3,
  "Agree"             = 4,
  "Strongly agree"    = 5
)

# --------------------------------------------------------------------------
# 1. RUN STATISTICS TO GET THE PERFECT ORDER AND LETTERS
# --------------------------------------------------------------------------
stats_data <- d0 %>%
  filter(!is.na(response)) %>%
  mutate(score = likert_lookup[response])

m1 <- lmer(score ~ ecosystem_service + (1 | ResponseId), data = stats_data)
pairwise_comps <- emmeans(m1, pairwise ~ ecosystem_service, lmerTest.limit = 36000, pbkrtest.limit = 36000)

# Extract and sort your compact letter display groups cleanly by estimated mean
cld_result <- emmeans:::cld.emmGrid(pairwise_comps$emmeans, Letters = letters, Reversed = FALSE) %>% 
  as.data.frame() %>%
  mutate(ecosystem_service = as.character(ecosystem_service)) %>%
  arrange(emmean) %>% # Sort lowest emmean to highest emmean
  mutate(cld_group = trimws(.group))

# --------------------------------------------------------------------------
# 2. PREPARE WIDE DATA STRUCTURE (Strict Factor Enforcement)
# --------------------------------------------------------------------------
wide_data <- d0 %>%
  filter(!is.na(response)) %>%
  dplyr::select(ResponseId, ecosystem_service, response) %>%
  pivot_wider(names_from = ecosystem_service, values_from = response) %>%
  dplyr::select(-ResponseId) %>%
  as.data.frame() # Force into a standard data frame base

# Reorder columns to match your statistical emmeans ranking
ordered_services <- cld_result$ecosystem_service
wide_data <- wide_data[, ordered_services]

# Rename the columns to include your significance letters
names(wide_data) <- paste0(cld_result$ecosystem_service, " (", cld_result$cld_group, ")")

# CRITICAL FIX: Explicitly convert every single column to a factor 
# with identical levels as the VERY LAST STEP before running the plot.
wide_data[] <- lapply(wide_data, function(x) {
  factor(x, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
})

# --------------------------------------------------------------------------
# 3. BUILD AND CUSTOMIZE THE LIKERT GRAPH (Clean Edge Totals Only)
# --------------------------------------------------------------------------
likert_obj <- likert(wide_data)

# Wrap plot in suppression tools to keep console logs completely clean
suppressMessages(suppressWarnings(
  plot(likert_obj, 
       centered = TRUE, 
       wrap = 40,
       plot.percents = FALSE,       # FIX: Turning this FALSE stops individual category text labels
       plot.percent.low = TRUE,     # Keeps the total negative summary active at the far left edge
       plot.percent.high = TRUE,    # Keeps the total positive summary active at the far right edge
       plot.percent.neutral = FALSE # Hides the neutral percentage entirely
  )
)) +
  scale_fill_manual(
    values = c(
      "Strongly disagree" = "#5F1415",
      "Disagree"          = "#DA8A8B",
      "Neutral"           = "grey70",
      "Agree"             = "#879FDB",
      "Strongly agree"    = "#002F70"
    ),
    breaks = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
  ) +
  labs(
    title = "Use and/or experience within ocean and coastal areas:",
    x = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Save
# ggsave("./doc/q_ecosystem_service_state_likert_no_influencer.png", width = 12, height = 6, units = "in")
ggsave("./doc/q_ecosystem_service_state_likert_influencer.png", width = 12, height = 6, units = "in")
