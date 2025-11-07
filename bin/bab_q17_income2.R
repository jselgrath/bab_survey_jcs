# Equity in Ocean Access (benefits and barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: graph mpa awareness by income

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 
library(scales)
library(colorspace)
library(purrr)
library(broom)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long4S.csv")%>%
  select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,Q17:Q32_4,Q20a_5,Q27,Q25,Q27,Q28,Q31b,YEAR)%>%
  mutate(Q27 = as.character(Q27), Q17 = as.character(Q17)) %>%
  glimpse()
d1



# -- select MPA, income  questions --
d2<-d1%>%
  filter(Q17!=4) %>% # error - unsure of source
  filter(YEAR==2025)%>%  # 2024 data have fewer income categories
  select(response_id,Q17,Q27)%>%
  filter(!is.na(Q17), !is.na(Q27)) %>%   
  filter(Q27!="Choose not to answer")%>%
  mutate(
    Q27 = factor(
      Q27,
      levels = c(
        "Less than $10,000",
        "$10,000 to $19,999",
        "$20,000 to $39,999",
        "$40,000 to $59,999",
        "$60,000 to $119,000",
        "$120,000 to $179,999",
        "$180,000 to $239,999",
        "$240,000 or more"#,
        # "Choose not to answer"  
      )))%>%
  glimpse()

# -- relevel and combine categories from 2024 data to match 2025 data, or to collapse small categories -------------------

# Familiarity wth  MPAs -----------------
d3 <- d2 %>%
  filter(!is.na(Q17)) %>%
  mutate(Q17 = fct_collapse(Q17,
                            "Extremely familiar" = c("Very familiar", "Extremely familiar")))%>%
  mutate(Q17 = fct_collapse(Q17,
                            "Not familiar (never heard of)" = c("Not familiar at all (never heard of them)")))%>%
  mutate(Q17 = factor(
    Q17,
    levels = c(
      "Not familiar (never heard of)",
      "Slightly familiar",
      "Moderately familiar",
      "Extremely familiar"
    )))%>%
  glimpse()
unique(d3$Q17)


# ---- 1) Denominators per income (different sample sizes)
denom_by_income <- d3 %>%
  count(Q27, name = "N_income")   # N_i


# ---- 2) Counts per (Q17, Q27) and merge denominators
d_counts <- d3 %>%
  count(Q17, Q27, name = "n") %>%
  complete(Q17, Q27, fill = list(n = 0)) %>%
  left_join(denom_by_income, by = "Q27") %>%
  mutate(pct_income = n / N_income)   # proportion within income for this Q17

# ---- 3) For each Q17 facet, test if the rate differs across income groups
# prop.test(successes_by_income, totals_by_income)
tests <- d_counts %>%
  group_by(Q17) %>%
  summarise(
    test = list(prop.test(x = n, n = N_income)),  # χ² with df = (#income - 1)
    .groups = "drop"
  ) %>%
  mutate(
    chi_stat = map_dbl(test, ~ unname(.x$statistic)),
    chi_df   = map_dbl(test, ~ unname(.x$parameter)),
    chi_p    = map_dbl(test, ~ .x$p.value),
    fmt_p    = ifelse(chi_p < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001)),
    facet_lab = paste0(Q17, "\n\u03C7\u00B2(", chi_df, ") = ", round(chi_stat, 2), ", p ", fmt_p)
  )

# ---- Add n per Q17 group ----
N_by_Q17 <- d3 %>%
  count(Q17, name = "N_total")

# facet labs 
facet_labs <- tests %>%
  left_join(N_by_Q17, by = "Q17") %>%
  mutate(
    facet_lab = paste0(
      Q17, "\nn = ", N_total, ", ",
      "\u03C7\u00B2(", chi_df, ") = ", round(chi_stat, 2), ", p ", fmt_p
    )
  ) %>%
  select(Q17, facet_lab) %>%
  deframe()


# ---- 4) Plot: facet by Q17, y = proportion within income (different denominators)
ggplot(d_counts, aes(x = Q27, y = pct_income, fill = Q27)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  geom_text(aes(label = percent(pct_income, accuracy = 1)),
            vjust = -0.25, size = 4.8) +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(d_counts$pct_income) * 1.15)) +
  scale_fill_discrete_sequential(palette = "Heat") +
  facet_wrap(~ Q17, ncol = 4, labeller = as_labeller(facet_labs)) +
  labs(
    x = "Income",
    y = "Proportion of Respondents\n(within income bracket)"#,
    # title = "Proportion with each MPA familiarity level by income group"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    strip.text   = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )


ggsave("./doc/q17_mpa_famil_income3.png",   width = 18, height = 8,     # size in inches
       units = "in",               # "in", "cm", or "mm"
       dpi = 300,                 # resolution 
       bg = "white" )              # background or "transparent" 

