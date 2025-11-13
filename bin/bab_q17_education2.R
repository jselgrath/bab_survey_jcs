# Equity in Ocean Access (benefits and barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: graph mpa awareness by Education

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

# ---- Load and select relevant columns ----
d1 <- readr::read_csv("./results/data_long5.csv", show_col_types = FALSE) %>%
  select(quest_comb, response_id, Q1, Q2, Q4, Q5, Q8,
         Q17:Q32_4, Q20a_5, Q25, Q27, Q28, Q31b, YEAR) %>%
  filter(Q17!=4) %>% # error - unsure of source
  mutate(
    Q17 = as.character(Q17),
    Q28 = as.character(Q28)
  )%>%
  filter(!is.na(Q17), !is.na(Q28))

unique(d1$Q17)

# ---- Keep only 2025 data and relevant vars ----
d2 <- d1 %>%
  # filter(YEAR == 2025) %>%
  select(response_id, Q17, Q28) %>%
  filter(!is.na(Q17), !is.na(Q28)) %>%
  filter(Q28 != "Choose not to answer")

d3 <- d2 %>%
  mutate(
    Q17 = forcats::fct_collapse(
      Q17,
      "Extremely familiar" = c("Very familiar", "Extremely familiar"),
      "Not familiar (never heard of)" = "Not familiar at all (never heard of them)"
    ),
    Q17 = factor(Q17, levels = q17_levels)
  )

unique(d3$Q17)

# ---- Recode Q17 (MPA familiarity) into 4 levels ----
q17_levels <- c(
  "Not familiar (never heard of)",
  "Slightly familiar",
  "Moderately familiar",
  "Extremely familiar"
)



# ---- Clean & shorten Education (Q28) ----
edu_levels_short <- c(
  "Other",
  "HS or GED",
  "Vocational",
  "Some college/\nAssociate",
  "Bachelor’s",
  "Graduate (MA/PhD)"
)



d4 <- d3 %>%
  mutate(
    # normalize punctuation/spacing
    Q28 = stringr::str_replace_all(Q28, "’", "'"),
    Q28 = stringr::str_squish(Q28),
    
    # map by pattern (robust to small text differences)
    Q28_clean = dplyr::case_when(
      stringr::str_detect(Q28, regex("^some high school|GED", ignore_case = TRUE)) ~ "HS or GED",
      stringr::str_detect(Q28, regex("^vocational", ignore_case = TRUE))           ~ "Vocational",
      stringr::str_detect(Q28, regex("^some college|associate", ignore_case = TRUE)) ~ "Some college/\nAssociate",
      stringr::str_detect(Q28, regex("bachelor", ignore_case = TRUE))              ~ "Bachelor’s",
      stringr::str_detect(Q28, regex("^graduate|masters|master's|phd|professional", ignore_case = TRUE)) ~ "Graduate (MA/PhD)",
      stringr::str_detect(Q28, regex("^other$", ignore_case = TRUE))               ~ "Other",
      TRUE ~ NA_character_   # anything unmatched becomes NA so we can drop it
    )
  ) %>%
  filter(!is.na(Q28_clean)) %>%                     # drop NA education
  mutate(Q28 = factor(Q28_clean, levels = edu_levels_short)) %>%  # final factor
  select(response_id, Q17, Q28)                     # keep the fields we need

# ---- Denominators per education (sample size by education) ----
denom_by_education <- d4 %>%
  count(Q28, name = "N_edu")

# ---- Counts per Q17 × Q28 ----
d_counts <- d4 %>%
  count(Q17, Q28, name = "n") %>%
  tidyr::complete(Q17, Q28, fill = list(n = 0)) %>%
  left_join(denom_by_education, by = "Q28") %>%
  mutate(pct_edu = n / N_edu)

# ---- prop.test per Q17 (do proportions differ across education groups?) ----
tests <- d_counts %>%
  group_by(Q17) %>%
  summarise(test = list(prop.test(x = n, n = N_edu)), .groups = "drop") %>%
  mutate(
    chi_stat = purrr::map_dbl(test, ~ unname(.x$statistic)),
    chi_df   = purrr::map_dbl(test, ~ unname(.x$parameter)),
    chi_p    = purrr::map_dbl(test, ~ .x$p.value),
    fmt_p    = ifelse(chi_p < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001))
  )

# ---- Total n per Q17 (for facet strip labels) ----
N_by_Q17 <- d4 %>%
  count(Q17, name = "N_total")

# ---- Facet labels with χ²(df)=stat, p ----
facet_labs <- tests %>%
  left_join(N_by_Q17, by = "Q17") %>%
  transmute(
    Q17,
    facet_lab = paste0(
      Q17, "\n(n = ", N_total, ")\n",
      "\u03C7\u00B2(", chi_df, ") = ", round(chi_stat, 2), ", p ", fmt_p
    )
  ) %>%
  tibble::deframe()

# ---- Plot ----
p <- ggplot(d_counts, aes(x = Q28, y = pct_edu, fill = Q28)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  geom_text(aes(label = percent(pct_edu, accuracy = 1)),
            vjust = -0.25, size = 4.8) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(d_counts$pct_edu) * 1.15)) +
  scale_fill_discrete_sequential(palette = "SunsetDark") +
  facet_wrap(~ Q17, ncol = 4, labeller = as_labeller(facet_labs)) +
  labs(
    x = "Education",
    y = "Proportion of Respondents\n(within education bracket)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    strip.text   = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  )

print(p)

# ---- Save ----
ggsave("./doc/q17_mpa_famil_education3.png",
       plot = p, width = 18, height = 8, units = "in",
       dpi = 300, bg = "white")

