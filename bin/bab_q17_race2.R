# ----------------------------------------------------------
# Equity in Ocean Access (Benefits and Barriers (bab))
# Author: Jennifer Selgrath

# Goal: Graph MPA awareness (Q17) by race (Q24) with per-facet prop.test()
# ----------------------------------------------------------

library(tidyverse)    
library(colorspace)   
library(here)         # project-safe file paths
# -----------------------------------------------

# I/O ----
in_csv  <- here::here("results", "data_long4.csv")
out_png <- here::here("doc", "q17_mpa_famil_race3.png")

# Data load & select ----
d1 <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
  select(
    quest_comb, response_id, Q1, Q2, Q4, Q5, Q8,
    Q17:Q32_4, Q20a_5, Q24, Q25, Q27, Q28, Q31b, YEAR
  ) %>%
  mutate(
    Q24 = as.character(Q24),
    Q17 = as.character(Q17)
  )%>%
  glimpse()

# Keep only variables we need for this plot ----
d3 <- d1 %>%
  select(response_id, Q17, Q18, Q29, Q30, Q24)

# Recode/collapse Q17 (MPA familiarity) to 4 levels ----
q17_levels <- c(
  "Not familiar (never heard of)",
  "Slightly familiar",
  "Moderately familiar",
  "Extremely familiar"
)

d4 <- d3 %>%
  filter(!is.na(Q17)) %>%
  mutate(
    Q17 = forcats::fct_collapse(
      Q17,
      "Extremely familiar" = c("Very familiar", "Extremely familiar"),
      "Not familiar (never heard of)" = "Not familiar at all (never heard of them)"
    ),
    Q17 = factor(Q17, levels = q17_levels)
  )

# Explode multi-select race (Q24) so multi-race respondents count in each chosen race ----
d4a <- d4 %>%
  mutate(Q24 = as.character(Q24), Q17 = as.character(Q17)) %>%
  tidyr::separate_rows(Q24, sep = ",") %>%
  mutate(Q24 = stringr::str_trim(Q24)) %>%
  filter(
    !is.na(Q24), Q24 != "",
    !Q24 %in% c("please specify:", "Choose not to answer", "Another race or ethnicity")
  ) %>%
  distinct(response_id, Q24, Q17) %>%               # avoid duplicates
  mutate(Q17 = factor(Q17, levels = q17_levels))

# Race levels & pretty labels (with line breaks) ----
race_levels <- c(
  "American Indian or Alaska Native",
  "Asian",
  "Black or African American",
  "Hispanic or Latino",
  "Native Hawaiian or Pacific Islander",
  "White"
)
race_labels <- c(
  "American Indian or\nAlaska Native",
  "Asian",
  "Black or\nAfrican American",
  "Hispanic or Latino",
  "Native Hawaiian or\nPacific Islander",
  "White"
)

d4a2 <- d4a %>%
  filter(!is.na(Q17)) %>%                 # <-- add this
  filter(!is.na(Q24)) %>%
  mutate(
    Q24 = factor(Q24, levels = race_levels, labels = race_labels),
    Q17 = factor(Q17, levels = q17_levels)
  )

# Counts per Race × Q17; zero-fill missing combos ----
counts <- d4a2 %>%
  count(Q24, Q17, name = "n") %>%
  tidyr::complete(Q24, Q17, fill = list(n = 0))

# Denominators per race (different sample sizes) ----
denom_by_race <- counts %>%
  group_by(Q24) %>%
  summarise(N_race = sum(n), .groups = "drop")

# Merge and compute proportions within race ----
d_counts <- counts %>%
  left_join(denom_by_race, by = "Q24") %>%
  mutate(pct_within_race = n / N_race)

# prop.test per Q17: does the proportion differ across races? ----
tests <- d_counts %>%
  group_by(Q17) %>%
  summarise(
    test = list(prop.test(x = n, n = N_race)),   # vectorized across races for this Q17
    .groups = "drop"
  ) %>%
  mutate(
    chi_stat = purrr::map_dbl(test, ~ unname(.x$statistic)),
    chi_df   = purrr::map_dbl(test, ~ unname(.x$parameter)),
    chi_p    = purrr::map_dbl(test, ~ .x$p.value),
    fmt_p    = ifelse(chi_p < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001))
  )

# n per Q17 (for facet label) ----
N_by_Q17 <- d4a2 %>% count(Q17, name = "N_total")

# Facet labels: Q17 + n + χ²(df)=stat, p ----
facet_labs <- tests %>%
  left_join(N_by_Q17, by = "Q17") %>%
  mutate(
    facet_lab = paste0(
      Q17, "\nn = ", N_total, ", ",
      "\u03C7\u00B2(", chi_df, ") = ", round(chi_stat, 2), ", p ", fmt_p
    )
  ) %>%
  select(Q17, facet_lab) %>%
  tibble::deframe()

# Plot ----
p <- ggplot(d_counts, aes(x = Q24, y = pct_within_race, fill = Q24)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  geom_text(aes(label = scales::percent(pct_within_race, accuracy = 1)),
            vjust = -0.25, size = 4.8) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, max(d_counts$pct_within_race) * 1.15)
  ) +
  scale_fill_discrete_sequential(palette = "Hawaii") +
  facet_wrap(~ Q17, ncol = 4, labeller = as_labeller(facet_labs)) +
  labs(
    x = "Race",
    y = "Percent of Respondents\n(within race)"
    # title = "MPA Familiarity by Race (prop.test per familiarity level)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x   = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text    = element_text(face = "bold", lineheight = 1.1),
    axis.title.x  = element_text(margin = margin(t = 10)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text( hjust = 1),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(, linewidth = 0.6)) #color = "white"
  

print(p)

# Save ----
ggsave(
  filename = out_png,
  plot = p,
  width = 18, height = 8, units = "in",
  dpi = 300, bg = "white"
)
