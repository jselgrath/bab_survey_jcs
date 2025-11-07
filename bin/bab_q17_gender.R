# Equity in Ocean Access (benefits and barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: graph mpa awareness by gender

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
# -------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

# ---- Load and select relevant columns ----
d1 <- readr::read_csv("./results/data_long4.csv", show_col_types = FALSE) %>%
  select(quest_comb, response_id, Q1, Q2, Q4, Q5, Q8,
         Q17:Q32_4, Q20a_5, Q25, Q27, Q28, Q31b, YEAR) %>%
  filter(Q17!=4) %>% # error - unsure of source
  mutate(
    Q17 = as.character(Q17),
    Q25 = as.character(Q25)
  )%>%
  filter(!is.na(Q17), !is.na(Q25))%>%
  glimpse()

unique(d1$Q17)
unique(d1$Q25)

# Recode/collapse Q17 (MPA familiarity) to 4 levels ----
q17_levels <- c(
  "Not familiar (never heard of)",
  "Slightly familiar",
  "Moderately familiar",
  "Extremely familiar"
)

# ---- Keep only 2025 data and relevant vars ----
d2 <- d1 %>%
  # filter(YEAR == 2025) %>%
  select(response_id, Q17, Q25) %>%
  filter(!is.na(Q17), !is.na(Q25)) %>%
  filter(Q25 != "Choose not to answer")

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

# Explode multi-select race (Q25) so multi-race respondents count in each chosen race ----

d3a<-d3

# fix so does not get exploded below
d3a$Q25<-gsub("Transgender, non-binary, or another gender","Non-binary",d3a$Q25)

unique(d3a$Q25)

d4 <- d3a %>%
  mutate(Q25 = as.character(Q25), Q17 = as.character(Q17)) %>%
  tidyr::separate_rows(Q25, sep = ",") %>%
  mutate(Q25 = stringr::str_trim(Q25)) %>%
  distinct(response_id, Q25, Q17) %>%               # avoid duplicates
  mutate(Q17 = factor(Q17, levels = q17_levels))

unique(d4$Q25)

# ---- Clean & shorten gender (Q25) ----
gender_levels_short <- c(
  "Female",
  "Non-binary",
  "Male"
)





# ---- Denominators per gender (sample size by gender) ----
denom_by_gender <- d4 %>%
  count(Q25, name = "N_gender")

# ---- Counts per Q17 × Q25 ----
d_counts <- d4 %>%
  count(Q17, Q25, name = "n") %>%
  tidyr::complete(Q17, Q25, fill = list(n = 0)) %>%
  left_join(denom_by_gender, by = "Q25") %>%
  mutate(pct_gender = n / N_gender)%>%
  mutate(Q25 = factor(Q25, levels = gender_levels_short))%>%
  glimpse()

levels(d_counts$Q25)


# ---- prop.test per Q17 (do proportions differ across gender groups?) ----
tests <- d_counts %>%
  group_by(Q17) %>%
  summarise(test = list(prop.test(x = n, n = N_gender)), .groups = "drop") %>%
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
      Q17, "\nn = ", N_total, " ",
      "\u03C7\u00B2(", chi_df, ") = ", round(chi_stat, 2), ", p ", fmt_p
    )
  ) %>%
  tibble::deframe()

# ---- Plot ----
p <- ggplot(d_counts, aes(x = Q25, y = pct_gender, fill = Q25)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  geom_text(aes(label = percent(pct_gender, accuracy = 1)),
            vjust = -0.25, size = 4.8) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(d_counts$pct_gender) * 1.15)) +
  scale_fill_discrete_sequential(palette = "Viridis") +
  facet_wrap(~ Q17, ncol = 4, labeller = as_labeller(facet_labs)) +
  labs(
    x = "Gender",
    y = "Proportion of Respondents\n(within gender)"
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
ggsave("./doc/q17_mpa_famil_gender3.png",
       plot = p, width = 18, height = 8, units = "in",
       dpi = 300, bg = "white")

