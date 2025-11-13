# Equity in Ocean Access (benefits and barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: graph mpa awareness by activity

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
# -------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

# ---- Load and select relevant columns ----
d1 <- readr::read_csv("./results/data_long5.csv", show_col_types = FALSE) %>%
  select(quest_comb, response_id, Q1, Q2, Q4, Q5, Q8,
         Q17:Q32_4, Q20a_5, Q25, Q27, Q28, Q31b, YEAR) %>%
  filter(Q17!=4) %>% # error - unsure of source
  mutate(
    Q17 = as.character(Q17),
    Q4 = as.character(Q4),
    Q5 = as.character(Q5)
  )%>%
  filter(!is.na(Q17), !is.na(Q4), !is.na(Q5))%>%
  glimpse()

unique(d1$Q17)
unique(d1$Q4)

# Recode/collapse Q17 (MPA familiarity) to 4 levels ----
q17_levels <- c(
  "Not familiar (never heard of)",
  "Slightly familiar",
  "Moderately familiar",
  "Extremely familiar"
)



d3 <- d1 %>%
  mutate(
    Q17 = forcats::fct_collapse(
      Q17,
      "Extremely familiar" = c("Very familiar", "Extremely familiar"),
      "Not familiar (never heard of)" = "Not familiar at all (never heard of them)"
    ),
    Q17 = factor(Q17, levels = q17_levels)
  )

unique(d3$Q17)

# Explode multi-select activity (Q4) so multi-activity respondents count in each chosen activity ----

d3a<-d3


d4 <- d3a %>%
  mutate(Q4 = as.character(Q4), Q17 = as.character(Q17)) %>%
  tidyr::separate_rows(Q4, sep = ",") %>%
  mutate(Q4 = stringr::str_trim(Q4)) %>%
  distinct(response_id, Q4, Q17) %>%               # avoid duplicates
  mutate(Q17 = factor(Q17, levels = q17_levels))%>%
  filter(Q4!="")%>%
  glimpse()

unique(d4$Q4)

# ---- Clean & shorten gender (Q4) ----
  # activity_labels <- c(
  #   "Beach Games/Sports",
  #   "Biking/Skating/Skateboarding",
  #   "Boating (Engine Powered)",
  #   "Cultural/Religious Ceremony",
  #   "Family/Gathering Activities",
  #   "Festivals (Music/Food)",
  #   "Fishing/Food Gathering",
  #   "Nature Observation/Photography",
  #   "None/No Response",
  #   "Other",
  #   "Paid Work",
  #   "Paddle/Kite/Sail Boarding/Kayaking",
  #   "Relaxing/Reading/Meditation",
  #   "Snorkeling/Scuba Diving",
  #   "Surfing",
  #   "Swimming/Bodysurfing",
  #   "Volunteering",
  #   "Walking/Running"
  # )




# ---- Denominators per gender (sample size by gender) ----
denom_by_gender <- d4 %>%
  count(Q4, name = "N_activity")

# ---- Counts per Q17 × Q4 ----
d_counts <- d4 %>%
  count(Q17, Q4, name = "n") %>%
  tidyr::complete(Q17, Q4, fill = list(n = 0)) %>%
  left_join(denom_by_activity, by = "Q4") %>%
  mutate(pct_activity = n / N_activity)%>%
  mutate(Q4 = factor(Q4, levels = activity_levels_short))%>%
  glimpse()

levels(d_counts$Q4)


# ---- prop.test per Q17 (do proportions differ across activity groups?) ----
tests <- d_counts %>%
  group_by(Q17) %>%
  summarise(test = list(prop.test(x = n, n = N_activity)), .groups = "drop") %>%
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
p <- ggplot(d_counts, aes(x = Q4, y = pct_activity, fill = Q4)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  geom_text(aes(label = percent(pct_activity, accuracy = 1)),
            vjust = -0.25, size = 4.8) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(d_counts$pct_activity) * 1.15)) +
  scale_fill_discrete_sequential(palette = "Viridis") +
  facet_wrap(~ Q17, ncol = 4, labeller = as_labeller(facet_labs)) +
  labs(
    x = "Activity",
    y = "Proportion of Respondents\n(within activity)"
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
ggsave("./doc/q17_mpa_famil_activity3.png",
       plot = p, width = 18, height = 8, units = "in",
       dpi = 300, bg = "white")

