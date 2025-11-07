# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset cleaned data for graphing and making long version - ecosystem services

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long4.csv")%>%
  select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,Q17:Q32_4,Q20a_5,Q24,Q25,Q27,Q28,Q31b,YEAR)%>%
  mutate(Q24 = as.character(Q24), Q17 = as.character(Q17)) %>%
  glimpse()
d1


levels(factor(d1$Q24)) # check race categories


# -- select MPA, NMS and Race questions --
d3<-d1%>%
  filter(Q17!=4) %>% # error - unsure of source
  select(response_id,Q17,Q18,Q29,Q30,Q24)


# -- relevel and combine categories from 2024 data to match 2025 data, or to collapse small categories -------------------

# Familiarity wth  MPAs -----------------
d4 <- d3 %>%
  filter(!is.na(Q17)) %>%
  mutate(Q17 = fct_collapse(Q17,
                            "Extremely familiar" = c("Very familiar", "Extremely familiar")))%>%
  mutate(Q17 = fct_collapse(Q17,
                            "Not familiar\n(never heard of)" = c("Not familiar at all (never heard of them)")))%>%
  mutate(Q17 = factor(
    Q17,
    levels = c(
      "Not familiar\n(never heard of)",
      "Slightly familiar",
      "Moderately familiar",
      "Extremely familiar"
    )))%>%
  glimpse()
unique(d4$Q17)



# Familiarity wth  NMS -----------------
d5 <- d3 %>%
  filter(!is.na(Q18)) %>%
  mutate(Q18 = fct_collapse(Q18,
                            "Extremely familiar" = c("Very familiar", "Extremely familiar")))%>%
  mutate(Q18 = fct_collapse(Q18,
                            "Not familiar\n(never heard of)" = c("Not familiar at all (never heard of them)")))%>%
  mutate(Q18 = factor(
    Q18,
    levels = c(
      "Not familiar\n(never heard of)",
      "Slightly familiar",
      "Moderately familiar",
      "Extremely familiar"
    )))%>%
  glimpse()
unique(d5$Q18)



# ----------------------------------
# -- general graphs ------------

# -- MPA graphs - Q17 ---------------------------

# -- organize --
counts_mpa <- d4 %>%
  group_by(Q17) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_mpa <- counts_mpa %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()


# -- compare expected frequencies to observed values --
chi1 <- chisq.test(props_mpa$n) # default = equal proportions
chi_stat1 <- unname(chi1$statistic)
chi_df1   <- unname(chi1$parameter)
chi_p1   <- chi1$p.value
fmt_p1 <- ifelse(chi_p1 < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001)) #  formatted p

resids1 <- chi1$stdres
data.frame(Q17 = props_mpa$Q17, Residual = resids1)


# -- graph --
ggplot(props_mpa, aes(x = Q17, y = pct, fill = Q17)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_mpa$pct) * 1.15)) +
  labs(
    x = "Familiarity with MPAs",
    y = "Percent of Respondents",
    # title = "Distribution of MPA familiarity",
    subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
                      chi_df1, ") = ", round(chi_stat1, 2), ", p ", fmt_p1)) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),  # top margin (move x-label down)
    axis.title.y = element_text(margin = margin(r = 10)),   # right margin (move y-label left)
    plot.subtitle = element_text(
      color = "grey40",    
      hjust = 1 )         
  )+
  scale_fill_discrete_sequential(palette = "Teal")


ggsave("./doc/q17_mpa_famil.png",   width = 6, height = 8,     # size in inches
  units = "in",              # "in", "cm", or "mm"
  dpi = 300,                 # resolution (300+ for publication quality)
  bg = "white"               # background color (use "transparent" if needed)
)



# -- NMS graphs - Q18 ------------------------------
counts_nms <- d5 %>%
  group_by(Q18) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_nms <- counts_nms %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# -- compare expected frequencies to observed values --
chi2 <- chisq.test(props_nms$n) # default = equal proportions
chi_stat2 <- unname(chi2$statistic)
chi_df2   <- unname(chi2$parameter)
chi_p2   <- chi2$p.value
fmt_p2 <- ifelse(chi_p2 < .001, "< 0.001", scales::number(chi_p2, accuracy = 0.001)) #  formatted p

resids2 <- chi2$stdres
data.frame(Q18 = props_nms$Q18, Residual = resids2)

# -- graph --
ggplot(props_nms, aes(x = Q18, y = pct, fill = Q18)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_nms$pct) * 1.15)) +
  labs(
    x = "Familiarity with National Marine Sanctuaries",
    y = "Percent of Respondents",
    # title = "Distribution of nms familiarity",
    subtitle = paste0("\u03C7\u00B2(",
                      chi_df2, ") = ", round(chi_stat2, 2), ", p ", fmt_p2)) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),   
    plot.subtitle = element_text(
      color = "grey40",    
      hjust = 1 )   # move y-label left
  )+
  scale_fill_discrete_sequential(palette = "Teal")


ggsave("./doc/q18_nms_famil.png",   width = 6, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)


# --------------------------------------------------------
# Familiarity wth  MPAs purpose - Q29 (2025 only) -----------------
d6 <- d3 %>%
  filter(!is.na(Q29))%>% 
mutate(Q29 = factor(
  Q29,
  levels = c(
    "No understanding",
    "Slight understanding",
    "Moderate understanding",
    "Full understanding"
  )))%>%
  glimpse()
unique(d6$Q29)

counts_Q29 <- d6 %>%
  group_by(Q29) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_Q29 <- counts_Q29 %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# -- compare expected frequencies to observed values --
chi3 <- chisq.test(props_Q29$n) # default = equal proportions
chi_stat3 <- unname(chi3$statistic)
chi_df3   <- unname(chi3$parameter)
chi_p3   <- chi3$p.value
fmt_p3 <- ifelse(chi_p3 < .001, "< 0.001", scales::number(chi_p3, accuracy = 0.001)) #  formatted p

resids3 <- chi3$stdres
data.frame(Q29 = props_Q29$Q29, Residual = resids3)

# -- graph --
ggplot(props_Q29, aes(x = Q29, y = pct, fill = Q29)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_Q29$pct) * 1.15)) +
  labs(
    x = "Purpose of MPAs", # and NMS
    y = "Percent of Respondents",
    # title = "Understand Role of MPAs/NMS",
    subtitle = paste0("\u03C7\u00B2(",
                      chi_df3, ") = ", round(chi_stat3, 2), ", p ", fmt_p3)) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),   
    plot.subtitle = element_text(
      color = "grey40",    
      hjust = 1 )   # move y-label left
  )+
  scale_fill_discrete_sequential(palette = "Purp")



ggsave("./doc/q29_mpa_purpose.png",   width = 6, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)  



# --------------------------------------------------------
# Familiarity wth  MPAs science - Q30 -----------------
d7 <- d3 %>%
  filter(!is.na(Q30))%>%# Familiarity wth  MPAs -----------------
mutate(Q30 = factor(
  Q30,
  levels = c(
    "No understanding",
    "Slight understanding",
    "Moderate understanding",
    "Full understanding"
  )))%>%
  glimpse()
unique(d7$Q30)

counts_Q30 <- d7 %>%
  group_by(Q30) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_Q30 <- counts_Q30 %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# -- compare expected frequencies to observed values --
chi4 <- chisq.test(props_Q30$n) # default = equal proportions
chi_stat4 <- unname(chi4$statistic)
chi_df4   <- unname(chi4$parameter)
chi_p4   <- chi4$p.value
fmt_p4 <- ifelse(chi_p4 < .001, "< 0.001", scales::number(chi_p4, accuracy = 0.001)) #  formatted p

resids4 <- chi4$stdres
data.frame(Q30 = props_Q30$Q30, Residual = resids4)

# -- graph --
ggplot(props_Q30, aes(x = Q30, y = pct, fill = Q30)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_Q30$pct) * 1.15)) +
  labs(
    x = "Role of Science for MPAs", # and NMS
    y = "Percent of Respondents",
    # title = "Understand Role of Science for MPAs/NMS",
    subtitle = paste0("\u03C7\u00B2(", #
                      chi_df4, ") = ", round(chi_stat4, 2), ", p ", fmt_p4)) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),   
    plot.subtitle = element_text(
      color = "grey40",    
      hjust = 1 )   # move y-label left
  )+
  scale_fill_discrete_sequential(palette = "Peach")



ggsave("./doc/Q30_mpa_science.png",   width = 6, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)  











