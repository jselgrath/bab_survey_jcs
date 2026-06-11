# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: basic analysis and graphs for MPA and NMS questions

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
# setwd("G:/My Drive/research/r_projects/bab_survey_jcs/")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long9.csv")%>%
  # select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,QMPA_Aware:Q32_4,Q20a_5,Q24,Q25,Q27,Q28,Q31b,YEAR)%>%
  # mutate(Q24 = as.character(Q24), QMPA_Aware = as.character(QMPA_Aware)) %>%
  glimpse()
d1


# levels(factor(d1$Q24)) # check race categories


# -- select MPA, NMS and Race questions --
# d3<-d1%>%
#   filter(QMPA_Aware!=4) %>% # error - unsure of source
#   # select(response_id,QMPA_Aware,QSanct_Aware,QMPA_Purpose,QMPA_Science,Q24)%>%
#   glimpse()


# -- relevel and combine categories from 2024 data to match 2025 data, or to collapse small categories -------------------

# Familiarity wth  MPAs -----------------
d4 <- d1 %>%
  filter(!is.na(QMPA_Aware)) %>%
  mutate(QMPA_Aware = fct_collapse(QMPA_Aware,
                            "Extremely familiar" = c("Very familiar", "Extremely familiar")))%>%
  mutate(QMPA_Aware = fct_collapse(QMPA_Aware,
                            "Not familiar\n(never heard of)" = c("Not familiar at all (never heard of them)")))%>%
  mutate(QMPA_Aware = factor(
    QMPA_Aware,
    levels = c(
      "Not familiar\n(never heard of)",
      "Slightly familiar",
      "Moderately familiar",
      "Extremely familiar"
    )))%>%
  glimpse()
unique(d4$QMPA_Aware)



# Familiarity wth  NMS -----------------
d5 <- d1 %>%
  filter(!is.na(QSanct_Aware)) %>%
  mutate(QSanct_Aware = fct_collapse(QSanct_Aware,
                            "Extremely familiar" = c("Very familiar", "Extremely familiar")))%>%
  mutate(QSanct_Aware = fct_collapse(QSanct_Aware,
                            "Not familiar\n(never heard of)" = c("Not familiar at all (never heard of them)")))%>%
  mutate(QSanct_Aware = factor(
    QSanct_Aware,
    levels = c(
      "Not familiar\n(never heard of)",
      "Slightly familiar",
      "Moderately familiar",
      "Extremely familiar"
    )))%>%
  glimpse()
unique(d5$QSanct_Aware)



# ----------------------------------
# -- general graphs ------------

# -- MPA graphs - QMPA_Aware ---------------------------

# -- organize --
counts_mpa <- d4 %>%
  group_by(QMPA_Aware) %>%
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
data.frame(QMPA_Aware = props_mpa$QMPA_Aware, Residual = resids1)


# -- graph --
ggplot(props_mpa, aes(x = QMPA_Aware, y = pct, fill = QMPA_Aware)) +
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


ggsave("./doc/QMPA_Aware_mpa_famil.png",   width = 6, height = 8,     # size in inches
  units = "in",              # "in", "cm", or "mm"
  dpi = 300,                 # resolution (300+ for publication quality)
  bg = "white"               # background color (use "transparent" if needed)
)



# -- NMS graphs - QSanct_Aware ------------------------------
counts_nms <- d5 %>%
  group_by(QSanct_Aware) %>%
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
data.frame(QSanct_Aware = props_nms$QSanct_Aware, Residual = resids2)

# -- graph --
ggplot(props_nms, aes(x = QSanct_Aware, y = pct, fill = QSanct_Aware)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_nms$pct) * 1.15)) +
  labs(
    x = "Familiarity with\nNational Marine Sanctuaries",
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


ggsave("./doc/QSanct_Aware_nms_famil.png",   width = 6, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)


# --------------------------------------------------------
# Familiarity wth  MPAs purpose - QMPA_Purpose (2025 only) -----------------
d6 <- d1 %>%
  filter(!is.na(QMPA_Purpose))%>% 
mutate(QMPA_Purpose = factor(
  QMPA_Purpose,
  levels = c(
    "No understanding",
    "Slight understanding",
    "Moderate understanding",
    "Full understanding"
  )))%>%
  glimpse()
unique(d6$QMPA_Purpose)

counts_QMPA_Purpose <- d6 %>%
  group_by(QMPA_Purpose) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_QMPA_Purpose <- counts_QMPA_Purpose %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# -- compare expected frequencies to observed values --
chi3 <- chisq.test(props_QMPA_Purpose$n) # default = equal proportions
chi_stat3 <- unname(chi3$statistic)
chi_df3   <- unname(chi3$parameter)
chi_p3   <- chi3$p.value
fmt_p3 <- ifelse(chi_p3 < .001, "< 0.001", scales::number(chi_p3, accuracy = 0.001)) #  formatted p

resids3 <- chi3$stdres
data.frame(QMPA_Purpose = props_QMPA_Purpose$QMPA_Purpose, Residual = resids3)

# -- graph --
ggplot(props_QMPA_Purpose, aes(x = QMPA_Purpose, y = pct, fill = QMPA_Purpose)) +
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  # Note: I removed ylim(0,.35) because it conflicts with scale_y_continuous below
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, max(props_QMPA_Purpose$pct) * 1.15)) +
  labs(
    x = "Purpose of\nSanctuaries & MPAs", # Fixed the slash to \n
    y = "Percent of Respondents",
    subtitle = paste0("\u03C7\u00B2(", chi_df3, ") = ", round(chi_stat3, 2), ", p ", fmt_p3)) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), # Added hjust = 0.5
    axis.title.y = element_text(margin = margin(r = 10)),   
    plot.subtitle = element_text(color = "grey40", hjust = 1)
  ) +
  scale_fill_discrete_sequential(palette = "Purp")



ggsave("./doc/QMPA_Purpose_mpa_purpose.png",   width = 6, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)  



# --------------------------------------------------------
# Familiarity wth  MPAs science - QMPA_Science -----------------
d7 <- d1 %>%
  filter(!is.na(QMPA_Science))%>%# Familiarity wth  MPAs -----------------
mutate(QMPA_Science = factor(
  QMPA_Science,
  levels = c(
    "No understanding",
    "Slight understanding",
    "Moderate understanding",
    "Full understanding"
  )))%>%
  glimpse()
unique(d7$QMPA_Science)

counts_QMPA_Science <- d7 %>%
  group_by(QMPA_Science) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_QMPA_Science <- counts_QMPA_Science %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# -- compare expected frequencies to observed values --
chi4 <- chisq.test(props_QMPA_Science$n) # default = equal proportions
chi_stat4 <- unname(chi4$statistic)
chi_df4   <- unname(chi4$parameter)
chi_p4   <- chi4$p.value
fmt_p4 <- ifelse(chi_p4 < .001, "< 0.001", scales::number(chi_p4, accuracy = 0.001)) #  formatted p

resids4 <- chi4$stdres
data.frame(QMPA_Science = props_QMPA_Science$QMPA_Science, Residual = resids4)

# -- graph --
ggplot(props_QMPA_Science, aes(x = QMPA_Science, y = pct, fill = QMPA_Science)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_QMPA_Science$pct) * 1.15)) +
  labs(
    x = "Role of Science for\nSanctuaries & MPAs", # and NMS
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



ggsave("./doc/QMPA_Science_mpa_science.png",   width = 6, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)  











