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

d1<-read_csv("./results/data_long5.csv")%>%
  select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,Q17:Q32_4,Q20a_5,Q24,Q25,Q27,Q28,Q31b,YEAR)%>%
  mutate(Q24 = as.character(Q24), Q4 = as.character(Q4), Q5 = as.character(Q5)) %>%
  mutate(Q2=if_else(Q2=="Less than once a year (i.e., rarely or never)","Less than once a year",Q2))%>%
  glimpse()
d1


levels(factor(d1$Q24)) # check race categories

# organizing activity categories
activities <- c(
  "Swimming or bodysurfing",
  "Group or family gatherings or activities",
  "Meditation/Reading/Relaxing",
  "Enjoy the views/sunsets from car",
  "Fishing or collecting food",
  "Beach games or sports",
  "Surfing",
  "Observing or photographing nature",
  "Bicycling/Roller skating/Skateboarding",
  "Snorkeling/Scuba Diving",
  "Walking or running",
  "Sailing/Boating",
  "Paddleboarding/Kiteboarding/Kayaking",
  "Paid work",
  "Festivals",
  "Cultural or religious ceremonies",
  "Another activity",
  "Volunteering"
)

activities_ordered <- c(
  sort(activities[activities != "Another activity"]),
  "Another activity"
)

activities_ordered


# -- select Activity and Frequency questions --
d3<-d1%>%
  # filter(Q17!=4) %>% # error - unsure of source
  select(response_id,Q1,Q2,Q4,Q5)%>%
  glimpse()

unique(d3$Q1)
unique(d3$Q2)
unique(d3$Q4)
unique(d3$Q5)

# -- relevel and combine categories from 2024 data to match 2025 data, or to collapse small categories -------------------

# Activities -----------------
# d4 <- d3 %>%
#   filter(!is.na(Q4)) %>%
#   glimpse()
# unique(d4$Q4)



# ----------------------------------
# -- Activity graphs - Q5 ---------------------------

# -- organize --
counts_activity <- d3 %>%
  filter(!is.na(Q5))%>%
  filter(Q5!="Choose not to answer")%>%
  mutate(
    Q5 = fct_relevel(Q5,
                     sort(unique(Q5[Q5 != "Another activity"])),
                     "Another activity"))%>%
  group_by(Q5) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_activity <- counts_activity %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()


# -- compare expected frequencies to observed values --
chi1 <- chisq.test(props_activity$n) # default = equal proportions
chi_stat1 <- unname(chi1$statistic)
chi_df1   <- unname(chi1$parameter)
chi_p1   <- chi1$p.value
fmt_p1 <- ifelse(chi_p1 < .001, "< 0.001", scales::number(chi_p, accuracy = 0.001)) #  formatted p

resids1 <- chi1$stdres
data.frame(Q5 = props_activity$Q5, Residual = resids1)


# -- graph --
ggplot(props_activity, aes(x = Q5, y = pct, fill = Q5)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_activity$pct) * 1.15)) +
  labs(
    x = "Most Important Activity",
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


ggsave("./doc/Q5_activity.png",   width = 15, height = 7,     # size in inches
  units = "in",              # "in", "cm", or "mm"
  dpi = 300,                 # resolution (300+ for publication quality)
  bg = "white"               # background color (use "transparent" if needed)
)



# Q4 --------------------
# Explode multi-select activity (Q4) so multi-activity respondents count in each chosen activity ----
d4 <- d3 %>%
  mutate(Q4 = as.character(Q4)) %>%
  tidyr::separate_rows(Q4, sep = ",") %>%
  mutate(Q4 = stringr::str_trim(Q4)) %>%
  filter(
    !is.na(Q4), Q4 != "",
    !Q4 %in% c("Choose not to answer") #, "Another activity"
  ) %>%
  mutate(
    Q4 = fct_relevel(Q4,
                     sort(unique(Q4[Q4 != "Another activity"])),
                     "Another activity"))%>%
  distinct(response_id, Q4) %>%               # avoid duplicates
  glimpse()


# -- Activity graphs - Q4 ---------------------------

# -- organize --
counts_activity_q4 <- d4 %>%
  filter(!is.na(Q4))%>%
  filter(Q4!="Choose not to answer")%>%
  group_by(Q4) %>%
  summarise(n = n_distinct(response_id), .groups = "drop")

props_activity_q4 <- counts_activity_q4 %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()


# -- compare expected frequencies to observed values --
chi1_q4 <- chisq.test(props_activity_q4$n) # default = equal proportions
chi_stat1_q4 <- unname(chi1_q4$statistic)
chi_df1_q4   <- unname(chi1_q4$parameter)
chi_p1_q4   <- chi1_q4$p.value
fmt_p1_q4 <- ifelse(chi_p1_q4 < .001, "< 0.001", scales::number(chi_p_q4, accuracy = 0.001)) #  formatted p

resids1_q4 <- chi1_q4$stdres
data.frame(Q4 = props_activity_q4$Q4, Residual = resids1_q4)


# -- graph --
ggplot(props_activity_q4, aes(x = Q4, y = pct, fill = Q4)) +
  geom_col(show.legend = FALSE) +  # hide redundant legend (optional)
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  ylim(0,.35)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(props_activity$pct) * 1.15)) +
  labs(
    x = "Activities",
    y = "Percent of Respondents",
    subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
                      chi_df1_q4, ") = ", round(chi_stat1_q4, 2), ", p ", fmt_p1_q4)) +
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


ggsave("./doc/Q4_activity.png",  width = 15, height = 7,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution (300+ for publication quality)
       bg = "white"               # background color (use "transparent" if needed)
)
