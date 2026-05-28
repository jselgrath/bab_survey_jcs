# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: basic analysis and graphs for barriers questions

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long9.csv")%>%
  # filter(!is.na(QBarriers_1)) %>%
  # mutate(QBarriers_1=as.factor(QBarriers_1,
  #   levels = c("Strongly disagree",
  #              "Disagree",
  #              "Neutral",
  #              "Agree",
  #              "Strongly agree")))%>%
  glimpse()
d1$QBarriers_1



d2 <- d1 %>%
  filter(!is.na(QBarriers_1)) %>%
  mutate(QBarriers_1=factor(QBarriers_1,
         levels = c("Strongly disagree",
                    "Disagree",
                    "Neutral",
                    "Agree",
                    "Strongly agree")))%>%
  glimpse()
unique(d2$QBarriers_1)


ggplot(d2, aes(x = QBarriers_1)) + geom_bar()+
  labs(
    x = "Barrier",
    y = "No. Respondents")+
    # title = "Distribution of MPA familiarity",
    # subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
    # chi_df1, ") = ", round(chi_stat1, 2), ", p ", fmt_p1)) +
    theme_minimal(base_size = 18) 

  labs(
    x = "Barrier",
    y = "TBD",
    # title = "Distribution of MPA familiarity",
    # subtitle = paste0("\u03C7\u00B2(", #Chi-squared goodness-of-fit: \u03C7\u00B2(
                      # chi_df1, ") = ", round(chi_stat1, 2), ", p ", fmt_p1)) +
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
  
  
# barriers questions
QBarriers_1:QBarriers_10