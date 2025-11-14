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

d0<-read_csv("./results/q13_headers.csv")%>%
  glimpse() # check order

d0$question<-row.names(d0)
d0<-d0%>%select(-question)
d0
glimpse(d0)
unique(d0)



d0<-gsub("I am interested in ocean experiences or activities","Interest",d0)
d0<-gsub("time_to_visit","Time",d0)
d0<-gsub("I feel welcome at beaches and coastal areas","Feeling_Welcome",d0)
d0<-gsub("The amenities/infrastructure I need or want are available","Amenities_Infrastructure",d0)

d0<-str_replace_all(d0, "\\s*\\([^)]*\\)", "")


d0<-gsub("I have access to and can afford  transportation to the beach or coasts","Transportation",d0)
d0<-gsub("I possess the required skills or knowledge to participate in ocean activities","Skills_Knowledge",d0)
d0<-gsub("I can afford or have access to necessary tools, gear, or equipment","Gear",d0)
d0<-gsub("I feel safe from environmental factors","Environmental_Safety",d0)
d0<-gsub("I understand the associated rules and regulations","Understanding_Rules_Regulations",d0)
d0<-gsub("I feel protected by law or rule enforcers","Feeling_Protected",d0)

# d0<-gsub("I am interested in ocean experiences or activities","Interest",d0)
d0
glimpse(d0)

d1<-read_csv("./results/data_long5.csv")%>%
  select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,Q13_1:Q13_9,Q24,Q25,Q27,Q28,Q31b,YEAR)%>%
  mutate(Q24 = as.character(Q24), Q4 = as.character(Q4), Q5 = as.character(Q5)) %>%
  mutate(across(
    .cols = contains("Q"), # Select columns whose names contain "Q"
    .fns = ~ if_else(.x == "Neither agree nor disagree", "Neutral", .x)
  ))%>%
  glimpse()
d1

d2<-d1%>%
  select(starts_with("Q13"))%>%
  glimpse()

colnames(d2)<-d0
d2
glimpse(d2)

# pivot longer
d2_long <- d2 %>%
  pivot_longer(
    cols = Interest:Feeling_Protected, # Selects all columns from Interest up to Feeling_Protected
    names_to = "barrier",
    values_to = "value"
  )

# n respondents - includes NA --------------
d2c<-d2_long%>%
  group_by(barrier)%>%
  summarize(
    n=n())%>%
  glimpse() 


# responses - includes NA --------------
d2d<-d2_long%>%
  group_by(barrier,value)%>%
  summarize(
    n_val=n())%>%
  mutate(pct=round(n_val/d2c$n[1],3))%>%
  glimpse()

# n respondents - no NA --------------
d2e<-d2_long%>%
  filter(!is.na(value))%>%
  group_by(barrier)%>%
  summarize(
    n_tot=n())%>%
  glimpse() 

# responses - no NA --------------
d2f<-d2_long%>%
  filter(!is.na(value))%>%
  group_by(barrier,value)%>%
  summarize(
    n_val=n())%>%
  full_join(d2e)%>%
  mutate(pct=round(n_val/n_tot,3))%>%
  glimpse()

# -----------------------------------------------
# prep for graphing ------------------------------
source("./bin/deets.R")


# # order factors ---------------------------
d2f$value <- factor(d2f$value, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))

d2f


#make Disagree and Strongly disagree percentages negative show up of the left side of the y-axis
d2g<-d2f%>%
  group_by(barrier)%>%
  reframe(pct2=c(pct[value=='Strongly agree'],
                 pct[value=='Agree'],
                 pct[value=='Neutral']/2,
                 -pct[value=='Neutral']/2,
                 -pct[value=='Disagree'],
                 -pct[value=='Strongly disagree']),
          value = c('Strongly agree','Agree', 'Neutral', 'Neutral', 'Disagree','Strongly disagree'),
          overall = sum(pct2)) %>%
  mutate(barrier = reorder(barrier, overall))%>%
  glimpse()

d2g$value<-ordered(d2g$value, levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree","Neutral"))


##Order Prompts so the highest level of "Strongly agree" is at the top
Factor_Order<-d2g[which(d2g$value=="Strongly agree"),]
Factor_Order<-Factor_Order[order(Factor_Order$pct2),]
Order<-Factor_Order$barrier
d2g$barrier <- ordered(d2g$barrier, levels=Order)

# graph -------------------------------------

ggplot(d2g, aes(y=barrier, x=pct2, fill=value)) + 
  geom_col(orientation = 'y', width = 0.6) +
  geom_bar(stat="identity") +
  scale_fill_manual(
    # values = c( "#fe6100","#ffb000", "grey50", "#648fff", "#785ef0"),
    values = c( "#002F70", "#879FDB", "grey50", "#DA8A8B", "#5F1415"),
    breaks=c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
    name="")+
  geom_vline(xintercept = 0) +
  xlab("% of Respondents") + 
  ylab("")+
  xlim(c(-1,1))+
  theme_bw() + 
  ggtitle("Use and/or experience within ocean and \ncoastal areas in California:")+
  deets9


ggsave("./doc/q13_barrier.png", width=12, height=4.5, units="in")




##Transform Data so that each column is a survey response and each row is a prompt
d3<-d1%>%
  select(quest_comb,response_id,Q1,Q2,Q4,Q5,Q8,Q24,Q25,Q27,Q28,Q31b,YEAR)%>% # no Q13
  glimpse()

d4<-cbind(d3,d2)
glimpse(d4)



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

# -- Transform Data so that each column is a survey response and each row is a prompt
d4<-as.data.frame(t(d3))%>%
  mutate(barrier=V1)%>%
  select(-V1)%>%
  pivot_longer(cols = !c(barrier))%>%
  select(-name)%>% # keep to link to demographics
  # filter(!is.na(value))%>%
  glimpse()
d13a_low


# ----------------------------------
# -- Barriers - Q13 ---------------------------

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
