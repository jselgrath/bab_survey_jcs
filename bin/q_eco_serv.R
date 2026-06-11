# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing q12 es data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 
library(colorspace)

# --------------------------------------------------------------------------
# INFLUENCER
# --------------------------------------------------------------------------
include_influencers <- FALSE # Set to TRUE or FALSE

# --------------------------------------------------------------------------
# LOAD DATA & PRE-PROCESS
# --------------------------------------------------------------------------
rm(list = setdiff(ls(all = TRUE), "include_influencers")) 
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

d1 <- read_csv("./results/data_long9.csv")

if (include_influencers == FALSE) {
  d1 <- d1 %>% filter(influencer_any_b != 1)
}

# list from questions  
es<-c("support a diversity of marine life",
      "inspire artistic and creative expression",
      "help produce and renew clean air and water",
      "are important for future generations",
      "provide fisheries, recreation, or tourism opportunities that support economic benefit",
      "provide opportunities for education learning and science",
      "help define our heritage culture and identity",
      "are a place for our favorite outdoor recreation activities",
      "help us feel connected to the natural world",
      "care for us when we care for them")%>%
  glimpse()
glimpse(es)



# shorter version
es2<-c("biodiversity",
       "artistic_inspiration",
       "clean_air_water",
       "future_generations",
       "economic_benefit",
       "education_learning_science",
       "heritage_culture_identity",
       "recreation",
       "connected_to_nature",
       "care")%>%
  glimpse()







###Load in Data file -------------------
# d12<-read_csv("./results/q12_es_long.csv")%>%
  # glimpse()

# bayview dataset
# d12<-read_csv("./results/q12_es_long_bayview.csv")%>%
#   mutate(services=Prompt, services2=Ecosystem_Service)%>%
#   mutate(Prompt=as.factor(Prompt))%>%
#   glimpse()

#santa barbara dataset
d12<-read_csv("./results/q12_es_long_ventura.csv")%>%
  mutate(services=Prompt, services2=Ecosystem_Service)%>%
  mutate(Prompt=as.factor(Prompt))%>%
  glimpse()


# ventura dataset
# d12<-read_csv("./results/q12_es_long_ventura.csv")%>%
#   mutate(services=Prompt, services2=Ecosystem_Service)%>%
#   mutate(Prompt=as.factor(Prompt))%>%
#   glimpse()



# summarize -------------------

# n respondents - includes NA --------------
d12c<-d12%>%
  group_by(services,services2)%>%
  summarize(
    n=n())%>%
  glimpse() #587


# responses - includes NA --------------
d12d<-d12%>%
  group_by(services,services2,value)%>%
  summarize(
    n_val=n())%>%
  mutate(pct=round(n_val/d12c$n[1],3))%>%
  glimpse()

# n respondents - no NA --------------
d12e<-d12%>%
  filter(!is.na(value))%>%
  group_by(services,services2)%>%
  summarize(
    n_tot=n())%>%
  glimpse() #587 with NA, ranges without

# responses - no NA --------------
d12f<-d12%>%
  filter(!is.na(value))%>%
  group_by(services,services2,value)%>%
  summarize(
    n_val=n())%>%
  full_join(d12e)%>%
  mutate(pct=round(n_val/n_tot,3))%>%
  glimpse()


# -----------------------------------------------
# prep for graphing ------------------------------
source("./bin/deets.R")


# # order factors ---------------------------
d12f$value <- factor(d12f$value, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))

d12f

#make Disagree and Strongly disagree percentages negative show up of the left side of the y-axis
d12g<-d12f%>%
  group_by(services, services2)%>%
  reframe(pct2=c(pct[value=='Strongly agree'],
                   pct[value=='Agree'],
                   pct[value=='Neutral']/2,
                   -pct[value=='Neutral']/2,
                   -pct[value=='Disagree'],
                   -pct[value=='Strongly disagree']),
          value = c('Strongly agree','Agree', 'Neutral', 'Neutral', 'Disagree','Strongly disagree'),
          overall = sum(pct2)) %>%
  mutate(services2 = reorder(services2, overall))%>%
  # mutate(value = factor(value, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree")))%>%
  glimpse()

d12g$value<-ordered(d12g$value, levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree","Neutral"))


##Order Prompts so the highest level of "Strongly agree" is at the top
Factor_Order<-d12g[which(d12g$value=="Strongly agree"),]
Factor_Order<-Factor_Order[order(Factor_Order$pct2),]
Order<-Factor_Order$services
d12g$services <- ordered(d12g$services, levels=Order)

# graph -------------------------------------
source("./bin/deets.R")

ggplot(d12g, aes(y=services2, x=pct2, fill=value)) + 
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
  ggtitle("Ocean and coastal areas are important to me, \nmy family, and/or my community because they:") +
  deets9

# ggsave("./doc/q13_es.png", width=12, height=4.5, units="in")
ggsave("./doc/q13_es_ventura.png", width=12, height=4.5, units="in")


