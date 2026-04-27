# Equity in Ocean Access (MPAs Equity and Climate (mec))
# Jennifer Selgrath - base code from Tim Frawley
# California Marine Sanctuary Foundation/ CINMS

# goal: organize and clean data from qualtrics

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d1<-read_csv("./data/bab_mec_combined_data_20251023.csv")%>%
  glimpse()
d1
colnames(d1)

###Load in Data file, Change Path Name to where it is stored on your machine
# d1<-read.csv("./data/Combined_Data_Final_1.31.2025.csv")%>% 
  #Combined_Data_8.27.24 #Combined_Data_9.1.24
  # glimpse()

###Data Cleaning ------------------------------------------------------

##Tag Data that are Paper Survey Entries
d2<-d1%>%
  mutate(format0=str_detect(Q32, "PAPER_DATA_ENTRY"))%>%
  mutate(format=if_else(format0==TRUE&YEAR==2024, "Paper","Unknown"))%>%
  mutate(format=if_else(format0==FALSE&YEAR==2024|is.na(format0)&YEAR==2024,"Digital",format))%>%
  select(-format0)%>%
  # filter(Q23!="I do not live in California")%>% # drops about 1/2 responses so should fix
  glimpse()
names(d2)
  
# d2[,c(1,7,236,243,244)]%>%
#   view()

##Save row with question Specific Prompts for later
header<-d2[7,]
header2<-paste0(d2[1,],"_",d2[7,])
header2<-gsub(" ", "", header2) # remove spaces
header2

# ##Remove Digital Responses (i.e., tablet or phone where people spent less than 400 seconds on the Survey)
# d2$format<-ifelse(d2$format=="Digital" & d2$Duration_Seconds < 400, "Bad_Digital", d2$format)
# d2<-d2[which(!d2$format=="Bad_Digital"),]
# 
# ##Remove Paper Data Entries of Low (i.e., 3) Quality
# Paper_Data<-d2[grepl("PAPER_DATA_ENTRY", d2$Q32), , drop = FALSE]
# Bad_Paper_Data<-d2[grepl("DATA_QUALITY: 3", d2$Q32), , drop = FALSE]
# Bad_Data<-as.list(Bad_Paper_Data$ResponseId)
# 
# ###Remove Data where people answered less than 60% of the questions
# d2<-d2[-which(d2$ResponseId %in% Bad_Data), ]
# d2$Progress<-as.numeric(d2$Progress)
# d2<-d2[which(d2$Progress > 60),]

###Remove Data where people are not residents of California
# d2<-d2[which(!d2$Q23=="I do not live in California"),]

##Fix Variable Race Descriptor
d2$Q24<-gsub("Native Hawaiian or Other Pacific Islander", "Native Hawaiian or Pacific Islander", d2$Q24)


###Add Question Specific Header Info Back In
d2<-tibble(rbind(header, d2))%>%
  glimpse()

# transpose --------------
###Load in Data file
d3<-d2%>%
  select(quest_comb=`COMBINED QUESTIONS`,response_id=ResponseId,Q1:format)%>%  # remove metadata from surveys - later could include Q0
  rename_with(~ make.names(
    .x %>%
      str_remove_all(" ") %>%
      str_replace("Q3a2...24", "Q3a2") %>%
      str_replace("Q3a2...27", "Q3a2") %>%
      str_replace("Q30...234", "Q30") %>%
      str_replace("Q30...184", "Q30") %>%
      str_replace("Q31...185", "Q31a") %>%
      str_replace("Q31...235", "Q31b"),
    unique = TRUE
  ))%>%
  glimpse()

questions2<-colnames(d3)


##transpose Data so that each column is a survey response and each row is a prompt
d4<-as.data.frame(t(d3))%>%
  mutate(questions=V1)%>%
  glimpse()
d4[1:10,]

d5<-cbind(d4,questions2)

glimpse(d5)

# export formatted data --------------------------------
write_csv(d3,"./results/data_long.csv")
write_csv(d5,"./results/data_wide.csv")
