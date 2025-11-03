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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")

###Load in Data file, Change Path Name to where it is stored on your machine
Data<-read.csv("./data/Combined_Data_Final_1.31.2025.csv")%>% 
  #Combined_Data_8.27.24 #Combined_Data_9.1.24
  glimpse()

###Data Cleaning ------------------------------------------------------

##Tag Data that are Paper Survey Entries
Data$Format<-str_detect(Data$Q32, "PAPER_DATA_ENTRY")
Data$Format<-ifelse(Data$Format==TRUE, "Paper", "Digital")

##Save row with question Specific Prompts for later
Headers<-Data[1,]

##Remove Digital Responses (i.e., tablet or phone where people spent less than 500 seconds on the Survey)
Data$Format<-ifelse(Data$Format=="Digital" & Data$Duration_Seconds < 500, "Bad_Digital", Data$Format)
Data<-Data[which(!Data$Format=="Bad_Digital"),]

##Remove Paper Data Entries of Low (i.e., 3) Quality
Paper_Data<-Data[grepl("PAPER_DATA_ENTRY", Data$Q32), , drop = FALSE]
Bad_Paper_Data<-Data[grepl("DATA_QUALITY: 3", Data$Q32), , drop = FALSE]
Bad_Data<-as.list(Bad_Paper_Data$ResponseId)

###Remove Data where people answered less than 60% of the questions
Data<-Data[-which(Data$ResponseId %in% Bad_Data), ]
Data$Progress<-as.numeric(Data$Progress)
Data<-Data[which(Data$Progress > 60),]

###Remove Data where people are not residents of California
Data<-Data[which(!Data$Q23=="I do not live in California"),]

##Fix Variable Race Descriptor
Data$Q24<-gsub("Native Hawaiian or Other Pacific Islander", "Native Hawaiian or Pacific Islander", Data$Q24)


###Add Question Specific Header Info Back In
Data<-tibble(rbind(Headers, Data))%>%
  glimpse()

##Transform Data so that each column is a survey response and each row is a prompt
Data2<-as.data.frame(t(Data))%>%
  mutate(subquestions=V1)%>%
  glimpse()

# make empty cells NA
Data2[Data2==""]<-NA 

Data2$subquestions

names(Data2)

# export formatted data --------------------------------
write_csv(Data,"./results/data_wide.csv")
write_csv(Data2,"./results/data_long.csv")
