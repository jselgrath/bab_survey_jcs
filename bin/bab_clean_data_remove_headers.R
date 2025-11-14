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
  glimpse()

colnames(d1)[colnames(d1) == "Q21a_2"] <- "Q13_10"
  
# -- remove first few rows which contain question info
d2<-d1[-c(1:8),]

# -- select barrier prompts --
d3<-d1%>%
  select(starts_with("Q13"))%>%
  glimpse()

d4<-d3[c(1),]%>%glimpse()

write_csv(d2,"./results/data_long5.csv")
write_csv(d4,"./results/q13_headers.csv")
