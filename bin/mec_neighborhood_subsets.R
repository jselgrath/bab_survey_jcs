# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset data for focus groups

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

# d1<-read_csv("./results/data_wide.csv")%>%glimpse()



###Load in Data file
d1<-read_csv("./results/data_wide.csv")%>%
  select(Q1:Format,ResponseId)%>%  # remove metadata from surveys
  glimpse()

d1[d1==""]<-NA

Headers<-d1[1,]%>%
  mutate(Geography=NA)%>%
  glimpse()

# d1<-rbind(Headers, d0)

names(d1)

colnames(d1)
d1[1,]

# -----------------------------
# Subset data to focus groups


##Specify Geographic Bin, Shift this Based on the community you want to look at
d1$Q22<-as.numeric(d1$Q22)%>% # zip code
  glimpse()

# Bayview
db<-d1%>%
  mutate(Geography=ifelse(Q22==94124|Q22==94134, "SF Bayview", "Other"))%>% 
  glimpse()

# "Watsonville/Pajaro/Castroville"
dw<-d1%>%
  mutate(Geography=ifelse(Q22==95019|Q22==95076|Q22==95077|Q22==95012, "Watsonville/Pajaro/Castroville", "Other"))%>% 
  glimpse()

# Santa Barbara (eastside/westside)/Goleta
dsb<-d1%>%
  mutate(Geography=ifelse(Q22==93103|Q22==93101|Q22==93111|Q22==93117, "SB Eastside/SB Westside/Goleta", "Other"))%>% 
  glimpse()

# Ventura/Oxnard/Port Hueneme/ Sta. Paula
dvoh<-d1%>%
  mutate(Geography=ifelse(Q22==93001|Q22==93036|Q22==93030|Q22==93035| #vent ox
Q22==93033|Q22==93041| # port Hueneme
Q22==93060, # Sta Paula
"Ventura/Oxnard/Port Hueneme/ Santa Paula", "Other"))%>% 
  glimpse()




# ---------------------------------------------------------------------------
##transpose Data so that each column is a survey response and each row is a prompt

#bayview
db1<-as.data.frame(t(db))%>%
  mutate(subquestions=V1)%>%
  glimpse()
db1[1:10,]
db1$subquestions

#watsonville
dw1<-as.data.frame(t(dw))%>%
  mutate(subquestions=V1)%>%
  glimpse()

#santa barbara
dsb1<-as.data.frame(t(dsb))%>%
  mutate(subquestions=V1)%>%
  glimpse()

# ventura
dvoh1<-as.data.frame(t(dvoh))%>%
  mutate(subquestions=V1)%>%
  glimpse()


# save data
write_csv(db,"./results/data_wide_bayview.csv")
write_csv(dw,"./results/data_wide_watsonville.csv")
write_csv(dsb,"./results/data_wide_sbarbara.csv")
write_csv(dvoh,"./results/data_wide_ventura.csv")


write_csv(db1,"./results/data_long_bayview.csv")
write_csv(dw1,"./results/data_long_watsonville.csv")
write_csv(dsb1,"./results/data_long_sbarbara.csv")
write_csv(dvoh1,"./results/data_long_ventura.csv")
