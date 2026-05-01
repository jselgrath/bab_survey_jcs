# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: temp version of new responses - for undergrad projects

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
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long4.csv")%>%
  select(ResponseId,UserLanguage,"QMapping_North_County":"Final_Y",Comments,QDemographic_PrimaryZip:QDemographic_Swimming, Mechanism, Version)%>%
  glimpse

unique(d1$Mechanism) #"Online"    "In_Person"

d2<-read_csv("./data/bab_survey_2026_tribal_20260427_20_new.csv")%>%
  select(ResponseId,UserLanguage,Q3:Q3b18_1_y,Comments=Q50,Q37b:Q49, Mechanism)%>%
  mutate(Mechanism=(if_else(Mechanism=="online","Online",Mechanism)))%>% 
  mutate(Mechanism=(if_else(Mechanism=="Paper","In_Person",Mechanism)))%>% 
  mutate(Version="P2_tribal_new")%>%
  glimpse()

unique(d2$Mechanism) #"Online"    "In_Person"

d3<-read_csv("./data/bab_survey_2025_in_person_prize - norcal_20260427_23_new.csv")%>%
  select(ResponseId,UserLanguage,Q3:Q3b18_1_y,Comments=Q50,Q37b:Q49, Mechanism)%>%
  mutate(Version="P2_norcal_ipp_new")%>%
  mutate(Mechanism=(if_else(Mechanism=="Paper","In_Person",Mechanism)))%>% 
  mutate(Mechanism=(if_else(Mechanism=="Tablet","In_Person",Mechanism)))%>% 
  glimpse()

unique(d3$Mechanism)

names(d2)
names(d3)

# new responses -----------

# combine
d3a<-d3[3:nrow(d3),]

d5<-rbind(d2,d3a)%>%
  glimpse()

d6_names<-d5[1,]

d6<-d5[c(3:nrow(d5)),]%>%
  glimpse()

unique(d6$Version)

# make one column for map locations
names(d1)

names(d6)
unique(d6$Q3)

d7<-d6%>%
  # assign counties
  mutate(QMapping_North_County=if_else(Q3=="Northen California",Q3a1,NA))%>%
  mutate(QMapping_Central_County=if_else(Q3=="Central California",Q3a2,NA))%>%
  mutate(QMapping_South_County=if_else(Q3=="Southern California",Q3a3,NA))%>%
  glimpse()




# checkign coordinate column names -----------
filter(d7,QMapping_North_County=="Del Norte")%>%glimpse()
filter(d7,QMapping_North_County=="Humboldt")%>%glimpse()
filter(d7,QMapping_North_County=="Mendicino")%>%glimpse()
filter(d7,QMapping_North_County=="Sonoma")%>%glimpse()
filter(d7,QMapping_North_County=="Marin")%>%glimpse()
filter(d7,QMapping_North_County=="San Francisco")%>%glimpse()
filter(d7,QMapping_Central_County=="San Francisco")%>%glimpse()
filter(d7,QMapping_Central_County=="San Mateo")%>%glimpse()
filter(d7,QMapping_Central_County=="Santa Cruz")%>%glimpse()
filter(d7,QMapping_Central_County=="Monterey")%>%glimpse()
filter(d7,QMapping_Central_County=="San Luis Obispo")%>%glimpse()
filter(d7,QMapping_Central_County=="Santa Barbara")%>%glimpse()

filter(d7,QMapping_South_County=="Santa Barbara")%>%glimpse()
filter(d7,QMapping_South_County=="Ventura")%>%glimpse()
filter(d7,QMapping_South_County=="Los Angeles")%>%glimpse()
filter(d7,QMapping_South_County=="Orange")%>%glimpse()
filter(d7,QMapping_South_County=="San Diego")%>%glimpse()



# vector of empty values -------------
glimpse(d7)

d7$Final_X<-0
d7$Final_Y<-0

# assign coordinates -----------------
d8 <- d7 %>%
  mutate(
    Final_X = case_when(
      QMapping_North_County == "Del Norte"       ~ as.numeric(Q3b18_1_x),
      QMapping_North_County == "Humboldt"        ~ as.numeric(Q3b16_1_x),
      QMapping_North_County == "Mendicino"       ~ as.numeric(Q3b15_1_x),
      QMapping_North_County == "Sonoma"          ~ as.numeric(Q3b14_1_x),
      QMapping_Central_County == "Santa Cruz"    ~ as.numeric(Q3b9_1_x),
      QMapping_Central_County == "San Luis Obispo" ~ as.numeric(Q3b7_1_x),
      QMapping_South_County == "Ventura"         ~ as.numeric(Q3b4_1_x),
      QMapping_South_County == "Los Angeles"     ~ as.numeric(Q3b3_1_x),
      QMapping_South_County == "San Diego"       ~ as.numeric(Q3b1_1_x),
      TRUE ~ NA_real_ # This catches anything else as a numeric NA
    ),
    Final_Y = case_when(
      QMapping_North_County == "Del Norte"       ~ as.numeric(Q3b18_1_y),
      QMapping_North_County == "Humboldt"        ~ as.numeric(Q3b16_1_y),
      QMapping_North_County == "Mendicino"       ~ as.numeric(Q3b15_1_y),
      QMapping_North_County == "Sonoma"          ~ as.numeric(Q3b14_1_y),
      QMapping_Central_County == "Santa Cruz"    ~ as.numeric(Q3b9_1_y),
      QMapping_Central_County == "San Luis Obispo" ~ as.numeric(Q3b7_1_y),
      QMapping_South_County == "Ventura"         ~ as.numeric(Q3b4_1_y),
      QMapping_South_County == "Los Angeles"     ~ as.numeric(Q3b3_1_y),
      QMapping_South_County == "San Diego"       ~ as.numeric(Q3b1_1_y),
      TRUE ~ NA_real_
    )
  ) %>%
  glimpse()

# check - # NAs that are left are real NAs
filter(d8,is.na(Final_X))%>%select(QMapping_North_County:Final_Y,Q3b12_1_x:Q3b18_1_y)

d9<-d8%>%
  select(-Q3,-Q3a1,-Q3a2,-Q3a3)%>%
  select(ResponseId,UserLanguage,QMapping_North_County:Final_Y,Comments,Q37b:Q49,Mechanism,Version)%>%
  glimpse()

colnames(d9)<-colnames(d1)
  glimpse(d9)

d10<-rbind(d1,d9)


# clean education
d11<- d10 %>%
  mutate(
    # normalize punctuation/spacing
    QDemographic_Education = stringr::str_replace_all(QDemographic_Education, "’", "'"),
    QDemographic_Education = stringr::str_squish(QDemographic_Education),
    
    # map by pattern (robust to small text differences)
    QDemographic_Education_clean = dplyr::case_when(
      stringr::str_detect(QDemographic_Education, regex("^some high school|GED", ignore_case = TRUE)) ~ "HS or GED",
      stringr::str_detect(QDemographic_Education, regex("^vocational", ignore_case = TRUE))           ~ "Vocational",
      stringr::str_detect(QDemographic_Education, regex("^some college|associate", ignore_case = TRUE)) ~ "Some college/Associate",
      stringr::str_detect(QDemographic_Education, regex("bachelor", ignore_case = TRUE))              ~ "Bachelor’s",
      stringr::str_detect(QDemographic_Education, regex("^graduate|masters|master's|phd|professional", ignore_case = TRUE)) ~ "Graduate",
      stringr::str_detect(QDemographic_Education, regex("^other$", ignore_case = TRUE))               ~ "Other",
      TRUE ~ NA_character_   # anything unmatched becomes NA so we can drop it
    )
  ) %>%
  select(-QDemographic_Country_State,-QDemographic_Education)%>%
glimpse()


# make one map column, add map abbreviations
d12<-d11%>%
  mutate(QMapping_County = coalesce(
    QMapping_North_County, 
    QMapping_Central_County, 
    QMapping_South_County
  )) %>%
  mutate(map = case_when(
    tolower(QMapping_County) == "san diego"     ~ "san diego",
    tolower(QMapping_County) == "orange"        ~ "orange",
    tolower(QMapping_County) == "los angeles"   ~ "la",
    tolower(QMapping_County) == "ventura"       ~ "ventura",
    tolower(QMapping_County) == "santa barbara" ~ "sb",
    tolower(QMapping_County) == "san luis obispo" ~ "slo",
    tolower(QMapping_County) == "monterey"      ~ "monterey",
    tolower(QMapping_County) == "santa cruz"    ~ "santa cruz",
    tolower(QMapping_County) == "san mateo"     ~ "san mateo",
    tolower(QMapping_County) == "san francisco" ~ "san francisco",
    tolower(QMapping_County) == "alameda"       ~ "alameda",
    tolower(QMapping_County) == "marin"         ~ "marin",
    tolower(QMapping_County) == "sonoma"        ~ "sonoma",
    tolower(QMapping_County) == "mendicino"     ~ "mendocino", # Fixed spelling
    tolower(QMapping_County) == "humboldt"      ~ "humboldt",
    tolower(QMapping_County) == "del norte"     ~ "del norte",
    TRUE ~ NA_character_
  ))


# save
write_csv(d12,"./results/undergrad_projects_20260428.csv")
