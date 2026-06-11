# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath
# California Marine Sanctuary Foundation/ CINMS
#-------------------------------------

# goal: organize and clean data from qualtrics
# add variables for coastal counties and CDFW regions

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)


# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./data/Merged_Cleaned_QC_Final_5.8.2026.csv")%>% #Merged_Cleaned_QC_Final_v1.csv
  mutate(QActual_Time=if_else(QActual_Time=="Less than once a year (i.e., rarely or never)","Less than once a year",QActual_Time))%>%
  mutate(response_id=ResponseId)%>%
  mutate(QE_Grocery=QE_Groccery)%>% #typo
  dplyr::select(-QE_Groccery)%>%
  glimpse()

# fix zips that are typos
unique(d1$QDemographic_PrimaryZip)
d1$QDemographic_PrimaryZip[d1$QDemographic_PrimaryZip=="75012"]<-"95012"
d1$QDemographic_PrimaryZip[d1$QDemographic_PrimaryZip=="82691"]<-"92691"

d1
colnames(d1)

##Save row with question Specific Prompts for later
# view(d1[1,]) # prompts
tail(d1[2,]) # I think this row is not needed

# questions
d_questions<-d1[1,]

#misc, not needed row
d1[2,]

#remove questions and misc row  
d4<-d1[3:nrow(d1),]
glimpse(d4)  

# remove people who took survey 2xby email
d5 <- d4 %>%
  # Convert emails to lowercase to ensure "User@me.com" matches "user@me.com"
  mutate(Email = tolower(Email)) %>% 
  # keeps only the first occurrence of each email
  filter(!duplicated(Email) | is.na(Email))%>%
  glimpse()

# duplicate entries from paper surveys
d6<-d5%>%
  filter(response_id!="R_32beB27wLsOJ2lS"|response_id!="R_5zBazkNnoYksEJb"|response_id!="R_50nPaVBB5fep6pd")%>%

  # add coastal county variable
  mutate(county_coastal=if_else(Primary_County=="San Diego"| Primary_County=="Orange" | Primary_County=="Los Angeles"  |
  Primary_County=="Ventura"| Primary_County=="Santa Barbara" | Primary_County=="San Luis Obispo"  |
  Primary_County=="Monterey"| Primary_County=="Santa Cruz" | Primary_County=="San Mateo"  |
  Primary_County=="San Francisco"| Primary_County=="Marin" | Primary_County=="Sonoma"  |
  Primary_County=="Mendocino"| Primary_County=="Humboldt" | Primary_County=="Del Norte", "Coastal","Inland"))%>%
  
  # set counties to CDFW regions
  mutate(
    county_cdfw_region = case_when(
      # 1 - Northern Region
      Primary_County %in% c("Del Norte", "Humboldt", "Lassen", "Mendocino", "Modoc", 
                    "Shasta", "Siskiyou", "Tehamas", "Tehama", "Trinity") ~ "Northern",
      
      # 2 - North Central Region
      Primary_County %in% c("Alpine", "Amador", "Butte", "Calaveras", "Colusa", 
                    "El Dorado", "Glenn", "Lake", "Nevada", "Placer", 
                    "Plumas", "Sierra", "Sutter", "Yolo", "Yuba") ~ "North Central",
      
      # 3 - Bay Delta Region
      Primary_County %in% c("Alameda", "Contra Costa", "Marin", "Napa", "Sacramento", 
                    "San Mateo", "Santa Clara", "Santa Cruz", "San Francisco", 
                    "Solano", "Sonoma") ~ "Bay Delta",
      
      # 4 - Central Region
      Primary_County %in% c("Fresno", "Kern", "Kings", "Madera", "Mariposa", 
                    "Merced", "Monterey", "San Benito", "San Luis Obispo", 
                    "Stanislaus", "Tulare", "Tuolumne") ~ "Central",
      
      # 5 - South Coast Region
      Primary_County %in% c("Los Angeles", "Orange", "San Diego", "Santa Barbara", 
                    "Ventura") ~ "South Coast",
      
      # 6 - Inland Deserts Region
      Primary_County %in% c("Imperial", "Inyo", "Mono", "Riverside", 
                    "San Bernardino") ~ "Inland Deserts",
      
      # Fallback case handling for spelling errors/missing fields
      TRUE ~ "Unknown/Other"))%>%


  glimpse()


unique(d6$Primary_County)



# export formatted data --------------------------------
write_csv(d4,"./results/data_long_duplicate_emails.csv")
write_csv(d6,"./results/data_long.csv")
write_csv(d_questions,"./results/data_questions.csv")
