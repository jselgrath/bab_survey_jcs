# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: subset cleaned data for graphing and making long version - ecosystem services

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# note in cases where write in text fits two categories, then it is assigned the first one that matches

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# comments - for activities and dogs
d0<-read_csv("./results/q32_bab_comments_cleaned_demographics_not_all_versions.csv")%>%
  select(ResponseId,comment_clean,use_activity)%>%
  glimpse()

d1a<-read_csv("./results/data_long4.csv")%>%
  # select(ResponseId,QDesired_Time,QActual_Time,QImportant_Activities,QImportant_Activities_TEXT,QImportant_Activities_Most,QImportant_Activities_Most_TEXT,QTransport_Time,QActivity_Mentor,QActivity_Mentor_TEXT,
         # QActivity_Companion,QFishing,QFishing_Type,QDemographic_Home:QDemographic_Swimming,Version,Mechanism,EJ_Bin,Distance,Distance_Binned)%>%
  # mutate(QActual_Time4 = as.character(QActual_Time4), Q4 = as.character(Q4), Q5 = as.character(Q5)) %>%
  mutate(QActual_Time=if_else(QActual_Time=="Less than once a year (i.e., rarely or never)","Less than once a year",QActual_Time))%>%
  glimpse()
d1a

# check for important activities
sum(is.na(d1a$QImportant_Activities))
sum(!is.na(d1a$QImportant_Activities))
unique(d1a$QImportant_Activities)

d1<-d1a%>%
  left_join(d0)%>%
  glimpse

glimpse(d1)

names(d1)

# -- check Activity and Frequency questions --
unique(d1$QDesired_Time)
unique(d1$QActual_Time)
unique(d1$QImportant_Activities)
unique(d1$QImportant_Activities_TEXT)
unique(d1$QImportant_Activities_Most)


# cleaning write in answers ------------------
standardize_to_official <- function(text) {
  text <- str_to_lower(text)
  
  case_when(
    # --- 1. Active Recreation ---
    str_detect(text, "surf") ~ "Surfing",
    str_detect(text, "swim|bath|soak|cool|boogey|boogie") ~ "Swimming or bodysurfing",
    str_detect(text, "walk|run|hiking|m.s walks|dog|puppy|dig|perr|犬|狗") ~ "Walking or running",
    str_detect(text, "snorkeling|scuba|freediving|diving") ~ "Snorkeling/Scuba Diving",
    str_detect(text, "paddle|kayak|canoe|rowing|outrigger|row") ~ "Paddleboarding/Kiteboarding/Kayaking",
    str_detect(text, "sail|boat|ship|maritime") ~ "Sailing/Boating",
    str_detect(text, "bicycling|roller|skate|skating|atv|roll") ~ "Bicycling/Roller skating/Skateboarding",
    str_detect(text, "games|sports|baseball|bat|kite") ~ "Beach games or sports",
    
    # --- 2. Views & Nature ---
    str_detect(text, "view|sunset|admire|looking at ocean") ~ "Enjoy the views/sunsets from car",
    str_detect(text, "tide|observing|photo|nature|docent|birding|beach combing|looking at ocean life|aquarium|exploring|agetes|monitoring|research|shell ") ~ "Observing or photographing nature",
    str_detect(text, "relax|meditation|read|vibing|nap|sunbathing|sun tan|yoga|quiet|art|paint|singing|create|people watching|sketch|breeze|retirement|listening") ~ "Meditation/Reading/Relaxing",
    
    # --- 3. Social/Cultural/Work ---
    str_detect(text, "fishing|collecting food|clam|crab|spearfishing") ~ "Fishing or collecting food",
    str_detect(text, "ceremon|indigenous|native|lakota|land back|spirit|religious|prayer|offering|ancestor") ~ "Cultural or religious ceremonies",
    str_detect(text, "festival") ~ "Festivals",
    str_detect(text, "group|family|gathering|kids|friend|visitors|school|party|drum circle|danc|bonfire|dine|visit|foodie|drinking|Dining") ~ "Group or family gatherings or activities",
    str_detect(text, "volunteer|clean|restoration|trash|invasive|dándoles|unpaid") ~ "Volunteering",
    str_detect(text, "job|income|paid work|navy|work|coach|institute|commercial") ~ "Paid work",
    
    is.na(text) ~ NA_character_,
    TRUE ~ "Another activity"
  )
}





# Helper function to merge two comma-separated strings -------------------------
merge_activities <- function(original, cleaned) {
  # Map over the two vectors
  map2_chr(original, cleaned, function(x, y) {
    # If both are NA, return NA
    if (is.na(x) && is.na(y)) return(NA_character_)
    
    # Combine into a single vector, splitting by comma
    combined <- c(
      str_split(x, ",\\s*")[[1]], 
      str_split(y, ",\\s*")[[1]]
    )
    
    # Remove NAs, empty strings, and duplicates
    unique_list <- combined[!is.na(combined) & combined != ""] %>% 
      unique() %>% 
      str_trim()
    
    # Paste back together with a single comma and space
    if (length(unique_list) == 0) return(NA_character_)
    paste(unique_list, collapse = ", ")
  })
}


# run function for activities -----------------------------------------
d2 <- d1 %>%
  mutate(
    # cleaned version of the text 
    QImportant_Activities_Cleaned = standardize_to_official(QImportant_Activities_TEXT),
    
    # merge text answers with main list
    QImportant_Activities2 = merge_activities(QImportant_Activities, QImportant_Activities_Cleaned),
    
    # remove "Another activity" ONLY IF we successfully added a new cleaned category
    #  use regex to handle potential variations in commas/spacing
    QImportant_Activities2 = if_else(
      !is.na(QImportant_Activities_Cleaned) & QImportant_Activities_Cleaned != "Another activity",
      str_remove(QImportant_Activities2, "Another activity"),
      QImportant_Activities2)
  ) %>%
  
  # Clean up commas left by the removal
  mutate(
    QImportant_Activities2 = QImportant_Activities2 %>%
      str_replace_all(",\\s*,", ",") %>%    # Replace double commas with single
      str_remove("^\\s*,\\s*") %>%           # Remove leading comma
      str_remove("\\s*,\\s*$") %>%           # Remove trailing comma
      str_squish())%>%                           # Remove extra internal whitespace
  

  # most important activities
  mutate(
    # Create the refined 'Most Important' column
    QImportant_Activities_Most2 = case_when(
      # 1. If 'Most' is "Another activity", replace it with our cleaned write-in
      QImportant_Activities_Most == "Another activity" & !is.na(QImportant_Activities_Cleaned) ~ QImportant_Activities_Cleaned,
      
      # 2. If 'Most' is "Another activity" but no clean category was found, keep original
      # (This preserves the data even if our regex didn't catch the specific activity)
      QImportant_Activities_Most == "Another activity" & is.na(QImportant_Activities_Cleaned) ~ "Another activity",
      
      # 3. Otherwise, keep the original checkbox selection
      TRUE ~ QImportant_Activities_Most
    ),

  
  
    # add columns for sub-categories ----------
   
    is_spiritual_cultural = if_else(
      QImportant_Activities_Cleaned == "Cultural or religious ceremonies" | 
        str_detect(QImportant_Activities, fixed("Cultural or religious ceremonies")) |
        str_detect(str_to_lower(QImportant_Activities_TEXT), "spirit|religious|meditation|drum circle|singing|yoga|lakota|prayer|offering|ancestor|dance|ceremon|indigenous|native"),
      "Spiritual/Cultural Use", "Non-spiritual use", missing = "Non-spiritual use"
    ),
    
    # B. SCIENTIFIC RESEARCH FLAG
    is_research = if_else(
      QImportant_Activities_Cleaned == "Scientific Research" | 
        str_detect(str_to_lower(QImportant_Activities_TEXT), "research|monitor|science|field|condor|salmon|whale|clam survey|intertidal work|transect|ecology|marine mammal"),
      "Research/Monitoring", "Non-research use", missing = "Non-research"
    ),
    
    # C. DOG FLAG
    is_dog_activity = if_else(
      str_detect(str_to_lower(QImportant_Activities_TEXT), "dog|puppy|dig|perr|犬|狗") | 
        str_detect(str_to_lower(comment_clean), "dog|puppy|canine|pet|perr|犬|狗"), 
      "Dog related", "No dog mentioned", missing = "No dog mentioned"
    ),
    
    # D. CIVIC ENGAGEMENT FLAG
    is_civic_engagement = if_else(
      QImportant_Activities_Cleaned == "Volunteering" |
        str_detect(QImportant_Activities, fixed("Volunteering")) |
        str_detect(str_to_lower(QImportant_Activities_TEXT), "activism|restoration|clean up|stewardship|land back"),
      "Civic/Stewardship use", "Non-civic use", missing = "Non-civic use"
    ),
    
    # E. CONSUMPTIVE FLAG
    is_consumptive = if_else(
      QImportant_Activities_Cleaned == "Fishing or collecting food" | 
        str_detect(QImportant_Activities, fixed("Fishing or collecting food")) |
        str_detect(str_to_lower(QImportant_Activities_TEXT), "collect shell|glass|rock hunt|agetes|渔|钓"), 
      "Consumptive", "Non-consumptive", missing = "Non-consumptive"
    )
  ) %>%
  
  # formatting clean up
  mutate(QImportant_Activities2 = str_replace_all(QImportant_Activities2, ", ,", ","),
         QImportant_Activities2 = str_replace_all(QImportant_Activities2, ",,", ","),
         QImportant_Activities = str_replace_all(QImportant_Activities, ",,", ","),
         QImportant_Activities2 = str_remove(QImportant_Activities2, "^, "))%>%
  glimpse()





# check----------------------------
filter(d2,is.na(QImportant_Activities_Cleaned))%>%
  glimpse()

d2 %>% 
  filter(!is.na(QImportant_Activities_TEXT)) %>% 
  select(QImportant_Activities, QImportant_Activities_TEXT, QImportant_Activities2) %>% 
  head(20)


# See how well it worked
table(d2$QImportant_Activities_Cleaned)
table(d2$QImportant_Activities)
table(d2$is_spiritual_cultural)
table(d2$is_consumptive)



# check other ways
d2%>%select(QImportant_Activities_TEXT,QImportant_Activities2)%>%view()


# save ------------
write_csv(d2,"./results/data_long5.csv")
