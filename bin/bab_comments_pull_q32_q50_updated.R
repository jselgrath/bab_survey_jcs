# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

# goal: separate open comment question with respondent ID from other data for cleaning
# updated function - done after most surveys were cleaned except last three

# notes on codes-----------
# hdn - humboldt del norte
# sn - sonoma
# sb -  santa barbara, pilot
# la - los angeles
# sd - san diego
# op - online prize
# tp - tribal pilot
# ippsc - in person prize southern calfornia


# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(textclean)
library(stringi)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./results/undergrad_projects_20260428c.csv")%>% #("./results/data_long5.csv")%>%
  glimpse()

unique(d1$Version)

length(unique(d1$ResponseId)) # confirm this is unique to all responses


# select ID and comment and Version
# d2<-d1%>%
#   select(response_id,Version,format,Q32)%>%
#   glimpse()



# List of new column names -- may update
new_cols <- c(
  "comments_clean",
  "language",
  "paper",
  "barriers",
  "climate_change",
  "concerns",
  "joy",
  "management",
  "marine_life",
  "mobility",
  "mpa_sanctuary",
  "ocean_values",
  "places",
  "relationship",
  "social_change",
  "solutions",
  "use_activity",
  "wellbeing",
  "survey_appreciation",
  "quote_noteworthy",
  "other_fill_in"
)


# function to remove blank lines and add new text
f1 <- function(data = d1, Version, new_col = new_cols) {
  data %>%
    # Filter for the specific versions 
    filter(Version %in% .env$Version) %>% 
    
    # Filter out empty comments
    filter(!Comments %in% c("", "N/A", "No", "no")) %>%
    
    mutate(
      # Standardize encoding to UTF-8 using stringi 
      Comments = stringi::stri_encode(Comments, from = NULL, to = "UTF-8"),
      
      # 4. Fix common mojibake (like â€™ to ')
      Comments = replace_non_ascii(Comments), 
      
      # 5. Automatically replace ALL emojis with text descriptions
      Comments = replace_emoji(Comments),
      
      # 6. Manual fixes for any lingering "broken" strings from your specific dataset
      Comments = gsub("â€™", "'", Comments, fixed = TRUE)
    ) %>%
    
    # 7. Add the new columns
    mutate(!!!setNames(rep(list(NA), length(new_col)), new_col)) %>%
    glimpse()
}

# all Versions

unique(d1$Version)

#2024
d_online2<-f1(data=d1,Version="P1_V2_Online", new_col=new_cols)
d_online3<-f1(data=d1,Version="P1_V3_Online", new_col=new_cols)
d_ucsc<-f1(data=d1,Version="P1_UCSC_V2", new_col=new_cols)
d_ucsb<-f1(data=d1,Version="P1_UCSB_V2", new_col=new_cols)
d_ucsb2<-f1(data=d1,Version="P2_Template", new_col=new_cols) #UCSB_template 
d_sf<-f1(data=d1,Version="P1_Lucas_V2", new_col=new_cols)

#2025
d_hdn<-f1(data=d1,Version="P2_HDN_Honor.", new_col=new_cols)
d_sn<-f1(data=d1,Version="P2_Sonoma_Honor.", new_col=new_cols) # sonoma
d_sb<-f1(data=d1,Version="P2_SB_Honor.", new_col=new_cols)    # pilot 2025
d_la<-f1(data=d1,Version="P2_LA_Honor.", new_col=new_cols)
d_sd<-f1(data=d1,Version="P2_SD_Honor.", new_col=new_cols)

d_op<-f1(data=d1,Version="P2_Online_Prize", new_col=new_cols)
d_tp<-f1(data=d1,Version="P2_Tribal_Pilot", new_col=new_cols)

# not done yet - list
target_versions <- c("P2_NorCal_Prize", "P2_tribal_new", "P2_norcal_ipp_new")
d_ippnc <- f1(data = d1, Version = target_versions, new_col = new_cols)%>%
  glimpse()

# check
unique(d_ippnc$Version)
# d_blank<-f1(data=d1,Version="", new_col=new_cols)



# export formatted data --------------------------------
write_csv(d_online2,"./results/q32_mec_survey_2024_comments_online2.csv")
write_csv(d_online3,"./results/q32_mec_survey_2024_comments_online3.csv")
write_csv(d_ucsc,"./results/q32_mec_survey_2024_comments_ucsc.csv")
write_csv(d_sf,"./results/q32_mec_survey_2024_comments_sf.csv")
write_csv(d_ucsb,"./results/q32_mec_survey_2024_comments_ucsb.csv")
write_csv(d_blank,"./results/q32_mec_survey_2024_comments_blank.csv")

write_csv(d_hdn,"./results/q32_bab_survey_2025_comments_hdn.csv")
write_csv(d_sn,"./results/q32_bab_survey_2025_comments_sn.csv")
write_csv(d_sb,"./results/q32_bab_survey_2025_comments_sb.csv")
write_csv(d_la,"./results/q32_bab_survey_2025_comments_la.csv")
write_csv(d_sd,"./results/q32_bab_survey_2025_comments_sd.csv")
write_csv(d_op,"./results/q32_bab_survey_2025_comments_op.csv")
write_csv(d_tp,"./results/q32_bab_survey_2025_comments_tp.csv")
write_csv(d_ippsc,"./results/q32_bab_survey_2025_comments_ippsc.csv")

write_csv(d_ippnc,"./results/q32_bab_survey_2025_comments_ippnc_tribal2.csv")

# remove demographics - save simpler version for students
d_ippnc2<-d_ippnc%>%
  select(ResponseId,UserLanguage,Comments,Mechanism,Version)

write_csv(d_ippnc2,"./results/q32_bab_survey_2025_comments_ippnc_tribal2_simple.csv")
