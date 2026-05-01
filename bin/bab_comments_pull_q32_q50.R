# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

# goal: separate open comment question with respondent ID from other data for cleaning

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

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./doc/undergrad_projects_20260428.csv")%>% #("./results/data_long5.csv")%>%
  glimpse()

unique(d1$Version)

length(unique(d1$ResponseId)) # confirm this is unique to all responses


# select ID and comment and version
# d2<-d1%>%
#   select(response_id,VERSION,format,Q32)%>%
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
f1<-function(data=d2,version, new_col=new_cols){
  data%>%
    # filter(VERSION==version)%>%
    filter(Comments!="",
           Comments!="N/A",
           Comments!="No",
           Comments!="no")%>%
    mutate(Comments=gsub("ðŸ˜", "[emogi_Beaming Face With Smiling Eyes]", Comments),
           Comments=gsub("ðŸŒŠ", "[emogi_Wave]", Comments),
           Comments=gsub("â€™", "'", Comments))%>% 
    mutate(!!!setNames(rep(list(NA), length(new_cols)), new_cols))%>%
    glimpse()
}

# all versions

unique(d2$VERSION)

#2024
d_online2<-f1(data=d2,version="Online_2", new_col=new_cols)
d_online3<-f1(data=d2,version="Online_3", new_col=new_cols)
d_ucsc<-f1(data=d2,version="UCSC", new_col=new_cols)
d_ucsb<-f1(data=d2,version="UCSB", new_col=new_cols)
d_ucsb2<-f1(data=d2,version="UCSB_template", new_col=new_cols)
d_sf<-f1(data=d2,version="Lucas", new_col=new_cols)

#2025
d_hdn<-f1(data=d2,version="HDN", new_col=new_cols)
d_sn<-f1(data=d2,version="SN", new_col=new_cols) # sonoma
d_sb<-f1(data=d2,version="SB", new_col=new_cols)    # pilot 2025
d_la<-f1(data=d2,version="LA", new_col=new_cols)
d_sd<-f1(data=d2,version="SD", new_col=new_cols)

d_op<-f1(data=d2,version="ONLINE_PRIZE", new_col=new_cols)
d_tp<-f1(data=d2,version="Tribal_Pilot", new_col=new_cols)
d_ippsc<-f1(data=d2,version="in_person_prize_socal", new_col=new_cols)



d_blank<-f1(data=d2,version="", new_col=new_cols)



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
