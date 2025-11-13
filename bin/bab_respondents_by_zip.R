# Jennifer Selgrath
# NOAA CINMS / CMSF
#
# GOAL: summarize counts of survey respondents by zip code
# ---------------------------------------
library(tidyverse)
# ---------------------------------------

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

d1<-read_csv("./data/bab_mec_combined_data_20251023.csv")%>%
  glimpse()


# range of CA zip codes
# (a) ZIP (column Q22) between 90001 y 96162 (CA ZIP codes) *

d1$Q22

d2<-d1%>%
  mutate(Q22=as.numeric(Q22))%>%
  filter(Q22>=90001&Q22<=96162)%>%
  group_by(Q22)%>%
  summarize(respondent_n=length(Q22))%>%
  select(zip_code=Q22,respondent_n)%>%
  glimpse()

write_csv(d2,"./results/zip_sample_size.csv")
