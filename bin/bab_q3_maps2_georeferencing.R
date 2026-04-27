# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

# goal: check georectifying against survey maps

# notes on codes-----------
# hdn - humboldt del norte
# sn - sonoma
# sb -  santa barbara, pilot
# la - los angeles
# sd - san diego
# op - online prize
# tp - tribal pilot
# ippsc - in person prize southern calfornia

# maps to check against here: https://docs.google.com/spreadsheets/d/1SCQfjCVaj0fk0LSarMIVoUtfmWAvB2THA5O6BIG_91g/edit?usp=sharing


# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("G:/My Drive/research/r_projects/bab_survey_jcs/")

d0<-read_csv("./data/bab_q3_map_calculations_20260227.csv")%>%
  glimpse()

d1<-read_csv("./results/q3_bab_survey_2025_maps_hdn_georect.csv")%>%
  glimpse()
# write_csv(d_sn,"./results/q3_bab_survey_2025_maps_sn_georect.csv")
# d1<-read_csv(d_sb,"./results/q3_bab_survey_2025_maps_sb_georect.csv")
d2<-read_csv("./results/q3_bab_survey_2025_maps_la_georect.csv")%>%
  glimpse()
# write_csv(d_sd,"./results/q3_bab_survey_2025_maps_sd_georect.csv")
# write_csv(d_op,"./results/q3_bab_survey_2025_maps_op_georect.csv")
# write_csv(d_tp,"./results/q3_bab_survey_2025_maps_tp_georect.csv")
# write_csv(d_ippsc,"./results/q3_bab_survey_2025_maps_ippsc_georect.csv")


# humboldt maps ------------
d1_dn<-d1%>%
  filter(map_north=="Del Norte")%>%
  mutate(map=map_north)%>%
  left_join(d0)%>%
  glimpse()

d1_h<-d1%>%
  filter(map_north=="Humboldt")%>%
  mutate(map=map_north)%>%
  left_join(d0)%>%
  glimpse()

d1_m<-d1%>%
  filter(map_north=="Mendicino")%>%
  mutate(map=map_north)%>%
  left_join(d0)%>%
  glimpse()

d1_sf<-d1%>%
  filter(map_north=="San Francisco")%>%
  mutate(map=map_north)%>%
  left_join(d0)%>%
  glimpse()

# la maps ------------
d2_sd<-d2%>%
  filter(map_south=="San Diego")%>%
  mutate(map=map_south)%>%
  left_join(d0)%>%
  glimpse()

d2_oc<-d2%>%
  filter(map_south=="Orange")%>%
  mutate(map=map_south)%>%
  left_join(d0)%>%
  glimpse()

d2_la<-d2%>%
  filter(map_south=="Los Angeles")%>%
  mutate(map="la")%>% # make lowercase
  select(version:region,map,la_x, la_y,year=YEAR)%>%
  left_join(d0)%>%
  mutate(
    x=round(((x2_y2_cell_size*la_x)+x1),2),
    y=round((-(x2_y2_cell_size*la_y)+y1),2)
  )%>%
  glimpse()

d2_v<-d2%>%
  filter(map_south=="Ventura")%>%
  mutate(map=map_south)%>%
  left_join(d0)%>%
  glimpse()

d2_sb<-d2%>%
  filter(map_south=="Santa Barbara")%>%
  mutate(map=map_south)%>%
  select(version:region,map,sb_s_x, sb_s_y,year=YEAR)%>%
  mutate(map="sb")%>% # make lowercase
  left_join(d0)%>%
  mutate(
    x=((x2_y2_cell_size*sb_s_x)+x1),
    y=(-(x2_y2_cell_size*sb_s_y)+y1)
  )%>%
  glimpse()


# graphs --------------------

# save
write_csv(d2_la,"./results/q3_georef_check_la.csv")
write_csv(d2_sb,"./results/q3_georef_check_sb5.csv")
