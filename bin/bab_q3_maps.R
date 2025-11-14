# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

# goal: pull map codes for each survey for figuring out conversions from pixel space to geographic space

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


# --- START OF SCRIPT ---
# 1. Force-kill Google Drive to prevent file locking/sync errors
# system("taskkill /f /im GoogleDriveFS.exe", ignore.stderr = TRUE, ignore.stdout = TRUE)

# Custom function to save CSV while avoiding Drive sync locks
# safe_write_csv <- function(data, path) {
#   success <- FALSE
#   attempts <- 0
#   
#   while(!success && attempts < 5) {
#     tryCatch({
#       write_csv(data, path)
#       success <- TRUE
#     }, error = function(e) {
#       attempts <<- attempts + 1
#       message("Google Drive is busy, retrying in 2 seconds... (Attempt ", attempts, ")")
#       Sys.sleep(2) # Pauses R for 2 seconds to let Drive finish syncing
#     })
#   }
# }


# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("G:/My Drive/research/r_projects/bab_survey_jcs")

d1<-read_csv("./results/data_long5.csv")%>%
  filter(YEAR==2025)%>%
  glimpse()

unique(d1$VERSION)

length(unique(d1$response_id)) # confirm this is unique to all responses


# select ID and map coordinates and version
d2<-d1%>%
  select(VERSION,response_id,VERSION,format,Q3a2:Q3b18_1_y,YEAR)%>%
  select(-Q3_4_TEXT)%>% # remove non-responses
  filter(Q3a2!="",
         Q3a2!="N/A",
         !is.na(Q3a2))%>%
  glimpse()

names(d2)
head(d2)


# List of new column names -- may update
new_cols <- c(
  "version",  "response_id",  "format",  "region",  
  "map_north",  "map_central",  "map_south",   
  "sd_x",    "sd_y",
  "ora_x" ,   "ora_y",    
  "la_x",    "la_y",    
  "ve_x",    "ve_y" ,   
  "sb_s_x",    "sb_s_y",   
  "sb_n_x",    "sb_n_y",
  "slo_x",    "slo_y", 
  "mon_x" ,   "mon_y",    
  "sc_x" ,   "sc_y",    
  "sma_x",    "sma_y" ,   
  "sf_s_x",   "sf_s_y",  
  "sf_n_x",   "sf_n_y",   
  "mar_x",   "mar_y",   
  "son_x" ,  "son_y",   
  "men_x",   "men_y",   
  "hum_x",   "hum_y", 
  "dn_x",   "dn_y",   
  "YEAR")

# update column names -------------
colnames(d2)<-new_cols
d2
# view(d2)
names(d2)

# define core columns for EVERY file for f2------------
core_cols <- c("version", "response_id", "format", "region", "YEAR","map_north",  "map_central",  "map_south")

# Create a list of counties and the regional columns they might appear in
# map_tasks <- list(
#   "San Diego"       = c("map_south"),
#   "Orange"          = c("map_south"),
#   "Los Angeles"     = c("map_south"),
#   "ventura"         = c("map_south"),
#   "Santa Barbara"   = c("map_south", "map_central"), # Both regions
#   "San Luis Obispo" = c("map_central"),
#   "Monterey"        = c("map_central"),
#   "Santa Cruz"      = c("map_central"),
#   "San Mateo"       = c("map_central"),
#   "San Francisco"   = c("map_central", "map_north"), # Both regions
#   "Marin"           = c("map_north"),
#   "Sonoma"          = c("map_north"),
#   "Mendicino"       = c("map_north"), #Mendocino
#   "Humboldt"        = c("map_north"),
#   "Del Norte"       = c("map_north")
# )

# Mapping tasks with their corresponding column prefixes
# (The prefix is the letters before the first "_")
map_tasks <- list(
  "san_diego"       = list(cols = "map_south",    prefix = "sd"),
  "orange"          = list(cols = "map_south",    prefix = "ora"),
  "los_angeles"     = list(cols = "map_south",    prefix = "la"),
  "ventura"         = list(cols = "map_south",    prefix = "ve"),
  "santa_barbara"   = list(cols = c("map_south", "map_central"), prefix = "sb"),
  "san_luis_obispo" = list(cols = "map_central",  prefix = "slo"),
  "monterey"        = list(cols = "map_central",  prefix = "mnt"),
  "santa_cruz"      = list(cols = "map_central",  prefix = "sc"),
  "san_mateo"       = list(cols = "map_central",  prefix = "sma"),
  "san_francisco"   = list(cols = c("map_central", "map_north"), prefix = "sf"),
  "marin"           = list(cols = "map_north",    prefix = "mar"),
  "sonoma"          = list(cols = "map_north",    prefix = "son"),
  "mendicino"       = list(cols = "map_north",    prefix = "men"),
  "humboldt"        = list(cols = "map_north",    prefix = "hum"),
  "del_norte"       = list(cols = "map_north",    prefix = "dn"))


# function to select map from specific survey for developing georectifying conversions
f1<-function(data=d2,version1){
  data%>%
    filter(version==version1)%>%
    glimpse()
}

# function to select map from specific survey for developing georectifying conversions
f2 <- function(data, region_col, map_name) {
  data %>%
    filter(!!sym(region_col) == map_name) %>% # pass column name as a string
    glimpse()
}

# all versions

unique(d2$version)

# -----------------------------------
# breaking apart maps by survey version - for developing georectifying algorithms
# -----------------------------------
#2024
# d_online2<-f1(data=d2,version="Online_2", new_col=new_cols)
# d_online3<-f1(data=d2,version="Online_3", new_col=new_cols)
# d_ucsc<-f1(data=d2,version="UCSC", new_col=new_cols)
# d_ucsb<-f1(data=d2,version="UCSB", new_col=new_cols)
# d_ucsb2<-f1(data=d2,version="UCSB_template", new_col=new_cols)
# d_sf<-f1(data=d2,version="Lucas", new_col=new_cols)

#2025
d_hdn<- f1(data=d2,version1="HDN")
d_sn<-f1(data=d2,version1="SN") # sonoma
d_sb<-f1(data=d2,version1="SB")    # pilot 2025
d_la<-f1(data=d2,version1="LA")
d_sd<-f1(data=d2,version1="SD")

d_op<-f1(data=d2,version1="ONLINE_PRIZE")
d_tp<-f1(data=d2,version1="Tribal_Pilot")
d_ippsc<-f1(data=d2,version1="in_person_prize_socal")



# d_blank<-f1(data=d2,version1="")

# -----------------------------------
# breaking apart coordinates by map for all survey versions from 2025
# -----------------------------------




# This will run the function for every county in the list
# Loop through the list to filter and save
iwalk(map_tasks, function(config, county_label) {
  
  pretty_name <- str_replace_all(county_label, "_", " ") %>% str_to_title()
  
  filtered_data <- d2 %>%
    # 1. Filter rows
    filter(if_any(all_of(config$cols), ~ .x == pretty_name)) %>%
    # 2. Select only core cols + any column starting with the county's prefix
    select(all_of(core_cols), starts_with(config$prefix))
  
  # 3. Save
  if(nrow(filtered_data) > 0) {
    file_path <- paste0("./results/q3_bab_survey_2025_maps_", county_label, ".csv")
    write_csv(filtered_data, file_path)
    message("Saved clean file: ", county_label, " with prefix '", config$prefix, "'")
  }
})


# -------------------
# make a summary table # -------------------
# -------------------
# Create an empty list to store the count results
summary_results <- list()

# Iterate through the tasks and count occurrences
for (county_label in names(map_tasks)) {
  
  # CHANGE HERE: Access the 'cols' sub-element of the config list
  cols <- map_tasks[[county_label]]$cols
  
  # Format the 'Pretty' name for matching (e.g., santa_barbara -> Santa Barbara)
  pretty_name <- str_replace_all(county_label, "_", " ") %>% str_to_title()
  
  # Count the rows (using if_any to handle multiple columns like SF/SB)
  count_val <- d2 %>%
    filter(if_any(all_of(cols), ~ .x == pretty_name)) %>%
    nrow()
  
  # Store in a small dataframe
  summary_results[[county_label]] <- tibble(
    county = pretty_name,
    count = count_val,
    regions = paste(cols, collapse = ", ")
  )
}

# Bind into one table
final_summary <- bind_rows(summary_results)
print(final_summary)


# Save 
write_csv(final_summary, "./doc/q3_bab_survey_2025_summary_report.csv")



# export formatted data --------------------------------
# write_csv(d_online2,"./results/q3_mec_survey_2024_maps_online2.csv")
# write_csv(d_online3,"./results/q3_mec_survey_2024_maps_online3.csv")
# write_csv(d_ucsc,"./results/q3_mec_survey_2024_maps_ucsc.csv")
# write_csv(d_sf,"./results/q3_mec_survey_2024_maps_sf.csv")
# write_csv(d_ucsb,"./results/q3_mec_survey_2024_maps_ucsb.csv")
# write_csv(d_blank,"./results/q3_mec_survey_2024_maps_blank.csv")

# map locations for georectifying by survey
write_csv(d_hdn,"./results/q3_bab_survey_2025_maps_hdn_georect.csv")
write_csv(d_sn,"./results/q3_bab_survey_2025_maps_sn_georect.csv")
write_csv(d_sb,"./results/q3_bab_survey_2025_maps_sb_georect.csv")
write_csv(d_la,"./results/q3_bab_survey_2025_maps_la_georect.csv")
write_csv(d_sd,"./results/q3_bab_survey_2025_maps_sd_georect.csv")
write_csv(d_op,"./results/q3_bab_survey_2025_maps_op_georect.csv")
write_csv(d_tp,"./results/q3_bab_survey_2025_maps_tp_georect.csv")
write_csv(d_ippsc,"./results/q3_bab_survey_2025_maps_ippsc_georect.csv")



