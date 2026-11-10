# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: plot maps of responses by state
# ------------------
library(tidyverse)
library(sf)
library(tigris)
library(zipcodeR)      # For fast ZIP -> State mapping
library(colorspace)    # For sequential_hcl()
library(ggspatial)     # For annotation_north_arrow and annotation_scale


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<-read_csv("./data/Merged_and_Cleaned_10_2_2026.csv")%>%
  glimpse()

unique(d1$QDemographic_Home)

# remove headers -------
d2<-d1%>%
  select(ResponseId,QDemographic_Home,QDemographic_PrimaryZip,QDemographic_Country_State,QDemographic_CA_Years)%>%
  filter(ResponseId!="Response ID"& ResponseId!="{\"ImportId\":\"_recordId\"}")%>%
  glimpse()

# check
d2%>%filter(QDemographic_Home!="California")

# -------------------------------------------------------------------------
# 1. Map ZIP Codes to States & Count Respondents
# -------------------------------------------------------------------------

# Clean and standardize ZIP codes (pad with leading zeros to 5 digits)
survey_states <- d1 %>%
  # Filter only US respondents if necessary using QDemographic_Country_State
  filter(!is.na(QDemographic_PrimaryZip)) %>%
  mutate(
    clean_zip = str_extract(as.character(QDemographic_PrimaryZip), "^\\d{3,5}"),
    clean_zip = sprintf("%05d", as.numeric(clean_zip))
  ) %>%
  # Join with zipcodeR reference database to get state abbreviations
  left_join(
    zipcodeR::zip_code_db %>% select(zipcode, state),
    by = c("clean_zip" = "zipcode")
  )

# Summarize counts by state abbreviation
state_counts <- survey_states %>%
  filter(!is.na(state)) %>%
  count(state, name = "count")

# -------------------------------------------------------------------------
# 2. Prepare Spatial Data (Shifting AK & HI for standard US layouts)
# -------------------------------------------------------------------------

# tigris::states(cb = TRUE) downloads generalized boundaries
# shift_geometry() positions Alaska and Hawaii neatly below the Southwest
us_states_sf <- tigris::states(cb = TRUE, resolution = "20m") %>%
  shift_geometry() %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) # Keep 50 states + DC

# -------------------------------------------------------------------------
# 3. Join Counts and Bin
# -------------------------------------------------------------------------

us_map_binned <- us_states_sf %>%
  left_join(state_counts, by = c("STUSPS" = "state")) %>%
  mutate(
    count = replace_na(count, 0),
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count >= 1  & count <= 5  ~ "1-5",
      count >= 6  & count <= 10 ~ "6-10",   
      count >= 11 & count <= 20 ~ "11-20",  
      count > 20                ~ ">3,800"  
    ),
    count_cat = factor(
      count_cat,
      levels = c("0", "1-5", "6-10", "11-20", ">3,800")
    )
  )%>%
  glimpse()

# -------------------------------------------------------------------------
# 4. Generate the Map
# -------------------------------------------------------------------------

sunset_colors_us <- sequential_hcl(5, "SunsetDark", rev = TRUE)

p_us <- ggplot() +
  geom_sf(
    data = us_map_binned,
    aes(fill = count_cat),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = c(
      ">3,800" = sunset_colors_us[5],
      "11-20"  = sunset_colors_us[3],
      "6-10"   = sunset_colors_us[2],
      "1-5"    = sunset_colors_us[1],
      "0"      = "grey92"
    ),
    name = "No.\nResponses",
    breaks = c(">3,800", "11-20", "6-10", "1-5", "0"),
    drop = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.92, 0.25),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.2, "in"),
    height = unit(0.4, "in"),
    width = unit(0.2, "in"),
    style = north_arrow_orienteering(text_size = 6)
  ) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "in"),
    pad_y = unit(0.3, "in"),
    unit_category = "imperial",
    width_hint = 0.18,
    style = "ticks",
    text_cex = 0.8
  )

p_us

# save ------
ggsave("./doc/bab_map_us_states.png", p_us, width = 10, height = 6.5, dpi = 300)

