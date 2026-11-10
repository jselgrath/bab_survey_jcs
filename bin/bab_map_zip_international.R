# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: plot maps of responses by country
# ------------------
library(tidyverse)
library(sf)
library(colorspace)
library(scales)
library(lwgeom)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)


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
unique(d2$QDemographic_Country_State)

# writeClipboard(d2$QDemographic_Home)
# writeClipboard(d2$QDemographic_PrimaryZip)

# -------------------------------------------------------------------------
# 1. Clean Data, Cross-Reference CA ZIPs, & Classify "home"
# -------------------------------------------------------------------------

d_clean <- d2 %>%
  mutate(
    # Standardize 5-digit ZIPs
    clean_zip = str_extract(as.character(QDemographic_PrimaryZip), "^\\d{3,5}"),
    clean_zip = sprintf("%05d", as.numeric(clean_zip)),
    
    # Safe numeric conversion for CA years
    ca_years_num = suppressWarnings(as.numeric(str_extract(as.character(QDemographic_CA_Years), "[0-9.]+")))
  ) %>%
  # Join with zipcode database to resolve state from ZIP code
  left_join(
    zipcodeR::zip_code_db %>% select(zipcode, state),
    by = c("clean_zip" = "zipcode")
  )

# Classify respondents into "California", "US Other", and "International"
d_classified <- d_clean %>%
  mutate(
    raw_loc   = str_trim(QDemographic_Country_State),
    home_cat  = str_trim(QDemographic_Home),
    zip_state = state,
    
    # Text helper flags
    lived_ca_text = str_detect(coalesce(home_cat, ""), regex("lived in California", ignore_case = TRUE)) |
                    str_detect(coalesce(raw_loc, ""), regex("lived in California", ignore_case = TRUE)),
    not_ca_text   = str_detect(coalesce(home_cat, ""), regex("do not live in California", ignore_case = TRUE)) |
                    str_detect(coalesce(raw_loc, ""), regex("do not live in California", ignore_case = TRUE)),

    home = case_when(
      # -------------------------------------------------------------------
      # 1. CALIFORNIA
      # -------------------------------------------------------------------
      home_cat == "California" |
        lived_ca_text |
        (!is.na(ca_years_num) & ca_years_num > 0) |
        (!is.na(zip_state) & zip_state == "CA") |
        str_to_lower(raw_loc) %in% c("california", "ca", "santa clara") ~ "California",

      # -------------------------------------------------------------------
      # 2. US OTHER (Another State)
      # -------------------------------------------------------------------
      home_cat == "Another state" |
        (!is.na(zip_state) & zip_state != "CA") |
        str_to_lower(raw_loc) %in% c("kentucky", "united states", "us", "usa") |
        (not_ca_text & !is.na(zip_state)) ~ "US Other",

      # -------------------------------------------------------------------
      # 3. INTERNATIONAL (Non-US country)
      # -------------------------------------------------------------------
      home_cat == "Non-US country" |
        (!is.na(raw_loc) & 
           !str_to_lower(raw_loc) %in% c(
             "what country do you live in?",
             "{\"importid\":\"qid138_text\"}",
             "i've lived in california for the number of years marked below:",
             "i do not live in california",
             "california", "ca", "santa clara", "kentucky", "united states", "us", "usa"
           )) ~ "International",

      # Fallback if they only picked "I do not live in California" without detail
      not_ca_text ~ "US Other",

      # Everyone else genuinely left demographic questions blank
      TRUE ~ NA_character_
    ),

    # Standardize foreign country text & assign USA to domestic
    country_cleaned = case_when(
      home %in% c("California", "US Other") ~ "United States",
      str_to_lower(raw_loc) %in% c("mexico", "méxico") ~ "Mexico",
      str_to_lower(raw_loc) == "uk" ~ "United Kingdom",
      str_to_lower(raw_loc) == "hong kong" ~ "Hong Kong",
      str_to_lower(raw_loc) == "korea" ~ "South Korea",
      str_to_lower(raw_loc) == "french polynesia" ~ "French Polynesia",
      raw_loc %in% c(
        "What country do you live in?",
        "{\"ImportId\":\"QID138_TEXT\"}",
        "I've lived in California for the number of years marked below:",
        "I do not live in California"
      ) ~ NA_character_,
      TRUE ~ raw_loc
    )
  )

# Check the new breakdown:
table(d_classified$home, useNA = "always")

d_classified %>%
  filter(is.na(home)) %>%
  select(ResponseId, QDemographic_Home, QDemographic_PrimaryZip, QDemographic_Country_State, QDemographic_CA_Years) %>%
  print(n = 30)

# -------------------------------------------------------------------------
# 2. Derive ISO3 Codes & Aggregate Country Counts
# -------------------------------------------------------------------------

country_counts <- d_classified %>%
  filter(!is.na(home)) %>%
  mutate(
    iso3 = case_when(
      home %in% c("California", "US Other") ~ "USA",
      TRUE ~ countrycode(country_cleaned, origin = "country.name", destination = "iso3c")
    )
  ) %>%
  filter(!is.na(iso3)) %>%
  count(iso3, name = "count")

# -------------------------------------------------------------------------
# 3. Global Boundaries & Binning (Robinson Projection)
# -------------------------------------------------------------------------

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  st_transform(crs = "ESRI:54030")

world_map_data <- world_sf %>%
  mutate(iso_a3_clean = if_else(iso_a3 == "-99" | is.na(iso_a3), adm0_a3, iso_a3)) %>%
  mutate(iso_a3_clean = case_when(
    admin == "French Polynesia" ~ "PYF",
    admin == "Hong Kong" ~ "HKG",
    TRUE ~ iso_a3_clean
  )) %>%
  left_join(country_counts, by = c("iso_a3_clean" = "iso3")) %>%
  mutate(
    count = replace_na(count, 0),
    count_cat = case_when(
      count == 0                ~ "0",
      count >= 1  & count <= 5  ~ "1-5",
      # count >= 6  & count <= 15 ~ "6-15",
      # count >= 16 & count <= 50 ~ "16-50",
      count > 50                ~ ">4,100"
    ),
    count_cat = factor(
      count_cat,
      levels = c("0",  "1-5",  ">4,100") #"6-15", "16-50",
    )
  )

unique(world_map_data$count)
# -------------------------------------------------------------------------
# 4. Plot Global Map
# -------------------------------------------------------------------------

sunset_colors_global <- sequential_hcl(5, "SunsetDark", rev = TRUE)

p_global <- ggplot() +
  geom_sf(
    data = world_map_data,
    aes(fill = count_cat),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      ">4,100" = sunset_colors_global[5],
      # "16-50"  = sunset_colors_global[4],
      # "6-15"   = sunset_colors_global[3],
      "1-5"    = sunset_colors_global[1],
      # "1"      = sunset_colors_global[1],
      "0"      = "grey92"
    ),
    name = "No.\nResponses",
    breaks = c(">4,100", "1-5", "0"),
    drop = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.12, 0.38),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

p_global

# save ------
ggsave("./doc/bab_map_us_world.png", p_global, width = 10, height = 6.5, dpi = 300)

