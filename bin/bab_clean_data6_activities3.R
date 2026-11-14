# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# Goal: Clean write-in activities, enforce single-valued most important activity,
#       and create binary variables (q_activity_consumptive_most_b, q_activity_restoration)

# ----------------------------------------------------------
# Load libraries -------------------------------------------
library(tidyverse)
library(stringr)
library(purrr)

# --------------------------------------------------------------------------
# Load data ----------------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1 <- read_csv("./results/data_long5.csv", guess_max = Inf) %>%
  glimpse()

# -------------------------------------------------------------------------
# 1. Regex dictionary to classify write-in text (ordered by priority)
# -------------------------------------------------------------------------
# NOTE: For single-choice columns (q_activity_most3), the FIRST matching category in this list wins.
activity_text <- list(
  # --- Existing Survey Categories ---
  "Walking/Running/Hiking" = 
    "(?i)\\b(walk|walking|walks|stroll|hike|hiking|dog walk|walking dog|walk.*?dog|bring dig|dog beach|off[- ]leash dog|m\\.s walks)\\b",
  
  "Swimming/Bodysurfing" = 
    "(?i)\\b(swim|swimming|bodysurf|boogie board|boogey board|bathing|cooling and bathing|dipping my toes)\\b",
  
  "Surfing" = 
    "(?i)(?<!want to |used to )\\b(surf|surfing|surfer|surfboard|surf coach|surf class)\\b",
  
  "Fishing or collecting food" = 
    "(?i)\\b(fish|fishing|commercial fish|fish commercially)\\b",
  
  "Paddleboarding/Kiteboarding/Kayaking/Canoeing" = 
    "(?i)\\b(kayak|kayaking|canoe|canoeing|rowing|outrigger|paddle)\\b",
  
  "Sailing/Boating" = 
    "(?i)\\b(sailing|sail|tall ships|boat|boating)\\b",
  
  "Beach games/Sports/Yoga" = 
    "(?i)\\b(yoga|sports|volleyball|flying a kite|flying kites|horseback riding|atv use)\\b",
  
  "Enjoying views/sunsets" = 
    "(?i)\\b(pier and (?:admiring|enjoying)|people watching|catching the breeze)\\b",
  
  "Meditation/Reading/Relaxing/Art/Health" = 
    "(?i)\\b(vibing out|relaxing|sunbath|sun bath|sun tanning|napping|listening to music|art|artwork|painting|sketchin|singing|crafts|creating)\\b",
  
  "Nature Observing/Photographing/Education/Research" = 
    "(?i)\\b(tide pool|tidepool|tidepooling|rocks looking at ocean life|birding|research|ecological|scientific|monitoring|field trip|lectures|aquarium|docent|clam survey|studying|digging sand crabs)\\b",
  
  "Group/Family gatherings or activities" = 
    "(?i)\\b(kids|children|playing with sand toys|bringing visitors|visit friend|drum circle|bonfire|bonfires|dining|dine out|foodie|drinking)\\b",
  
  "Paid work" = 
    "(?i)\\b(commercial fish|fish commercially|us navy|coaching.*?surf|coach|working|paid work|job)\\b",
  
  "Volunteering" = 
    "(?i)\\b(volunteer|cleaning up beach|pick up trash|cleaning|unpaid work|helping people|habitat restoration|restoration work|removing invasive|planting native)\\b",
  
  "Cultural or religious ceremonies" = 
    "(?i)\\b(native singing|indigenous|ceremonials|ceremony|ceremonies)\\b",

  # --- Merged Additional Categories ---
  "Group/Family gatherings or activities" = 
    "(?i)\\b(camp|camping)\\b",
  
  "Nature Observing/Photographing/Education/Research" = 
    "(?i)\\b(beach combing|beachcombing|collect shell|finding shells|rock exploring|rock hunting|finding shells rocks|agetes?|agates?)\\b"
)

# -------------------------------------------------------------------------
# 2. Sanitize & Classification Functions
# -------------------------------------------------------------------------
f_sanitize_text <- function(txt) {
  if (is.na(txt)) return(NA_character_)
  t <- str_replace_all(txt, "[\u00A0\u200B\uFEFF]", " ")
  t <- str_replace_all(t, "[‘’`]", "'")
  t <- str_replace_all(t, "[“”]", "\"")
  t <- str_remove_all(t, "^[\\s,]+|[\\s,]+$")
  t <- str_squish(t)
  if (t %in% c("", "NA", "null", "None", "none", "-")) return(NA_character_)
  return(t)
}

# Helper to guarantee a single value (takes first comma-separated item if multiple exist)
f_first_activity <- function(txt) {
  t <- f_sanitize_text(txt)
  if (is.na(t)) return(NA_character_)
  first_val <- str_trim(str_split_1(t, "\\s*,+\\s*")[1])
  if (first_val %in% c("", "NA")) return(NA_character_)
  return(first_val)
}

f_classify_writein <- function(txt) {
  if (is.na(txt) || str_trim(txt) %in% c("", "NA")) return(NA_character_)
  
  t <- str_trim(txt)
  
  # Exclude non-activity commentary: residency, accessibility, barriers, uninterpretable notes
  exclusion_regex <- regex(
    "^(live along the coast|i live on a boat|i live.*?from the sea|i live.*?from the ocean|live a block|yo vivo en la playa|retirement|we need better wheelchair access|all|sex|roll)$",
    ignore_case = TRUE
  )
  if (str_detect(t, exclusion_regex)) return(NA_character_)
  
  # Exclude expressions of unmet intent/barriers or past activities
  is_barrier_or_past <- str_detect(
    t, 
    regex("(?i)\\b(avoid contact with the water|sewage|used to swim|used to body surf)\\b")
  )
  if (is_barrier_or_past) return(NA_character_)
  
  matched <- c()
  for (cat in names(activity_text)) {
    if (str_detect(t, regex(activity_text[[cat]], ignore_case = TRUE))) {
      matched <- c(matched, cat)
    }
  }
  
  if (length(matched) == 0) return(NA_character_)
  paste(unique(matched), collapse = ", ")
}

f_merge_all_activities <- function(...) {
  inputs <- list(...)
  
  sanitized <- map_chr(inputs, f_sanitize_text)
  valid_inputs <- sanitized[!is.na(sanitized)]
  if (length(valid_inputs) == 0) return(NA_character_)
  
  tokens <- unlist(lapply(valid_inputs, function(x) str_split_1(x, "\\s*,+\\s*")))
  tokens <- str_trim(tokens)
  
  filtered <- tokens[!tokens %in% c(
    "Another activity", 
    "Other", 
    "Other, please specify:", 
    "Other, please specify", 
    "Choose not to answer", 
    "NA", 
    ""
  )]
  
  clean_tokens <- unique(filtered)
  if (length(clean_tokens) == 0) return(NA_character_)
  
  paste(clean_tokens, collapse = ", ")
}

# -------------------------------------------------------------------------
# 3. Binary Flag Regexes (Restoration & Consumptive Write-ins)
# -------------------------------------------------------------------------
restoration_regex <- regex(
  "habitat restoration|restoration work|restoration project|removing invasive|planting native|kelp restoration|dune restoration|outplanting|culling urchins|clearing urchins", 
  ignore_case = TRUE
)

consumptive_writein_regex <- regex(
  "beach ?combing|collect(ing)? shell|finding shells|shell hunting|finding glass|sea ?glass|glass|rock hunt(ing)?|rock exploring|agetes?|agates?|渔|钓",
  ignore_case = TRUE
)

# -------------------------------------------------------------------------
# 4. Transformation Pipeline
# -------------------------------------------------------------------------
other_labels <- c("Other", "Other, please specify:", "Other, please specify", "Another activity")

d2 <- d1 %>%
  mutate(
    # 1. Cleaned versions of original multiple-choice columns
    q_activities2 = map_chr(QImportant_Activities, f_sanitize_text),
    
    # Enforce single value on q_activities_most2
    q_activities_most2 = if ("QImportant_Activities_Most" %in% names(.)) {
      map_chr(QImportant_Activities_Most, f_first_activity)
    } else {
      NA_character_
    },
    
    # 2. Classified write-ins for general activities (can hold multiple for q_activity_all3)
    q_activities_text_2 = map_chr(QImportant_Activities_TEXT, f_classify_writein),
    
    # 3. Classified write-ins for most important activities
    q_activities_most_text_2 = if ("QImportant_Activities_Most_TEXT" %in% names(.)) {
      map_chr(QImportant_Activities_Most_TEXT, f_classify_writein)
    } else {
      NA_character_
    },
    
    # 4. Standardized SINGLE "most important activity"
    q_activity_most3 = case_when(
      !is.na(q_activities_most_text_2) ~ map_chr(q_activities_most_text_2, f_first_activity),
      
      q_activities_most2 %in% other_labels | is.na(q_activities_most2) ~ map_chr(q_activities_text_2, f_first_activity),
      
      q_activities_most2 %in% c("Choose not to answer", "NA", "") ~ NA_character_,
      
      TRUE ~ q_activities_most2
    ),
    
    # Helper: Grab the relevant write-in text ONLY when respondent chose "Another activity" / "Other" or left Most blank
    writein_for_most = case_when(
      (q_activities_most2 %in% other_labels | is.na(q_activities_most2)) &
        ("QImportant_Activities_Most_TEXT" %in% names(.)) &
        !is.na(QImportant_Activities_Most_TEXT) ~ as.character(QImportant_Activities_Most_TEXT),
      
      (q_activities_most2 %in% other_labels | is.na(q_activities_most2)) &
        !is.na(QImportant_Activities_TEXT) ~ as.character(QImportant_Activities_TEXT),
      
      TRUE ~ NA_character_
    ),
    
    # 5. Binary consumptive flag for MOST important activity (1 = consumptive, 0 = non-consumptive)
    q_activity_consumptive_most_b = case_when(
      # Branch A: Respondent chose a specific option (NOT "Another activity" / "Other") -> use ONLY that choice
      !is.na(q_activities_most2) & !(q_activities_most2 %in% other_labels) ~
        if_else(q_activities_most2 == "Fishing or collecting food", 1L, 0L),
      
      # Branch B: Respondent chose "Another activity" / "Other" (or NA) -> check if resolved category is fishing OR write-in matches consumptive terms
      TRUE ~ if_else(
        (!is.na(q_activity_most3) & q_activity_most3 == "Fishing or collecting food") |
          (!is.na(writein_for_most) & str_detect(writein_for_most, consumptive_writein_regex)),
        1L, 0L
      )
    ),
    
    # 6. Binary restoration flag (evaluates write-ins and comments)
    q_activity_restoration = if_else(
      (!is.na(QImportant_Activities_TEXT) & str_detect(QImportant_Activities_TEXT, restoration_regex)) |
      (if ("QImportant_Activities_Most_TEXT" %in% names(.)) {
        !is.na(QImportant_Activities_Most_TEXT) & str_detect(QImportant_Activities_Most_TEXT, restoration_regex)
      } else { FALSE }) |
      (!is.na(Comments) & str_detect(Comments, restoration_regex)),
      1L, 0L
    ),
    
    # 7. Comprehensive merged activity column (keeps all matched activities)
    q_activity_all3 = pmap_chr(
      list(q_activities2, q_activities_text_2, q_activities_most_text_2, comment_activity),
      f_merge_all_activities
    )
  ) %>%
  select(-writein_for_most)

# -------------------------------------------------------------------------
# 5. Verification & Save
# -------------------------------------------------------------------------
# Assert that NO rows in q_activities_most2 or q_activity_most3 contain commas (multiple values)
stopifnot(!any(str_detect(d2$q_activities_most2, ","), na.rm = TRUE))
stopifnot(!any(str_detect(d2$q_activity_most3, ","), na.rm = TRUE))

# Check breakdown of q_activity_consumptive_most_b across raw choices and resolved categories

d2 %>%
  filter(q_activities_most2=="Another activity" & q_activity_consumptive_most_b==1) %>%
  select(q_activities2,q_activity_all3,q_activities_text_2, comment_activity,comment2)
  # view()
  # filter(q_activity_consumptive_most_b==1)%>%
  

d2 %>%
  count(q_activities_most2, q_activity_most3, q_activity_consumptive_most_b) %>%
  filter(q_activity_consumptive_most_b==1)%>%
  print(n = 40)


unique(d2$q_activity_most3)

# save -----
write_csv(d2, "./results/data_long6.csv")