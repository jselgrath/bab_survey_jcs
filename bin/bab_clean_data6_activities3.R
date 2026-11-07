# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: clean write in activities

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(stringr)
library(purrr)


# q_activities2 (whitespace and trailing/leading commas cleaned)
# q_activities_most2 (cleaned version of raw choice).
# q_activities_text_2 (classified general write-ins)
# q_activities_most_text_2 (classified "most important" write-ins).
# Derived Most Important: q_activity_most3 ((resolves "Other" / "Another activity") using q_activities_most_text_2 and q_activities_text_2).
# Overall Merged Activity: q_activity_all3 (merges q_activities2, q_activities_text_2, q_activities_most_text_2, and comment_activity).Binary Flag: restoration_b_2
# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# comments - for activities and dogs
# d0<-read_csv("./results/q_comments_cleaned.csv")%>%
#   glimpse()

d1<-read_csv("./results/data_long5.csv")%>%
  glimpse()


# ask gemini for code to clean and translate comments based on actual responses ------------------------------

# these are activity categories in the ocean access survey. 
writeClipboard(d1$QImportant_Activities)
# can you write code for write-in activities to classify them in existing categories where appropriate, and to make new categories as needed. and make a separate binary column $restoration_b where (1= activity is about restoration). These are the write in activities:
writeClipboard(as.character(d1$QImportant_Activities_TEXT))

# I have now cleaned the activity write in text for any activity and there another write in column for the most important activity.  $QImportant_Activities_Most_TEXT. would it be more efficent to write a new chunk of code (new R file) to clean that or to do it as part of the same functions here. This the list of write in most important activities:
writeClipboard(as.character(d1$QImportant_Activities_Most_TEXT))
unique(d1$QImportant_Activities_Most_TEXT)

# -------------------------------------------------------------------------
# 1. list to classify write in text (all options identified in writeClipboard text that was entered into Gemini and iteratively adjusted over many iterations)
# -------------------------------------------------------------------------

activity_text <- list(
  # --- Existing Survey Categories ---
  "Walking/Running/Hiking" = 
    "(?i)\\b(walk|walking|walks|stroll|hike|hiking|dog walk|walking dog|walk.*?dog|bring dig|dog beach|off[- ]leash dog|m\\.s walks)\\b",
  
  "Swimming/Bodysurfing" = 
    "(?i)\\b(swim|swimming|bodysurf|boogie board|boogey board|bathing|cooling and bathing|dipping my toes)\\b",
  
  # Matches personal surfing as well as surf coaching/instruction
  "Surfing" = 
    "(?i)(?<!want to |used to )\\b(surf|surfing|surfer|surfboard|surf coach|surf class)\\b",
  
  # Matches fishing recreationally, commercially, or collecting
  "Fishing or collecting food" = 
    "(?i)\\b(fish|fishing|commercial fish|fish commercially|digging sand crabs)\\b",
  
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
    "(?i)\\b(tide pool|tidepool|tidepooling|rocks looking at ocean life|birding|research|ecological|scientific|monitoring|field trip|lectures|aquarium|docent|clam survey|studying)\\b",
  
  "Group/Family gatherings or activities" = 
    "(?i)\\b(kids|children|playing with sand toys|bringing visitors|visit friend|drum circle|bonfire|bonfires|dining|dine out|foodie|drinking)\\b",
  
  # Catches paid work across instruction, commercial harvesting, military, etc.
  "Paid work" = 
    "(?i)\\b(commercial fish|fish commercially|us navy|coaching.*?surf|coach|working|paid work|job)\\b",
  
  "Volunteering" = 
    "(?i)\\b(volunteer|cleaning up beach|pick up trash|cleaning|unpaid work|helping people|habitat restoration|restoration work|removing invasive|planting native)\\b",
  
  "Cultural or religious ceremonies" = 
    "(?i)\\b(native singing|indigenous|ceremonials)\\b",

  # --- Additional Categories ---
  "Camping" = 
    "(?i)\\b(camp|camping)\\b",
  
  "Beachcombing" = 
    "(?i)\\b(beach combing|beachcombing|collect shell|finding shells|rock exploring|rock hunting|finding shells rocks|agetes?|agates?)\\b"
)

# -------------------------------------------------------------------------
# 2. SANITIZE & CLASSIFICATION FUNCTIONS
# -------------------------------------------------------------------------
f_sanitize_text <- function(txt) {
  if (is.na(txt)) return(NA_character_)
  t <- str_replace_all(txt, "[\u00A0\u200B\uFEFF]", " ") # Non-breaking / zero-width spaces
  t <- str_replace_all(t, "[‘’`]", "'")                   # Curly apostrophes
  t <- str_replace_all(t, "[“”]", "\"")                  # Curly quotes
  t <- str_remove_all(t, "^[\\s,]+|[\\s,]+$")             # Leading/trailing commas & whitespace
  t <- str_squish(t)
  if (t %in% c("", "NA", "null", "None", "none", "-")) return(NA_character_)
  return(t)
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

# Variable-argument merger that splits, strips invalid entries, and deduplicates
f_merge_all_activities <- function(...) {
  inputs <- list(...)
  
  # Sanitize and drop NAs
  sanitized <- map_chr(inputs, f_sanitize_text)
  valid_inputs <- sanitized[!is.na(sanitized)]
  if (length(valid_inputs) == 0) return(NA_character_)
  
  # Split on any comma cluster, handling trailing/leading and repeated commas
  tokens <- unlist(lapply(valid_inputs, function(x) str_split_1(x, "\\s*,+\\s*")))
  tokens <- str_trim(tokens)
  
  # Exclude placeholders and metadata
  filtered <- tokens[!tokens %in% c(
    "Another activity", 
    "Other",
    "Other, please specify:",
    "Other, please specify",
    "Choose not to answer", 
    "NA", 
    ""
  )]
  
  # Deduplicate categories
  clean_tokens <- unique(filtered)
  
  if (length(clean_tokens) == 0) return(NA_character_)
  
  # Guaranteed single ", " between terms and no trailing comma
  paste(clean_tokens, collapse = ", ")
}

# -------------------------------------------------------------------------
# 3. RESTORATION REGEX (Write-ins + Comments)
# -------------------------------------------------------------------------
restoration_regex <- regex(
  "habitat restoration|restoration work|restoration project|removing invasive|planting native|kelp restoration|dune restoration|outplanting|culling urchins|clearing urchins", 
  ignore_case = TRUE
)

# -------------------------------------------------------------------------
# 4. TRANSFORMATION PIPELINE (Preserves originals, creates numbered versions)
# -------------------------------------------------------------------------
d2 <- d1 %>%
  mutate(
    # 1. Cleaned versions of original multiple-choice columns (preserving original names)
    q_activities2 = map_chr(QImportant_Activities, f_sanitize_text),
    
    q_activities_most2 = if ("QImportant_Activities_Most" %in% names(.)) {
      map_chr(QImportant_Activities_Most, f_sanitize_text)
    } else {
      NA_character_
    },
    
    # 2. Classified write-ins for general activities
    q_activities_text_2 = map_chr(QImportant_Activities_TEXT, f_classify_writein),
    
    # 3. Classified write-ins for most important activities
    q_activities_most_text_2 = if ("QImportant_Activities_Most_TEXT" %in% names(.)) {
      map_chr(QImportant_Activities_Most_TEXT, f_classify_writein)
    } else {
      NA_character_
    },
    
    # 4. Standardized single "most important activity" resolving "Other" selections
    q_activity_most3 = case_when(
      !is.na(q_activities_most_text_2) ~ q_activities_most_text_2,
      
      q_activities_most2 %in% c("Other", "Other, please specify:", "Other, please specify", "Another activity") | 
        is.na(q_activities_most2) ~ q_activities_text_2,
      
      q_activities_most2 %in% c("Choose not to answer", "NA", "") ~ NA_character_,
      
      TRUE ~ q_activities_most2
    ),
    
    # 5. Binary restoration flag (evaluates write-ins and comments)
    restoration_b_2 = if_else(
      (!is.na(QImportant_Activities_TEXT) & str_detect(QImportant_Activities_TEXT, restoration_regex)) |
      (if ("QImportant_Activities_Most_TEXT" %in% names(.)) {
        !is.na(QImportant_Activities_Most_TEXT) & str_detect(QImportant_Activities_Most_TEXT, restoration_regex)
      } else { FALSE }) |
      (!is.na(Comments) & str_detect(Comments, restoration_regex)),
      1L, 0L
    ),
    
    # 6. Comprehensive merged activity column
    q_activity_all3 = pmap_chr(
      list(q_activities2, q_activities_text_2, q_activities_most_text_2, comment_activity),
      f_merge_all_activities
    )
  )

# -------------------------------------------------------------------------
# 5. INSPECT OUTPUT & SAVE
# -------------------------------------------------------------------------
d2 %>%
  select(
    QImportant_Activities,
    q_activities2,
    QImportant_Activities_Most,
    q_activity_most3,
    QImportant_Activities_TEXT,
    q_activities_text_2,
    restoration_b_2,
    q_activity_all3
  ) %>%
  filter(!is.na(q_activities_text_2) | !is.na(QImportant_Activities_Most)) %>%
  print(n = 25)

write_csv(d2, "./results/data_long6.csv")