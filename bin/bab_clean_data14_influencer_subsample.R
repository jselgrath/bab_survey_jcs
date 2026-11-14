# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: sub sample influencer period

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(lubridate)


# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# summaries
d00<-read_csv("./doc/fishing_influencer_summary.csv", show_col_types = FALSE)%>%
  glimpse()

# update this input to be the most cleaned version 
d1 <- read_csv("./results/data_long13.csv", guess_max = Inf) %>%
  glimpse()


set.seed(2026) # for reproducibility 

# Baseline online fishing rate vs. Influencer period rate (100%)
p_base <- d00$percent[9] / 100
p_inf  <- d00$percent[5] / 100

# what to retain: influencer anglers needed to match p_base 
target_ratio_any <- p_base / p_inf
target_ratio_any 

d_subsampled <- bind_rows(
  d1 %>% filter(influencer_any_b != 1 | is.na(influencer_any_b)),
  d1 %>% filter(influencer_any_b == 1) %>% slice_sample(prop = target_ratio_any, replace = FALSE)
)%>%
  glimpse()

# review ------
retention_summary <- tibble(
  Subgroup = c(
    "Non-Influencer Rows (Kept 100%)",
    "Influencer Anglers (Subsampled)",
    "Total Dataset"
  ),
  Original_n = c(
    sum(d1$influencer_any_b != 1 | is.na(d1$influencer_any_b)),
    sum(d1$influencer_any_b == 1, na.rm = TRUE),
    nrow(d1)
  ),
  Subsampled_n = c(
    sum(d_subsampled$influencer_any_b != 1 | is.na(d_subsampled$influencer_any_b)),
    sum(d_subsampled$influencer_any_b == 1, na.rm = TRUE),
    nrow(d_subsampled)
  )
) %>%
  mutate(
    Dropped_n = Original_n - Subsampled_n,
    Pct_Retained = round((Subsampled_n / Original_n) * 100, 1)
  )

print(retention_summary)


# ----------------------------------------------------------
# 2. Fishing Rate Comparison (Online Only & All Mechanisms)
# ----------------------------------------------------------
f_summarize_rates <- function(df, label) {
  bind_rows(
    df %>%
      filter(Mechanism == "Online") %>%
      summarise(
        Dataset = label,
        Scope = "Online Only",
        n = n(),
        n_fish_any = sum(fishing_any_b == 1, na.rm = TRUE),
        pct_fish_any = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
        n_fish_most = sum(fishing_most_b == 1, na.rm = TRUE),
        pct_fish_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
      ),
    df %>%
      summarise(
        Dataset = label,
        Scope = "All Mechanisms (Online + In-Person)",
        n = n(),
        n_fish_any = sum(fishing_any_b == 1, na.rm = TRUE),
        pct_fish_any = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
        n_fish_most = sum(fishing_most_b == 1, na.rm = TRUE),
        pct_fish_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
      )
  )
}

rate_comparison <- bind_rows(
  f_summarize_rates(d1, "1. Original (Unadjusted)"),
  f_summarize_rates(d_subsampled, "2. Subsampled (target_ratio_any)"),
  f_summarize_rates(d1 %>% filter(influencer_any_b != 1 | is.na(influencer_any_b)), "3. Non-Influencer Only (Baseline)")
) %>%
  arrange(Scope, Dataset)

print(rate_comparison, n = Inf)


# save -----------
write_csv(d_subsampled, "./results/data_long14.csv")
write_csv(rate_comparison, "./doc/influencer_subsampling.csv")





# # option 2 -----------
# # --------------------------
# # 1. Extract baseline and influencer rates from fishing_influencer_summary.csv
# nrm <- d00$percent[10] / 100  # Baseline online rate: fishing as MOST important (~5.7%)
# nim <- d00$percent[6]  / 100  # Influencer online rate: fishing as MOST important (~82.1%)
# nra <- d00$percent[9]  / 100  # Baseline online rate: fishing as ANY important (~17.7%)
# nia <- d00$percent[5]  / 100  # Influencer online rate: fishing as ANY important (~100%)
# 
# # 2. Compute retention proportions for the two mutually exclusive angler subgroups
# # Subgroup A: Fishing is MOST important (fishing_most_b == 1)
# prop_keep_most <- nrm / nim
# 
# # Subgroup B: Fishing is ANY important, but NOT most important (fishing_most_b == 0)
# # Derived so total retained anglers match nra/nia while Subgroup A matches nrm/nim
# p_inf_most_share <- mean(d1$fishing_most_b[d1$influencer_any_b == 1] == 1, na.rm = TRUE)
# p_inf_sec_share  <- 1 - p_inf_most_share
# 
# target_ratio_any <- nra / nia
# prop_keep_sec <- max(0, min(1, (target_ratio_any - (p_inf_most_share * prop_keep_most)) / p_inf_sec_share))
# 
# # 3. Subsample each subgroup independently and recombine with all non-influencer rows
# inf_most_sub <- d1 %>%
#   filter(influencer_any_b == 1, fishing_most_b == 1) %>%
#   slice_sample(prop = prop_keep_most, replace = FALSE)
# 
# inf_sec_sub <- d1 %>%
#   filter(influencer_any_b == 1, fishing_most_b != 1 | is.na(fishing_most_b)) %>%
#   slice_sample(prop = prop_keep_sec, replace = FALSE)
# 
# d_subsampled2 <- bind_rows(
#   d1 %>% filter(influencer_any_b != 1 | is.na(influencer_any_b)),
#   inf_most_sub,
#   inf_sec_sub
# )
# 
# # 4. Verify alignment of both fishing_any_b and fishing_most_b against baseline
# bind_rows(
#   d1 %>%
#     filter(Mechanism == "Online", influencer_any_b != 1 | is.na(influencer_any_b)) %>%
#     summarise(
#       Group = "Baseline Online (No Influencer)",
#       n = n(),
#       pct_fishing_any  = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
#       pct_fishing_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
#     ),
#   d_subsampled2 %>%
#     filter(Mechanism == "Online") %>%
#     summarise(
#       Group = "Full Online (After Stratified Subsampling)",
#       n = n(),
#       pct_fishing_any  = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
#       pct_fishing_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
#     )
# ) %>%
#   print()
# 
# 
# 
# 
# # option 3 ------------------
# # ------------------------------------
# # 1. True baseline online fishing rates (from the non-influencer period)
# #    Use your summary table rates OR compute directly from non-influencer online rows:
# p_any  <- d00$percent[9]  / 100  # 0.177 (17.7% any fishing in non-influencer online period)
# p_most <- d00$percent[10] / 100  # 0.057 (5.7% most important fishing in non-influencer online period)
# 
# # 2. Count total non-fishing online respondents and existing non-influencer online anglers in d1
# n_nonfish_online <- d1 %>%
#   filter(Mechanism == "Online", fishing_any_b != 1 | is.na(fishing_any_b)) %>%
#   nrow()
# 
# n_base_most <- d1 %>%
#   filter(Mechanism == "Online", influencer_any_b != 1 | is.na(influencer_any_b), fishing_most_b == 1) %>%
#   nrow()
# 
# n_base_sec <- d1 %>%
#   filter(Mechanism == "Online", influencer_any_b != 1 | is.na(influencer_any_b),
#          fishing_any_b == 1, fishing_most_b != 1 | is.na(fishing_most_b)) %>%
#   nrow()
# 
# # 3. Total anglers needed across ALL online responses to match p_any and p_most
# total_needed_most <- round(n_nonfish_online * (p_most / (1 - p_any)))
# total_needed_sec  <- round(n_nonfish_online * ((p_any - p_most) / (1 - p_any)))
# 
# # Subtract the non-influencer anglers already present so we only draw the difference
# n_target_most <- max(0, total_needed_most - n_base_most)
# n_target_sec  <- max(0, total_needed_sec  - n_base_sec)
# 
# # Safety caps
# n_avail_most <- d1 %>% filter(influencer_any_b == 1, fishing_most_b == 1) %>% nrow()
# n_avail_sec  <- d1 %>% filter(influencer_any_b == 1, fishing_most_b != 1 | is.na(fishing_most_b)) %>% nrow()
# 
# n_target_most <- min(n_target_most, n_avail_most)
# n_target_sec  <- min(n_target_sec, n_avail_sec)
# 
# # 4. Subsample exact counts from each subgroup and recombine
# inf_most_sub <- d1 %>%
#   filter(influencer_any_b == 1, fishing_most_b == 1) %>%
#   slice_sample(n = n_target_most, replace = FALSE)
# 
# inf_sec_sub <- d1 %>%
#   filter(influencer_any_b == 1, fishing_most_b != 1 | is.na(fishing_most_b)) %>%
#   slice_sample(n = n_target_sec, replace = FALSE)
# 
# d_subsampled3 <- bind_rows(
#   d1 %>% filter(influencer_any_b != 1 | is.na(influencer_any_b)),
#   inf_most_sub,
#   inf_sec_sub
# )
# 
# # 5. Verify exact alignment against target baseline rates (17.3% and 5.9%)
# d_subsampled3 %>%
#   filter(Mechanism == "Online") %>%
#   summarise(
#     Group = "Full Online (After Exact Stratified Subsampling)",
#     n = n(),
#     pct_fishing_any  = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
#     pct_fishing_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
#   ) %>%
#   print()
# 
# bind_rows(
#   d1 %>%
#     filter(Mechanism == "Online", influencer_any_b != 1 | is.na(influencer_any_b)) %>%
#     summarise(
#       Group = "Baseline Online (No Influencer)",
#       n = n(),
#       pct_fishing_any  = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
#       pct_fishing_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
#     ),
#   d_subsampled3 %>%
#   filter(Mechanism == "Online") %>%
#   summarise(
#     Group = "Full Online (After Exact Stratified Subsampling)",
#     n = n(),
#     pct_fishing_any  = round(mean(fishing_any_b == 1, na.rm = TRUE) * 100, 1),
#     pct_fishing_most = round(mean(fishing_most_b == 1, na.rm = TRUE) * 100, 1)
#   )) %>%
#   print()
# 


