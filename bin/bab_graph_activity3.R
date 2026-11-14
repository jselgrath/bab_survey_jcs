# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# Goal: Generate Raw & Weighted Activity Plots Faceted by Influencer Period(influencer_any_b) across Full State, Coastal Counties, and Focal Counties

# X2 tests whether distribution of activities shifted significantly during the influencer period

# note - see folder ./doc/activities_with_influencer to see effects of influencer prior to weighting. (using data without running code to subsample influencer period). here using weighted, subsampled data 
# ----------------------------------------------------------
# 1. Load libraries ----------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(ggpattern)
library(stringr)

rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

# ----------------------------------------------------------
# 2. Shared Labels & Regex Definitions ---------------------
activity_labels <- c(
  "Fishing or collecting food"                        = "Fishing/Collecting Food",
  "Group/Family gatherings or activities"             = "Family/Group Activities",
  "Meditation/Reading/Relaxing/Art/Health"            = "Meditation/Relaxing/Art/Health",
  "Swimming/Bodysurfing"                              = "Swimming/Bodysurfing",
  "Festivals"                                         = "Festivals",
  "Bicycling/Roller skating/Skateboarding"            = "Cycling/Skating",
  "Paid work"                                         = "Paid Work",
  "Beach games/Sports/Yoga"                           = "Beach Games/Sports/Yoga",
  "Walking/Running/Hiking"                            = "Walking/Running/Hiking",
  "Enjoying views/sunsets"                            = "Enjoying Views/Sunsets",
  "Surfing"                                           = "Surfing",
  "Cultural or religious ceremonies"                  = "Cultural/Religious Ceremonies",
  "Nature Observing/Photographing/Education/Research" = "Nature Observation",
  "Paddleboarding/Kiteboarding/Kayaking/Canoeing"     = "Paddle/Kayak/Canoe",
  "Snorkeling/Scuba Diving"                           = "Snorkeling/Scuba",
  "Volunteering"                                      = "Volunteering",
  "Sailing/Boating"                                   = "Sailing/Boating",
  "Beachcombing"                                      = "Beachcombing",
  "Camping"                                           = "Camping",
  "Habitat restoration / Conservation action"         = "Habitat Restoration/Conservation",
  "Another activity"                                  = "Another Activity"
)

consumptive_writein_regex <- regex(
  "beach ?combing|collect(ing)? shell|finding shells|shell hunting|finding glass|sea ?glass|glass|rock hunt(ing)?|rock exploring|agetes?|agates?|渔|钓",
  ignore_case = TRUE
)

# ----------------------------------------------------------
# 3. Faceted Plotting Function (Stacked Vertically: ncol = 1)
# ----------------------------------------------------------
f_plot_activities_influencer <- function(file_path, scope_tag, pop_weight_col, weighted = TRUE) {
  
  mode_tag <- if (weighted) "weighted" else "raw"
  message(sprintf("=== Running Stacked Influencer Facets: %s (%s) ===", scope_tag, mode_tag))
  
  d1 <- read_csv(file_path, guess_max = Inf, show_col_types = FALSE) %>%
    mutate(
      w_plot = if (weighted) as.numeric(.data[[pop_weight_col]]) else 1.0,
      w_stat = if (weighted) as.numeric(weight_within_stratum) else 1.0,
      influencer_group = factor(
        if_else(influencer_any_b == 1, "Influencer Period", "Non-Influencer Period", missing = "Non-Influencer Period"),
        levels = c("Non-Influencer Period", "Influencer Period")
      )
    )
  
  # Calculate respondent sample sizes per facet for clean facet strip headers
  facet_n_most <- d1 %>%
    filter(!is.na(q_activity_most3), q_activity_most3 != "", q_activity_most3 != "Choose not to answer") %>%
    group_by(influencer_group) %>%
    summarise(n_resp = n_distinct(response_id), .groups = "drop") %>%
    mutate(facet_label = sprintf("%s (n = %s)", influencer_group, scales::comma(n_resp)))
  
  facet_map_most <- setNames(facet_n_most$facet_label, facet_n_most$influencer_group)
  
  # -------------------------------------------------------------------------
  # PLOT 1: Most Important Activity (Stacked Facets)
  # -------------------------------------------------------------------------
  props_most <- d1 %>%
    mutate(q_activity_most3 = as.character(q_activity_most3)) %>%
    tidyr::separate_rows(q_activity_most3, sep = ",") %>%
    mutate(q_activity_most3 = str_trim(q_activity_most3)) %>%
    filter(
      !is.na(q_activity_most3), 
      q_activity_most3 != "",
      q_activity_most3 != "Choose not to answer"
    ) %>%
    distinct(response_id, q_activity_most3, .keep_all = TRUE) %>%
    mutate(
      consumptive_label = factor(
        if_else(q_activity_consumptive_most_b == 1, "Consumptive", "Non-consumptive", missing = "Non-consumptive"),
        levels = c("Non-consumptive", "Consumptive")
      )
    ) %>%
    group_by(influencer_group, q_activity_most3, consumptive_label) %>%
    summarise(
      val_plot = sum(w_plot, na.rm = TRUE),
      val_stat = sum(w_stat, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    group_by(influencer_group) %>%
    mutate(pct = val_plot / sum(val_plot)) %>%
    ungroup()
  
  # Order x-axis categories by Non-Influencer baseline share (highest to lowest)
  baseline_order_most <- props_most %>%
    filter(influencer_group == "Non-Influencer Period") %>%
    group_by(q_activity_most3) %>%
    summarise(base_pct = sum(pct), .groups = "drop") %>%
    arrange(desc(base_pct)) %>%
    pull(q_activity_most3)
  
  all_levels_most <- union(baseline_order_most, unique(props_most$q_activity_most3))
  if ("Another activity" %in% all_levels_most) {
    all_levels_most <- c(setdiff(all_levels_most, "Another activity"), "Another activity")
  }
  
  props_most <- props_most %>%
    mutate(q_activity_most3 = factor(q_activity_most3, levels = all_levels_most))
  
  totals_most <- props_most %>%
    group_by(influencer_group, q_activity_most3) %>%
    summarise(
      total_stat = sum(val_stat),
      total_pct  = sum(pct),
      .groups    = "drop"
    )
  
  chi_mat1 <- totals_most %>%
    select(influencer_group, q_activity_most3, total_stat) %>%
    tidyr::pivot_wider(names_from = influencer_group, values_from = total_stat, values_fill = 0) %>%
    select(-q_activity_most3) %>%
    as.matrix()
  
  chi1 <- chisq.test(chi_mat1)
  fmt_p1 <- ifelse(chi1$p.value < .001, "< 0.001", scales::number(chi1$p.value, accuracy = 0.001))
  
  all_acts_most <- unique(props_most$q_activity_most3)
  pattern_most <- setNames(rep("none", length(all_acts_most)), all_acts_most)
  if ("Another activity" %in% names(pattern_most)) pattern_most["Another activity"] <- "stripe"
  
  p1 <- ggplot(props_most, aes(x = q_activity_most3, y = pct, fill = consumptive_label, pattern = q_activity_most3)) +
    geom_col_pattern(
      position = "stack", show.legend = TRUE,
      pattern_color = "transparent", pattern_fill = "#d43d51",
      pattern_density = 0.4, pattern_spacing = 0.02, color = NA
    ) +
    geom_text(
      data = totals_most,
      aes(x = q_activity_most3, y = total_pct, label = scales::percent(total_pct, accuracy = 0.1)),
      inherit.aes = FALSE, vjust = -0.4, size = 3.2
    ) +
    # Stack panels vertically (1 column, 2 rows)
    facet_wrap(~ influencer_group, ncol = 1, labeller = labeller(influencer_group = facet_map_most)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(totals_most$total_pct) * 1.18)) +
    scale_x_discrete(labels = activity_labels) +
    scale_fill_manual(values = c("Non-consumptive" = "#79ABE2", "Consumptive" = "#d43d51"), name = "Activity Type") +
    scale_pattern_manual(values = pattern_most, guide = "none") +
    guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
    labs(
      x = "Most Important Activity",
      y = if (weighted) "Weighted Percent of Adults" else "Percent of Respondents",
      subtitle = paste0("Period Comparison: \u03C7\u00B2(", unname(chi1$parameter), ") = ", round(unname(chi1$statistic), 2), ", p ", fmt_p1)
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(size = 15, face = "bold"),
      legend.position = "top",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(color = "grey40", hjust = 1),
      panel.spacing = unit(1.5, "lines")
    )
  
  ggsave(sprintf("./doc/bab_activities_most_influencer_%s_%s.png", scope_tag, mode_tag), p1, width = 12, height = 14, units = "in", dpi = 300, bg = "white")
  
  # -------------------------------------------------------------------------
  # PLOT 2: All Important Activities (Stacked Facets)
  # -------------------------------------------------------------------------
  writein_cols <- intersect(
    c("QImportant_Activities_TEXT", "QImportant_Activities_Most_TEXT",
      "q_activities_text_2", "q_activities_most_text_2"),
    names(d1)
  )
  
  d_activities <- d1 %>%
    mutate(q_activity_all3 = as.character(q_activity_all3)) %>%
    tidyr::separate_rows(q_activity_all3, sep = ",") %>%
    mutate(q_activity_all3 = stringr::str_trim(q_activity_all3)) %>%
    filter(
      !is.na(q_activity_all3), 
      q_activity_all3 != "",
      q_activity_all3 != "Choose not to answer"
    ) %>%
    distinct(response_id, q_activity_all3, .keep_all = TRUE) %>%
    mutate(
      has_consumptive_writein = if (length(writein_cols) > 0) {
        rowSums(
          across(all_of(writein_cols), ~ !is.na(.x) & str_detect(as.character(.x), consumptive_writein_regex)),
          na.rm = TRUE
        ) > 0
      } else {
        FALSE
      },
      q_activity_consumptive_b = if_else(
        q_activity_all3 %in% c("Fishing or collecting food", "Beachcombing") |
          (q_activity_all3 == "Nature Observing/Photographing/Education/Research" & has_consumptive_writein),
        1L, 0L
      ),
      consumptive_label = factor(
        if_else(q_activity_consumptive_b == 1L, "Consumptive", "Non-consumptive"),
        levels = c("Non-consumptive", "Consumptive")
      )
    ) %>%
    select(-has_consumptive_writein)
  
  facet_n_all <- d_activities %>%
    group_by(influencer_group) %>%
    summarise(n_resp = n_distinct(response_id), .groups = "drop") %>%
    mutate(facet_label = sprintf("%s (n = %s)", influencer_group, scales::comma(n_resp)))
  
  facet_map_all <- setNames(facet_n_all$facet_label, facet_n_all$influencer_group)
  
  props_all <- d_activities %>%
    group_by(influencer_group, q_activity_all3, consumptive_label) %>%
    summarise(
      val_plot = sum(w_plot, na.rm = TRUE),
      val_stat = sum(w_stat, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    group_by(influencer_group) %>%
    mutate(pct = val_plot / sum(val_plot)) %>%
    ungroup()
  
  baseline_order_all <- props_all %>%
    filter(influencer_group == "Non-Influencer Period") %>%
    group_by(q_activity_all3) %>%
    summarise(base_pct = sum(pct), .groups = "drop") %>%
    arrange(desc(base_pct)) %>%
    pull(q_activity_all3)
  
  all_levels_all <- union(baseline_order_all, unique(props_all$q_activity_all3))
  if ("Another activity" %in% all_levels_all) {
    all_levels_all <- c(setdiff(all_levels_all, "Another activity"), "Another activity")
  }
  
  props_all <- props_all %>%
    mutate(q_activity_all3 = factor(q_activity_all3, levels = all_levels_all))
  
  totals_all <- props_all %>%
    group_by(influencer_group, q_activity_all3) %>%
    summarise(
      total_stat = sum(val_stat),
      total_pct  = sum(pct),
      .groups    = "drop"
    )
  
  chi_mat2 <- totals_all %>%
    select(influencer_group, q_activity_all3, total_stat) %>%
    tidyr::pivot_wider(names_from = influencer_group, values_from = total_stat, values_fill = 0) %>%
    select(-q_activity_all3) %>%
    as.matrix()
  
  chi2 <- chisq.test(chi_mat2)
  fmt_p2 <- ifelse(chi2$p.value < .001, "< 0.001", scales::number(chi2$p.value, accuracy = 0.001))
  
  all_acts_all <- unique(props_all$q_activity_all3)
  pattern_all <- setNames(rep("none", length(all_acts_all)), all_acts_all)
  if ("Another activity" %in% names(pattern_all)) pattern_all["Another activity"] <- "stripe"
  
  p2 <- ggplot(props_all, aes(x = q_activity_all3, y = pct, fill = consumptive_label, pattern = q_activity_all3)) +
    geom_col_pattern(
      position = "stack", show.legend = TRUE,
      pattern_color = "transparent", pattern_fill = "#d43d51",
      pattern_density = 0.4, pattern_spacing = 0.02, color = NA
    ) +
    scale_fill_manual(values = c("Non-consumptive" = "#007EB3", "Consumptive" = "#d43d51"), name = "Activity Type") +
    scale_pattern_manual(values = pattern_all, guide = "none") +
    guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
    geom_text(
      data = totals_all,
      aes(x = q_activity_all3, y = total_pct, label = scales::percent(total_pct, accuracy = 0.1)),
      inherit.aes = FALSE, vjust = -0.4, size = 3.2
    ) +
    # Stack panels vertically (1 column, 2 rows)
    facet_wrap(~ influencer_group, ncol = 1, labeller = labeller(influencer_group = facet_map_all)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(totals_all$total_pct) * 1.18)) +
    scale_x_discrete(labels = activity_labels) +
    labs(
      x = "Activity",
      y = if (weighted) "Weighted Percent of Total Activities" else "Percent of Total Activities",
      subtitle = paste0("Period Comparison: \u03C7\u00B2(", unname(chi2$parameter), ") = ", round(unname(chi2$statistic), 2), ", p ", fmt_p2)
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(size = 15, face = "bold"),
      legend.position = "top",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(color = "grey40", hjust = 1),
      panel.spacing = unit(1.5, "lines")
    )
  
  ggsave(sprintf("./doc/bab_activities_all_influencer_%s_%s.png", scope_tag, mode_tag), p2, width = 12, height = 14, units = "in", dpi = 300, bg = "white")
  
  invisible(list(most = props_most, all = props_all))
}


# ----------------------------------------------------------
# 4. Run Across All 3 Scopes (Raw & Weighted = 6 Runs) -----
# ----------------------------------------------------------
run_grid <- tidyr::crossing(
  tibble(
    file_path      = c("./results/data_final_weighted.csv",
                       "./results/data_final_weighted_coastal.csv",
                       "./results/data_final_weighted_focal.csv"),
    scope_tag      = c("statewide", "coastal", "focal"),
    pop_weight_col = c("weight_statewide", "weight_coastal_pop", "weight_focal_pop")
  ),
  weighted = c(FALSE, TRUE)
)

influencer_results <- purrr::pmap(run_grid, f_plot_activities_influencer)
names(influencer_results) <- paste0(run_grid$scope_tag, "_", ifelse(run_grid$weighted, "weighted", "raw"))
