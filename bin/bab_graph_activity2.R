# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# Goal: Generate Raw & Weighted Activity Plots (Most Important & All Activities;
#       Consumptive, Strict Nature, & Broad Nature) across Full State, Coastal, and Focal Counties

# X2 tests whether bars are equal

# ----------------------------------------------------------
# 1. Load libraries ----------------------------------------
library(tidyverse)
library(scales)
library(colorspace)
library(ggpattern)
library(stringr)

rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

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
  "Habitat restoration / Conservation action"         = "Habitat Restoration/Conservation",
  "Another activity"                                  = "Another Activity"
)

consumptive_writein_regex <- regex(
  "beach ?combing|collect(ing)? shell|finding shells|shell hunting|finding glass|sea ?glass|glass|rock hunt(ing)?|rock exploring|agetes?|agates?|渔|钓",
  ignore_case = TRUE
)

# Activity-level nature interaction definitions (used for exploded q_activity_all3 & fallback)
nature_strict_regex <- regex(
  "Nature Observing|Fishing|Collecting|Tidepool|Snorkeling|Scuba|Kayak|Paddle|Boating|Habitat restoration|Conservation|Beachcombing",
  ignore_case = TRUE
)

nature_broad_regex <- regex(
  "Nature Observing|Fishing|Collecting|Tidepool|Snorkeling|Scuba|Kayak|Paddle|Boating|Habitat restoration|Conservation|Beachcombing|Swimming|Bodysurfing|Surfing|Walking|Running|Hiking|Enjoying views",
  ignore_case = TRUE
)

# Helper function to build & save standardized bar plots
f_build_activity_plot <- function(df_props, x_col, fill_col, fill_values, legend_title,
                                  x_lab, y_lab, out_prefix, out_dir, scope_tag, mode_tag,
                                  pattern_fill_col = "#0A1F2E") {
  
  totals_df <- df_props %>%
    group_by(.data[[x_col]]) %>%
    summarise(
      total_stat = sum(val_stat),
      total_pct  = sum(pct),
      .groups    = "drop"
    )
  
  chi_res <- chisq.test(totals_df$total_stat)
  fmt_p <- ifelse(chi_res$p.value < .001, "< 0.001", scales::number(chi_res$p.value, accuracy = 0.001))
  
  all_acts <- unique(df_props[[x_col]])
  pattern_vec <- setNames(rep("none", length(all_acts)), all_acts)
  if ("Another activity" %in% names(pattern_vec)) pattern_vec["Another activity"] <- "stripe"
  
  p <- ggplot(
    df_props,
    aes(
      x       = .data[[x_col]],
      y       = pct,
      fill    = .data[[fill_col]],
      pattern = .data[[x_col]]
    )
  ) +
    geom_col_pattern(
      position        = "stack",
      show.legend     = TRUE,
      pattern_color   = "transparent",
      pattern_fill    = pattern_fill_col,
      pattern_density = 0.4,
      pattern_spacing = 0.02,
      color           = NA
    ) +
    geom_text(
      data        = totals_df,
      aes(x = .data[[x_col]], y = total_pct, label = scales::percent(total_pct, accuracy = 0.1)),
      inherit.aes = FALSE,
      vjust       = -0.4,
      size        = 3.3
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, max(totals_df$total_pct) * 1.18)
    ) +
    scale_x_discrete(labels = activity_labels) +
    scale_fill_manual(values = fill_values, name = legend_title) +
    scale_pattern_manual(values = pattern_vec, guide = "none") +
    guides(fill = guide_legend(override.aes = list(pattern = "none"))) +
    labs(
      x        = x_lab,
      y        = y_lab,
      subtitle = paste0("\u03C7\u00B2(", unname(chi_res$parameter), ") = ", round(unname(chi_res$statistic), 2), ", p ", fmt_p)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x          = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position      = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.text          = element_text(size = 15),
      legend.title         = element_text(size = 16, face = "bold"),
      plot.subtitle        = element_text(color = "grey40", hjust = 1)
    )
  
  ggsave(file.path(out_dir, sprintf("%s_%s_%s.png", out_prefix, scope_tag, mode_tag)), p, width = 12, height = 10, units = "in", dpi = 300, bg = "white")
  ggsave(file.path(out_dir, sprintf("%s_wide_%s_%s.png", out_prefix, scope_tag, mode_tag)), p, width = 18, height = 8, units = "in", dpi = 300, bg = "white")
  
  invisible(p)
}

# ----------------------------------------------------------
# 3. Master Plotting Function ------------------------------
f_plot_activities <- function(file_path, scope_tag, pop_weight_col, weighted = TRUE) {
  
  mode_tag <- if (weighted) "weighted" else "raw"
  message(sprintf("=== Running: %s (%s) ===", scope_tag, mode_tag))
  
  # Route focal plots to ./doc, and statewide/coastal plots to ./doc/activity_variations
  out_dir <- if (scope_tag == "focal") "./doc" else "./doc/activity_variations"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  d1 <- read_csv(file_path, guess_max = Inf, show_col_types = FALSE)
  
  # Set active weight column (1.0 for raw; population weight for weighted)
  d1 <- d1 %>%
    mutate(
      w_plot = if (weighted) as.numeric(.data[[pop_weight_col]]) else 1.0,
      w_stat = if (weighted) as.numeric(weight_within_stratum) else 1.0
    )
  
  # Check for any unmapped category labels
  all_unique_acts <- unique(c(
    str_trim(unlist(str_split(d1$q_activity_most3, ","))),
    str_trim(unlist(str_split(d1$q_activity_all3, ",")))
  ))
  all_unique_acts <- all_unique_acts[!is.na(all_unique_acts) & !all_unique_acts %in% c("", "Choose not to answer")]
  missing_labels <- setdiff(all_unique_acts, names(activity_labels))
  if (length(missing_labels) > 0) {
    warning("Missing labels in activity_labels: ", paste(missing_labels, collapse = ", "))
  }
  
  y_lab_most <- if (weighted) "Percent of Adults" else "Percent of Respondents"
  y_lab_all  <- "Percent of Total Activities"
  
  # ========================================================================
  # PREP 1: Most Important Activity (q_activity_most3) Base Table
  # ========================================================================
  d_most_base <- d1 %>%
    mutate(q_activity_most3 = as.character(q_activity_most3)) %>%
    tidyr::separate_rows(q_activity_most3, sep = ",") %>%
    mutate(q_activity_most3 = str_trim(q_activity_most3)) %>%
    filter(
      !is.na(w_plot),
      !is.na(q_activity_most3),
      q_activity_most3 != "",
      q_activity_most3 != "Choose not to answer"
    ) %>%
    distinct(response_id, q_activity_most3, .keep_all = TRUE) %>%
    mutate(
      consumptive_label = factor(
        if_else(q_activity_consumptive_most_b == 1, "Consumptive", "Non-consumptive", missing = "Non-consumptive"),
        levels = c("Non-consumptive", "Consumptive")
      ),
      nature_strict_val = if ("nature_interact_strict" %in% names(.)) {
        nature_interact_strict
      } else {
        as.numeric(str_detect(q_activity_most3, nature_strict_regex))
      },
      nature_broad_val = if ("nature_interact_broad" %in% names(.)) {
        nature_interact_broad
      } else {
        as.numeric(str_detect(q_activity_most3, nature_broad_regex))
      },
      nature_strict_label = factor(
        nature_strict_val,
        levels = c(1, 0),
        labels = c("Marine Nature Interaction", "Other Coastal Activity")
      ),
      nature_broad_label = factor(
        nature_broad_val,
        levels = c(1, 0),
        labels = c("Marine Nature Interaction", "Other Coastal Activity")
      )
    )
  
  # Helper to summarize & order Most Important Activity by any grouping column
  prep_most_summary <- function(df, group_col) {
    res <- df %>%
      filter(!is.na(.data[[group_col]])) %>%
      group_by(q_activity_most3, .data[[group_col]]) %>%
      summarise(
        val_plot = sum(w_plot, na.rm = TRUE),
        val_stat = sum(w_stat, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        pct = val_plot / sum(val_plot),
        q_activity_most3 = fct_reorder(q_activity_most3, pct, .fun = sum, .desc = TRUE)
      )
    if ("Another activity" %in% levels(res$q_activity_most3)) {
      res <- res %>% mutate(q_activity_most3 = fct_relevel(q_activity_most3, "Another activity", after = Inf))
    }
    res
  }
  
  # PLOT 1: Most Important Activity - Consumptive vs Non-consumptive
  props_most <- prep_most_summary(d_most_base, "consumptive_label")
  f_build_activity_plot(
    props_most, "q_activity_most3", "consumptive_label",
    c("Non-consumptive" = "#5DA2B0", "Consumptive" = "#D24818"),
    "Activity Type", "Most Important Activity", y_lab_most,
    "bab_activities_most", out_dir, scope_tag, mode_tag, pattern_fill_col = "#D24818"
  )
  
  # PLOT 3: Most Important Activity - Strict Nature Interaction
  props_most_nature_strict <- prep_most_summary(d_most_base, "nature_strict_label")
  f_build_activity_plot(
    props_most_nature_strict, "q_activity_most3", "nature_strict_label",
    c("Marine Nature Interaction" = "#3E91A2", "Other Coastal Activity" = "#0A1F2E"),
    "Activity Type", "Most Important Activity", y_lab_most,
    "bab_activities_most_nature_strict", out_dir, scope_tag, mode_tag, pattern_fill_col = "#0A1F2E"
  )
  
  # PLOT 4: Most Important Activity - Broad Nature Interaction
  props_most_nature_broad <- prep_most_summary(d_most_base, "nature_broad_label")
  f_build_activity_plot(
    props_most_nature_broad, "q_activity_most3", "nature_broad_label",
    c("Marine Nature Interaction" = "#2B7A78", "Other Coastal Activity" = "#0A1F2E"),
    "Activity Type", "Most Important Activity", y_lab_most,
    "bab_activities_most_nature_broad", out_dir, scope_tag, mode_tag, pattern_fill_col = "#0A1F2E"
  )
  
  # ========================================================================
  # PREP 2: All Important Activities (q_activity_all3 - Exploded Multi-Select)
  # ========================================================================
  writein_cols <- intersect(
    c("QImportant_Activities_TEXT", "QImportant_Activities_Most_TEXT",
      "q_activities_text_2", "q_activities_most_text_2"),
    names(d1)
  )
  
  d_all_base <- d1 %>%
    mutate(q_activity_all3 = as.character(q_activity_all3)) %>%
    tidyr::separate_rows(q_activity_all3, sep = ",") %>%
    mutate(q_activity_all3 = str_trim(q_activity_all3)) %>%
    filter(
      !is.na(w_plot),
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
        q_activity_all3 %in% c("Fishing or collecting food") |
          (q_activity_all3 == "Nature Observing/Photographing/Education/Research" & has_consumptive_writein),
        1L, 0L
      ),
      consumptive_label = factor(
        if_else(q_activity_consumptive_b == 1L, "Consumptive", "Non-consumptive"),
        levels = c("Non-consumptive", "Consumptive")
      ),
      # Classify each exploded activity by strict & broad nature definitions
      nature_strict_label = factor(
        if_else(str_detect(q_activity_all3, nature_strict_regex), 1L, 0L),
        levels = c(1L, 0L),
        labels = c("Marine Nature Interaction", "Other Coastal Activity")
      ),
      nature_broad_label = factor(
        if_else(str_detect(q_activity_all3, nature_broad_regex), 1L, 0L),
        levels = c(1L, 0L),
        labels = c("Marine Nature Interaction", "Other Coastal Activity")
      )
    ) %>%
    select(-has_consumptive_writein)
  
  prep_all_summary <- function(df, group_col) {
    res <- df %>%
      filter(!is.na(.data[[group_col]])) %>%
      group_by(q_activity_all3, .data[[group_col]]) %>%
      summarise(
        val_plot = sum(w_plot, na.rm = TRUE),
        val_stat = sum(w_stat, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        pct = val_plot / sum(val_plot),
        q_activity_all3 = fct_reorder(q_activity_all3, pct, .fun = sum, .desc = TRUE)
      )
    if ("Another activity" %in% levels(res$q_activity_all3)) {
      res <- res %>% mutate(q_activity_all3 = fct_relevel(q_activity_all3, "Another activity", after = Inf))
    }
    res
  }
  
  # PLOT 2: All Activities - Consumptive vs Non-consumptive
  props_all <- prep_all_summary(d_all_base, "consumptive_label")
  f_build_activity_plot(
    props_all, "q_activity_all3", "consumptive_label",
    c("Non-consumptive" = "#8EBEC8", "Consumptive" = "#D24818"),
    "Activity Type", "Activity", y_lab_all,
    "bab_activities_all", out_dir, scope_tag, mode_tag, pattern_fill_col = "#D24818"
  )
  
  # PLOT 5: All Activities - Strict Nature Interaction
  props_all_nature_strict <- prep_all_summary(d_all_base, "nature_strict_label")
  f_build_activity_plot(
    props_all_nature_strict, "q_activity_all3", "nature_strict_label",
    c("Marine Nature Interaction" = "#3E91A2", "Other Coastal Activity" = "#0A1F2E"),
    "Activity Type (Strict)", "Activity", y_lab_all,
    "bab_activities_all_nature_strict", out_dir, scope_tag, mode_tag, pattern_fill_col = "#0A1F2E"
  )
  
  # PLOT 6: All Activities - Broad Nature Interaction
  props_all_nature_broad <- prep_all_summary(d_all_base, "nature_broad_label")
  f_build_activity_plot(
    props_all_nature_broad, "q_activity_all3", "nature_broad_label",
    c("Marine Nature Interaction" = "#2B7A78", "Other Coastal Activity" = "#0A1F2E"),
    "Activity Type (Broad)", "Activity", y_lab_all,
    "bab_activities_all_nature_broad", out_dir, scope_tag, mode_tag, pattern_fill_col = "#0A1F2E"
  )
  
  invisible(list(
    most               = props_most,
    most_nature_strict = props_most_nature_strict,
    most_nature_broad  = props_most_nature_broad,
    all                = props_all,
    all_nature_strict  = props_all_nature_strict,
    all_nature_broad   = props_all_nature_broad
  ))
}

# ----------------------------------------------------------
# 4. Run Across All 3 Scopes (Raw & Weighted = 6 Runs) -----
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

# Execute all 6 combinations (focal -> ./doc; statewide & coastal -> ./doc/activity_variations)
results_list <- purrr::pmap(run_grid, f_plot_activities)
names(results_list) <- paste0(run_grid$scope_tag, "_", ifelse(run_grid$weighted, "weighted", "raw"))