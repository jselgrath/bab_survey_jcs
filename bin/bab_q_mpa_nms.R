# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation / CINMS

# Goal: Weighted analysis and graphs for MPA and NMS questions (Focal Counties)

# ----------------------------------------------------------
# 1. Load libraries ----------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# ----------------------------------------------------------
# 2. Load and prepare data ---------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1 <- read_csv("./results/data_final_weighted_focal.csv", guess_max = Inf, show_col_types = FALSE) %>%
  mutate(
    # Collapse and order MPA Familiarity
    QMPA_Aware_clean = fct_collapse(
      QMPA_Aware,
      "Extremely familiar"             = c("Very familiar", "Extremely familiar"),
      "Not familiar\n(never heard of)" = c("Not familiar at all (never heard of them)")
    ) %>%
      factor(levels = c(
        "Not familiar\n(never heard of)",
        "Slightly familiar",
        "Moderately familiar",
        "Extremely familiar"
      )),
    
    # Collapse and order NMS Familiarity
    QSanct_Aware_clean = fct_collapse(
      QSanct_Aware,
      "Extremely familiar"             = c("Very familiar", "Extremely familiar"),
      "Not familiar\n(never heard of)" = c("Not familiar at all (never heard of them)")
    ) %>%
      factor(levels = c(
        "Not familiar\n(never heard of)",
        "Slightly familiar",
        "Moderately familiar",
        "Extremely familiar"
      )),
    
    # Order MPA/NMS Purpose (2025 only)
    QMPA_Purpose_clean = factor(
      QMPA_Purpose,
      levels = c(
        "No understanding",
        "Slight understanding",
        "Moderate understanding",
        "Full understanding"
      )
    ),
    
    # Order Role of Science for MPAs/NMS (2025 only)
    QMPA_Science_clean = factor(
      QMPA_Science,
      levels = c(
        "No understanding",
        "Slight understanding",
        "Moderate understanding",
        "Full understanding"
      )
    )
  )

# ----------------------------------------------------------
# 3. Reusable Weighted Plotting Function -------------------
f_plot_mpa_nms <- function(data, col_name, x_label, palette_name, out_file) {
  
  # Deduplicate by response_id and remove missing responses/weights
  df_clean <- data %>%
    filter(!is.na(.data[[col_name]]), !is.na(weight_focal_pop), !is.na(weight_within_stratum)) %>%
    distinct(response_id, .keep_all = TRUE)
  
  # Weighted counts & percentages
  props <- df_clean %>%
    group_by(category = .data[[col_name]]) %>%
    summarise(
      raw_n    = n(),
      val_plot = sum(weight_focal_pop, na.rm = TRUE),
      val_stat = sum(weight_within_stratum, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(pct = val_plot / sum(val_plot))
  
  # Chi-squared goodness-of-fit test (scaled to actual sample size via weight_within_stratum)
  chi <- suppressWarnings(chisq.test(props$val_stat))
  chi_stat <- unname(chi$statistic)
  chi_df   <- unname(chi$parameter)
  fmt_p    <- ifelse(chi$p.value < .001, "< 0.001", scales::number(chi$p.value, accuracy = 0.001))
  
  # Print standardized residuals to console for inspection
  message(sprintf("\n--- %s (n = %s) ---", gsub("\n", " ", x_label), scales::comma(sum(props$raw_n))))
  print(data.frame(
    Category = props$category,
    Raw_n    = props$raw_n,
    Pct      = round(props$pct * 100, 1),
    Std_Res  = round(chi$stdres, 2)
  ))
  
  # Build plot
  p <- ggplot(props, aes(x = category, y = pct, fill = category)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.4, size = 3.8) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, max(props$pct) * 1.15)
    ) +
    scale_fill_discrete_sequential(palette = palette_name) +
    labs(
      x = x_label,
      y = "Weighted Percent of Adults",
      subtitle = paste0("\u03C7\u00B2(", chi_df, ") = ", round(chi_stat, 2), ", p ", fmt_p)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(face = "bold"),
      axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.subtitle = element_text(color = "grey40", hjust = 1)
    )
  
  ggsave(out_file, plot = p, width = 6, height = 8, units = "in", dpi = 300, bg = "white")
  invisible(props)
}

# ----------------------------------------------------------
# 4. Generate All 4 Weighted Focal Graphs ------------------
mpa_nms_specs <- tribble(
  ~col_name,            ~x_label,                                        ~palette_name, ~out_file,
  "QMPA_Aware_clean",   "Familiarity with MPAs",                         "Teal",        "./doc/bab_graph_mpa_focal_weighted.png",
  "QSanct_Aware_clean", "Familiarity with\nNational Marine Sanctuaries", "Teal",        "./doc/bab_graph_nms_focal_weighted.png",
  "QMPA_Purpose_clean", "Purpose of\nSanctuaries & MPAs",                "Purp",        "./doc/bab_graph_mpa_purpose_focal_weighted.png",
  "QMPA_Science_clean", "Role of Science for\nSanctuaries & MPAs",       "Peach",       "./doc/bab_graph_mpa_science_focal_weighted.png"
)

mpa_nms_results <- purrr::pmap(
  mpa_nms_specs,
  ~ f_plot_mpa_nms(d1, ..1, ..2, ..3, ..4)
)
names(mpa_nms_results) <- c("mpa_aware", "nms_aware", "mpa_purpose", "mpa_science")