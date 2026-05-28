# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: summarizing q13 barrier question

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 
library(colorspace)
library(purrr)
library(readr)


# ---------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")
source("./bin/deets.R")

###Load in Data file -------------------
d0<-read_csv("./results/q13_barrier_long.csv")%>%
  glimpse()

# focus group version
d1<-read_csv("./results/q13_barrier_long_fg.csv")%>%
  glimpse()
names(d1)

# focus group list version - have not made this yet
# l1<- read_rds("./results/q13_barrier_long_fg.rds")
# l1$Santa_Rosa

# make lists for each fg
fg<-unique(d1$Focus_Group)

l1 <- d1 %>%
  filter(Focus_Group %in% fg) %>%
  group_split(Focus_Group) %>%
  set_names(., map_chr(., ~unique(.x$Focus_Group)))

# Glimpse every data frame in the list to inspect them
# walk(l1, glimpse)
l1$Eureka


# map(l1, your_function)

# f1<-function(list){

# summarize -------------------

# n respondents - includes NA --------------
d1c<-d1%>%
  group_by(barrier,Focus_Group)%>%
  summarize(
    n=n())%>%
  glimpse() 


# responses - includes NA --------------
d1d<-d1%>%
  group_by(barrier,response,Focus_Group)%>%
  summarize(
    n_val=n())%>%
  mutate(pct=round(n_val/d1c$n[1],3))%>%
  glimpse()

# n respondents - no NA --------------
d1e<-d1%>%
  filter(!is.na(response,Focus_Group))%>%
  group_by(barrier)%>%
  summarize(
    n_tot=n())%>%
  glimpse() 

# responses - no NA --------------
d1f<-d1%>%
  filter(!is.na(response,Focus_Group))%>%
  group_by(barrier,response)%>%
  summarize(
    n_val=n())%>%
  full_join(d1e)%>%
  mutate(pct=round(n_val/n_tot,3))%>%
  glimpse()

unique(d1f$response)

# -----------------------------------------------
# prep for graphing ------------------------------
source("./bin/deets.R")


# # order factors ---------------------------
d1f$response <- factor(d1f$response, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))

d1f
# view(d1f)

#make Disagree and Strongly disagree percentages negative show up of the left side of the y-axis
d1g0<-d1f%>%
  mutate(
    pct2 = case_when(
      response %in% c("Strongly agree", "Agree") ~ pct,
      response == "Neutral"                      ~ pct / 2,
      response %in% c("Disagree", "Strongly disagree") ~ -pct,
      TRUE ~ pct
    )
  )
neutral_rows <- d1g0 %>% filter(response == "Neutral") %>% mutate(pct2 = -pct2)
d1g2 <- bind_rows(d1g0, neutral_rows)

d1g <- d1g2 %>%
  group_by(barrier) %>%
  mutate(
    # Sum only the positive agreement sides to order your chart cleanly
    overall = sum(pct2[pct2 > 0]) 
  ) %>%
  ungroup() %>%
  mutate(barrier = reorder(barrier, overall)) %>%
  glimpse()

# order
d1g$response<-ordered(d1g$response, levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree","Neutral"))


##Order Prompts so the highest level of "Strongly agree" is at the top -------------
Factor_Order<-d1g[which(d1g$response=="Strongly agree"),]
Factor_Order<-Factor_Order[order(Factor_Order$pct2),]
Order<-Factor_Order$barrier
d1g$barrier <- ordered(d1g$barrier, levels=Order)

# graph -------------------------------------
source("./bin/deets.R")

ggplot(d1g, aes(y = barrier, x = pct2, fill = response)) + 
  # 1. Kept only geom_col (removed duplicate geom_bar)
  geom_col(orientation = 'y', width = 0.6) +
  
  # 2. Fixed 'response =' to 'values =' 
  scale_fill_manual(
    values = c(
      "Strongly agree"    = "#002F70", 
      "Agree"             = "#879FDB", 
      "Neutral"           = "grey50", 
      "Disagree"          = "#DA8A8B", 
      "Strongly disagree" = "#5F1415"
    ),
    breaks = c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
    name = ""
  ) +
  geom_vline(xintercept = 0) +
  xlab("% of Respondents") + 
  ylab("") +
  xlim(c(-1, 1)) +
  theme_bw() + 
  ggtitle("Use and/or experience within ocean and \ncoastal areas:") +
  deets9

ggsave("./doc/q13_barrier_fg_all.png", width=12, height=4.5, units="in")
# ggsave("./doc/q13_barrier_low.png", width=12, height=4.5, units="in")















# again as a function --------------------
f1 <- function(df) {
  # 0. Extract the group name for dynamic plot titles and file saving
  # (Assumes all rows in the current df belong to the same Focus_Group)
  group_name <- unique(df$Focus_Group)[1]
  if (is.na(group_name)) group_name <- "Unknown_Group"
  
  # 1. Summarize total respondents (no NA)
  d1e <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier) %>%
    summarize(n_tot = n(), .groups = "drop")
  
  # 2. Summarize response percentages (no NA)
  d1f <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier, response) %>%
    summarize(n_val = n(), .groups = "drop") %>%
    full_join(d1e, by = "barrier") %>%
    mutate(pct = round(n_val / n_tot, 3))
  
  # Ensure response is a factor with explicit levels
  d1f$response <- factor(
    d1f$response, 
    levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree")
  )
  
  # 3. Prep for Likert divergent graphing (handling the split Neutral)
  d1g0 <- d1f %>%
    mutate(
      pct2 = case_when(
        response %in% c("Strongly agree", "Agree") ~ pct,
        response == "Neutral"                      ~ pct / 2,
        response %in% c("Disagree", "Strongly disagree") ~ -pct,
        TRUE ~ pct
      )
    )
  
  neutral_rows <- d1g0 %>% filter(response == "Neutral") %>% mutate(pct2 = -pct2)
  d1g2 <- bind_rows(d1g0, neutral_rows)
  
  # 4. Calculate overall agreement for standard sorting
  d1g <- d1g2 %>%
    group_by(barrier) %>%
    mutate(overall = sum(pct2[pct2 > 0])) %>%
    ungroup()
  
  # Factor order for plot fill logic
  d1g$response <- ordered(
    d1g$response, 
    levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree", "Neutral")
  )
  
  # 5. Order items so highest "Strongly agree" is at the top
  factor_order <- d1g %>% 
    filter(response == "Strongly agree") %>% 
    arrange(pct2)
  
  d1g$barrier <- ordered(d1g$barrier, levels = factor_order$barrier)
  
  # 6. Graph Generation
  # Dynamically including the Group Name in the title
  p <- ggplot(d1g, aes(y = barrier, x = pct2, fill = response)) + 
    geom_col(orientation = 'y', width = 0.6) +
    scale_fill_manual(
      values = c(
        "Strongly agree"    = "#002F70", 
        "Agree"             = "#879FDB", 
        "Neutral"           = "grey50", 
        "Disagree"          = "#DA8A8B", 
        "Strongly disagree" = "#5F1415"
      ),
      breaks = c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
      name = ""
    ) +
    geom_vline(xintercept = 0) +
    xlab("% of Respondents") + 
    ylab("") +
    xlim(c(-1, 1)) +
    theme_bw() + 
    ggtitle(paste0("Use and/or experience within ocean and \ncoastal areas in ", group_name, ":")) +
    deets9
  
  # 7. Dynamic File Saving
  # Converts "SD_South_County" to a clean filename component
  clean_filename <- paste0("./doc/q13_barrier_fg_", tolower(group_name), ".png")
  ggsave(clean_filename, plot = p, width = 12, height = 4.5, units = "in")
  
  # Return the plot object silently in case you want to view it in R
  return(p)
}


source("./bin/deets.R")

# 2. Run the function automatically across every data frame in your list `l1`
walk(l1, f1)











# NEGATIVE FUNCTION

f2 <- function(df) {
  # 0. Extract the group name for dynamic plot titles and file saving
  group_name <- unique(df$Focus_Group)[1]
  if (is.na(group_name)) group_name <- "Unknown_Group"
  
  # 1. Summarize total respondents (no NA)
  d1e <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier) %>%
    summarize(n_tot = n(), .groups = "drop")
  
  # 2. Summarize response percentages (no NA)
  d1f <- df %>%
    filter(!is.na(response)) %>%
    group_by(barrier, response) %>%
    summarize(n_val = n(), .groups = "drop") %>%
    full_join(d1e, by = "barrier") %>%
    mutate(pct = round(n_val / n_tot, 3))
  
  d1f$response <- factor(
    d1f$response, 
    levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree")
  )
  
  # 3. Prep for Likert divergent graphing (handling the split Neutral)
  d1g0 <- d1f %>%
    mutate(
      pct2 = case_when(
        response %in% c("Strongly agree", "Agree") ~ pct,
        response == "Neutral"                      ~ pct / 2,
        response %in% c("Disagree", "Strongly disagree") ~ -pct,
        TRUE ~ pct
      )
    )
  
  neutral_rows <- d1g0 %>% filter(response == "Neutral") %>% mutate(pct2 = -pct2)
  d1g2 <- bind_rows(d1g0, neutral_rows)
  
  # 4. Calculate overall agreement for standard sorting
  d1g <- d1g2 %>%
    group_by(barrier) %>%
    mutate(overall = sum(pct2[pct2 > 0])) %>%
    ungroup()
  
  d1g$response <- ordered(
    d1g$response, 
    levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree", "Neutral")
  )
  
  # 5. NEW ORDER LOGIC: Sort by highest "Strongly disagree"
  # Since Strongly Disagree pct2 values are negative (e.g., -0.40 vs -0.10), 
  # arranging them in ascending order puts the biggest disagreement at the top.
  factor_order_disagree <- d1g %>% 
    filter(response %in% c("Strongly disagree", "Disagree")) %>% 
    group_by(barrier) %>% 
    summarize(total_disagreement = sum(pct2), .groups = "drop") %>% 
    arrange(desc(total_disagreement))
  
  # Apply the flipped order to your barrier factor
  d1g$barrier <- ordered(d1g$barrier, levels = factor_order_disagree$barrier)
  
  # 6. Graph Generation
  p <- ggplot(d1g, aes(y = barrier, x = pct2, fill = response)) + 
    geom_col(orientation = 'y', width = 0.6) +
    scale_fill_manual(
      values = c(
        "Strongly agree"    = "#002F70", 
        "Agree"             = "#879FDB", 
        "Neutral"           = "grey50", 
        "Disagree"          = "#DA8A8B", 
        "Strongly disagree" = "#5F1415"
      ),
      breaks = c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
      name = ""
    ) +
    geom_vline(xintercept = 0) +
    xlab("% of Respondents") + 
    ylab("") +
    coord_cartesian(xlim = c(-1, 1)) +
    theme_bw() + 
    ggtitle(paste0("Use and/or experience within ocean and \ncoastal areas in ", group_name, ":")) +
    deets9
  
  # 7. Dynamic File Saving (Appends '_disagreement' to the filename)
  clean_filename <- paste0("./doc/q13_barrier_fg_", tolower(group_name), "_disagreement.png")
  ggsave(clean_filename, plot = p, width = 12, height = 4.5, units = "in")
  
  return(p)
}


walk(l1, f2)
