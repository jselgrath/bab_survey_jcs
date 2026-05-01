# Jennifer Selgrath 
# Equity in Ocean Access (Benefits and Barriers (bab))
# California Marine Sanctuary Foundation/ CINMS

# goal: separate open comment question with respondent ID from other data for cleaning
# updated function - done after most surveys were cleaned except last three

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
library(textclean)
library(stringi)
library(readxl)
library(janitor)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# 1. Get a list of file paths
folder_path <- "./data/comments/"
files <- list.files(path = folder_path, 
                    pattern = "\\.xlsx$|\\.xls$", 
                    full.names = TRUE)
files

# CONVERT TO CSV ---
  walk(files, function(f) {
    df <- read_excel(f)
    csv_name <- paste0(tools::file_path_sans_ext(f), ".csv")
    write_csv(df, csv_name)
  })
