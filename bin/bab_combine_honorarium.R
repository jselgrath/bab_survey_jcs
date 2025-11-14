# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: load .csv files from 2025 survey versions and combine

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# ------------------------------------------------------------------
# load data ######--------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/bab_survey_jcs")

# Set your folder path
folder <- "./data/08.10.2025_bab_qualtrics_data"

# List all CSV files
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

# Read them into a named list
d_list0 <- lapply(files, read.csv, stringsAsFactors = FALSE)
names(d_list0) <- basename(files)

# create a colunm in each dataset with the file name
d_list <- imap(d_list0, ~ mutate(.x, source_file = .y))
glimpse(d_list)

# Extracd_list# Extract column names for each file
colnames_list <- lapply(d_list, colnames)

# View them
colnames_list

# compare unique column names
unique_cols <- sort(unique(unlist(colnames_list)))
unique_cols


# Make a data frame showing whether each file contains each column
comparison_matrix <- sapply(colnames_list, function(x) unique_cols %in% x)
rownames(comparison_matrix) <- unique_cols

comparison_matrix


# Identify columns that differ among files
cols_differ <- unique_cols[rowSums(comparison_matrix) != length(colnames_list)]

cols_differ


# look at names
names(d_list)
d_list2<-d_list[1:5]
names(d_list2)

glimpse(d_list2[[1]])
glimpse(d_list2[[2]])



# Extract column names for each file
colnames_list2 <- lapply(d_list2, colnames)

# View them
colnames_list2

# compare unique column names
unique_cols2 <- sort(unique(unlist(colnames_list2)))
unique_cols2


# Make a data frame showing whether each file contains each column
comparison_matrix2 <- sapply(colnames_list2, function(x) unique_cols2 %in% x)
rownames(comparison_matrix2) <- unique_cols2

comparison_matrix2


# Identify columns that differ among files
cols_differ2 <- unique_cols2[rowSums(comparison_matrix2) != length(colnames_list2)]

cols_differ2


# combine in person honorarium survey data
d_honorarium <- do.call(rbind, d_list2)
glimpse(d_honorarium)

# save honorarium versions
write_csv(d_honorarium,"./results/data_honorarium_versions.csv")
