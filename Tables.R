
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, here, arsenal, purrr, janitor, dplyr)    # Load pre-requisite packages

Sys.setlocale("LC_ALL", "English")
rm(list = ls())     # CLEAR GLOBAL ENVIRONMENT (RESET R)

setwd(here())      # Auto-set work directory to the parent folder
source("propofol_utils.R")              # Load utility functions

# Re-save xlsx file as csv before running
# Edit def_columns.csv as needed if more columns need to be imported
r_data <- fread("./data/23-007416 Main Data File.csv", header=TRUE, colClasses="character")
f_data <- r_data %filter_by% read.csv("./data/def_columns.csv")    # Filter data and cast to proper types

r_data_pancreatitis <- fread("./data/pancreatitis.csv", header=TRUE, colClasses="character")
f_data_pancreatitis <- r_data_pancreatitis %filter_by% read.csv("./data/def_columns_pancreatitis.csv")    # Filter data and cast to proper types

# Rename MDE Search Results for Lab Tests.csv before running
r_data_hba1c <- fread("./data/hba1c.csv", header=TRUE, colClasses="character")
f_data_hba1c <- r_data_hba1c %filter_by% read.csv("./data/def_columns_hba1c.csv")

source("propofol_filtering_code.R")   # Filter dataset to keep needed columns only

# Rename the two cohort groups
f_data$hypertgl <- as.character(f_data$hypertgl)

# Make a copy of f_data for SMD purposes
f_data_smd <- f_data

f_data[f_data$hypertgl==0,]$hypertgl <- "No Hypertriglyceridemia During ICU Stay"
f_data[f_data$hypertgl==1,]$hypertgl <- "Hypertriglyceridemia During ICU Stay"
f_data[f_data$hypertgl==-1,]$hypertgl <- "No Triglyceride Measurements During ICU Stay"

baseline_export_data <- f_data %filter_by% read.csv("./data/def_baseline_export.csv")
comparison_export_data <- f_data[!is.na(f_data$triglycerides),] %filter_by%
  read.csv("./data/def_comparison_export.csv")
#
# # MEAN/SD
# write2word(create_tbl(data=baseline_export_data, group_by="hypertgl", total=TRUE, digits=2),
#            "baseline_table_mean.doc", title="Baseline Data", quiet=TRUE)
# write2word(create_tbl(data=comparison_export_data, group_by="hypertgl", total=TRUE, digits=2),
#            "comparison_table_mean.doc", title="Comparison Data", quiet=TRUE)
#
# # MEDIAN/IQR
write2word(create_tbl(data=baseline_export_data, group_by="hypertgl", total=TRUE, digits=2,numeric.test="kwt",
                      numeric.stats=c("Nmiss","median","q1q3")),
           "baseline_table_median.doc", title="Baseline Data", quiet=TRUE)
write2word(create_tbl(data=comparison_export_data, group_by="hypertgl", total=TRUE, digits=2, numeric.test="kwt",
                      numeric.stats=c("Nmiss","median","q1q3")),
           "comparison_table_median.doc", title="Comparison Data", quiet=TRUE)
#
# remove(baseline_export_data)
# remove(comparison_export_data)
