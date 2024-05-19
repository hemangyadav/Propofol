if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, stddiff)

Sys.setlocale("LC_ALL", "English")
rm(list = ls())     # CLEAR GLOBAL ENVIRONMENT (RESET R)

setwd(here())      # Auto-set work directory to the parent folder
source("propofol_descriptive_table.R")

remove(f_data)

# Filter f_data to only include patients with valid TGL measurements in ICU
f_data_smd <- f_data_smd %>% filter(hypertgl != -1)

# 21MAR2024: Switch to approach to using a filtering csv file
f_data_smd_baseline <- as.data.frame(f_data_smd %filter_by% read.csv("./data/def_baseline_export.csv"))
f_data_smd_comparison <- as.data.frame(f_data_smd %filter_by% read.csv("./data/def_comparison_export.csv"))

# Create dataframe to store SMD outcomes
baseline_smd <- data.frame(Variable = character(), SMD = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
                           stringsAsFactors = FALSE)
comparison_smd <- data.frame(Variable = character(), SMD = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
                             stringsAsFactors = FALSE)

# Loop through all columns except for hypertgl
for (var in colnames(f_data_smd_baseline)[-which(colnames(f_data_smd_baseline) == "hypertgl")]) {
  baseline_smd <- bind_rows(baseline_smd, get_smd(f_data_smd_baseline, var, "hypertgl"))
  rm (var)
}

for (var in colnames(f_data_smd_comparison)[-which(colnames(f_data_smd_comparison) == "hypertgl")]) {
  comparison_smd <- bind_rows(comparison_smd, get_smd(f_data_smd_comparison, var, "hypertgl"))
  rm (var)
}

write.csv(baseline_smd, "baseline_table_smd_unmatched.csv")
write.csv(comparison_smd, "comparison_table_smd_unmatched.csv")