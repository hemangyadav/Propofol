#This could should be used in conjunction with the other files

f_data <- f_data %>% mutate("statin_combined" = atorvastatin_icu | lovastatin_icu | rosuvastatin_icu | simvastatin_icu)
f_data <- f_data %>% mutate("vasopressor_combined" = norepinephrine_icu | epinephrine_icu)
f_data <- f_data %>% mutate("antipsychotics_combined" = aripiprazole_icu | clozapine_icu | ziprasidone_icu | olanzapine_icu | risperidone_icu | quetiapine_icu)
f_data <- f_data %>% mutate("steroids_combined" = dexamethasone_icu | hydrocortisone_icu | methylprednisolone_icu | prednisone_icu)


#Community vs academic
f_data <- f_data %>% mutate(
  community_hospital = admission_icu == "Mankato ICU" | admission_icu == "Eau Claire ICU")

# Process sedative data
f_data <- f_data %>% rowwise() %>% mutate(
  # Detect which sedatives are in progress at PSD0 (at propofol initiation)
  # Check through fentanyl, dex, midazolam, ketamine, hmorphone, lorazepam
  fentanyl_at_PSD0 = propofol_sttime %is_between% list(fentanyl_sttime, fentanyl_endtime),
  dex_at_PSD0 = propofol_sttime %is_between% list(dex_sttime, dex_endtime),
  midazolam_at_PSD0 = propofol_sttime %is_between% list(midazolam_sttime, midazolam_endtime),
  ketamine_at_PSD0 = propofol_sttime %is_between% list(ketamine_sttime, ketamine_endtime),
  hmorphone_at_PSD0 = propofol_sttime %is_between% list(hmorphone_sttime, hmorphone_endtime),
  lorazepam_at_PSD0 = propofol_sttime %is_between% list(lorazepam_sttime, lorazepam_endtime),

  # Detect which sedatives are started on or at end of propofol
  fentanyl_at_propofol_endtime = fentanyl_sttime %time_is_equals_after% propofol_endtime,
  dex_at_propofol_endtime = dex_sttime %time_is_equals_after% propofol_endtime,
  midazolam_at_propofol_endtime = midazolam_sttime %time_is_equals_after% propofol_endtime,
  ketamine_at_propofol_endtime = ketamine_sttime %time_is_equals_after% propofol_endtime,
  hmorphone_at_propofol_endtime = hmorphone_sttime %time_is_equals_after% propofol_endtime,
  lorazepam_at_propofol_endtime = lorazepam_sttime %time_is_equals_after% propofol_endtime,
  
  # Detect which sedatives are started at or after hypertgl start
  fentanyl_after_hypertgl = fentanyl_sttime %time_is_equals_after% hypertgl_sttime,
  dex_after_hypertgl = dex_sttime %time_is_equals_after% hypertgl_sttime,
  midazolam_after_hypertgl = midazolam_sttime %time_is_equals_after% hypertgl_sttime,
  ketamine_after_hypertgl = ketamine_sttime %time_is_equals_after% hypertgl_sttime,
  hmorphone_after_hypertgl = hmorphone_sttime %time_is_equals_after% hypertgl_sttime,
  lorazepam_after_hypertgl = lorazepam_sttime %time_is_equals_after% hypertgl_sttime,
  
  # Get number of days from hypertgl onset to start of sedatives
  fentanyl_ndays_hypertgl_start = get_n_days_after_if_equal_after(fentanyl_sttime, hypertgl_sttime),
  dex_ndays_hypertgl_start = get_n_days_after_if_equal_after(dex_sttime, hypertgl_sttime),
  midazolam_ndays_hypertgl_start = get_n_days_after_if_equal_after(midazolam_sttime, hypertgl_sttime),
  ketamine_ndays_hypertgl_start = get_n_days_after_if_equal_after(ketamine_sttime, hypertgl_sttime),
  hmorphone_ndays_hypertgl_start = get_n_days_after_if_equal_after(hmorphone_sttime, hypertgl_sttime),
  lorazepam_ndays_hypertgl_start = get_n_days_after_if_equal_after(lorazepam_sttime, hypertgl_sttime),

  # Get number of hours from hypertgl onset to start of sedatives
  fentanyl_nhours_hypertgl_start = get_time_after_if_equal_after(fentanyl_sttime, hypertgl_sttime, "hours"),
  dex_nhours_hypertgl_start = get_time_after_if_equal_after(dex_sttime, hypertgl_sttime, "hours"),
  midazolam_nhours_hypertgl_start = get_time_after_if_equal_after(midazolam_sttime, hypertgl_sttime, "hours"),
  ketamine_nhours_hypertgl_start = get_time_after_if_equal_after(ketamine_sttime, hypertgl_sttime, "hours"),
  hmorphone_nhours_hypertgl_start = get_time_after_if_equal_after(hmorphone_sttime, hypertgl_sttime, "hours"),
  lorazepam_nhours_hypertgl_start = get_time_after_if_equal_after(lorazepam_sttime, hypertgl_sttime, "hours"),
  
  # Get number of days from end of propofol to start of sedatives
  # Sedative that started before propofol is marked as NA
  fentanyl_ndays_propofol_end = get_n_days_after_if_equal_after(fentanyl_sttime, propofol_endtime),
  dex_ndays_propofol_end = get_n_days_after_if_equal_after(dex_sttime, propofol_endtime),
  midazolam_ndays_propofol_end = get_n_days_after_if_equal_after(midazolam_sttime, propofol_endtime),
  ketamine_ndays_propofol_end = get_n_days_after_if_equal_after(ketamine_sttime, propofol_endtime),
  hmorphone_ndays_propofol_end = get_n_days_after_if_equal_after(hmorphone_sttime, propofol_endtime),
  lorazepam_ndays_propofol_end = get_n_days_after_if_equal_after(lorazepam_sttime, propofol_endtime),

  # Get number of hours from end of propofol to start of sedatives
  # Sedative that started before propofol is marked as NA
  fentanyl_nhours_propofol_end = get_time_after_if_equal_after(fentanyl_sttime, propofol_endtime, "hours"),
  dex_nhours_propofol_end = get_time_after_if_equal_after(dex_sttime, propofol_endtime, "hours"),
  midazolam_nhours_propofol_end = get_time_after_if_equal_after(midazolam_sttime, propofol_endtime, "hours"),
  ketamine_nhours_propofol_end = get_time_after_if_equal_after(ketamine_sttime, propofol_endtime, "hours"),
  hmorphone_nhours_propofol_end = get_time_after_if_equal_after(hmorphone_sttime, propofol_endtime, "hours"),
  lorazepam_nhours_propofol_end = get_time_after_if_equal_after(lorazepam_sttime, propofol_endtime, "hours"),
) %>% ungroup()

# Summarize sedative columns
f_data <- f_data %>% mutate(
  # Check if ANY sedatives are being used at PSD0
  sedative_at_PSD0 = dex_at_PSD0 | midazolam_at_PSD0 |
    ketamine_at_PSD0 | lorazepam_at_PSD0,

  # Check if ANY new sedatives are started at end of propofol
  sedative_at_propofol_endtime = dex_at_propofol_endtime |
    midazolam_at_propofol_endtime | ketamine_at_propofol_endtime | lorazepam_at_propofol_endtime,
  
  # Check if ANY new sedatives are started at hypertgl onset
  sedative_after_hypertgl = dex_after_hypertgl |
    midazolam_after_hypertgl | ketamine_after_hypertgl | lorazepam_after_hypertgl,

  # Get number of days from hypertgl onset to start of new sedatives
  earliest_new_sedative_ndays_hypertgl_start = {
    min_ndays <- pmin(dex_ndays_hypertgl_start, midazolam_ndays_hypertgl_start,
                      ketamine_ndays_hypertgl_start, lorazepam_ndays_hypertgl_start, na.rm = TRUE)
    ifelse(is.infinite(min_ndays), NA, min_ndays)
  },

  # Get number of hours from hypertgl onset to start of new sedatives
  earliest_new_sedative_nhours_hypertgl_start = {
    min_nhours <- pmin(dex_nhours_hypertgl_start, midazolam_nhours_hypertgl_start,
                       ketamine_nhours_hypertgl_start, lorazepam_nhours_hypertgl_start, na.rm = TRUE)
    ifelse(is.infinite(min_nhours), NA, min_nhours)
  },

  # Get number of days from end of propofol to start of new sedatives
  # Earliest date taken if multiple sedatives started
  earliest_new_sedative_ndays_propofol_end = {
    min_ndays <- pmin(dex_ndays_propofol_end, midazolam_ndays_propofol_end,
      ketamine_ndays_propofol_end, lorazepam_ndays_propofol_end, na.rm = TRUE)
    ifelse(is.infinite(min_ndays), NA, min_ndays)
  },
  
  # Get number of hours from end of propofol to start of new sedatives
  # Earliest date taken if multiple sedatives started
  earliest_new_sedative_nhours_propofol_end = {
    min_nhours <- pmin(dex_nhours_propofol_end, midazolam_nhours_propofol_end,
      ketamine_nhours_propofol_end, lorazepam_nhours_propofol_end, na.rm = TRUE)
    ifelse(is.infinite(min_nhours), NA, min_nhours)
  }
  
  ) %>% ungroup()

# Get number of patients who developed hypertriglyceridemia before propofol start time
f_data <- f_data %>% mutate(
  hypertgl_after_propofol_end =
    ifelse(is.na(hypertgl_sttime), FALSE, difftime(propofol_endtime, hypertgl_sttime, units = "hours") < 0)
)

# Process duration and dose of propofol until hypertriglyceridemia
f_data <- f_data %>% mutate(
  # Calculate number of days from propofol start to hypertriglyceridemia; for table purposes
  hypertgl_psd = difftime(hypertgl_sttime, propofol_sttime, units = "hours") / 24,

  # Create a rounded version of hypertgl_psd, for grabbing dosing information
  hypertgl_psd_r = round(hypertgl_psd, digits = 0),

  # Set negative values (hypertgl before propofol) or missing values to NA
  hypertgl_psd = if_else(hypertgl_after_propofol_end | (!is.na(hypertgl_psd) & hypertgl_psd < 0), NA, hypertgl_psd),
  hypertgl_psd_r = if_else(hypertgl_after_propofol_end | (!is.na(hypertgl_psd_r) & hypertgl_psd_r < 0), NA, hypertgl_psd_r),

  # Exclude cases where hypertriglyceridemia developed after 10+ days
  hypertgl_psd_r = if_else(hypertgl_psd_r > 10, NA, hypertgl_psd_r)
) %>% rowwise() %>% mutate(
  propofol_dose_before_hypertgl = ifelse(is.na(hypertgl_psd_r), NA,
   sum(c_across(starts_with("propofol_dose_"))[0:hypertgl_psd_r], na.rm = TRUE)),

  propofol_dose_per_kg_before_hypertgl = ifelse(is.na(hypertgl_psd_r), NA,
   sum(c_across(starts_with("propofol_dose_"))[0:hypertgl_psd_r], na.rm = TRUE) / weight)

) %>% ungroup()

f_data <- f_data %>% mutate(
  # Get number of days from developing hypertriglyceridemia to propofol end
  ndays_from_hypertgl_to_propofol_endtime =
    ifelse(is.na(hypertgl_sttime) | difftime(propofol_endtime, hypertgl_sttime, units = "hours") <= 0, NA,
           difftime(propofol_endtime, hypertgl_sttime, units = "hours") / 24)
) %>% rowwise() %>% mutate(
  # Get cumulative propofol dose from developing hypertriglyceridemia to propofol end
  propofol_dose_after_hypertgl = ifelse(is.na(hypertgl_psd_r), NA,
    sum(c_across(starts_with("propofol_dose_"))[hypertgl_psd_r+1:11], na.rm = TRUE)),

    propofol_dose_per_kg_after_hypertgl = ifelse(is.na(hypertgl_psd_r), NA,
    sum(c_across(starts_with("propofol_dose_"))[hypertgl_psd_r+1:11], na.rm = TRUE) / weight)
) %>% ungroup()


# Get number of patients who developed hypertriglyceridemia before propofol start time
f_data <- f_data %>% mutate(
  hypertgl_before_propofol =
    ifelse(is.na(hypertgl_sttime), NA, difftime(hypertgl_sttime, propofol_sttime, units = "hours") < 0)
)

f_data <- f_data %>% mutate(
  time_first_to_worst_tgl =
    ifelse(is.na(first_triglyceride_time) | is.na(worst_tgl_time) |
             difftime(worst_tgl_time, first_triglyceride_time, units = "hours") == 0, NA,
      difftime(worst_tgl_time, first_triglyceride_time, units = "hours") / 24)
)

f_data <- f_data %>% mutate(
  first_tgl_date_as_PSD_r = round(difftime(first_triglyceride_time, propofol_sttime, units = "hours") / 24),
  worst_tgl_date_as_PSD_r = round(difftime(worst_tgl_time, propofol_sttime, units="hours") / 24),

  first_tgl_date_as_PSD_r = ifelse(first_tgl_date_as_PSD_r < 0, 0,
                                 ifelse(first_tgl_date_as_PSD_r > 10, NA, first_tgl_date_as_PSD_r)),
  worst_tgl_date_as_PSD_r = ifelse(worst_tgl_date_as_PSD_r < 0, 0,
                                 ifelse(worst_tgl_date_as_PSD_r > 10, NA, worst_tgl_date_as_PSD_r))
)

f_data <- f_data %>% rowwise() %>% mutate(
  propofol_dose_from_first_to_worst_tgl =
    ifelse(is.na(first_tgl_date_as_PSD_r) | is.na(worst_tgl_date_as_PSD_r) | is.na(time_first_to_worst_tgl), NA,
           sum(c_across(starts_with("propofol_dose_"))[first_tgl_date_as_PSD_r:worst_tgl_date_as_PSD_r], na.rm = TRUE)), 
  
  propofol_dose_per_kg_from_first_to_worst_tgl =
    ifelse(is.na(first_tgl_date_as_PSD_r) | is.na(worst_tgl_date_as_PSD_r) | is.na(time_first_to_worst_tgl), NA,
           sum(c_across(starts_with("propofol_dose_"))[first_tgl_date_as_PSD_r:worst_tgl_date_as_PSD_r], na.rm = TRUE) / weight)
) %>% ungroup()

# Basic data transformations & cleaning
f_data$race <- f_data$race %remap_by% read.csv("./data/def_race.csv")    # Remap race values to 1997 OMB classifications

f_data$cum_propofol_time <- difftime(f_data$propofol_endtime, f_data$propofol_sttime, units = "hours") / 24

f_data <- f_data %>% rowwise() %>% mutate(emulsion_after_propofol =   # Check if emulsions are started after propofol
  fat_sttime %is_between% list(propofol_sttime, propofol_endtime) |   # Please fucking kill me, this is disgusting
  fat_egg_sttime %is_between% list(propofol_sttime, propofol_endtime) |
  fat_fish_sttime %is_between% list(propofol_sttime, propofol_endtime) |
  fatty_acid_sttime %is_between% list(propofol_sttime, propofol_endtime) |
  kabiven_sttime %is_between% list(propofol_sttime, propofol_endtime) |
  kabiven_fat_sttime %is_between% list(propofol_sttime, propofol_endtime)
) %>% ungroup()

f_data <- f_data %>% mutate(
  hfnc_days =
    ifelse(is.na(hfnc_endtime) | is.na(hfnc_sttime), NA, difftime(hfnc_endtime, hfnc_sttime, units = "hours") / 24)
)

# Implement correction for NIMV data entry error
f_data[f_data$nimv==0, ]$nimv_days <- NA

# Rename the cohort groups
f_data$hypertgl <- as.numeric(f_data$hypertgl)
f_data[is.na(f_data$triglycerides),]$hypertgl <- -1
# hypertgl = 0 --> No hypertgl in ICU
# hypertgl = 1 --> hypertgl in ICU
# hypertgl = -1 --> tgl measurements not available in ICU

f_data <- subset(f_data, hypertgl != -1)

f_data <- merge (f_data_pancreatitis, f_data, by = "id", all.y = TRUE, all.x = FALSE)


f_data <- f_data %>% mutate(acute_pancreatitis_before_icu = acute_pancreatitis_icu == 1,
                            acute_pancreatitis_during_icu = acute_pancreatitis_icu == 2)

f_data$pancreatitis <- 
  ifelse(f_data$acute_pancreatitis_icu == 0 | is.na(f_data$acute_pancreatitis_icu), 0,
  ifelse(f_data$pancreatitis_ass_propofol == 1, 1, -1))

f_data$pancreatitis_ass_propofol <- 
  ifelse(f_data$pancreatitis_ass_propofol == 0, "No association",
  ifelse(f_data$pancreatitis_ass_propofol == 1, "Possible Association", "Likely association"))

# Add a column for the cumulative propofol dose per kg
f_data <- f_data %>% mutate(
  cum_propofol_dose_per_kg = cum_propofol_dose / weight)

# Remove those where hypertgl_before_propofol is true
f_data <- subset(f_data, hypertgl_before_propofol != TRUE | is.na(hypertgl_before_propofol))

# Combine both diabetes columns
f_data <- f_data %>% mutate("dm_combined" = dm_no_complications | dm_complications)

## HBA1c addition
f_data_hba1c <- f_data_hba1c %>% filter(!is.na(hba1c))  # Filter out rows where hba1c is NA

# Round hospital admission time and hba1c collection time for date comparisons
f_data$hosp_admit_time_rounded <- as.Date(f_data$hosp_admit_time)
f_data_hba1c$hba1c_collection_date_rounded <- as.Date(f_data_hba1c$hba1c_collection_date)

# Create temporary dataframe with just the id and hosp admit time
temp_f_data_id_hosptime <- subset(f_data, select=c("id","hosp_admit_time_rounded"))

# Merge hosp admit time to f_data_hba1c for time eligibility checking
f_data_hba1c_with_admit_time <- f_data_hba1c %>% left_join(temp_f_data_id_hosptime, by = "id")

# Calculate time difference between HbA1c collection date and admit date per row
f_data_hba1c_with_admit_time <- f_data_hba1c_with_admit_time %>% mutate(
  collect_to_hosp_interval = abs(hosp_admit_time_rounded - hba1c_collection_date_rounded)
)

# Only keep rows with collect_to_hosp_interval < 31 days
f_data_hba1c_with_admit_time <- f_data_hba1c_with_admit_time %>% filter(collect_to_hosp_interval < 31)

f_data_hba1c_with_admit_time <- f_data_hba1c_with_admit_time %>%
  group_by(id) %>%
  slice_min(order_by = collect_to_hosp_interval, n = 1, with_ties = FALSE) %>%
  ungroup()

f_data_hba1c_with_admit_time <- subset(f_data_hba1c_with_admit_time, select=c("id","hba1c"))

f_data <- merge (f_data_hba1c_with_admit_time, f_data, by = "id", all.y = TRUE, all.x = FALSE)

# Find time to paralytic use after hypertgl
f_data <- f_data %>% rowwise() %>% mutate(
  paralytic_after_hypertgl = paralytic_sttime %time_is_equals_after% hypertgl_sttime,
  paralytic_nday_after_hypertgl = get_time_after_if_equal_after(paralytic_sttime, hypertgl_sttime, "days"),
  paralytic_nhour_after_hypertgl = get_time_after_if_equal_after(paralytic_sttime, hypertgl_sttime, "hours")
)

# Find time to hypertgl after CRRT
f_data <- f_data %>% rowwise() %>% mutate(
  hypertgl_after_crrt = hypertgl_sttime %time_is_equals_after% crrt_sttime,
  hypertgl_nday_after_crrt = get_time_after_if_equal_after(hypertgl_sttime, crrt_sttime, "days"),
  hypertgl_nhour_after_crrt = get_time_after_if_equal_after(hypertgl_sttime, crrt_sttime, "hours")
)

f_data <- f_data %>% rowwise() %>% mutate(
  hmorphone_after_propofol_end =  hmorphone_sttime %time_is_equals_after% propofol_endtime,
  fentanyl_after_propofol_end = fentanyl_sttime %time_is_equals_after% propofol_endtime,

  hmorphone_nday_after_propofol_end = get_time_after_if_equal_after(hmorphone_sttime, propofol_endtime, "days"),
  hmorphone_nhour_after_propofol_end = get_time_after_if_equal_after(hmorphone_sttime, propofol_endtime, "hours"),

  fentanyl_nday_after_propofol_end = get_time_after_if_equal_after(fentanyl_sttime, propofol_endtime, "days"),
  fentanyl_nhour_after_propofol_end = get_time_after_if_equal_after(fentanyl_sttime, propofol_endtime, "hours"),

  midazolam_after_propofol_end =  midazolam_sttime %time_is_equals_after% propofol_endtime,
  lorazepam_after_propofol_end = lorazepam_sttime %time_is_equals_after% propofol_endtime,

  midazolam_nday_after_propofol_end = get_time_after_if_equal_after(midazolam_sttime, propofol_endtime, "days"),
  midazolam_nhour_after_propofol_end = get_time_after_if_equal_after(midazolam_sttime, propofol_endtime, "hours"),

  lorazepam_nday_after_propofol_end = get_time_after_if_equal_after(lorazepam_sttime, propofol_endtime, "days"),
  lorazepam_nhour_after_propofol_end = get_time_after_if_equal_after(lorazepam_sttime, propofol_endtime, "hours"),
)

f_data <- f_data %>% rowwise() %>% mutate(
  opioid_after_propofol_end =  hmorphone_after_propofol_end | fentanyl_after_propofol_end,
  opioid_nday_after_propofol_end = ifelse(is.na(hmorphone_nday_after_propofol_end) & !is.na(fentanyl_nday_after_propofol_end),
                                          fentanyl_nday_after_propofol_end,
                                          ifelse(is.na(fentanyl_nday_after_propofol_end) & !is.na(hmorphone_nday_after_propofol_end),
                                                 hmorphone_nday_after_propofol_end,
                                                 ifelse(!is.na(hmorphone_nday_after_propofol_end) & !is.na(fentanyl_nday_after_propofol_end),
                                                        ifelse(hmorphone_nday_after_propofol_end < fentanyl_nday_after_propofol_end,
                                                               hmorphone_nday_after_propofol_end, fentanyl_nday_after_propofol_end),
                                                        NA))),
  opioid_nhour_after_propofol_end = ifelse(is.na(hmorphone_nhour_after_propofol_end) & !is.na(fentanyl_nhour_after_propofol_end),
                                           fentanyl_nhour_after_propofol_end,
                                           ifelse(is.na(fentanyl_nhour_after_propofol_end) & !is.na(hmorphone_nhour_after_propofol_end),
                                                  hmorphone_nhour_after_propofol_end,
                                                  ifelse(!is.na(hmorphone_nhour_after_propofol_end) & !is.na(fentanyl_nhour_after_propofol_end),
                                                         ifelse(hmorphone_nhour_after_propofol_end < fentanyl_nhour_after_propofol_end,
                                                                hmorphone_nhour_after_propofol_end, fentanyl_nhour_after_propofol_end),
                                                         NA))),

  benzo_after_propofol_end =  midazolam_after_propofol_end | lorazepam_after_propofol_end,
  benzo_nday_after_propofol_end = ifelse(is.na(midazolam_nday_after_propofol_end) & !is.na(lorazepam_nday_after_propofol_end),
                                         lorazepam_nday_after_propofol_end,
                                         ifelse(is.na(lorazepam_nday_after_propofol_end) & !is.na(midazolam_nday_after_propofol_end),
                                                midazolam_nday_after_propofol_end,
                                                ifelse(!is.na(midazolam_nday_after_propofol_end) & !is.na(lorazepam_nday_after_propofol_end),
                                                       ifelse(midazolam_nday_after_propofol_end < lorazepam_nday_after_propofol_end,
                                                              midazolam_nday_after_propofol_end, lorazepam_nday_after_propofol_end),
                                                       NA))),
  benzo_nhour_after_propofol_end = ifelse(is.na(midazolam_nhour_after_propofol_end) & !is.na(lorazepam_nhour_after_propofol_end),
                                          lorazepam_nhour_after_propofol_end,
                                          ifelse(is.na(lorazepam_nhour_after_propofol_end) & !is.na(midazolam_nhour_after_propofol_end),
                                                 midazolam_nhour_after_propofol_end,
                                                 ifelse(!is.na(midazolam_nhour_after_propofol_end) & !is.na(lorazepam_nhour_after_propofol_end),
                                                        ifelse(midazolam_nhour_after_propofol_end < lorazepam_nhour_after_propofol_end,
                                                               midazolam_nhour_after_propofol_end, lorazepam_nhour_after_propofol_end),
                                                        NA))),
)

# Clean up variables
remove(f_data_hba1c_with_admit_time, temp_f_data_id_hosptime)
remove(r_data, r_data_hba1c, r_data_pancreatitis)
remove(f_data_hba1c, f_data_pancreatitis)

