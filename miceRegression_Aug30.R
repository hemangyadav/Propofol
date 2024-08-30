if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, mice)

setwd(here())

data <- read.csv("./panc mice 27aug2024/panc_mice_only_data.csv")
source("./panc mice 27aug2024/mice_tools.R")

# Save a copy of the ClinicNumber column for later consolidation
# Then remove the ClinicNumber from the dataframe (do not want to impute using ClinicNumber)
MRN_column <- data[["id"]]
data[["id"]] <- NULL

ohe_data <- one_hot_encode_auto(data)
ohe_safe_data <- safe_column_names(ohe_data$encoded_df)
impute_type <- determine_column_types(ohe_safe_data$df_safe)

mice_data <- ohe_safe_data$df_safe

for(i in seq_len(nrow(impute_type))) {
  feature_name <- impute_type$FEATURE[i]
  feature_type <- impute_type$TYPE[i]

  if(feature_name %in% names(mice_data)) {
    if(feature_type == "continuous") {
      mice_data[[feature_name]] <- as.numeric(mice_data[[feature_name]])
    } else if(feature_type == "binary") {
      # Assuming binary columns are 0 and 1, convert to FALSE and TRUE
      mice_data[[feature_name]] <- as.logical(as.numeric(mice_data[[feature_name]]))
    } else if(feature_type == "ordinal") {
      # Convert to factor; further detail might be needed to specify order
      mice_data[[feature_name]] <- as.factor(mice_data[[feature_name]])
    }
  }
}

rm(feature_name, feature_type, i)

print_missing_summary(mice_data)
max_missing_percentage(mice_data)

imputed_data <- mice(mice_data[impute_type[["FEATURE"]]],
                     m=max_missing_percentage(mice_data),
                     impute_type[["IMPUTE"]],
                     maxit=5)

# Get the first imputed dataset; alternatively, an average can be used
complete_data <- complete(imputed_data, action=1)
print_missing_summary(complete_data)

complete_data_col_decoded <- decode_column_names(complete_data, ohe_safe_data$lookup_table)
complete_data_ohe_decoded <- decode_one_hot(complete_data_col_decoded, ohe_data$encoding_info)
print_missing_summary(complete_data_ohe_decoded)
max_missing_percentage(complete_data_ohe_decoded)

complete_data_ohe_decoded$ClinicNumber <- MRN_column

write.csv(complete_data_ohe_decoded, "./panc mice 27aug2024/panc_mice_imputed_data.csv")
