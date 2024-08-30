if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, mice)

#' Automatically One-Hot Encode Columns
#'
#' This function automatically identifies categorical columns in a dataframe
#' and one-hot encodes them. Columns are considered categorical if they cannot
#' be cast to numeric (ignoring NA values). Columns with numeric values and NA
#' are treated as continuous.
#'
#' @param df A dataframe containing the data to be encoded.
#' @return A list with two elements: `encoded_df`, the dataframe with one-hot encoded columns,
#'         and `encoding_info`, a list storing the encoding information for each encoded column.
#' @note NA values in categorical columns will be preserved and carried over to the one-hot encoded columns.
one_hot_encode_auto <- function(df) {
  encoding_info <- list()

  # Identify columns that need encoding
  cols_to_encode <- sapply(df, function(col) {
    # Ignore NA values and check if the non-NA values can be cast to numeric
    non_na_col <- col[!is.na(col)]
    can_be_numeric <- all(!is.na(suppressWarnings(as.numeric(as.character(non_na_col)))))

    !can_be_numeric
  })

  # Perform one-hot encoding
  for (col in names(df)[cols_to_encode]) {
    unique_values <- sort(unique(df[[col]]), na.last = TRUE)
    unique_values <- unique_values[!is.na(unique_values)]  # Remove NA from unique values
    encoding_info[[col]] <- unique_values

    for (val in unique_values) {
      new_col_name <- paste(col, val, sep = "_")
      df[[new_col_name]] <- ifelse(df[[col]] == val, 1, ifelse(is.na(df[[col]]), NA, 0))
    }

    df[[col]] <- NULL
  }

  return(list(encoded_df = df, encoding_info = encoding_info))
}

#' Decode One-Hot Encoded Columns
#'
#' This function decodes one-hot encoded columns in a dataframe back to their original
#' categorical string encoding using the provided encoding information.
#'
#' @param encoded_df A dataframe containing the one-hot encoded columns.
#' @param encoding_info A list containing the encoding information used to decode the dataframe.
#' @return A dataframe with the one-hot encoded columns replaced by their original categorical values.
decode_one_hot <- function(encoded_df, encoding_info) {
  for (col in names(encoding_info)) {
    unique_values <- encoding_info[[col]]
    encoded_cols <- paste(col, unique_values, sep = "_")

    decoded_col <- apply(encoded_df[, encoded_cols], 1, function(row) {
      if (all(is.na(row))) {
        return(NA)
      } else {
        return(unique_values[which(row == 1)])
      }
    })

    encoded_df[[col]] <- decoded_col
    encoded_df <- encoded_df[, !names(encoded_df) %in% encoded_cols]
  }

  return(encoded_df)
}

#' Determine Column Types for a Dataframe
#'
#' This function inspects each column in a dataframe to classify it as continuous,
#' binary, or ordinal categorical. Binary columns are identified if they only contain
#' the values 0 and 1. Continuous columns are identified if they contain decimal
#' values or if their range exceeds 10. Ordinal categorical columns are those that
#' do not meet the binary or continuous criteria.
#'
#' @param df A dataframe to be analyzed.
#'
#' @return A dataframe with the following columns:
#' \item{FEATURE}{The name of the feature/column in the original dataframe.}
#' \item{TYPE}{The type of the feature: "continuous", "binary", or "ordinal".}
#' \item{IMPUTE}{The recommended MICE imputation method: "pmm" for continuous, "logreg"
#' for binary, and "polr" for ordinal.}
#'
#' @examples
#' df <- data.frame(
#'   ID = 1:5,
#'   Category = c(1, 2, 3, 2, 1),
#'   BinaryFeature = c(0, 1, 0, 1, 1),
#'   Score = c(100.5, 85.2, 93.1, 78.6, 88.4)
#' )
#'
#' feature_types <- determine_column_types(df)
determine_column_types <- function(df) {
  # Initialize a dataframe to store the results
  results <- data.frame(FEATURE = character(),
                        TYPE = character(),
                        IMPUTE = character(),
                        stringsAsFactors = FALSE)

  for (col in names(df)) {
    feature_type <- "ordinal"  # Default to ordinal
    impute_method <- "polr"

    # Exclude NA values from the checks
    non_na_values <- df[[col]][!is.na(df[[col]])]

    # Check if the column name ends with '_Charlson'
    if (grepl("_Charlson$", col)) {
      feature_type <- "ordinal"
      impute_method <- "polr"
    } else {
      # Check if binary (0, 1)
      if (all(non_na_values %in% c(0, 1))) {
        feature_type <- "binary"
        impute_method <- "logreg"
      } else {
        # Check if continuous (contains decimals or range > 10)
        if (any(non_na_values %% 1 != 0) || (max(non_na_values) - min(non_na_values)) > 10) {
          feature_type <- "continuous"
          impute_method <- "pmm"
        }
      }
    }

    # Add the result to the dataframe
    results <- rbind(results, data.frame(FEATURE = col,
                                         TYPE = feature_type,
                                         IMPUTE = impute_method,
                                         stringsAsFactors = FALSE))
  }

  return(results)
}

#' Calculate Maximum Percentage of Missing Values in a Dataframe
#'
#' This function calculates the maximum percentage of missing values
#' across all columns in a dataframe and rounds the result up to the nearest 10.
#'
#' @param df A dataframe containing the data to be analyzed.
#' @return An integer representing the maximum percentage of missing values,
#'         rounded up to the nearest 10.
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   A = c(1, 2, NA, 4, NA),
#'   B = c(NA, NA, 3, 4, 5),
#'   C = c(1, NA, 3, NA, 5)
#' )
#'
#' # Get the maximum percentage of missing values, rounded up to nearest 10
#' max_missing_value <- max_missing_percentage(df)
#' print(max_missing_value)  # Outputs 60
max_missing_percentage <- function(df) {
  # Calculate the percentage of missing values for each column
  missing_percentages <- sapply(df, function(col) {
    mean(is.na(col)) * 100
  })

  # Find the maximum percentage
  max_missing <- max(missing_percentages)

  # Round up to the nearest 10
  rounded_max_missing <- ceiling(max_missing / 10) * 10

  return(rounded_max_missing)
}

#' Print Summary of Missing Values in a Dataframe
#'
#' This function calculates the percentage of missing values and the number
#' of rows with missing values for each column in a dataframe. It prints a summary
#' of these statistics.
#'
#' @param df A dataframe containing the data to be analyzed.
#' @return This function does not return a value; it prints a summary to the console.
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   A = c(1, 2, NA, 4, NA),
#'   B = c(NA, NA, 3, 4, 5),
#'   C = c(1, NA, 3, NA, 5)
#' )
#'
#' # Print a summary of missing values
#' print_missing_summary(df)
print_missing_summary <- function(df) {
  # Total number of rows in the dataframe
  total_rows <- nrow(df)

  # Print the header
  cat("Missing Values Summary:\n")
  cat(sprintf("%-15s %-20s %-20s\n", "Column", "Missing (%)", "Missing (count)"))
  cat(rep("-", 55), "\n", sep = "")

  # Calculate missing values summary and print for each column
  for (col in names(df)) {
    missing_count <- sum(is.na(df[[col]]))
    missing_percentage <- (missing_count / total_rows) * 100

    cat(sprintf("%-15s %-20.2f %-20d\n", col, missing_percentage, missing_count))
  }
}

#' Convert DataFrame Column Names to Safe Names
#'
#' This function converts the column names of a dataframe to safe names
#' by replacing unsafe characters with underscores. It returns a list
#' containing the transformed dataframe and a lookup table for decoding.
#'
#' @param df A dataframe whose column names need to be transformed.
#' @return A list containing the transformed dataframe (`df_safe`) and
#' a lookup table (`lookup_table`) for decoding the column names.
safe_column_names <- function(df) {

  # Original column names
  original_names <- colnames(df)

  # Safe column names: replace non-alphanumeric characters with underscores
  safe_names <- make.names(original_names, unique = TRUE)

  # Apply safe names to the dataframe
  colnames(df) <- safe_names

  # Create a lookup table for decoding
  lookup_table <- data.frame(
    original = original_names,
    safe = safe_names,
    stringsAsFactors = FALSE
  )

  # Return the modified dataframe and the lookup table
  list(df_safe = df, lookup_table = lookup_table)
}

#' Revert Safe Column Names to Original Names
#'
#' This function reverts the safe column names of a dataframe back
#' to their original names using a provided lookup table.
#'
#' @param df A dataframe with safe column names.
#' @param lookup_table A dataframe containing the original and safe column names.
#' @return A dataframe with the original column names restored.
decode_column_names <- function(df, lookup_table) {

  # Create a named vector for decoding
  decode_vector <- setNames(lookup_table$original, lookup_table$safe)

  # Apply the original names to the dataframe
  colnames(df) <- decode_vector[colnames(df)]

  # Return the dataframe with original names
  df
}

