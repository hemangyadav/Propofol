#This is the main propofol code
#Last edited May 17, 2024

#This code was developed jointly by Kiyan Heybati (heybati.kiyan@mayo.edu), Jiawen Deng (jiawen.deng@mail.utoronto.ca), and Fangwen Zhou (fwzhresearch@gmail.com) with guidance from Dr. Hemang Yadav (yadav.hemang@mayo.edu)

library("dplyr")

# The function take in two posix and returns a logical
# If either posix is NA, return False
# If the first posix is at the same time or after the second posix, return True
# If the first posix is before the second posix, also return False
'%time_is_equals_after%' <- function(posix1, posix2) {
  return(ifelse(is.na(posix1) | is.na(posix2), FALSE, posix1 >= posix2))
}

# The function take in two posix and returns a logical
# If either posix is NA, return NA
# If the first posix is at the same time or after the second posix, return True
# If the first posix is before the second posix, also return False
'%time_is_equals_after_w_na%' <- function(posix1, posix2) {
  return(ifelse(is.na(posix1) | is.na(posix2), NA, posix1 >= posix2))
}

# Selects columns defined in def_columns.csv or def_comparison_export.csv, etc.
# These sheets have 3 columns: 1) original column name, 2) new column name, and 3) column type
# The function takes in a dataframe and a csv sheet and returns a dataframe
# The function 1) removes all columns in the original dataframe not in the csv
# 2) renames all defined columns in the original dataframe from the original column name to the new column name in the csv
# 3) cast all defined columns in the original dataframe to the column type specified in the csv, if available

'%filter_by%' <- function(data, col_defs) {
  cleaned_col_defs <- col_defs[!grepl("^#", col_defs[[1]]), ]   # Remove comment rows with "#" prefix

  cleaned_col_defs[cleaned_col_defs == "" | cleaned_col_defs == " "] <- NA
  #cleaned_col_defs %>% remove_empty("rows")

  selected_cols <- cleaned_col_defs[["col_name"]]       # Pull a vector of selected column names from the csv file
  col_types <- cleaned_col_defs[["col_type"]]           # Pull a vector of column types from the csv file
  new_col_names <- cleaned_col_defs[["col_name_new"]]   # Pull a vector of new column names from the csv file

  indices <- match(selected_cols, names(data))  # Replace the corresponding old column names
  names(data)[indices] <- new_col_names

  for (i in seq_along(new_col_names)) {         # Iterate through column names
    col_name <- new_col_names[i]                # and cast column value to correct type
    if (is.na(col_types[i])) next

    if (col_types[i] == "logical") {            # based on col_types
      data[[col_name]] <- as.logical(as.integer(data[[col_name]]))
    } else if (col_types[i] == "numeric") {
      data[[col_name]] <- as.numeric(data[[col_name]])
    } else if (col_types[i] == "posix") {
        for (row in data[[col_name]]) {
          if (is.na(row) | row == "")
            next
          if (!is.na(as.POSIXct(row, format="%m-%d-%Y %I:%M:%S %p",tz="GMT"))){
            print (paste(col_name, "'s date format uses dash"))
            data[[col_name]] <- as.POSIXct(data[[col_name]],format="%m-%d-%Y %I:%M:%S %p",tz="GMT", optional = TRUE)
            break
          }
          else if (!is.na(as.POSIXct(row, format="%m/%d/%Y %I:%M:%S %p",tz="GMT"))){
            print (paste(col_name, "'s date format uses slash"))
            data[[col_name]] <- as.POSIXct(data[[col_name]],format="%m/%d/%Y %I:%M:%S %p",tz="GMT", optional = TRUE)
            break
          }
            # Added additional date formatting handler to deal with HbA1c data
          else if (!is.na(as.POSIXct(row, format="%Y-%m-%d %H:%M", tz="GMT"))){
            print (paste(col_name, " is using weird POSIX formatting. Is this for HbA1c data?"))
            data[[col_name]] <- as.POSIXct(data[[col_name]],format="%Y-%m-%d %H:%M", tz="GMT", optional = TRUE)
            break
          }
          else
            data[[col_name]] <- NA
            #stop("Date format does not match")
        }
    }
  }

  data <- data %>%
    mutate(across(where(is.character), ~na_if(.x, ""))) %>%
    mutate(across(where(is.character), ~na_if(.x, " ")))

  return(select(data,all_of(new_col_names)))    # Apply column selection filter and return df

}

# Remaps value for a given column into new values
# As defined by remap_defs
'%remap_by%' <- function(data, remap_defs) {
  cleaned_remap_defs <- remap_defs[!grepl("^#", remap_defs[[1]]), ]   # Remove comment rows with "#" prefix

  cleaned_remap_defs[cleaned_remap_defs == "" | cleaned_remap_defs == " "] <- NA
  cleaned_remap_defs <- na.omit(cleaned_remap_defs)

  indices <- match(data, cleaned_remap_defs$original)
  data[!is.na(indices)] <- cleaned_remap_defs$remapped[indices[!is.na(indices)]]

  return(data)

}

# Checks if a given posix is in-between two other posix(s) [inclusive]
# Returns FALSE if posix1 is NA
# Takes in a posix and a vector (list) of two posix, and returns a logical
'%is_between%' <- function(posix1, posix_vec) {
    if (is.na(posix1)) {
        return(FALSE)
    }
    
    if (!is.vector(posix_vec) || length(posix_vec) != 2) {
        stop("The POSIX vector must be a vector with exactly two POSIXct items.")
    }
    
    posix2 <- posix_vec[[1]]
    posix3 <- posix_vec[[2]]
    
    if (is.na(posix2) || is.na(posix3)) {
        return(FALSE)
    }
    
    posix_vec <- sort(unlist(posix_vec))
    
    posix2 <- posix_vec[1]
    posix3 <- posix_vec[2]
    
    result <- posix1 >= posix2 & posix1 <= posix3
    return(result)
    
}

'%is_between_vectorized%' <- function(posix_vec, posix_between) {
    # if (is.na(posix_vec)) {
    #     stop ('posix vector is null.')
    #   return(FALSE)
    # }
    
    if (!is.list(posix_between) || length(posix_between) != 2) {
        stop("The POSIX must be a list with exactly two POSIXct items.")
    }
    
    # if (any(is.na(posix_between[[1]])) || any(is.na(posix_between[[2]]))) {
    #     stop ('posix_between is null.')
    #   return(FALSE)
    # }
    
    
    start_time <- posix_between[[1]]
    end_time <- posix_between[[2]]
    
    result <- ifelse (is.na(posix_vec) | is.na(start_time) | is.na(end_time), FALSE, ifelse (
        posix_vec >= start_time & posix_vec <= end_time, TRUE, FALSE))
    return(result)
}

# Finds the earliest posix within a given vector
# Returns NA if the posix vector is empty

# Takes in a vector of posix and returns a posix
find_earliest_posix <- function(posix_vec) {
  posix_vec_no_na <- na.omit(unlist(posix_vec))

  if (length(posix_vec_no_na) == 0) {
    return(NA)
  }

  earliest_time <- min(posix_vec_no_na)

  return(earliest_time)

}

find_latest_posix <- function(posix_vec) {
    posix_vec_no_na <- na.omit(unlist(posix_vec))
    
    if (length(posix_vec_no_na) == 0) {
        return(NA)
    }
    
    latest_time <- max(posix_vec_no_na)
    
    return(latest_time)
    
}

find_time <- function (posix_vec1, posix_vec2, time) {
    if (length(posix_vec1) != length(posix_vec2)) {
        stop ("Vector size is not the same.")
    }
    
    if (!time %in% c('earlier', 'later')) {
        stop ("Third parameter must be either 'earlier' or 'later'")
    }
    
    if (time == 'earlier')
        output <- ifelse(is.na(posix_vec1), posix_vec2, ifelse (
            is.na(posix_vec2), posix_vec1, ifelse (
                posix_vec1 <= posix_vec2, posix_vec1, posix_vec2)))
    else
        output <- ifelse(is.na(posix_vec1), posix_vec2, ifelse (
            is.na(posix_vec2), posix_vec1, ifelse (
                posix_vec1 <= posix_vec2, posix_vec1, posix_vec2)))
    
    return (as.POSIXct(output, tz="GMT"))
}

# Custom wrapper function for tableby that automatically includes all tables
# except for column names in the "exclude" vector.
create_tbl <- function(data, group_by, exclude=NULL, numeric.test="anova", cat.test="chisq",
                       numeric.stats=c("Nmiss","meansd"), ...) {
  predictors <- colnames(data)
  predictors <- predictors[predictors != group_by]

  for (i in exclude) {
    predictors <- predictors[predictors != i]
  }

  formula_str <- paste(group_by, " ~ ", paste(predictors, collapse = " + "))
  
  return(tableby(as.formula(formula_str),
          data = data, test = TRUE,
          numeric.test=numeric.test, cat.test=cat.test,
          numeric.stats=numeric.stats, ...))
}

# Custom wrapper function for tableby that automatically includes all tables
# except for column names in the "exclude" vector.
prop_match <- function(data, group_by, method = "nearest",
                       replace = FALSE, ratio = 2, match_by, ...) {

  data$MRN <- data$id

  data$id <- seq_len(nrow(data))
  # row.names(data) <- data$id

  # What the hell is going on here
  data <- data %>% drop_na(!!!rlang::syms(match_by))
  
  formula_str <- paste(group_by, " ~ ", paste(match_by, collapse = " + "))
  print(formula_str)

  matchit_model <- matchit(as.formula(formula_str),
                           data = data, replace = replace, ratio = ratio, method = method, ...)

  matched_data <- match.data(matchit_model)

  # matched_pair_data <- matchit_model$match.matrix

  # # IMPORTANT STEPS FOR GETTING THE MATCH PAIR INFORMATION OUT OF THIS MODEL
  # # STEP 1: CONVERT MATCH_PAIR_DATA FROM MATRIX TO DF
  # matched_pair_data_df <- as.data.frame(matched_pair_data)
  # colnames(matched_pair_data_df) <- c("ctl1", "ctl2")
  # matched_pair_data_df$case_id <- rownames(matched_pair_data_df)
  # matched_pair_data_df <- matched_pair_data_df[c("case_id", "ctl1", "ctl2")]
  # rownames(matched_pair_data_df) <- NULL
  #
  # # STEP 2: INITIATE STRATA VARIABLE FOR CLOGIT
  # matched_data$strata <- NA
  #
  # # STEP 3: ITERATE THROUGH MATCHED_PAIR_DATA_DF
  # for(i in seq_len(nrow(matched_pair_data_df))) {
  #
  #   # Extract the ids for the current stratum
  #   current_ids <- unlist(matched_pair_data_df[i, c("case_id", "ctl1", "ctl2")])
  #
  #   # Identify and update rows in `data`
  #   matched_data$strata[matched_data$id %in% current_ids] <- i
  # }

  return(matched_data)
}

# Takes in two posix and returns an integer
# If posix 1 is equal to or later than posix 2, return the number of days
# If posix 1 is before posix 2, return NA
# get_n_days_after_if_equal_after <- function(posix1, posix2) {
#   # Directly return NA if any is NA, avoiding further computation
#   if (is.na(posix1) || is.na(posix2)) 
#     return(NA)

#   if (posix1 < posix2) 
#     return (NA)

#   # Convert to Date once and store, to avoid converting twice
#   date1 <- as.Date(posix1)
#   date2 <- as.Date(posix2)
  
#   # Use a single if statement to check the condition and return the result
#   if (date1 >= date2) {
#     return(date1 - date2)
#   }
  
#   return(NA)
# }

get_n_days_after_if_equal_after <- function(end_posix, start_posix) {
  # Directly return NA if any is NA, avoiding further computation
  if (is.na(end_posix) || is.na(start_posix)) 
    return(NA)

  if (end_posix < start_posix) 
    return (NA)

  return (as.numeric(difftime(end_posix, start_posix, units = "days")))
}


# Takes in two posix and returns an integer
get_time_after_if_equal_after <- function(end_posix, start_posix, unit) {
  # Directly return NA if any is NA, avoiding further computation
  if (is.na(end_posix) || is.na(start_posix)) 
    return(NA)
  
  if (end_posix < start_posix) 
    return (NA)

  if (!unit %in% c("auto", "secs", "mins", "hours",
                   "days", "weeks"))
                   stop("Invalid Unit")

  return (as.numeric(difftime(end_posix, start_posix, units = unit)))
}

get_var_type <- function(x) {

  if (is.null(x) || length(unique(x)) == 0){
    return (NA)
  }

  if ((is.numeric(x) || is.double(x)) && length(unique(x)) > 2) {
    return("numeric")
  } else if (length(unique(x)) == 2) {
    return("binary")

  } else if (!is.null(unique(x)[1]) && !is.na(unique(x)[1]) && is.logical(unique(x)[1])) {
    return("binary")
  } else if (!is.null(unique(x)[2]) && !is.na(unique(x)[2]) && is.logical(unique(x)[2])) {
    return ("binary")
  } else {
    return("categorical")
  }
}

get_smd <- function(data, var, group_var) {
  col_data <- data[[var]]
  var_type <- get_var_type(col_data)

 
  if (is.na(var_type)){
    return(data.frame(
    Variable = var,
    SMD = NA,
    CI_Lower = NA,
    CI_Upper = NA
    ))
  }

  if (!is.na(var_type)) {
    if (var_type == "numeric") {
      std_out <- stddiff.numeric(data = data,
                                 gcol = which(names(data) == group_var),
                                 vcol = which(names(data) == var))
    } else if (var_type == "binary") {
      std_out <- stddiff.binary(data = data,
                                gcol = which(names(data) == group_var),
                                vcol = which(names(data) == var))
    } else if (var_type == "categorical") {
      std_out <- stddiff.category(data = data,
                                  gcol = which(names(data) == group_var),
                                  vcol = which(names(data) == var))
    }
  }

  new_row <- data.frame(
    Variable = var,
    SMD = std_out[1, "stddiff"],
    CI_Lower = std_out[1, "stddiff.l"],
    CI_Upper = std_out[1, "stddiff.u"]
  )

  return(new_row)
}
