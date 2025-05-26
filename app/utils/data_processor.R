# --- Data Processing Utility Functions ---

#' Detect Years from Selected Data Row
#'
#' @param data_df The selected data dataframe.
#' @param selected_row_name The name of the series selected in the UI (from column 1).
#' @return A list containing 'years' (numeric vector), 'cols' (numeric vector of original column indices), and 'log' (string). Returns list with NULL years/cols on failure.
detect_financial_years <- function(data_df, selected_row_name) {
  log_text <- paste0("Year Detection (Row: '", selected_row_name, "'):\n")
  
  if (is.null(data_df) || nrow(data_df) == 0) {
    log_text <- paste0(log_text, "Error: Selected data is empty.\n")
    return(list(years = NULL, cols = NULL, log = log_text))
  }
  
  if (is.null(selected_row_name) || selected_row_name == "") {
    log_text <- paste0(log_text, "Error: No row selected for year detection.\n")
    return(list(years = NULL, cols = NULL, log = log_text))
  }
  
  # Assuming the first column contains the series names
  year_row <- data_df[data_df[[1]] == selected_row_name, , drop = FALSE] # Use drop=FALSE
  
  if (nrow(year_row) == 0) {
    log_text <- paste0(log_text, "Error: Row '", selected_row_name, "' not found in data.\n")
    return(list(years = NULL, cols = NULL, log = log_text))
  }
  
  # Skip first column and known metadata columns
  skip_cols_names <- c(names(data_df)[1], "sheet_name", "import_timestamp", "row_id")
  potential_year_cols_indices <- which(!colnames(data_df) %in% skip_cols_names)
  
  if(length(potential_year_cols_indices) == 0) {
    log_text <- paste0(log_text, "Error: No potential year columns found after excluding metadata.\n")
    return(list(years = NULL, cols = NULL, log = log_text))
  }
  
  # Get values and attempt conversion, handling potential errors
  year_values_raw <- unlist(year_row[1, potential_year_cols_indices])
  year_values_num <- suppressWarnings(as.numeric(as.character(year_values_raw))) # Handle non-numeric gracefully
  
  # Identify valid years (simple check)
  valid_year_indices_in_subset <- which(!is.na(year_values_num) & year_values_num >= 1900 & year_values_num <= 2100)
  
  if (length(valid_year_indices_in_subset) > 0) {
    detected_years <- year_values_num[valid_year_indices_in_subset]
    # Map back to original column indices
    original_cols_indices <- potential_year_cols_indices[valid_year_indices_in_subset]
    
    log_text <- paste0(log_text, "Success: Found ", length(detected_years),
                       " potential years: ", paste(detected_years, collapse=", "), ".\n",
                       "Corresponding original columns: ", paste(original_cols_indices, collapse=", "), "\n")
    
    return(list(years = detected_years, cols = original_cols_indices, log = log_text))
  } else {
    log_text <- paste0(log_text, "Error: No valid years (1900-2100) found in the selected row.\n")
    return(list(years = NULL, cols = NULL, log = log_text))
  }
}


#' Calculate Standardized Financial Data
#'
#' @param selected_data_df Data frame of selected data.
#' @param defined_metrics List of metric definitions (name, type, components).
#' @param year_info List containing 'years' (numeric vector) and 'cols' (numeric vector of col indices).
#' @param replace_na_with_zero Logical, whether to replace NA/blanks with 0.
#' @return A list containing 'data' (the standardized data frame) and 'log' (string).
calculate_standardized_data <- function(selected_data_df, defined_metrics, year_info, replace_na_with_zero) {
  log_text <- "Standardization Calculation Log:\n\n"
  
  if (is.null(selected_data_df) || nrow(selected_data_df) == 0) {
    log_text <- paste0(log_text, "Error: No selected data available.\n")
    return(list(data = NULL, log = log_text))
  }
  if (is.null(defined_metrics) || length(defined_metrics) == 0) {
    log_text <- paste0(log_text, "Error: No metrics have been defined.\n")
    return(list(data = NULL, log = log_text))
  }
  if (is.null(year_info) || is.null(year_info$years) || length(year_info$years) == 0) {
    log_text <- paste0(log_text, "Error: Year information is missing or invalid.\n")
    return(list(data = NULL, log = log_text))
  }
  # Ensure replace_na_with_zero is a logical value
  replace_na_with_zero <- isTRUE(replace_na_with_zero)
  
  detected_years <- year_info$years
  year_cols_indices <- year_info$cols # Use original indices
  
  log_text <- paste0(log_text, "Using ", length(detected_years), " years: ",
                     paste(detected_years, collapse=", "), "\n",
                     "From original columns: ", paste(year_cols_indices, collapse=", "), "\n\n")
  
  # Create the structure for the result dataframe
  result_df <- data.frame(metric = names(defined_metrics), stringsAsFactors = FALSE)
  for (year in detected_years) {
    result_df[[as.character(year)]] <- NA_real_ # Initialize with NA
  }
  rownames(result_df) <- names(defined_metrics) # Use metric names as rownames for easier access
  
  # Process each metric
  for (metric_name in names(defined_metrics)) {
    metric_definition <- defined_metrics[[metric_name]]
    components <- metric_definition$components
    
    log_text <- paste0(log_text, "Processing metric: ", metric_name, "\n")
    
    # Initialize metric values for the current metric row
    current_metric_values <- rep(0, length(detected_years))
    names(current_metric_values) <- as.character(detected_years)
    
    # Process each component
    if (length(components) > 0) {
      for (j in 1:length(components)) {
        comp <- components[[j]]
        series_name <- comp$series
        operation <- comp$operation
        factor_val <- as.numeric(comp$factor) # Ensure factor is numeric
        if(is.na(factor_val)) factor_val <- 1 # Default factor to 1 if invalid
        
        op_symbol <- switch(operation, "add" = "+", "subtract" = "-", "+")
        log_text <- paste0(log_text, "  ", if(j==1) "Starting with " else op_symbol,
                           " series: '", series_name, "'")
        if (factor_val != 1) {
          log_text <- paste0(log_text, " * ", factor_val)
        }
        log_text <- paste0(log_text, "\n")
        
        # Find the row for this series in the selected data (assuming first col has names)
        series_row_data <- selected_data_df[selected_data_df[[1]] == series_name, , drop = FALSE]
        
        if (nrow(series_row_data) > 0) {
          # Extract values for each year using the original column indices
          for (i in 1:length(year_cols_indices)) {
            col_idx <- year_cols_indices[i]
            year <- detected_years[i]
            year_char <- as.character(year)
            
            if (col_idx <= ncol(series_row_data)) {
              raw_value <- series_row_data[1, col_idx]
              
              # Convert to numeric carefully, handling various non-numeric forms
              clean_value <- trimws(as.character(raw_value))
              # Check for actual NA values OR empty strings first
              if (is.na(clean_value) || clean_value == "") {
                numeric_value <- NA_real_
              } else {
                # Only attempt conversion if it's not NA and not an empty string
                numeric_value <- suppressWarnings(as.numeric(clean_value))
                # Note: as.numeric will correctly return NA if clean_value is non-numeric (e.g., "abc")
                # This NA will then be handled by the is.na(numeric_value) check below.
              }
              
              
              # Handle NA/missing based on flag (This part remains the same and is now safer)
              if (is.na(numeric_value)) {
                if (isTRUE(replace_na_with_zero)) {
                  numeric_value <- 0
                  log_text <- paste0(log_text, "    Replaced missing/non-numeric value with 0 for year ", year, ".\n")
                } else {
                  log_text <- paste0(log_text, "    Skipping calculation for year ", year, " due to missing/non-numeric value (Replace NA is FALSE).\n")
                  next # Skip this year if value is NA and not replacing
                }
              }
              
              # Apply operation
              value_to_apply <- numeric_value * factor_val
              if (operation == "add") {
                current_metric_values[year_char] <- current_metric_values[year_char] + value_to_apply
              } else if (operation == "subtract") {
                current_metric_values[year_char] <- current_metric_values[year_char] - value_to_apply
              }
            } else {
              log_text <- paste0(log_text, "    Warning: Column index ", col_idx, " for year ", year, " out of bounds for series '", series_name, "'.\n")
            }
          } # End loop through years/columns
        } else {
          log_text <- paste0(log_text, "  Warning: Series '", series_name, "' not found in selected data.\n")
          # If a component series is missing, the result for this metric might be incomplete/incorrect.
          # Current behavior: continues calculation, effectively treating the missing series as 0.
        }
      } # End loop through components
    } else {
      log_text <- paste0(log_text, "  Warning: No components defined for this metric. Result will be 0.\n")
    }
    
    # Add calculated values to the result dataframe row for this metric
    result_df[metric_name, as.character(detected_years)] <- current_metric_values
    
  } # End loop through metrics
  
  log_text <- paste0(log_text, "\nSuccessfully created standardized dataset with ",
                     nrow(result_df), " metrics across ", length(detected_years), " years.")
  
  # Reset rownames before returning
  rownames(result_df) <- NULL
  return(list(data = result_df, log = log_text))
}


#' Prepare Data for Assumption Plotting
#'
#' @param assumptions_df Assumptions dataframe.
#' @param selected_assumption_name The name of the assumption row to plot.
#' @return A data frame with 'Year' and 'Value' columns for plotting, or NULL.
prepare_assumption_plot_data <- function(assumptions_df, selected_assumption_name) {
  if (is.null(assumptions_df) || nrow(assumptions_df) == 0 || is.null(selected_assumption_name)) {
    return(NULL)
  }
  
  assumption_row_data <- assumptions_df[assumptions_df$Assumption == selected_assumption_name, , drop = FALSE]
  if (nrow(assumption_row_data) == 0) {
    return(NULL)
  }
  
  year_cols <- names(assumption_row_data)[grepl("^\\d{4}$", names(assumption_row_data))]
  if (length(year_cols) == 0) {
    return(NULL)
  }
  
  years <- as.numeric(year_cols)
  values <- as.numeric(assumption_row_data[, year_cols]) # Already decimals
  
  plot_data <- data.frame(Year = years, Value = values)
  return(plot_data)
}


#' Prepare Data for Metrics Plotting
#'
#' @param projections_df Projections dataframe.
#' @param metrics_to_plot Vector of metric names to include.
#' @return A long-format data frame with 'Metric', 'Year', 'Value' columns, or NULL.
prepare_metrics_plot_data <- function(projections_df, metrics_to_plot = NULL) {
  if (is.null(projections_df) || nrow(projections_df) == 0) {
    return(NULL)
  }
  
  year_cols <- names(projections_df)[grepl("^\\d{4}$", names(projections_df))]
  if (length(year_cols) == 0) {
    return(NULL)
  }
  years_num <- as.numeric(year_cols)
  
  # Default metrics if not specified (up to PV of FCFE)
  if (is.null(metrics_to_plot)) {
    pv_index <- which(projections_df$Metric == "Present Value of FCFE")
    if (length(pv_index) > 0) {
      metrics_to_plot <- projections_df$Metric[1:pv_index[1]]
    } else {
      metrics_to_plot <- projections_df$Metric # Plot all if PV not found
    }
  }
  
  # Filter projections to relevant metrics and years
  plot_subset <- projections_df[projections_df$Metric %in% metrics_to_plot, c("Metric", year_cols)]
  
  # Convert to long format for plotting (e.g., with tidyr)
  # Ensure tidyr is loaded (best practice: load in global.R)
  plot_data_long <- tidyr::pivot_longer(plot_subset,
                                        cols = all_of(year_cols),
                                        names_to = "Year",
                                        values_to = "Value")
  
  # Convert Year column to numeric and remove NAs
  plot_data_long$Year <- as.numeric(plot_data_long$Year)
  plot_data_long <- na.omit(plot_data_long) # Remove rows where Value is NA
  
  # Ensure Metric is a factor for consistent plotting order
  plot_data_long$Metric <- factor(plot_data_long$Metric, levels = metrics_to_plot)
  
  
  return(plot_data_long)
}


#' Parse Imported Assumptions File Content
#'
#' @param file_content Character vector, lines read from the assumptions CSV file.
#' @return A list containing 'assumptions_df' and 'params_list', or list(error = "message") on failure.
parse_imported_assumptions_file <- function(file_content) {
  # Ensure the last line is complete by adding a newline if necessary
  if (length(file_content) > 0 && nchar(file_content[length(file_content)]) > 0) {
    file_content <- c(file_content, "")
  }
  
  separator_idx <- grep("--- DCF PARAMETERS BELOW ---", file_content)
  
  if (length(separator_idx) == 0 || separator_idx == 1) {
    return(list(error = "Invalid file format: Missing or misplaced DCF parameters separator line."))
  }
  
  # Split the content
  assumptions_content <- file_content[1:(separator_idx - 1)]
  # Skip separator line and potential blank line after it
  params_start_line <- separator_idx + 1
  # Skip header row for parameters
  if(params_start_line + 1 <= length(file_content)) {
    params_content <- file_content[(params_start_line + 1) : length(file_content)]
    params_header <- file_content[params_start_line] # Capture header
  } else {
    return(list(error = "Invalid file format: Missing parameter data after separator."))
  }
  
  
  # --- Parse Assumptions ---
  assumptions_df <- tryCatch({
    read.csv(text = paste(assumptions_content, collapse = "\n"),
             stringsAsFactors = FALSE,
             check.names = FALSE,
             na.strings = c("NA", "", "NULL")) # Handle various NA representations
  }, error = function(e) { NULL })
  
  if (is.null(assumptions_df)) {
    return(list(error = paste("Error parsing assumptions table:", e$message)))
  }
  
  # Clean and validate assumptions
  if (!"Assumption" %in% colnames(assumptions_df)) {
    return(list(error = "'Assumption' column missing in the imported assumptions table."))
  }
  colnames(assumptions_df) <- trimws(colnames(assumptions_df))
  colnames(assumptions_df) <- gsub("^X", "", colnames(assumptions_df)) # Remove X prefix if present
  
  year_cols <- colnames(assumptions_df)[grepl("^\\d{4}$", colnames(assumptions_df))]
  if (length(year_cols) == 0) {
    return(list(error = "No valid year columns (YYYY) detected in assumptions table."))
  }
  year_cols <- sort(year_cols) # Ensure order
  
  # Reorder and select columns
  assumptions_df <- assumptions_df[, c("Assumption", year_cols)]
  
  # Convert year columns to numeric, coercing errors to NA
  for (col in year_cols) {
    assumptions_df[[col]] <- suppressWarnings(as.numeric(assumptions_df[[col]]))
    # Assuming imported values are DECIMALS. If they could be percentages, add check:
    # if (any(abs(assumptions_df[[col]]) > 1, na.rm = TRUE) && all(assumptions_df[[col]] >= -1000 & assumptions_df[[col]] <= 1000, na.rm = TRUE)) { # Heuristic: if > 1 and reasonable range, assume %
    #   assumptions_df[[col]] <- assumptions_df[[col]] / 100
    # }
  }
  
  # --- Parse Parameters ---
  params_df <- tryCatch({
    # Use the captured header
    read.csv(text = c(params_header, params_content),
             stringsAsFactors = FALSE,
             na.strings = c("NA", "", "NULL"))
  }, error = function(e) { NULL })
  
  if (is.null(params_df) || !all(c("Parameter", "Value") %in% colnames(params_df))) {
    return(list(error = "Error parsing parameters table or required columns missing."))
  }
  
  # Convert to a named list for easier use
  params_list <- as.list(params_df$Value)
  names(params_list) <- params_df$Parameter
  # Ensure numeric types where expected
  numeric_params <- c("risk_free_rate", "equity_premium", "beta", "country_premium",
                      "long_run_inflation_expectations", "additional_terminal_growth",
                      "market_cap", "base_revenue", "non_operating_assets", "CAP") # Add CAP if it's exported
  for(p_name in numeric_params) {
    if(p_name %in% names(params_list)) {
      params_list[[p_name]] <- suppressWarnings(as.numeric(params_list[[p_name]]))
    }
  }
  
  
  return(list(assumptions_df = assumptions_df, params_list = params_list))
}
