# File: utils/dcf_calculator.R

#' Calculate Discount Rate (Cost of Equity using CAPM)
#' @param dcf_parameters A list containing numeric parameters: risk_free_rate (%),
#'   equity_premium (%), beta, country_premium (%).
#' @return Numeric discount rate (decimal).
calculate_discount_rate <- function(dcf_parameters) {
  # Use %||% for safe access with defaults
  risk_free_rate <- (dcf_parameters$risk_free_rate %||% 0) / 100
  equity_premium <- (dcf_parameters$equity_premium %||% 0) / 100
  beta <- dcf_parameters$beta %||% 1
  country_premium <- (dcf_parameters$country_premium %||% 0) / 100
  
  # Basic validation inside function
  if (!is.numeric(risk_free_rate) || !is.numeric(equity_premium) || !is.numeric(beta) || !is.numeric(country_premium)) {
    warning("Non-numeric input provided to calculate_discount_rate")
    return(NA_real_) # Return NA on invalid input
  }
  
  discount_rate <- risk_free_rate + (equity_premium * beta) + country_premium
  return(discount_rate)
}

#' Calculate Terminal Growth Rate
#' @param dcf_parameters A list containing numeric parameters: long_run_inflation (%),
#'   additional_terminal_growth (%).
#' @return Numeric terminal growth rate (decimal).
calculate_terminal_growth_rate <- function(dcf_parameters) {
  # Use %||% for safe access with defaults
  long_run_inflation <- dcf_parameters$long_run_inflation %||% 2
  additional_growth <- dcf_parameters$additional_terminal_growth %||% 0
  
  # Basic validation inside function
  if (!is.numeric(long_run_inflation) || !is.numeric(additional_growth)) {
    warning("Non-numeric input provided to calculate_terminal_growth_rate")
    return(NA_real_) # Return NA on invalid input
  }
  
  terminal_growth_rate <- (long_run_inflation + additional_growth) / 100
  return(terminal_growth_rate)
}

#' Calculate DCF Projections
#'
#' @param assumptions_df Data frame of yearly assumptions. Assumes Col 1 "Assumption", subsequent columns are years (YYYY).
#' @param base_revenue Numeric value for the starting revenue (corresponding to the year *before* the first projection year).
#' @param non_op_assets Numeric value for non-operating assets.
#' @param dcf_params List containing discount_rate, terminal_growth_rate, CAP, and optionally market_cap and last_hist_year.
#' @return A data frame with the financial projections. Returns NULL if inputs are invalid.
calculate_dcf_projections <- function(assumptions_df, base_revenue, non_op_assets, dcf_params, std_data) {
  
  # --- Calculate last_hist_year INTERNALLY ---
  print("calculate_dcf_projections: Determining last_hist_year internally...")
  last_hist_year <- NULL
  if (!is.null(std_data)) { # Prioritize standardized data
    std_year_cols <- grep("^\\d{4}$", colnames(std_data), value = TRUE)
    if (length(std_year_cols) > 0) {
      last_hist_year <- max(as.numeric(std_year_cols))
    }
  }
  if (is.null(last_hist_year) && !is.null(assumptions_df)) { # Fallback to assumptions
    assum_year_cols <- grep("^\\d{4}$", colnames(assumptions_df), value = TRUE)
    if (length(assum_year_cols) > 0) {
      first_proj_year_in_table <- min(as.numeric(assum_year_cols))
      last_hist_year <- first_proj_year_in_table - 1
    }
  }
  if (is.null(last_hist_year)) { # Final fallback
    last_hist_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    warning(paste("calculate_dcf_projections: Could not determine last_hist_year, defaulting to", last_hist_year))
  }
  print(paste("  Internal last_hist_year:", last_hist_year))
  # --- End Calculate last_hist_year ---
  
  # Input validation
  print("calculate_dcf_projections: Starting validation...")
  req_assum_cols <- c("Revenue Growth", "Operating Profit Margin", "Interest Expense",
                      "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                      "Incremental FC", "Net Debt Issuance")
  
  # Print individual checks
  assum_valid <- !is.null(assumptions_df) && ("Assumption" %in% colnames(assumptions_df)) && all(req_assum_cols %in% assumptions_df$Assumption)
  print(paste("  Assumptions valid?", assum_valid))
  if (!assum_valid) print(paste("  Missing assumptions:", paste(setdiff(req_assum_cols, assumptions_df$Assumption), collapse=", ")))
  
  base_rev_valid <- !is.null(base_revenue) && is.numeric(base_revenue)
  print(paste("  Base Revenue valid?", base_rev_valid))
  
  noa_valid <- !is.null(non_op_assets) && is.numeric(non_op_assets)
  print(paste("  NOA valid?", noa_valid))
  
  params_exist <- !is.null(dcf_params)
  print(paste("  dcf_params exists?", params_exist))
  
  required_params_present <- FALSE
  params_list_names <- NULL
  if (params_exist && is.list(dcf_params)) {
    params_list_names <- names(dcf_params)
    required_params <- c("discount_rate", "terminal_growth_rate", "CAP")
    required_params_present <- all(required_params %in% params_list_names)
    print(paste("  Required params present?", required_params_present))
    if (!required_params_present) print(paste("  Missing params:", paste(setdiff(required_params, params_list_names), collapse=", ")))
  }
  
  last_hist_year_valid <- !is.null(last_hist_year) && is.numeric(last_hist_year)
  print(paste("  Internal last_hist_year valid?", last_hist_year_valid))
  
  if (is.null(assumptions_df) || !("Assumption" %in% colnames(assumptions_df)) || !all(req_assum_cols %in% assumptions_df$Assumption) ||
      is.null(base_revenue) || !is.numeric(base_revenue) ||
      is.null(non_op_assets) || !is.numeric(non_op_assets) ||
      is.null(dcf_params) || !all(c("discount_rate", "terminal_growth_rate", "CAP") %in% names(dcf_params)) || # Removed last_hist_year
      !last_hist_year_valid # Add check for internal value
  ) {
    warning("calculate_dcf_projections: Validation failed (check assumptions, base values, params, OR internal last_hist_year calc). Returning NULL.")
    return(NULL)
  }
  print("calculate_dcf_projections: Validation passed.")
  # --- End Validation ---
  
  discount_rate <- dcf_params$discount_rate
  terminal_growth_rate <- dcf_params$terminal_growth_rate
  cap <- as.integer(dcf_params$CAP)

  # --- Determine Projection Years (Uses last_hist_year directly) ---
  projection_years <- seq(from = last_hist_year + 1, length.out = cap)
  projection_year_cols <- as.character(projection_years)
  first_projection_year_col <- projection_year_cols[1]
  # --- End Determine Projection Years ---
  
  
  # Get year columns *available* in the assumptions table for lookups
  available_assumption_year_cols <- intersect(projection_year_cols, colnames(assumptions_df))
  
  if (length(available_assumption_year_cols) == 0) {
    warning("No matching year columns found between target projection years and assumptions table.")
    return(NULL) # Cannot proceed without assumptions
  }
  # Check if the number of available columns matches CAP. If not, we might need to extrapolate assumptions.
  # For now, we will only calculate for years where assumptions ARE available.
  if (length(available_assumption_year_cols) != cap) {
    warning(paste("Number of available assumption columns (", length(available_assumption_year_cols),
                  ") does not match target CAP (", cap, "). Projections may be incomplete.", sep=""))
    # Update projection_year_cols to only include those available? Or proceed and handle missing lookups?
    # Let's proceed but use the available ones for lookups. The projection structure will have all target years.
  }
  
  
  # Create structure for projections using TARGET projection years
  projection_rows <- c("Revenue", "Operating Profit", "Interest Expense",
                       "Profit Before Tax", "Taxes", "Net Income",
                       "Depreciation & Amortisation", "Incremental Working Capital Investment",
                       "Operating Cash Flow", "Incremental Fixed Capital Investment",
                       "Net Debt Issuance", "Free Cash Flow to Equity", "FCFE Growth (%)",
                       "Present Value of FCFE",
                       "Terminal Value",
                       "Present Value of Terminal Value",
                       "NPV",
                       "Non-Operating Assets",
                       "Intrinsic Value",
                       "Proportion of Market Cap")
  
  projections <- data.frame(Metric = projection_rows, stringsAsFactors = FALSE)
  # Initialize ALL target projection year columns with NA
  for (yr_char in projection_year_cols) {
    projections[[yr_char]] <- NA_real_
  }
  # Use Metric as row names for easier lookup during calculation, remove later
  rownames(projections) <- projections$Metric
  
  # Helper to get assumption value safely for a given projection year
  get_assumption <- function(name, target_proj_year_char) {
    # Find the corresponding column in the assumptions table (might be the same or different if extrapolation needed)
    # For now, assume direct match:
    lookup_col <- target_proj_year_char
    if (lookup_col %in% colnames(assumptions_df)) {
      val <- assumptions_df[assumptions_df$Assumption == name, lookup_col]
      if(length(val) == 1 && is.numeric(val) && !is.na(val)) return(val) else return(0) # Return 0 if NA or not found
    } else {
      # Handle cases where assumption is missing for the target year (e.g., use last available?)
      # For now, return 0 and issue a warning once?
      # warning(paste("Assumption", name, "not found for year", target_proj_year_char))
      return(0)
    }
  }
  
  # --- Projection Calculation Loop ---
  current_revenue <- base_revenue
  fcfe_values_list <- list() # Store FCFE for each calculated year
  
  # Loop through the TARGET projection years
  for (year_idx in 1:cap) {
    target_year_col <- projection_year_cols[year_idx] # e.g., "2025"
    
    # Get assumptions for this target year (using helper)
    revenue_growth <- get_assumption("Revenue Growth", target_year_col)
    op_margin <- get_assumption("Operating Profit Margin", target_year_col)
    interest_expense_rate <- get_assumption("Interest Expense", target_year_col)
    tax_rate <- get_assumption("Cash Tax Rate", target_year_col)
    da_rate <- get_assumption("Depreciation & Amortisation", target_year_col)
    wc_rate <- get_assumption("Incremental WC", target_year_col)
    fc_rate <- get_assumption("Incremental FC", target_year_col)
    debt_issuance_rate <- get_assumption("Net Debt Issuance", target_year_col)
    
    # Calculations
    revenue <- current_revenue * (1 + revenue_growth)
    operating_profit <- revenue * op_margin
    interest_expense <- revenue * interest_expense_rate # Apply rate to current year's revenue
    profit_before_tax <- operating_profit - interest_expense
    taxes <- max(0, profit_before_tax * tax_rate) # Ensure taxes aren't negative
    net_income <- profit_before_tax - taxes
    da <- revenue * da_rate # Apply rate to current year's revenue
    revenue_change <- revenue - current_revenue
    # Ensure incremental rates are applied correctly (often based on revenue change)
    wc_investment <- if(abs(revenue_change) > 1e-6) revenue_change * wc_rate else 0 # Avoid tiny changes causing large WC/FC swings
    fc_investment <- if(abs(revenue_change) > 1e-6) revenue_change * fc_rate else 0
    debt_issuance <- revenue * debt_issuance_rate # Apply rate to current year's revenue
    fcfe <- net_income + da - fc_investment - wc_investment + debt_issuance
    
    # Store values in the correct target year column
    projections["Revenue", target_year_col] <- revenue
    projections["Operating Profit", target_year_col] <- operating_profit
    projections["Interest Expense", target_year_col] <- interest_expense
    projections["Profit Before Tax", target_year_col] <- profit_before_tax
    projections["Taxes", target_year_col] <- taxes
    projections["Net Income", target_year_col] <- net_income
    projections["Depreciation & Amortisation", target_year_col] <- da
    projections["Incremental Working Capital Investment", target_year_col] <- wc_investment
    # projections["Operating Cash Flow", target_year_col] <- operating_cash_flow # Calculate if needed
    projections["Incremental Fixed Capital Investment", target_year_col] <- fc_investment
    projections["Net Debt Issuance", target_year_col] <- debt_issuance
    projections["Free Cash Flow to Equity", target_year_col] <- fcfe
    
    fcfe_values_list[[target_year_col]] <- fcfe # Store by year name
    current_revenue <- revenue # Update revenue for next iteration
  } # End projection loop
  
  # --- Calculate Revenue CAGR ---
  # Ensure this uses the correct columns now
  revenue_cagr_values <- rep(NA_real_, length(projection_year_cols))
  names(revenue_cagr_values) <- projection_year_cols
  if (base_revenue > 0) { # Avoid division by zero
    revenue_row_values <- as.numeric(projections["Revenue", projection_year_cols])
    for (i in 1:length(projection_year_cols)) {
      year_char <- projection_year_cols[i]
      # Use projections[row, col] access which is more robust if rownames change
      current_proj_revenue <- projections[projections$Metric == "Revenue", year_char]
      if (!is.na(current_proj_revenue) && current_proj_revenue > 0) { # Ensure positive revenue
        # CAGR is (End/Start)^(1/N) - 1, where N is number of periods elapsed (i)
        revenue_cagr_values[year_char] <- (current_proj_revenue / base_revenue)^(1 / i) - 1
      }
    }
  }
  # Create new row (ensure structure matches: Metric + year columns)
  cagr_row_df <- data.frame(Metric = "Revenue CAGR (%)", stringsAsFactors = FALSE)
  cagr_row_df[1, projection_year_cols] <- revenue_cagr_values * 100 # Store as percentage points
  # --- End Calculate Revenue CAGR ---
  
  # --- Insert CAGR row into projections dataframe ---
  # Find index of Revenue row
  revenue_row_index <- which(projections$Metric == "Revenue")
  if (length(revenue_row_index) == 1) {
    # Insert using rbind after splitting the dataframe
    projections <- rbind(
      projections[1:revenue_row_index, ],
      cagr_row_df,
      projections[(revenue_row_index + 1):nrow(projections), ]
    )
    # Reset rownames if they got messed up (important!)
    rownames(projections) <- NULL
    # Update rownames used for lookup if needed (if you still use them after this point)
    rownames(projections) <- projections$Metric # Re-apply if lookups needed below
  } else {
    # Fallback: append if Revenue row not found (less ideal position)
    projections <- rbind(projections, cagr_row_df)
    rownames(projections) <- NULL # Reset rownames
    rownames(projections) <- projections$Metric # Re-apply rownames
  }
  # --- End Insert CAGR row ---
  
  
  # --- Post-Loop Calculations ---
  # Use the list directly which preserves names
  fcfe_num_vector <- unlist(fcfe_values_list[projection_year_cols]) # Ensure correct order and length matching proj years
  
  # FCFE Growth
  fcfe_growth_rates <- rep(NA_real_, cap)
  if(cap > 1) {
    for(i in 2:cap) {
      # Use positional access based on the vector derived from projection_year_cols order
      if(!is.na(fcfe_num_vector[i-1]) && fcfe_num_vector[i-1] != 0 && !is.na(fcfe_num_vector[i])) {
        fcfe_growth_rates[i] <- (fcfe_num_vector[i] / fcfe_num_vector[i-1]) - 1
      }
    }
  }
  # Assign back using projection_year_cols names/positions
  projections["FCFE Growth (%)", projection_year_cols] <- fcfe_growth_rates * 100
  
  # Present Value of FCFE
  pv_fcfe_values <- numeric(cap)
  for (i in 1:cap) {
    if (!is.na(fcfe_num_vector[i])) {
      pv_fcfe_values[i] <- fcfe_num_vector[i] / ((1 + discount_rate)^i)
    } else {
      pv_fcfe_values[i] <- NA_real_
    }
  }
  projections["Present Value of FCFE", projection_year_cols] <- pv_fcfe_values
  
  # Terminal Value
  final_fcfe <- tail(fcfe_num_vector, 1) # Get the FCFE corresponding to the *last projection year*
  terminal_value <- NA_real_
  pv_terminal_value <- NA_real_
  # Ensure discount rate > terminal growth rate for calculation
  if(!is.na(final_fcfe) && !is.na(discount_rate) && !is.na(terminal_growth_rate) && (discount_rate > terminal_growth_rate)) {
    terminal_value <- (final_fcfe * (1 + terminal_growth_rate)) / (discount_rate - terminal_growth_rate)
    # PV of Terminal Value is discounted back from the end of the explicit forecast period (cap)
    pv_terminal_value <- terminal_value / ((1 + discount_rate)^cap)
  } else {
    if (!is.na(discount_rate) && !is.na(terminal_growth_rate) && discount_rate <= terminal_growth_rate) {
      warning("Terminal value cannot be calculated: Discount Rate <= Terminal Growth Rate.")
    }
  }
  
  # NPV
  npv <- sum(pv_fcfe_values, na.rm = TRUE) + ifelse(is.na(pv_terminal_value), 0, pv_terminal_value)
  
  # Intrinsic Value
  intrinsic_value <- npv + non_op_assets
  
  # Store final summary values (typically in the first projection year column for display)
  projections["Terminal Value", first_projection_year_col] <- terminal_value
  projections["Present Value of Terminal Value", first_projection_year_col] <- pv_terminal_value
  projections["NPV", first_projection_year_col] <- npv
  projections["Non-Operating Assets", first_projection_year_col] <- non_op_assets
  projections["Intrinsic Value", first_projection_year_col] <- intrinsic_value
  
  # Market Cap comparison
  market_cap_param <- dcf_params$market_cap
  if(!is.null(market_cap_param) && is.numeric(market_cap_param) && !is.na(market_cap_param) && market_cap_param > 0) {
    prop_of_market_cap <- intrinsic_value / market_cap_param # Store as proportion
    projections["Proportion of Market Cap", first_projection_year_col] <- prop_of_market_cap
  } else {
    # Ensure row exists before trying to remove
    if ("Proportion of Market Cap" %in% projections$Metric) {
      projections <- projections[projections$Metric != "Proportion of Market Cap", ]
    }
  }
  
  # Reset row names before returning
  rownames(projections) <- NULL
  return(projections)
}

# --- NEW HELPER FUNCTION ---
#' Run the Full DCF Calculation Steps
#'
#' This function orchestrates the validation, rate calculation, and projection calculation.
#' It takes primitive R objects as input.
#'
#' @param assumptions_df Assumptions data frame.
#' @param base_revenue Base revenue number.
#' @param non_op_assets Non-operating assets number.
#' @param dcf_params_list A standard R list containing parameters like risk_free_rate,
#'        equity_premium, beta, country_premium, long_run_inflation,
#'        additional_terminal_growth, CAP, market_cap.
#' @param std_data Standardized data frame (used to determine last_hist_year).
#' @return The resulting projections data frame, or NULL if any validation fails.
run_full_dcf_calculation <- function(assumptions_df, base_revenue, non_op_assets, dcf_params_list, std_data) {
  
  print("--- run_full_dcf_calculation START ---")
  # --- Input Validation ---
  valid <- TRUE
  if (is.null(assumptions_df) || !"Assumption" %in% colnames(assumptions_df) || ncol(assumptions_df) <= 1) { print("Validation failed: assumptions invalid."); valid <- FALSE }
  if (is.null(base_revenue) || !is.numeric(base_revenue)) { print("Validation failed: base_rev invalid."); valid <- FALSE }
  if (is.null(non_op_assets) || !is.numeric(non_op_assets) || non_op_assets < 0) { print("Validation failed: noa invalid."); valid <- FALSE }
  if (is.null(dcf_params_list) || !is.list(dcf_params_list)) { print("Validation failed: dcf_params_list invalid."); valid <- FALSE }
  if (valid && (is.null(dcf_params_list$CAP) || !is.numeric(dcf_params_list$CAP) || dcf_params_list$CAP <= 0)) { print("Validation failed: CAP invalid."); valid <- FALSE }
  
  # Check rate params *before* trying to calculate rates
  required_rate_params <- c("risk_free_rate", "equity_premium", "beta", "country_premium", "long_run_inflation", "additional_terminal_growth")
  if (valid) {
    missing_params <- setdiff(required_rate_params, names(dcf_params_list))
    non_numeric_params <- required_rate_params[sapply(required_rate_params, function(p) { val <- dcf_params_list[[p]]; is.null(val) || !is.numeric(val) || is.na(val) })]
    if (length(missing_params) > 0 || length(non_numeric_params) > 0) {
      print("Validation failed: Missing or non-numeric rate parameters."); valid <- FALSE
    }
  }
  
  if (!valid) { print("Exiting run_full_dcf_calculation due to validation failure."); return(NULL) }
  # --- End Validation ---
  
  
  # --- Calculate Rates ---
  # Create a working copy to add calculated rates
  params_for_projection <- dcf_params_list
  params_for_projection$discount_rate <- calculate_discount_rate(params_for_projection)
  params_for_projection$terminal_growth_rate <- calculate_terminal_growth_rate(params_for_projection)
  
  # Check if rate calculations were successful (returned non-NA)
  if(is.na(params_for_projection$discount_rate) || is.na(params_for_projection$terminal_growth_rate)) {
    print("Validation failed: Rate calculation resulted in NA.")
    return(NULL)
  }
  print("Rates calculated successfully.")
  
  
  # --- Call Core Projection Function ---
  # Ensure std_data is passed
  print("Calling core calculate_dcf_projections...")
  projection_result <- calculate_dcf_projections(
    assumptions_df = assumptions_df,
    base_revenue = base_revenue,
    non_op_assets = non_op_assets,
    dcf_params = params_for_projection, # Pass list with calculated rates
    std_data = std_data
  )
  
  print("--- run_full_dcf_calculation END ---")
  return(projection_result)
}
# --- END NEW HELPER ---

#' Perform Assumption Auto-Calculation based on Standardized Historical Data
#'
#' @param assumptions_df Current assumptions dataframe (e.g., app_state$dcf$assumptions).
#'        Must have an "Assumption" column and year columns (YYYY).
#' @param standardized_data_df Standardized data dataframe (e.g., app_state$standardization$standardized_data).
#'        Must have a metric column (assumed first col) and year columns (YYYY).
#' @param auto_calc_flags A list or vector of 8 booleans indicating which rows
#'        (corresponding to the standard 8 assumptions) to auto-calculate.
#' @param years_to_use A list or vector of 8 integers indicating how many
#'        historical years to use for the calculation for each corresponding row.
#' @return A modified assumptions dataframe with calculated values applied to selected rows/years.
#'         Returns the original assumptions_df if standardized data is invalid or calculations fail.
perform_assumption_auto_calc <- function(assumptions_df, standardized_data_df, auto_calc_flags, years_to_use) {
  
  print("Running perform_assumption_auto_calc...")
  
  # --- Input Validation ---
  if (is.null(standardized_data_df) || nrow(standardized_data_df) == 0 || ncol(standardized_data_df) <= 1) {
    warning("Auto-Calc: Standardized data is missing or invalid.")
    return(assumptions_df) # Return original if no standardized data
  }
  if (is.null(assumptions_df) || nrow(assumptions_df) == 0 || !"Assumption" %in% colnames(assumptions_df)) {
    warning("Auto-Calc: Input assumptions_df is invalid.")
    return(assumptions_df)
  }
  if (length(auto_calc_flags) != 8 || length(years_to_use) != 8) {
    warning("Auto-Calc: auto_calc_flags or years_to_use does not have length 8.")
    return(assumptions_df)
  }
  if (nrow(assumptions_df) < 8) {
    warning("Auto-Calc: assumptions_df has fewer than 8 rows.")
    # Proceed cautiously, might only calculate for available rows
  }
  
  
  # Get metric column name (more robustly)
  metric_col <- colnames(standardized_data_df)[1]
  if (!metric_col %in% colnames(standardized_data_df)) { # Double check
    warning("Auto-Calc: Could not identify metric column in standardized_data_df.")
    return(assumptions_df)
  }
  
  
  # Get year columns from standardized data (numeric columns excluding the metric column)
  potential_year_cols_std <- setdiff(colnames(standardized_data_df), metric_col)
  year_cols_std <- potential_year_cols_std[sapply(standardized_data_df[, potential_year_cols_std, drop = FALSE], is.numeric)]
  year_cols_std <- sort(year_cols_std) # Ensure chronological order
  
  if (length(year_cols_std) == 0) {
    warning("Auto-Calc: No numeric year columns found in standardized data.")
    return(assumptions_df) # Return original if no numeric year columns found
  }
  print(paste("Auto-Calc: Found standardized years:", paste(year_cols_std, collapse=", ")))
  
  
  # Get year columns from the assumptions table (should be all except "Assumption")
  year_cols_assum <- setdiff(colnames(assumptions_df), "Assumption")
  year_cols_assum <- year_cols_assum[grepl("^\\d{4}$", year_cols_assum)] # Ensure they look like years
  if (length(year_cols_assum) == 0) {
    warning("Auto-Calc: No year columns found in assumptions table to update.")
    return(assumptions_df)
  }
  print(paste("Auto-Calc: Target assumption years:", paste(year_cols_assum, collapse=", ")))
  
  
  # Work on a copy
  modified_assumptions_df <- assumptions_df
  
  # Standard assumption names expected in order
  standard_assumption_order <- c("Revenue Growth", "Operating Profit Margin", "Interest Expense",
                                 "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                                 "Incremental FC", "Net Debt Issuance")
  
  # Helper function to get a specific metric row from standardized data
  get_metric_row <- function(metric_name) {
    idx <- which(standardized_data_df[[metric_col]] == metric_name)
    if(length(idx) > 0) standardized_data_df[idx[1], , drop = FALSE] else NULL
  }
  
  
  # Loop through each assumption row (1 to 8)
  for (row_idx in 1:length(standard_assumption_order)) {
    # Ensure flags/years_to_use indices are valid
    if (row_idx > length(auto_calc_flags) || row_idx > length(years_to_use)) next
    
    if (isTRUE(auto_calc_flags[[row_idx]])) {
      assumption_name <- standard_assumption_order[row_idx]
      # Check if this assumption name actually exists in the target df
      target_row_in_assum_df <- which(modified_assumptions_df$Assumption == assumption_name)
      if (length(target_row_in_assum_df) == 0) {
        warning(paste("Auto-Calc: Assumption", assumption_name, "not found in assumptions table. Skipping."))
        next # Skip if assumption row doesn't exist
      }
      target_row_in_assum_df <- target_row_in_assum_df[1] # Use first match if multiple
      
      num_hist_years <- as.integer(years_to_use[[row_idx]])
      calculated_value <- NA_real_ # Default to NA
      
      
      print(paste("Auto-Calc: Processing", assumption_name, "using", num_hist_years, "hist years."))
      
      # Ensure we don't request more history than available
      num_hist_years <- min(num_hist_years, length(year_cols_std))
      if (num_hist_years <= 0) {
        warning(paste("Auto-Calc: Invalid number of history years (<=0) for", assumption_name))
        next # Skip if no history available/requested
      }
      
      
      # --- Calculation Logic ---
      # Select relevant historical year columns from standardized data
      hist_year_cols <- tail(year_cols_std, num_hist_years)
      # For CAGR/Changes, need one extra year if available
      hist_year_cols_plus_one <- if (length(year_cols_std) >= num_hist_years + 1) {
        tail(year_cols_std, num_hist_years + 1)
      } else {
        character(0) # Not enough years for change calculation
      }
      
      
      # Revenue Growth (CAGR)
      if (assumption_name == "Revenue Growth") {
        revenue_row <- get_metric_row("Revenue")
        if (!is.null(revenue_row) && length(hist_year_cols_plus_one) >= 2) {
          start_val <- suppressWarnings(as.numeric(revenue_row[[ hist_year_cols_plus_one[1] ]]))
          end_val <- suppressWarnings(as.numeric(revenue_row[[ tail(hist_year_cols_plus_one, 1) ]]))
          # Check validity for CAGR
          if (!is.na(start_val) && !is.na(end_val) && start_val != 0 && sign(start_val) == sign(end_val) && start_val > 0) {
            calculated_value <- (end_val / start_val)^(1 / num_hist_years) - 1
          } else {
            warning("Auto-Calc: Cannot calculate Revenue Growth CAGR (invalid start/end values).")
          }
        } else { warning("Auto-Calc: Not enough data for Revenue Growth CAGR.") }
      }
      
      # Operating Profit Margin (% of Revenue)
      else if (assumption_name == "Operating Profit Margin") {
        op_row <- get_metric_row("Operating Profit")
        rev_row <- get_metric_row("Revenue")
        if (!is.null(op_row) && !is.null(rev_row) && length(hist_year_cols) > 0) {
          op_vals <- suppressWarnings(as.numeric(op_row[, hist_year_cols]))
          rev_vals <- suppressWarnings(as.numeric(rev_row[, hist_year_cols]))
          # Calculate margin where revenue is not zero
          valid_indices <- which(!is.na(op_vals) & !is.na(rev_vals) & rev_vals != 0)
          if(length(valid_indices) > 0) {
            margins <- op_vals[valid_indices] / rev_vals[valid_indices]
            calculated_value <- mean(margins, na.rm = TRUE)
          } else { warning("Auto-Calc: No valid data points for OP Margin calculation.") }
        } else { warning("Auto-Calc: Missing Operating Profit or Revenue data.") }
      }
      
      # Interest Expense (% of Revenue)
      else if (assumption_name == "Interest Expense") {
        int_row <- get_metric_row("Interest Expense")
        rev_row <- get_metric_row("Revenue")
        if (!is.null(int_row) && !is.null(rev_row) && length(hist_year_cols) > 0) {
          int_vals <- suppressWarnings(as.numeric(int_row[, hist_year_cols]))
          rev_vals <- suppressWarnings(as.numeric(rev_row[, hist_year_cols]))
          valid_indices <- which(!is.na(int_vals) & !is.na(rev_vals) & rev_vals != 0)
          if(length(valid_indices) > 0) {
            ratios <- int_vals[valid_indices] / rev_vals[valid_indices]
            calculated_value <- mean(ratios, na.rm = TRUE)
          } else { warning("Auto-Calc: No valid data points for Interest Expense % Revenue.") }
        } else { warning("Auto-Calc: Missing Interest Expense or Revenue data.") }
      }
      
      # Cash Tax Rate (% of PBT)
      else if (assumption_name == "Cash Tax Rate") {
        tax_row <- get_metric_row("Tax")
        # Attempt to get PBT. If not defined, maybe calculate it? For now, require it.
        pbt_row <- get_metric_row("Profit Before Tax")
        if (is.null(pbt_row)) {
          op_row <- get_metric_row("Operating Profit")
          int_row <- get_metric_row("Interest Expense")
          if (!is.null(op_row) && !is.null(int_row)) {
            warning("Auto-Calc: 'Profit Before Tax' not found in standardized data, attempting calculation (OP - IntExp).")
            pbt_calculated_vals <- suppressWarnings(as.numeric(op_row[, year_cols_std])) - suppressWarnings(as.numeric(int_row[, year_cols_std]))
            # Create a temporary row structure
            pbt_row <- data.frame(Metric = "Profit Before Tax")
            pbt_row[1, year_cols_std] <- pbt_calculated_vals
          } else {
            warning("Auto-Calc: Cannot calculate Cash Tax Rate - PBT or components missing.")
          }
        }
        
        
        if (!is.null(tax_row) && !is.null(pbt_row) && length(hist_year_cols) > 0) {
          tax_vals <- suppressWarnings(as.numeric(tax_row[, hist_year_cols]))
          pbt_vals <- suppressWarnings(as.numeric(pbt_row[, hist_year_cols]))
          # Calculate rate where PBT is positive and tax value exists
          valid_indices <- which(!is.na(tax_vals) & !is.na(pbt_vals) & pbt_vals > 0)
          if(length(valid_indices) > 0) {
            rates <- tax_vals[valid_indices] / pbt_vals[valid_indices]
            # Constrain rate between 0 and 1 (0% and 100%)
            calculated_value <- max(0, min(1, mean(rates, na.rm = TRUE)))
          } else { warning("Auto-Calc: No valid data points (PBT > 0) for Cash Tax Rate.") }
        } else { if(is.null(pbt_row)) warning("Auto-Calc: Cannot calculate Cash Tax Rate - PBT missing.") else warning("Auto-Calc: Missing Tax data.") }
      }
      
      # Depreciation & Amortisation (% of Revenue)
      else if (assumption_name == "Depreciation & Amortisation") {
        da_row <- get_metric_row("Depreciation & Amortisation")
        rev_row <- get_metric_row("Revenue")
        if (!is.null(da_row) && !is.null(rev_row) && length(hist_year_cols) > 0) {
          da_vals <- suppressWarnings(as.numeric(da_row[, hist_year_cols]))
          rev_vals <- suppressWarnings(as.numeric(rev_row[, hist_year_cols]))
          valid_indices <- which(!is.na(da_vals) & !is.na(rev_vals) & rev_vals != 0)
          if(length(valid_indices) > 0) {
            ratios <- da_vals[valid_indices] / rev_vals[valid_indices]
            calculated_value <- mean(ratios, na.rm = TRUE)
          } else { warning("Auto-Calc: No valid data points for D&A % Revenue.") }
        } else { warning("Auto-Calc: Missing D&A or Revenue data.") }
      }
      
      # Incremental WC (% of Change in Revenue)
      else if (assumption_name == "Incremental WC") {
        wc_row <- get_metric_row("Working Capital Excluding Cash and Cash Equivalents")
        rev_row <- get_metric_row("Revenue")
        if (!is.null(wc_row) && !is.null(rev_row) && length(hist_year_cols_plus_one) >= 2) {
          wc_vals <- suppressWarnings(as.numeric(wc_row[, hist_year_cols_plus_one]))
          rev_vals <- suppressWarnings(as.numeric(rev_row[, hist_year_cols_plus_one]))
          wc_change <- diff(wc_vals)
          rev_change <- diff(rev_vals)
          # Calculate where revenue change is not zero
          valid_indices <- which(!is.na(wc_change) & !is.na(rev_change) & rev_change != 0)
          if(length(valid_indices) > 0) {
            ratios <- wc_change[valid_indices] / rev_change[valid_indices]
            calculated_value <- mean(ratios, na.rm = TRUE)
          } else { warning("Auto-Calc: No valid data points (non-zero revenue change) for Incremental WC.") }
        } else { warning("Auto-Calc: Not enough data or missing WC/Revenue for Incremental WC.") }
      }
      
      # Incremental FC (% of Change in Revenue) - Assuming Fixed Capital is CAPEX here
      else if (assumption_name == "Incremental FC") {
        # NOTE: This interpretation might need adjustment based on how "Fixed Capital" is defined.
        # Standard method is often CAPEX / Change in Revenue.
        # If "Fixed Capital" represents the *stock* of fixed assets, need Change in Fixed Assets.
        # Let's assume "Fixed Capital" represents CAPEX for now.
        capex_row <- get_metric_row("Fixed Capital") # Assuming this IS CAPEX
        rev_row <- get_metric_row("Revenue")
        if (!is.null(capex_row) && !is.null(rev_row) && length(hist_year_cols_plus_one) >= 2) {
          # Capex corresponds to the *end* year of the change period
          capex_vals <- suppressWarnings(as.numeric(capex_row[, tail(hist_year_cols_plus_one, num_hist_years)])) # Get CAPEX for the N years
          rev_vals <- suppressWarnings(as.numeric(rev_row[, hist_year_cols_plus_one]))
          rev_change <- diff(rev_vals) # N revenue changes
          
          valid_indices <- which(!is.na(capex_vals) & !is.na(rev_change) & rev_change != 0)
          if(length(valid_indices) > 0) {
            ratios <- capex_vals[valid_indices] / rev_change[valid_indices]
            calculated_value <- mean(ratios, na.rm = TRUE)
          } else { warning("Auto-Calc: No valid data points (non-zero revenue change) for Incremental FC.") }
        } else { warning("Auto-Calc: Not enough data or missing Fixed Capital/Revenue for Incremental FC.") }
      }
      
      # Net Debt Issuance (% of Revenue)
      else if (assumption_name == "Net Debt Issuance") {
        debt_row <- get_metric_row("Net Debt Issuance")
        rev_row <- get_metric_row("Revenue")
        if (!is.null(debt_row) && !is.null(rev_row) && length(hist_year_cols) > 0) {
          debt_vals <- suppressWarnings(as.numeric(debt_row[, hist_year_cols]))
          rev_vals <- suppressWarnings(as.numeric(rev_row[, hist_year_cols]))
          valid_indices <- which(!is.na(debt_vals) & !is.na(rev_vals) & rev_vals != 0)
          if(length(valid_indices) > 0) {
            ratios <- debt_vals[valid_indices] / rev_vals[valid_indices]
            calculated_value <- mean(ratios, na.rm = TRUE)
          } else { warning("Auto-Calc: No valid data points for Net Debt Issuance % Revenue.") }
        } else { warning("Auto-Calc: Missing Net Debt Issuance or Revenue data.") }
      }
      
      
      # --- Apply the calculated value ---
      if (!is.na(calculated_value) && is.finite(calculated_value)) {
        print(paste("  Applying calculated value", round(calculated_value, 4), "to", assumption_name))
        # Apply to all future years in the assumptions table
        modified_assumptions_df[target_row_in_assum_df, year_cols_assum] <- calculated_value
      } else {
        warning(paste("Auto-Calc: Calculation resulted in NA/Inf for", assumption_name,". Keeping original values."))
      }
    } # End if auto_calc_flags[[row_idx]] is TRUE
  } # End loop through assumption rows
  
  print("Finished perform_assumption_auto_calc.")
  return(modified_assumptions_df)
}