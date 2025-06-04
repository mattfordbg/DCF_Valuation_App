# --- State Management Functions ---

# Create a centralized app state object
initialize_app_state <- function() {
  reactiveValues(
    # Data import and processing
    data = list(
      raw = NULL, selected = NULL, import_count = 0,
      import_metadata = list(last_import_time = NULL, sheets_imported = character(0))
    ),
    
    # Standardization process
    standardization = list(
      years_detected = NULL, components = numeric(0), defined_metrics = list(),
      standardized_data = NULL, calculation_log = "", last_used_component = ""
    ),
    
    # --- Main DCF model core ---
    dcf = list(
      assumptions = data.frame( # Default structure
        Assumption = c("Revenue Growth","Operating Profit Margin","Interest Expense","Cash Tax Rate","Depreciation & Amortisation","Incremental WC","Incremental FC","Net Debt Issuance"), "2024" = 0.05, check.names = FALSE
      ),
      # --- MODIFIED: Explicitly make parameters reactive ---
      parameters = reactiveValues( # Wrap list in reactiveValues()
        risk_free_rate = 0, equity_premium = 0, beta = 1, country_premium = 0,
        long_run_inflation = 2, additional_terminal_growth = 0, CAP = 10, market_cap = 0
      ),
      # --- END MODIFICATION ---
      base_revenue = 0, non_operating_assets = 0, projections = NULL
    ),
    
    # --- Hurdle DCF Model ---
    hurdle_dcf = list(
      assumptions = data.frame( # Default structure
        Assumption = c("Revenue Growth","Operating Profit Margin","Interest Expense","Cash Tax Rate","Depreciation & Amortisation","Incremental WC","Incremental FC","Net Debt Issuance"), "2030" = 0.05, check.names = FALSE
      ),
      # --- MODIFIED: Explicitly make parameters reactive ---
      parameters = reactiveValues( # Wrap list in reactiveValues()
        risk_free_rate = 0, equity_premium = 0, beta = 1, country_premium = 0,
        long_run_inflation = 2, additional_terminal_growth = 0, CAP = 10, market_cap = 0
      ),
      # --- END MODIFICATION ---
      base_revenue = 0, non_operating_assets = 0, projections = NULL,
      hurdle_inputs = list(target_multiple = 5, holding_period = 5, dilution_pct = 20),
      last_hist_year = NULL
    ),
    
    # --- UI states ---
    ui = list(
      programmatic_update = FALSE,
      # Initialize directly as lists for clarity
      fade_inputs_modified = as.list(rep(FALSE, 8)),
      hurdle_fade_inputs_modified = as.list(rep(FALSE, 8)),
      hurdle_params_modified = list(
        risk_free_rate = FALSE, equity_premium = FALSE, beta = FALSE, country_premium = FALSE,
        long_run_inflation = FALSE, additional_terminal_growth = FALSE, CAP = FALSE
      ),
      imported_file_data = NULL
    )
  )
}

# Get state values safely
get_raw_data <- function(state) state$data$raw
get_selected_data <- function(state) state$data$selected
get_import_count <- function(state) state$data$import_count
get_years_detected <- function(state) state$standardization$years_detected
get_components_suffixes <- function(state) {
  # Ensure it returns numeric(0) if NULL or not numeric
  suffixes <- state$standardization$components
  if (is.null(suffixes) || !is.numeric(suffixes)) {
    return(numeric(0))
  } else {
    return(suffixes)
  }
}
get_defined_metrics <- function(state) state$standardization$defined_metrics
get_standardized_data <- function(state) state$standardization$standardized_data
get_calculation_log <- function(state) state$standardization$calculation_log
get_assumptions_data <- function(state) state$dcf$assumptions
get_projections_data <- function(state) state$dcf$projections
get_dcf_parameter <- function(state, param_name) state$dcf$parameters[[param_name]]
get_base_revenue <- function(state) state$dcf$base_revenue
get_non_operating_assets <- function(state) state$dcf$non_operating_assets
is_programmatic_update <- function(state) state$ui$programmatic_update
get_fade_inputs_modified <- function(state) state$ui$fade_inputs_modified
get_last_used_component <- function(state) state$standardization$last_used_component

get_hurdle_assumptions_data <- function(state) state$hurdle_dcf$assumptions
get_hurdle_projections_data <- function(state) state$hurdle_dcf$projections
get_hurdle_dcf_parameter <- function(state, param_name) state$hurdle_dcf$parameters[[param_name]]
get_hurdle_base_revenue <- function(state) state$hurdle_dcf$base_revenue
get_hurdle_non_operating_assets <- function(state) state$hurdle_dcf$non_operating_assets
get_hurdle_inputs <- function(state) state$hurdle_dcf$hurdle_inputs
get_hurdle_fade_inputs_modified <- function(state) state$ui$hurdle_fade_inputs_modified
get_hurdle_params_modified <- function(state) state$ui$hurdle_params_modified

# Set state values safely
set_raw_data <- function(state, value) state$data$raw <- value
set_selected_data <- function(state, value) state$data$selected <- value
increment_import_count <- function(state, count = 1) state$data$import_count <- state$data$import_count + count
reset_import_count <- function(state) state$data$import_count <- 0

set_years_detected <- function(state, value) state$standardization$years_detected <- value
set_components_suffixes <- function(state, value) {
  # Ensure value is numeric or NULL/empty
  if (is.null(value) || length(value) == 0) {
    state$standardization$components <- numeric(0)
  } else if (is.numeric(value)) {
    state$standardization$components <- unique(value) # Store unique numeric values
  } else {
    warning("Attempted to set components state with non-numeric value.")
    state$standardization$components <- numeric(0) # Fallback
  }
}
set_defined_metrics <- function(state, value) state$standardization$defined_metrics <- value
set_standardized_data <- function(state, value) state$standardization$standardized_data <- value
set_calculation_log <- function(state, value) state$standardization$calculation_log <- value
set_last_used_component <- function(state, value) state$standardization$last_used_component <- value

set_assumptions_data <- function(state, value) state$dcf$assumptions <- value
set_projections_data <- function(state, value) state$dcf$projections <- value
set_base_revenue <- function(state, value) state$dcf$base_revenue <- value
set_non_operating_assets <- function(state, value) state$dcf$non_operating_assets <- value
set_programmatic_update <- function(state, value) state$ui$programmatic_update <- value

set_fade_input_modified <- function(state, index, value) {
  # Check list validity and index bounds carefully
  current_list <- state$ui$fade_inputs_modified
  if (!is.null(current_list) && is.list(current_list) && length(current_list) == 8 && # Check length is 8
      index >= 1 && index <= 8) { # Use fixed index check
    if(is.logical(value) && length(value) == 1) {
      # Use list indexing
      state$ui$fade_inputs_modified[[index]] <- value
    } else {
      warning(paste("Attempted to set fade_input_modified with non-logical value for index:", index))
    }
  } else {
    warning(paste("Attempted to set fade_input_modified - list invalid or index out of bounds (1-8):", index))
  }
}

set_hurdle_assumptions_data <- function(state, value) state$hurdle_dcf$assumptions <- value
set_hurdle_projections_data <- function(state, value) state$hurdle_dcf$projections <- value
set_hurdle_base_revenue <- function(state, value) state$hurdle_dcf$base_revenue <- value
set_hurdle_non_operating_assets <- function(state, value) state$hurdle_dcf$non_operating_assets <- value
set_hurdle_inputs <- function(state, value) state$hurdle_dcf$hurdle_inputs <- value # Set whole list
set_hurdle_fade_input_modified <- function(state, index, value) {
  # Check list validity and index bounds carefully
  current_list <- state$ui$hurdle_fade_inputs_modified
  if (!is.null(current_list) && is.list(current_list) && length(current_list) == 8 && # Check length is 8
      index >= 1 && index <= 8) { # Use fixed index check
    if(is.logical(value) && length(value) == 1) {
      # Use list indexing
      state$ui$hurdle_fade_inputs_modified[[index]] <- value
    } else {
      warning(paste("Attempted to set hurdle_fade_input_modified with non-logical value for index:", index))
    }
  } else {
    warning(paste("Attempted to set hurdle_fade_input_modified - list invalid or index out of bounds (1-8):", index))
  }
}
set_hurdle_param_modified <- function(state, param_name, value) {
  # Ensure param_name exists in the list before setting
  if (param_name %in% names(state$ui$hurdle_params_modified)) {
    state$ui$hurdle_params_modified[[param_name]] <- value
  } else {
    warning(paste("Attempted to set modified flag for unknown hurdle parameter:", param_name))
  }
}

# More complex mutators
add_component_suffix <- function(state, suffix) {
  # Ensure suffix is numeric
  if (!is.numeric(suffix) || length(suffix) != 1) {
    warning("Invalid suffix provided to add_component_suffix.")
    return()
  }
  
  # Get current suffixes using the getter (ensures type)
  current_suffixes <- get_components_suffixes(state)
  
  # Print for debugging
  print(paste("Current suffixes before adding:", paste(current_suffixes, collapse=", ")))
  print(paste("Adding new suffix:", suffix))
  
  # Add only if not already present
  if (!suffix %in% current_suffixes) {
    state$standardization$components <- c(current_suffixes, suffix)
    print(paste("New suffixes:", paste(state$standardization$components, collapse=", ")))
  } else {
    print(paste("Suffix", suffix, "already exists, not adding"))
  }
}

remove_component_suffix <- function(state, suffix) {
  if (!is.numeric(suffix) || length(suffix) != 1) {
    warning("Invalid suffix provided to remove_component_suffix.")
    return()
  }
  current_suffixes <- get_components_suffixes(state)
  state$standardization$components <- current_suffixes[current_suffixes != suffix]
}

add_defined_metric <- function(state, metric_name, metric_data) {
  state$standardization$defined_metrics[[metric_name]] <- metric_data
}

remove_defined_metric <- function(state, metric_name) {
  state$standardization$defined_metrics[[metric_name]] <- NULL
}

append_calculation_log <- function(state, text) {
  state$standardization$calculation_log <- paste0(
    state$standardization$calculation_log, text, "\n"
  )
}

update_assumption_value <- function(state, assumption_name, year, value) {
  row_idx <- which(state$dcf$assumptions$Assumption == assumption_name)
  if(length(row_idx) > 0) {
    state$dcf$assumptions[row_idx, year] <- value
  }
}

# Reset different parts of the state
reset_data <- function(state) {
  state$data$raw <- NULL
  state$data$selected <- NULL
  state$data$import_count <- 0
  state$data$import_metadata$last_import_time <- NULL
  state$data$import_metadata$sheets_imported <- character(0)
}

reset_standardization <- function(state) {
  state$standardization$years_detected <- NULL
  state$standardization$components <- list()
  state$standardization$defined_metrics <- list()
  state$standardization$standardized_data <- NULL
  state$standardization$calculation_log <- ""
  state$standardization$last_used_component <- ""
}

reset_components <- function(state) {
  # This function primarily affects UI state tracked in standardization$components
  # It should likely clear the UI elements too, which might be better handled in the module server
  state$standardization$components <- numeric(0)
}
