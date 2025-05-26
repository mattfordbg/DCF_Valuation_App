
#' Apply Fade to a Subset of Assumption Row Values
#'
#' @param values_vector Numeric vector of current assumption values (decimals) for the entire forecast period.
#' @param start_period Integer, the 1-based index (period number) where the fade should start.
#' @param end_period Integer, the 1-based index (period number) where the fade should end.
#' @param initial_value Numeric, the target value (percentage) at the beginning of the fade period (start_period).
#' @param final_value Numeric, the target value (percentage) at the end of the fade period (end_period).
#' @param fade_type String: "linear", "exponential", "geometric", "logistic".
#' @param fade_params List containing additional parameters needed for specific fades
#'                    (e.g., 'm' for exponential/geometric, 'alpha', 'inflection_year' for logistic).
#' @return Numeric vector of assumption values (decimals) with the specified subset faded. Returns NULL on critical error.
apply_assumption_fade <- function(values_vector, start_period, end_period, initial_value, final_value, fade_type, fade_params = list()) {
  
  # --- Input Validation ---
  if(is.null(values_vector) || !is.numeric(values_vector) || length(values_vector) == 0) {
    stop("Invalid or empty 'values_vector' provided.")
  }
  total_periods <- length(values_vector)
  
  if(is.null(start_period) || !is.numeric(start_period) || start_period < 1 || start_period > total_periods) {
    stop(paste("Invalid 'start_period'. Must be between 1 and", total_periods))
  }
  if(is.null(end_period) || !is.numeric(end_period) || end_period < 1 || end_period > total_periods) {
    stop(paste("Invalid 'end_period'. Must be between 1 and", total_periods))
  }
  if(start_period > end_period) {
    stop("'start_period' cannot be greater than 'end_period'.")
  }
  if(is.null(initial_value) || !is.numeric(initial_value) || is.na(initial_value)) {
    stop("Invalid or missing 'initial_value' for fade.")
  }
  if(is.null(final_value) || !is.numeric(final_value) || is.na(final_value)) {
    stop("Invalid or missing 'final_value' for fade.")
  }
  if(is.null(fade_type) || !fade_type %in% c("linear", "exponential", "geometric", "logistic")) {
    stop(paste("Invalid or unsupported fade type:", fade_type))
  }
  
  # Print key inputs for debugging
  print(paste("apply_assumption_fade (subset) called with:",
              "periods =", start_period, "to", end_period,
              "type =", fade_type,
              "initial(%) =", initial_value,
              "final(%) =", final_value))
  print("Fade Params:")
  print(fade_params)
  
  # --- Initialization ---
  result_vector <- values_vector # Start with original values
  n_steps <- end_period - start_period + 1 # Number of steps IN the fade
  init_val_dec <- initial_value / 100  # Convert target % to decimal
  final_val_dec <- final_value / 100   # Convert target % to decimal
  
  # Handle edge case: fade over 0 or 1 period
  if (n_steps <= 1) {
    print("Fade length <= 1, setting value at start_period directly.")
    result_vector[start_period] <- final_val_dec # Set the single point to the target final value
    return(result_vector)
  }
  
  # --- Fade Calculation Loop (over the subset) ---
  faded_subset_values <- numeric(n_steps) # Store calculated values for the subset
  
  if (fade_type == "linear") {
    for (t in 0:(n_steps - 1)) { # Time index from 0 to n_steps-1
      current_step_val <- init_val_dec + ((final_val_dec - init_val_dec) / (n_steps - 1)) * t
      faded_subset_values[t + 1] <- current_step_val
    }
  } else if (fade_type %in% c("exponential", "geometric")) {
    m <- fade_params$m %||% (if(fade_type == "geometric") 2 else 1) # Default shape if needed
    if (!is.numeric(m) || is.na(m) || m < 0) m <- (if(fade_type == "geometric") 2 else 1) # Ensure valid default
    
    if (abs(init_val_dec - final_val_dec) < 1e-9) { # If start/end targets are the same
      faded_subset_values <- rep(init_val_dec, n_steps)
    } else {
      if (fade_type == "exponential") {
        # Use value slightly offset from final for calculation stability if needed
        g_n <- if(init_val_dec > final_val_dec) final_val_dec + 1e-9 else final_val_dec - 1e-9
        log_arg <- (g_n - final_val_dec) / (init_val_dec - final_val_dec)
        
        k <- 1 # Default k
        if(log_arg > 0) {
          # Calculate k based on n_steps
          k <- -(1 / ((n_steps - 1)^m)) * log(log_arg)
        } else {
          warning("Could not calculate exponential k parameter (log_arg <= 0), defaulting k=1.")
        }
        print(paste("Exponential fade calculated k:", k))
        
        for (t in 0:(n_steps - 1)) { # Time index 0 to n_steps-1
          current_step_val <- final_val_dec + (init_val_dec - final_val_dec) * exp(-k * (t^m))
          faded_subset_values[t + 1] <- current_step_val
        }
      } else { # Geometric
        for (t in 0:(n_steps - 1)) { # Time index 0 to n_steps-1
          current_step_val <- final_val_dec + ((init_val_dec - final_val_dec) * (1 - ((t / (n_steps - 1))^m)))
          faded_subset_values[t + 1] <- current_step_val
        }
      }
    }
  } else if (fade_type == "logistic") {
    alpha <- fade_params$alpha %||% (if(initial_value > final_value) 1 else -1) # Default based on direction
    # Inflection point 'beta' needs to be relative to the start of the fade period (0-based t)
    inflection_year <- fade_params$inflection_year %||% floor((total_periods + 1) / 2) # Default year if missing
    # Convert inflection year to 0-based index relative to the fade start
    beta_t <- inflection_year - start_period
    
    print(paste("Logistic fade params: alpha=", alpha, "inflection_year=", inflection_year, "beta_t=", beta_t))
    
    if (abs(init_val_dec - final_val_dec) < 1e-9) {
      faded_subset_values <- rep(init_val_dec, n_steps)
    } else {
      # Calculate raw logistic values for the subset period
      raw_logistic_vals <- numeric(n_steps)
      for (t in 0:(n_steps - 1)) { # Time index 0 to n_steps-1
        raw_logistic_vals[t + 1] <- final_val_dec + ((init_val_dec - final_val_dec) / (1 + exp(alpha * (t - beta_t))))
      }
      
      # Get the calculated values at the start (t=0) and end (t=n_steps-1) of the raw curve
      val_at_t0 <- raw_logistic_vals[1]
      val_at_tn_minus_1 <- raw_logistic_vals[n_steps]
      
      # Linearly scale the intermediate values to ensure exact endpoints
      if (abs(val_at_tn_minus_1 - val_at_t0) < 1e-9) { # Avoid division by zero
        faded_subset_values <- rep(init_val_dec, n_steps)
      } else {
        for(t in 0:(n_steps - 1)) {
          scaled_val <- init_val_dec + (raw_logistic_vals[t+1] - val_at_t0) * (final_val_dec - init_val_dec) / (val_at_tn_minus_1 - val_at_t0)
          faded_subset_values[t + 1] <- scaled_val
        }
      }
      # Ensure endpoints are exact after scaling
      faded_subset_values[1] <- init_val_dec
      faded_subset_values[n_steps] <- final_val_dec
    }
  } else {
    # Should not happen due to validation, but return original subset if it does
    warning(paste("Unknown fade type encountered in calculation:", fade_type))
    return(result_vector) # Return unmodified vector on error
  }
  
  # --- Assign calculated subset back to the result vector ---
  result_vector[start_period:end_period] <- faded_subset_values
  
  return(result_vector)
}