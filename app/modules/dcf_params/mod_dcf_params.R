# --- DCF Parameters Module (Sidebar) ---

dcfParamsUI <- function(id) {
  ns <- NS(id)
  div(class="fixed-sidebar", # Keep class for CSS targeting
      h4("DCF Parameters"),
      
      # Use namespaced IDs
      numericInput(ns("risk_free_rate"), "Risk-Free Rate (%)", value = 0.0, step=0.1),
      numericInput(ns("equity_premium"), "Equity Risk Premium (%)", value = 0.0, step=0.1),
      numericInput(ns("beta"), "Beta", value = 1.0, step=0.05),
      numericInput(ns("country_premium"), "Country Risk Premium (%)", value = 0.0, step=0.1),
      numericInput(ns("long_run_inflation"), "Long-run Inflation (%)", value = 2.0, step=0.1),
      numericInput(ns("additional_terminal_growth"), "Add'l Terminal Growth (%)", value = 0.0, step=0.1),
      numericInput(ns("CAP"), "Competitive Advantage Period", value = 5, min = 1, max = 100, step = 1),
      numericInput(ns("market_cap"), "Market Cap", value = 0, min = 0),
      
      hr(),
      h4("Calculated Rates"),
      verbatimTextOutput(ns("discount_rate_display")),
      verbatimTextOutput(ns("terminal_growth_rate_display"))
  )
}

dcfParamsServer <- function(input, output, session, app_state, state_prefix = "dcf") {
  ns <- session$ns
  
  # Helper to safely get parameter value
  get_param_value <- function(param_name, default_value) {
    param_sublist <- app_state[[state_prefix]]$parameters
    # Ensure reactivity is triggered if sublist is NULL initially
    if (is.null(param_sublist)) return(default_value)
    value <- param_sublist[[param_name]]
    # Use isTRUE to handle potential NA in is.numeric result safely
    if (is.null(value) || !isTRUE(is.numeric(value)) || isTRUE(is.na(value))) default_value else value
  }
  
  # Helper to set the modified flag only for the hurdle instance
  maybe_set_modified_flag <- function(param_name) {
    if (state_prefix == "hurdle_dcf") {
      print(paste("Setting hurdle modified flag TRUE for:", param_name))
      set_hurdle_param_modified(app_state, param_name, TRUE)
    }
  }
  
  # --- Observe inputs and update central state ---
  # --- Observe inputs and update central state ---
  observeEvent(input$risk_free_rate, {
    val <- input$risk_free_rate # Read input
    # Check if input is valid numeric before proceeding
    if (!is.null(val) && is.numeric(val) && !is.na(val)) {
      current_state_val <- get_param_value("risk_free_rate", NA) # Get current state value
      # Use identical for comparison to handle potential differences robustly
      if (!identical(val, current_state_val)) {
        app_state[[state_prefix]]$parameters$risk_free_rate <- val
        maybe_set_modified_flag("risk_free_rate")
      }
    } else {
      print(paste(state_prefix,"- risk_free_rate input is not a valid number:", val))
      # Optionally reset input to current state value if invalid?
      # updateNumericInput(session, "risk_free_rate", value = get_param_value("risk_free_rate", 0))
    }
  }, ignoreNULL = FALSE) # Important: ignoreNULL=FALSE to catch deletion
  
  observeEvent(input$equity_premium, {
    val <- input$equity_premium
    if (!is.null(val) && is.numeric(val) && !is.na(val)) {
      current_state_val <- get_param_value("equity_premium", NA)
      if (!identical(val, current_state_val)) {
        app_state[[state_prefix]]$parameters$equity_premium <- val
        maybe_set_modified_flag("equity_premium")
      }
    } else {
      print(paste(state_prefix,"- equity_premium input is not a valid number:", val))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$beta, {
    val <- input$beta
    if (!is.null(val) && is.numeric(val) && !is.na(val)) {
      current_state_val <- get_param_value("beta", NA)
      if (!identical(val, current_state_val)) {
        app_state[[state_prefix]]$parameters$beta <- val
        maybe_set_modified_flag("beta")
      }
    } else {
      print(paste(state_prefix,"- beta input is not a valid number:", val))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$country_premium, {
    val <- input$country_premium
    if (!is.null(val) && is.numeric(val) && !is.na(val)) {
      current_state_val <- get_param_value("country_premium", NA)
      if (!identical(val, current_state_val)) {
        app_state[[state_prefix]]$parameters$country_premium <- val
        maybe_set_modified_flag("country_premium")
      }
    } else {
      print(paste(state_prefix,"- country_premium input is not a valid number:", val))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$long_run_inflation, {
    val <- input$long_run_inflation
    if (!is.null(val) && is.numeric(val) && !is.na(val)) {
      current_state_val <- get_param_value("long_run_inflation", NA)
      if (!identical(val, current_state_val)) {
        app_state[[state_prefix]]$parameters$long_run_inflation <- val
        maybe_set_modified_flag("long_run_inflation")
      }
    } else {
      print(paste(state_prefix,"- long_run_inflation input is not a valid number:", val))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$additional_terminal_growth, {
    val <- input$additional_terminal_growth
    if (!is.null(val) && is.numeric(val) && !is.na(val)) {
      current_state_val <- get_param_value("additional_terminal_growth", NA)
      if (!identical(val, current_state_val)) {
        app_state[[state_prefix]]$parameters$additional_terminal_growth <- val
        maybe_set_modified_flag("additional_terminal_growth")
      }
    } else {
      print(paste(state_prefix,"- additional_terminal_growth input is not a valid number:", val))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$CAP, {
    # Keep previous CAP logic with validation
    cap_val_input <- input$CAP
    req(cap_val_input) # Require some input
    cap_val <- suppressWarnings(as.integer(cap_val_input)) # Try converting to integer
    current_cap_state <- get_param_value("CAP", NA) # Use NA default for comparison
    
    if (!is.na(cap_val) && cap_val >= 1) { # Check if integer conversion worked and value is valid
      if (!identical(cap_val, current_cap_state)) { # Use identical
        app_state[[state_prefix]]$parameters$CAP <- cap_val
        maybe_set_modified_flag("CAP")
        print(paste(state_prefix, "CAP state updated to:", cap_val))
      }
    } else {
      showNotification("Invalid CAP value entered. Must be an integer >= 1.", type="warning")
      # Revert UI only if state value is valid
      valid_state_cap <- get_param_value("CAP", NULL) # Get current state CAP or NULL
      if(!is.null(valid_state_cap)) {
        updateNumericInput(session, "CAP", value = valid_state_cap)
      }
    }
  }, ignoreNULL = FALSE) # Allow handling empty input
  
  observeEvent(input$market_cap, {
    # Keep previous market cap logic
    val <- input$market_cap
    if (state_prefix == "dcf") {
      if (!is.null(val) && is.numeric(val) && !is.na(val) && val >= 0) { # Added val >= 0 check
        current_state_val <- get_param_value("market_cap", NA)
        if (!identical(val, current_state_val)) {
          app_state$dcf$parameters$market_cap <- val
        }
      } else {
        print(paste(state_prefix,"- market_cap input is not a valid non-negative number:", val))
      }
    } else { shinyjs::disable("market_cap") }
  }, ignoreNULL = FALSE)
  
  # --- Calculate and display derived rates ---
  # Reactive expression for discount rate
  discount_rate_calc <- reactive({
    req(app_state[[state_prefix]], app_state[[state_prefix]]$parameters)
    req(app_state[[state_prefix]]$parameters$risk_free_rate, # Add specific reqs
        app_state[[state_prefix]]$parameters$equity_premium,
        app_state[[state_prefix]]$parameters$beta,
        app_state[[state_prefix]]$parameters$country_premium)
    
    params_for_calc <- NULL # Get params list robustly
    if(is.reactivevalues(app_state[[state_prefix]]$parameters)){
      params_for_calc <- tryCatch(reactiveValuesToList(app_state[[state_prefix]]$parameters), error = function(e) NULL)
    } else if (is.list(app_state[[state_prefix]]$parameters)) {
      params_for_calc <- app_state[[state_prefix]]$parameters
    }
    req(params_for_calc)
    
    # --- CALL GLOBAL FUNCTION ---
    calculate_discount_rate(params_for_calc) # Now calls function defined elsewhere
  })
  
  # Reactive expression for terminal growth rate
  terminal_growth_rate_calc <- reactive({
    req(app_state[[state_prefix]], app_state[[state_prefix]]$parameters)
    req(app_state[[state_prefix]]$parameters$long_run_inflation, # Add specific reqs
        app_state[[state_prefix]]$parameters$additional_terminal_growth)
    
    params_for_calc <- NULL # Get params list robustly
    if(is.reactivevalues(app_state[[state_prefix]]$parameters)){
      params_for_calc <- tryCatch(reactiveValuesToList(app_state[[state_prefix]]$parameters), error = function(e) NULL)
    } else if (is.list(app_state[[state_prefix]]$parameters)) {
      params_for_calc <- app_state[[state_prefix]]$parameters
    }
    req(params_for_calc)
    
    # --- CALL GLOBAL FUNCTION ---
    calculate_terminal_growth_rate(params_for_calc) # Now calls function defined elsewhere
  })
  
  output$discount_rate_display <- renderText({
    rate <- discount_rate_calc()
    # Add prefix to output
    paste0(toupper(state_prefix)," Disc Rate: ", format(rate * 100, nsmall = 2, digits=2), "%")
  })
  
  output$terminal_growth_rate_display <- renderText({
    rate <- terminal_growth_rate_calc()
    paste0(toupper(state_prefix), " Term Gr: ", format(rate * 100, nsmall = 2, digits=2), "%")
  })
  
  # --- Update module inputs if parameters change elsewhere (e.g., import or sync) ---
  observe({
    req(app_state[[state_prefix]], app_state[[state_prefix]]$parameters) # Depend on correct state slice
    params <- app_state[[state_prefix]]$parameters # Read from correct state slice
    
    # Check if parameters object is accessible and has elements
    param_names <- names(params)
    if(length(param_names) == 0) return()
    
    freezeReactiveValue(input, "risk_free_rate")
    if (isolate(input$risk_free_rate) != get_param_value("risk_free_rate", 0)) {
      updateNumericInput(session, "risk_free_rate", value = get_param_value("risk_free_rate", 0))
    }
    freezeReactiveValue(input, "equity_premium")
    if (isolate(input$equity_premium) != get_param_value("equity_premium", 0)) {
      updateNumericInput(session, "equity_premium", value = get_param_value("equity_premium", 0))
    }
    freezeReactiveValue(input, "beta")
    if (isolate(input$beta) != get_param_value("beta", 1)) {
      updateNumericInput(session, "beta", value = get_param_value("beta", 1))
    }
    freezeReactiveValue(input, "country_premium")
    if (isolate(input$country_premium) != get_param_value("country_premium", 0)) {
      updateNumericInput(session, "country_premium", value = get_param_value("country_premium", 0))
    }
    freezeReactiveValue(input, "long_run_inflation")
    if (isolate(input$long_run_inflation) != get_param_value("long_run_inflation", 2)) {
      updateNumericInput(session, "long_run_inflation", value = get_param_value("long_run_inflation", 2))
    }
    freezeReactiveValue(input, "additional_terminal_growth")
    if (isolate(input$additional_terminal_growth) != get_param_value("additional_terminal_growth", 0)) {
      updateNumericInput(session, "additional_terminal_growth", value = get_param_value("additional_terminal_growth", 0))
    }
    freezeReactiveValue(input, "CAP")
    if (isolate(input$CAP) != get_param_value("CAP", 10)) {
      updateNumericInput(session, "CAP", value = get_param_value("CAP", 10))
    }
    # Only update market cap UI if it's the main DCF module
    if (state_prefix == "dcf") {
      freezeReactiveValue(input, "market_cap")
      if (isolate(input$market_cap) != get_param_value("market_cap", 0)) {
        updateNumericInput(session, "market_cap", value = get_param_value("market_cap", 0))
      }
    } else {
      # Optionally disable market cap input for hurdle module
      shinyjs::disable("market_cap")
    }
    
  })
  
}
