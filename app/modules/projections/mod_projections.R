# --- DCF Projections Module (Main Content Area of DCF Tab) ---

# Helper function for fade UI rows (UPDATED with Start/End Period)
fadeRowUI <- function(ns, row_num, label, init_val, final_val, default_start = 1, default_end = 5) {
  div(
    id = ns(paste0("fade_row_", row_num)),
    style = "display: flex; margin-bottom: 5px; align-items: center; gap: 5px;", # Added gap
    div(style = "width: 20%; font-size: 90%;", label), # Reduced label width slightly
    div(style = "width: 10%;", numericInput(ns(paste0("fade_start_pd_", row_num)), NULL, value = default_start, min = 1, step = 1)),
    div(style = "width: 10%;", numericInput(ns(paste0("fade_end_pd_", row_num)), NULL, value = default_end, min = 1, step = 1)),
    div(style = "width: 15%;", numericInput(ns(paste0("fade_init_", row_num)), NULL, value = init_val, step = 0.1)),
    div(style = "width: 15%;", numericInput(ns(paste0("fade_final_", row_num)), NULL, value = final_val, step = 0.1)),
    div(
      style = "width: 20%;", # Adjusted width
      div(
        class = "fade-params", # Class for potential JS targeting if needed
        style = "display: flex; align-items: center;",
        selectInput(ns(paste0("fade_type_", row_num)), NULL,
                    choices = c("Linear" = "linear",
                                "Exponential" = "exponential",
                                "Geometric" = "geometric",
                                "Logistic" = "logistic"),
                    selected = "linear",
                    width = "120px") # Adjusted width slightly
        # Placeholder for dynamic shape/gradient inputs:
        # We will insert UI here using insertUI in the server logic
      )
    ),
    div(style = "width: 10%;", actionButton(ns(paste0("apply_fade_", row_num)), "Apply", class = "btn-sm btn-primary"))
  )
}


projectionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Assumptions Import/Export ---
    fluidRow(
      column(6, # Adjust width as needed
             fileInput(ns("import_assumptions"), "Import Assumptions (*.csv)",
                       accept = c(".csv"),
                       buttonLabel = "Browse...",
                       placeholder = "No file selected")
      ),
      column(6, align = "right", # Keep export button separate
             downloadButton(ns("export_assumptions"), "Export Assumptions")
      )
    ),
    hr(),
    
    # --- Auto-calculation Controls ---
    fluidRow(column(12, h4("Auto-calculation Controls"))),
    fluidRow(
      column(12,
             div(style = "margin-bottom: 15px; display: flex; gap: 10px;",
                 actionButton(ns("select_all_auto"), "Select All", class = "btn-info btn-sm"),
                 actionButton(ns("deselect_all_auto"), "Deselect All", class = "btn-secondary btn-sm"))
      )
    ),
    fluidRow(
      # Column 1
      column(6,
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_1"), "Revenue Growth", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_1"), "Years:", value=3, min=1, max=10, step=1))),
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_2"), "Operating Profit Margin", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_2"), "Years:", value=3, min=1, max=10, step=1))),
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_3"), "Interest Expense", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_3"), "Years:", value=3, min=1, max=10, step=1))),
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_4"), "Cash Tax Rate", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_4"), "Years:", value=3, min=1, max=10, step=1)))
      ),
      # Column 2
      column(6,
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_5"), "Depreciation & Amortisation", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_5"), "Years:", value=3, min=1, max=10, step=1))),
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_6"), "Incremental WC", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_6"), "Years:", value=3, min=1, max=10, step=1))),
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_7"), "Incremental FC", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_7"), "Years:", value=3, min=1, max=10, step=1))),
             div(style="display:flex; align-items:center; margin-bottom:8px;", div(style="width:180px;", checkboxInput(ns("auto_row_8"), "Net Debt Issuance", value=TRUE)), div(style="flex-grow:1;", numericInput(ns("calc_years_8"), "Years:", value=3, min=1, max=10, step=1)))
      )
    ),
    fluidRow(column(12, align="center", actionButton(ns("apply_auto_calc"), "Apply Auto-Calculations", class="btn-primary"))),
    hr(),
    
    # --- Fade Controls ---
    fluidRow(column(12, h4("Fade Assumptions"), p("Set subset periods (1 to CAP), initial/final values (%), and fade type."))),
    fluidRow(
      column(12,
             div(id = ns("fade_controls_container"), # Namespaced container
                 style = "margin-bottom: 15px;",
                 # UPDATED Header Row
                 div(style="display:flex; margin-bottom:10px; font-weight:bold; gap: 5px;",
                     div(style="width:20%;", "Assumption"),
                     div(style="width:10%; text-align: center;", "Start Pd"), # Centered
                     div(style="width:10%; text-align: center;", "End Pd"),   # Centered
                     div(style="width:15%; text-align: center;", "Initial (%)"),
                     div(style="width:15%; text-align: center;", "Final (%)"),
                     div(style="width:20%;", "Fade Type"),
                     div(style="width:10%;", "")),
                 # Use helper function to generate rows - providing defaults
                 fadeRowUI(ns, 1, "Revenue Growth", 5, 2, 1, 5), # Default end period is just a placeholder
                 fadeRowUI(ns, 2, "Operating Profit Margin", 15, 15, 1, 5),
                 fadeRowUI(ns, 3, "Interest Expense", 5, 5, 1, 5),
                 fadeRowUI(ns, 4, "Cash Tax Rate", 25, 25, 1, 5),
                 fadeRowUI(ns, 5, "Depreciation & Amortisation", 5, 5, 1, 5),
                 fadeRowUI(ns, 6, "Incremental WC", 10, 10, 1, 5),
                 fadeRowUI(ns, 7, "Incremental FC", 10, 10, 1, 5),
                 fadeRowUI(ns, 8, "Net Debt Issuance", 5, 5, 1, 5)
             )
      )
    ),
    fluidRow(column(12, align="center", actionButton(ns("apply_all_fades"), "Apply All Fades", class="btn-primary"))),
    hr(),
    
    # --- Assumption Visualization ---
    fluidRow(
      column(12,
             h4("Assumption Visualization"),
             selectInput(ns("assumption_to_plot"), "Select Assumption:", choices = NULL), # Choices set in server
             selectInput(ns("plot_type"), "Plot Type:",
                         choices = c("Data Points" = "points", "Connected Lines" = "lines",
                                     "Logarithmic Fit" = "logarithmic", "Exact Polynomial Fit" = "poly", "Smoothed Fit" = "smooth"),
                         selected = "lines"),
             plotOutput(ns("assumption_plot"))
      )
    ),
    hr(),
    
    # --- Assumptions Table ---
    fluidRow(
      column(12,
             h4("Annual Forecasting Assumptions"),
             div(style = "height: 300px; overflow-y: auto;", # Ensure scrollable container
                 rHandsontableOutput(ns("assumptions_table"))),
             br()
      )
    ),
    
    # --- Projections Area (moved below assumptions) ---
    # Separated into its own section at the bottom of the tab in app.R
    # This module focuses on managing the assumptions above the projection table
  )
}

projectionsServer <- function(input, output, session, app_state, state_prefix = "dcf") {
  ns <- session$ns
  
  # Helper function to get the correct assumptions data
  get_current_assumptions <- function() {
    if (state_prefix == "hurdle_dcf") get_hurdle_assumptions_data(app_state) else get_assumptions_data(app_state)
  }
  # Helper function to set the correct assumptions data
  set_current_assumptions <- function(value) {
    if (state_prefix == "hurdle_dcf") set_hurdle_assumptions_data(app_state, value) else set_assumptions_data(app_state, value)
  }
  # Helper function to get the correct fade modified flags
  get_current_fade_modified <- function() {
    if (state_prefix == "hurdle_dcf") get_hurdle_fade_inputs_modified(app_state) else get_fade_inputs_modified(app_state)
  }
  # Helper function to set the correct fade modified flag
  set_current_fade_modified <- function(index, value) {
    if (state_prefix == "hurdle_dcf") set_hurdle_fade_input_modified(app_state, index, value) else set_fade_input_modified(app_state, index, value)
  }
  
  # --- Observer to Synchronize Assumptions Table Columns with CAP State ---
  observe({
    # Access CAP from the correct state slice
    req(app_state[[state_prefix]]$parameters$CAP)
    target_cap <- as.integer(app_state[[state_prefix]]$parameters$CAP)
    req(target_cap >= 1) # Ensure target CAP is valid
    
    # Use helper to get assumptions for the current context
    current_assumptions <- get_current_assumptions()
    
    # Determine last historical year (consistent logic for both DCFs)
    std_data <- get_standardized_data(app_state) # Std data is common
    last_hist_year <- NULL
    if (!is.null(std_data)) {
      std_year_cols <- grep("^\\d{4}$", colnames(std_data), value = TRUE)
      if (length(std_year_cols) > 0) {
        last_hist_year <- max(as.numeric(std_year_cols))
      }
    }
    # Special handling for hurdle DCF last_hist_year (set explicitly)
    if (state_prefix == "hurdle_dcf" && !is.null(app_state$hurdle_dcf$last_hist_year)) {
      last_hist_year <- app_state$hurdle_dcf$last_hist_year
    } else if (is.null(last_hist_year) && !is.null(current_assumptions)) {
      # Fallback: Infer from existing assumption columns if std data missing
      assum_year_cols <- grep("^\\d{4}$", colnames(current_assumptions), value = TRUE)
      if (length(assum_year_cols) > 0) {
        first_proj_year_in_table <- min(as.numeric(assum_year_cols))
        last_hist_year <- first_proj_year_in_table - 1
      }
    }
    # Final fallback
    if (is.null(last_hist_year)) {
      last_hist_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
      print(paste(state_prefix,"Warning: Could not determine last historical year, defaulting base to", last_hist_year))
    }
    
    
    # Calculate target projection years based on the determined last_hist_year
    target_proj_years <- seq(from = last_hist_year + 1, length.out = target_cap)
    target_proj_year_cols <- as.character(target_proj_years)
    
    print(paste(state_prefix, "Target CAP:", target_cap))
    print(paste(state_prefix, "Last Hist Year:", last_hist_year))
    print(paste(state_prefix, "Target Proj Years:", paste(target_proj_year_cols, collapse=", ")))
    
    # Ensure current_assumptions is valid before proceeding
    if (is.null(current_assumptions) || !"Assumption" %in% colnames(current_assumptions)) {
      print(paste(state_prefix, "Assumptions data invalid or missing 'Assumption' column, cannot sync columns."))
      # Optionally recreate default structure if needed
      return()
    }
    
    current_proj_year_cols <- setdiff(colnames(current_assumptions), "Assumption")
    print(paste(state_prefix, "Current Assum Cols:", paste(current_proj_year_cols, collapse=", ")))
    
    # Check if adjustment is needed
    if (!identical(sort(current_proj_year_cols), sort(target_proj_year_cols))) { # Use sort for comparison robustness
      print(paste("Adjusting", state_prefix, "assumptions table columns to match target CAP..."))
      set_programmatic_update(app_state, TRUE) # Prevent table edit loops
      
      new_assumptions <- current_assumptions[, "Assumption", drop = FALSE] # Start with just the Assumption column
      
      # Add target columns, copying existing data or using defaults
      for (year_col in target_proj_year_cols) {
        if (year_col %in% current_proj_year_cols) {
          # Copy existing data if year matches
          new_assumptions[[year_col]] <- current_assumptions[[year_col]]
        } else {
          # Add new column with default value (e.g., 0.05 or last available value)
          last_available_year_col <- if(length(current_proj_year_cols) > 0) tail(sort(current_proj_year_cols), 1) else NULL # Use sorted last year
          default_value <- if (!is.null(last_available_year_col) && last_available_year_col %in% names(current_assumptions)) {
            current_assumptions[[last_available_year_col]] # Use last year's value
          } else {
            0.05 # Fallback default
          }
          # Ensure default is a vector of the correct length
          new_assumptions[[year_col]] <- rep(default_value, length.out = nrow(new_assumptions))
        }
      }
      # Ensure columns are in chronological order
      new_assumptions <- new_assumptions[, c("Assumption", target_proj_year_cols), drop=FALSE]
      
      
      # Use the correct setter based on prefix
      set_current_assumptions(new_assumptions)
      set_programmatic_update(app_state, FALSE)
      print(paste(state_prefix, "Assumptions table columns synchronized with CAP."))
    } else {
      print(paste(state_prefix, "Assumptions table columns already match target CAP. No change needed."))
    }
  }) # End CAP Synchronization Observer
  
  # --- Initialize Assumptions on Standardized Data Load (Only for Main DCF) ---
  observe({
    # This observer ONLY initializes the MAIN dcf assumptions when std data loads
    if (state_prefix == "dcf") {
      std_data <- get_standardized_data(app_state)
      req(std_data)
      
      current_assumptions <- get_assumptions_data(app_state) # Get main assumptions
      
      if (is.null(current_assumptions) || !"Assumption" %in% colnames(current_assumptions) || ncol(current_assumptions) <=1 ) {
        print("Initializing default MAIN DCF assumptions structure...")
        target_cap <- app_state$dcf$parameters$CAP %||% 10
        std_year_cols <- grep("^\\d{4}$", colnames(std_data), value = TRUE)
        last_hist_year <- if (length(std_year_cols) > 0) max(as.numeric(std_year_cols)) else as.numeric(format(Sys.Date(), "%Y")) - 1
        target_proj_years <- seq(from = last_hist_year + 1, length.out = target_cap)
        target_proj_year_cols <- as.character(target_proj_years)
        
        default_assumptions <- data.frame(
          Assumption = c("Revenue Growth", "Operating Profit Margin", "Interest Expense",
                         "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                         "Incremental FC", "Net Debt Issuance"),
          stringsAsFactors = FALSE, check.names = FALSE
        )
        for (year_col in target_proj_year_cols) { default_assumptions[[year_col]] <- 0.05 }
        
        set_programmatic_update(app_state, TRUE)
        set_assumptions_data(app_state, default_assumptions) # Set main assumptions
        set_programmatic_update(app_state, FALSE)
        print("Default MAIN DCF assumptions initialized.")
      }
    }
  }) # End Initialization Observer
  
  # --- Fix Corrupted Assumptions Observer ---
  observe({
    # This checks BOTH dcf and hurdle_dcf assumptions for corruption
    check_and_fix_assumptions <- function(prefix) {
      assumptions <- if (prefix == "hurdle_dcf") get_hurdle_assumptions_data(app_state) else get_assumptions_data(app_state)
      if (!is.null(assumptions) && ncol(assumptions) == 1 && "Message" %in% colnames(assumptions)) {
        print(paste("Detected corrupted assumptions for", prefix, ", fixing..."))
        
        target_cap <- app_state[[prefix]]$parameters$CAP %||% 10
        # Determine last hist year again for context
        std_data <- get_standardized_data(app_state)
        last_hist_year <- NULL
        if (!is.null(std_data)) { std_year_cols <- grep("^\\d{4}$", colnames(std_data), value = TRUE); if (length(std_year_cols) > 0) last_hist_year <- max(as.numeric(std_year_cols)) }
        if (prefix == "hurdle_dcf" && !is.null(app_state$hurdle_dcf$last_hist_year)) { last_hist_year <- app_state$hurdle_dcf$last_hist_year }
        if (is.null(last_hist_year)) { last_hist_year <- as.numeric(format(Sys.Date(), "%Y")) - 1 }
        
        target_proj_years <- seq(from = last_hist_year + 1, length.out = target_cap)
        target_proj_year_cols <- as.character(target_proj_years)
        
        fixed_assumptions <- data.frame(
          Assumption = c("Revenue Growth", "Operating Profit Margin", "Interest Expense",
                         "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                         "Incremental FC", "Net Debt Issuance"),
          stringsAsFactors = FALSE, check.names = FALSE
        )
        for (year_col in target_proj_year_cols) { fixed_assumptions[[year_col]] <- 0.05 }
        
        set_programmatic_update(app_state, TRUE)
        if (prefix == "hurdle_dcf") set_hurdle_assumptions_data(app_state, fixed_assumptions) else set_assumptions_data(app_state, fixed_assumptions)
        set_programmatic_update(app_state, FALSE)
        print(paste("Fixed", prefix, "assumptions data structure"))
      }
    }
    check_and_fix_assumptions("dcf")        # Check main DCF
    check_and_fix_assumptions("hurdle_dcf") # Check Hurdle DCF
  })
  
  # --- Assumptions Table Rendering (Uses Helper) ---
  output$assumptions_table <- renderRHandsontable({
    tryCatch({
      assumptions <- get_current_assumptions() # Use helper
      print(paste("DEBUGGING", toupper(state_prefix), "ASSUMPTIONS TABLE:"))
      print(paste("Assumptions is NULL:", is.null(assumptions)))
      if (!is.null(assumptions)) { print(paste("Dims:", nrow(assumptions), "x", ncol(assumptions))); print(paste("Cols:", paste(colnames(assumptions), collapse=", "))) }
      if (is.null(assumptions)) { stop("No assumptions data available") }
      if (ncol(assumptions) < 1) { stop("Assumptions data must have at least one column") }
      if (ncol(assumptions) == 1 && !"Message" %in% colnames(assumptions)) { stop("Assumptions data must have multiple columns (or be an error message)")}
      if (ncol(assumptions) > 1 && !"Assumption" %in% colnames(assumptions)) { stop("Missing 'Assumption' column") }
      
      display_assumptions <- assumptions
      if (ncol(display_assumptions) > 1 && "Assumption" %in% colnames(display_assumptions)){
        year_cols_display <- setdiff(colnames(display_assumptions), "Assumption")
        if(length(year_cols_display) > 0) {
          for(col in year_cols_display) {
            if(col %in% names(display_assumptions) && is.numeric(display_assumptions[[col]])) { display_assumptions[[col]] <- display_assumptions[[col]] * 100 }
          }
        }
      }
      
      result <- rhandsontable(display_assumptions, rowHeaders = FALSE, stretchH = "all", height = 300)
      if (ncol(display_assumptions) > 1 && "Assumption" %in% colnames(display_assumptions)){
        result <- result %>% hot_col("Assumption", readOnly = TRUE) %>% hot_cols(format = "0.00") %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) # Disable context menu edits for now
      } else { result <- result %>% hot_col(colnames(display_assumptions)[1], readOnly = TRUE) }
      
      print(paste("Successfully created rhandsontable for", state_prefix))
      return(result)
    }, error = function(e) {
      print(paste("ERROR in renderRHandsontable for", state_prefix, ":", e$message))
      error_df <- data.frame(Message = paste("Error:", e$message), stringsAsFactors = FALSE)
      return(rhandsontable(error_df) %>% hot_col("Message", readOnly = TRUE))
    })
  })
  
  
  # --- Assumptions Import / Export (Only for Main DCF instance) ---
  # We disable these controls for the hurdle instance
  observe({
    if(state_prefix == "hurdle_dcf") {
      shinyjs::disable("import_assumptions")
      shinyjs::disable("export_assumptions")
    } else {
      shinyjs::enable("import_assumptions")
      shinyjs::enable("export_assumptions")
    }
  })
  
  observeEvent(input$import_assumptions, {
    # This observer only runs in the main "dcf" instance due to the disable logic above
    req(input$import_assumptions, state_prefix == "dcf")
    tryCatch({
      file_content <- readLines(input$import_assumptions$datapath, warn = FALSE)
      parsed_data <- parse_imported_assumptions_file(file_content)
      if (!is.null(parsed_data$error)) {
        stop(parsed_data$error)
      }
      
      # --- Directly Apply Parsed Data ---
      assumptions_df <- parsed_data$assumptions_df
      params_list <- parsed_data$params_list
      
      set_programmatic_update(app_state, TRUE) # Prevent loops
      set_assumptions_data(app_state, assumptions_df) # Update assumptions
      
      # Update main DCF parameters ELEMENT BY ELEMENT
      print("Import Observer: Updating main DCF parameters...")
      if(!is.null(params_list)) { # Check if params were parsed
        for (param_name in names(params_list)) {
          # Check if the parameter exists in the app state structure
          if (param_name %in% names(app_state$dcf$parameters)) {
            # Update the reactive value directly
            app_state$dcf$parameters[[param_name]] <- params_list[[param_name]]
            print(paste("  Updated dcf param:", param_name, "to", params_list[[param_name]]))
          } else {
            print(paste("  Skipping unknown param from file:", param_name))
          }
        }
        # Special handling for param names that differ between import file and state
        if ("long_run_inflation_expectations" %in% names(params_list)) {
          app_state$dcf$parameters$long_run_inflation <- params_list$long_run_inflation_expectations
          print(paste("  Updated dcf param: long_run_inflation from import's long_run_inflation_expectations"))
        }
        # Update CAP separately if needed (though CAP observer handles sync now)
        if ("CAP" %in% names(params_list)) {
          app_state$dcf$parameters$CAP <- params_list$CAP
          print(paste("  Updated dcf param: CAP to", params_list$CAP))
        }
      } else {
        print("Import Observer: No parameters found in imported file.")
      }
      
      # Update base revenue & NOA state
      if(!is.null(params_list$base_revenue)) set_base_revenue(app_state, params_list$base_revenue)
      if(!is.null(params_list$non_operating_assets)) set_non_operating_assets(app_state, params_list$non_operating_assets)
      
      # Clear projections (they will auto-recalculate)
      set_projections_data(app_state, NULL)
      
      set_programmatic_update(app_state, FALSE) # Release lock
      
      showNotification("Imported assumptions applied to MAIN DCF.", type = "message")
      # --- End Directly Apply Parsed Data ---
      
    }, error = function(e) {
      showNotification(paste("Error processing import file:", e$message), type = "error", duration=8)
    })
  })
  
  output$export_assumptions <- downloadHandler(
    # This handler only activates for the main "dcf" instance
    filename = function() { paste("dcf_assumptions_", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "") },
    content = function(file) {
      assum_data <- get_assumptions_data(app_state)
      params_state <- reactiveValuesToList(app_state$dcf$parameters) # Get current params
      base_rev <- get_base_revenue(app_state)
      noa <- get_non_operating_assets(app_state)
      
      # Write assumptions table
      write.csv(assum_data, file, row.names = FALSE, na = "")
      
      # Separator
      write.table(data.frame(X1="--- DCF PARAMETERS BELOW ---"), file, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)
      
      # Parameters DF
      params_df_export <- data.frame(
        Parameter = c("risk_free_rate", "equity_premium", "beta", "country_premium",
                      "long_run_inflation_expectations", "additional_terminal_growth", # Match import names
                      "market_cap", "base_revenue", "non_operating_assets", "CAP"),
        Value = c(params_state$risk_free_rate, params_state$equity_premium, params_state$beta,
                  params_state$country_premium, params_state$long_run_inflation, # Use state name
                  params_state$additional_terminal_growth, params_state$market_cap,
                  base_rev, noa, params_state$CAP)
      )
      # Append parameters
      write.table(params_df_export, file, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
    }
  )
  
  # --- Auto-Calculation (Uses correct assumption getters/setters) ---
  observeEvent(input$select_all_auto, { lapply(1:8, function(i) updateCheckboxInput(session, paste0("auto_row_", i), value = TRUE)) })
  observeEvent(input$deselect_all_auto, { lapply(1:8, function(i) updateCheckboxInput(session, paste0("auto_row_", i), value = FALSE)) })
  
  observeEvent(input$apply_auto_calc, {
    std_data <- get_standardized_data(app_state)
    current_assumptions <- get_current_assumptions() # Use helper
    req(std_data, current_assumptions)
    
    auto_flags <- lapply(1:8, function(i) isTRUE(input[[paste0("auto_row_", i)]]))
    hist_years <- lapply(1:8, function(i) input[[paste0("calc_years_", i)]])
    
    modified_assumptions <- perform_assumption_auto_calc(
      assumptions_df = current_assumptions, standardized_data_df = std_data,
      auto_calc_flags = auto_flags, years_to_use = hist_years
    )
    
    set_programmatic_update(app_state, TRUE)
    set_current_assumptions(modified_assumptions) # Use helper
    set_programmatic_update(app_state, FALSE)
    
    showNotification(paste("Applied auto-calculations to", sum(unlist(auto_flags)), "rows for", state_prefix), type = "message")
  })
  
  
  # --- Fade Controls (Uses correct getters/setters) ---
  observe({ # Observer updating fade period inputs based on CAP
    current_cap <- app_state[[state_prefix]]$parameters$CAP
    req(current_cap, current_cap > 0)
    for (i in 1:8) {
      updateNumericInput(session, paste0("fade_start_pd_", i), max = current_cap)
      updateNumericInput(session, paste0("fade_end_pd_", i), max = current_cap)
      if (isolate(input[[paste0("fade_end_pd_", i)]]) > current_cap || is.null(isolate(input[[paste0("fade_end_pd_", i)]]))) { updateNumericInput(session, paste0("fade_end_pd_", i), value = current_cap) }
      current_start <- isolate(input[[paste0("fade_start_pd_", i)]]); current_end <- isolate(input[[paste0("fade_end_pd_", i)]]) %||% current_cap; if (!is.null(current_start) && (current_start > current_end || current_start > current_cap)) { updateNumericInput(session, paste0("fade_start_pd_", i), value = 1) }
    }
  })
  
  observe({ # Observer updating fade init/final based on assumptions
    current_assumptions <- get_current_assumptions() # Use helper
    req(current_assumptions)
    # Check for valid dataframe structure AFTER getting it
    if (is.null(current_assumptions) || ncol(current_assumptions) <= 1 || !"Assumption" %in% colnames(current_assumptions)) {
      print(paste(state_prefix,"Skipping fade init/final update - assumptions invalid"))
      return()
    }
    
    year_cols <- colnames(current_assumptions)[grepl("^\\d{4}$", colnames(current_assumptions))]
    if (length(year_cols) < 1) {
      print(paste(state_prefix,"Skipping fade init/final update - no year columns"))
      return()
    }
    
    # --- MODIFIED CHECK ---
    # Use isTRUE() for safe checking against NULL or non-TRUE values
    if (!isTRUE(is_programmatic_update(app_state))) {
      # --- END MODIFICATION ---
      
      modified_inputs <- get_current_fade_modified() # Use helper
      # Add check for modified_inputs validity
      if (is.null(modified_inputs) || !is.list(modified_inputs) || length(modified_inputs) != 8) {
        print(paste(state_prefix, "Fade modified inputs state is invalid."))
        return() # Exit if modified flags aren't ready
      }
      
      first_year <- year_cols[1]; last_year <- tail(year_cols, 1)
      
      for(i in 1:min(8, nrow(current_assumptions))) { # Ensure loop doesn't exceed actual rows
        # Use isTRUE for safe check against NULL/NA modified flag values
        if (!isTRUE(modified_inputs[[i]])) {
          first_val <- if (first_year %in% names(current_assumptions)) current_assumptions[i, first_year] * 100 else NA
          last_val <- if (last_year %in% names(current_assumptions)) current_assumptions[i, last_year] * 100 else NA
          if(is.numeric(first_val) && !is.na(first_val)) { updateNumericInput(session, paste0("fade_init_", i), value = first_val) }
          if(is.numeric(last_val) && !is.na(last_val)) { updateNumericInput(session, paste0("fade_final_", i), value = last_val) }
        }
      }
    } else {
      print(paste(state_prefix, "Skipping fade init/final update due to programmatic flag."))
    }
  })
  
  
  for(i in 1:8) { # Tracking manual changes
    local({
      local_i <- i
      observeEvent(input[[paste0("fade_init_", local_i)]], { set_current_fade_modified(local_i, TRUE) }, ignoreInit = TRUE) # Use helper
      observeEvent(input[[paste0("fade_final_", local_i)]], { set_current_fade_modified(local_i, TRUE) }, ignoreInit = TRUE) # Use helper
      observeEvent(input[[paste0("fade_start_pd_", local_i)]], { set_current_fade_modified(local_i, TRUE) }, ignoreInit = TRUE) # Use helper
      observeEvent(input[[paste0("fade_end_pd_", local_i)]], { set_current_fade_modified(local_i, TRUE) }, ignoreInit = TRUE) # Use helper
    })
  }
  
  # Update dynamic fade parameter UI
  for (i in 1:8) { local({ local_i <- i; observeEvent(input[[paste0("fade_type_", local_i)]], { updateFadeShapeParameter(local_i) }, ignoreInit = TRUE) }) }
  
  # Function to update dynamic UI for fade parameters (shape/gradient)
  updateFadeShapeParameter <- function(row_idx) {
    print(paste("Updating fade shape parameters UI for row", row_idx))
    
    tryCatch({
      # Get fade type
      fade_type_input_id <- paste0("fade_type_", row_idx)
      fade_type <- input[[fade_type_input_id]]
      req(fade_type) # Ensure fade_type is available
      print(paste("Fade type:", fade_type))
      
      # UI selectors
      shape_container_selector <- paste0("#", ns(paste0("shape_param_container_", row_idx)))
      fade_type_selector <- paste0("#", ns(fade_type_input_id))
      
      # Try to remove existing UI (safely in case it doesn't exist)
      removeUI(selector = shape_container_selector, immediate = TRUE) # Use immediate=TRUE
      
      # Add appropriate UI based on fade type
      if (fade_type %in% c("exponential", "geometric")) {
        default_value <- if(fade_type == "geometric") 2 else 1
        print(paste("Adding shape parameter UI with default value:", default_value))
        
        insertUI(
          selector = fade_type_selector, # Insert relative to the select input itself
          where = "afterEnd", # Place it after the select input
          ui = div(
            id = ns(paste0("shape_param_container_", row_idx)),
            style = "margin-left: 5px; display: inline-block; vertical-align: middle;", # Adjust style for better alignment
            numericInput(ns(paste0("shape_param_", row_idx)), "Shape:",
                         value = default_value, min = 0.1, step = 0.1, width = "80px") # Reduced width
          ),
          immediate = TRUE
        )
      } else if (fade_type == "logistic") {
        current_cap <- app_state$dcf$parameters$CAP %||% 5 # Get current CAP
        # Note: Logistic Inflection point input max should ideally be tied to CAP as well
        updateNumericInput(session, paste0("inflection_param_", row_idx), max = current_cap)
        
        beta_default <- floor((current_cap + 1) / 2) # Midpoint Year (1 to CAP)
        # Default alpha based on direction of initial/final values
        initial_val <- isolate(input[[paste0("fade_init_", row_idx)]]) %||% 0
        final_val <- isolate(input[[paste0("fade_final_", row_idx)]]) %||% 0
        alpha_default <- if(initial_val > final_val) 1 else -1 # Default steepness
        
        print(paste("Adding logistic parameters UI with alpha:", alpha_default, "beta default year:", beta_default))
        
        insertUI(
          selector = fade_type_selector,
          where = "afterEnd",
          ui = div(
            id = ns(paste0("shape_param_container_", row_idx)),
            style = "display: inline-flex; flex-direction: row; gap: 5px; margin-left: 5px; vertical-align: middle;", # Use flex row
            div(numericInput(ns(paste0("gradient_param_", row_idx)), "Gradient:",
                             value = alpha_default, step = 0.1, width = "80px")),
            div(numericInput(ns(paste0("inflection_param_", row_idx)), "Inflect. Yr:",
                             value = beta_default, min = 1, max = current_cap, step = 1, width = "80px")) # Max set to CAP
          ),
          immediate = TRUE
        )
      }
      
      print("Successfully updated fade shape parameters UI")
    }, error = function(e) {
      print(paste("Error updating fade shape parameters UI for row", row_idx, ":", e$message))
      # Optionally show a notification to the user if UI update fails critically
      # showNotification(paste("UI Error for row", row_idx), type="warning")
    })
  }
  
  
  # Observe fade type changes to update UI
  for (i in 1:8) {
    local({
      local_i <- i
      observeEvent(input[[paste0("fade_type_", local_i)]], {
        print(paste("Fade type changed for row", local_i))
        updateFadeShapeParameter(local_i)
      }, ignoreInit = TRUE) # Ignore initial run
    })
  }
  
  
  # --- Apply Fade (Individual Row Observers - DEFINED DIRECTLY IN LOOP) ---
  for (i in 1:8) {
    # Force evaluation of i for this specific observer instance
    local({
      row_idx <- i
      
      observeEvent(input[[paste0("apply_fade_", row_idx)]], {
        # --- START DEBUGGING ---
        print(paste0("--- apply_fade_", row_idx, " triggered ---"))
        tryCatch({
          current_cap <- app_state[[state_prefix]]$parameters$CAP # Use prefix
          current_assumptions <- get_current_assumptions() # Use helper
          req(current_cap, current_assumptions)
          if (is.null(current_assumptions)) return()
          
          cap_value <- current_cap # Use the fetched CAP
          assumption_name <- current_assumptions$Assumption[row_idx]
          year_cols <- names(current_assumptions)[grepl("^\\d{4}$", names(current_assumptions))]
          total_periods_in_data <- length(year_cols) # Total years available in assumptions
          print(paste("Total Periods in Data:", total_periods_in_data))
          print(paste("Year Cols:", paste(year_cols, collapse=", ")))
          
          if (total_periods_in_data == 0) {
            stop("No year columns found in assumptions data")
          }
          
          current_values_vector <- try(as.numeric(current_assumptions[row_idx, year_cols]), silent = TRUE)
          print(paste("Current Values:", if(inherits(current_values_vector, "try-error")) "Error reading" else paste(round(current_values_vector, 4), collapse=", ")))
          
          # Read subset period inputs
          start_pd <- input[[paste0("fade_start_pd_", row_idx)]]; end_pd <- input[[paste0("fade_end_pd_", row_idx)]]
          initial_val_pct <- input[[paste0("fade_init_", row_idx)]]; final_val_pct <- input[[paste0("fade_final_", row_idx)]]
          fade_type <- input[[paste0("fade_type_", row_idx)]]
          assumption_name <- current_assumptions$Assumption[row_idx]
          year_cols <- names(current_assumptions)[grepl("^\\d{4}$", names(current_assumptions))]
          total_periods_in_data <- length(year_cols)
          current_values_vector <- as.numeric(current_assumptions[row_idx, year_cols])
          
          # --- Validation ---
          req(start_pd, end_pd, initial_val_pct, final_val_pct, fade_type) # Basic non-null check
          if (!is.numeric(start_pd) || !is.numeric(end_pd) || !is.numeric(initial_val_pct) || !is.numeric(final_val_pct)) {
            stop("Fade input values must be numeric.")
          }
          if (start_pd < 1 || end_pd < 1 || start_pd > total_periods_in_data || end_pd > total_periods_in_data) {
            stop(paste0("Start/End periods (", start_pd, "-", end_pd, ") must be between 1 and ", total_periods_in_data, "."))
          }
          if (start_pd > end_pd) {
            stop("Start period cannot be after end period.")
          }
          if (!fade_type %in% c("linear", "exponential", "geometric", "logistic")) {
            stop(paste("Invalid fade type:", fade_type))
          }
          if (inherits(current_values_vector, "try-error")){
            stop("Error reading current values vector from assumptions data")
          }
          
          # --- Get Fade Parameters ---
          fade_params <- list() # ... (logic to get fade params m, alpha, inflection_year) ...
          if (fade_type %in% c("exponential", "geometric")) { shape_param_raw <- input[[paste0("shape_param_", row_idx)]]; if (!is.null(shape_param_raw) && is.numeric(shape_param_raw) && !is.na(shape_param_raw)) { fade_params$m <- shape_param_raw } else { fade_params$m <- if(fade_type == "geometric") 2 else 1 }}
          else if (fade_type == "logistic") { gradient_param_raw <- input[[paste0("gradient_param_", row_idx)]]; inflection_param_raw <- input[[paste0("inflection_param_", row_idx)]]; if (!is.null(gradient_param_raw) && is.numeric(gradient_param_raw) && !is.na(gradient_param_raw)) { fade_params$alpha <- gradient_param_raw } else { fade_params$alpha <- if(!is.null(initial_val_pct) && !is.null(final_val_pct) && initial_val_pct > final_val_pct) 1 else -1 }; if (!is.null(inflection_param_raw) && is.numeric(inflection_param_raw) && !is.na(inflection_param_raw) && inflection_param_raw >= 1 && inflection_param_raw <= total_periods_in_data) { fade_params$inflection_year = inflection_param_raw } else { fade_params$inflection_year = floor((total_periods_in_data + 1) / 2) } }
          
          faded_values_dec <- apply_assumption_fade(
            values_vector = current_values_vector, start_period = start_pd, end_period = end_pd,
            initial_value = initial_val_pct, final_value = final_val_pct, fade_type = fade_type, fade_params = fade_params
          )
          
          if (!is.null(faded_values_dec) && length(faded_values_dec) == length(year_cols)) {
            set_programmatic_update(app_state, TRUE)
            modified_assumptions <- get_current_assumptions() # Use helper
            modified_assumptions[row_idx, year_cols] <- faded_values_dec
            set_current_assumptions(modified_assumptions) # Use helper
            set_programmatic_update(app_state, FALSE)
            set_current_fade_modified(row_idx, FALSE) # Use helper
            showNotification(paste("Applied fade to", assumption_name, "for", state_prefix), type = "message")
          } else { stop("Fade calculation failed") }
        }, error = function(e) { showNotification(paste("Fade Error:", e$message), type = "error") })
      })
    })
  }
  
  
  # Apply All Fades (UPDATED)
  observeEvent(input$apply_all_fades, {
    current_assumptions <- get_current_assumptions() # Use helper
    req(current_assumptions)
    total_periods_in_data <- length(colnames(current_assumptions)[grepl("^\\d{4}$", colnames(current_assumptions))])
    if (total_periods_in_data == 0) { showNotification("No year columns found.", type="error"); return() }
    
    year_cols <- names(current_assumptions)[grepl("^\\d{4}$", names(current_assumptions))]
    assumptions_modified <- current_assumptions
    errors_occurred <- FALSE
    
    set_programmatic_update(app_state, TRUE)
    
    for (i in 1:8) {
      tryCatch({
        # Read inputs for this row
        start_pd <- input[[paste0("fade_start_pd_", i)]]
        end_pd <- input[[paste0("fade_end_pd_", i)]]
        initial_val_pct <- input[[paste0("fade_init_", i)]]
        final_val_pct <- input[[paste0("fade_final_", i)]]
        fade_type <- input[[paste0("fade_type_", i)]]
        
        # Validation for this row's inputs
        req(start_pd, end_pd, initial_val_pct, final_val_pct, fade_type)
        
        if (!is.numeric(start_pd) || !is.numeric(end_pd) || !is.numeric(initial_val_pct) || !is.numeric(final_val_pct)) {
          stop("Fade input values must be numeric.")
        }
        if (start_pd < 1 || end_pd < 1 || start_pd > total_periods_in_data || end_pd > total_periods_in_data) {
          stop(paste0("Start/End periods (", start_pd, "-", end_pd, ") must be between 1 and ", total_periods_in_data, "."))
        }
        if (start_pd > end_pd) {
          stop("Start period cannot be after end period.")
        }
        if (!fade_type %in% c("linear", "exponential", "geometric", "logistic")) {
          stop(paste("Invalid fade type:", fade_type))
        }
        
        # Get current values vector for the row
        current_values_vector <- as.numeric(assumptions_modified[i, year_cols])
        
        # Get fade parameters
        fade_params <- list()
        if (fade_type %in% c("exponential", "geometric")) {
          shape_param_raw <- input[[paste0("shape_param_", i)]]
          if (!is.null(shape_param_raw) && is.numeric(shape_param_raw) && !is.na(shape_param_raw)) { fade_params$m <- shape_param_raw } else { fade_params$m <- if(fade_type == "geometric") 2 else 1 }
        } else if (fade_type == "logistic") {
          gradient_param_raw <- input[[paste0("gradient_param_", i)]]
          inflection_param_raw <- input[[paste0("inflection_param_", i)]]
          if (!is.null(gradient_param_raw) && is.numeric(gradient_param_raw) && !is.na(gradient_param_raw)) { fade_params$alpha <- gradient_param_raw } else { fade_params$alpha <- if(initial_val_pct > final_val_pct) 1 else -1 }
          if (!is.null(inflection_param_raw) && is.numeric(inflection_param_raw) && !is.na(inflection_param_raw) && inflection_param_raw >= 1 && inflection_param_raw <= total_periods_in_data) { fade_params$inflection_year <- inflection_param_raw } else { fade_params$inflection_year <- floor((total_periods_in_data + 1) / 2) }
        }
        
        # Call modified fade function
        faded_values_dec <- apply_assumption_fade(
          values_vector = current_values_vector,
          start_period = start_pd,
          end_period = end_pd,
          initial_value = initial_val_pct,
          final_value = final_val_pct,
          fade_type = fade_type,
          fade_params = fade_params
        )
        
        # Update the row in the modified dataframe if successful
        if (!is.null(faded_values_dec) && length(faded_values_dec) == length(year_cols)) {
          assumptions_modified[i, year_cols] <- faded_values_dec
          set_fade_input_modified(app_state, i, FALSE) # Reset flag
        } else {
          stop(paste("Fade calculation failed or returned incorrect length.")) # Error for this row
        }
      }, error = function(e) {
        # Log error for this specific row and set flag
        error_msg <- paste("Error applying fade for row", i, "(", assumptions_modified$Assumption[i], ") in 'Apply All':", e$message)
        print(error_msg)
        showNotification(error_msg, type="error", duration=5)
        errors_occurred <<- TRUE # Modify flag in parent environment
      }) # End tryCatch for row i
    } # End for loop
    
    # Update state only if no errors occurred OR decide if partial update is okay
    if (!errors_occurred) {
      set_assumptions_data(app_state, assumptions_modified)
      showNotification("Applied fades to all assumptions", type = "message")
    } else {
      showNotification("Errors occurred during 'Apply All Fades'. Some rows may not be updated. Check console.", type = "warning", duration=8)
      # Optionally revert: set_assumptions_data(app_state, current_assumptions)
      # Or allow partial update by still calling set_assumptions_data(app_state, assumptions_modified)
      # Current behaviour allows partial update if desired. Let's keep it simple for now.
      set_assumptions_data(app_state, assumptions_modified) # Apply partial updates
    }
    
    set_programmatic_update(app_state, FALSE) # Release lock regardless of errors
    
    # Trigger projections? (Optional)
    # if (get_base_revenue(app_state) > 0) { shinyjs::delay(50, shinyjs::click(ns_main("generate_projections"))) }
  })
  
  
  # --- Assumption Visualization ---
  observe({
    # Update choices when assumptions data changes
    assumptions <- get_current_assumptions() # Use helper
    if (is.data.frame(assumptions) && "Assumption" %in% colnames(assumptions)) { updateSelectInput(session, "assumption_to_plot", choices = assumptions$Assumption) } else { updateSelectInput(session, "assumption_to_plot", choices = c("No data" = "")) }
  })
  
  
  output$assumption_plot <- renderPlot({
    req(input$assumption_to_plot) # Require a selection
    assumptions_data <- get_current_assumptions() # Use helper
    plot_type <- input$plot_type %||% "lines" # Default plot type
    
    # Validate assumptions data before proceeding
    if (is.null(assumptions_data) || !is.data.frame(assumptions_data) || !"Assumption" %in% colnames(assumptions_data) || nrow(assumptions_data) == 0) {
      plot.new(); title("No assumptions data available to plot")
      return()
    }
    # Ensure selected assumption exists in the data
    if (!input$assumption_to_plot %in% assumptions_data$Assumption) {
      plot.new(); title(paste("Selected assumption '", input$assumption_to_plot, "' not found."))
      return()
    }
    
    
    plot_data <- prepare_assumption_plot_data(assumptions_data, input$assumption_to_plot) # Use utility
    
    if (!is.null(plot_data) && nrow(plot_data) > 0) {
      p <- ggplot(plot_data, aes(x = Year, y = Value))
      
      # Add geom based on type
      if(plot_type == "points") {
        p <- p + geom_point()
      } else if (plot_type == "lines") {
        p <- p + geom_line() + geom_point()
      } else if (plot_type == "smooth" && nrow(plot_data) > 2) {
        p <- p + geom_point() + geom_smooth(method = "loess", se = FALSE, color = "red", span=0.75)
      } else if (plot_type == "poly") {
        # --- START: Exact Interpolation using Splines (for Single Assumption) ---
        plot_data_filtered <- plot_data %>% filter(!is.na(Year) & !is.na(Value))
        n_points = n_distinct(plot_data_filtered$Year)
        
        if (n_points >= 2) {
          data_sorted = arrange(plot_data_filtered, Year)
          interpolation_line_data <- tryCatch({
            interp_func <- splinefun(data_sorted$Year, data_sorted$Value, method = "natural")
            years_seq = seq(min(data_sorted$Year), max(data_sorted$Year), length.out = 100)
            predicted_values = interp_func(years_seq)
            data.frame(Year = years_seq, PredictedValue = predicted_values)
          }, error = function(e) {
            warning(paste("Spline interpolation failed for assumption plot:", e$message))
            NULL # Return NULL if spline fails
          })
          
          p <- p + geom_point() # Show original points
          if (!is.null(interpolation_line_data)) {
            p <- p + geom_line(data = interpolation_line_data, aes(y = PredictedValue), color = "blue")
          }
          
        } else {
          # If less than 2 points, just show points
          p <- p + geom_point()
        }
        # --- END: Exact Interpolation using Splines ---
        
      } else if (plot_type == "logarithmic" && nrow(plot_data) > 1) {
        # Placeholder for potential log fit implementation if needed later
        p <- p + geom_point() + labs(caption = "Logarithmic fit not implemented.")
      } else {
        # Default to points if other conditions fail
        p <- p + geom_point()
      }
      
      # --- Apply Common Formatting (Titles, Axes) ---
      p <- p + labs(title = paste("Assumption:", input$assumption_to_plot),
                    x = "Year", y = "Value") +
        theme_minimal() +
        # Bold Title
        theme(plot.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) # Keep angled X labels
      
      # --- Conditional Y-axis Formatting for Assumption Plot ---
      # Check if all non-NA values in the relevant range are effectively integers
      y_values_in_range <- plot_data$Value[!is.na(plot_data$Value)]
      # Check if they are percentages (decimals between -1 and 1 perhaps?) - adjust logic if needed
      # Let's format as percent always for assumptions
      p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
      
      # --- End Conditional Y-axis Formatting ---
      
      print(p) # Render the plot
      
    } else {
      plot.new(); title(paste("No plottable data for assumption:", input$assumption_to_plot))
    }
  })
  
  
  # --- Observe Table Edits ---
  observeEvent(input$assumptions_table, {
    # Check if the update comes from user interaction and not programmatic update
    if (!is.null(input$assumptions_table) && !is_programmatic_update(app_state)) {
      print("User edit detected in assumptions table")
      tryCatch({
        updated_assumptions_pct <- hot_to_r(input$assumptions_table)
        
        # Basic validation of the edited data
        if (!is.data.frame(updated_assumptions_pct) || !"Assumption" %in% colnames(updated_assumptions_pct)) {
          stop("Edited data is not valid (missing Assumption column or not a data frame).")
        }
        
        # Convert percentages back to decimals
        updated_assumptions_dec <- updated_assumptions_pct
        year_cols_edit <- setdiff(colnames(updated_assumptions_dec), "Assumption")
        
        for (col in year_cols_edit) {
          # Ensure column exists and attempt numeric conversion robustly
          if (col %in% names(updated_assumptions_dec)) {
            # Try converting, handle potential errors gracefully
            numeric_col <- suppressWarnings(as.numeric(updated_assumptions_dec[[col]]))
            # Only divide by 100 if conversion was successful and resulted in numbers
            if (all(!is.na(numeric_col))) {
              updated_assumptions_dec[[col]] <- numeric_col / 100
            } else {
              warning(paste("Non-numeric data found in column", col, "during edit conversion. Keeping original values."))
              # Optionally, revert the column or handle NAs specifically
              updated_assumptions_dec[[col]] <- updated_assumptions_pct[[col]] # Keep original if conversion fails
            }
          }
        }
        
        print("Setting assumptions data from user edit")
        # Update the central state
        # No need for set_programmatic_update here as this IS the user interaction
        set_assumptions_data(app_state, updated_assumptions_dec)
        
        # Trigger projection recalc (if desired - need generate button ID from main app)
        # if (get_base_revenue(app_state) > 0) { shinyjs::delay(50, shinyjs::click(ns_main("generate_projections"))) }
      }, error = function(e) {
        error_msg <- paste("Error processing assumptions table edit:", e$message)
        print(error_msg)
        showNotification(error_msg, type="error", duration=8)
        # Optionally, force a re-render of the table to revert the user's invalid edit
        # This requires careful state management to avoid loops
      })
    } else if (is_programmatic_update(app_state)) {
      print("Skipping assumptions table observer due to programmatic update flag.")
    }
  })
  
  
} # End projectionsServer