# --- Formula Builder Module ---

formulaBuilderUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             selectInput(ns("metric_type"), "Select Metric:",
                         choices = c("Select a metric type" = "",
                                     "Revenue" = "revenue",
                                     "Operating Profit" = "operating_profit",
                                     "Interest Expense" = "interest_expense",
                                     "Tax" = "tax",
                                     "Net Income" = "net_income",
                                     "Working Capital Excluding Cash and Cash Equivalents" = "working_capital_investment",
                                     "Depreciation & Amortisation" = "depreciation_and_amortisation",
                                     "Fixed Capital" = "fixed_capital_investment",
                                     "Net Debt Issuance" = "net_debt_issuance",
                                     "Non-Operating Assets" = "non_operating_assets"),
                         selected = "revenue" 
             )
      ),
    ),
    fluidRow(
      column(12,
             h4("Series Selection & Operations"),
             p("Build your calculation by selecting components and operations")
      )
    ),
    fluidRow(
      column(12,
             div(id = ns("formula_builder_container"), # Use namespaced container ID
                 div(
                   id = ns("formula_components"),
                   style = "margin-bottom: 15px; padding: 10px; border: 1px solid #eee; border-radius: 5px;",
                   h5("Formula Components"),
                   div(id = ns("components_container")) # Container for dynamic UI
                 ),
                 actionButton(ns("add_component_btn"), "Add Component", class = "btn-info"),
                 actionButton(ns("clear_components_btn"), "Clear All Components", class = "btn-warning"),
                 hr(),
                 verbatimTextOutput(ns("formula_preview"))
             )
      )
    ),
    fluidRow(
      column(12,
             actionButton(ns("add_metric_btn"), "Add Metric to Dataset", class = "btn-success")
      )
    )
  )
}

# File: modules/formula_builder/mod_formula_builder.R

formulaBuilderServer <- function(input, output, session, app_state) {
  ns <- session$ns
  
  # --- State Tracking ---
  add_initial_component_trigger <- reactiveVal(0)
  # Track the highest suffix ID ever used in this session to limit input checks
  max_suffix_used <- reactiveVal(0)
  
  # --- Helper: Remove Component UI and State ---
  remove_component_ui_and_state <- function(suffix) {
    print(paste("Removing component with suffix:", suffix))
    component_div_id <- ns(paste0("component_", suffix)) # ID of the div wrapping the inputs for this suffix
    # Remove UI element
    shinyjs::runjs(sprintf(
      "var el = $('#%s'); if (el.length) { Shiny.unbindAll(el[0]); el.remove(); }",
      component_div_id
    ))
    # Remove suffix from central state tracker
    remove_component_suffix(app_state, suffix)
  }
  
  # --- Helper: Clear All Components ---
  clear_all_components <- function() {
    print("Clearing all components")
    suffixes_to_remove <- isolate(get_components_suffixes(app_state))
    if (length(suffixes_to_remove) > 0) {
      lapply(suffixes_to_remove, remove_component_ui_and_state)
    }
    # Ensure state is fully reset
    reset_components(app_state)
    # Reset max suffix tracker as well? Maybe not, let it grow.
  }
  
  
  # --- Helper: Add Single Component UI ---
  add_single_component_ui_direct <- function() {
    data_df <- isolate(get_selected_data(app_state))
    if(is.null(data_df) || nrow(data_df) == 0) return()
    series_names <- unique(data_df[[1]]); series_names <- series_names[!is.na(series_names)]
    if(length(series_names) == 0) return()
    
    current_suffixes <- isolate(get_components_suffixes(app_state)) # Use getter
    is_first_component_in_build <- length(current_suffixes) == 0
    
    # --- Corrected suffix calculation ---
    new_id_suffix <- if (is_first_component_in_build) {
      1
    } else {
      # max(numeric(0)) is -Inf, max(numeric_vector) works. Add 0 for safety.
      max(0, current_suffixes) + 1
    }
    # --- End Correction ---
    
    # Update max suffix tracker
    if (new_id_suffix > isolate(max_suffix_used())) {
      max_suffix_used(new_id_suffix)
    }
    
    # --- Determine Default Selection ---
    default_selection <- ""
    if (length(series_names) > 0) {
      if (is_first_component_in_build) {
        last_used <- isolate(get_last_used_component(app_state))
        last_used_index <- match(last_used, series_names)
        if (!is.null(last_used) && last_used != "" && !is.na(last_used_index) && last_used_index < length(series_names)) {
          default_selection <- series_names[last_used_index + 1]
        } else { default_selection <- if (length(series_names) >= 2) series_names[2] else series_names[1] }
      } else {
        last_suffix <- new_id_suffix - 1 # Should be the max of current_suffixes
        if (last_suffix > 0) {
          prev_input_id <- paste0("component_series_", last_suffix)
          prev_selected_series <- isolate(input[[prev_input_id]])
          if (!is.null(prev_selected_series) && prev_selected_series != "") {
            prev_index <- match(prev_selected_series, series_names)
            if (!is.na(prev_index) && prev_index < length(series_names)) {
              default_selection <- series_names[prev_index + 1]
            } else { 
              default_selection <- series_names[1] 
            }
          } else { default_selection <- series_names[1] }
        } else { default_selection <- series_names[1] }
      }
    }
    # --- End Default Selection ---
    
    # --- Determine Default Operation ---
    metric_type_input <- isolate(input$metric_type %||% "revenue")
    default_operation <- if(metric_type_input %in% c("depreciation_and_amortisation", "interest_expense")) "subtract" else "add"
    # --- End Default Operation ---
    
    print(paste("Adding UI for component suffix:", new_id_suffix, "Default series:", default_selection))
    component_div_id <- ns(paste0("component_", new_id_suffix)) # ID for the wrapping div
    
    # Insert UI directly
    insertUI(
      selector = paste0("#", ns("components_container")),
      ui = div(
        id = component_div_id, # Use suffix-based ID for the wrapping div
        style = "display: flex; margin-bottom: 10px; align-items: center;",
        div(style = "flex: 4;",
            selectInput(ns(paste0("component_series_", new_id_suffix)), NULL,
                        choices = c("Select a series" = "", series_names),
                        selected = default_selection, width = "100%",
                        selectize = FALSE # Keep selectize FALSE from prev attempts, add JS init below
            )
        ),
        div(style = "flex: 1; margin: 0 10px;",
            selectInput(ns(paste0("component_op_", new_id_suffix)), NULL,
                        choices = c("+" = "add", "-" = "subtract"),
                        selected = default_operation, width = "100%")
        ),
        div(style = "flex: 1;",
            numericInput(ns(paste0("component_factor_", new_id_suffix)), NULL,
                         value = 1, width = "100%")
        ),
        div(style = "margin-left: 10px;",
            # Button ID tied to suffix
            actionButton(ns(paste0("remove_component_", new_id_suffix)), NULL,
                         icon = icon("times"), class = "btn-danger btn-sm")
        )
      )
    )
    
    # --- Add JS Initialization for Selectize ---
    selectize_input_id_jq <- paste0("#", ns(paste0("component_series_", new_id_suffix)))
    print(paste("Attempting JS init for:", selectize_input_id_jq))
    shinyjs::delay(100, {
      shinyjs::runjs(sprintf(
        "var el = $('%s'); \n if (el.length && !el.hasClass('selectized')) { \n   console.log('Initializing selectize for %s'); \n   el.selectize({ create: false }); \n   if (el.selectize && el.selectize()[0].selectize) { \n      el.selectize()[0].selectize.setValue('%s', true); \n      console.log('Set value for %s'); \n   } else { console.error('Selectize object not found after init attempt for %s'); } \n } else if (el.length && el.selectize && el.selectize()[0].selectize) { \n   el.selectize()[0].selectize.setValue('%s', true); \n   console.log('Set value for already selectized %s'); \n } else { console.error('Could not find or initialize selectize for %s'); }",
        selectize_input_id_jq, selectize_input_id_jq, default_selection,
        selectize_input_id_jq, selectize_input_id_jq, default_selection,
        selectize_input_id_jq, selectize_input_id_jq
      ))
    })
    # --- END JS ---
    
    # Register component suffix in state
    add_component_suffix(app_state, new_id_suffix)
    
    # --- Setup Remove Observer ---
    # Define the observer in the main server context
    observeEvent(input[[paste0("remove_component_", new_id_suffix)]], {
      remove_component_ui_and_state(new_id_suffix)
    }, ignoreInit = TRUE, once = TRUE, autoDestroy = TRUE, domain = session)
  }
  
  
  # --- Observer to DETECT Need for Initial/Reset Component ---
  observe({
    req(get_selected_data(app_state))
    component_suffixes <- get_components_suffixes(app_state) # Use renamed getter
    if (length(component_suffixes) == 0) {
      print("Detected empty component list, incrementing trigger.")
      add_initial_component_trigger(isolate(add_initial_component_trigger()) + 1)
    }
  })
  
  
  # --- Observer to ACTUALLY Add the Initial/Reset Component ---
  observeEvent(add_initial_component_trigger(), {
    trigger_value <- add_initial_component_trigger()
    if (trigger_value > 0) {
      if (length(isolate(get_components_suffixes(app_state))) == 0) { # Use renamed getter
        print(paste("Trigger Observer: Calling add_single_component_ui_direct (Trigger:", trigger_value, ")"))
        add_single_component_ui_direct()
      } else { 
        print(paste("Trigger Observer: Skipped add, components no longer empty (Trigger:", trigger_value, ")"))
      }
    }
  }, ignoreInit = TRUE)
  
  
  # --- Button Observers ---
  observeEvent(input$add_component_btn, {
    print("Add component button clicked")
    add_single_component_ui_direct()
  })
  
  observeEvent(input$clear_components_btn, {
    print("Clear components button clicked")
    clear_all_components()
    # Trigger observer will add one back
  })
  
  observeEvent(input$add_metric_btn, {
    req(input$metric_type)
    metric_type_selected <- input$metric_type
    if (metric_type_selected == "") { showNotification("Please select a metric type", type = "error"); return() }
    metric_name <- switch(metric_type_selected, 
                          "revenue" = "Revenue",
                          "operating_profit" = "Operating Profit",
                          "interest_expense" = "Interest Expense",
                          "tax" = "Tax",
                          "net_income" = "Net Income",
                          "working_capital_investment" = "Working Capital Excluding Cash and Cash Equivalents",
                          "depreciation_and_amortisation" = "Depreciation & Amortisation",
                          "fixed_capital_investment" = "Fixed Capital",
                          "net_debt_issuance" = "Net Debt Issuance",
                          "non_operating_assets" = "Non-Operating Assets",
                          metric_type_selected)
    
    # Build components list by reading inputs based on state suffixes
    component_suffixes <- isolate(get_components_suffixes(app_state)) # Use renamed getter
    
    metric_components_def <- list()
    valid_component_found <- FALSE
    last_series_in_definition <- ""
    
    if (length(component_suffixes) > 0) {
      print("--- Reading component inputs (add_metric_btn) ---")
      for (suffix in component_suffixes) { # Iterate through suffixes stored in state
        component_id <- paste0("component_series_", suffix)
        series_input <- isolate(input[[component_id]])
        
        print(paste(" > Reading suffix:", suffix, "ID:", component_id, "Value:", series_input))
        
        if (!is.null(series_input) && series_input != "") {
          op_input <- isolate(input[[paste0("component_op_", suffix)]])
          factor_input <- isolate(input[[paste0("component_factor_", suffix)]])
          metric_components_def[[length(metric_components_def) + 1]] <- list(
            series = series_input,
            operation = op_input %||% "add",
            factor = factor_input %||% 1
          )
          valid_component_found <- TRUE
          last_series_in_definition <- series_input
        }
      }
      print("--- Finished reading inputs ---")
    }
    
    if (!valid_component_found) { showNotification("Please select a series for at least one component", type = "error"); return() }
    
    if (last_series_in_definition != "") { set_last_used_component(app_state, last_series_in_definition) }
    add_defined_metric(app_state, metric_name, list(components = metric_components_def, type = metric_type_selected))
    showNotification(paste("Metric", metric_name, "added/updated"), type = "message")
    
    # --- Reset UI ---
    clear_all_components()
    # Trigger observer will add one back.
    
    # --- Cycle Metric Dropdown ---
    metric_choices_list <- c("Select a metric type" = "",
                             "Revenue" = "revenue",
                             "Operating Profit" = "operating_profit",
                             "Interest Expense" = "interest_expense",
                             "Tax" = "tax",
                             "Net Income" = "net_income",
                             "Working Capital Excluding Cash and Cash Equivalents" = "working_capital_investment",
                             "Depreciation & Amortisation" = "depreciation_and_amortisation",
                             "Fixed Capital" = "fixed_capital_investment",
                             "Net Debt Issuance" = "net_debt_issuance",
                             "Non-Operating Assets" = "non_operating_assets")
    metric_values_ordered <- metric_choices_list[metric_choices_list != ""]
    current_pos <- match(metric_type_selected, metric_values_ordered)
    if(!is.na(current_pos) && current_pos < length(metric_values_ordered)) {
      next_metric_value <- metric_values_ordered[current_pos + 1]
      updateSelectInput(session, "metric_type", selected = next_metric_value)
    } else { updateSelectInput(session, "metric_type", selected = "") }
  })
  
  # --- Formula Preview ---
  formula_string <- reactive({
    component_suffixes <- get_components_suffixes(app_state) # Use renamed getter
    
    # Explicitly depend on the dynamic inputs based on suffixes
    input_dependencies <- lapply(component_suffixes, function(suffix) {
      list(
        input[[paste0("component_series_", suffix)]],
        input[[paste0("component_op_", suffix)]],
        input[[paste0("component_factor_", suffix)]]
      )
    })
    
    if (length(component_suffixes) == 0) return("Formula: No components added yet")
    
    print("--- Building formula string ---")
    formula_parts <- character(0)
    first_valid <- TRUE
    for (suffix in component_suffixes) { # Iterate through suffixes
      component_id <- paste0("component_series_", suffix)
      series_input <- input[[component_id]]
      
      print(paste(" > Formula string - Reading ID:", component_id, "Value:", series_input))
      
      if (!is.null(series_input) && series_input != "") {
        op_input <- input[[paste0("component_op_", suffix)]]
        factor_input <- input[[paste0("component_factor_", suffix)]]
        op_symbol <- switch(op_input %||% "add", "add" = "+", "subtract" = "-", "+")
        factor_text <- if(!is.null(factor_input) && factor_input != 1) paste0(" * ", factor_input) else ""
        series_text <- paste0("「", series_input, "」")
        if (first_valid) {
          if (op_input == "subtract") { formula_parts <- c(formula_parts, "-", series_text, factor_text) }
          else { formula_parts <- c(formula_parts, series_text, factor_text) }
          first_valid <- FALSE
        } else {
          formula_parts <- c(formula_parts, op_symbol, series_text, factor_text)
        }
      }
    } # End for loop
    
    if (length(formula_parts) == 0) return("Formula: Select series for components")
    # Ensure final cleanup is complete
    final_formula <- paste(formula_parts, collapse = " ")
    final_formula <- gsub("\\s+", " ", trimws(final_formula))
    # Optional: Handle leading +/- signs consistently
    if (startsWith(final_formula, "+ ")) final_formula <- sub("^\\+ ", "", final_formula)
    if (startsWith(final_formula, "- ")) final_formula <- sub("^- ", "-", final_formula) # Keep leading minus?
    
    paste("Formula:", final_formula)
  })
  
  output$formula_preview <- renderText({ formula_string() })
  
} # End formulaBuilderServer