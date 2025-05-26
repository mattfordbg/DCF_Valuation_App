# --- Data Standardization Module ---

dataStandardUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4("Calculation Information"),
        div(
          style = "margin-top: 25px; margin-bottom: 10px;",
          tags$div(
            style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; background-color: #f9f9f9;",
            h4("Calculation Reminders:"),
            tags$p("Remember to check if series are negative."),
            tags$p("WC excl. Cash: Current Assets − Cash − Current Liabs + Short-Term Debt")
          )
        )
      ), # <<< End sidebarPanel (Correct)
      
      # --- Start mainPanel ---
      mainPanel(
        fluidRow(
          column(12,
                 h4("Map Your Data to Standardised Metrics"),
                 p("For each metric, select series and apply operations as needed."),
                 hr()
          )
        ), # <<< End Intro Row
        
        # --- Year selection ---
        fluidRow(
          column(6,
                 h4("Year Selection"),
                 selectInput(ns("year_row"), "Select Row Containing Years:",
                             choices = c("Select a series" = "")),
                 actionButton(ns("detect_years_btn"), "Add Years", class = "btn-info"),
                 verbatimTextOutput(ns("year_detection_result"))
          ), # <<< End column 6
          column(6,
                 h4("Advanced"),
                 checkboxInput(ns("replace_na"), "Replace NA/missing with 0", TRUE),
                 hr(),
                 p("Or import an existing standardized dataset:"),
                 fileInput(ns("upload_std_file"), "Upload Standardized CSV File", accept = ".csv")
                 # --- REMOVED Extra closing parenthesis here ---
          ) # <<< End column 6
        ), # <<< End Year Selection Row (Correct place)
        
        # --- Call Formula Builder Module ---
        formulaBuilderUI(ns("formula_build")), # Pass namespace
        
        # --- Defined Metrics ---
        fluidRow(
          column(12,
                 hr(),
                 h4("Defined Metrics"),
                 DT::DTOutput(ns("metrics_table")),
                 actionButton(ns("remove_metric_btn"), "Remove Selected Metric", class = "btn-danger")
          )
        ), # <<< End Defined Metrics Row
        
        # --- Results section ---
        fluidRow(
          column(12,
                 hr(),
                 h4("Standardised Dataset (Years as Columns)"),
                 DTOutput(ns("standardized_data"))
          )
        ), # <<< End Results Row
        
        # --- Calculation/Download Buttons ---
        fluidRow(
          column(12, style = "padding-bottom: 30px;", # Keep the padding
                 div(style = "margin-top: 20px; display: flex; justify-content: center; gap: 20px;",
                     actionButton(ns("calculate_std_btn"), "Calculate Standardised Data",
                                  class = "btn-primary btn-lg"),
                     downloadButton(ns("download_std_data"), "Download Standardised Data",
                                    class = "btn-success btn-lg")
                 )
          )
        ) # <<< End Buttons Row
        
      ) # <<< CORRECT Closing parenthesis for mainPanel
      
    ) # <<< End sidebarLayout
  ) # <<< End tagList
} # <<< End dataStandardUI function


dataStandardServer <- function(input, output, session, app_state) {
  ns <- session$ns
  
  # --- Call Formula Builder Server ---
  # This module needs access to app_state to get series names and update defined metrics
  callModule(formulaBuilderServer, "formula_build", app_state)
  
  # --- Year Detection Logic ---
  # Update year row selector choices based on selected data
  observe({
    req(get_selected_data(app_state))
    data_df <- get_selected_data(app_state)
    if(is.null(data_df) || nrow(data_df) == 0) {
      series_names <- character(0)
    } else {
      series_names <- unique(data_df[[1]]) # Assume first col has names
      series_names <- series_names[!is.na(series_names)]
    }
    
    choices <- c("Select a series" = "")
    if(length(series_names) > 0) {
      choices <- c(choices, setNames(series_names, series_names))
      # Don't set selected here, let user choose or default empty
    }
    updateSelectInput(session, "year_row",
                      choices = choices,
                      selected = if(length(series_names) > 0) series_names[1] else "")
  })
  
  # Year detection button action
  observeEvent(input$detect_years_btn, {
    req(get_selected_data(app_state), input$year_row)
    data_df <- get_selected_data(app_state)
    selected_row <- input$year_row
    
    year_result <- detect_financial_years(data_df, selected_row) # Use utility function
    
    if (!is.null(year_result$years)) {
      set_years_detected(app_state, list(
        years = year_result$years,
        cols = year_result$cols,
        log = year_result$log
      ))
    } else {
      set_years_detected(app_state, NULL)
    }
    
    output$year_detection_result <- renderText({
      year_info <- get_years_detected(app_state)
      if(!is.null(year_info)) year_info$log else year_result$log
    })
  })
  
  # --- Defined Metrics Table ---
  # Function to update the table (called when metrics change)
  updateMetricsTable <- function() {
    output$metrics_table <- renderDT({
      metrics <- get_defined_metrics(app_state)
      if(length(metrics) == 0) {
        return(datatable(data.frame(Message = "No metrics defined yet"), options = list(dom='t'), rownames=FALSE))
      }
      
      metrics_df <- imap_dfr(metrics, ~{ # Use imap_dfr for cleaner construction
        comp_text <- map_chr(.x$components, function(c) {
          factor_text <- if(!is.null(c$factor) && c$factor != 1) paste0(" * ", c$factor) else ""
          op_symbol <- switch(c$operation, "add" = "+", "subtract" = "-", "?")
          paste0(op_symbol, " 「", c$series, "」", factor_text)
        })
        if(length(comp_text) > 0 && startsWith(comp_text[1], "+ ")) {
          comp_text[1] <- sub("^\\+ ", "", comp_text[1])
        } else if (length(comp_text) > 0 && startsWith(comp_text[1], "- ")) {
          # Keep leading minus if first operation is subtract
        }
        data.frame(
          Metric = .y,
          Type = .x$type %||% NA_character_, # Handle missing type gracefully
          Formula = paste(comp_text, collapse = " ")
        )
      })
      
      datatable(metrics_df,
                options = list(pageLength = 10, dom = 'tip'),
                selection = 'single',
                rownames = FALSE)
    })
  }
  
  # Initial rendering and update when defined_metrics change
  observe({
    get_defined_metrics(app_state) # Dependency
    updateMetricsTable()
  })
  
  # Remove selected metric
  observeEvent(input$remove_metric_btn, {
    req(input$metrics_table_rows_selected)
    metrics <- get_defined_metrics(app_state)
    metrics_list <- get_defined_metrics(app_state)
    metric_names_ordered <- names(metrics_list) # Get names in current order
    
    if(length(metric_names_ordered) == 0 || length(input$metrics_table_rows_selected) == 0) {
      return()
    }
    
    selected_index <- input$metrics_table_rows_selected
    if (selected_index > 0 && selected_index <= length(metric_names_ordered)) {
      selected_metric_name <- metric_names_ordered[selected_index]
      remove_defined_metric(app_state, selected_metric_name)
      showNotification(paste("Metric", selected_metric_name, "removed"), type = "message")
      # Table will update via the observe above
    }
  })
  
  
  # --- Standardized Data Calculation & Display ---
  # Calculate button action
  observeEvent(input$calculate_std_btn, {
    # Gather Inputs
    req(
      get_selected_data(app_state), 
      get_years_detected(app_state), 
      get_defined_metrics(app_state),
      !is.null(input$replace_na)
      )
    data_df <- get_selected_data(app_state)
    metrics <- get_defined_metrics(app_state)
    year_info <- get_years_detected(app_state)
    replace_na <- isTRUE(input$replace_na)
    
    if (is.null(year_info) || is.null(year_info$years) || length(year_info$years) == 0) {
      showNotification("Please detect years successfully before calculating", type = "error")
      return()
    }
    if (is.null(metrics) || length(metrics) == 0) {
      showNotification("Please define at least one metric before calculating", type = "error")
      return()
    }
    
    # Call calculation function
    calculation_result <- calculate_standardized_data(
      selected_data_df = data_df,
      defined_metrics = metrics,
      year_info = year_info,
      replace_na_with_zero = replace_na
    )
    
    # Update State
    # After successfully calculating standardized data:
    if (!is.null(calculation_result$data)) {
      set_standardized_data(app_state, calculation_result$data)
      set_calculation_log(app_state, calculation_result$log)
      
      # Extract and set base revenue from standardized data
      std_data <- calculation_result$data
      
      # Look for the Revenue row and get the most recent year's value
      revenue_row <- std_data[std_data$metric == "Revenue", ]
      if (nrow(revenue_row) > 0) {
        year_cols <- grep("^\\d{4}$", colnames(revenue_row), value = TRUE)
        if (length(year_cols) > 0) {
          # Find the most recent year
          most_recent_year <- max(as.numeric(year_cols))
          most_recent_year_col <- as.character(most_recent_year)
          
          # Get the base revenue value
          base_revenue_value <- revenue_row[[most_recent_year_col]]
          if (!is.null(base_revenue_value) && is.numeric(base_revenue_value) && !is.na(base_revenue_value)) {
            print(paste("Setting base revenue to:", base_revenue_value, "from year", most_recent_year))
            set_base_revenue(app_state, base_revenue_value)
          }
        }
      }
      
      # Extract and set non-operating assets
      noa_row <- std_data[std_data$metric == "Non-Operating Assets", ]
      if (nrow(noa_row) > 0) {
        year_cols <- grep("^\\d{4}$", colnames(noa_row), value = TRUE)
        if (length(year_cols) > 0) {
          most_recent_year <- max(as.numeric(year_cols))
          most_recent_year_col <- as.character(most_recent_year)
          
          noa_value <- noa_row[[most_recent_year_col]]
          if (!is.null(noa_value) && is.numeric(noa_value) && !is.na(noa_value)) {
            print(paste("Setting non-operating assets to:", noa_value, "from year", most_recent_year))
            set_non_operating_assets(app_state, noa_value)
          }
        }
      }
      
      # AUTOMATICALLY INITIALIZE ASSUMPTIONS
      year_cols <- grep("^\\d{4}$", colnames(std_data), value = TRUE)
      if (length(year_cols) > 0) {
        # Create fresh assumptions with the detected years
        new_assumptions <- data.frame(
          Assumption = c("Revenue Growth", "Operating Profit Margin", "Interest Expense",
                         "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                         "Incremental FC", "Net Debt Issuance"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
        # Add year columns with default values
        for (year in year_cols) {
          new_assumptions[[year]] <- 0.05  # 5% default
        }
        
        print("Setting assumptions data in app state")
        set_programmatic_update(app_state, TRUE)
        set_assumptions_data(app_state, new_assumptions)
        app_state$dcf$parameters$CAP <- length(year_cols)
        set_programmatic_update(app_state, FALSE)
        
        # Trigger projections calculation with a delay to ensure all UI updates are complete
        shinyjs::delay(500, {
          print("Auto-triggering projections calculation after standardization")
          # Use session=NULL since we're calling from within a module
          recalculate_projections(app_state, auto_triggered = TRUE)
        })
      }
      
      showNotification("Standardized data calculated", type = "message")
    } else {
      set_standardized_data(app_state, NULL)
      set_calculation_log(app_state, calculation_result$log)
      showNotification("Standardized data calculation failed. Check log.", type = "error")
    }
    set_programmatic_update(app_state, FALSE) # Seems safe to reset here
  })
  
  #Observer for Standardized File Upload
  observeEvent(input$upload_std_file, {
    req(input$upload_std_file)
    tryCatch({
      uploaded_data <- read.csv(input$upload_std_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      
      # Basic Validation
      if (!"metric" %in% colnames(uploaded_data)) stop("Uploaded file missing 'metric' column.")
      year_cols_indices <- grep("^(X)?[0-9]{4}$", colnames(uploaded_data))
      if (length(year_cols_indices) == 0) stop("Uploaded file has no year-like columns (YYYY or XYYYY).")
      
      # Clean year columns
      col_names <- colnames(uploaded_data)
      cleaned_year_names <- sapply(year_cols_indices, function(idx) {
        name <- col_names[idx]
        if (startsWith(name, "X")) substr(name, 2, nchar(name)) else name
      })
      col_names[year_cols_indices] <- cleaned_year_names
      colnames(uploaded_data) <- col_names
      
      # Convert year columns to numeric
      for(yr_col in cleaned_year_names) {
        uploaded_data[[yr_col]] <- suppressWarnings(as.numeric(uploaded_data[[yr_col]]))
      }
      
      # Update state DIRECTLY
      set_programmatic_update(app_state, TRUE)
      set_standardized_data(app_state, uploaded_data)
      years_numeric <- as.numeric(cleaned_year_names)
      # Make sure cols index is relative to the *original* uploaded data frame
      set_years_detected(app_state, list(
        years = years_numeric,
        cols = year_cols_indices,
        log = paste("Years detected from uploaded file:", paste(sort(years_numeric), collapse = ", "))
      ))
      upload_log <- paste0("Imported standardized dataset: ", input$upload_std_file$name, "\n",
                           nrow(uploaded_data), " metrics across ", length(years_numeric), " years.")
      set_calculation_log(app_state, upload_log)
      
      # Extract and set base revenue
      revenue_row <- uploaded_data[uploaded_data$metric == "Revenue", ]
      if (nrow(revenue_row) > 0) {
        year_cols <- grep("^\\d{4}$", colnames(revenue_row), value = TRUE)
        if (length(year_cols) > 0) {
          most_recent_year <- max(as.numeric(year_cols))
          most_recent_year_col <- as.character(most_recent_year)
          
          base_revenue_value <- revenue_row[[most_recent_year_col]]
          if (!is.null(base_revenue_value) && is.numeric(base_revenue_value) && !is.na(base_revenue_value)) {
            print(paste("Setting base revenue to:", base_revenue_value, "from year", most_recent_year))
            set_base_revenue(app_state, base_revenue_value)
          }
        }
      }
      
      # Extract and set non-operating assets
      noa_row <- uploaded_data[uploaded_data$metric == "Non-Operating Assets", ]
      if (nrow(noa_row) > 0) {
        year_cols <- grep("^\\d{4}$", colnames(noa_row), value = TRUE)
        if (length(year_cols) > 0) {
          most_recent_year <- max(as.numeric(year_cols))
          most_recent_year_col <- as.character(most_recent_year)
          
          noa_value <- noa_row[[most_recent_year_col]]
          if (!is.null(noa_value) && is.numeric(noa_value) && !is.na(noa_value)) {
            print(paste("Setting non-operating assets to:", noa_value, "from year", most_recent_year))
            set_non_operating_assets(app_state, noa_value)
          }
        }
      }
      
      # Initialize assumptions with year columns
      if (length(years_numeric) > 0) {
        new_assumptions <- data.frame(
          Assumption = c("Revenue Growth", "Operating Profit Margin", "Interest Expense",
                         "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                         "Incremental FC", "Net Debt Issuance"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
        # Add year columns with default values
        for (year in sort(as.character(years_numeric))) {
          new_assumptions[[year]] <- 0.05  # 5% default
        }
        
        # Update assumptions
        set_assumptions_data(app_state, new_assumptions)
        app_state$dcf$parameters$CAP <- length(years_numeric)
      }
      
      set_programmatic_update(app_state, FALSE)
      
      # Switch to DCF tab
      shinyjs::delay(300, {
        shinyjs::runjs('$("a[data-value=\'DCF\']").tab("show");')
      })
      
      # Trigger projections calculation with a delay
      shinyjs::delay(500, {
        print("Auto-triggering projections calculation after file upload")
        recalculate_projections(app_state, auto_triggered = TRUE)
      })
      
      showNotification("Successfully imported and applied standardized dataset.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error processing uploaded standardized file:", e$message), type = "error", duration=8)
      set_programmatic_update(app_state, FALSE) # Ensure flag reset on error
    })
  })

  # Display standardized data table
  output$standardized_data <- DT::renderDataTable({
    req(get_standardized_data(app_state))
    std_df <- get_standardized_data(app_state)
    
    datatable(std_df,
              options = list(
                pageLength = -1, # Show all
                scrollX = TRUE,
                scrollY = "400px",
                dom = 'ti', # Simple table
                # buttons = c('copy', 'csv', 'excel'), # Add export buttons if desired
                columnDefs = list(list(
                  targets = "_all", # Apply to all columns
                  render = JS( # Format numbers nicely
                    "function(data, type, row, meta) {",
                    "  if(type === 'display' && data !== null && data !== '' && typeof data === 'number') {",
                    "      return data.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 2});",
                    "  }",
                    "  return data;",
                    "}"
                  )
                ))
              ),
              # extensions = 'Buttons', # Required if using buttons option
              rownames = FALSE
    )
  })
  
  # Download handler for standardized data
  output$download_std_data <- downloadHandler(
    filename = function() {
      paste("standardized-financial-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      std_data_to_write <- get_standardized_data(app_state)
      if (!is.null(std_data_to_write)){
        write.csv(std_data_to_write, file, row.names = FALSE, na = "") # Write NA as blank
      } else {
        writeLines("No standardized data available to download.", file)
      }
    }
  )
  
}
