# --- Data Import Module ---

dataImportUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Choose Excel File",
                  accept = c(".xlsx", ".xls")),
        uiOutput(ns("sheet_selector")),
        actionButton(ns("import_btn"), "Import Selected Rows",
                     class = "btn-primary"),
        div(id = ns("status_indicator"), style = "margin-top: 10px; color: green; display: none;",
            icon("check"), "Data imported successfully!"),
        
        # Clear button for accumulated data
        div(style = "margin-top: 10px;",
            actionButton(ns("clear_btn"), "Clear All Imported Data",
                         class = "btn-warning btn-sm")),
        
        # Reminder framework
        tags$div(
          style = "margin-top: 25px; margin-bottom: 10px;",
          tags$h4("Import Reminders:"),
          tags$div(
            style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; background-color: #f9f9f9;",
            tags$p("Remember to import:", style = "font-weight: bold;"),
            tags$ul(
              tags$li("Years"),
              tags$li("Income Statement",
                      tags$ul(
                        tags$li("Total Revenue"),
                        tags$li("Operating Profit"),
                        tags$li("Interest Expense"),
                        tags$li("Tax Paid"),
                        tags$li("Net Income")
                      )
              ),
              tags$li("Balance Sheet",
                      tags$ul(
                        tags$li("Cash and Short-Term Investments"),
                        tags$li("Total Current Assets"),
                        tags$li("Non-Operating Assets"),
                        tags$li("Short-Term Debt and Current Portion of Long-Term Liabilities"),
                        tags$li("Total Current Liabilities"),
                        tags$li("Non-Current Receivables and Loans"),
                        tags$li("Non-Current Payables")
                      )
              ),
              tags$li("Cash Flow Statement",
                      tags$ul(
                        tags$li("Depreciation & Amortisation"),
                        tags$li("CAPEX"),
                        tags$li("Net Debt Issuance")
                      ))
            )
          )
        )
      ),
      
      mainPanel(
        DT::dataTableOutput(ns("preview"))
      )
    )
  )
}

dataImportServer <- function(input, output, session, app_state) {
  ns <- session$ns # Get namespace function
  
  # Update sheet selector when file is uploaded
  output$sheet_selector <- renderUI({
    req(input$file)
    # Error handling for invalid file types could be added here
    sheets <- tryCatch(excel_sheets(input$file$datapath), error = function(e) NULL)
    if (is.null(sheets)) {
      showNotification("Error reading Excel file. Ensure it's a valid .xlsx or .xls file.", type="error")
      return(NULL)
    }
    selectInput(ns("sheet"), "Select Sheet", choices = sheets)
  })
  
  # Read data when file and sheet are selected
  observe({
    req(input$file, input$sheet)
    
    # Use tryCatch for robust file reading
    df <- tryCatch({
      read_excel(
        input$file$datapath,
        sheet = input$sheet,
        col_names = FALSE # Use X1, X2, etc.
      )
    }, error = function(e) {
      showNotification(paste("Error reading sheet:", input$sheet, "-", e$message), type="error")
      NULL
    })
    
    if (!is.null(df)) {
      # Store the sheet name in the data
      df$sheet_name <- input$sheet
      
      # Force numeric columns and add row_id (moved from global server)
      df <- df %>%
        mutate(across(where(~!is.numeric(.) && all(grepl("^[0-9.,-]+$|^$", as.character(.)))), # More robust check for numeric-like strings
                      ~suppressWarnings(as.numeric(gsub(",", "", .))))) %>% # Handle commas
        mutate(row_id = row_number()) # Add row ID for selection tracking
      
      set_raw_data(app_state, df)
    } else {
      set_raw_data(app_state, NULL) # Clear raw data on error
    }
  })
  
  
  # Display preview of the data with row selection enabled
  output$preview <- DT::renderDataTable({
    req(get_raw_data(app_state))
    raw_df <- get_raw_data(app_state)
    datatable_data <- raw_df %>% select(-any_of(c("sheet_name", "row_id"))) # Select display cols
    
    # Use DT options with JS renderer for rounding
    DT::datatable(datatable_data,
                  options = list(
                    pageLength = -1, # Show all rows
                    scrollY = "400px",
                    scrollX = TRUE,
                    dom = 'ti', # Simple table + info
                    # --- ADDED columnDefs with JS from old_code ---
                    columnDefs = list(list(
                      targets = "_all", # Apply to all columns
                      render = JS( # JS function to format numbers for display
                        "function(data, type, row, meta) {",
                        "  if(type === 'display' && data !== null && data !== '') {",
                        "    // Attempt to parse as float",
                        "    var num = parseFloat(String(data).replace(/,/g, ''));", # Handle potential commas
                        "    // Check if parsing was successful",
                        "    if(!isNaN(num)) {",
                        "      // Round to 2 decimal places for display",
                        "      return (Math.round(num * 100) / 100).toLocaleString(undefined, {minimumFractionDigits: 2, maximumFractionDigits: 2});", # Ensure 2 DP
                        "    }",
                        "  }",
                        "  // Return original data if not display, null, empty, or not a number",
                        "  return data;",
                        "}"
                      )
                    )) # End list for columnDefs targets
                    # --- END ADDED columnDefs ---
                  ), # End list for options
                  selection = 'multiple',
                  rownames = FALSE
    ) # No need for %>% formatRound() anymore
  })
  
  # Import selected rows when button is clicked - ACCUMULATE data
  observeEvent(input$import_btn, {
    req(get_raw_data(app_state))
    # Need the row indices from the *displayed* data, which map to row_id in raw
    selected_rows_displayed <- input$preview_rows_selected
    raw_data_current <- get_raw_data(app_state)
    
    if(length(selected_rows_displayed) > 0 && !is.null(raw_data_current)) {
      # Map displayed row indices back to the original row_id
      selected_row_ids <- raw_data_current$row_id[selected_rows_displayed]
      
      # Get currently selected rows using row_id
      new_rows <- raw_data_current[raw_data_current$row_id %in% selected_row_ids, ]
      new_rows$row_id <- NULL # Remove temporary ID
      
      # Add timestamp
      new_rows$import_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # Get current accumulated data
      current_data <- get_selected_data(app_state)
      
      # --- Simplified and Robust Data Combination ---
      
      # Ensure new_rows is a valid data frame before proceeding
      if (!is.data.frame(new_rows) || nrow(new_rows) == 0) {
        # showNotification("No valid new rows selected or data format issue.", type="warning") # REMOVED
        return() # Stop processing
      }
      
      # Get current accumulated data (might be NULL)
      current_data <- get_selected_data(app_state)
      
      combined_data <- NULL # Initialize combined data
      
      tryCatch({
        if (!is.null(current_data)) {
          # Ensure current_data is also a dataframe
          if (!is.data.frame(current_data)) {
            stop("Stored selected data is not a valid data frame.")
          }
          # bind_rows handles column differences by filling with NA automatically
          combined_data <- dplyr::bind_rows(current_data, new_rows)
        } else {
          # This is the first import, combined_data is just new_rows
          combined_data <- new_rows
        }
        
        # Proceed only if bind_rows succeeded
        if (!is.null(combined_data)) {
          set_selected_data(app_state, combined_data)
          # ... (rest of the success logic: increment count, metadata, notification, etc.) ...
          
          # --- Place the success logic (previously after the if/else) here ---
          increment_import_count(app_state, nrow(new_rows))
          app_state$data$import_metadata$last_import_time <- Sys.time()
          # Ensure sheet_name exists before trying to unique it
          if("sheet_name" %in% names(combined_data)) {
            app_state$data$import_metadata$sheets_imported <- unique(c(app_state$data$import_metadata$sheets_imported, combined_data$sheet_name[!is.na(combined_data$sheet_name)]))
          }
          
          # showNotification( # REMOVED
          #   paste("Imported", nrow(new_rows), "rows. Total:", get_import_count(app_state), "rows"),
          #   type = "message",
          #   duration = 3
          # )
          
          # Auto-advance sheet (keep existing logic)
          req(input$file)
          sheets <- tryCatch(excel_sheets(input$file$datapath), error = function(e) NULL)
          if (!is.null(sheets)) {
            current_idx <- match(input$sheet, sheets)
            if(!is.na(current_idx) && current_idx < length(sheets)) {
              next_sheet <- sheets[current_idx + 1]
              updateSelectInput(session, "sheet", selected = next_sheet)
            }
          }
          
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("$('#%s').show(); setTimeout(function() { $('#%s').fadeOut('slow'); }, 3000);",
                                   ns("status_indicator"), ns("status_indicator")))
            shinyjs::runjs(sprintf("$('#%s').addClass('btn-success').removeClass('btn-primary'); setTimeout(function() { $('#%s').addClass('btn-primary').removeClass('btn-success'); }, 1000);",
                                   ns("import_btn"), ns("import_btn")))
          })
          # --- End of success logic ---
          
        } # End if(!is.null(combined_data))
        
      }, error = function(e) {
        # Catch errors during the bind_rows or subsequent steps
        error_msg <- paste("Error combining imported data:", e$message)
        # Provide more context if possible
        if(grepl("bind_rows", e$message, ignore.case = TRUE)) {
          error_msg <- paste(error_msg, "Check if column types are compatible between imports or if data structure is unexpected.")
        }
        print(paste("Error details:", error_msg)) # Log to console
        # Print structure of inputs for debugging
        print("Structure of current_data before error:")
        print(str(current_data))
        print("Structure of new_rows before error:")
        print(str(new_rows))
        showNotification(error_msg, type="error", duration = 10)
        # Optionally: Don't update selected_data state on error
        # set_selected_data(app_state, current_data) # Revert to previous state? Or leave as is?
      })
      # --- End Simplified Combination ---
      
      # Update import counter only if combine succeeded
      if (!is.null(get_selected_data(app_state))) { # Check state was updated
        increment_import_count(app_state, nrow(new_rows))
        
        # Update metadata
        app_state$data$import_metadata$last_import_time <- Sys.time()
        app_state$data$import_metadata$sheets_imported <- unique(c(get_selected_data(app_state)$sheet_name))
        
        
        # Visual feedback
        # showNotification( # REMOVED
        #   paste("Imported", nrow(new_rows), "rows. Total:", get_import_count(app_state), "rows"),
        #   type = "message",
        #   duration = 3
        # )
        
        # Auto-advance sheet
        req(input$file) # Ensure file input hasn't vanished
        sheets <- tryCatch(excel_sheets(input$file$datapath), error = function(e) NULL)
        if (!is.null(sheets)) {
          current_idx <- match(input$sheet, sheets)
          if(!is.na(current_idx) && current_idx < length(sheets)) {
            next_sheet <- sheets[current_idx + 1]
            updateSelectInput(session, "sheet", selected = next_sheet)
          }
        }
        
        
        # Use shinyjs::delay with NAMESPACED IDs
        shinyjs::delay(100, {
          shinyjs::runjs(sprintf("$('#%s').show(); setTimeout(function() { $('#%s').fadeOut('slow'); }, 3000);",
                                 ns("status_indicator"), ns("status_indicator")))
          shinyjs::runjs(sprintf("$('#%s').addClass('btn-success').removeClass('btn-primary'); setTimeout(function() { $('#%s').addClass('btn-primary').removeClass('btn-success'); }, 1000);",
                                 ns("import_btn"), ns("import_btn")))
        })
      }
    } else {
      # showNotification("No rows selected or raw data missing.", type="warning") # REMOVED
    }
  })
  
  # Clear all imported data
  observeEvent(input$clear_btn, {
    reset_data(app_state) # Resets selected, raw, count, metadata
    # Optionally clear file input
    # shinyjs::reset("file") # Might require input binding ID if fileInput is complex
    # showNotification("All imported data cleared", type = "warning", duration = 3) # REMOVED
  })
  
}
