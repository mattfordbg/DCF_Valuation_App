# --- Start of Content Pasted from global.R ---
# --- Libraries ---
library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(purrr)
library(shinyjs)
library(rhandsontable)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(usethis)

# --- Constants ---
required_metrics <- c(
  "Revenue", "Operating Profit", "Interest Expense", "Tax", "Net Income",
  "Depreciation & Amortisation", "Working Capital Excluding Cash and Cash Equivalents",
  "Fixed Capital", "Net Debt Issuance", "Non-Operating Assets"
)

# --- Source Utility Functions ---
# Paths are relative to this file's location (app/)
source("utils/state_manager.R")
source("utils/data_processor.R")
source("utils/dcf_calculator.R")
source("utils/fade_models.R")

# --- Source Module Files ---
source("modules/data_import/mod_data_import.R")
source("modules/data_standard/mod_data_standard.R")
source("modules/formula_builder/mod_formula_builder.R")
source("modules/dcf_params/mod_dcf_params.R")
source("modules/projections/mod_projections.R")

# --- Helper for %||% operator ---
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
# --- End of Content Pasted from global.R ---

# --- Main App UI and Server ---

ui <- fluidPage(
  shinyjs::useShinyjs(), # Initialize shinyjs
  
  # Include custom JS for scrolling fix
  tags$head(
    tags$script(src = "fix_scrolling.js") # Reference the file in www/
  ),
  
  # CSS Styles (Keep main layout styles here)
  tags$head(
    tags$style(HTML("
      /* Make the tab bar fixed at the top with reduced size */
      .nav-tabs {
        position: fixed;
        top: 35px; /* Space for the title */
        width: 100%;
        background: white;
        z-index: 1000;
        padding-top: 2px;
        padding-bottom: 2px;
        border-bottom: 1px solid #ddd;
      }
      .nav-tabs > li > a { padding: 5px 10px; font-size: 14px; }
      .tab-content { padding-top: 70px; }
      .shiny-output-error, .shiny-output-error-validation { top: 65px; }
      .title-panel {
        position: fixed; top: 0; width: 100%; background: white;
        z-index: 1001; padding: 5px 15px; border-bottom: 1px solid #eee;
      }
      .title-panel h2 { font-size: 18px; margin-top: 5px; margin-bottom: 5px; }

      /* DCF & Hurdle Tab Layout Styles */
      .dcf-container { display: flex; flex-direction: column; width: 100%; }
      .top-section { display: flex; width: 100%; }
      .fixed-sidebar { /* Style defined in dcfParams module UI */ }
      .scrollable-content { width: 75%; margin-left: 0; padding: 15px; }
      .full-width-section { width: 100%; margin-top: 20px; padding-top: 20px; border-top: 1px solid #ddd; }
      /* Ensure scrollable content doesn't overlap sidebar */
       .top-section > .fixed-sidebar + .scrollable-content { margin-left: 25%; width: 75%; }

       /* Hurdle specific inputs styling */
       .hurdle-inputs-container { padding: 15px; border: 1px solid #eee; border-radius: 5px; margin-bottom: 20px; background-color: #fdfdfd;}
       .hurdle-inputs-container .form-group { margin-bottom: 5px; } /* Reduce margin between hurdle inputs */
    "))
  ),
  
  # Fixed Title Panel
  div(class = "title-panel", titlePanel("Valuation Tool")),
  
  # Main Tabset Panel
  tabsetPanel(id = "main_tabs",
              tabPanel("Preview",
                       # Call Data Import Module UI
                       dataImportUI("data_import_mod")
              ),
              tabPanel("Selected Data",
                       # This tab only shows accumulated data, handled in the main server for now
                       DT::dataTableOutput("selected_data_display"),
                       div(style = "margin-top: 10px;", textOutput("import_summary_display"))
              ),
              tabPanel("Standardise Data",
                       # Call Data Standardization Module UI
                       dataStandardUI("data_standard_mod")
              ),
              tabPanel("DCF",
                       div(class = "dcf-container",
                           div(class = "top-section",
                               # Call DCF Parameters Module UI (Sidebar)
                               dcfParamsUI("dcf_params_mod"),
                               
                               # Call Projections Module UI (Main Content Area)
                               div(class = "scrollable-content",
                                   projectionsUI("projections_mod")
                               )
                           ), # End top-section
                           
                           # --- Financial Projections Display Area (Full Width Below) ---
                           div(class = "full-width-section",
                               fluidRow(
                                 column(12,
                                        hr(),
                                        h4("Financial Projections"),
                                        p("Enter base revenue/NOA and generate projections.")
                                 )
                               ),
                               fluidRow(
                                 column(3, numericInput("base_revenue_input", "Base Revenue (Latest Year)", value = 0, min = 0)),
                                 column(3, numericInput("non_operating_assets_input", "Non-Operating Assets", value = 0, min = 0)),
                                 column(6, br(), actionButton("generate_projections_btn", "Generate Projections", class = "btn-primary"))
                               ),
                               fluidRow(
                                 column(12, h5("Projections Table"), DTOutput("projections_table_display"), br())
                               ),
                               fluidRow(
                                 column(12, align = "center",
                                        downloadButton("export_dcf_excel_btn", "Export Complete DCF Model", class = "btn-success btn-lg"),
                                        style = "padding-bottom: 30px;"
                                        )
                               )
                           ) # End full-width-section
                       ) # End dcf-container
              ), # End DCF TabPanel
              
              # --- Hurdle Tab ---
              tabPanel("Hurdle",
                       div(class = "dcf-container", # Use same container class
                           div(class = "top-section",
                               # Instantiate DCF Parameters Module UI with HURDLE ID
                               dcfParamsUI("hurdle_dcf_params_mod"),
                               
                               # Main Content Area for Hurdle
                               div(class = "scrollable-content",
                                   # --- Hurdle Specific Inputs ---
                                   div(class="hurdle-inputs-container",
                                       h4("Hurdle Parameters"),
                                       fluidRow(
                                         column(4, numericInput("hurdle_target_multiple", "Target Return (x)", value = 5, min = 0.1, step = 0.1)),
                                         column(4, numericInput("hurdle_holding_period", "Holding Period (Yrs)", value = 5, min = 1, step = 1)),
                                         column(4, numericInput("hurdle_dilution_pct", "Est. Dilution (%)", value = 0, min = 0, max = 100, step = 1))
                                       ),
                                       fluidRow(
                                         column(12, actionButton("calculate_hurdle_base", "Calculate Hurdle Base Values", class="btn-info"))
                                       )
                                   ), # End hurdle-inputs-container
                                   
                                   # Instantiate Projections Module UI with HURDLE ID
                                   projectionsUI("hurdle_projections_mod")
                                   
                               ) # End scrollable-content for Hurdle
                           ), # End top-section for Hurdle
                           
                           # --- Hurdle Financial Projections Display Area ---
                           div(class = "full-width-section",
                               fluidRow(
                                 column(12, hr(), h4("Hurdle DCF Financial Projections"))
                               ),
                               fluidRow(
                                 column(12, h5("Projections Table"), DTOutput("hurdle_projections_table_display"), br()) # New Output ID
                               ),
                               fluidRow(
                                 column(12, align = "center",
                                        downloadButton("export_hurdle_dcf_excel_btn", "Export Hurdle DCF Model", class = "btn-success btn-lg"), # New Button ID
                                        style = "padding-bottom: 30px;"
                                 )
                               )
                           ) # End full-width-section for Hurdle
                       ) # End dcf-container for Hurdle
              ), # End Hurdle TabPanel
              
              tabPanel("Graphs",
                       # Graphs module could be created, or keep simple logic here for now
                       fluidRow(
                         column(12,
                                selectInput("graph_plot_type_select", "Select Plot Type",
                                            choices = c("Data Points" = "points", "Connected Lines" = "lines",
                                                        "Logarithmic Fit" = "logarithmic", "Exact Polynomial Fit" = "poly",
                                                        "Smoothed Fit" = "smooth"),
                                            selected = "lines"),
                                plotOutput("metrics_plots_display", height = "1200px") # Reduced height slightly
                         )
                       )
              ) # End Graphs TabPanel
  ) # End tabsetPanel
) # End fluidPage


server <- function(input, output, session) {
  
  # --- Initialize Central State ---
  app_state <- initialize_app_state()
  
  # --- Call Module Servers ---
  callModule(dataImportServer, "data_import_mod", app_state)
  callModule(dataStandardServer, "data_standard_mod", app_state)
  # formulaBuilderServer is called *within* dataStandardServer
  # Main DCF Modules
  callModule(dcfParamsServer, "dcf_params_mod", app_state, state_prefix = "dcf")
  callModule(projectionsServer, "projections_mod", app_state, state_prefix = "dcf") # Now passing prefix
  
  # Hurdle DCF Modules
  callModule(dcfParamsServer, "hurdle_dcf_params_mod", app_state, state_prefix = "hurdle_dcf")
  callModule(projectionsServer, "hurdle_projections_mod", app_state, state_prefix = "hurdle_dcf") # Now passing prefix
  
  
  # --- ADDED: Observer to Synchronize Main DCF Params to Hurdle (if not modified) ---
  observe({
    # --- PRIMARY DEPENDENCY: Main DCF parameters ---
    main_params_reactive <- app_state$dcf$parameters
    # Trigger invalidation explicitly if it changes
    main_params_trigger <- main_params_reactive$CAP # Or any other element
    
    print("Sync Observer: Checking Main DCF parameter changes...")
    
    # --- ISOLATE reads of other state components ---
    isolate({
      hurdle_params_reactive <- app_state$hurdle_dcf$parameters
      hurdle_modified_flags <- get_hurdle_params_modified(app_state) # Reads ui state
      
      # --- Robust parameter access (still needed inside isolate) ---
      main_params <- NULL
      if (!is.null(main_params_reactive) && is.reactivevalues(main_params_reactive)) {
        main_params <- tryCatch({ reactiveValuesToList(main_params_reactive) }, error = function(e) {print(paste("Warning (Sync): Error dcf$params toList:", e$message)); NULL })
      } else if (is.list(main_params_reactive)) { print("Warning (Sync): dcf$params is regular list."); main_params <- main_params_reactive }
      
      # Proceed only if main_params and flags were successfully retrieved
      # Use standard R checks now as they are isolated
      if (is.null(main_params) || is.null(hurdle_modified_flags) || is.null(hurdle_params_reactive)) {
        print("Sync Observer: Skipping - Required state components not ready.")
        return() # Exit observer if state isn't ready
      }
      # --- END Robust parameter access ---
      
      update_performed <- FALSE
      # Set flag ONLY if we are about to make changes
      # set_programmatic_update(app_state, TRUE) # Moved inside loop
      
      tryCatch({
        for(param_name in names(hurdle_modified_flags)) {
          if (param_name %in% names(main_params) && !isTRUE(hurdle_modified_flags[[param_name]])) {
            current_hurdle_val <- hurdle_params_reactive[[param_name]]
            new_main_val <- main_params[[param_name]]
            if (!is.null(new_main_val) && !identical(current_hurdle_val, new_main_val)) {
              # Set flag BEFORE updating state
              if (!update_performed) {
                set_programmatic_update(app_state, TRUE)
                update_performed <- TRUE # Set flag only once
              }
              print(paste("Sync Observer: Updating hurdle", param_name, "from", current_hurdle_val, "to", new_main_val))
              hurdle_params_reactive[[param_name]] <- new_main_val
            }
          }
        } # End for loop
      }, error = function(e) {
        print(paste("Error in parameter sync observer logic:", e$message))
      }, finally = {
        # Reset flag only if it was set by this observer instance
        if (update_performed) {
          set_programmatic_update(app_state, FALSE)
        }
        print("Sync Observer: Finished check.")
      }) # End tryCatch
    }) # --- END ISOLATE ---
    
  }) # End Sync Observer
  
  # --- NEW Recalculation Helpers (Defined inside Server) ---
  
  # Helper for MAIN DCF
  recalculate_projections <- function(app_state, auto_triggered = FALSE) {
    print("Server: recalculate_projections triggered")
    # --- Gather State ---
    assumptions <- get_assumptions_data(app_state)
    base_rev <- get_base_revenue(app_state)
    noa <- get_non_operating_assets(app_state)
    std_data <- get_standardized_data(app_state)
    # Robustly get params as a list
    params_list <- NULL
    if (!is.null(app_state$dcf$parameters) && is.reactivevalues(app_state$dcf$parameters)) { params_list <- tryCatch({ reactiveValuesToList(app_state$dcf$parameters) }, error = function(e) NULL) } else if (is.list(app_state$dcf$parameters)) { params_list <- app_state$dcf$parameters }
    
    # --- Call Global Calculation Helper ---
    proj_result <- run_full_dcf_calculation(
      assumptions_df = assumptions,
      base_revenue = base_rev,
      non_op_assets = noa,
      dcf_params_list = params_list, # Pass the list
      std_data = std_data
    )
    
    # --- Update State ---
    if (!is.null(proj_result)) { print("Main DCF calculation successful."); } else { print("Main DCF calculation failed or returned NULL.") } # REMOVED showNotification
    set_projections_data(app_state, proj_result)
  }
  
  # Helper for HURDLE DCF
  recalculate_hurdle_projections <- function(app_state, auto_triggered = FALSE) {
    print("Server: recalculate_HURDLE_projections triggered")
    # --- Gather State ---
    hurdle_assumptions <- get_hurdle_assumptions_data(app_state)
    hurdle_base_rev <- get_hurdle_base_revenue(app_state)
    hurdle_noa <- get_hurdle_non_operating_assets(app_state)
    std_data <- get_standardized_data(app_state) # Need this too
    # Robustly get hurdle params as list
    hurdle_params_list <- NULL
    if (!is.null(app_state$hurdle_dcf$parameters) && is.reactivevalues(app_state$hurdle_dcf$parameters)) { hurdle_params_list <- tryCatch(reactiveValuesToList(app_state$hurdle_dcf$parameters), error=function(e) NULL) } else if (is.list(app_state$hurdle_dcf$parameters)) { hurdle_params_list <- app_state$hurdle_dcf$parameters }
    hurdle_inputs <- get_hurdle_inputs(app_state) # Still needed for post-processing
    
    # --- Call Global Calculation Helper ---
    hurdle_proj_result_raw <- run_full_dcf_calculation(
      assumptions_df = hurdle_assumptions,
      base_revenue = hurdle_base_rev,
      non_op_assets = hurdle_noa,
      dcf_params_list = hurdle_params_list, # Pass the list
      std_data = std_data
    )
    
    # --- Post-Process and Update State ---
    if (!is.null(hurdle_proj_result_raw)) {
      print("Hurdle DCF raw calculation successful. Post-processing...")
      # ... (Keep post-processing logic to calculate and add/update the Hurdle Multiple row) ...
      hurdle_intrinsic_value_row <- hurdle_proj_result_raw[hurdle_proj_result_raw$Metric == "Intrinsic Value", ]; target_future_value <- app_state$hurdle_dcf$target_future_value_for_calc %||% 0; hurdle_return_multiple <- NA
      if (nrow(hurdle_intrinsic_value_row) > 0) { first_year_col <- colnames(hurdle_proj_result_raw)[grepl("^\\d{4}$", colnames(hurdle_proj_result_raw))][1]; hurdle_intrinsic_value <- hurdle_intrinsic_value_row[[first_year_col]]; if (!is.null(target_future_value) && target_future_value > 0 && is.numeric(hurdle_intrinsic_value)) { hurdle_return_multiple <- hurdle_intrinsic_value / target_future_value } }; print(paste("Hurdle Achieved Multiple:", hurdle_return_multiple)); prop_row_idx <- which(hurdle_proj_result_raw$Metric == "Proportion of Market Cap"); if (length(prop_row_idx) > 0) { hurdle_proj_result_raw$Metric[prop_row_idx] <- "Hurdle Multiple Achieved (x)"; hurdle_proj_result_raw[prop_row_idx, -1] <- NA; hurdle_proj_result_raw[prop_row_idx, first_year_col] <- hurdle_return_multiple } else { new_row <- data.frame(Metric = "Hurdle Multiple Achieved (x)"); new_row[1, setdiff(colnames(hurdle_proj_result_raw), "Metric")] <- NA; new_row[1, first_year_col] <- hurdle_return_multiple; hurdle_proj_result_raw <- rbind(hurdle_proj_result_raw, new_row) }
      
      set_hurdle_projections_data(app_state, hurdle_proj_result_raw)
    } else {
      print("Hurdle DCF calculation failed or returned NULL.")
      set_hurdle_projections_data(app_state, NULL)
    }
  }
  # --- END NEW Recalculation Helpers ---
  
  # --- OBSERVE block defining the downloadHandler ---
  observe({
    # This will run whenever projections data changes
    proj_data <- get_projections_data(app_state)
    
    if (!is.null(proj_data)) {
      # Update the download handler whenever projections change
      output$export_dcf_excel_btn <- downloadHandler(
        filename = function() {
          print("Export Button: Generating filename...") # DEBUG
          paste("dcf_valuation_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
        },
        content = function(file) {
          print("Export Button: Content function started...") # DEBUG
          # Use tryCatch within content for better error isolation
          tryCatch({
            print("Export Button: Checking requirements...") # DEBUG
            req(get_assumptions_data(app_state), get_projections_data(app_state))
            print("Export Button: Requirements met.") # DEBUG
            assum_data <- get_assumptions_data(app_state)
            proj_data <- get_projections_data(app_state)
            
            # --- MODIFIED: Robust Parameter Access ---
            params_list <- NULL
            if (!is.null(app_state$dcf$parameters) && is.reactivevalues(app_state$dcf$parameters)) {
              params_list <- tryCatch({
                reactiveValuesToList(app_state$dcf$parameters)
              }, error = function(e) {
                print(paste("Warning (Export): Error converting dcf$parameters to list:", e$message))
                list( # Fallback access
                  risk_free_rate = app_state$dcf$parameters$risk_free_rate,
                  equity_premium = app_state$dcf$parameters$equity_premium,
                  beta = app_state$dcf$parameters$beta,
                  country_premium = app_state$dcf$parameters$country_premium,
                  long_run_inflation = app_state$dcf$parameters$long_run_inflation,
                  additional_terminal_growth = app_state$dcf$parameters$additional_terminal_growth,
                  CAP = app_state$dcf$parameters$CAP,
                  market_cap = app_state$dcf$parameters$market_cap
                )
              })
            } else if (is.list(app_state$dcf$parameters)) {
              print("Warning (Export): app_state$dcf$parameters is a regular list.")
              params_list <- app_state$dcf$parameters
            }
            
            # Check if params_list could be retrieved
            if (is.null(params_list)) {
              stop("Failed to retrieve valid DCF parameters for export.")
            }
            # --- END MODIFICATION ---
            
            
            print("Export Button: Data fetched.") # DEBUG
            print(paste("Params List class:", class(params_list))) # DEBUG Params
            # Add check for required params for rate calculation within export
            required_rate_params_export <- c("risk_free_rate", "equity_premium", "beta", "country_premium",
                                             "long_run_inflation", "additional_terminal_growth")
            missing_params_export <- setdiff(required_rate_params_export, names(params_list))
            if (length(missing_params_export) > 0) {
              stop(paste("Missing required parameters in params_list for rate calculation during export:", paste(missing_params_export, collapse=", ")))
            }
            # Check they are numeric
            non_numeric_export <- required_rate_params_export[sapply(params_list[required_rate_params_export], function(p)!is.numeric(p) || is.na(p))]
            if (length(non_numeric_export) > 0) {
              stop(paste("Non-numeric parameters found during export:", paste(non_numeric_export, collapse=", ")))
            }
            
            
            wb <- createWorkbook()
            addWorksheet(wb, "DCF Valuation")
            modifyBaseFont(wb, fontSize = 11, fontName = "Lato")
            print("Export Button: Workbook created.") # DEBUG
            
            # --- Create Styles ---
            title_style <- createStyle(textDecoration="bold", fontSize=16, fontName="Lato")
            header_style <- createStyle(textDecoration="bold", fontSize=14, fontName="Lato")
            header_row_style <- createStyle(textDecoration="bold", fontSize=12, fontName="Lato")
            params_numeric_style <- createStyle(numFmt="0.00", halign="center", fontName="Lato")
            assumptions_percent_style <- createStyle(numFmt="0.00%", halign="center", fontName="Lato")
            projections_numeric_style <- createStyle(numFmt="#,##0", halign="center", fontName="Lato")
            dark_gray_style <- createStyle(fgFill="#E6E6E6", fontName="Lato")
            light_gray_style <- createStyle(fgFill="#F2F2F2", fontName="Lato")
            white_style <- createStyle(fgFill="#FFFFFF", fontName="Lato")
            bold_style <- createStyle(textDecoration="bold", fontName="Lato")
            market_cap_style <- createStyle(numFmt="0\"%\"", halign="center", fontName="Lato") # Ensure this is correct format for %
            centered_style <- createStyle(halign="center", fontName="Lato")
            
            
            # --- Write Data & Styles (Copied & adjusted startRow logic) ---
            current_row <- 1
            writeData(wb, 1, "DCF Valuation Model", startRow = current_row, startCol = 1)
            addStyle(wb, 1, style=title_style, rows=current_row, cols=1)
            current_row <- current_row + 2
            
            # Parameters
            print("Export Button: Writing Parameters...") # DEBUG
            writeData(wb, 1, "Model Parameters", startRow = current_row, startCol = 1)
            addStyle(wb, 1, style=header_style, rows=current_row, cols=1)
            current_row <- current_row + 1
            
            # Calculate rates *within* the download handler using fetched params
            discount_rate <- calculate_discount_rate(params_list)
            terminal_growth_rate <- calculate_terminal_growth_rate(params_list)
            print(paste("Export Button: Calculated rates - Discount:", discount_rate, "Terminal Growth:", terminal_growth_rate)) # DEBUG
            
            params_df_exp <- data.frame(
              Parameter = c("Risk-Free Rate", "Equity Premium", "Beta", "Country Risk Premium",
                            "Discount Rate (calculated)", "Long-run Inflation Expectations",
                            "Additional Terminal Growth", "Terminal Growth Rate (calculated)"),
              Value = c(params_list$risk_free_rate, params_list$equity_premium, params_list$beta,
                        params_list$country_premium, discount_rate * 100, params_list$long_run_inflation,
                        params_list$additional_terminal_growth, terminal_growth_rate * 100),
              Unit = c("%", "%", "", "%", "%", "%", "%", "%")
            )
            
            writeData(wb, 1, params_df_exp, startRow = current_row, startCol = 1, colNames = TRUE)
            addStyle(wb, 1, style=dark_gray_style, rows=current_row, cols=1:3, gridExpand=TRUE)
            addStyle(wb, 1, style=header_row_style, rows=current_row, cols=1:3, gridExpand=TRUE, stack=TRUE)
            current_row <- current_row + 1
            
            addStyle(wb, 1, style=params_numeric_style, rows=current_row:(current_row+nrow(params_df_exp)-1), cols=2:3, gridExpand=TRUE)
            addStyle(wb, 1, style=dark_gray_style, rows=current_row:(current_row+nrow(params_df_exp)-1), cols=1, gridExpand=TRUE)
            
            for (i_style in seq(current_row, current_row+nrow(params_df_exp)-1, by=2)) {
              addStyle(wb, 1, style=white_style, rows=i_style, cols=2:3, gridExpand=TRUE, stack=TRUE)
            }
            for (i_style in seq(current_row+1, current_row+nrow(params_df_exp)-1, by=2)) {
              addStyle(wb, 1, style=light_gray_style, rows=i_style, cols=2:3, gridExpand=TRUE, stack=TRUE)
            }
            current_row <- current_row + nrow(params_df_exp) + 1
            
            
            # Assumptions
            print("Export Button: Writing Assumptions...") # DEBUG
            writeData(wb, 1, "Assumptions Table", startRow = current_row, startCol = 1)
            addStyle(wb, 1, style=header_style, rows=current_row, cols=1)
            current_row <- current_row + 1
            
            writeData(wb, 1, assum_data, startRow = current_row, startCol = 1, colNames = TRUE)
            addStyle(wb, 1, style=dark_gray_style, rows=current_row, cols=1:ncol(assum_data), gridExpand=TRUE)
            addStyle(wb, 1, style=header_row_style, rows=current_row, cols=1:ncol(assum_data), gridExpand=TRUE, stack=TRUE)
            addStyle(wb, 1, style=centered_style, rows=current_row, cols=2:ncol(assum_data), gridExpand=TRUE, stack=TRUE)
            current_row <- current_row + 1
            
            for(i_style in 2:ncol(assum_data)) {
              addStyle(wb, 1, style=assumptions_percent_style, rows=current_row:(current_row+nrow(assum_data)-1), cols=i_style, gridExpand=TRUE)
            }
            addStyle(wb, 1, style=dark_gray_style, rows=current_row:(current_row+nrow(assum_data)-1), cols=1, gridExpand=TRUE)
            
            for (i_style in seq(current_row, current_row+nrow(assum_data)-1, by=2)) {
              addStyle(wb, 1, style=white_style, rows=i_style, cols=2:ncol(assum_data), gridExpand=TRUE, stack=TRUE)
            }
            for (i_style in seq(current_row+1, current_row+nrow(assum_data)-1, by=2)) {
              addStyle(wb, 1, style=light_gray_style, rows=i_style, cols=2:ncol(assum_data), gridExpand=TRUE, stack=TRUE)
            }
            current_row <- current_row + nrow(assum_data) + 1
            
            
            # Projections
            print("Export Button: Writing Projections...") # DEBUG
            writeData(wb, 1, "Projections Table", startRow = current_row, startCol = 1)
            addStyle(wb, 1, style=header_style, rows=current_row, cols=1)
            current_row <- current_row + 1
            
            writeData(wb, 1, proj_data, startRow = current_row, startCol = 1, colNames = TRUE)
            addStyle(wb, 1, style=dark_gray_style, rows=current_row, cols=1:ncol(proj_data), gridExpand=TRUE)
            addStyle(wb, 1, style=header_row_style, rows=current_row, cols=1:ncol(proj_data), gridExpand=TRUE, stack=TRUE)
            addStyle(wb, 1, style=centered_style, rows=current_row, cols=2:ncol(proj_data), gridExpand=TRUE, stack=TRUE)
            current_row <- current_row + 1
            
            for(i_style in 2:ncol(proj_data)) {
              addStyle(wb, 1, style=projections_numeric_style, rows=current_row:(current_row+nrow(proj_data)-1), cols=i_style, gridExpand=TRUE)
            }
            addStyle(wb, 1, style=dark_gray_style, rows=current_row:(current_row+nrow(proj_data)-1), cols=1, gridExpand=TRUE)
            
            for (i_style in seq(current_row, current_row+nrow(proj_data)-1, by=2)) {
              addStyle(wb, 1, style=white_style, rows=i_style, cols=2:ncol(proj_data), gridExpand=TRUE, stack=TRUE)
            }
            for (i_style in seq(current_row+1, current_row+nrow(proj_data)-1, by=2)) {
              addStyle(wb, 1, style=light_gray_style, rows=i_style, cols=2:ncol(proj_data), gridExpand=TRUE, stack=TRUE)
            }
            
            intrinsic_value_row_idx <- which(proj_data$Metric == "Intrinsic Value")
            if(length(intrinsic_value_row_idx) > 0) {
              addStyle(wb, 1, style=bold_style, rows=current_row+intrinsic_value_row_idx-1,
                       cols=1:ncol(proj_data), gridExpand=TRUE, stack=TRUE) # Bold whole row
            }
            
            proportion_row_idx <- which(proj_data$Metric == "Proportion of Market Cap")
            if(length(proportion_row_idx) > 0) {
              # Ensure the market_cap_style formats as percentage correctly
              # The numFmt="0\"%\"" just adds a literal "%", use "0.00%" for actual percent formatting
              market_cap_percent_style <- createStyle(numFmt="0.0%", halign="center", fontName="Lato") # Correct format
              # Apply the correct style
              addStyle(wb, 1, style=market_cap_percent_style, rows=current_row+proportion_row_idx-1,
                       cols=2:ncol(proj_data), gridExpand=TRUE, stack=TRUE)
              # Note: The value in proj_data for "Proportion of Market Cap" should be a decimal (e.g., 1.07)
              # The Excel format "0.0%" will display it as 107.0%
            }
            
            # Set column widths
            print("Export Button: Setting column widths...") # DEBUG
            setColWidths(wb, 1, cols = 1, widths = 35)
            # Use max_cols calculated earlier based on actual data written
            if (!exists("max_cols")) { # Calculate if somehow not defined earlier (e.g., if params failed)
              max_cols <- max(ncol(assum_data), ncol(proj_data))
            }
            if (max_cols > 1) { # Ensure there's at least one data column
              setColWidths(wb, 1, cols = 2:max_cols, widths = 15)
            }
            
            # Save workbook
            print("Export Button: Saving workbook...") # DEBUG
            saveWorkbook(wb, file, overwrite = TRUE)
            print("Export Button: Content function finished successfully.") # DEBUG
            
          }, error = function(e) {
            # Log the error and stop the app from crashing, show notification
            print(paste("Error inside export content function:", e$message))
            showNotification(paste("Error generating Excel export:", e$message), type="error", duration=10)
            # To prevent HTML download, could write an empty file or error message
            # writeLines(paste("Error generating Excel file:", e$message), file)
          }) # End tryCatch
        } # End content function
      ) # End downloadHandler
    } # End if (!is.null(proj_data))
  }) # End observe
  
  # --- Main DCF Projections Trigger Observer ---
  observe({
    # Explicitly depend on the state variables that should trigger recalculation
    assumptions_data <- get_assumptions_data(app_state)
    dcf_params <- app_state$dcf$parameters # Depend on the whole parameter list
    base_revenue <- get_base_revenue(app_state)
    non_op_assets <- get_non_operating_assets(app_state)
    
    # Add req() for minimum viability, but logic inside recalculate_projections handles details
    req(assumptions_data, dcf_params, base_revenue, non_op_assets)
    print("Triggering Main DCF Recalculation Observer")
    recalculate_projections(app_state, auto_triggered = TRUE)
  }) 
  
  # --- Main Server Logic (Inter-module or simple displays) ---
  
  # Display selected data (reads from central state)
  output$selected_data_display <- DT::renderDataTable({
    req(get_selected_data(app_state))
    selected_df_orig <- get_selected_data(app_state)
    datatable_data <- selected_df_orig %>% select(-any_of(c("sheet_name", "import_timestamp"))) # Select display cols
    
    # Use DT options with JS renderer for rounding
    DT::datatable(datatable_data,
                  options = list(
                    pageLength = 10, # Keep original options
                    scrollX = TRUE,
                    scrollY='400px',
                    dom='tipr',
                    # --- ADDED columnDefs with JS from old_code ---
                    columnDefs = list(list(
                      targets = "_all", # Apply to all columns
                      render = JS( # JS function to format numbers for display
                        "function(data, type, row, meta) {",
                        "  if(type === 'display' && data !== null && data !== '') {",
                        "    var num = parseFloat(String(data).replace(/,/g, ''));", # Handle potential commas
                        "    if(!isNaN(num)) {",
                        "      return (Math.round(num * 100) / 100).toLocaleString(undefined, {minimumFractionDigits: 2, maximumFractionDigits: 2});", # Ensure 2 DP
                        "    }",
                        "  }",
                        "  return data;",
                        "}"
                      )
                    )) # End list for columnDefs targets
                    # --- END ADDED columnDefs ---
                  ), # End list for options
                  rownames = FALSE
    ) # No need for %>% formatRound() anymore
  })
  
  # Display import summary (reads from central state)
  output$import_summary_display <- renderText({
    count <- get_import_count(app_state)
    if(count > 0) {
      sheets <- app_state$data$import_metadata$sheets_imported
      paste("Total imported rows:", count, "from", length(sheets), "sheets.")
    } else {
      "No data has been imported yet."
    }
  })
  
  # --- Standardised Data Import ---
  
  # Update base revenue input when the app state changes
  observe({
    base_rev_state <- get_base_revenue(app_state)
    if (!is.null(base_rev_state) && is.numeric(base_rev_state) && 
        base_rev_state != isolate(input$base_revenue_input)) {
      updateNumericInput(session, "base_revenue_input", value = base_rev_state)
    }
  })
  
  # Update NOA input when the app state changes
  observe({
    noa_state <- get_non_operating_assets(app_state)
    if (!is.null(noa_state) && is.numeric(noa_state) && 
        noa_state != isolate(input$non_operating_assets_input)) {
      updateNumericInput(session, "non_operating_assets_input", value = noa_state)
    }
  })
  
  # Function to programmatically click the generate projections button
  trigger_projections_calculation <- function() {
    shinyjs::delay(100, {
      print("Triggering projections calculation via button click")
      shinyjs::click("generate_projections_btn")
    })
  }
  
  # --- DCF Projections Generation & Display (Connects UI to Calculation) ---
  
  observe({
    std_data <- get_standardized_data(app_state)
    assumptions_data <- get_assumptions_data(app_state)
    print("Checking data flow:")
    print(paste("Standardized data available:", !is.null(std_data)))
    print(paste("Assumptions data available:", !is.null(assumptions_data)))
    if (!is.null(assumptions_data)) {
      print(paste("Assumptions columns:", paste(colnames(assumptions_data), collapse=", ")))
    }
  })
  
  # Observe main UI inputs for Base Revenue / NOA and update state
  observeEvent(input$base_revenue_input, { val <- input$base_revenue_input; if (is.numeric(val) && !is.na(val) && val != (app_state$dcf$base_revenue %||% -1)) { set_base_revenue(app_state, val) } })
  observeEvent(input$non_operating_assets_input, { val <- input$non_operating_assets_input; if (is.numeric(val) && !is.na(val) && val != (app_state$dcf$non_operating_assets %||% -1)) { set_non_operating_assets(app_state, val) } })
  observe({ base_rev_state <- get_base_revenue(app_state); if(!is.null(base_rev_state) && isolate(input$base_revenue_input) != base_rev_state) { updateNumericInput(session, "base_revenue_input", value = base_rev_state) } })
  observe({ noa_state <- get_non_operating_assets(app_state); if(!is.null(noa_state) && isolate(input$non_operating_assets_input) != noa_state) { updateNumericInput(session, "non_operating_assets_input", value = noa_state) } })
  
  
  # Connect the generate projections button to the recalculate function (for MAIN DCF)
  observeEvent(input$generate_projections_btn, {
    recalculate_projections(app_state, auto_triggered = FALSE) # Calls server-local helper
  })
  
  # Display projections table (reads from central state)
  output$projections_table_display <- renderDT({
    req(get_projections_data(app_state))
    proj_df_orig <- get_projections_data(app_state)
    
    # --- Data Pre-processing for Display ---
    proj_df_display <- proj_df_orig
    year_cols <- names(proj_df_display)[grepl("^\\d{4}$", names(proj_df_display))]
    rows_to_format_specifically <- c("Revenue CAGR (%)", "Proportion of Market Cap", "FCFE Growth (%)") # Add others?
    metrics_to_round <- setdiff(proj_df_display$Metric, rows_to_format_specifically)
    
    # 1. Round general numeric values in year columns
    for (col in year_cols) {
      # Ensure column exists and is numeric before proceeding
      if(col %in% names(proj_df_display) && is.numeric(proj_df_display[[col]])) {
        # Round rows that DON'T have special formatting
        rows_idx_general <- which(proj_df_display$Metric %in% metrics_to_round)
        proj_df_display[rows_idx_general, col] <- round(proj_df_display[rows_idx_general, col], digits = 0)
      }
    }
    
    # 2. Apply Specific Formatting (convert columns to character as needed)
    proj_df_display[, year_cols] <- lapply(proj_df_display[, year_cols], as.character) # Convert year cols to character
    
    # Format CAGR
    cagr_row_idx <- which(proj_df_display$Metric == "Revenue CAGR (%)")
    if (length(cagr_row_idx) == 1) {
      for (col in year_cols) {
        val <- suppressWarnings(as.numeric(proj_df_orig[cagr_row_idx, col])) # Use original numeric value
        if(is.numeric(val) && !is.na(val)) {
          formatted_val <- paste0(formatC(val, format = "f", digits = 0), "%") # Corrected: No * 100
          proj_df_display[cagr_row_idx, col] <- paste0("<i>", formatted_val, "</i>")
        } else { proj_df_display[cagr_row_idx, col] <- "" }
      }
    }
    # Format FCFE Growth (similar to CAGR)
    fcfe_growth_row_idx <- which(proj_df_display$Metric == "FCFE Growth (%)")
    if (length(fcfe_growth_row_idx) == 1) {
      for (col in year_cols) {
        val <- suppressWarnings(as.numeric(proj_df_orig[fcfe_growth_row_idx, col])) # Use original value
        if(is.numeric(val) && !is.na(val)) {
          formatted_val <- paste0(formatC(val, format = "f", digits = 1), "%") # Corrected: No * 100
          proj_df_display[fcfe_growth_row_idx, col] <- paste0("<i>", formatted_val, "</i>") # Added italics
        } else { proj_df_display[fcfe_growth_row_idx, col] <- "" }
      }
    }
    
    # Format Proportion of Market Cap
    market_cap_row_idx <- which(proj_df_display$Metric == "Proportion of Market Cap")
    if (length(market_cap_row_idx) == 1) {
      for (col in year_cols) {
        val <- suppressWarnings(as.numeric(proj_df_orig[market_cap_row_idx, col])) # Use original numeric value (e.g., 1.07)
        if(is.numeric(val) && !is.na(val)) {
          formatted_val <- paste0(formatC(val * 100, format = "f", digits = 0), "%") # Multiply by 100 here (e.g., 107%)
          proj_df_display[market_cap_row_idx, col] <- formatted_val
        } else { proj_df_display[market_cap_row_idx, col] <- "" }
      }
    }
    # --- End Data Pre-processing ---
    
    
    # 3. Create Datatable (use the display version)
    datatable(proj_df_display,
              options = list(pageLength = -1, scrollX = TRUE, dom = 't', ordering=FALSE),
              rownames = FALSE,
              colnames = c("Metric", names(proj_df_display)[-1]),
              escape = FALSE # Allow HTML italics for CAGR
    ) # No need for formatCurrency/formatPercentage as data is pre-formatted
  })
  
  # --- HURDLE TAB SPECIFIC LOGIC ---
  
  # Observe Hurdle Inputs and update state
  observeEvent(input$hurdle_target_multiple, {
    req(input$hurdle_target_multiple)
    if(is.numeric(input$hurdle_target_multiple)) app_state$hurdle_dcf$hurdle_inputs$target_multiple <- input$hurdle_target_multiple
  })
  observeEvent(input$hurdle_holding_period, {
    req(input$hurdle_holding_period)
    hp <- as.integer(input$hurdle_holding_period)
    if(!is.na(hp) && hp >=1) app_state$hurdle_dcf$hurdle_inputs$holding_period <- hp
  })
  observeEvent(input$hurdle_dilution_pct, {
    req(input$hurdle_dilution_pct)
    if(is.numeric(input$hurdle_dilution_pct)) app_state$hurdle_dcf$hurdle_inputs$dilution_pct <- input$hurdle_dilution_pct
  })
  
  
  # "Calculate Hurdle Base Values" Button Logic
  observeEvent(input$calculate_hurdle_base, {
    print("Calculating Hurdle Base Values...")
    tryCatch({
      # --- Get Required Inputs ---
      main_dcf_proj <- get_projections_data(app_state) # Main DCF projections
      main_dcf_assum <- get_assumptions_data(app_state) # Main DCF assumptions
      main_dcf_params <- reactiveValuesToList(app_state$dcf$parameters) # Main DCF parameters
      hurdle_inputs <- get_hurdle_inputs(app_state) # Target mult, period, dilution

      # Inherit market cap from main DCF for display in Hurdle's parameter panel
      app_state$hurdle_dcf$parameters$market_cap <- main_dcf_params$market_cap %||% 0
      set_hurdle_param_modified(app_state, "market_cap", FALSE) # Reset modified flag
      print(paste("Hurdle panel display market_cap set from DCF tab:", app_state$hurdle_dcf$parameters$market_cap))

      # Calculate the target_future_value for the hurdle multiple calculation
      # This uses the main DCF market cap as the base.
      current_mkt_cap_from_main_dcf <- main_dcf_params$market_cap %||% 0
      target_mult <- hurdle_inputs$target_multiple %||% 1
      dilution <- hurdle_inputs$dilution_pct %||% 0
      calculated_target_future_value <- 0
      if (current_mkt_cap_from_main_dcf > 0 && (1 - dilution/100) > 0) {
        calculated_target_future_value <- current_mkt_cap_from_main_dcf * target_mult / (1 - dilution/100)
      } else if (current_mkt_cap_from_main_dcf > 0) {
        print("Warning: Dilution >= 100%, target future value for multiple calc cannot be determined meaningfully.")
        calculated_target_future_value <- NA
      }
      app_state$hurdle_dcf$target_future_value_for_calc <- calculated_target_future_value # Store for use in projection calc
      print(paste("Calculated target_future_value_for_calc (for Hurdle Multiple):", app_state$hurdle_dcf$target_future_value_for_calc))
      
      req(main_dcf_proj, main_dcf_assum, main_dcf_params, hurdle_inputs) # Ensure required data exists
      
      holding_period <- as.integer(hurdle_inputs$holding_period %||% 5)
      main_proj_years <- colnames(main_dcf_proj)[grepl("^\\d{4}$", colnames(main_dcf_proj))]
      
      if (length(main_proj_years) < holding_period) {
        stop(paste("Holding period (", holding_period, ") exceeds available projection years (", length(main_proj_years), ") in main DCF."))
      }
      
      # --- Determine Hurdle Start Year and Base Revenue ---
      exit_year_main_dcf_idx <- holding_period
      exit_year_main_dcf_col <- main_proj_years[exit_year_main_dcf_idx]
      hurdle_base_revenue <- main_dcf_proj[main_dcf_proj$Metric == "Revenue", exit_year_main_dcf_col]
      
      if (is.null(hurdle_base_revenue) || !is.numeric(hurdle_base_revenue) || is.na(hurdle_base_revenue)) {
        stop(paste("Could not retrieve valid Revenue for exit year", exit_year_main_dcf_col, "from main DCF."))
      }
      
      hurdle_start_year <- as.numeric(exit_year_main_dcf_col) + 1
      print(paste("Hurdle DCF Start Year:", hurdle_start_year))
      print(paste("Hurdle Base Revenue:", hurdle_base_revenue))
      
      # --- Set Hurdle Base Values in State ---
      set_programmatic_update(app_state, TRUE) # Prevent loops
      
      set_hurdle_base_revenue(app_state, hurdle_base_revenue)
      # Use initial NOA for hurdle NOA (simplification)
      set_hurdle_non_operating_assets(app_state, get_non_operating_assets(app_state))
      # Set last_hist_year for hurdle DCF (year before it starts)
      app_state$hurdle_dcf$last_hist_year <- hurdle_start_year - 1
      
      
      # --- Initialize Hurdle Parameters (copy from main DCF element-wise) ---
      # main_dcf_params is already available as a list from earlier req()
      current_hurdle_cap <- app_state$hurdle_dcf$parameters$CAP %||% 10 # Store current hurdle CAP
      
      # Copy parameters one by one to preserve reactivity of the list itself
      print("Copying parameters from main DCF to hurdle DCF...")
      for(param_name in names(main_dcf_params)) {
        # Don't copy CAP (keep hurdle's own) or market_cap (handled above for display)
        if (param_name != "CAP" && param_name != "market_cap" && param_name %in% names(app_state$hurdle_dcf$parameters)) {
          # Update the reactive value element directly
          app_state$hurdle_dcf$parameters[[param_name]] <- main_dcf_params[[param_name]]
          print(paste("  Copied", param_name, ":", main_dcf_params[[param_name]]))
        }
      }
      # Restore the original hurdle CAP after copying others
      app_state$hurdle_dcf$parameters$CAP <- current_hurdle_cap
      print(paste("  Restored Hurdle CAP:", current_hurdle_cap))
      
      print("Resetting modified flags for copied hurdle parameters (excluding market_cap)...")
      for(param_name in names(main_dcf_params)) {
        if (param_name != "CAP" && param_name != "market_cap" && param_name %in% names(app_state$ui$hurdle_params_modified)) {
          set_hurdle_param_modified(app_state, param_name, FALSE)
          print(paste("  Reset modified flag for hurdle parameter:", param_name))
        }
      }
      
      # --- Initialize Hurdle Assumptions ---
      # Copy last year's assumptions from main DCF as starting point for hurdle
      hurdle_cap <- current_hurdle_cap
      hurdle_proj_years <- seq(from = hurdle_start_year, length.out = hurdle_cap)
      hurdle_proj_year_cols <- as.character(hurdle_proj_years)
      last_main_dcf_assum_col <- tail(colnames(main_dcf_assum)[grepl("^\\d{4}$", colnames(main_dcf_assum))], 1)
      
      new_hurdle_assumptions <- main_dcf_assum[, "Assumption", drop = FALSE]
      if (length(last_main_dcf_assum_col) == 1) {
        # Use last year of main DCF assumptions as the value for ALL hurdle years initially
        last_values <- main_dcf_assum[[last_main_dcf_assum_col]]
        for(col in hurdle_proj_year_cols) {
          new_hurdle_assumptions[[col]] <- last_values
        }
      } else {
        # Fallback: Use 5% if main DCF assumptions invalid
        for(col in hurdle_proj_year_cols) {
          new_hurdle_assumptions[[col]] <- 0.05
        }
      }
      set_hurdle_assumptions_data(app_state, new_hurdle_assumptions)
      
      
      set_programmatic_update(app_state, FALSE) # Release lock
      
      # Trigger hurdle projections recalculation (use delay?)
      shinyjs::delay(100, {
        print("Triggering hurdle projection calculation after base value calc.")
        recalculate_hurdle_projections(app_state, auto_triggered = TRUE)
      })
      
      
    }, error = function(e) {
      print(paste("Error calculating hurdle base values:", e$message))
      showNotification(paste("Error:", e$message), type="error", duration=8)
      set_programmatic_update(app_state, FALSE) # Ensure lock is released on error
    })
  })
  
  # --- Hurdle Projections Trigger Observer ---
  observe({
    # Depend on hurdle specific state elements
    hurdle_assumptions_data <- get_hurdle_assumptions_data(app_state)
    hurdle_dcf_params <- app_state$hurdle_dcf$parameters
    hurdle_base_revenue <- get_hurdle_base_revenue(app_state)
    hurdle_non_op_assets <- get_hurdle_non_operating_assets(app_state)
    hurdle_inputs_react <- get_hurdle_inputs(app_state) # Depend on hurdle inputs too
    
    req(hurdle_assumptions_data, hurdle_dcf_params, hurdle_base_revenue, hurdle_non_op_assets, hurdle_inputs_react)
    
    print("Triggering Hurdle DCF Recalculation Observer")
    recalculate_hurdle_projections(app_state, auto_triggered = TRUE)
  })
  
  
  # --- Display Hurdle Projections Table ---
  output$hurdle_projections_table_display <- renderDT({
    req(get_hurdle_projections_data(app_state))
    hurdle_proj_df_orig <- get_hurdle_projections_data(app_state)
    
    # --- Data Pre-processing for Hurdle Display ---
    hurdle_proj_df_display <- hurdle_proj_df_orig
    year_cols <- names(hurdle_proj_df_display)[grepl("^\\d{4}$", names(hurdle_proj_df_display))]
    
    # Identify rows needing special formatting
    hurdle_multiple_row_name <- "Hurdle Multiple Achieved (x)"
    rows_to_format_percent <- c("Revenue CAGR (%)", "FCFE Growth (%)")
    # Exclude the hurdle multiple row from general rounding
    metrics_to_round <- setdiff(hurdle_proj_df_display$Metric, c(rows_to_format_percent, hurdle_multiple_row_name))
    
    # 1. Round general numeric values
    for (col in year_cols) {
      if(col %in% names(hurdle_proj_df_display) && is.numeric(hurdle_proj_df_display[[col]])) {
        rows_idx_general <- which(hurdle_proj_df_display$Metric %in% metrics_to_round)
        if(length(rows_idx_general) > 0) {
          hurdle_proj_df_display[rows_idx_general, col] <- round(hurdle_proj_df_display[rows_idx_general, col], digits = 0)
        }
      }
    }
    
    # 2. Apply Specific Formatting (convert cols to character)
    if (length(year_cols) > 0) {
      first_year_col_name <- year_cols[1]

      for (col_idx in seq_along(year_cols)) {
        col_name_char <- year_cols[col_idx]

        for (row_idx in 1:nrow(hurdle_proj_df_display)) {
          metric_name <- hurdle_proj_df_display$Metric[row_idx]
          current_val <- hurdle_proj_df_display[row_idx, col_name_char] # This value is already rounded

          is_special_metric_row <- metric_name %in% c("Revenue CAGR (%)", "FCFE Growth (%)", "Hurdle Multiple Achieved (x)")

          if (!is_special_metric_row && is.numeric(current_val) && !is.na(current_val)) {
            # For the first year column, format with nsmall = 0.
            # For other year columns, format with nsmall = 2.
            # This applies to general numeric rows that have been rounded.
            if (col_name_char == first_year_col_name) {
              hurdle_proj_df_display[row_idx, col_name_char] <- format(current_val, big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = 0)
            } else {
              hurdle_proj_df_display[row_idx, col_name_char] <- format(current_val, big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = 2)
            }
          } else if (!is_numeric(current_val) || is.na(current_val)) {
            # If already character (e.g. from a previous incorrect format) or NA, ensure it's character for DT.
            # Special metric rows will also fall here initially if their rounded numeric value was taken.
            # Their specific string formatting later will overwrite this.
            hurdle_proj_df_display[row_idx, col_name_char] <- as.character(current_val)
          }
          # If it's a special metric row and current_val was numeric, it's now a character version of the number.
          # The specific formatting for these rows (e.g., adding '%' or 'x') applied AFTER this loop will handle them.
        }
      }
    }
    
    
    # Format CAGR %
    cagr_row_idx <- which(hurdle_proj_df_orig$Metric == "Revenue CAGR (%)")
    if (length(cagr_row_idx) == 1) {
      for (col in year_cols) {
        val <- suppressWarnings(as.numeric(hurdle_proj_df_orig[cagr_row_idx, col]))
        if(is.numeric(val) && !is.na(val)) {
          hurdle_proj_df_display[cagr_row_idx, col] <- paste0("<i>", formatC(val, digits = 0, format = "f"), "%</i>")
        } else { hurdle_proj_df_display[cagr_row_idx, col] <- "" }
      }
    }
    # Format FCFE Growth %
    fcfe_growth_row_idx <- which(hurdle_proj_df_orig$Metric == "FCFE Growth (%)")
    if (length(fcfe_growth_row_idx) == 1) {
      for (col in year_cols) {
        val <- suppressWarnings(as.numeric(hurdle_proj_df_orig[fcfe_growth_row_idx, col]))
        if(is.numeric(val) && !is.na(val)) {
          hurdle_proj_df_display[fcfe_growth_row_idx, col] <- paste0("<i>", formatC(val, digits = 1, format = "f"), "%</i>") # Added italics
        } else { hurdle_proj_df_display[fcfe_growth_row_idx, col] <- "" }
      }
    }
    
    # Format Hurdle Multiple (e.g., "5.1x")
    hurdle_row_idx <- which(hurdle_proj_df_orig$Metric == hurdle_multiple_row_name)
    if (length(hurdle_row_idx) == 1) {
      # Only format the first year column where the value exists
      first_year_col <- year_cols[1]
      val <- suppressWarnings(as.numeric(hurdle_proj_df_orig[hurdle_row_idx, first_year_col]))
      if(is.numeric(val) && !is.na(val)) {
        hurdle_proj_df_display[hurdle_row_idx, first_year_col] <- paste0(formatC(val, digits = 1, format = "f"), "x")
      } else {
        hurdle_proj_df_display[hurdle_row_idx, first_year_col] <- ""
      }
      # Clear other year columns for this row
      if(length(year_cols) > 1) {
        hurdle_proj_df_display[hurdle_row_idx, year_cols[-1]] <- ""
      }
    }
    # --- End Data Pre-processing ---
    
    
    # 3. Create Datatable
    datatable(hurdle_proj_df_display,
              options = list(pageLength = -1, scrollX = TRUE, dom = 't', ordering=FALSE),
              rownames = FALSE,
              colnames = c("Metric", year_cols),
              escape = FALSE # Allow HTML italics
    )
  })
  
  # --- Setup the HURDLE DCF Excel export ---
  observe({
    hurdle_proj_data <- get_hurdle_projections_data(app_state)
    
    if (!is.null(hurdle_proj_data)) {
      output$export_hurdle_dcf_excel_btn <- downloadHandler(
        filename = function() { paste("hurdle_dcf_valuation_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "") },
        content = function(file) {
          tryCatch({
            req(get_hurdle_assumptions_data(app_state), get_hurdle_projections_data(app_state))
            hurdle_assum_data <- get_hurdle_assumptions_data(app_state)
            hurdle_proj_data <- get_hurdle_projections_data(app_state)
            hurdle_params_list <- reactiveValuesToList(app_state$hurdle_dcf$parameters)
            hurdle_inputs <- get_hurdle_inputs(app_state)
            main_dcf_params <- reactiveValuesToList(app_state$dcf$parameters) # For current market cap
            
            wb <- createWorkbook()
            addWorksheet(wb, "Hurdle DCF Valuation")
            modifyBaseFont(wb, fontSize = 11, fontName = "Lato")
            
            # --- Create Styles (same as main DCF) ---
            title_style <- createStyle(textDecoration="bold", fontSize=16, fontName="Lato")
            header_style <- createStyle(textDecoration="bold", fontSize=14, fontName="Lato")
            header_row_style <- createStyle(textDecoration="bold", fontSize=12, fontName="Lato")
            params_numeric_style <- createStyle(numFmt="0.00", halign="center", fontName="Lato")
            assumptions_percent_style <- createStyle(numFmt="0.00%", halign="center", fontName="Lato")
            projections_numeric_style <- createStyle(numFmt="#,##0", halign="center", fontName="Lato")
            dark_gray_style <- createStyle(fgFill="#E6E6E6", fontName="Lato")
            light_gray_style <- createStyle(fgFill="#F2F2F2", fontName="Lato")
            white_style <- createStyle(fgFill="#FFFFFF", fontName="Lato")
            bold_style <- createStyle(textDecoration="bold", fontName="Lato")
            hurdle_multiple_style <- createStyle(numFmt="0.0\"x\"", halign="center", fontName="Lato") # Format for "5.1x"
            centered_style <- createStyle(halign="center", fontName="Lato")
            
            
            # --- Write Data & Styles ---
            current_row <- 1
            writeData(wb, 1, "Hurdle DCF Valuation Model", startRow = current_row, startCol = 1); addStyle(wb, 1, style=title_style, rows=current_row, cols=1); current_row <- current_row + 2
            
            # --- Hurdle Input Parameters ---
            writeData(wb, 1, "Hurdle Setup Parameters", startRow = current_row, startCol = 1); addStyle(wb, 1, style=header_style, rows=current_row, cols=1); current_row <- current_row + 1
            hurdle_setup_df <- data.frame(
              Parameter = c("Current Market Cap", "Target Return Multiple", "Holding Period (Yrs)", "Estimated Dilution (%)", "Target Future Value (Implied)"),
              Value = c(main_dcf_params$market_cap %||% 0,
                        hurdle_inputs$target_multiple %||% NA,
                        hurdle_inputs$holding_period %||% NA,
                        hurdle_inputs$dilution_pct %||% NA,
                        hurdle_params_list$market_cap %||% NA) # Target value stored here
            )
            writeData(wb, 1, hurdle_setup_df, startRow=current_row, colNames=TRUE); addStyle(wb,1,style=header_row_style,rows=current_row,cols=1:2, gridExpand=TRUE); current_row <- current_row + nrow(hurdle_setup_df) + 1
            
            
            # --- Hurdle DCF Parameters ---
            writeData(wb, 1, "Hurdle DCF Model Parameters", startRow = current_row, startCol = 1); addStyle(wb, 1, style=header_style, rows=current_row, cols=1); current_row <- current_row + 1
            discount_rate <- calculate_discount_rate(hurdle_params_list)
            terminal_growth_rate <- calculate_terminal_growth_rate(hurdle_params_list)
            params_df_exp <- data.frame(
              Parameter = c("Risk-Free Rate", "Equity Premium", "Beta", "Country Risk Premium",
                            "Discount Rate (calculated)", "Long-run Inflation Expectations",
                            "Additional Terminal Growth", "Terminal Growth Rate (calculated)", "Hurdle CAP"),
              Value = c(hurdle_params_list$risk_free_rate, hurdle_params_list$equity_premium, hurdle_params_list$beta,
                        hurdle_params_list$country_premium, discount_rate * 100, hurdle_params_list$long_run_inflation,
                        hurdle_params_list$additional_terminal_growth, terminal_growth_rate * 100, hurdle_params_list$CAP),
              Unit = c("%", "%", "", "%", "%", "%", "%", "%", "Yrs")
            )
            writeData(wb, 1, params_df_exp, startRow = current_row, colNames = TRUE)
            # Apply Styles...
            addStyle(wb, 1, style=dark_gray_style, rows=current_row, cols=1:3, gridExpand=TRUE); addStyle(wb, 1, style=header_row_style, rows=current_row, cols=1:3, gridExpand=TRUE, stack=TRUE); current_row <- current_row + 1
            addStyle(wb, 1, style=params_numeric_style, rows=current_row:(current_row+nrow(params_df_exp)-1), cols=2:3, gridExpand=TRUE)
            addStyle(wb, 1, style=dark_gray_style, rows=current_row:(current_row+nrow(params_df_exp)-1), cols=1, gridExpand=TRUE)
            for (i_style in seq(current_row, current_row+nrow(params_df_exp)-1, by=2)) { addStyle(wb, 1, style=white_style, rows=i_style, cols=2:3, gridExpand=TRUE, stack=TRUE) }
            for (i_style in seq(current_row+1, current_row+nrow(params_df_exp)-1, by=2)) { addStyle(wb, 1, style=light_gray_style, rows=i_style, cols=2:3, gridExpand=TRUE, stack=TRUE) }
            current_row <- current_row + nrow(params_df_exp) + 1
            
            
            # --- Hurdle Assumptions ---
            writeData(wb, 1, "Hurdle Assumptions Table", startRow = current_row, startCol = 1); addStyle(wb, 1, style=header_style, rows=current_row, cols=1); current_row <- current_row + 1
            writeData(wb, 1, hurdle_assum_data, startRow = current_row, colNames = TRUE)
            # Apply Styles...
            addStyle(wb, 1, style=dark_gray_style, rows=current_row, cols=1:ncol(hurdle_assum_data), gridExpand=TRUE); addStyle(wb, 1, style=header_row_style, rows=current_row, cols=1:ncol(hurdle_assum_data), gridExpand=TRUE, stack=TRUE); addStyle(wb, 1, style=centered_style, rows=current_row, cols=2:ncol(hurdle_assum_data), gridExpand=TRUE, stack=TRUE); current_row <- current_row + 1
            for(i_style in 2:ncol(hurdle_assum_data)) { addStyle(wb, 1, style=assumptions_percent_style, rows=current_row:(current_row+nrow(hurdle_assum_data)-1), cols=i_style, gridExpand=TRUE) }
            addStyle(wb, 1, style=dark_gray_style, rows=current_row:(current_row+nrow(hurdle_assum_data)-1), cols=1, gridExpand=TRUE)
            for (i_style in seq(current_row, current_row+nrow(hurdle_assum_data)-1, by=2)) { addStyle(wb, 1, style=white_style, rows=i_style, cols=2:ncol(hurdle_assum_data), gridExpand=TRUE, stack=TRUE) }
            for (i_style in seq(current_row+1, current_row+nrow(hurdle_assum_data)-1, by=2)) { addStyle(wb, 1, style=light_gray_style, rows=i_style, cols=2:ncol(hurdle_assum_data), gridExpand=TRUE, stack=TRUE) }
            current_row <- current_row + nrow(hurdle_assum_data) + 1
            
            
            # --- Hurdle Projections ---
            writeData(wb, 1, "Hurdle Projections Table", startRow = current_row, startCol = 1); addStyle(wb, 1, style=header_style, rows=current_row, cols=1); current_row <- current_row + 1
            writeData(wb, 1, hurdle_proj_data, startRow = current_row, colNames = TRUE)
            # Apply Styles...
            addStyle(wb, 1, style=dark_gray_style, rows=current_row, cols=1:ncol(hurdle_proj_data), gridExpand=TRUE); addStyle(wb, 1, style=header_row_style, rows=current_row, cols=1:ncol(hurdle_proj_data), gridExpand=TRUE, stack=TRUE); addStyle(wb, 1, style=centered_style, rows=current_row, cols=2:ncol(hurdle_proj_data), gridExpand=TRUE, stack=TRUE); current_row <- current_row + 1
            for(i_style in 2:ncol(hurdle_proj_data)) { addStyle(wb, 1, style=projections_numeric_style, rows=current_row:(current_row+nrow(hurdle_proj_data)-1), cols=i_style, gridExpand=TRUE) }
            addStyle(wb, 1, style=dark_gray_style, rows=current_row:(current_row+nrow(hurdle_proj_data)-1), cols=1, gridExpand=TRUE)
            for (i_style in seq(current_row, current_row+nrow(hurdle_proj_data)-1, by=2)) { addStyle(wb, 1, style=white_style, rows=i_style, cols=2:ncol(hurdle_proj_data), gridExpand=TRUE, stack=TRUE) }
            for (i_style in seq(current_row+1, current_row+nrow(hurdle_proj_data)-1, by=2)) { addStyle(wb, 1, style=light_gray_style, rows=i_style, cols=2:ncol(hurdle_proj_data), gridExpand=TRUE, stack=TRUE) }
            
            # Special formatting for Intrinsic Value (Bold)
            intrinsic_value_row_idx <- which(hurdle_proj_data$Metric == "Intrinsic Value")
            if(length(intrinsic_value_row_idx) > 0) { addStyle(wb, 1, style=bold_style, rows=current_row+intrinsic_value_row_idx-1, cols=1:ncol(hurdle_proj_data), gridExpand=TRUE, stack=TRUE) }
            
            # Special formatting for Hurdle Multiple (e.g., 5.1x)
            hurdle_multiple_row_idx <- which(hurdle_proj_data$Metric == "Hurdle Multiple Achieved (x)")
            if(length(hurdle_multiple_row_idx) > 0) {
              addStyle(wb, 1, style=hurdle_multiple_style, rows=current_row+hurdle_multiple_row_idx-1, cols=2:ncol(hurdle_proj_data), gridExpand=TRUE, stack=TRUE)
            }
            
            
            # Set column widths
            setColWidths(wb, 1, cols = 1, widths = 35)
            # Calculate max_cols based on data written
            max_cols_hurdle <- max(ncol(params_df_exp), ncol(hurdle_assum_data), ncol(hurdle_proj_data), ncol(hurdle_setup_df))
            if (max_cols_hurdle > 1) { setColWidths(wb, 1, cols = 2:max_cols_hurdle, widths = 15) }
            
            # Save workbook
            saveWorkbook(wb, file, overwrite = TRUE)
            
          }, error = function(e){
            print(paste("Error exporting hurdle DCF:", e$message))
            showNotification(paste("Hurdle Export failed:", e$message), type="error")
          })
        }
      )
    }
  })
  
  # --- Graphs Display ---
  output$metrics_plots_display <- renderPlot({
    req(get_projections_data(app_state), input$graph_plot_type_select)
    projections <- get_projections_data(app_state)
    plot_type <- input$graph_plot_type_select
    
    metrics_plot_data <- prepare_metrics_plot_data(projections) # Use utility
    
    if (!is.null(metrics_plot_data) && nrow(metrics_plot_data) > 0) {
      p <- ggplot(metrics_plot_data, aes(x = Year, y = Value)) +
        facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
        labs(title = "Financial Projections", x = "Year", y = "Value") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"), strip.text = element_text(size = 8), axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Add geom based on type
      if (plot_type == "points") {
        p <- p + geom_point(size=1.5)
      } else if (plot_type == "lines") {
        p <- p + geom_line() + geom_point(size=1.5)
      } else if (plot_type == "smooth") {
        p <- p + geom_point(size=1.5) + geom_smooth(method = "loess", se = FALSE, span = 0.8, color="red")
      }   else if (plot_type == "poly") {
        # --- START: Exact Interpolation using Splines ---
        # Use splinefun for guaranteed interpolation through points
        interpolation_data <- metrics_plot_data %>%
          filter(!is.na(Year) & !is.na(Value)) %>% # Ensure no NAs for splinefun
          group_by(Metric) %>%
          reframe({
            n_points = n_distinct(Year) # Need distinct points for interpolation
            if (n_points >= 2) { # Spline needs at least 2 distinct points
              # Sort data by Year for splinefun
              data_sorted = arrange(pick(everything()), Year)
              tryCatch({
                # Create the spline interpolation function
                # method="natural" is often smoother at boundaries
                interp_func <- splinefun(data_sorted$Year, data_sorted$Value, method = "natural")
                
                # Generate a dense sequence of years for a smooth plot line
                years_seq = seq(min(data_sorted$Year), max(data_sorted$Year), length.out = 100)
                
                # Predict values using the interpolation function
                predicted_values = interp_func(years_seq)
                
                # Return data frame for plotting the smooth line
                data.frame(Year = years_seq, PredictedValue = predicted_values)
              }, error = function(e) {
                warning(paste("Spline interpolation failed for Metric:", first(Metric), "-", e$message))
                # Return empty df if spline fails for this group
                data.frame(Year=numeric(), PredictedValue=numeric())
              })
            } else {
              # Return empty df if less than 2 distinct points
              data.frame(Year=numeric(), PredictedValue=numeric())
            }
          }) %>%
          ungroup()
        
        # Add original points first, then the calculated interpolation line
        p <- p + geom_point(size=1.5)
        if (nrow(interpolation_data) > 0) {
          p <- p + geom_line(data = interpolation_data, aes(y = PredictedValue), color = "blue")
        }
        # --- END: Exact Interpolation using Splines ---
      } else if (plot_type == "logarithmic") {
        # Keep existing point plot for logarithmic for now, as fit is complex
        p <- p + geom_point(size=1.5)
        # Add a note that logarithmic fit is approximate or needs review
        p <- p + labs(caption = "Logarithmic fit option requires review for accuracy across facets.")
      }
      
      print(p)
    } else {
      plot.new(); title("No projection data available to plot")
    }
  }, height = 1200)
  
}  # End Server


# --- Run App ---
shinyApp(ui = ui, server = server) # Comment out if running via RStudio button