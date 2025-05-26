# --- Component Row Module ---
# File: modules/mod_component_row.R

library(shiny)
library(shinyjs)

componentRowUI <- function(id) {
  ns <- NS(id)
  div(id = ns("component_row_div"),
      style = "display: flex; margin-bottom: 10px; align-items: center;",
      div(style = "flex: 4;",
          # Use uiOutput for the dynamic select input
          uiOutput(ns("series_ui"))
      ),
      div(style = "flex: 1; margin: 0 10px;",
          # Keep other inputs static - they initialize okay
          selectInput(ns("op"), NULL,
                      choices = c("+" = "add", "-" = "subtract"),
                      selected = "add", width = "100%")
      ),
      div(style = "flex: 1;",
          numericInput(ns("factor"), NULL,
                       value = 1, width = "100%")
      ),
      div(style = "margin-left: 10px;",
          actionButton(ns("remove_btn"), NULL,
                       icon = icon("times"), class = "btn-danger btn-sm")
      )
  )
}

componentRowServer <- function(input, output, session, app_state, component_key, series_choices, default_selection) {
  ns <- session$ns
  
  # --- Flag to track if ALL inputs have been initialized ---
  inputs_initialized <- reactiveVal(FALSE)
  
  # --- Render the Select Input UI ---
  output$series_ui <- renderUI({
    # Prepare choices (same logic as before)
    choices_for_select <- c("Select a series" = "")
    if (!is.null(series_choices) && length(series_choices) > 0) {
      series_choices_char <- as.character(series_choices)
      choices_for_select <- c(choices_for_select, setNames(series_choices_char, series_choices_char))
    } else {
      # Add a message if no choices available?
      choices_for_select <- c("No series available" = "")
    }
    
    # Determine selected value
    valid_selection <- default_selection %in% series_choices
    selected_value <- if(valid_selection) default_selection else ""
    
    # Create the actual selectInput
    selectInput(ns("series"), # Use the ID expected by input$series later
                label = NULL,
                choices = choices_for_select,
                selected = selected_value,
                width = "100%")
  })
  
  # --- Simplified Initialization ---
  # Set 'op' to 'add' by default initially. User can change it.
  # We avoid reading parent input here to prevent context errors.
  observe({
    updateSelectInput(session, "op", selected = "add")
    updateNumericInput(session, "factor", value = 1)
  }, once = TRUE) # Use once=TRUE from base observe if suitable, or flag if needed
  
  # --- Remove Button Logic ---
  remove_trigger <- reactiveVal(0)
  observeEvent(input$remove_btn, {
    removeUI(selector = paste0("#", ns("component_row_div")), immediate = TRUE)
    remove_component(app_state, component_key)
    remove_trigger(remove_trigger() + 1)
  }, domain = session)
  
  # --- Return Values ---
  current_values <- reactive({
    # Important: Need req() to ensure input$series exists before reading
    req(input$series)
    list(
      series = input$series,
      op = input$op,
      factor = input$factor %||% 1,
      is_valid = !is.null(input$series) && input$series != ""
    )
  })
  
  return(list(
    values = current_values,
    removed = remove_trigger
  ))
}