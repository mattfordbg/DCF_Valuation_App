# DCF Valuation App

## Overview

The DCF (Discounted Cash Flow) Valuation App is a tool built with R and Shiny. Its purpose is to provide a comprehensive platform for the financial valuation of companies using the DCF methodology.

## Features

*   **Excel Data Import**: Import financial statements (Income Statement, Balance Sheet, Cash Flow Statement) from Excel files.
*   **Flexible Data Standardization**: A dedicated module for mapping and standardizing imported financial data, including a formula builder to define custom financial metrics.
*   **Detailed DCF Parameter Input**: Allows users to input all necessary components for the DCF model, including:
    *   Cost of Equity components (e.g., risk-free rate, beta, market risk premium).
    *   Terminal growth rate.
    *   Capitalization (CAP) parameters.
*   **Annual Assumption Setting for Projections**: Configure annual assumptions for financial projections (e.g., revenue growth rates, operating margins, tax rates) with multiple input methods:
    *   Manual input for direct control.
    *   Auto-calculation based on historical data averages or trends.
    *   Fade functionality to smoothly transition assumptions from initial values to terminal values over the projection period.
*   **Valuation Tabs**:
    *   **DCF Tab**: Displays the main Discounted Cash Flow valuation results.
    *   **Hurdle Tab**: Provides a separate view for hurdle rate analysis or alternative valuation scenarios.
*   **Visualization**: Generates charts and graphs to visualize key assumptions and projected financial metrics, aiding in understanding the model's dynamics.
*   **Export Capabilities**: Export the complete DCF models, underlying assumptions, and standardized data to Excel or CSV formats for further analysis or reporting.

## Installation

### Prerequisites

*   R: [Download R](https://cran.r-project.org/)
*   RStudio (recommended): [Download RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### Required R Packages

The following R packages are required to run the application. You can install them by running the command below in your R console:

```R
install.packages(c("shiny", "readxl", "DT", "dplyr", "purrr", "shinyjs", "rhandsontable", "openxlsx", "ggplot2", "tidyr", "usethis"))
```

### How to Run

1.  **Clone or Download**:
    *   Clone the repository: `git clone <repository_url>`
    *   Or, download the source code as a ZIP file and extract it.
2.  **Open in RStudio**:
    *   Navigate to the extracted directory.
    *   Open the `DCF_Valuation_App.Rproj` file in RStudio. This will set the correct working directory and manage project dependencies.
3.  **Run the App**:
    *   Open the `app/app.R` file in the RStudio editor.
    *   Click the "Run App" button in the RStudio IDE (usually located at the top of the editor pane).
    *   Alternatively, you can run the app from the R console by executing: `shiny::runApp('app')`

## File Structure

*   `DCF_Valuation_App.Rproj`: The RStudio project file. It helps manage the project environment and dependencies.
*   `app/app.R`: The main application file containing both the User Interface (UI) logic and the server-side logic for the Shiny app.
*   `app/global.R`: (Currently, its functionalities are integrated within `app/app.R`). This file is intended for global settings, loading libraries, and sourcing utility functions or modules that need to be available across the entire application.
*   `app/modules/`: This directory contains various Shiny modules, each responsible for a specific section or functionality of the app:
    *   `data_import/`: Handles the import of financial data from Excel files.
    *   `data_standard/`: Manages the mapping of imported data to a standardized financial statement format.
    *   `formula_builder/`: Provides tools for users to define custom formulas for financial metrics during data standardization.
    *   `dcf_params/`: Allows users to input and manage parameters specific to the DCF valuation model (e.g., discount rates, terminal growth).
    *   `projections/`: Manages the input, calculation, and visualization of financial projection assumptions.
*   `app/utils/`: Contains utility R scripts that provide helper functions for calculations, data manipulation, and state management:
    *   `data_processor.R`: Includes functions for cleaning imported data, detecting financial years, and performing data standardization logic.
    *   `dcf_calculator.R`: Houses the core logic for DCF calculations, including projecting financial statements and calculating discount rates.
    *   `fade_models.R`: Contains functions to implement the "fade" logic for financial assumptions, allowing smooth transitions over time.
    *   `state_manager.R`: Implements centralized state management for the application, ensuring data consistency across different modules and user interactions.
*   `app/www/`: Stores static web assets that can be used by the Shiny app, such as:
    *   `fix_scrolling.js`: A JavaScript file to address specific UI scrolling behaviors.
*   `.gitignore`: Specifies intentionally untracked files and directories that Git should ignore (e.g., user-specific files, large data files).
*   `README.md`: This file, providing an overview and guide to the application.
*   `LICENSE`: Contains the licensing information for the project.

## Workflow Overview

A typical user workflow for the DCF Valuation App is as follows:

1.  **Import Data**: Upload financial statements (Income Statement, Balance Sheet, Cash Flow) from an Excel file using the `Data Import` module.
2.  **Standardize Data**: Map the imported financial line items to a predefined standard template and create custom metrics using the `Data Standardization` and `Formula Builder` modules.
3.  **Set DCF Parameters**: Input key DCF parameters such as the risk-free rate, beta, market risk premium, and terminal growth rate in the `DCF Parameters` module.
4.  **Set Assumptions**: Define annual projections for various financial drivers (e.g., revenue growth, margins, tax rates) in the `Projections` module. Users can manually input these, use historical averages, or apply fade functions.
5.  **View Projections & Valuation**: Analyze the projected financial statements, key ratios, and the resulting DCF valuation in the main valuation tabs ("DCF" and "Hurdle").
6.  **Export Results**: Export the DCF model, assumptions, and standardized data to Excel or CSV for offline analysis or reporting.
