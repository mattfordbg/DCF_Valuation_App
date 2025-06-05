import streamlit as st
import pandas as pd
import numpy as np # Will be needed for some default numeric values

def initialize_app_state():
    """
    Initializes the Streamlit session state with a structure analogous
    to the R Shiny app's reactiveValues.
    """
    if 'app_state_initialized' not in st.session_state:
        st.session_state.data = {
            'raw': None,  # Will store the raw DataFrame from an uploaded sheet
            'selected': None,  # Will store the accumulated DataFrame of selected rows
            'import_count': 0,
            'import_metadata': {
                'last_import_time': None,
                'sheets_imported': []
            }
        }

        st.session_state.standardization = {
            'years_detected': [], # List of strings or numbers
            'components': [], # List of numbers (suffixes)
            'defined_metrics': {}, # Dict to store metric_name: metric_data
            'standardized_data': None, # Pandas DataFrame
            'calculation_log': "",
            'last_used_component': "",
            'metric_mappings': {}, # Added for standardization UI
            'replace_na_with_zero': True # Added for standardization processing
        }

        st.session_state.dcf = {
            'assumptions': None, # Will be a Pandas DataFrame, initialized when needed
            'parameters': { # Direct dict, Streamlit handles reactivity
                'risk_free_rate': 0.0,
                'equity_premium': 0.0,
                'beta': 1.0,
                'country_premium': 0.0,
                'long_run_inflation': 2.0, # As percentage points
                'additional_terminal_growth': 0.0, # As percentage points
                'CAP': 10, # Number of years
                'market_cap': 0.0
            },
            'base_revenue': 0.0,
            'non_operating_assets': 0.0,
            'projections': None # Will be a Pandas DataFrame
        }

        st.session_state.hurdle_dcf = {
            'assumptions': None, # Will be a Pandas DataFrame, initialized when needed
            'parameters': {
                'risk_free_rate': 0.0,
                'equity_premium': 0.0,
                'beta': 1.0,
                'country_premium': 0.0,
                'long_run_inflation': 2.0,
                'additional_terminal_growth': 0.0,
                'CAP': 10,
                'market_cap': 0.0 # This will store the target future value for hurdle
            },
            'base_revenue': 0.0,
            'non_operating_assets': 0.0,
            'projections': None, # Will be a Pandas DataFrame
            'hurdle_inputs': {
                'target_multiple': 5.0,
                'holding_period': 5, # Years
                'dilution_pct': 20.0 # As percentage points
            },
            'last_hist_year': None # Numeric year
        }

        st.session_state.ui = {
            'programmatic_update': False, # Flag to prevent reactive loops
            'fade_inputs_modified': [False] * 8, # List of booleans
            'hurdle_fade_inputs_modified': [False] * 8, # List of booleans
            'hurdle_params_modified': {
                'risk_free_rate': False, 'equity_premium': False, 'beta': False,
                'country_premium': False, 'long_run_inflation': False,
                'additional_terminal_growth': False, 'CAP': False
            },
            'imported_file_data': None # Temp storage for uploaded file bytes or path
        }

        st.session_state.app_state_initialized = True
        # print("App state initialized.") # Avoid print in library code

def append_calculation_log(log_text):
    if 'standardization' in st.session_state and 'calculation_log' in st.session_state.standardization:
        st.session_state.standardization['calculation_log'] += log_text + "\n"
    else:
        st.session_state.standardization['calculation_log'] = log_text + "\n"

def add_component_suffix(suffix):
    if 'standardization' not in st.session_state:
        st.session_state.standardization = {} # Initialize if not present
    if 'components' not in st.session_state.standardization:
        st.session_state.standardization['components'] = []

    if isinstance(suffix, (int, float)) and suffix not in st.session_state.standardization['components']:
        st.session_state.standardization['components'].append(suffix)
    elif not isinstance(suffix, (int, float)):
        # Consider logging this or raising an error if strict typing is needed
        pass # print(f"Warning: Suffix '{suffix}' is not numeric.")
    else:
        pass # print(f"Suffix {suffix} already exists.")


def reset_data_state():
    """Resets the data import related parts of the session state."""
    st.session_state.data = {
        'raw': None,
        'selected': None,
        'import_count': 0,
        'import_metadata': {
            'last_import_time': None,
            'sheets_imported': []
        }
    }
    if 'ui' in st.session_state: # Check if ui state exists
        st.session_state.ui['imported_file_data'] = None
    # print("Data state reset.")
