import streamlit as st
import io # Added for Excel export
# Corrected imports for running `streamlit run app/streamlit_app.py` from root,
# or if `app` itself is in PYTHONPATH.
# For `streamlit run -m app.streamlit_app`, `from .utils...` is also correct.
from .utils.state_manager import initialize_app_state
from .data_import_component import data_import_ui
from .utils.dcf_calculator import run_full_dcf_calculation, calculate_discount_rate, calculate_terminal_growth_rate # Added imports

def main():
    st.set_page_config(layout="wide", page_title="DCF Valuation Tool")

    # Initialize session state - this should be one of the first things
    # It's wrapped in a check to run only once per session.
    if 'app_state_initialized' not in st.session_state:
        initialize_app_state()

    st.title("Comprehensive DCF Valuation Tool")

    tab1, tab2, tab3, tab4, tab5, tab6 = st.tabs([
        "Data Import & Preview",
        "View Selected Data",
        "Standardise Data",
        "DCF Analysis",
        "Hurdle Analysis",
        "Graphs"
    ])

    with tab1:
        st.header("Data Import and Preview")
        # data_import_ui function is called here.
        data_import_ui()

    with tab2:
        st.header("View All Accumulated Selected Data")
        # Ensure 'data' and 'selected' keys exist before trying to access them.
        selected_data = st.session_state.get('data', {}).get('selected')

        if selected_data is not None and not selected_data.empty:
            st.info(f"Displaying {len(selected_data)} accumulated rows.")
            st.dataframe(selected_data, use_container_width=True)

            st.subheader("Import Summary")
            import_count = st.session_state.get('data', {}).get('import_count', 0)
            sheets_imported_list = st.session_state.get('data', {}).get('import_metadata', {}).get('sheets_imported', [])
            last_import_time = st.session_state.get('data', {}).get('import_metadata', {}).get('last_import_time', 'N/A')

            st.markdown(f"- **Total Rows Imported:** {import_count}") # import_count is now total rows, not num of import actions
            st.markdown(f"- **Sheets Imported From:** {', '.join(sheets_imported_list) if sheets_imported_list else 'None'}")
            st.markdown(f"- **Last Import Timestamp:** {last_import_time}")

        else:
            st.info("No data has been selected or imported yet. Please use the 'Data Import & Preview' tab.")

    with tab3:
        st.header("Standardise Data")

        selected_data_df = st.session_state.data.get('selected')

        if selected_data_df is None or selected_data_df.empty:
            st.info("Please import data using the 'Data Import & Preview' tab first.")
        else:
            st.subheader("Selected Raw Data for Standardization")
            st.dataframe(selected_data_df.head(), use_container_width=True)

            st.subheader("Map Data to Standard Metrics")
            required_metrics = [
                "Revenue", "Operating Profit", "Interest Expense", "Tax", "Net Income",
                "Depreciation & Amortisation",
                "Working Capital Excluding Cash and Cash Equivalents",
                "Fixed Capital", "Net Debt Issuance", "Non-Operating Assets"
            ]

            # Ensure 'metric_mappings' exists in session state
            if 'metric_mappings' not in st.session_state.standardization:
                st.session_state.standardization['metric_mappings'] = {}

            available_columns = [""] + list(selected_data_df.columns) # Add empty option for no selection

            mapping_cols = st.columns(2)
            for i, metric_name in enumerate(required_metrics):
                with mapping_cols[i % 2]:
                    # Create a unique key for each selectbox
                    selectbox_key = f"map_{metric_name.lower().replace(' ', '_')}"

                    # Get current selection if it exists
                    current_selection = st.session_state.standardization['metric_mappings'].get(metric_name)
                    # Determine index for selectbox, default to 0 (empty string) if not found
                    try:
                        idx = available_columns.index(current_selection) if current_selection in available_columns else 0
                    except ValueError: # Should not happen if "" is in available_columns
                        idx = 0

                    chosen_col = st.selectbox(
                        f"Map: {metric_name}",
                        options=available_columns,
                        index=idx,
                        key=selectbox_key,
                        # on_change can be added here if immediate update is needed without a button
                        # For now, values will be read when "Process" button is clicked
                    )
                    # Store mapping: if user selects empty string, it means unmapped
                    if chosen_col:
                        st.session_state.standardization['metric_mappings'][metric_name] = chosen_col
                    elif metric_name in st.session_state.standardization['metric_mappings']:
                         del st.session_state.standardization['metric_mappings'][metric_name]


            st.subheader("Identify Year Columns")
            # Ensure 'years_detected' exists
            if 'years_detected' not in st.session_state.standardization:
                st.session_state.standardization['years_detected'] = []

            # Filter for potentially numeric columns as options for years
            potential_year_cols = [col for col in selected_data_df.columns if selected_data_df[col].astype(str).str.match(r'^\d{4}(\.\d)?$').any()]


            selected_years = st.multiselect(
                "Select columns representing financial years:",
                options=list(selected_data_df.columns), # Offer all columns
                default=st.session_state.standardization.get('years_detected', []), # Pre-select if already chosen
                key="standardization_years_detected_multiselect"
                # on_change can be added here too, but will rely on button for now
            )
            st.session_state.standardization['years_detected'] = selected_years

            # Checkbox for NA replacement
            # Ensure 'replace_na_with_zero' is initialized in state_manager
            if 'replace_na_with_zero' not in st.session_state.standardization:
                st.session_state.standardization['replace_na_with_zero'] = True # Default

            st.session_state.standardization['replace_na_with_zero'] = st.checkbox(
                "Replace NA/missing values with 0 after standardization",
                value=st.session_state.standardization['replace_na_with_zero'],
                key="std_replace_na_checkbox"
            )


            if st.button("Process and Standardise Data", key="std_process_btn"):
                metric_mappings = st.session_state.standardization.get('metric_mappings', {})
                year_columns_selected = st.session_state.standardization.get('years_detected', [])
                replace_na = st.session_state.standardization.get('replace_na_with_zero', True)

                # Validation
                if not selected_data_df.empty and selected_data_df.iloc[:, 0].name != selected_data_df.columns[0]:
                    # If first column is an index and not a regular column (e.g. after set_index without drop)
                    # This case needs careful handling based on actual data structure.
                    # For now, assume selected_data_df.columns[0] is the metric name column from import.
                    pass # Assuming it's fine for now.

                metric_name_col_in_selected = selected_data_df.columns[0] # Assumption: first col has the metric names

                if not metric_mappings:
                    st.error("No metric mappings defined. Please map your data columns to standard metrics.")
                elif not year_columns_selected:
                    st.error("No year columns selected. Please identify which columns represent financial years.")
                else:
                    st.info("Processing and standardising data...")
                    try:
                        # Ensure year columns are sorted chronologically for processing
                        # This assumes year columns are sortable as strings that represent numbers
                        sorted_year_cols = sorted(year_columns_selected, key=lambda x: float(x) if x.replace('.','',1).isdigit() else float('inf'))

                        standardized_data_dict = {"Metric": required_metrics} # required_metrics defined above for selectboxes

                        for year_col_name in sorted_year_cols: # Iterate through user-selected year columns
                            year_data_values = []
                            for std_metric in required_metrics: # For each standard metric we need
                                source_series_name = metric_mappings.get(std_metric)
                                if source_series_name:
                                    # Find the row in selected_df that matches this source_series_name
                                    # (source_series_name is a value in the first column of selected_df)
                                    row_match = selected_data_df[selected_data_df[metric_name_col_in_selected] == source_series_name]
                                    if not row_match.empty:
                                        # Get the value from the current year column for that matched row
                                        raw_value = row_match[year_col_name].iloc[0]
                                        year_data_values.append(raw_value)
                                    else:
                                        year_data_values.append(None) # Metric mapped but source name not found in data for this year
                                else:
                                    year_data_values.append(None) # Standard metric not mapped by user
                            standardized_data_dict[year_col_name] = year_data_values

                        standardized_df_final = pd.DataFrame(standardized_data_dict)
                        standardized_df_final = standardized_df_final.set_index("Metric") # Set "Metric" as index

                        # Convert data to numeric, coercing errors.
                        for col in standardized_df_final.columns: # Only apply to year columns
                            standardized_df_final[col] = pd.to_numeric(standardized_df_final[col], errors='coerce')

                        if replace_na:
                            standardized_df_final = standardized_df_final.fillna(0)

                        st.session_state.standardization['standardized_data'] = standardized_df_final.reset_index() # Store with 'Metric' as col
                        st.success("Data processed and standardized successfully!")

                        # Automatic updates to DCF state
                        if not standardized_df_final.empty:
                            latest_year_col = standardized_df_final.columns[-1] # Assumes columns are sorted years

                            if "Revenue" in standardized_df_final.index:
                                st.session_state.dcf['base_revenue'] = standardized_df_final.loc["Revenue", latest_year_col]

                            if "Non-Operating Assets" in standardized_df_final.index:
                                noa_val = standardized_df_final.loc["Non-Operating Assets", latest_year_col]
                                st.session_state.dcf['non_operating_assets'] = noa_val if pd.notna(noa_val) else 0.0


                            # Initialize DCF assumptions based on standardized years
                            dcf_assumption_years = list(standardized_df_final.columns) # these are already sorted
                            if dcf_assumption_years:
                                default_dcf_assumptions = {
                                    'Assumption': [
                                        "Revenue Growth", "Operating Profit Margin", "Interest Expense",
                                        "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                                        "Incremental FC", "Net Debt Issuance"
                                    ]
                                }
                                for year in dcf_assumption_years: # Create columns for each standardized year
                                    default_dcf_assumptions[year] = [0.05] * 8 # Default rate
                                st.session_state.dcf['assumptions'] = pd.DataFrame(default_dcf_assumptions)
                                st.session_state.dcf['parameters']['CAP'] = len(dcf_assumption_years)
                                st.info("DCF assumptions and CAP updated based on standardized data years.")

                            # Auto-update Hurdle assumptions as well, similar to DCF
                            if 'hurdle_dcf' in st.session_state:
                                st.session_state.hurdle_dcf['assumptions'] = st.session_state.dcf['assumptions'].copy()
                                st.session_state.hurdle_dcf['parameters']['CAP'] = st.session_state.dcf['parameters']['CAP']
                                if "Revenue" in standardized_df_final.index: # Update hurdle base revenue too.
                                     st.session_state.hurdle_dcf['base_revenue'] = standardized_df_final.loc["Revenue", latest_year_col]
                                if "Non-Operating Assets" in standardized_df_final.index:
                                     noa_val_h = standardized_df_final.loc["Non-Operating Assets", latest_year_col]
                                     st.session_state.hurdle_dcf['non_operating_assets'] = noa_val_h if pd.notna(noa_val_h) else 0.0

                                st.info("Hurdle DCF assumptions and CAP also updated.")


                    except Exception as e:
                        st.error(f"Error during data standardization: {e}")
                        st.session_state.standardization['standardized_data'] = None

            st.subheader("Standardized Data Preview")
            standardized_df = st.session_state.standardization.get('standardized_data')
            if standardized_df is not None and not standardized_df.empty:
                st.dataframe(standardized_df.head(), use_container_width=True)
            else:
                st.info("Standardized data will appear here after processing.")

    with tab4:
        st.header("DCF Analysis")

        # Ensure dcf state structure exists (it should from initialize_app_state)
        if 'dcf' not in st.session_state:
            st.session_state.dcf = {
                'parameters': {},
                'base_revenue': 0.0,
                'non_operating_assets': 0.0,
                'assumptions': None,
                'projections': None
            }
        if 'parameters' not in st.session_state.dcf: # Should be initialized by state_manager
             st.session_state.dcf['parameters'] = {
                'risk_free_rate': 1.0, 'equity_premium': 5.0, 'beta': 1.0,
                'country_premium': 0.5, 'long_run_inflation': 2.0,
                'additional_terminal_growth': 0.0, 'CAP': 10, 'market_cap': 0.0
            }


        # --- DCF Parameters in Sidebar ---
        with st.sidebar: # Parameters will appear in the sidebar for this tab
            st.subheader("DCF Parameters")

            # Helper for on_change to update dcf.parameters
            def update_dcf_param(key):
                st.session_state.dcf['parameters'][key] = st.session_state[f"dcf_param_{key}"]

            st.number_input(
                "Risk-Free Rate (%)",
                value=st.session_state.dcf['parameters'].get('risk_free_rate', 1.0),
                key="dcf_param_risk_free_rate",
                on_change=update_dcf_param, args=('risk_free_rate',), format="%.2f", step=0.1
            )
            st.number_input(
                "Equity Premium (%)",
                value=st.session_state.dcf['parameters'].get('equity_premium', 5.0),
                key="dcf_param_equity_premium",
                on_change=update_dcf_param, args=('equity_premium',), format="%.2f", step=0.1
            )
            st.number_input(
                "Beta",
                value=st.session_state.dcf['parameters'].get('beta', 1.0),
                key="dcf_param_beta",
                on_change=update_dcf_param, args=('beta',), format="%.2f", step=0.05
            )
            st.number_input(
                "Country Risk Premium (%)",
                value=st.session_state.dcf['parameters'].get('country_premium', 0.0),
                key="dcf_param_country_premium",
                on_change=update_dcf_param, args=('country_premium',), format="%.2f", step=0.1
            )
            st.number_input(
                "Long-run Inflation Expectations (%)",
                value=st.session_state.dcf['parameters'].get('long_run_inflation', 2.0),
                key="dcf_param_long_run_inflation",
                on_change=update_dcf_param, args=('long_run_inflation',), format="%.2f", step=0.1
            )
            st.number_input(
                "Additional Terminal Growth (%)",
                value=st.session_state.dcf['parameters'].get('additional_terminal_growth', 0.0),
                key="dcf_param_additional_terminal_growth",
                on_change=update_dcf_param, args=('additional_terminal_growth',), format="%.2f", step=0.1
            )
            st.number_input(
                "Projection Years (CAP)",
                value=st.session_state.dcf['parameters'].get('CAP', 10),
                key="dcf_param_CAP",
                on_change=update_dcf_param, args=('CAP',), step=1, min_value=1, max_value=20
            )
            st.number_input(
                "Market Cap (for comparison)",
                value=st.session_state.dcf['parameters'].get('market_cap', 0.0),
                key="dcf_param_market_cap",
                on_change=update_dcf_param, args=('market_cap',), format="%.0f", step=1000.0
            )

        # --- Main Area in DCF Tab ---
        col_main1, col_main2 = st.columns(2)
        with col_main1:
            st.number_input(
                "Base Revenue (Latest Year)",
                value=float(st.session_state.dcf.get('base_revenue', 0.0)), # Ensure float for number_input
                key="dcf_base_revenue",
                on_change=lambda: st.session_state.dcf.update({'base_revenue': st.session_state.dcf_base_revenue}),
                format="%.0f", step=1000.0
            )
        with col_main2:
            st.number_input(
                "Non-Operating Assets",
                value=float(st.session_state.dcf.get('non_operating_assets', 0.0)), # Ensure float
                key="dcf_non_operating_assets",
                on_change=lambda: st.session_state.dcf.update({'non_operating_assets': st.session_state.dcf_non_operating_assets}),
                format="%.0f", step=100.0
            )

        st.subheader("Assumptions Table")
        if st.session_state.dcf.get('assumptions') is None:
            import pandas as pd # Import pandas here, or at the top of the file
            default_assumptions_data = {
                'Assumption': [
                    "Revenue Growth", "Operating Profit Margin", "Interest Expense",
                    "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                    "Incremental FC", "Net Debt Issuance"
                ],
                str(pd.Timestamp.now().year + 1): [0.05, 0.15, 0.01, 0.20, 0.03, 0.02, 0.04, 0.005] # Example rates
            }
            st.session_state.dcf['assumptions'] = pd.DataFrame(default_assumptions_data)
            st.info("Initialized with default assumptions for the next year. This table will be editable and extendable for more years.")

        # Display assumptions table (will be st.data_editor later)
        # The edited DataFrame from st.data_editor directly updates the session state variable.
        st.session_state.dcf['assumptions'] = st.data_editor(
            st.session_state.dcf['assumptions'],
            key="dcf_assumptions_editor",
            column_config={
                "Assumption": st.column_config.TextColumn(disabled=True)
                # Year columns will be editable by default.
                # We can add specific number_column_config for them if needed for formatting/validation.
            },
            use_container_width=True,
            num_rows="fixed" # Assumptions are fixed, only values change.
        )

        if st.button("Generate Projections", key="dcf_generate_projections_btn"):
            # Retrieve inputs from session state
            dcf_params_list = st.session_state.dcf.get('parameters')
            assumptions_df = st.session_state.dcf.get('assumptions')
            base_revenue = st.session_state.dcf.get('base_revenue')
            non_op_assets = st.session_state.dcf.get('non_operating_assets')

            # Ensure standardization state structure exists, then get standardized_data
            if 'standardization' not in st.session_state:
                 st.session_state.standardization = {'standardized_data': None} # Basic init if not present
            std_data = st.session_state.standardization.get('standardized_data')

            # Basic Validation
            if assumptions_df is None or assumptions_df.empty:
                st.error("Assumptions table is missing or empty. Please ensure assumptions are loaded or defined.")
            elif dcf_params_list is None:
                st.error("DCF parameters are missing.")
            elif base_revenue is None or not isinstance(base_revenue, (int, float)): # Base revenue can be 0
                st.error("Base Revenue is not valid.")
            elif non_op_assets is None or not isinstance(non_op_assets, (int, float)): # NOA can be 0
                st.error("Non-Operating Assets is not valid.")
            else:
                st.info("Generating financial projections...")
                try:
                    projection_result = run_full_dcf_calculation(
                        assumptions_df=assumptions_df,
                        base_revenue=float(base_revenue), # Ensure float
                        non_op_assets=float(non_op_assets), # Ensure float
                        dcf_params_list=dcf_params_list,
                        std_data=std_data
                    )
                    st.session_state.dcf['projections'] = projection_result

                    if projection_result is None:
                        st.warning("Projection generation failed or returned no data. Check input parameters and assumptions. The `run_full_dcf_calculation` function might have encountered an issue (e.g. invalid rates, missing assumption columns for projection years).")
                    else:
                        st.success("Financial projections generated successfully!")
                        # No explicit rerun needed, Streamlit will update the display of projections table
                except Exception as e:
                    st.error(f"An error occurred during projection generation: {e}")
                    st.session_state.dcf['projections'] = None


        st.subheader("Financial Projections")
        if st.session_state.dcf.get('projections') is None:
            st.info("Projections will appear here after generation.")
        else:
            st.dataframe(st.session_state.dcf['projections'], use_container_width=True)

        # --- Excel Export Logic ---
        def _prepare_excel_export():
            output_buffer = io.BytesIO()
            with pd.ExcelWriter(output_buffer, engine='openpyxl') as excel_writer:
                # 1. Parameters Sheet
                raw_params = st.session_state.dcf.get('parameters', {})
                # Make a copy to add calculated rates
                export_params = raw_params.copy()

                # Calculate and add discount rate and terminal growth rate
                # These functions expect dicts with specific keys
                # Ensure the parameter names match exactly what these functions expect
                # (e.g. 'risk_free_rate', not 'Risk-Free Rate (%)')
                # The keys in st.session_state.dcf['parameters'] are already correct.

                discount_rate_val = calculate_discount_rate(raw_params)
                terminal_growth_val = calculate_terminal_growth_rate(raw_params)

                export_params['calculated_discount_rate'] = discount_rate_val
                export_params['calculated_terminal_growth_rate'] = terminal_growth_val

                params_df = pd.DataFrame(list(export_params.items()), columns=['Parameter', 'Value'])
                params_df.to_excel(excel_writer, sheet_name='DCF Parameters', index=False)

                # 2. Assumptions Sheet
                assumptions_df = st.session_state.dcf.get('assumptions')
                if assumptions_df is not None:
                    assumptions_df.to_excel(excel_writer, sheet_name='Assumptions', index=False)
                else: # Write an empty sheet if no assumptions
                    pd.DataFrame().to_excel(excel_writer, sheet_name='Assumptions', index=False)

                # 3. Projections Sheet
                projections_df = st.session_state.dcf.get('projections')
                if projections_df is not None:
                    projections_df.to_excel(excel_writer, sheet_name='Projections', index=False)
                else: # Write an empty sheet if no projections
                    pd.DataFrame().to_excel(excel_writer, sheet_name='Projections', index=False)

            return output_buffer.getvalue()

        excel_bytes_data = _prepare_excel_export()

        st.download_button(
            label="Export Complete DCF Model",
            data=excel_bytes_data,
            file_name="dcf_valuation_model.xlsx",
            mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            key="dcf_export_btn"
        )
        # Feedback for download is implicit by browser download starting.
        # A st.success message here would appear before download is initiated by user.

    with tab5:
        st.header("Hurdle Analysis")

        # Ensure hurdle_dcf state structure exists (from initialize_app_state)
        if 'hurdle_dcf' not in st.session_state: # Should be pre-initialized
            st.session_state.hurdle_dcf = {
                'parameters': {}, 'hurdle_inputs': {}, 'base_revenue': 0.0,
                'non_operating_assets': 0.0, 'assumptions': None, 'projections': None
            }
        if 'parameters' not in st.session_state.hurdle_dcf:
            st.session_state.hurdle_dcf['parameters'] = {
                'risk_free_rate': 1.0, 'equity_premium': 5.0, 'beta': 1.0,
                'country_premium': 0.5, 'long_run_inflation': 2.0,
                'additional_terminal_growth': 0.0, 'CAP': 10, 'market_cap': 0.0 # market_cap is target val here
            }
        if 'hurdle_inputs' not in st.session_state.hurdle_dcf:
             st.session_state.hurdle_dcf['hurdle_inputs'] = {
                'target_multiple': 5.0, 'holding_period': 5, 'dilution_pct': 20.0
            }


        # --- Hurdle DCF Parameters in Sidebar ---
        # These will add to the existing sidebar if DCF tab was also rendered
        with st.sidebar:
            st.subheader("Hurdle DCF Parameters")

            def update_hurdle_dcf_param(key):
                st.session_state.hurdle_dcf['parameters'][key] = st.session_state[f"hurdle_dcf_param_{key}"]

            st.number_input(
                "Risk-Free Rate (%) (Hurdle)",
                value=st.session_state.hurdle_dcf['parameters'].get('risk_free_rate', 1.0),
                key="hurdle_dcf_param_risk_free_rate",
                on_change=update_hurdle_dcf_param, args=('risk_free_rate',), format="%.2f", step=0.1
            )
            # ... (Repeat for equity_premium, beta, country_premium, long_run_inflation, additional_terminal_growth, CAP)
            # For brevity, only showing a few, but all relevant ones from DCF should be here if they can differ.
            # The state_manager initializes these for hurdle_dcf.
            st.number_input(
                "Equity Premium (%) (Hurdle)",
                value=st.session_state.hurdle_dcf['parameters'].get('equity_premium', 5.0),
                key="hurdle_dcf_param_equity_premium",
                on_change=update_hurdle_dcf_param, args=('equity_premium',), format="%.2f", step=0.1
            )
            st.number_input(
                "Beta (Hurdle)",
                value=st.session_state.hurdle_dcf['parameters'].get('beta', 1.0),
                key="hurdle_dcf_param_beta",
                on_change=update_hurdle_dcf_param, args=('beta',), format="%.2f", step=0.05
            )
             # ... and other financial parameters similar to DCF ...
            st.number_input(
                "Projection Years (CAP) (Hurdle)",
                value=st.session_state.hurdle_dcf['parameters'].get('CAP', 10),
                key="hurdle_dcf_param_CAP",
                on_change=update_hurdle_dcf_param, args=('CAP',), step=1, min_value=1, max_value=20
            )


        # --- Main Area in Hurdle Tab ---
        st.subheader("Hurdle Specific Inputs")

        def update_hurdle_input(key):
            st.session_state.hurdle_dcf['hurdle_inputs'][key] = st.session_state[f"hurdle_input_{key}"]

        col_hurdle_inputs1, col_hurdle_inputs2, col_hurdle_inputs3 = st.columns(3)
        with col_hurdle_inputs1:
            st.number_input(
                "Target Return Multiple (x)",
                value=st.session_state.hurdle_dcf['hurdle_inputs'].get('target_multiple', 5.0),
                key="hurdle_input_target_multiple",
                on_change=update_hurdle_input, args=('target_multiple',), format="%.1f", step=0.1
            )
        with col_hurdle_inputs2:
            st.number_input(
                "Holding Period (Yrs)",
                value=st.session_state.hurdle_dcf['hurdle_inputs'].get('holding_period', 5),
                key="hurdle_input_holding_period",
                on_change=update_hurdle_input, args=('holding_period',), step=1, min_value=1
            )
        with col_hurdle_inputs3:
            st.number_input(
                "Estimated Dilution (%)",
                value=st.session_state.hurdle_dcf['hurdle_inputs'].get('dilution_pct', 20.0),
                key="hurdle_input_dilution_pct",
                on_change=update_hurdle_input, args=('dilution_pct',), format="%.1f", step=1.0
            )

        if st.button("Calculate Hurdle Base Values", key="hurdle_calc_base_btn"):
            main_dcf_proj = st.session_state.dcf.get('projections')
            main_dcf_assum = st.session_state.dcf.get('assumptions')
            main_dcf_params = st.session_state.dcf.get('parameters')
            hurdle_inputs = st.session_state.hurdle_dcf.get('hurdle_inputs')

            valid_inputs = True
            if main_dcf_proj is None or main_dcf_proj.empty:
                st.error("Main DCF projections are not available. Please generate DCF projections first.")
                valid_inputs = False
            if main_dcf_params is None:
                st.error("Main DCF parameters are not available.")
                valid_inputs = False
            if hurdle_inputs is None:
                st.error("Hurdle inputs are not available.")
                valid_inputs = False

            if valid_inputs:
                holding_period = hurdle_inputs.get('holding_period', 5)
                if not isinstance(holding_period, int) or holding_period <= 0:
                    st.error("Holding Period must be a positive integer.")
                    valid_inputs = False

            if valid_inputs:
                main_proj_year_cols = [col for col in main_dcf_proj.columns if col.isdigit()]
                if not main_proj_year_cols:
                    st.error("Could not identify year columns in main DCF projections.")
                    valid_inputs = False
                elif holding_period > len(main_proj_year_cols):
                    st.error(f"Holding period ({holding_period} yrs) exceeds available projection years ({len(main_proj_year_cols)} yrs) in main DCF.")
                    valid_inputs = False

                if valid_inputs:
                    try:
                        # Determine Hurdle Base Revenue and Start Year
                        exit_year_column_name = main_proj_year_cols[holding_period - 1]
                        hurdle_base_revenue = main_dcf_proj.loc["Revenue", exit_year_column_name]
                        hurdle_start_year = int(exit_year_column_name) + 1

                        st.session_state.hurdle_dcf['base_revenue'] = hurdle_base_revenue
                        st.session_state.hurdle_dcf['non_operating_assets'] = st.session_state.dcf.get('non_operating_assets', 0.0)
                        st.session_state.hurdle_dcf['last_hist_year'] = hurdle_start_year - 1

                        # Initialize/Update Hurdle DCF Parameters
                        # Copy relevant params, exclude 'CAP' and 'market_cap' initially
                        params_to_copy = ['risk_free_rate', 'equity_premium', 'beta', 'country_premium',
                                          'long_run_inflation', 'additional_terminal_growth']
                        for p_key in params_to_copy:
                            if p_key in main_dcf_params:
                                st.session_state.hurdle_dcf['parameters'][p_key] = main_dcf_params[p_key]
                        # Hurdle CAP might be different, retain its own or default if not set from main DCF explicitly
                        # st.session_state.hurdle_dcf['parameters']['CAP'] = main_dcf_params.get('CAP', 10) # Or keep separate

                        # Calculate target_future_value for hurdle
                        current_mkt_cap = main_dcf_params.get('market_cap', 0)
                        target_mult = hurdle_inputs.get('target_multiple', 1)
                        dilution_pct = hurdle_inputs.get('dilution_pct', 0)

                        if (1 - dilution_pct / 100) == 0:
                            st.error("Dilution percentage cannot be 100%.")
                            target_future_value = 0 # Or handle as error
                        else:
                            target_future_value = current_mkt_cap * target_mult / (1 - dilution_pct / 100)
                        st.session_state.hurdle_dcf['parameters']['market_cap'] = target_future_value

                        # Initialize/Update Hurdle Assumptions
                        hurdle_cap_years = st.session_state.hurdle_dcf['parameters'].get('CAP', 10)
                        new_hurdle_assumption_cols = [str(hurdle_start_year + i) for i in range(hurdle_cap_years)]

                        hurdle_assumptions_df = pd.DataFrame({
                            'Assumption': [
                                "Revenue Growth", "Operating Profit Margin", "Interest Expense",
                                "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                                "Incremental FC", "Net Debt Issuance"
                            ]
                        })

                        if main_dcf_assum is not None and not main_dcf_assum.empty:
                            main_dcf_year_cols = [col for col in main_dcf_assum.columns if col != 'Assumption' and col.isdigit()]
                            if main_dcf_year_cols:
                                last_year_main_assum_values = main_dcf_assum[main_dcf_year_cols[-1]]
                                for new_col in new_hurdle_assumption_cols:
                                    hurdle_assumptions_df[new_col] = last_year_main_assum_values
                            else: # Fallback if main DCF assumptions have no year columns
                                for new_col in new_hurdle_assumption_cols:
                                    hurdle_assumptions_df[new_col] = [0.07] * len(hurdle_assumptions_df) # Default values
                        else: # Fallback if no main DCF assumptions
                            for new_col in new_hurdle_assumption_cols:
                                hurdle_assumptions_df[new_col] = [0.07] * len(hurdle_assumptions_df) # Default values

                        st.session_state.hurdle_dcf['assumptions'] = hurdle_assumptions_df
                        st.success("Hurdle base values calculated and assumptions initialized.")
                        st.experimental_rerun() # To update UI with new state values

                    except KeyError as e:
                        st.error(f"Error accessing data (likely 'Revenue' in projections or year column): {e}. Ensure main DCF projections are complete.")
                    except IndexError as e:
                        st.error(f"Error accessing projection year (Holding period might be too long): {e}")
                    except Exception as e:
                        st.error(f"An unexpected error occurred: {e}")


        col_hurdle_main1, col_hurdle_main2 = st.columns(2)
        with col_hurdle_main1:
            st.number_input(
                "Base Revenue (for Hurdle DCF)",
                value=float(st.session_state.hurdle_dcf.get('base_revenue', 0.0)),
                key="hurdle_dcf_base_revenue",
                on_change=lambda: st.session_state.hurdle_dcf.update({'base_revenue': st.session_state.hurdle_dcf_base_revenue}),
                format="%.0f", step=1000.0,
                help="This might be auto-calculated by 'Calculate Hurdle Base Values'"
            )
        with col_hurdle_main2:
            st.number_input(
                "Non-Operating Assets (for Hurdle DCF)",
                value=float(st.session_state.hurdle_dcf.get('non_operating_assets', 0.0)),
                key="hurdle_dcf_non_operating_assets",
                on_change=lambda: st.session_state.hurdle_dcf.update({'non_operating_assets': st.session_state.hurdle_dcf_non_operating_assets}),
                format="%.0f", step=100.0
            )

        st.subheader("Hurdle Assumptions Table")
        if st.session_state.hurdle_dcf.get('assumptions') is None:
            # Re-use pandas import from top if already there
            default_hurdle_assumptions_data = {
                'Assumption': [
                    "Revenue Growth", "Operating Profit Margin", "Interest Expense",
                    "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
                    "Incremental FC", "Net Debt Issuance"
                ],
                str(pd.Timestamp.now().year + 1): [0.07] * 8 # Slightly different defaults for hurdle
            }
            st.session_state.hurdle_dcf['assumptions'] = pd.DataFrame(default_hurdle_assumptions_data)
            st.info("Initialized with default hurdle assumptions.")

        st.session_state.hurdle_dcf['assumptions'] = st.data_editor(
            st.session_state.hurdle_dcf['assumptions'],
            key="hurdle_assumptions_editor",
            column_config={"Assumption": st.column_config.TextColumn(disabled=True)},
            use_container_width=True,
            num_rows="fixed"
        )

        if st.button("Generate Hurdle Projections", key="hurdle_generate_projections_btn"):
            # Retrieve inputs from hurdle_dcf session state
            hurdle_params_list = st.session_state.hurdle_dcf.get('parameters')
            hurdle_assumptions_df = st.session_state.hurdle_dcf.get('assumptions')
            hurdle_base_revenue = st.session_state.hurdle_dcf.get('base_revenue')
            hurdle_non_op_assets = st.session_state.hurdle_dcf.get('non_operating_assets')
            # std_data is not directly used for hurdle projections in the same way as main DCF,
            # as last_hist_year is derived from main DCF's exit.
            # calculate_dcf_projections will infer last_hist_year from assumptions_df if std_data is None.

            # Basic Validation
            if hurdle_assumptions_df is None or hurdle_assumptions_df.empty:
                st.error("Hurdle Assumptions table is missing or empty. Please calculate hurdle base values first.")
            elif hurdle_params_list is None:
                st.error("Hurdle DCF parameters are missing.")
            elif hurdle_base_revenue is None or not isinstance(hurdle_base_revenue, (int, float)):
                st.error("Hurdle Base Revenue is not valid.")
            elif hurdle_non_op_assets is None or not isinstance(hurdle_non_op_assets, (int, float)):
                st.error("Hurdle Non-Operating Assets is not valid.")
            else:
                st.info("Generating hurdle financial projections...")
                try:
                    hurdle_projection_result = run_full_dcf_calculation(
                        assumptions_df=hurdle_assumptions_df,
                        base_revenue=float(hurdle_base_revenue),
                        non_op_assets=float(hurdle_non_op_assets),
                        dcf_params_list=hurdle_params_list,
                        std_data=None # Pass None, last_hist_year is set in hurdle_dcf or inferred by func
                    )
                    st.session_state.hurdle_dcf['projections'] = hurdle_projection_result

                    if hurdle_projection_result is None:
                        st.warning("Hurdle projection generation failed. Check parameters and assumptions.")
                    else:
                        st.success("Hurdle financial projections generated successfully!")
                except Exception as e:
                    st.error(f"An error occurred during hurdle projection generation: {e}")
                    st.session_state.hurdle_dcf['projections'] = None

        st.subheader("Hurdle Financial Projections")
        if st.session_state.hurdle_dcf.get('projections') is None:
            st.info("Hurdle projections will appear here after generation.")
        else:
            st.dataframe(st.session_state.hurdle_dcf['projections'], use_container_width=True)

        # --- Hurdle Excel Export Logic ---
        def _prepare_hurdle_excel_export():
            output_buffer = io.BytesIO()
            with pd.ExcelWriter(output_buffer, engine='openpyxl') as excel_writer:
                # 1. Hurdle Setup Parameters Sheet
                hurdle_inputs = st.session_state.hurdle_dcf.get('hurdle_inputs', {})
                main_dcf_market_cap = st.session_state.dcf.get('parameters', {}).get('market_cap', 0)
                hurdle_target_future_value = st.session_state.hurdle_dcf.get('parameters', {}).get('market_cap', 0) # This is the calculated target

                hurdle_setup_data = {
                    "Parameter": [
                        "Current Market Cap (from Main DCF)",
                        "Target Return Multiple (x)",
                        "Holding Period (Yrs)",
                        "Estimated Dilution (%)",
                        "Target Future Value (Implied)"
                    ],
                    "Value": [
                        main_dcf_market_cap,
                        hurdle_inputs.get('target_multiple'),
                        hurdle_inputs.get('holding_period'),
                        hurdle_inputs.get('dilution_pct'),
                        hurdle_target_future_value
                    ]
                }
                hurdle_setup_df = pd.DataFrame(hurdle_setup_data)
                hurdle_setup_df.to_excel(excel_writer, sheet_name='Hurdle Setup Parameters', index=False)

                # 2. Hurdle DCF Parameters Sheet
                raw_hurdle_params = st.session_state.hurdle_dcf.get('parameters', {})
                export_hurdle_params = raw_hurdle_params.copy()

                hurdle_discount_rate = calculate_discount_rate(raw_hurdle_params)
                hurdle_terminal_growth = calculate_terminal_growth_rate(raw_hurdle_params)

                export_hurdle_params['calculated_discount_rate'] = hurdle_discount_rate
                export_hurdle_params['calculated_terminal_growth_rate'] = hurdle_terminal_growth

                hurdle_params_df = pd.DataFrame(list(export_hurdle_params.items()), columns=['Parameter', 'Value'])
                hurdle_params_df.to_excel(excel_writer, sheet_name='Hurdle DCF Parameters', index=False)

                # 3. Hurdle Assumptions Sheet
                hurdle_assumptions_df = st.session_state.hurdle_dcf.get('assumptions')
                if hurdle_assumptions_df is not None:
                    hurdle_assumptions_df.to_excel(excel_writer, sheet_name='Hurdle Assumptions', index=False)
                else:
                    pd.DataFrame().to_excel(excel_writer, sheet_name='Hurdle Assumptions', index=False)

                # 4. Hurdle Projections Sheet
                hurdle_projections_df = st.session_state.hurdle_dcf.get('projections')
                if hurdle_projections_df is not None:
                    hurdle_projections_df.to_excel(excel_writer, sheet_name='Hurdle Projections', index=False)
                else:
                    pd.DataFrame().to_excel(excel_writer, sheet_name='Hurdle Projections', index=False)

            return output_buffer.getvalue()

        hurdle_excel_bytes_data = _prepare_hurdle_excel_export()

        st.download_button(
            label="Export Hurdle DCF Model",
            data=hurdle_excel_bytes_data,
            file_name="hurdle_dcf_valuation_model.xlsx",
            mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            key="hurdle_export_btn"
        )

    with tab6:
        st.header("Financial Projection Graphs (DCF Analysis)")

        projections_df = st.session_state.dcf.get('projections')

        if projections_df is None or projections_df.empty:
            st.info("Please generate DCF projections first on the 'DCF Analysis' tab to view graphs.")
        else:
            # Ensure 'Metric' is the index for easier data selection, or adapt to use it as a column.
            # The projections_df from dcf_calculator has 'Metric' as index and years as columns.
            # For st.line_chart, data should ideally be: index=years, columns=metrics or x='Year', y='Value'

            # Make a copy to manipulate for plotting
            plot_df = projections_df.copy()
            if 'Metric' in plot_df.columns: # If Metric is a column (e.g. after reset_index)
                plot_df = plot_df.set_index('Metric')

            # Convert year columns to numeric if they are strings, and sort them
            # This is important for st.line_chart to render x-axis correctly.
            try:
                year_columns = sorted([col for col in plot_df.columns if str(col).isdigit()], key=int)
                plot_df = plot_df[year_columns] # Keep only sorted year columns for data part
                plot_df.columns = pd.to_numeric(plot_df.columns) # Convert column headers to numeric years
            except Exception as e:
                st.error(f"Could not process year columns for plotting: {e}")
                st.stop()


            key_metrics_to_plot = [
                "Revenue", "Operating Profit", "Net Income", "Free Cash Flow to Equity",
                "Revenue CAGR (%)", "FCFE Growth (%)"
                # Add more metrics as desired, e.g., "Present Value of FCFE"
            ]

            for metric in key_metrics_to_plot:
                if metric in plot_df.index:
                    st.subheader(f"Chart: {metric}")
                    # Data for st.line_chart: DataFrame where index is x-axis, columns are series.
                    # Here, we have one series per metric.
                    metric_data = plot_df.loc[metric]
                    # Ensure data is numeric for plotting, convert if necessary
                    metric_data_numeric = pd.to_numeric(metric_data, errors='coerce')

                    if not metric_data_numeric.empty:
                        # st.line_chart expects data where columns are different lines.
                        # If metric_data_numeric is a Series, convert to DataFrame.
                        chart_data_df = metric_data_numeric.to_frame(name=metric)
                        chart_data_df.index.name = "Year" # Set index name for clarity on x-axis
                        st.line_chart(chart_data_df)
                    else:
                        st.warning(f"No numeric data available to plot for {metric}.")
                else:
                    st.warning(f"Metric '{metric}' not found in projections for plotting.")

            # Example of plotting multiple related metrics on one chart if desired:
            # st.subheader("Key Financials Overview")
            # metrics_for_overview = ["Revenue", "Operating Profit", "Net Income"]
            # overview_df = plot_df.loc[plot_df.index.intersection(metrics_for_overview)].T # Transpose: Years as index, Metrics as columns
            # if not overview_df.empty:
            #    st.line_chart(overview_df)
            # else:
            #    st.warning("Could not generate overview chart for key financials.")


if __name__ == "__main__":
    main()
