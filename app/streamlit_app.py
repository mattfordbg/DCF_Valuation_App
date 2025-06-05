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
        st.write("Placeholder for Data Standardisation UI and Logic.")
        # This is where mod_data_standard_ui() would go

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
            st.write("Logic for 'Calculate Hurdle Base Values' to be implemented.")
            # This would typically calculate target future value (market_cap for hurdle_dcf)
            # and potentially back-solve for a required base_revenue or starting point.

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
            st.write("Hurdle projection generation logic to be implemented.")

        st.subheader("Hurdle Financial Projections")
        if st.session_state.hurdle_dcf.get('projections') is None:
            st.info("Hurdle projections will appear here after generation.")
        else:
            st.dataframe(st.session_state.hurdle_dcf['projections'], use_container_width=True)

        st.download_button(
            label="Export Hurdle DCF Model (Placeholder)",
            data="Placeholder for Hurdle DCF export",
            file_name="hurdle_dcf_model.xlsx",
            mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            key="hurdle_export_btn"
        )

    with tab6:
        st.header("Graphs")
        st.write("Placeholder for Graphs and Visualizations.")
        # This is where mod_graphs_ui() would go

if __name__ == "__main__":
    main()
