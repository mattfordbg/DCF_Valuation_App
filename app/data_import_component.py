import streamlit as st
import pandas as pd
from datetime import datetime

def data_import_ui():
    """
    Handles the UI for data import, including file upload, sheet selection,
    and basic data accumulation.
    """
    st.sidebar.markdown("### Import Reminders")
    st.sidebar.markdown("""
    - Ensure your Excel file is not password protected.
    - Data should be in a structured table format.
    - The first row after any skipped rows should contain headers.
    - Ensure 'Metric' is the first column if data is standardized.
    """)

    st.header("Data Import Section")

    uploaded_file = st.file_uploader("Upload Excel File", type=["xlsx", "xls"])

    if 'sheet_names' not in st.session_state:
        st.session_state.sheet_names = []
    if 'selected_sheet' not in st.session_state:
        st.session_state.selected_sheet = None
    # Use 'editable_preview_df' for the DataFrame shown in st.data_editor
    if 'editable_preview_df' not in st.session_state:
        st.session_state.editable_preview_df = None
    if 'last_uploaded_filename' not in st.session_state:
        st.session_state.last_uploaded_filename = None
    # Key for the data_editor widget itself to retrieve its state
    DATA_EDITOR_KEY = "data_editor_preview"

    # Function to load and prepare sheet for preview
    def load_sheet_for_editing(file_buffer, sheet_name):
        try:
            current_xls = pd.ExcelFile(file_buffer)
            df_sheet = pd.read_excel(current_xls, sheet_name=sheet_name, header=0)

            # Add 'Select' column, 'sheet_name_temp', 'temp_row_id'
            df_sheet.insert(0, 'Select', False)
            df_sheet['sheet_name_temp'] = sheet_name
            df_sheet['temp_row_id'] = range(len(df_sheet)) # Simple row id

            st.session_state.editable_preview_df = df_sheet
            # Clear previous editor state by changing its key or resetting it if possible,
            # or rely on Streamlit's default behavior when data changes.
            # Forcing a rerun after loading new sheet can help reset editor if it holds onto old state.
            # st.experimental_rerun() # Might be too disruptive here, manage editor state carefully.

        except Exception as e:
            st.error(f"Error reading selected sheet '{sheet_name}': {e}")
            st.session_state.editable_preview_df = None

    if uploaded_file is not None:
        if st.session_state.last_uploaded_filename != uploaded_file.name:
            st.session_state.sheet_names = []
            st.session_state.selected_sheet = None
            st.session_state.editable_preview_df = None # Clear old preview
            try:
                xls = pd.ExcelFile(uploaded_file)
                st.session_state.sheet_names = xls.sheet_names
                st.session_state.last_uploaded_filename = uploaded_file.name
                if st.session_state.sheet_names:
                    st.session_state.selected_sheet = st.session_state.sheet_names[0]
                    # Automatically load the first sheet for preview
                    load_sheet_for_editing(uploaded_file, st.session_state.selected_sheet)
            except Exception as e:
                st.error(f"Error reading Excel file sheets: {e}")
                return

        if not st.session_state.sheet_names:
            st.warning("No sheets found or unable to read sheets.")
            return

        col1, col2 = st.columns([3,1])
        with col1:
            # Use a key for selectbox to allow programmatic updates if needed
            # However, direct update of st.session_state.selected_sheet before rerun is usually enough
            selected_sheet_name = st.selectbox(
                "Select Sheet",
                st.session_state.sheet_names,
                key="sheet_selector", # Added key
                index=st.session_state.sheet_names.index(st.session_state.selected_sheet) if st.session_state.selected_sheet and st.session_state.selected_sheet in st.session_state.sheet_names else 0
            )
            # If selection changes, update session state and reload
            if selected_sheet_name != st.session_state.selected_sheet:
                st.session_state.selected_sheet = selected_sheet_name
                st.session_state.editable_preview_df = None # Clear old preview
                load_sheet_for_editing(uploaded_file, st.session_state.selected_sheet)
                st.experimental_rerun() # Rerun to reflect new sheet choice and editor state

        with col2:
            if st.button("Reload Current Sheet", key="reload_sheet_btn"): # Changed from "Load Sheet"
                if st.session_state.selected_sheet:
                    load_sheet_for_editing(uploaded_file, st.session_state.selected_sheet)
                    # No rerun needed here as data editor will pick up st.session_state.editable_preview_df change
                else:
                    st.warning("No sheet selected to load.")

        if st.session_state.editable_preview_df is not None:
            st.markdown(f"#### Editable Preview of '{st.session_state.selected_sheet}'")
            # Use st.data_editor
            edited_df = st.data_editor(
                st.session_state.editable_preview_df,
                key=DATA_EDITOR_KEY,
                num_rows="dynamic", # useful for seeing all rows, but might be slow for huge sheets
                # column_config could be used to make 'Select' a checkbox
                column_config={"Select": st.column_config.CheckboxColumn("Select", default=False)}
            )
            # The 'edited_df' variable now holds the current state of the data_editor

            if st.button("Import Selected Rows", key="import_selected_rows_btn"):
                if edited_df is not None:
                    selected_rows_df = edited_df[edited_df['Select'] == True].copy()

                    if not selected_rows_df.empty:
                        # Clean up: remove 'Select', 'temp_row_id', rename 'sheet_name_temp'
                        selected_rows_df.rename(columns={'sheet_name_temp': 'sheet_name'}, inplace=True)
                        columns_to_drop = ['Select', 'temp_row_id']
                        # Check if columns exist before dropping
                        columns_to_drop_existing = [col for col in columns_to_drop if col in selected_rows_df.columns]
                        selected_rows_df.drop(columns=columns_to_drop_existing, inplace=True)

                        if st.session_state.data.get('selected') is None or st.session_state.data['selected'].empty:
                            st.session_state.data['selected'] = selected_rows_df
                        else:
                            st.session_state.data['selected'] = pd.concat(
                                [st.session_state.data['selected'], selected_rows_df],
                                ignore_index=True
                            )

                        st.session_state.data['import_count'] = len(st.session_state.data['selected'])

                        # Update metadata
                        current_sheet_name = st.session_state.selected_sheet
                        if 'data' not in st.session_state: st.session_state.data = {}
                        if 'import_metadata' not in st.session_state.data:
                            st.session_state.data['import_metadata'] = {'last_import_time': None, 'sheets_imported': []}

                        st.session_state.data['import_metadata']['last_import_time'] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                        if current_sheet_name not in st.session_state.data['import_metadata']['sheets_imported']:
                            st.session_state.data['import_metadata']['sheets_imported'].append(current_sheet_name)

                        st.success(f"Successfully imported {len(selected_rows_df)} rows from '{current_sheet_name}'. Total selected rows: {st.session_state.data['import_count']}.")

                        # Auto-advance sheet logic
                        current_sheet_index = st.session_state.sheet_names.index(current_sheet_name)
                        if current_sheet_index < len(st.session_state.sheet_names) - 1:
                            st.session_state.selected_sheet = st.session_state.sheet_names[current_sheet_index + 1]
                            # Automatically load the next sheet
                            load_sheet_for_editing(uploaded_file, st.session_state.selected_sheet)
                            st.experimental_rerun() # Rerun to update selectbox and data_editor
                        else:
                            st.info("All sheets have been processed or this was the last sheet.")
                            # Optionally clear editable_preview_df for the last sheet after import
                            st.session_state.editable_preview_df = None
                            st.experimental_rerun()


                    else:
                        st.warning("No rows were selected to import.")
                else:
                    st.warning("Data editor state not found.")

    else: # No file uploaded or file removed
        if st.session_state.last_uploaded_filename is not None:
            st.session_state.sheet_names = []
            st.session_state.selected_sheet = None
            st.session_state.editable_preview_df = None # Clear preview
            st.session_state.last_uploaded_filename = None
            # st.info("File removed or no file uploaded. State reset for preview.")


    if st.button("Clear All Accumulated Data & Preview", key="clear_accumulated_data_btn"):
        if 'data' not in st.session_state: st.session_state.data = {}
        st.session_state.data['raw'] = None # If 'raw' was used for original full sheet
        st.session_state.data['selected'] = None
        st.session_state.editable_preview_df = None # Clear editable preview
        st.session_state.data['import_count'] = 0
        st.session_state.data['import_metadata'] = {
            'last_import_time': None,
            'sheets_imported': []
        }
        # Reset UI-specific states
        st.session_state.sheet_names = []
        st.session_state.selected_sheet = None
        st.session_state.last_uploaded_filename = None
        # Clear the file uploader as well by resetting its key if it were assigned one,
        # or by rerunning after clearing related states.
        st.info("All accumulated data, preview, import metadata, and file upload state have been cleared.")
        st.experimental_rerun()

    # Optional: Display import metadata from session state if needed for debugging
    # ... (metadata display code can be added here or in main app)

```
