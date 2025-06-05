import pandas as pd
import numpy as np

# Helper for safe dictionary access, similar to R's %||%
def get_param(params, key, default):
    return params.get(key, default) if params else default

def calculate_discount_rate(dcf_parameters: dict) -> float:
    """
    Calculate Discount Rate (Cost of Equity using CAPM)
    :param dcf_parameters: A dict containing numeric parameters:
                           risk_free_rate (%), equity_premium (%),
                           beta, country_premium (%).
    :return: Numeric discount rate (decimal), or np.nan if inputs are invalid.
    """
    raw_risk_free_rate = get_param(dcf_parameters, 'risk_free_rate', 0)
    raw_equity_premium = get_param(dcf_parameters, 'equity_premium', 0)
    beta = get_param(dcf_parameters, 'beta', 1) # Beta is not a percentage
    raw_country_premium = get_param(dcf_parameters, 'country_premium', 0)

    # Validate types before arithmetic operations
    if not all(isinstance(val, (int, float)) for val in [raw_risk_free_rate, raw_equity_premium, beta, raw_country_premium]):
        # print("Warning: Non-numeric input provided to calculate_discount_rate") # Avoid print in library
        return np.nan

    risk_free_rate = raw_risk_free_rate / 100
    equity_premium = raw_equity_premium / 100
    country_premium = raw_country_premium / 100

    discount_rate = risk_free_rate + (equity_premium * beta) + country_premium
    return discount_rate

def calculate_terminal_growth_rate(dcf_parameters: dict) -> float:
    """
    Calculate Terminal Growth Rate
    :param dcf_parameters: A dict containing numeric parameters:
                           long_run_inflation (%),
                           additional_terminal_growth (%).
    :return: Numeric terminal growth rate (decimal), or np.nan if inputs are invalid.
    """
    raw_long_run_inflation = get_param(dcf_parameters, 'long_run_inflation', 2)
    raw_additional_growth = get_param(dcf_parameters, 'additional_terminal_growth', 0)

    # Validate types before arithmetic operations
    if not all(isinstance(val, (int, float)) for val in [raw_long_run_inflation, raw_additional_growth]):
        # print("Warning: Non-numeric input provided to calculate_terminal_growth_rate") # Avoid print
        return np.nan

    terminal_growth_rate = (raw_long_run_inflation + raw_additional_growth) / 100
    return terminal_growth_rate

def calculate_dcf_projections(
    assumptions_df: pd.DataFrame,
    base_revenue: float,
    non_op_assets: float,
    dcf_params: dict, # Contains calculated discount_rate, terminal_growth_rate, CAP, market_cap etc.
    std_data: pd.DataFrame | None # Standardized historical data, can be None
) -> pd.DataFrame | None:
    """
    Calculate DCF Projections.
    This is a complex function that will be built out.
    """
    # print("calculate_dcf_projections: Starting...") # Avoid print

    # --- Determine last_hist_year ---
    last_hist_year = None
    if std_data is not None and not std_data.empty:
        std_year_cols = [col for col in std_data.columns if isinstance(col, str) and col.isdigit()]
        if not std_year_cols and len(std_data.columns) > 1: # Fallback if headers are not years directly
             std_year_cols = [col for col in std_data.columns[1:] if isinstance(col, str) and col.isdigit()]

        if std_year_cols:
            try:
                last_hist_year = max(int(col) for col in std_year_cols)
            except ValueError:
                # print("Warning: Could not parse year columns from std_data headers.")
                pass

    if last_hist_year is None and assumptions_df is not None and not assumptions_df.empty:
        assum_year_cols = [col for col in assumptions_df.columns if isinstance(col, str) and col.isdigit()]
        if not assum_year_cols and len(assumptions_df.columns) > 1: # Fallback for assumptions_df
            assum_year_cols = [col for col in assumptions_df.columns[1:] if isinstance(col, str) and col.isdigit()]

        if assum_year_cols:
            try:
                first_proj_year_in_table = min(int(col) for col in assum_year_cols)
                last_hist_year = first_proj_year_in_table - 1
            except ValueError:
                # print("Warning: Could not parse year columns from assumptions_df headers.")
                pass

    if last_hist_year is None:
        last_hist_year = pd.Timestamp.now().year - 1
        # print(f"Warning: Could not determine last_hist_year, defaulting to {last_hist_year}")

    # print(f"Internal last_hist_year: {last_hist_year}")

    # --- Input validation ---
    required_assumptions = [
        "Revenue Growth", "Operating Profit Margin", "Interest Expense",
        "Cash Tax Rate", "Depreciation & Amortisation", "Incremental WC",
        "Incremental FC", "Net Debt Issuance"
    ]

    if assumptions_df is None or not isinstance(assumptions_df, pd.DataFrame) or \
       "Assumption" not in assumptions_df.columns or \
       not all(item in assumptions_df["Assumption"].tolist() for item in required_assumptions):
        # print("Validation failed: assumptions_df is invalid or missing required assumptions.")
        return None

    if not isinstance(base_revenue, (int, float)):
        # print("Validation failed: base_revenue is invalid.")
        return None

    if not isinstance(non_op_assets, (int, float)) or non_op_assets < 0:
        # print("Validation failed: non_op_assets is invalid.")
        return None

    if dcf_params is None or not isinstance(dcf_params, dict) or \
       not all(k in dcf_params for k in ["discount_rate", "terminal_growth_rate", "CAP"]):
        # print("Validation failed: dcf_params is invalid or missing required keys.")
        return None

    if not isinstance(dcf_params["CAP"], int) or dcf_params["CAP"] <=0:
        # print("Validation failed: CAP in dcf_params is invalid.")
        return None

    if not isinstance(last_hist_year, int):
        # print("Validation failed: last_hist_year is invalid.")
        return None

    # print("Validation passed.")

    discount_rate = dcf_params["discount_rate"]
    terminal_growth_rate = dcf_params["terminal_growth_rate"]
    cap_years = dcf_params["CAP"] # Renamed from 'cap' to avoid conflict with built-in

    # --- Determine Projection Years ---
    projection_years_int = list(range(last_hist_year + 1, last_hist_year + 1 + cap_years))
    projection_year_cols_str = [str(yr) for yr in projection_years_int]
    # first_projection_year_col_str = projection_year_cols_str[0] # Not used yet

    # --- Ensure assumptions_df has columns for all projection_year_cols_str ---
    # This is a simplification. The R code has more complex logic for available_assumption_year_cols
    # and how it handles missing assumption years. For now, we assume they must exist.
    missing_assumption_years = [yr_col for yr_col in projection_year_cols_str if yr_col not in assumptions_df.columns]
    if missing_assumption_years:
        # print(f"Error: Assumptions table missing data for projection years: {missing_assumption_years}")
        return None

    # Create structure for projections
    projection_rows = [
        "Revenue", "Revenue CAGR (%)", "Operating Profit", "Interest Expense",
        "Profit Before Tax", "Taxes", "Net Income",
        "Depreciation & Amortisation", "Incremental Working Capital Investment",
        "Operating Cash Flow",
        "Incremental Fixed Capital Investment",
        "Net Debt Issuance", "Free Cash Flow to Equity", "FCFE Growth (%)",
        "Present Value of FCFE", "Terminal Value",
        "Present Value of Terminal Value", "NPV",
        "Non-Operating Assets", "Intrinsic Value", "Proportion of Market Cap"
    ]

    projections_df = pd.DataFrame(index=projection_rows, columns=projection_year_cols_str)
    projections_df = projections_df.astype(float) # Initialize with float NaNs

    # --- Helper to get assumption values ---
    def get_assumption(assumption_name: str, year_col: str) -> float:
        value = assumptions_df.loc[assumptions_df["Assumption"] == assumption_name, year_col].iloc[0]
        return float(value) if pd.notna(value) else 0.0

    # --- Main Projection Loop ---
    current_revenue = base_revenue
    for i, year_col in enumerate(projection_year_cols_str):
        # Revenue
        if i == 0: # First projection year
            revenue_growth_rate = get_assumption("Revenue Growth", year_col)
            projections_df.loc["Revenue", year_col] = current_revenue * (1 + revenue_growth_rate)
        else:
            prev_year_col = projection_year_cols_str[i-1]
            revenue_growth_rate = get_assumption("Revenue Growth", year_col)
            projections_df.loc["Revenue", year_col] = projections_df.loc["Revenue", prev_year_col] * (1 + revenue_growth_rate)

        current_year_revenue = projections_df.loc["Revenue", year_col]

        # Operating Profit
        op_margin = get_assumption("Operating Profit Margin", year_col)
        projections_df.loc["Operating Profit", year_col] = current_year_revenue * op_margin

        # Interest Expense
        # In R, this is a % of revenue. If it's a fixed value, the assumption row would need to be "Interest Expense Amount"
        # Assuming "Interest Expense" in assumptions_df is a direct value or % of revenue based on its definition.
        # For this translation, let's assume it's a direct value taken from assumptions.
        # If it's a percentage of revenue, the R code implies `assumptions_df["Interest Expense"] * revenue`
        # The R code does: `op_profit_current_year * (get_assumption("Interest Expense", current_year_r_col) / current_year_revenue)`
        # This seems like interest_coverage_ratio * op_profit if "Interest Expense" assumption was 1/coverage_ratio
        # Or, if "Interest Expense" assumption is Interest/Revenue ratio, then assumption * revenue.
        # The R code is `value = projections_df["Operating Profit", year_col] * (assumptions_df["Interest Expense", year_col] / projections_df["Revenue", year_col])`
        # This implies the assumption "Interest Expense" is actually "Interest Expense / Revenue" ratio.
        # Let's stick to the R logic:
        # interest_expense_ratio = get_assumption("Interest Expense", year_col) # This is assumed to be Interest/Revenue
        # projections_df.loc["Interest Expense", year_col] = interest_expense_ratio * current_year_revenue
        # However, the R code later uses absolute interest expense for PBT.
        # The R formula `profit_before_tax = op_profit_current_year - interest_expense_current_year`
        # `interest_expense_current_year = get_assumption("Interest Expense", current_year_r_col)`
        # This indicates the "Interest Expense" assumption is an absolute value.
        projections_df.loc["Interest Expense", year_col] = get_assumption("Interest Expense", year_col)

        # Profit Before Tax
        projections_df.loc["Profit Before Tax", year_col] = projections_df.loc["Operating Profit", year_col] - projections_df.loc["Interest Expense", year_col]

        # Taxes
        cash_tax_rate = get_assumption("Cash Tax Rate", year_col)
        # Ensure PBT is not negative for tax calculation, or handle negative tax (refund)
        pbt = projections_df.loc["Profit Before Tax", year_col]
        projections_df.loc["Taxes", year_col] = max(0, pbt) * cash_tax_rate # Simplified: tax on positive PBT

        # Net Income
        projections_df.loc["Net Income", year_col] = pbt - projections_df.loc["Taxes", year_col]

        # Depreciation & Amortisation
        # Similar to Interest Expense, R code uses `get_assumption("Depreciation & Amortisation", current_year_r_col)`
        # This implies D&A is an absolute value from assumptions.
        da_value = get_assumption("Depreciation & Amortisation", year_col)
        projections_df.loc["Depreciation & Amortisation", year_col] = da_value

        # Incremental Working Capital Investment
        # R code: `get_assumption("Incremental WC", current_year_r_col)`
        wc_investment = get_assumption("Incremental WC", year_col)
        projections_df.loc["Incremental Working Capital Investment", year_col] = wc_investment

        # Operating Cash Flow (approximated as Net Income + D&A - Incremental WC)
        # R uses "Net Income" + "Depreciation & Amortisation" - "Incremental Working Capital Investment"
        # This is a common way to derive OCF from NI when starting point is NI for FCFE.
        projections_df.loc["Operating Cash Flow", year_col] = (
            projections_df.loc["Net Income", year_col] +
            projections_df.loc["Depreciation & Amortisation", year_col] -
            wc_investment # Already negative if it's an outflow in assumptions
        )

        # Incremental Fixed Capital Investment
        # R code: `get_assumption("Incremental FC", current_year_r_col)`
        fc_investment = get_assumption("Incremental FC", year_col)
        projections_df.loc["Incremental Fixed Capital Investment", year_col] = fc_investment

        # Net Debt Issuance
        # R code: `get_assumption("Net Debt Issuance", current_year_r_col)`
        debt_issuance = get_assumption("Net Debt Issuance", year_col)
        projections_df.loc["Net Debt Issuance", year_col] = debt_issuance

        # Free Cash Flow to Equity (FCFE)
        # FCFE = Net Income + D&A - WC Inv - FC Inv + Net Debt Issuance
        # Or using OCF: OCF - FC Inv + Net Debt Issuance
        projections_df.loc["Free Cash Flow to Equity", year_col] = (
            projections_df.loc["Net Income", year_col] +
            da_value -
            wc_investment - # wc_investment is typically positive for outflow in assumptions
            fc_investment + # fc_investment is typically positive for outflow in assumptions
            debt_issuance # debt_issuance is positive for new debt, negative for repayment
        )

    # --- After loop calculations ---

    # Revenue CAGR (%)
    # For each year relative to base_revenue
    for i, year_col in enumerate(projection_year_cols_str):
        num_years = i + 1
        current_year_revenue = projections_df.loc["Revenue", year_col]
        if base_revenue != 0 and current_year_revenue / base_revenue > 0:
             # CAGR = (Ending Value / Beginning Value)^(1/n) - 1
            cagr = (current_year_revenue / base_revenue)**(1/num_years) - 1
            projections_df.loc["Revenue CAGR (%)", year_col] = cagr * 100 # As percentage
        else:
            projections_df.loc["Revenue CAGR (%)", year_col] = 0 # Or np.nan if preferred

    # FCFE Growth (%)
    for i, year_col in enumerate(projection_year_cols_str):
        current_fcfe = projections_df.loc["Free Cash Flow to Equity", year_col]
        if i == 0:
            # No previous FCFE to compare against for the first projection year from this run
            # R code appears to leave this blank or uses a different base for first year.
            # For now, let's put NaN or 0.
            projections_df.loc["FCFE Growth (%)", year_col] = np.nan
        else:
            prev_year_col = projection_year_cols_str[i-1]
            prev_fcfe = projections_df.loc["Free Cash Flow to Equity", prev_year_col]
            if prev_fcfe != 0:
                growth = (current_fcfe / prev_fcfe) - 1
                projections_df.loc["FCFE Growth (%)", year_col] = growth * 100 # As percentage
            elif current_fcfe == 0 and prev_fcfe == 0:
                 projections_df.loc["FCFE Growth (%)", year_col] = 0.0
            else: # Current FCFE is non-zero, previous is zero (infinite growth)
                projections_df.loc["FCFE Growth (%)", year_col] = np.nan # Or a large number / specific indicator

    # Present Value of FCFE
    for i, year_col in enumerate(projection_year_cols_str):
        fcfe = projections_df.loc["Free Cash Flow to Equity", year_col]
        # Discount factor: 1 / (1 + dr)^ (year_number)
        # year_number for discounting is i + 1
        df = 1 / ((1 + discount_rate)**(i + 1))
        projections_df.loc["Present Value of FCFE", year_col] = fcfe * df

    # --- Summary Calculations (Typically for the first projection year column or a separate summary) ---
    # R code places these in the first year column. Let's use last year for TV calc and first for summary.
    last_projection_year_col = projection_year_cols_str[-1]
    first_projection_year_col = projection_year_cols_str[0]

    # Terminal Value
    # TV = (FCFE_last_year * (1 + terminal_growth_rate)) / (discount_rate - terminal_growth_rate)
    fcfe_last_year = projections_df.loc["Free Cash Flow to Equity", last_projection_year_col]
    if discount_rate <= terminal_growth_rate: # Avoid division by zero or negative denominator
        terminal_value = np.nan # Or some other indicator of error/instability
        # print("Warning: Discount rate must be greater than terminal growth rate for TV calculation.")
    else:
        terminal_value = (fcfe_last_year * (1 + terminal_growth_rate)) / (discount_rate - terminal_growth_rate)

    projections_df.loc["Terminal Value", first_projection_year_col] = terminal_value # Store in first year

    # Present Value of Terminal Value
    # PV_TV = TV / (1 + discount_rate)^CAP
    if pd.notna(terminal_value):
        pv_terminal_value = terminal_value / ((1 + discount_rate)**cap_years)
    else:
        pv_terminal_value = np.nan
    projections_df.loc["Present Value of Terminal Value", first_projection_year_col] = pv_terminal_value

    # NPV (Sum of PV of FCFEs + PV of Terminal Value)
    sum_pv_fcfe = projections_df.loc["Present Value of FCFE"].sum()
    if pd.notna(pv_terminal_value):
        npv = sum_pv_fcfe + pv_terminal_value
    else: # if PV_TV is nan, NPV is effectively sum of PV_FCFEs, but might indicate upstream issue
        npv = sum_pv_fcfe # Or np.nan if TV error should propagate more explicitly
    projections_df.loc["NPV", first_projection_year_col] = npv

    # Non-Operating Assets
    projections_df.loc["Non-Operating Assets", first_projection_year_col] = non_op_assets

    # Intrinsic Value
    # Intrinsic Value = NPV + Non-Operating Assets (if NPV is equity value)
    # The FCFE method directly calculates equity value.
    if pd.notna(npv):
        intrinsic_value = npv + non_op_assets
    else:
        intrinsic_value = np.nan # if NPV is nan
    projections_df.loc["Intrinsic Value", first_projection_year_col] = intrinsic_value

    # Proportion of Market Cap
    market_cap = dcf_params.get('market_cap', 0) # Get from dcf_params
    if pd.notna(intrinsic_value) and market_cap is not None and market_cap != 0:
        proportion_market_cap = intrinsic_value / market_cap
        projections_df.loc["Proportion of Market Cap", first_projection_year_col] = proportion_market_cap * 100 # As percentage
    else:
        projections_df.loc["Proportion of Market Cap", first_projection_year_col] = np.nan

    return projections_df

def run_full_dcf_calculation(
    assumptions_df: pd.DataFrame,
    base_revenue: float,
    non_op_assets: float,
    dcf_params_list: dict, # Raw parameters from session_state
    std_data: pd.DataFrame | None
) -> pd.DataFrame | None:
    """
    Orchestrates the validation, rate calculation, and projection calculation.
    """
    # print("--- run_full_dcf_calculation START ---")

    # --- Input Validation (Basic) ---
    if assumptions_df is None or not isinstance(assumptions_df, pd.DataFrame) or \
       "Assumption" not in assumptions_df.columns or assumptions_df.shape[1] <= 1:
        # print("Validation failed: assumptions_df invalid.")
        return None
    if not isinstance(base_revenue, (int, float)):
        # print("Validation failed: base_revenue invalid.")
        return None
    if not isinstance(non_op_assets, (int, float)) or non_op_assets < 0:
        # print("Validation failed: non_op_assets invalid.")
        return None
    if dcf_params_list is None or not isinstance(dcf_params_list, dict):
        # print("Validation failed: dcf_params_list invalid.")
        return None
    if not isinstance(dcf_params_list.get("CAP"), int) or dcf_params_list.get("CAP", 0) <= 0:
        # print("Validation failed: CAP in dcf_params_list invalid.")
        return None

    required_rate_params = ["risk_free_rate", "equity_premium", "beta", "country_premium",
                            "long_run_inflation", "additional_terminal_growth"]
    for p_name in required_rate_params:
        if not isinstance(dcf_params_list.get(p_name), (int,float)):
            # print(f"Validation failed: Parameter {p_name} is missing or not numeric in dcf_params_list.")
            return None

    # --- Calculate Rates ---
    params_for_projection = dcf_params_list.copy()
    params_for_projection['discount_rate'] = calculate_discount_rate(params_for_projection)
    params_for_projection['terminal_growth_rate'] = calculate_terminal_growth_rate(params_for_projection)

    if pd.isna(params_for_projection['discount_rate']) or pd.isna(params_for_projection['terminal_growth_rate']):
        # print("Validation failed: Rate calculation resulted in NA.")
        return None
    # print("Rates calculated successfully.")

    # --- Call Core Projection Function ---
    # print("Calling core calculate_dcf_projections...")
    projection_result = calculate_dcf_projections(
        assumptions_df=assumptions_df,
        base_revenue=base_revenue,
        non_op_assets=non_op_assets,
        dcf_params=params_for_projection,
        std_data=std_data
    )

    # print("--- run_full_dcf_calculation END ---")
    return projection_result

def perform_assumption_auto_calc(
    assumptions_df: pd.DataFrame,
    standardized_data_df: pd.DataFrame | None,
    auto_calc_flags: list[bool],
    years_to_use: list[int]
) -> pd.DataFrame:
    """
    Perform Assumption Auto-Calculation based on Standardized Historical Data.
    """
    # print("perform_assumption_auto_calc: Starting...")
    if assumptions_df is None:
        return pd.DataFrame()

    modified_assumptions_df = assumptions_df.copy()

    if standardized_data_df is None or standardized_data_df.empty or len(auto_calc_flags) != 8 or len(years_to_use) != 8:
        # print("Warning: Standardized data missing or invalid parameters for auto-calc. Returning original assumptions.")
        return modified_assumptions_df

    # --- Initial Validations ---
    if not isinstance(assumptions_df, pd.DataFrame) or "Assumption" not in assumptions_df.columns:
        # print("Error: assumptions_df is invalid.")
        return assumptions_df # Return original if invalid

    if not isinstance(standardized_data_df, pd.DataFrame) or standardized_data_df.empty:
        # print("Warning: Standardized data is missing or empty. No auto-calculation possible.")
        return assumptions_df

    if not (isinstance(auto_calc_flags, list) and len(auto_calc_flags) == 8 and all(isinstance(f, bool) for f in auto_calc_flags)):
        # print("Error: auto_calc_flags is invalid.")
        return assumptions_df

    if not (isinstance(years_to_use, list) and len(years_to_use) == 8 and all(isinstance(y, int) and y > 0 for y in years_to_use)):
        # print("Error: years_to_use is invalid.")
        return assumptions_df

    modified_assumptions_df = assumptions_df.copy()

    # --- Prepare standardized_data_df ---
    # Assuming the first column is the metric name, or it's the index
    if standardized_data_df.index.name == "Metric" or (standardized_data_df.columns[0] == "Metric" and not pd.api.types.is_numeric_dtype(standardized_data_df.iloc[:, 0])):
        if standardized_data_df.columns[0] == "Metric":
            std_data_processed = standardized_data_df.set_index("Metric")
        else:
            std_data_processed = standardized_data_df.copy()
    elif "Metric" in standardized_data_df.columns:
         std_data_processed = standardized_data_df.set_index("Metric")
    else: # Fallback: assume first column is metric if not numeric, otherwise cannot proceed.
        if pd.api.types.is_numeric_dtype(standardized_data_df.iloc[:, 0]):
            # print("Error: Metric column not clearly identified in standardized_data_df.")
            return assumptions_df
        std_data_processed = standardized_data_df.set_index(standardized_data_df.columns[0])

    numeric_year_cols = sorted([col for col in std_data_processed.columns if str(col).isdigit()], key=int)
    if not numeric_year_cols:
        # print("Warning: No numeric year columns found in standardized_data_df.")
        return assumptions_df

    # --- Define Assumption Calculation Logic ---
    # Order must match auto_calc_flags and years_to_use
    assumption_configs = [
        {"name": "Revenue Growth", "metric": "Revenue", "type": "cagr"},
        {"name": "Operating Profit Margin", "metric": "Operating Profit", "numerator": "Operating Profit", "denominator": "Revenue", "type": "avg_ratio"},
        {"name": "Interest Expense", "metric": "Interest Expense", "numerator": "Interest Expense", "denominator": "Revenue", "type": "avg_ratio_abs_fallback"}, # Special: could be % of Rev or abs
        {"name": "Cash Tax Rate", "metric": "Tax", "numerator": "Tax", "denominator_pbt": ["Operating Profit", "Interest Expense"], "type": "avg_ratio_constrained"}, # PBT = OP - IE
        {"name": "Depreciation & Amortisation", "metric": "Depreciation & Amortisation", "numerator": "Depreciation & Amortisation", "denominator": "Revenue", "type": "avg_ratio"},
        {"name": "Incremental WC", "metric_wc": "Working Capital Excluding Cash and Cash Equivalents", "metric_rev": "Revenue", "type": "avg_delta_ratio"},
        {"name": "Incremental FC", "metric_fc": "Fixed Capital", "metric_rev": "Revenue", "type": "avg_delta_ratio"}, # Assuming Fixed Capital is CAPEX
        {"name": "Net Debt Issuance", "metric": "Net Debt Issuance", "numerator": "Net Debt Issuance", "denominator": "Revenue", "type": "avg_ratio_abs_fallback"}
    ]

    projection_year_cols = [col for col in modified_assumptions_df.columns if col != "Assumption"]

    def get_hist_data(metric_name: str, num_years: int) -> pd.Series | None:
        if metric_name not in std_data_processed.index:
            # print(f"Warning: Metric '{metric_name}' not found in standardized_data.")
            return None

        available_years = [yr_col for yr_col in numeric_year_cols if pd.notna(std_data_processed.loc[metric_name, yr_col])]
        if not available_years:
            return None

        selected_years = sorted(available_years, key=int)[-num_years:]
        if not selected_years: return None # Should not happen if available_years is not empty

        data = std_data_processed.loc[metric_name, selected_years].astype(float)
        return data.dropna() # Drop NaNs within the selected series again

    for i, config in enumerate(assumption_configs):
        if not auto_calc_flags[i]:
            continue

        num_hist_years = min(years_to_use[i], len(numeric_year_cols)) # Cap at available years
        calculated_value = np.nan

        try:
            if config["type"] == "cagr":
                revenue_data = get_hist_data(config["metric"], num_hist_years)
                if revenue_data is not None and len(revenue_data) >= 2 and revenue_data.iloc[0] != 0 and (revenue_data.iloc[-1] / revenue_data.iloc[0]) > 0:
                    cagr = (revenue_data.iloc[-1] / revenue_data.iloc[0])**(1/(len(revenue_data)-1)) - 1
                    calculated_value = cagr
                elif revenue_data is not None and len(revenue_data) == 1: # Only one year of data, assume 0 growth
                     calculated_value = 0.0


            elif config["type"] == "avg_ratio" or config["type"] == "avg_ratio_abs_fallback":
                numer_data = get_hist_data(config["numerator"], num_hist_years)
                denom_data = get_hist_data(config["denominator"], num_hist_years)
                if numer_data is not None and denom_data is not None:
                    # Align data by index (years)
                    aligned_numer, aligned_denom = numer_data.align(denom_data, join='inner')
                    if not aligned_denom.empty:
                        ratios = aligned_numer.divide(aligned_denom.replace(0, np.nan)).dropna() # Avoid div by zero
                        if not ratios.empty:
                            calculated_value = ratios.mean()
                        elif config["type"] == "avg_ratio_abs_fallback" and not numer_data.empty : # Fallback to average of absolute numerator if ratio fails
                             calculated_value = numer_data.mean()


            elif config["type"] == "avg_ratio_constrained": # Cash Tax Rate
                tax_data = get_hist_data(config["numerator"], num_hist_years)
                op_data = get_hist_data(config["denominator_pbt"][0], num_hist_years)
                ie_data = get_hist_data(config["denominator_pbt"][1], num_hist_years)

                if tax_data is not None and op_data is not None and ie_data is not None:
                    aligned_tax, aligned_op = tax_data.align(op_data, join='inner')
                    aligned_ie, _ = ie_data.align(aligned_op, join='inner') # Align IE to already aligned OP

                    # Further align tax and op with the result of ie alignment
                    final_tax, final_op = aligned_tax.align(aligned_ie, join='inner')
                    final_ie = aligned_ie.loc[final_op.index] # Ensure all use the same final index

                    if not final_op.empty:
                        pbt_data = final_op - final_ie
                        # Use only positive PBT for tax rate calculation
                        positive_pbt_mask = pbt_data > 0
                        valid_tax_data = final_tax[positive_pbt_mask]
                        valid_pbt_data = pbt_data[positive_pbt_mask]

                        if not valid_pbt_data.empty:
                            ratios = valid_tax_data.divide(valid_pbt_data.replace(0, np.nan)).dropna()
                            if not ratios.empty:
                                calculated_value = np.clip(ratios.mean(), 0, 1) # Constrain between 0 and 1

            elif config["type"] == "avg_delta_ratio":
                val_data = get_hist_data(config.get("metric_wc") or config.get("metric_fc"), num_hist_years + 1) # Need one more year for delta
                rev_data = get_hist_data(config["metric_rev"], num_hist_years + 1)

                if val_data is not None and rev_data is not None and len(val_data) >= 2 and len(rev_data) >=2 :
                    aligned_val, aligned_rev = val_data.align(rev_data, join='inner')
                    if len(aligned_val) >= 2: # Ensure at least two common years for deltas
                        delta_val = aligned_val.diff().dropna()
                        delta_rev = aligned_rev.diff().dropna()

                        # Align deltas
                        aligned_delta_val, aligned_delta_rev = delta_val.align(delta_rev, join='inner')

                        if not aligned_delta_rev.empty:
                            ratios = aligned_delta_val.divide(aligned_delta_rev.replace(0, np.nan)).dropna()
                            if not ratios.empty:
                                calculated_value = ratios.mean()

            if pd.notna(calculated_value):
                # Update all projection years for this assumption
                assumption_row_idx = modified_assumptions_df[modified_assumptions_df["Assumption"] == config["name"]].index
                if not assumption_row_idx.empty:
                    for proj_col in projection_year_cols:
                        modified_assumptions_df.loc[assumption_row_idx, proj_col] = calculated_value

        except Exception as e:
            # print(f"Warning: Could not auto-calculate for '{config['name']}'. Error: {e}")
            # Keep original assumption if calculation fails
            pass

    return modified_assumptions_df
