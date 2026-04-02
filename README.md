# Jobs Report 2025

R scripts in this repository reproduce core labor market indicators used for the 2025 Jobs Report, mainly from LABLAC microdata, with some GLD replication scripts.

## Repository Structure

- `R/`: analysis scripts (country-level indicators, regional aggregates, growth, inequality, special topics)
- `Excel/`: report workbook(s)
- `LICENSE`

## Data and Assumptions

- Most scripts expect `.dta` files named like `LABLAC_<country>_<year>_q<quarter>_...dta`.
- Several scripts use placeholder paths (`<<DATALIBWEB_PATH_OR_GENERIC_URL>>`) that must be replaced before execution.
- Mexico should be processed with `nocohh` input for consistency with ILO labor statistics.
- Countries and periods are script-specific (some scripts use non-2016 starts for selected countries due to data availability or breaks).

## Environment

- R 4.x (tested in this workspace with R 4.4.1).
- Scripts install/load needed packages on demand. Common packages include:
  - `haven`, `dplyr`, `tidyr`, `stringr`, `purrr`
  - `readxl`, `writexl`, `lubridate`, `data.table`, `ineq`

## Recommended Execution Order

This order reflects script dependencies in the current code.

1. `R/01_05_LMR_country_nocohh.R`
2. `R/01_05_LMR_aggregates.R` (reads country-level output from step 1)
3. `R/02_06_07_employment_rate_groups.R`
4. `R/04_employment_count_demograph.R`
5. `R/06_09_employment_rate_aggregate.R` (uses output from step 3)
6. `R/T_A3_A6_employment_totals_growth.R` (uses output from step 4)
7. Remaining thematic scripts (mostly standalone once input paths are set):
   - `R/03_annualized_growth_wap_skill.R`
   - `R/03_T_A1_annualized_growth_jobs.R`
   - `R/05_youth_employment.R`
   - `R/07_13_changes_workers_nocohh.R`
   - `R/08_informality_wage_employment.R`
   - `R/09_11_ila_wage_growth_aggregate.R`
   - `R/09_11_T_A2_ila_wage_growth_countries.R`
   - `R/10_aggregate_percentiles.R` (expects specific percentile/weight sheets)
   - `R/12_A_01_earnings_workers_growth.R`
   - `R/13_last_year_jobs_category.R`
   - `R/A_02_gini_coefficient_calc.R`
   - `R/B1_underemployment_in_LAC.R`
   - `R/01_Replication_Employment_Growth_GLD_May_2025.r`
   - `R/02_Replication_gini_GLD_May_2025.r`

## Script Index (One-line Purpose)

- `R/01_05_LMR_country_nocohh.R`: country-level labor market rates, income percentiles, and weights across periods.
- `R/01_05_LMR_aggregates.R`: imputation plus LAC aggregate indicators (LAC-10/LAC-9/LAC-8/LAC-7 variants).
- `R/01_Replication_Employment_Growth_GLD_May_2025.r`: GLD-based replication of employment growth by age groups.
- `R/02_06_07_employment_rate_groups.R`: employment rates and mean earnings by gender, age, and skill groups.
- `R/02_Replication_gini_GLD_May_2025.r`: GLD-based wage inequality replication (weighted/unweighted Gini).
- `R/03_annualized_growth_wap_skill.R`: growth in working-age population and employment by skill level.
- `R/03_T_A1_annualized_growth_jobs.R`: annualized job growth by country and LAC aggregate.
- `R/04_employment_count_demograph.R`: absolute employment counts by demographic groups for selected years.
- `R/05_youth_employment.R`: youth labor market profile (rates, employment status, sector distribution).
- `R/06_09_employment_rate_aggregate.R`: regional weighted aggregates of employment rates and earnings.
- `R/07_13_changes_workers_nocohh.R`: worker composition changes between two periods.
- `R/08_informality_wage_employment.R`: trends in informality and wage employment.
- `R/09_11_ila_wage_growth_aggregate.R`: reported earnings analysis with LAC aggregate.
- `R/09_11_T_A2_ila_wage_growth_countries.R`: country-level reported earnings growth comparison.
- `R/10_aggregate_percentiles.R`: LAC-7 weighted aggregation of P20/median/P80 labor income.
- `R/12_A_01_earnings_workers_growth.R`: earnings and worker growth by skill, gender, and sector.
- `R/13_last_year_jobs_category.R`: latest-year worker counts by sector and demographics.
- `R/A_02_gini_coefficient_calc.R`: Gini coefficients for wage/labor income using LABLAC files.
- `R/B1_underemployment_in_LAC.R`: underemployment levels and LAC-9/LAC-10 aggregates.
- `R/T_A3_A6_employment_totals_growth.R`: LAC-9 totals and annualized growth from employment-count outputs.

## How to Run

1. Set `data_dir` / `input_path` and `output_dir` / `output_path` in each script.
2. Run scripts from RStudio or terminal, for example:

```bash
Rscript R/01_05_LMR_country_nocohh.R
```

3. Check generated `.xlsx` files in the configured output location (many scripts prefix filenames with `Sys.Date()`).

## Notes

- The code is intentionally script-based (not packaged); each file is meant to be executable on its own.
- Output values may change with LABLAC updates and with/without `nocohh` inputs.