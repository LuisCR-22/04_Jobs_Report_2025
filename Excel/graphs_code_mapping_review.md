# Report-to-Workbook Mapping Review

Source used for this phase: [graphs_data_description.md](graphs_data_description.md)

This note maps the scripts in `Report/` to the workbook sheets described in the graph inventory. It has been revised to reflect the final confirmed mappings after reading every script and cross-referencing with the Excel figure descriptions.

## Current Review Status

- Scripts mapped and retained: 18
- Out-of-scope scripts (omitted from `R/` folder): 5 (COL country spotlights and LinkedIn supplementary)

## Corrections Made in This Review

The previous mapping contained several errors that were corrected:

1. **`08_rate-aggregate-V3.R` was incorrectly mapped to Figures 2 & 5.**
   This script creates LAC-9 employment-rate aggregates by demographic group and LAC-7 earnings aggregates by group (it reads the output of `08_rate-groups-V3.R`). It does NOT produce country-level data (Figure 2) nor the quarterly LAC time-series (Figure 5). Corrected to **Figures 6, 9**.

2. **`08_rate-groups-V3.R` was missing Figure 2.**
   This script produces country-level employment rates and mean incomes by demographic group. It is the source for Figure 2 (country employment rates), Figure 6 (rates by group at country level), and Figure 7 (rate changes by country). Corrected to **Figures 2, 6, 7**.

3. **`02_LMR` scripts were missing Figure 5.**
   The aggregate script (`02_aggregates`) produces LAC-level quarterly LFP, Employment, Unemployment, Youth Unemployment, NEET, and Alt NEET rates. This is exactly Figure 5. Corrected to **Figures 1, 5** for both LMR scripts.

4. **`03_changes_workers_by_countries` also contributes to Figure 13.**
   This script produces changes in worker counts by sector and country between two periods. This sector-level change data feeds the bubble charts in Figure 13, alongside `13_last_year_jobs_by_cathegory.r` which provides the latest-year snapshot. Corrected to **Figures 7, 13**.

## Script-by-Script Final Mapping

| Old Code Name in `Report/` | Target Figures / Tables | New Code Name in `R/` | Notes |
| --- | --- | --- | --- |
| `02_2025-04-01-LMR-aggregates-graph-V4.R` | Figure 1, Figure 5 | `01_05_LMR_aggregates.R` | Produces LAC aggregate quarterly rates (LFP, Employment, Unemployment, NEET, income). Input for Figure 1 (macro trends) and Figure 5 (LAC aggregate rates). |
| `02_2025-04-02-LMR-compound-graph-nocohh-V4.R` | Figure 1, Figure 5 | `01_05_LMR_country_nocohh.R` | Produces country-level quarterly rates from LABLAC nocohh data. Feeds the aggregates script above. |
| `11_2025-04-03-underemployment in LAC.r` | Box Figure B1.a, B1.b | `B1_underemployment_in_LAC.R` | Underemployment rates (standard and alternative) by country and LAC aggregates. |
| `08_2025-03-04_employment rate-aggregate-V3.R` | Figure 6, Figure 9 | `06_09_employment_rate_aggregate.R` | Creates LAC-9 weighted employment rate aggregates by demographic group and LAC-7 earnings aggregates. Reads output of the groups script. |
| `06_2025-04-02-annualized-growth- jobs.R` | Figure 3, Table A1 | `03_T_A1_annualized_growth_jobs.R` | Annualized job growth rates across periods for LAC-8 countries with country-specific period exceptions. |
| `07_annualized_growth_wap_workers_by_skill_level..R` | Figure 3 | `03_annualized_growth_wap_skill.R` | Annualized growth in working-age population and employed workers by skill level (Low/Middle/High). |
| `10_2025-03-04-Employment Count Analysis by Demograph.r` | Figure 4 | `04_employment_count_demograph.R` | Employment counts by gender, age group, and skill level for 9 LAC countries across 2016/2019/2023/2024. |
| `20_Youth_employment_2016-2024-V2.R` | Figure 5 (auxiliary) | `05_youth_employment.R` | Youth-specific labor market analysis (ages 15-24): rates, sectors, employment type. Supplements Figure 5 youth components. |
| `08_2025-03-04_employment rate-groups-V3.R` | Figure 2, Figure 6, Figure 7 | `02_06_07_employment_rate_groups.R` | Country-level employment rates and mean incomes by demographic group (gender, age, skill). Primary source for Figure 2 (country rates), Figure 6 (group breakdowns), and Figure 7 (country rate changes). |
| `03_changes_workers_by_countries-V4-nocohh.r` | Figure 7, Figure 13 | `07_13_changes_workers_nocohh.R` | Changes in worker counts by country, sector, gender, age, and skill level between two periods. Contributes change data to Figure 7 and sector-level changes for Figure 13 bubble charts. |
| `09_informality and wage employment change-V2.r` | Figure 8 | `08_informality_wage_employment.R` | Informality rates (productive and social-protection definitions) and wage employment share changes by country. |
| `04-ila-wage-growth-aggregate.R` | Figure 9, Figure 11 | `09_11_ila_wage_growth_aggregate.R` | LAC aggregate reported labor income and wage growth with country weights. |
| `04-ila-wage-growth-all-countries-V2.R` | Figure 9, Figure 11, Table A2 | `09_11_T_A2_ila_wage_growth_countries.R` | Country-level reported labor income and wage annualized growth rates. |
| `05-create-aggregate-20-80-ila-percentiles.r` | Figure 10 | `10_aggregate_percentiles.R` | LAC-7 weighted P20, median, P80 labor income percentiles. |
| `01_2025-04-04-Annex-earnings-workers-growth-All-V4.r` | Figure 12, Annex 01 | `12_A_01_earnings_workers_growth.R` | Earnings and worker growth decomposition by skill, gender, and sector. Produces Figure 12 (decomposition) and Annex 01 (JQI changes). |
| `13_last_year_jobs_by_cathegory.r` | Figure 13a, 13b | `13_last_year_jobs_category.R` | Latest-year worker counts by sector, gender, age, employment type for all LAC countries. Provides the snapshot data for Figure 13 bubble charts. |
| `12_2025-04-04-Revised Gini coefficient calculaton-V2.r` | Annex 02 | `A_02_gini_coefficient_calc.R` | Gini coefficients for wage and income inequality by country-period. |
| `10_2025-04-03-LAC-9 employment totals and Growth Analysis.r` | Tables A3, A4, A5, A6 | `T_A3_A6_employment_totals_growth.R` | LAC-9 totals and annualized growth rates for employment counts by demographic group. Reads output of `04_employment_count_demograph.R`. |

## Out-Of-Scope Bucket (Discarded)

The following scripts do not have a current match in the workbook description and are excluded from the `R/` final map:

- `37_Colombia Youth Labor Market Analysis_by_gender.r` — COL country spotlight
- `38_Colombia job creation 2021-2024.R` — COL country spotlight
- `39_Linkedin-data management-V2.R` — LinkedIn supplementary data
- `39_Linkedin-regional-recent-growth.R` — LinkedIn supplementary data
- `39_Linkedin-sectoral-info.R` — LinkedIn supplementary data
