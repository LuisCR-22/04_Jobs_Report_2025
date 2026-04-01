# Report-to-Workbook Mapping Review

Source used for this first pass: [graphs_data_description.md](graphs_data_description.md)

This note maps the scripts in Report/ to the workbook sheets described in the graph inventory. It is written as a review log so we can keep track of what has already been checked and what still needs a second pass.

## Legend

- Confirmed: the script clearly matches the workbook sheet or data pipeline.
- Likely: the script appears to feed the sheet, but the final workbook output still needs a quick validation.
- Unmapped: no clear match in the current workbook description.
- Auxiliary: the script supports another producer script or provides a subset/variant.

## Current Review Status

- Scripts reviewed: 23
- Confirmed mappings: 13
- Likely mappings needing a second pass: 5
- Unmapped or out-of-scope scripts: 5

## Script-by-Script Review Log

| Code | Script | Status | Workbook target(s) | Notes |
| --- | --- | --- | --- | --- |
| 01 | [01_2025-04-04-Annex-earnings-workers-growth-All-V4.r](../Report/01_2025-04-04-Annex-earnings-workers-growth-All-V4.r) | Confirmed | Annex 01, Figure 12 | Main earnings/workers growth workbook. It also looks like a support source for Figure 9-style labor income comparisons. |
| 02a | [02_2025-04-01-LMR-aggregates-graph-V4.R](../Report/02_2025-04-01-LMR-aggregates-graph-V4.R) | Confirmed | Figure 1 | Main producer for the GDP, job creation, and labor earnings aggregate figure. |
| 02b | [02_2025-04-02-LMR-compound-graph-nocohh-V4.R](../Report/02_2025-04-02-LMR-compound-graph-nocohh-V4.R) | Likely | Figure 1 | Variant of the Figure 1 pipeline with a different sample/version. Keep as a backup path until the final workbook sheet is checked. |
| 03 | [03_changes_workers_by_countries-V4-nocohh.r](../Report/03_changes_workers_by_countries-V4-nocohh.r) | Likely | Figure 7 | Country-level worker-change analysis. It looks like the country-period companion to the employment-rate change graphics. |
| 04a | [04-ila-wage-growth-all-countries-V2.R](../Report/04-ila-wage-growth-all-countries-V2.R) | Confirmed | Figure 9, Figure 11, Table A2 | Country-level wage and income growth producer. This is the main source for the country comparison tables and the wage-growth panels. |
| 04b | [04-ila-wage-growth-aggregate.R](../Report/04-ila-wage-growth-aggregate.R) | Confirmed | Figure 9, Figure 11 | Aggregates the country-level wage/income data into the LAC summary. Downstream of 04a. |
| 05 | [05-create-aggregate-20-80-ila-percentiles.r](../Report/05-create-aggregate-20-80-ila-percentiles.r) | Confirmed | Figure 10 | Produces the P20, median, and P80 labor-income distribution data. |
| 06 | [06_2025-04-02-annualized-growth- jobs.R](../Report/06_2025-04-02-annualized-growth-%20jobs.R) | Confirmed | Figure 3, Table A1 | Annualized employment growth producer. The file name includes a space before jobs; that is intentional in the repository. |
| 07 | [07_annualized_growth_wap_workers_by_skill_level..R](../Report/07_annualized_growth_wap_workers_by_skill_level..R) | Likely | Figure 3 | Companion script for Figure 3. It appears to provide the working-age-population vs worker-growth comparison by skill level. |
| 08a | [08_2025-03-04_employment rate-aggregate-V3.R](../Report/08_2025-03-04_employment%20rate-aggregate-V3.R) | Confirmed | Figure 2, Figure 5 | Aggregate employment-rate and labor-market-status producer. This is the main source for the LAC summary rates. |
| 08b | [08_2025-03-04_employment rate-groups-V3.R](../Report/08_2025-03-04_employment%20rate-groups-V3.R) | Confirmed | Figure 6, Figure 7 | Disaggregated employment-rate producer by gender, age, and skill. Also feeds the country-period change comparison. |
| 09 | [09_informality and wage employment change-V2.r](../Report/09_informality%20and%20wage%20employment%20change-V2.r) | Confirmed | Figure 8 | Main informality and wage-employment change script. |
| 10a | [10_2025-03-04-Employment Count Analysis by Demograph.r](../Report/10_2025-03-04-Employment%20Count%20Analysis%20by%20Demograph.r) | Confirmed | Figure 4 | Main producer for the demographic employment-count snapshot. |
| 10b | [10_2025-04-03-LAC-9 employment totals and Growth Analysis.r](../Report/10_2025-04-03-LAC-9%20employment%20totals%20and%20Growth%20Analysis.r) | Confirmed | Tables A1, A3, A4, A5, A6 | Downstream aggregator for totals and growth tables. It is the review bucket for the employment growth tables and the coverage/exception metadata. |
| 11 | [11_2025-04-03-underemployment in LAC.r](../Report/11_2025-04-03-underemployment%20in%20LAC.r) | Confirmed | Figure B1.a, Figure B1.b | Underemployment producer for the box figures. |
| 12 | [12_2025-04-04-Revised Gini coefficient calculaton-V2.r](../Report/12_2025-04-04-Revised%20Gini%20coefficient%20calculaton-V2.r) | Confirmed | Annex 02 | Gini coefficient producer and series builder. |
| 13 | [13_last_year_jobs_by_cathegory.r](../Report/13_last_year_jobs_by_cathegory.r) | Likely | Figure 13a, Figure 13b | Sector reallocation bubble charts for Bolivia and Brazil. Keep this on the second-pass list to confirm the exact final output sheets. |
| 20 | [20_Youth_employment_2016-2024-V2.R](../Report/20_Youth_employment_2016-2024-V2.R) | Likely | Figure 5 youth components | Youth unemployment and NEET subset. It is not called out as a standalone workbook figure in the description, so treat it as an auxiliary source for the Figure 5 family. |
| 37 | [37_Colombia Youth Labor Market Analysis_by_gender.r](../Report/37_Colombia%20Youth%20Labor%20Market%20Analysis_by_gender.r) | Unmapped | None found in current workbook description | Colombia spotlight analysis. Keep in a separate side-project bucket unless a new workbook sheet is added later. |
| 38 | [38_Colombia job creation 2021-2024.R](../Report/38_Colombia%20job%20creation%202021-2024.R) | Unmapped | None found in current workbook description | Colombia-specific job-creation deep dive. Not part of the current workbook map. |
| 39a | [39_Linkedin-data management-V2.R](../Report/39_Linkedin-data%20management-V2.R) | Unmapped | None found in current workbook description | LinkedIn data preparation script. Likely feeds the LinkedIn side project, not the main workbook. |
| 39b | [39_Linkedin-regional-recent-growth.R](../Report/39_Linkedin-regional-recent-growth.R) | Unmapped | None found in current workbook description | LinkedIn regional growth analysis, outside the current workbook scope. |
| 39c | [39_Linkedin-sectoral-info.R](../Report/39_Linkedin-sectoral-info.R) | Unmapped | None found in current workbook description | LinkedIn sectoral analysis, outside the current workbook scope. |

## Workbook Sheet Map

| Workbook sheet | Script(s) | Relationship | Notes |
| --- | --- | --- | --- |
| Index | Manual | Manual | Navigation sheet; no code source identified. |
| Figure 1 | 02a, 02b | One-to-one plus variant | 02a is the main path; 02b is the alternate compound/nocohh version. |
| Figure B1.a, Figure B1.b | 11 | One-to-one | Both underemployment box figures come from the same script. |
| Figure 2 | 08a | One-to-one | Country employment-rate changes. |
| Figure 3 | 06, 07 | Main plus companion | 06 produces the growth table; 07 provides the working-age-population vs worker-growth companion. |
| Figure 4 | 10a | One-to-one | Demographic employment counts. |
| Figure 5 | 08a, 20 | Main plus auxiliary | 08a provides the aggregate rates; 20 appears to provide youth components. |
| Figure 6 | 08b | One-to-one | Employment-rate breakdown by demographic group. |
| Figure 7 | 08b, 03 | Multi-source | 08b is the main rate source; 03 looks like the country-level change companion. |
| Figure 8 | 09 | One-to-one | Informality and wage-employment changes. |
| Figure 9 | 04b, 01 | Multi-source | 04b is the main aggregator; 01 looks like an additional earnings/workers source. |
| Figure 10 | 05 | One-to-one | Percentile distribution figure. |
| Figure 11a, Figure 11b | 04a, 04b | Multi-source | 04a provides country-level values; 04b provides the aggregated view. |
| Figure 12 | 01 | One-to-one | Decomposition of labor-income changes. |
| Figure 13a, Figure 13b | 13 | One-to-many | Same script family produces the Bolivia and Brazil sector bubbles. |
| Annex 01 | 01 | One-to-one | Job quality and earnings/workers growth annex. |
| Annex 02 | 12 | One-to-one | Gini series annex. |
| Table A1 | 06, 10b | Main plus downstream table | Employment growth summary plus the LAC-9 totals pipeline. |
| Table A2 | 04a | One-to-one | Earnings growth summary. |
| Table A3 | 10b | One-to-one | Job creation rate table. |
| Table A4 | 10b | One-to-one | Variant job creation rate table. |
| Table A5 | 10b | One-to-one | Coverage and comparable-period metadata. |
| Table A6a, Table A6b | 10b | One-to-many | Exception tables for non-standard periods. |
| Table A7 | Manual | Manual | Definitions/glossary; no code source identified in the current Report folder. |

## Second-Pass List

These are the items I would review next if we want to tighten the mapping:

- [02_2025-04-02-LMR-compound-graph-nocohh-V4.R](../Report/02_2025-04-02-LMR-compound-graph-nocohh-V4.R)
- [03_changes_workers_by_countries-V4-nocohh.r](../Report/03_changes_workers_by_countries-V4-nocohh.r)
- [07_annualized_growth_wap_workers_by_skill_level..R](../Report/07_annualized_growth_wap_workers_by_skill_level..R)
- [13_last_year_jobs_by_cathegory.r](../Report/13_last_year_jobs_by_cathegory.r)
- [20_Youth_employment_2016-2024-V2.R](../Report/20_Youth_employment_2016-2024-V2.R)

## Out-Of-Scope Bucket

The following scripts do not have a current match in the workbook description and should stay in a separate bucket unless new sheets are added:

- [37_Colombia Youth Labor Market Analysis_by_gender.r](../Report/37_Colombia%20Youth%20Labor%20Market%20Analysis_by_gender.r)
- [38_Colombia job creation 2021-2024.R](../Report/38_Colombia%20job%20creation%202021-2024.R)
- [39_Linkedin-data management-V2.R](../Report/39_Linkedin-data%20management-V2.R)
- [39_Linkedin-regional-recent-growth.R](../Report/39_Linkedin-regional-recent-growth.R)
- [39_Linkedin-sectoral-info.R](../Report/39_Linkedin-sectoral-info.R)

## Notes

- The numbering in `Report/` is approximate rather than strict one-script-per-sheet.
- Some scripts are upstream producers and some are downstream aggregators; that is why a single workbook sheet can map to more than one script.
- `Table A7` and the `Index` sheet look manual rather than code-generated from the files currently in `Report/`.