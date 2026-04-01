# Jobs Brief 2025 — Graph & Data Description
**File:** `2025-04-24_Jobs Brief_ENG.xlsx`  
**Purpose:** Diagnostic reference to map each figure/table in the Excel workbook to the R scripts that produced them.  
**Total sheets:** 27 (Index + 16 figures + 2 annexes + 8 tables + 1 box figure)

---

## How to use this document

Each section below describes one sheet in the workbook:
- **Data shape:** rows × columns, key variables
- **Chart type** (if any) and what it shows
- **Geographic scope** and **time span**
- **Likely source script(s):** best guesses based on naming conventions — verify by reading the scripts

---

## INDEX sheet

Navigation map linking to all 24 figures and tables. No data or charts.

---

## Figure 1 — GDP, Job Creation, and Labor Earnings Growth in LAC

| Attribute | Details |
|-----------|---------|
| Dimensions | ~56 rows × 21 columns |
| Chart type | Line chart |
| Time span | 2017–2024 (quarterly) |
| Geography | LAC aggregate |
| Key variables | Real GDP (USD PPP 2017), Job Creation index, Labor Earnings (USD PPP 2017), Projection values |
| What it shows | Tracks how real GDP, job creation, and labor earnings co-moved over the period; includes a projection band for 2024 |

**Likely scripts:** `Report/02_2025-04-01-LMR-aggregates-graph-V4.R` or `Report/02_2025-04-02-LMR-compound-graph-nocohh-V4.R`

---

## Figure B1.a) — Labor Hour Reductions (LHRs) in LAC and Other Regions (Y-o-Y changes)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~101 rows × 17 columns |
| Chart type | Line chart |
| Time span | 2017–2025 (monthly or quarterly) |
| Geography | Individual LAC countries + sub-regional aggregates (Central America, Andean Region, Southern Cone) + non-LAC comparisons |
| Key variables | LHR values by date for: Argentina, Bolivia, Brazil, Chile, Colombia, Costa Rica, Dominican Republic, Ecuador, Guatemala, Mexico, Peru, Uruguay, Venezuela |
| What it shows | Year-over-year changes in Labor Hour Reductions across countries and sub-regions |

**Likely scripts:** `Report/11_2025-04-03-underemployment in LAC.r`

---

## Figure B1.b) — Average LHR by Region in 2017 and 2024–25

| Attribute | Details |
|-----------|---------|
| Dimensions | ~8 rows × 15 columns |
| Chart type | Bar chart (grouped) |
| Time span | 2017 vs March 2024–February 2025 |
| Geography | World regions (LAC sub-regions and comparators) |
| Key variables | `regionname`, average LHR for Jan–Dec 2017, average LHR for Mar 2024–Feb 2025 |
| What it shows | Comparison of average LHR levels across world regions between two periods |

**Likely scripts:** `Report/11_2025-04-03-underemployment in LAC.r`

---

## Figure 2 — Employment Rate Changes by Country

| Attribute | Details |
|-----------|---------|
| Dimensions | ~48 rows × 28 columns |
| Chart type | Bar chart |
| Time span | Circa 2016, 2019, 2022; growth rates 2016–2019, 2019–2024, 2016–2024 |
| Geography | ~15 individual LAC countries |
| Key variables | Country name/code, employment rate at each benchmark year, annualized growth rates for each sub-period |
| What it shows | How employment rates changed by country across the three sub-periods (pre-COVID, post-COVID recovery, overall) |

**Likely scripts:** `Report/08_2025-03-04_employment rate-aggregate-V3.R` or `Report/08_2025-03-04_employment rate-groups-V3.R`

---

## Figure 3 — Annualized Growth in 15+ Population and Employment

| Attribute | Details |
|-----------|---------|
| Dimensions | ~22 rows × 25 columns |
| Chart type | Bar chart (grouped or stacked) |
| Time span | Annualized rates for sub-periods (2016–2019, 2019–2024, 2016–2024) |
| Geography | LAC countries + LAC aggregate |
| Key variables | Working-Age Population (15+) growth rate, People Employed growth rate |
| What it shows | Side-by-side comparison of demographic growth vs employment growth — whether job creation kept pace with population growth |

**Likely scripts:** `Report/07_annualized_growth_wap_workers_by_skill_level..R` or `Report/06_2025-04-02-annualized-growth-jobs.R`

---

## Figure 4 — Employment Counts by Demographic Group

| Attribute | Details |
|-----------|---------|
| Dimensions | ~34 rows × 34 columns |
| Chart type | Bar chart (stacked or grouped) |
| Time span | Most recent comparable period (circa 2019 vs circa 2024) |
| Geography | LAC aggregate |
| Key variables | Category (Age / Gender / Skill / Location / Sector / Employment type), Subcategory label, absolute count / share |
| What it shows | Snapshot of who holds jobs in LAC: broken down by age groups, gender, skill level (low/middle/high), urban/rural, sector, and employment type (employee / self-employed / employer / unpaid) |

**Likely scripts:** `Report/10_2025-03-04-Employment Count Analysis by Demograph.r`

---

## Figure 5 — Labor Force Participation, Employment, Unemployment, and NEET Rates (LAC aggregate)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~37 rows × 11 columns |
| Chart type | Bar chart |
| Time span | Quarterly data across the full 2016–2024 range |
| Geography | LAC aggregate |
| Key variables | LFP rate, Employment rate, Unemployment rate, Youth Unemployment rate, NEET rate, Alternative NEET rate |
| What it shows | Trends in the main labor market status rates at the LAC level over time |

**Likely scripts:** `Report/08_2025-03-04_employment rate-aggregate-V3.R`

---

## Figure 6 — Employment Rates by Demographic Group (LAC aggregate)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~30 rows × 14 columns |
| Chart type | No embedded chart (data table only or chart in report body) |
| Time span | Period comparisons (2016–2019, 2019–2024) |
| Geography | LAC aggregate |
| Key variables | Total employment rate, by Gender (M/F), by Age group (15–24, 25–44, 45–54, 55–64, 65+), by Skill level (Low/Middle/High) |
| What it shows | Employment rate levels and changes disaggregated by gender, age, and skill — reveals which groups benefited most/least |

**Likely scripts:** `Report/08_2025-03-04_employment rate-groups-V3.R`

---

## Figure 7 — Employment Rate Changes by Country and Period

| Attribute | Details |
|-----------|---------|
| Dimensions | ~21 rows × 21 columns |
| Chart type | Bar chart |
| Time span | 2016–2019, 2019–2024, 2016–2024 |
| Geography | Individual LAC countries |
| Key variables | Country code/name, initial/final period values, annualized change for each sub-period |
| What it shows | Country-level employment rate changes across the three sub-periods; complements Figure 2 |

**Likely scripts:** `Report/08_2025-03-04_employment rate-groups-V3.R` or `Report/03_changes_workers_by_countries-V4-nocohh.r`

---

## Figure 8 — Informality and Wage Employment Changes

| Attribute | Details |
|-----------|---------|
| Dimensions | ~25 rows × 22 columns |
| Chart type | Bar chart |
| Time span | Change between circa 2019 and circa 2024 |
| Geography | Individual LAC countries + LAC aggregate |
| Key variables | Informality rate (productive definition), Informality rate (social protection definition), Wage employment share, country weights |
| What it shows | How informality and wage employment shares shifted by country — which countries became more or less formal |

**Likely scripts:** `Report/09_informality and wage employment change-V2.r`

---

## Figure 9 — Labor Income by Demographic Group (LAC aggregate)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~28 rows × 13 columns |
| Chart type | No embedded chart (data for chart in report) |
| Time span | Period comparisons |
| Geography | LAC aggregate |
| Key variables | Total labor income index, by Gender (M/F), by Age group, by Skill level (Low/Middle/High) |
| What it shows | How real labor income changed across demographic groups — mirrors Figure 6 but for earnings |

**Likely scripts:** `Report/04-ila-wage-growth-aggregate.R` or `Report/01_2025-04-04-Annex-earnings-workers-growth-All-V4.r`

---

## Figure 10 — Labor Income Distribution in LAC (Percentiles)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~17 rows × 6 columns |
| Chart type | Stock/range chart (fan chart showing distribution) |
| Time span | Annualized changes |
| Geography | LAC country-level |
| Key variables | Labor income index at the 20th percentile, median (50th), and 80th percentile |
| What it shows | Whether income growth was more concentrated at the top, bottom, or was broadly shared across the distribution |

**Likely scripts:** `Report/05-create-aggregate-20-80-ila-percentiles.r`

---

## Figure 11 a) and b) — Annualized Wage Growth vs GDP per Worker Growth

| Attribute | Details |
|-----------|---------|
| Dimensions | ~29 rows × 26 columns |
| Chart type | Bar chart (11a) + Scatter chart (11b) |
| Time span | Annualized rates for available periods |
| Geography | LAC countries + non-LAC comparator countries |
| Key variables | Country, LAC/non-LAC flag, Initial/Final year, Wage Growth (annualized), Labor Income Growth (annualized), GDP per worker growth |
| What it shows | (11a) Bar comparison of wage vs income growth by country; (11b) Scatter of wage growth vs GDP per worker — do wages track productivity? |

**Likely scripts:** `Report/04-ila-wage-growth-all-countries-V2.R` and/or `Report/04-ila-wage-growth-aggregate.R`

---

## Figure 12 — Decomposition of Labor Income Changes in High-Growth LAC Countries

| Attribute | Details |
|-----------|---------|
| Dimensions | ~25 rows × 9 columns |
| Chart type | Bar chart (stacked decomposition) |
| Time span | Change between two benchmark periods |
| Geography | High-growth LAC countries (subset) |
| Key variables | Decomposition components: Endowments (Education, Gender, Job characteristics, Other), Returns to endowments |
| What it shows | Oaxaca-Blinder-style decomposition — how much of the income change is explained by workers becoming more educated/skilled vs. returns to those characteristics rising |

**Likely scripts:** `Report/01_2025-04-04-Annex-earnings-workers-growth-All-V4.r`

---

## Figure 13 a) — Changes in Employment by Sector (Bolivia)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~28 rows × 23 columns |
| Chart type | Bubble chart |
| Time span | Change between two comparable periods |
| Geography | Bolivia |
| Key variables | `country`, `sector_label_etd`, `valuesEMP` (employment count), `compare_share` (sector share in base year), `change_share` (change in sector share), `relative_prod` (relative productivity) |
| What it shows | For each sector: x = relative productivity, y = change in employment share, bubble size = employment count — highlights whether Bolivia moved workers into higher-productivity sectors |

**Likely scripts:** `Report/13_last_year_jobs_by_cathegory.r`

---

## Figure 13 b) — Changes in Employment by Sector (Brazil)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~28 rows × 9 columns |
| Chart type | Bubble chart |
| Time span | Change between two comparable periods |
| Geography | Brazil |
| Key variables | Same structure as Figure 13a: `country`, `sector_label_etd`, `valuesEMP`, `compare_share`, `change_share`, `relative_prod` |
| What it shows | Same structural analysis as 13a but for Brazil — sectoral reallocation toward/away from productive sectors |

**Likely scripts:** `Report/13_last_year_jobs_by_cathegory.r`

---

## Annex 01 — Changes in Job Quality Index (JQI)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~20 rows × 17 columns |
| Chart type | Bar chart |
| Time span | Change between benchmark periods |
| Geography | LAC countries |
| Key variables | Change in JQI, Absolute Change in JQI, sub-components of the index |
| What it shows | How the overall quality of jobs (a composite index) changed across LAC countries |

**Likely scripts:** `Report/01_2025-04-04-Annex-earnings-workers-growth-All-V4.r`

---

## Annex 02 — Wage Inequality (Gini Coefficient)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~33 rows × 17 columns |
| Chart type | Bar chart + scatter chart |
| Time span | Series from ~2016 to latest available |
| Geography | LAC countries |
| Key variables | `countrycode`, `countryname`, `year`, `weighted_gini` |
| What it shows | Trends in wage inequality within each country and cross-country comparison of Gini coefficients |

**Likely scripts:** `Report/12_2025-04-04-Revised Gini coefficient calculaton-V2.r`  
**Replication script:** `R/02_Replication_gini_GLD_May_2025.r`

---

## Table A1 — Employment Growth (Annualized)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~20 rows × 21 columns |
| Countries covered | Argentina, Bolivia, Brazil, Chile, Colombia, Costa Rica, Dominican Republic, Ecuador, Mexico, Paraguay, Peru, El Salvador, Uruguay |
| Key variables | Annualized employment growth rates by period and country |
| What it shows | Summary table of employment growth rates used throughout the report figures |

**Likely scripts:** `Report/06_2025-04-02-annualized-growth-jobs.R` or `Report/10_2025-04-03-LAC-9 employment totals and Growth Analysis.r`

---

## Table A2 — Earnings Growth (Annualized)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~20 rows × 15 columns |
| Countries covered | Same as A1 |
| Key variables | Annualized labor earnings growth rates by period and country |
| What it shows | Summary table of earnings growth — corresponds to the earnings line in Figure 1 |

**Likely scripts:** `Report/04-ila-wage-growth-all-countries-V2.R`

---

## Table A3 — Job Creation Rate (Annualized)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~17 rows × 15 columns |
| Countries covered | Same as A1 |
| Key variables | Annualized job creation rates by period and country |
| What it shows | Alternative measure of job creation momentum (may differ from employment rate growth) |

**Likely scripts:** `Report/10_2025-04-03-LAC-9 employment totals and Growth Analysis.r`

---

## Table A4 — Job Creation Rate (Annualized) — variant

| Attribute | Details |
|-----------|---------|
| Dimensions | ~17 rows × 15 columns |
| Countries covered | Same as A1 |
| Key variables | Annualized job creation rates (variant definition or sample) |
| What it shows | Robustness variant or alternative weighting of A3 |

**Likely scripts:** `Report/10_2025-04-03-LAC-9 employment totals and Growth Analysis.r`

---

## Table A5 — LABLAC Coverage and Comparable Periods

| Attribute | Details |
|-----------|---------|
| Dimensions | ~28 rows × 32 columns |
| Countries covered | Full LAC coverage |
| Key variables | Country code/name, period covered, available survey years, LABLAC latest survey availability flag |
| What it shows | Metadata table documenting data availability — which countries have data for which years, and which periods are considered "comparable" |

**Likely scripts:** This is a metadata/documentation table, likely hand-assembled or from `Report/10_2025-04-03-LAC-9 employment totals and Growth Analysis.r`

---

## Table A6a — Exceptions to Standard Periods

| Attribute | Details |
|-----------|---------|
| Dimensions | ~9 rows × 25 columns |
| Key variables | Country, Period 2016–2019, Period 2019–2024, Period 2016–2024 |
| What it shows | Countries where non-standard comparable years were used (e.g., 2017 instead of 2016) due to survey availability |

---

## Table A6b — Additional Exceptions (2016–2022 period)

| Attribute | Details |
|-----------|---------|
| Dimensions | ~26 rows × 7 columns |
| Key variables | Country, period label |
| What it shows | Countries where only 2016–2022 data was available |

---

## Table A7 — Definitions

| Attribute | Details |
|-----------|---------|
| Dimensions | ~9 rows × 3 columns |
| Key variables | Term, Definition |
| What it shows | Glossary of key terms used throughout the report (informality definitions, employment rate definitions, etc.) |

---

## Cross-cutting notes

### Data source
All figures draw on **LABLAC** (Labor Statistics Database for Latin America and the Caribbean), using nationally representative household surveys. The replication scripts in `R/` use **GLD** (Global Labor Database, World Bank).

### Standard analysis periods
- **2016 → 2019:** Pre-COVID baseline
- **2019 → 2024:** COVID impact + recovery
- **2016 → 2024:** Full period

Some countries use different years (see Tables A6a/A6b) due to survey gaps.

### Script numbering convention
The `Report/` scripts are numbered to loosely match the figure numbers. The mapping is approximate:

| Script prefix | Probable figures |
|--------------|-----------------|
| `01_` | Annex 01, Figure 12 |
| `02_` | Figure 1 |
| `03_` | Figure 7 |
| `04_` | Figures 9, 11, Tables A2 |
| `05_` | Figure 10 |
| `06_` | Figure 3, Tables A3/A4 |
| `07_` | Figure 3 (WAP component) |
| `08_` | Figures 2, 5, 6, 7 |
| `09_` | Figure 8 |
| `10_` | Figure 4, Tables A1/A3/A4 |
| `11_` | Figures B1.a), B1.b) |
| `12_` | Annex 02 |
| `13_` | Figures 13a, 13b |
| `20_` | Youth sub-analysis (Figure 5 youth components) |
| `37_`, `38_` | Colombia country spotlight (not in main figures) |
| `39_` | LinkedIn data (supplementary) |
