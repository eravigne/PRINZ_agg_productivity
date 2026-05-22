# PRINZ_agg_productivity

This repository contains the R scripts used to:

- build sector-level productivity datasets from STAN, EU KLEMS, and NBER-CES;
- backtest and estimate `zeta`;
- translate CCC Sixth and Seventh Carbon Budget cost paths into implied aggregate TFP paths;
- produce the figures and tables used in the project.

The raw `Data/` folder and most generated `Graphs/` outputs are kept out of GitHub on purpose, while `Datasets/` stores the saved reproducibility objects tracked in the repo. This README explains how to recreate the local folder structure, where the public data come from, and which inputs are still project-specific helper files.

## Repository structure

- `main.R`: top-level pipeline runner for the supported end-to-end build.
- `0_set_up.R`: shared paths, package loading, plotting helpers, global parameters.
- `0.1_functions_forecast.R`: helper functions for interpolation, aggregation, and productivity calculations used by the energy and CCC scripts.
- `1_read_national_accounting_data.R`: reads STAN, EU KLEMS, and NBER-CES raw files and saves cleaned `.RData` objects in `Datasets/`.
- `2.1 Hindcasting_zeta.Rmd`: hindcasting and quantile analysis for `zeta`.
- `2.2 Hindcasting_Domar_agg.Rmd`: Domar/Hulten aggregation hindcasting and uncertainty quantiles.
- `2.3 regressing_zeta.R`: regression-based follow-up analysis using the hindcasting outputs.
- `3_CCC_CB6.R`: CCC Sixth Carbon Budget TFP calculations and plots.
- `3.1_CCC_CB6_annualisation_replication.R`: annualisation sensitivity analysis for CB6 and CB7.
- `3.1_CCC_confidence_intervals.R`: CCC uncertainty bands using hindcasting residuals.
- `3.2_CCC_CB7.R`: CCC Seventh Carbon Budget TFP calculations and plots.
- `3.3_plot_CB7_CB6.R`: comparison plots combining CB6, CB7, and uncertainty outputs.
- `4.0_treat_energy_prices.R`: optional energy-price database build from BEIS/Way-style inputs.
- `4_Energy_sector_focus_CLEAN.Rmd`: paper-facing energy notebook that builds the useful-energy figures and tables once the supporting inputs are available.
- `PAPER_REPRODUCTION.Rmd`: concise paper-order notebook showing where each main text and appendix figure/table/number comes from, using saved `Datasets/` and saved outputs whenever possible.
- `Schema/0_plot_schematic.R`: schematic figures.

## Requirements

The code assumes you run from the repository root, ideally through the RStudio project file `PRINZ_CLEAN_agg_productivity.Rproj`.

If RStudio chunk execution fails on Windows with errors mentioning `.Rproj.user/shared/notebooks/.../_rs_chunk_plot_%03d.png`, `unable to translate ... to a wide string`, or `unable to start png() device`, the problem is usually the project being opened from a path containing non-ASCII characters, such as an accented username folder. In that case:

- use `Rscript main.R` or `rmarkdown::render(...)` from the console for batch renders; or
- open the project through the helper script `open-rstudio-ascii.ps1`, which maps the repo to a temporary ASCII-only drive letter before launching RStudio.

Example:

```powershell
powershell -ExecutionPolicy Bypass -File .\open-rstudio-ascii.ps1
```

If RStudio still reuses a broken notebook cache after that, close it and rename the project-local `.Rproj.user/` directory before reopening the project.

There is no `renv.lock` in this repo, so package installation is manual. On a fresh machine, start with:

```r
pkgs <- c(
  "here", "rmarkdown", "signal",
  "tidyverse", "cowplot", "ggpubr", "extrafont", "readxl", "writexl",
  "ggfittext", "ggridges", "ggrepel", "knitr", "waterfalls", "ggforce",
  "ggdist", "directlabels", "gganimate", "gifski", "png", "kableExtra",
  "stargazer", "zoo", "data.table", "purrr", "xtable", "readODS",
  "viridis", "scales", "broom", "Polychrome", "RColorBrewer", "ggpattern",
  "ggview", "tidytext", "patchwork", "car", "lmtest", "sandwich", "nlme"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
```

## Create the local folder structure

Before running the scripts, create the expected input and output folders:

```r
dirs <- c(
  "Data/STAN",
  "Data/KLEMS_1995_2020",
  "Data/CCC_CB6",
  "Data/CCC_CB7",
  "Data/NBER_CES",
  "Data/ONS_data",
  "Data/Way_2022/Way22_forecasts",
  "Data/Energy_modelling",
  "Datasets",
  "Graphs/CCC",
  "Graphs/Zeta",
  "Graphs/Domar",
  "Tables/CCC",
  "Tables/Zeta",
  "Tables/Domar"
)

invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
```

## Run the full pipeline

The supported one-command build is now:

```r
source("main.R")
```

or from a shell:

```powershell
Rscript main.R
```

By default, `main.R` follows the saved-data replication path for energy: it expects `Datasets/df_prices_energy.RData` plus the tracked helper files in `Data/Energy_modelling/`, then renders `4_Energy_sector_focus_CLEAN.Rmd`.

If you explicitly want to rebuild the energy price dataset from the raw BEIS/Way inputs first, set:

```powershell
$env:PRINZ_REBUILD_ENERGY_PRICES = "true"
Rscript main.R
```

`main.R` runs the supported build steps in this order:

1. `1_read_national_accounting_data.R`
2. `2.1 Hindcasting_zeta.Rmd`
3. `2.2 Hindcasting_Domar_agg.Rmd`
4. `2.3 regressing_zeta.R`
5. `3_CCC_CB6.R`
6. `3.2_CCC_CB7.R`
7. `3.1_CCC_confidence_intervals.R`
8. `3.1_CCC_CB6_annualisation_replication.R`
9. `3.3_plot_CB7_CB6.R`
10. `4_Energy_sector_focus_CLEAN.Rmd`
11. `Schema/0_plot_schematic.R`

When `PRINZ_REBUILD_ENERGY_PRICES=true`, `4.0_treat_energy_prices.R` is inserted before the energy notebook as an optional raw rebuild step kept in the repo for transparency.

The script checks for key required inputs before each stage and stops with a clear error message if something important is missing.

## Paper-facing notebooks

- `PAPER_REPRODUCTION.Rmd`: the compact paper reproduction map. Render it when you want a single notebook that lists the main text and appendix assets, prints the saved figure locations, previews the figures, and pulls key numbers from saved results.
- `4_Energy_sector_focus_CLEAN.Rmd`: the dedicated energy-results notebook used for the energy section and appendix outputs.

Render them with:

```r
rmarkdown::render("PAPER_REPRODUCTION.Rmd")
rmarkdown::render("4_Energy_sector_focus_CLEAN.Rmd")
```

## Data sources

### Core public downloads used by the main workflow

| Expected local file | Used by | Public source | Notes |
|---|---|---|---|
| `Data/STAN/STANI4_2020_02102023162204854.csv` | `1_read_national_accounting_data.R` | OECD STAN dataset page: <https://www.oecd.org/en/data/datasets/structural-analysis-database.html> | Export the STAN data as CSV. The script expects this exact filename; if you download a newer extract, either rename it or update the path in the script. |
| `Data/KLEMS_1995_2020/national accounts.csv` | `1_read_national_accounting_data.R`, `2.1 Hindcasting_zeta.Rmd` | EU KLEMS download page: <https://euklems-intanprod-llee.luiss.it/download/> | Download the `All Countries` `National Accounts` CSV and save it under this local name. |
| `Data/KLEMS_1995_2020/growth accounts.csv` | `1_read_national_accounting_data.R`, `2.1 Hindcasting_zeta.Rmd` | EU KLEMS download page: <https://euklems-intanprod-llee.luiss.it/download/> | Download the `All Countries` `Growth Accounts Basic` CSV and save it under this local name. |
| `Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2.xlsx` | `3_CCC_CB6.R`, `3.1_CCC_CB6_annualisation_replication.R` | CCC Sixth Carbon Budget dataset: <https://www.theccc.org.uk/wp-content/uploads/2020/12/The-Sixth-Carbon-Budget-Dataset_v2.xlsx> | Also documented on the CCC publication page: <https://www.theccc.org.uk/publication/sixth-carbon-budget/> |
| `Data/CCC_CB7/The-Seventh-Carbon-Budget-full-dataset.xlsx` | `3.2_CCC_CB7.R`, `3.1_CCC_CB6_annualisation_replication.R` | CCC Seventh Carbon Budget dataset: <https://www.theccc.org.uk/wp-content/uploads/2025/02/The-Seventh-Carbon-Budget-full-dataset.xlsx> | Also documented on the CCC publication page: <https://www.theccc.org.uk/publication/the-seventh-carbon-budget/> |
| `Data/NBER_CES/nberces5818v1_n2012.csv` | `1_read_national_accounting_data.R`, `2.1 Hindcasting_zeta.Rmd` | NBER-CES Manufacturing Industry Database: <https://www.nber.org/research/data/nber-ces-manufacturing-industry-database> | Download the `2012 NAICS version` `CSV`. |
| `Data/NBER_CES/2012-NAICS-2-3-4-5-6-Digit-Codes-Listed-Numerically-1.xlsx` | `1_read_national_accounting_data.R`, `2.1 Hindcasting_zeta.Rmd` | NBER-CES Manufacturing Industry Database: <https://www.nber.org/research/data/nber-ces-manufacturing-industry-database> | Download the `2012 NAICS Industry Names` `Excel` file. |
| `Data/NBER_CES/BEA_GDP_data.xlsx` | `1_read_national_accounting_data.R`, `2.1 Hindcasting_zeta.Rmd` | BEA National GDP & Personal Income tables: <https://www.bea.gov/itable/national-gdp-and-personal-income> | The script reads table `T10104-A`. Save the export locally with this filename. |
| `Data/ONS_data/supplyanduseofproductsandindustrygva.xlsx` | `2.2 Hindcasting_Domar_agg.Rmd`, `3.1_CCC_confidence_intervals.R`, `3.3_plot_CB7_CB6.R` | ONS Supply and Use dataset: <https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/supplyanduseofproductsandindustrygvaukexperimental> | Save the downloaded workbook under this local name if ONS changes the filename. |
| `Data/ONS_data/outputperhourworked.xlsx` | `3.3_plot_CB7_CB6.R` | ONS Output per hour worked dataset: <https://www.ons.gov.uk/economy/economicoutputandproductivity/productivitymeasures/datasets/outputperhourworkeduk> | Save the workbook locally with this filename. |
| `Data/country_code.txt` | `1_read_national_accounting_data.R` | ISO country code CSV used in the code comments: <https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv> | The repo code reads a local text file. If needed, download the CSV above and convert or rename it to `country_code.txt`. |

### Inputs that are project-specific or only partially documented in code

These files are used by some scripts but are not recreated automatically from a single public download.

- `Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Balanced Net Zero Pathway.xlsx`
- `Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Headwinds.xlsx`
- `Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Tailwinds.xlsx`
- `Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Widespread Engagement.xlsx`
- `Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Widespread Innovation.xlsx`

These are processed CB6 scenario workbooks used by `3.1_CCC_CB6_annualisation_replication.R`. They do not appear to be created anywhere else in this repo.

- `Datasets/tab_sic_ccc_sectors.xlsx`

This SIC-to-CCC sector crosswalk is required by `3.1_CCC_confidence_intervals.R` and is not generated by the current codebase.

- `Data/Energy_modelling/*`
- `Data/Way_2022/Way22_forecasts/*`

These are used by `4.0_treat_energy_prices.R` and the energy extension helpers. The code makes clear that they rely on BEIS/DESNZ-style price assumptions and on the Way et al. technology forecast inputs. The paper that motivates the forecast files is:

- Way et al. (2022), *Empirically grounded technology forecasts and the energy transition*: <https://doi.org/10.1016/j.joule.2022.08.009>

At the moment, the repo does not include a fully scripted downloader that reconstructs the local `Energy_modelling` and `Way_2022` folders from public sources alone.

## Manual run order

### 1. Build the base productivity datasets

Run:

```r
source("1_read_national_accounting_data.R")
```

This produces the main derived objects in `Datasets/`, including:

- `df_stan_TFP_level.RData`
- `df_KLEMS_TFP_level.RData`
- `df_nber.RData`

### 2. Run the hindcasting notebooks

Run:

```r
rmarkdown::render("2.1 Hindcasting_zeta.Rmd")
rmarkdown::render("2.2 Hindcasting_Domar_agg.Rmd")
source("2.3 regressing_zeta.R")
```

These scripts populate `Datasets/` with the hindcasting objects later reused by the CCC uncertainty scripts, and write figures into `Graphs/Zeta` and `Graphs/Domar`.

### 3. Build the CCC TFP paths

Run:

```r
source("3_CCC_CB6.R")
source("3.2_CCC_CB7.R")
```

These create the main CB6 and CB7 cumulative TFP series and save outputs such as:

- `Datasets/cumsum_TFP_annual_CB6.RData`
- `Datasets/cumsum_TFP_annual_sector_CB6.RData`
- `Datasets/cumsum_TFP_annual_CB7.RData`
- `Datasets/cumsum_TFP_annual_sector_CB7.RData`

### 4. Optional CCC extensions

If you also have the processed helper files described above, you can run:

```r
source("3.1_CCC_CB6_annualisation_replication.R")
source("3.1_CCC_confidence_intervals.R")
source("3.3_plot_CB7_CB6.R")
```

These scripts produce:

- annualisation sensitivity outputs for CB6 and CB7;
- uncertainty bands based on hindcasting residuals;
- comparison figures combining CB6, CB7, and the error bands.

### 5. Optional energy-price extension

Run only if `Data/Energy_modelling/` and `Data/Way_2022/` have been prepared:

```r
source("4.0_treat_energy_prices.R")
```

This saves `Datasets/df_prices_energy.RData`.

### 6. Optional schematic figures

Run:

```r
source("Schema/0_plot_schematic.R")
```

## Notes and caveats

- The code assumes the repository root is the working directory.
- Some official data portals are live datasets. If a newer download changes sheet names, column names, or filenames, you may need to update the corresponding script paths or import ranges.
- The core public-data workflow is closest to reproducible for `1_read_national_accounting_data.R`, `2.1`, `2.2`, `3_CCC_CB6.R`, and `3.2_CCC_CB7.R`.
- The annualisation, uncertainty-band, and energy-price extensions still depend on local helper files that are not fully reconstructed by this repo alone.
- `main.R` covers the supported top-level build pipeline. It does not try to run archived notebooks in `old/` or the auxiliary `Data/STAN/STAN_compute_domar_gdp_xi.R` script, which depends on files not currently tracked in the repo.
