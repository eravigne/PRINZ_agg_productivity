#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_match <- grep(file_arg, cmd_args, value = TRUE)

  if (length(file_match) > 0) {
    return(normalizePath(sub(file_arg, "", file_match[1]), winslash = "/", mustWork = TRUE))
  }

  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else frame$ofile
    },
    character(1)
  )
  frame_files <- frame_files[!is.na(frame_files)]

  if (length(frame_files) > 0) {
    return(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = TRUE))
  }

  NULL
}

script_path <- get_script_path()
repo_root <- if (!is.null(script_path)) {
  dirname(script_path)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

for (path in c(
  "Datasets",
  "Graphs/CCC",
  "Graphs/Domar",
  "Graphs/Energy",
  "Graphs/Zeta",
  "Tables/CCC",
  "Tables/Domar",
  "Tables/Energy",
  "Tables/Zeta",
  "output"
)) {
  ensure_dir(path)
}

parse_flag <- function(name, default = FALSE) {
  raw_value <- Sys.getenv(name, unset = "")
  if (!nzchar(raw_value)) {
    return(default)
  }

  normalized <- tolower(trimws(raw_value))
  if (normalized %in% c("1", "true", "t", "yes", "y", "on")) {
    return(TRUE)
  }
  if (normalized %in% c("0", "false", "f", "no", "n", "off")) {
    return(FALSE)
  }

  warning(
    sprintf(
      "Unrecognized value '%s' for %s. Using default = %s.",
      raw_value,
      name,
      default
    ),
    call. = FALSE
  )
  default
}

assert_files <- function(paths, step_label) {
  if (length(paths) == 0) {
    return(invisible(NULL))
  }

  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      paste0(
        "[", step_label, "] Missing required input files:\n- ",
        paste(missing, collapse = "\n- "),
        "\n\nUpdate the local Data/ and Datasets/ folders, then rerun `main.R`."
      ),
      call. = FALSE
    )
  }
}

run_source_step <- function(script, step_label, required_files = character()) {
  assert_files(required_files, step_label)

  cat(sprintf("\n[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step_label))
  started_at <- Sys.time()

  source(
    script,
    local = new.env(parent = globalenv()),
    echo = FALSE,
    chdir = FALSE
  )

  elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  cat(sprintf("[%s] Completed in %.1f seconds\n", step_label, elapsed))
}

run_render_step <- function(script, step_label, required_files = character()) {
  assert_files(required_files, step_label)

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    install.packages("rmarkdown")
  }

  cat(sprintf("\n[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step_label))
  started_at <- Sys.time()

  rmarkdown::render(
    input = script,
    output_format = "all",
    clean = TRUE,
    envir = new.env(parent = globalenv())
  )

  elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  cat(sprintf("[%s] Completed in %.1f seconds\n", step_label, elapsed))
}

core_input_files <- c(
  "Data/STAN/STANI4_2020_02102023162204854.csv",
  "Data/KLEMS_1995_2020/growth accounts.csv",
  "Data/KLEMS_1995_2020/national accounts.csv",
  "Data/NBER_CES/BEA_GDP_data.xlsx",
  "Data/NBER_CES/nberces5818v1_n2012.csv",
  "Data/NBER_CES/2012-NAICS-2-3-4-5-6-Digit-Codes-Listed-Numerically-1.xlsx",
  "Data/country_code.txt"
)

annualisation_helper_files <- c(
  "Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Balanced Net Zero Pathway.xlsx",
  "Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Headwinds.xlsx",
  "Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Tailwinds.xlsx",
  "Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Widespread Engagement.xlsx",
  "Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2_Widespread Innovation.xlsx",
  "Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2.xlsx",
  "Data/CCC_CB7/The-Seventh-Carbon-Budget-full-dataset.xlsx"
)

energy_raw_input_files <- c(
  "Data/Energy_modelling/prices_energy_BEIS_import.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_gas.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_coal.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_oil.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_solar_electricity.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_wind_electricity.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_elec_storage_batt_ST_all.xlsx",
  "Data/Way_2022/Way22_forecasts/tech_forecasts_PEM_electrolysers.xlsx"
)

energy_replication_helper_files <- c(
  "Datasets/df_quantiles_KLEMS_error_extended.RData",
  "Datasets/df_prices_energy.RData",
  "Data/Energy_modelling/table_S15_conversionfactor.xlsx",
  "Data/Energy_modelling/table_conversionfactor_primary_final.xlsx",
  "Data/Energy_modelling/tech_params.csv",
  "Data/Energy_modelling/final_elec_from_hydrogen_share.xlsx",
  "Data/Energy_modelling/scenario_energy_mix_Way.xlsx",
  "Data/Energy_modelling/CCC_CB6_energy_mix.xlsx"
)

rebuild_energy_prices <- parse_flag("PRINZ_REBUILD_ENERGY_PRICES", default = FALSE)

cat(sprintf(
  "[config] Raw energy rebuild via 4.0_treat_energy_prices.R: %s\n",
  if (rebuild_energy_prices) {
    "enabled"
  } else {
    "disabled (default saved-data replication path)"
  }
))

steps <- list(
  list(
    label = "Build base productivity datasets",
    type = "source",
    script = "1_read_national_accounting_data.R",
    required = core_input_files
  ),
  list(
    label = "Render zeta hindcasting notebook",
    type = "render",
    script = "2.1 Hindcasting_zeta.Rmd",
    required = core_input_files
  ),
  list(
    label = "Render Domar aggregation hindcasting notebook",
    type = "render",
    script = "2.2 Hindcasting_Domar_agg.Rmd",
    required = c("Data/ONS_data/supplyanduseofproductsandindustrygva.xlsx")
  ),
  list(
    label = "Run zeta regression follow-up",
    type = "source",
    script = "2.3 regressing_zeta.R"
  ),
  list(
    label = "Build CCC Sixth Carbon Budget outputs",
    type = "source",
    script = "3_CCC_CB6.R",
    required = c("Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2.xlsx")
  ),
  list(
    label = "Build CCC Seventh Carbon Budget outputs",
    type = "source",
    script = "3.2_CCC_CB7.R",
    required = c("Data/CCC_CB7/The-Seventh-Carbon-Budget-full-dataset.xlsx")
  ),
  list(
    label = "Build CCC uncertainty bands",
    type = "source",
    script = "3.1_CCC_confidence_intervals.R",
    required = c(
      "Data/ONS_data/supplyanduseofproductsandindustrygva.xlsx",
      "Datasets/tab_sic_ccc_sectors.xlsx"
    )
  ),
  list(
    label = "Run CCC annualisation sensitivity analysis",
    type = "source",
    script = "3.1_CCC_CB6_annualisation_replication.R",
    required = annualisation_helper_files
  ),
  list(
    label = "Build combined CCC comparison plots",
    type = "source",
    script = "3.3_plot_CB7_CB6.R",
    required = c(
      "Data/ONS_data/outputperhourworked.xlsx",
      "Data/ONS_data/supplyanduseofproductsandindustrygva.xlsx"
    )
  )
)

if (rebuild_energy_prices) {
  steps <- c(
    steps,
    list(
      list(
        label = "Rebuild energy price dataset from raw Way/BEIS inputs",
        type = "source",
        script = "4.0_treat_energy_prices.R",
        required = energy_raw_input_files
      )
    )
  )
}

steps <- c(
  steps,
  list(
    list(
      label = "Render energy results notebook from saved replication inputs",
      type = "render",
      script = "4_Energy_sector_focus_CLEAN.Rmd",
      required = energy_replication_helper_files
    ),
    list(
      label = "Build schematic figures",
      type = "source",
      script = "Schema/0_plot_schematic.R"
    )
  )
)

build_started_at <- Sys.time()

for (step_index in seq_along(steps)) {
  step <- steps[[step_index]]
  step_label <- sprintf("Step %d/%d: %s", step_index, length(steps), step$label)
  required_files <- if (is.null(step$required)) character() else step$required

  if (identical(step$type, "source")) {
    run_source_step(step$script, step_label, required_files)
  } else if (identical(step$type, "render")) {
    run_render_step(step$script, step_label, required_files)
  } else {
    stop("Unsupported step type: ", step$type, call. = FALSE)
  }
}

session_info_path <- file.path("output", "session_info.txt")
if (requireNamespace("sessioninfo", quietly = TRUE)) {
  writeLines(capture.output(sessioninfo::session_info()), con = session_info_path)
} else {
  writeLines(capture.output(sessionInfo()), con = session_info_path)
}

elapsed_minutes <- as.numeric(difftime(Sys.time(), build_started_at, units = "mins"))
cat(sprintf(
  "\nBuild completed successfully in %.1f minutes.\nSession info written to %s\n",
  elapsed_minutes,
  session_info_path
))
