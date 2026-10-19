# Annualisation sensitivity analysis for CCC CB6 and CB7.
#
# Methodology
# 1. Read the CB6 processed workbooks with `readxl`, extract post-reallocation
#    capex and opex by sector, and combine them with the CB6 raw-data workbook's
#    annualised resource cost series.
# 2. Read the CB7 sector-level dataset with `readxl`, extract adjusted capex,
#    annualised capex, and opex, and reconstruct annual resource cost as
#    annualised capex plus opex.
# 3. Reuse the stored parameter table when it already exists. If it does not,
#    or if it is inconsistent with the current fitting rules, refresh only the
#    affected budget. CB6 fits both lifespan `n` and discount rate `r`; CB7
#    fixes `r = 3.5%` in line with the Seventh Carbon Budget methodology and
#    optimises only the lifespan.
# 4. Reapply the baseline `(n, r)` pairs to rebuild annualised costs, perturb
#    the baseline discount rate by +/- 1 and 2 percentage points for the
#    sector-specific sensitivity check, clamp the resulting rate at the
#    optimisation floor, and map year-on-year cost changes into cumulative TFP
#    effects using the same `zeta` and UK GDP scaling as in the old analysis.
# 5. Build ggplot objects for the annualisation comparison, the sector-specific
#    TFP sensitivity plot, and common-rate TFP sensitivity checks on a single
#    panel with a baseline uncertainty band; print them and save them under the
#    legacy CB6 names plus the CB7 variants.
#
# Outputs:
#   Graphs/CCC/g_annualisation_cost.jpeg          (CB6, legacy name)
#   Graphs/CCC/g_cumAhat_CCC_calc.jpeg            (CB6, legacy name)
#   Graphs/CCC/g_annualisation_cost_CB7.jpeg      (CB7)
#   Graphs/CCC/g_cumAhat_CCC_calc_CB7.jpeg        (CB7)
#   Graphs/CCC/g_cumAhat_CCC_calc_common_rate.jpeg      (CB6 common-rate check)
#   Graphs/CCC/g_cumAhat_CCC_calc_common_rate_CB7.jpeg  (CB7 common-rate check)
#   Graphs/CCC/g_discount_rate_by_sector.jpeg           (stored baseline rates)
#   Datasets/CCC_annualisation_optimised_params.csv

required_packages <- c("tidyverse", "readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  quoted <- paste(sprintf('"%s"', missing_packages), collapse = ", ")
  stop(
    sprintf(
      "Missing packages: %s. Install them with install.packages(c(%s)).",
      paste(missing_packages, collapse = ", "),
      quoted
    )
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

base_path <- getwd()
data_path <- file.path(base_path, "Data")
dataset_path <- file.path(base_path, "Datasets")
graph_path <- file.path(base_path, "Graphs", "CCC")
params_path <- file.path(dataset_path, "CCC_annualisation_optimised_params.csv")
klems_band_paths <- c(
  CB6 = file.path(dataset_path, "CCC_CB6_aggregate_error_bands.RData"),
  CB7 = file.path(dataset_path, "CCC_CB7_aggregate_error_bands.RData")
)
dir.create(dataset_path, recursive = TRUE, showWarnings = FALSE)
dir.create(graph_path, recursive = TRUE, showWarnings = FALSE)

# The old processed-cost annualisation block resets these values before fitting.
# Costs in this script are expressed in billion GBP, so GDP is in billion GBP.
GDP_UK_BN <- 2234
zeta <- 0.13
rate_floor <- 1e-5
common_rate_values <- c(0.01, 0.02, 0.03, 0.05, 0.074, 0.10, 0.12, 0.15)
fixed_discount_rates <- c(CB7 = 0.035)
fixed_rate_tolerance <- 1e-8

fit_scenarios <- c(
  CB6 = "Balanced Net Zero Pathway",
  CB7 = "Balanced Pathway"
)

num <- function(x) suppressWarnings(as.numeric(x))

year_cols <- function(years = 2020:2050) {
  as.character(years)
}

rolling_right_sum <- function(x, width) {
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    lo <- max(1, i - width + 1)
    out[i] <- sum(x[lo:i], na.rm = TRUE)
  }
  out
}

format_rate_pct <- function(r) {
  pct <- 100 * r
  ifelse(
    abs(pct - round(pct)) < 1e-9,
    sprintf("%.0f%%", pct),
    sprintf("%.1f%%", pct)
  )
}

load_klems_quartile_band <- function(budget_name, scenario_name) {
  path <- klems_band_paths[[budget_name]]
  if (!file.exists(path)) {
    stop("Missing KLEMS quartile band file: ", path)
  }

  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  obj_name <- ls(e)
  if (length(obj_name) == 0) {
    stop("No object found in ", path)
  }

  get(obj_name[[1]], envir = e) |>
    as_tibble() |>
    filter(scenario == scenario_name) |>
    # The band files store band levels; this plot needs offsets from the path.
    mutate(q25_error = q25_cumAhat - base_cumAhat,
           q75_error = q75_cumAhat - base_cumAhat) |>
    select(year, q25_error, q75_error)
}

# Data preparation ---------------------------------------------------------

load_cb6_cost_data <- function() {
  scenarios <- c(
    "Balanced Net Zero Pathway", "Headwinds", "Tailwinds",
    "Widespread Engagement", "Widespread Innovation"
  )

  processed <- map_dfr(scenarios, function(scenario_name) {
    path <- file.path(
      data_path,
      "CCC_CB6",
      paste0("The-Sixth-Carbon-Budget-Dataset_v2_", scenario_name, ".xlsx")
    )

    read_excel(
      path,
      range = "B825:AK853",
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ) |>
      as_tibble(.name_repair = "minimal") |>
      set_names(c("Region", "scenario", "Metric", "sector", "sector_bis", year_cols()))
  })

  operating_costs <- processed |>
    filter(
      Region == "UK",
      Metric %in% c(
        "Additional capital investment (post-reallocation)",
        "Additional operational cost (post-reallocation)"
      )
    ) |>
    mutate(
      metric = dplyr::recode(
        Metric,
        "Additional capital investment (post-reallocation)" = "capex",
        "Additional operational cost (post-reallocation)" = "opex"
      )
    ) |>
    select(scenario, sector, metric, all_of(year_cols())) |>
    pivot_longer(
      cols = all_of(year_cols()),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      year = as.integer(year),
      value = num(value)
    ) |>
    select(scenario, sector, metric, year, value) |>
    pivot_wider(
      names_from = metric,
      values_from = value,
      values_fill = list(value = 0)
    ) |>
    mutate(
      capex = replace_na(capex, 0),
      opex = replace_na(opex, 0)
    )

  base_2019 <- operating_costs |>
    distinct(scenario, sector) |>
    mutate(
      year = 2019L,
      capex = 0,
      opex = 0
    )

  annual_resource <- read_excel(
    file.path(data_path, "CCC_CB6", "The-Sixth-Carbon-Budget-Dataset_v2.xlsx"),
    sheet = "Raw data",
    col_types = "text"
  ) |>
    as_tibble() |>
    filter(
      Region == "UK",
      Metric == "Annualised resource costs"
    ) |>
    select(
      scenario = Scenario,
      sector = Amalgamation,
      all_of(year_cols())
    ) |>
    pivot_longer(
      cols = all_of(year_cols()),
      names_to = "year",
      values_to = "annual_resource"
    ) |>
    mutate(
      year = as.integer(year),
      annual_resource = num(annual_resource) / 1000
    )

  bind_rows(operating_costs, base_2019) |>
    left_join(annual_resource, by = c("scenario", "sector", "year")) |>
    mutate(budget = "CB6", .before = 1) |>
    select(budget, scenario, sector, year, capex, opex, annual_resource)
}

load_cb7_cost_data <- function() {
  keep_vars <- c(
    "Cost: additional capital expenditure (adjusted)",
    "Cost: additional capital expenditure annualised (adjusted)",
    "Cost: additional operating expenditure (adjusted)"
  )

  read_excel(
    file.path(data_path, "CCC_CB7", "The-Seventh-Carbon-Budget-full-dataset.xlsx"),
    sheet = "Sector-level data",
    col_types = "text"
  ) |>
    as_tibble() |>
    filter(
      num(year) >= 2020,
      country == "United Kingdom",
      variable %in% keep_vars
    ) |>
    mutate(
      year = as.integer(num(year)),
      value = num(value)
    ) |>
    select(scenario, country, sector, year, variable, value) |>
    pivot_wider(
      names_from = variable,
      values_from = value,
      values_fill = list(value = 0)
    ) |>
    rename(
      capex = `Cost: additional capital expenditure (adjusted)`,
      capex_ann = `Cost: additional capital expenditure annualised (adjusted)`,
      opex = `Cost: additional operating expenditure (adjusted)`
    ) |>
    mutate(
      budget = "CB7",
      annual_resource = (capex_ann + opex) / 1000,
      capex = capex / 1000,
      opex = opex / 1000
    ) |>
    select(budget, scenario, sector, year, capex, opex, annual_resource)
}

cb6_costs <- load_cb6_cost_data()
cb7_costs <- load_cb7_cost_data()
all_costs <- bind_rows(cb6_costs, cb7_costs)

# Annualisation and optimisation ------------------------------------------

annuity_factor <- function(n, r) {
  if (abs(r) < 1e-12) return(n)
  (1 - (1 + r)^(-n)) / r
}

annualise_sector <- function(costs, sector_name, n, r, scenario_name = NULL) {
  x <- costs |>
    filter(sector == sector_name)

  if (!is.null(scenario_name)) {
    x <- x |>
      filter(scenario == scenario_name)
  }

  x <- x |>
    arrange(scenario, sector, year)

  if (nrow(x) == 0) {
    return(tibble())
  }

  factor <- annuity_factor(n, r)

  x |>
    group_split(scenario, sector, .keep = TRUE) |>
    map_dfr(function(g) {
      g <- arrange(g, year)
      annualized_capex_payment <- g$capex / factor
      g |>
        mutate(
          total_cost = rolling_right_sum(annualized_capex_payment, n) + opex,
          discount_rate = r,
          horizon = n
        )
    })
}

sector_gap <- function(costs, sector_name, n, r, scenario_name) {
  x <- annualise_sector(costs, sector_name, n, r, scenario_name) |>
    filter(!is.na(annual_resource))

  if (nrow(x) == 0) {
    return(Inf)
  }

  sum(abs(x$total_cost - x$annual_resource), na.rm = TRUE)
}

optimise_sector <- function(costs, sector_name, scenario_name, max_n = 50,
                            r_lower = 1e-5, r_upper = 2, fixed_r = NULL) {
  best <- tibble(
    optimal_n = NA_integer_,
    optimal_r = NA_real_,
    minimized_gap = Inf
  )

  for (n in seq_len(max_n)) {
    if (is.null(fixed_r)) {
      opt <- try(
        optimize(
          f = function(r) sector_gap(costs, sector_name, n, r, scenario_name),
          interval = c(r_lower, r_upper)
        ),
        silent = TRUE
      )
      candidate_gap <- if (inherits(opt, "try-error")) Inf else opt$objective
      candidate_r <- if (inherits(opt, "try-error")) NA_real_ else opt$minimum
    } else {
      candidate_gap <- sector_gap(costs, sector_name, n, fixed_r, scenario_name)
      candidate_r <- fixed_r
    }

    if (is.finite(candidate_gap) && candidate_gap < best$minimized_gap[[1]]) {
      best <- tibble(
        optimal_n = n,
        optimal_r = candidate_r,
        minimized_gap = candidate_gap
      )
    }
  }

  best
}

optimise_budget <- function(costs, budget_name, scenario_name, fixed_r = NULL) {
  sectors <- costs |>
    filter(
      budget == budget_name,
      scenario == scenario_name,
      !is.na(annual_resource)
    ) |>
    distinct(sector) |>
    arrange(sector) |>
    pull(sector)

  map_dfr(sectors, function(sector_name) {
    cat("Optimising ", budget_name, " - ", sector_name, "\n", sep = "")

    optimise_sector(
      costs |> filter(budget == budget_name),
      sector_name,
      scenario_name,
      fixed_r = fixed_r
    ) |>
      mutate(
        budget = budget_name,
        scenario = scenario_name,
        sector = sector_name,
        .before = 1
      )
  }) |>
    select(budget, scenario, sector, optimal_n, optimal_r, minimized_gap)
}

read_params_table <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      optimal_n = as.integer(optimal_n),
      optimal_r = as.numeric(optimal_r),
      minimized_gap = as.numeric(minimized_gap)
    )
}

expected_param_index <- function(costs, budget_name, scenario_name) {
  costs |>
    filter(
      budget == budget_name,
      scenario == scenario_name,
      !is.na(annual_resource)
    ) |>
    distinct(budget, scenario, sector) |>
    arrange(sector)
}

needs_param_refresh <- function(params, costs, budget_name, scenario_name,
                                fixed_r = NULL) {
  required_cols <- c(
    "budget", "scenario", "sector",
    "optimal_n", "optimal_r", "minimized_gap"
  )
  if (!all(required_cols %in% names(params))) {
    return(TRUE)
  }

  expected <- expected_param_index(costs, budget_name, scenario_name)
  actual <- params |>
    filter(budget == budget_name, scenario == scenario_name) |>
    distinct(budget, scenario, sector) |>
    arrange(sector)

  if (!identical(expected, actual)) {
    return(TRUE)
  }

  budget_params <- params |>
    filter(budget == budget_name, scenario == scenario_name)

  if (nrow(budget_params) != nrow(expected)) {
    return(TRUE)
  }

  if (anyNA(budget_params$optimal_n) ||
      anyNA(budget_params$optimal_r) ||
      anyNA(budget_params$minimized_gap)) {
    return(TRUE)
  }

  if (!is.null(fixed_r) &&
      any(abs(budget_params$optimal_r - fixed_r) > fixed_rate_tolerance)) {
    return(TRUE)
  }

  FALSE
}

refresh_budget_params <- function(params, costs, budget_name, scenario_name,
                                  fixed_r = NULL) {
  if (is.null(fixed_r)) {
    cat("Refreshing parameters for ", budget_name, ".\n", sep = "")
  } else {
    cat(
      "Refreshing parameters for ", budget_name,
      " with fixed r = ", format_rate_pct(fixed_r), ".\n",
      sep = ""
    )
  }

  refreshed <- optimise_budget(
    costs,
    budget_name,
    scenario_name,
    fixed_r = fixed_r
  )

  params_base <- if (all(c("budget", "scenario") %in% names(params))) {
    params |>
      filter(!(budget == budget_name & scenario == scenario_name))
  } else {
    tibble()
  }

  params_base |>
    bind_rows(refreshed) |>
    arrange(budget, scenario, sector)
}

optimised_params <- if (file.exists(params_path)) {
  cat("Using stored parameters: ", params_path, "\n", sep = "")
  read_params_table(params_path)
} else {
  cat("Stored parameters not found. Initialising parameter table.\n")
  tibble()
}

params_changed <- FALSE
for (budget_name in names(fit_scenarios)) {
  scenario_name <- fit_scenarios[[budget_name]]
  fixed_r <- if (budget_name %in% names(fixed_discount_rates)) {
    fixed_discount_rates[[budget_name]]
  } else {
    NULL
  }

  if (needs_param_refresh(
    optimised_params,
    all_costs,
    budget_name,
    scenario_name,
    fixed_r = fixed_r
  )) {
    optimised_params <- refresh_budget_params(
      optimised_params,
      all_costs,
      budget_name,
      scenario_name,
      fixed_r = fixed_r
    )
    params_changed <- TRUE
  }
}

if (params_changed || !file.exists(params_path)) {
  write_csv(optimised_params, params_path)
}

excluded_sectors <- list(
  CB6 = "Networks",
  CB7 = character()
)

annualisation_long <- function(costs, params, budget_name, scenario_name, bias = 0,
                               common_rate = NULL) {
  params_to_use <- params |>
    filter(
      budget == budget_name,
      !is.na(optimal_n),
      !is.na(optimal_r),
      is.finite(optimal_r),
      is.finite(minimized_gap)
    )

  if (nrow(params_to_use) == 0) {
    return(tibble())
  }

  map_dfr(split(params_to_use, seq_len(nrow(params_to_use))), function(row) {
    sector_name <- row$sector[[1]]
    n <- row$optimal_n[[1]]
    base_rate <- if (is.null(common_rate)) row$optimal_r[[1]] else common_rate
    r <- max(rate_floor, base_rate + bias)

    x <- annualise_sector(
      costs |> filter(budget == budget_name),
      sector_name,
      n,
      r,
      scenario_name
    )

    if (nrow(x) == 0) {
      return(tibble())
    }

    bind_rows(
      x |>
        transmute(
          budget = budget_name,
          scenario,
          sector,
          year,
          horizon = n,
          discount_rate = r,
          bias = bias,
          common_rate = if (is.null(common_rate)) NA_real_ else common_rate,
          var = "annual_cost_ccc",
          value = annual_resource
        ),
      x |>
        transmute(
          budget = budget_name,
          scenario,
          sector,
          year,
          horizon = n,
          discount_rate = r,
          bias = bias,
          common_rate = if (is.null(common_rate)) NA_real_ else common_rate,
          var = "total_cost",
          value = total_cost
        )
    )
  })
}

# TFP sensitivity ----------------------------------------------------------

compute_sensitivity_tfp <- function(costs, params, budget_name, scenario_name,
                                    bias_values = seq(-0.02, 0.02, 0.01),
                                    common_rate = NULL) {
  x <- map_dfr(
    bias_values,
    ~annualisation_long(
      costs,
      params,
      budget_name,
      scenario_name,
      .x,
      common_rate = common_rate
    )
  )

  if (length(excluded_sectors[[budget_name]]) > 0) {
    x <- x |>
      filter(!sector %in% excluded_sectors[[budget_name]])
  }

  if (nrow(x) == 0) {
    return(tibble())
  }

  base_year <- x |>
    distinct(budget, scenario, sector, var, bias, common_rate) |>
    mutate(
      year = 2019L,
      value = 0
    )

  bind_rows(
    x |> select(budget, scenario, sector, var, year, bias, common_rate, value),
    base_year
  ) |>
    arrange(budget, scenario, sector, var, bias, common_rate, year) |>
    group_by(budget, scenario, sector, var, bias, common_rate) |>
    mutate(
      delta = value - lag(value),
      cumAhat = cumsum(replace_na(delta, 0) * (zeta - 1)) * 100 / GDP_UK_BN
    ) |>
    filter(!is.na(delta)) |>
    ungroup() |>
    group_by(budget, var, year, scenario, bias, common_rate) |>
    summarise(cumAhat = sum(cumAhat), .groups = "drop")
}

# Quick financing-cost approximation ----------------------------------------

compute_quick_financing_tfp <- function(costs, budget_name, scenario_name,
                                        financing_rate,
                                        cost_var = "annual_resource") {
  if (!cost_var %in% names(costs)) {
    stop("Unknown cost variable: ", cost_var)
  }

  costs |>
    filter(
      budget == budget_name,
      scenario == scenario_name
    ) |>
    transmute(
      sector,
      year,
      base_cost = .data[[cost_var]]
    ) |>
    filter(!is.na(base_cost)) |>
    arrange(sector, year) |>
    group_by(sector) |>
    mutate(
      delta_cost = base_cost - lag(base_cost),
      financing_cost = financing_rate * base_cost,
      delta_financing_cost = financing_rate * replace_na(delta_cost, 0)
    ) |>
    ungroup() |>
    group_by(year) |>
    summarise(
      financing_rate = financing_rate,
      financing_cost = sum(financing_cost, na.rm = TRUE),
      delta_financing_cost = sum(delta_financing_cost, na.rm = TRUE),
      ahat_financing = (zeta - 1) * delta_financing_cost * 100 / GDP_UK_BN,
      .groups = "drop"
    ) |>
    mutate(
      budget = budget_name,
      scenario = scenario_name,
      cost_var = cost_var,
      cumAhat_financing = cumsum(ahat_financing)
    ) |>
    select(
      budget,
      scenario,
      year,
      cost_var,
      financing_rate,
      financing_cost,
      delta_financing_cost,
      ahat_financing,
      cumAhat_financing
    )
}

# Plotting -----------------------------------------------------------------

make_annualisation_plot <- function(data, budget_label) {
  plot_data <- data |>
    filter(var %in% c("annual_cost_ccc", "total_cost")) |>
    filter(!sector %in% excluded_sectors[[budget_label]]) |>
    filter(!is.na(value)) |>
    mutate(
      series = dplyr::recode(
        var,
        annual_cost_ccc = "CCC annualised cost",
        total_cost = "Recomputed annualised cost"
      ),
      sector_strip = paste0(
        sector,
        "\n",
        "n: ", horizon,
        " - r: ", sprintf("%.2f", discount_rate)
      )
    )

  ggplot(
    plot_data,
    aes(x = year, y = value, colour = series, linetype = series)
  ) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~sector_strip, ncol = 4, scales = "free_y") +
    scale_colour_manual(
      values = c(
        "CCC annualised cost" = "#6C6C6C",
        "Recomputed annualised cost" = "#D55E00"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "CCC annualised cost" = "dashed",
        "Recomputed annualised cost" = "solid"
      )
    ) +
    labs(
      x = NULL,
      y = paste0(budget_label, ": annualised resource cost (bn GBP)"),
      colour = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 8)
    )
}

make_sensitivity_tfp_plot <- function(data, budget_label) {
  bias_levels <- c(-0.02, -0.01, 0, 0.01, 0.02)
  bias_palette <- c(
    "-0.02" = "#F8766D",
    "-0.01" = "#A3A500",
    "0.00" = "#E76BF3",
    "0.01" = "#00BF7D",
    "0.02" = "#00B0F6"
  )

  plot_data <- data |>
    filter(!is.na(cumAhat)) |>
    mutate(
      bias_label = sprintf("bias %+0.2f", bias),
      bias_key = sprintf("%.2f", bias),
      series = dplyr::recode(
        var,
        annual_cost_ccc = "CCC annualised cost",
        total_cost = "Recomputed annualised cost"
      )
    )

  ggplot(
    plot_data,
    aes(x = year, y = cumAhat, colour = bias_label, linetype = series)
  ) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(
      values = set_names(bias_palette[sprintf("%.2f", bias_levels)], sprintf("bias %+0.2f", bias_levels))
    ) +
    scale_linetype_manual(
      values = c(
        "CCC annualised cost" = "solid",
        "Recomputed annualised cost" = "longdash"
      )
    ) +
    scale_y_continuous(
      labels = function(x) paste0(format(round(x, 1), nsmall = 1), "pp")
    ) +
    labs(
      title = budget_label,
      x = "Year",
      y = "Cumulative TFP growth (log pp)",
      colour = "Discount rate bias",
      linetype = "CCC vs. recomputed",
      caption = "Source: CCC"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

make_common_rate_sensitivity_plot <- function(data, baseline_data, klems_band, budget_label) {
  common_lines <- data |>
    filter(!is.na(cumAhat), !is.na(common_rate), var == "total_cost") |>
    mutate(rate_label = format_rate_pct(common_rate))

  baseline_lines <- baseline_data |>
    filter(!is.na(cumAhat), bias == 0) |>
    mutate(
      series = dplyr::recode(
        var,
        annual_cost_ccc = "CCC annualised cost",
        total_cost = "Baseline"
      )
    )

  uncertainty_band <- baseline_lines |>
    filter(series == "Baseline") |>
    select(year, baseline_cumAhat = cumAhat) |>
    left_join(klems_band, by = "year") |>
    mutate(
      ymin = baseline_cumAhat + q25_error,
      ymax = baseline_cumAhat + q75_error
    ) |>
    filter(!is.na(ymin), !is.na(ymax))

  common_palette <- grDevices::hcl.colors(
    length(common_rate_values),
    palette = "Dark 3"
  )
  names(common_palette) <- format_rate_pct(common_rate_values)

  ggplot() +
    geom_ribbon(
      data = uncertainty_band,
      aes(x = year, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey70",
      alpha = 0.35
    ) +
    geom_line(
      data = common_lines,
      aes(x = year, y = cumAhat, colour = rate_label),
      linewidth = 0.75,
      alpha = 0.95
    ) +
    geom_line(
      data = baseline_lines,
      aes(x = year, y = cumAhat, linetype = series),
      colour = "black",
      linewidth = 0.9
    ) +
    scale_colour_manual(values = common_palette) +
    scale_linetype_manual(
      values = c(
        "CCC annualised cost" = "dashed",
        "Baseline" = "solid"
      )
    ) +
    scale_y_continuous(
      labels = function(x) paste0(format(round(x, 1), nsmall = 1), "pp")
    ) +
    labs(
      title = paste0(budget_label, " common-rate sensitivity"),
      x = "Year",
      y = "Cumulative TFP growth (log pp)",
      colour = "Common discount rate",
      linetype = NULL,
      caption = "Ribbon = KLEMS q25-q75 uncertainty around the baseline. Source: CCC"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

make_discount_rate_plot <- function(params) {
  plot_data <- params |>
    mutate(
      optimal_r_pct = 100 * optimal_r,
      sector_budget = paste0(sector, " (", budget, ")")
    ) |>
    arrange(optimal_r_pct) |>
    mutate(sector_budget = forcats::fct_inorder(sector_budget))

  ggplot(plot_data, aes(x = optimal_r_pct, y = sector_budget, colour = budget)) +
    geom_segment(
      aes(x = 0, xend = optimal_r_pct, yend = sector_budget),
      linewidth = 0.45,
      alpha = 0.6
    ) +
    geom_point(size = 2.4) +
    scale_colour_manual(values = c(CB6 = "#D55E00", CB7 = "#0072B2")) +
    labs(
      title = "Baseline discount rates by sector",
      x = "Discount rate (%)",
      y = NULL,
      colour = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
}

print_graph <- function(plot, width, height) {
  if (interactive()) {
    print(plot)
  } else {
    tmp <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp, width = width, height = height)
    on.exit({
      grDevices::dev.off()
      unlink(tmp)
    }, add = TRUE)
    print(plot)
  }

  invisible(plot)
}

save_graph <- function(plot, name, width, height) {
  out_path <- file.path(graph_path, paste0(name, ".jpeg"))
  print_graph(plot, width, height)
  ggsave(
    filename = out_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    device = "jpeg",
    bg = "white"
  )
  cat("Saved: ", out_path, "\n", sep = "")
}

# Run analysis -------------------------------------------------------------

cb6_annualised <- annualisation_long(
  all_costs,
  optimised_params,
  "CB6",
  fit_scenarios[["CB6"]]
)
cb7_annualised <- annualisation_long(
  all_costs,
  optimised_params,
  "CB7",
  fit_scenarios[["CB7"]]
)

cb6_tfp <- compute_sensitivity_tfp(
  all_costs,
  optimised_params,
  "CB6",
  fit_scenarios[["CB6"]]
)
cb7_tfp <- compute_sensitivity_tfp(
  all_costs,
  optimised_params,
  "CB7",
  fit_scenarios[["CB7"]]
)
cb7_financing_tfp_quick <- compute_quick_financing_tfp(
  all_costs,
  "CB7",
  fit_scenarios[["CB7"]],
  financing_rate = fixed_discount_rates[["CB7"]],
  cost_var = "annual_resource"
)
cb6_klems_band <- load_klems_quartile_band("CB6", fit_scenarios[["CB6"]])
cb7_klems_band <- load_klems_quartile_band("CB7", fit_scenarios[["CB7"]])
cb6_common_rate_tfp <- map_dfr(
  common_rate_values,
  ~compute_sensitivity_tfp(
    all_costs,
    optimised_params,
    "CB6",
    fit_scenarios[["CB6"]],
    bias_values = 0,
    common_rate = .x
  )
)
cb7_common_rate_tfp <- map_dfr(
  common_rate_values,
  ~compute_sensitivity_tfp(
    all_costs,
    optimised_params,
    "CB7",
    fit_scenarios[["CB7"]],
    bias_values = 0,
    common_rate = .x
  )
)

g_annualisation_cost <- make_annualisation_plot(cb6_annualised, "CB6")
g_cumAhat_CCC_calc <- make_sensitivity_tfp_plot(cb6_tfp, "CB6")
g_annualisation_cost_CB7 <- make_annualisation_plot(cb7_annualised, "CB7")
g_cumAhat_CCC_calc_CB7 <- make_sensitivity_tfp_plot(cb7_tfp, "CB7")
g_cumAhat_CCC_calc_common_rate <- make_common_rate_sensitivity_plot(
  cb6_common_rate_tfp,
  cb6_tfp,
  cb6_klems_band,
  "CB6"
)
g_cumAhat_CCC_calc_common_rate_CB7 <- make_common_rate_sensitivity_plot(
  cb7_common_rate_tfp,
  cb7_tfp,
  cb7_klems_band,
  "CB7"
)
g_discount_rate_by_sector <- make_discount_rate_plot(optimised_params)
g_annualisation_cost
g_cumAhat_CCC_calc
g_annualisation_cost_CB7
g_cumAhat_CCC_calc_CB7
g_cumAhat_CCC_calc_common_rate
g_cumAhat_CCC_calc_common_rate_CB7
g_discount_rate_by_sector
save_graph(g_annualisation_cost, "g_annualisation_cost", 8, 6)
save_graph(g_cumAhat_CCC_calc, "g_cumAhat_CCC_calc", 7, 5)
save_graph(g_annualisation_cost_CB7, "g_annualisation_cost_CB7", 8, 6)
save_graph(g_cumAhat_CCC_calc_CB7, "g_cumAhat_CCC_calc_CB7", 7, 5)
save_graph(
  g_cumAhat_CCC_calc_common_rate,
  "g_cumAhat_CCC_calc_common_rate",
  8.5,
  5.5
)
save_graph(
  g_cumAhat_CCC_calc_common_rate_CB7,
  "g_cumAhat_CCC_calc_common_rate_CB7",
  8.5,
  5.5
)
save_graph(
  g_discount_rate_by_sector,
  "g_discount_rate_by_sector",
  7,
  6
)

cat(
  "Saved: ",
  params_path,
  "\n",
  sep = ""
)
