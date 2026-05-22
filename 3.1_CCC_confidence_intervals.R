# Compute CCC forecast error bands from hindcasting calibration
# -------------------------------------------------------------
# Maps KLEMS zeta-hindcasting uncertainty onto CCC projections via
# Domar-weighted aggregation. Errors are de-meaned so the CCC baseline
# sits at the median of each band.
#
# Sign convention: error = predicted − actual (positive = overestimate).

library(here)
source(here("0_set_up.R"))
source(here("0.1_functions_forecast.R"))

# ── Data ──────────────────────────────────────────────────────────────────────

load(file.path(dataset_path, "cumsum_TFP_annual_CB7.RData"),             verbose = TRUE)
load(file.path(dataset_path, "cumsum_TFP_annual_CB6.RData"),             verbose = TRUE)
load(file.path(dataset_path, "cumsum_TFP_annual_sector_CB7.RData"),      verbose = TRUE)
load(file.path(dataset_path, "cumsum_TFP_annual_sector_CB6.RData"),      verbose = TRUE)
load(file.path(dataset_path, "df_quantiles_KLEMS_error_extended.RData"), verbose = TRUE)
load(file.path(dataset_path, "df_quantiles_hulten_extented.RData"),      verbose = TRUE)

# ── CCC data objects ──────────────────────────────────────────────────────────

cb7_to_ccc <- c(
  "Industry"                  = "M&C",
  "Land use"                  = "LULUCF sinks",
  "Residential buildings"     = "Res buildings",
  "Non-residential buildings" = "Non res buildings",
  "Engineered removals"       = "Removals"
)

ccc_cb7 <- list(
  sector = cumsum_TFP_annual_sector_CB7 %>%
    dplyr::mutate(sector = dplyr::recode(sector, !!!cb7_to_ccc)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(
      tidyr::nesting(scenario, country, sector, variable_unit, variable),
      year = 2024,
      fill = list(value = 0, delta_annual_resource = 0, cumAhat_raw = 0, cumAhat = 0)
    ),
  agg = cumsum_TFP_annual_CB7 %>%
    dplyr::ungroup() %>%
    tidyr::complete(
      tidyr::nesting(scenario),
      year = 2024,
      fill = list(cumAhat_agg = 0, cumAhat_agg_raw = 0)
    )
)

ccc_cb6 <- list(
  sector = cumsum_TFP_annual_sector_CB6 %>% dplyr::rename(year = Year, scenario = Scenario),
  agg    = cumsum_TFP_annual_CB6          %>% dplyr::rename(year = Year, scenario = Scenario)
)

forecast_start_year_cb7 <- 2025L
forecast_start_year_cb6 <- 2021L
error_distribution_empirical_cutoff <- 20L

# ── Check error distribution ──────────────────────────────────────────────────

###
# Zeta
###

df_quantiles_error_extended %>%
  ggplot(aes(x = horizon)) +
  geom_ribbon(aes(ymin=q10,ymax=q90,fill="[q10, q90]"),alpha=0.3)+
  geom_ribbon(aes(ymin=q25,ymax=q75,fill="[q25, q75]"),alpha=0.3)+
  geom_line(aes(y=q50),colour="black")+
  scale_fill_manual(values=c("[q10, q90]"="lightblue",
                             "[q25, q75]"="navy"))+
  mytheme+
  scale_y_continuous( labels = function(x) scales::number_format(scale=100,suffix='%')(transform_bp_into_pp(x)))+
  labs(colour="",
       y="Prediction error",
       fill="",x="Horizon")+
  geom_vline(xintercept = error_distribution_empirical_cutoff,linetype=2)+
  annotate("text", x=(error_distribution_empirical_cutoff+.5), y=0.2, label="Extrapolation \nof the trend", size=3)


###
# Domar
###

df_quantiles_hulten_extented %>%
  ggplot(aes(x = horizon)) +
  geom_ribbon(aes(ymin=q10,ymax=q90,fill="[q10, q90]"),alpha=0.3)+
  geom_ribbon(aes(ymin=q25,ymax=q75,fill="[q25, q75]"),alpha=0.3)+
  geom_line(aes(y=q50),colour="black")+
  scale_fill_manual(values=c("[q10, q90]"="lightblue",
                             "[q25, q75]"="navy"))+
  mytheme+
  scale_y_continuous( labels = function(x) scales::number_format(scale=100,suffix='%')(transform_bp_into_pp(x)))+
  labs(colour="",
       y="Prediction error",
       fill="",x="Horizon")+
  geom_vline(xintercept = error_distribution_empirical_cutoff,linetype=2)+
  annotate("text", x=(error_distribution_empirical_cutoff+.5), y=0.06, label="Extrapolation \nof the trend", size=3)




# ── Domar weights ─────────────────────────────────────────────────────────────

supply_use_Products <- read_excel(
  file.path(data_path, "ONS_data/supplyanduseofproductsandindustrygva.xlsx"),
  sheet = "Table 2", skip = 2, col_names = TRUE
)

supply_use_long <- supply_use_Products %>%
  dplyr::filter(Price == "CP") %>%
  tidyr::gather(key = Year, value = value, -c(1:4)) %>%
  dplyr::rename(SIC02_code = `Industry SIC Code`) %>%
  tidyr::separate(SIC02_code, into = c("code_SIC", "code_SIC_bis"), sep = "&") %>%
  dplyr::select(-code_SIC_bis) %>%
  dplyr::mutate(
    code_SIC = trimws(code_SIC),
    SIC      = stringr::str_sub(code_SIC, 1, 1),
    SIC      = dplyr::if_else(SIC == "T", "TOTAL", SIC),
    value    = as.numeric(as.character(value)),
    Year     = as.numeric(Year)
  ) %>%
  dplyr::select(Year, Component, code_SIC, SIC, value)

latest_year <- max(supply_use_long$Year, na.rm = TRUE)
ons_latest <- supply_use_long %>%
  dplyr::filter(Year == latest_year)

# Explicit concordance used for Domar weights. Detailed SIC matches take
# precedence over letter-level matches. Where the ONS table does not split a
# broad SIC between two CCC sectors, we impose explicit 50/50 shares.
ccc_domar_concordance <- tibble::tribble(
  ~match_level, ~sic_key,  ~sector,               ~sector_share,
  "detail",     "C29",     "Surface transport",   1,
  "detail",     "D351",    "Electricity supply",  1,
  "detail",     "D352_3",  "Fuel supply",         1,
  "detail",     "H50",     "Shipping",            1,
  "detail",     "H51",     "Aviation",            1,
  "letter",     "A",       "Agriculture",         0.5,
  "letter",     "A",       "LULUCF sinks",        0.5,
  "letter",     "B",       "M&C",                 1,
  "letter",     "C",       "M&C",                 1,
  "letter",     "E",       "Waste",               1,
  "letter",     "F",       "Non res buildings",   0.5,
  "letter",     "F",       "Res buildings",       0.5,
  "letter",     "H",       "Surface transport",   1
)

share_check <- ccc_domar_concordance %>%
  dplyr::summarise(total_share = sum(sector_share), .by = c(match_level, sic_key))
stopifnot(all(abs(share_check$total_share - 1) < 1e-12))

# Economy-wide P1/GVA Domar ratio (latest year)
totals_latest <- ons_latest %>%
  dplyr::group_by(Component) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

total_gva_latest <- totals_latest %>%
  dplyr::filter(Component == "GVA") %>%
  dplyr::pull(value)

total_p1_latest <- totals_latest %>%
  dplyr::filter(Component == "P1") %>%
  dplyr::pull(value)

tot_domar <- total_p1_latest / total_gva_latest

# Sector-level Domar weights (latest year)
domar_weights_by_code <- ons_latest %>%
  dplyr::group_by(code_SIC, SIC, Component) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::summarise(
    domar_weight = sum(value[Component == "P1"], na.rm = TRUE) / total_gva_latest,
    .by = c(code_SIC, SIC)
  )

detail_map <- ccc_domar_concordance %>%
  dplyr::filter(match_level == "detail") %>%
  dplyr::transmute(code_SIC = sic_key, sector, sector_share)

letter_map <- ccc_domar_concordance %>%
  dplyr::filter(match_level == "letter") %>%
  dplyr::transmute(SIC = sic_key, sector, sector_share)

domar_assignments <- dplyr::bind_rows(
  domar_weights_by_code %>%
    dplyr::semi_join(detail_map, by = "code_SIC") %>%
    dplyr::left_join(detail_map, by = "code_SIC"),
  domar_weights_by_code %>%
    dplyr::anti_join(dplyr::distinct(detail_map, code_SIC), by = "code_SIC") %>%
    dplyr::semi_join(letter_map, by = "SIC") %>%
    dplyr::left_join(letter_map, by = "SIC"),
  domar_weights_by_code %>%
    dplyr::anti_join(dplyr::distinct(detail_map, code_SIC), by = "code_SIC") %>%
    dplyr::anti_join(dplyr::distinct(letter_map, SIC), by = "SIC") %>%
    dplyr::mutate(sector = "Non_ccc", sector_share = 1)
)

allocation_check <- domar_assignments %>%
  dplyr::summarise(share_sum = sum(sector_share), .by = c(code_SIC, SIC))
stopifnot(all(abs(allocation_check$share_sum - 1) < 1e-12))

domar_weight_ccc_sectors <- domar_assignments %>%
  dplyr::mutate(domar_weight = domar_weight * sector_share) %>%
  dplyr::summarise(domar_weight = sum(domar_weight), .by = sector)

domar_sum_A_to_F <- domar_weights_by_code %>%
  dplyr::filter(SIC %in% LETTERS[1:6]) %>%
  dplyr::summarise(dw = sum(domar_weight, na.rm = TRUE)) %>%
  dplyr::pull(dw)

domar_sum_A_to_F_plus_H <- domar_weights_by_code %>%
  dplyr::filter(SIC %in% c(LETTERS[1:6], "H")) %>%
  dplyr::summarise(dw = sum(domar_weight, na.rm = TRUE)) %>%
  dplyr::pull(dw)

# Aggregate CCC Domar scale: ratio of CCC sector Domar to economy-wide Domar
domar_sum_ccc <- domar_weight_ccc_sectors %>%
  dplyr::filter(sector != "Non_ccc") %>%
  dplyr::summarise(dw = sum(domar_weight)) %>%
  dplyr::pull(dw)

domar_scale_ccc <- domar_sum_ccc / tot_domar

stopifnot(domar_sum_A_to_F > 0)
stopifnot(domar_sum_A_to_F_plus_H > 0)
stopifnot(domar_sum_ccc > 0)
stopifnot(domar_scale_ccc > 0, domar_scale_ccc < 1)
message(sprintf("Aggregate Domar: %.3f", tot_domar))
message(sprintf("Raw Domar sum for SIC A-F plus H: %.3f", domar_sum_A_to_F_plus_H))
message(sprintf("Aggregate CCC Domar sum: %.3f", domar_sum_ccc))
message(sprintf("Aggregate CCC Domar scale: %.3f", domar_scale_ccc))

ccc_error_propagation_coverage_summary <- list(
  total_domar = tot_domar,
  domar_sum_A_to_F = domar_sum_A_to_F,
  domar_sum_A_to_F_plus_H = domar_sum_A_to_F_plus_H,
  domar_sum_ccc = domar_sum_ccc,
  domar_scale_ccc = domar_scale_ccc,
  latest_year = latest_year
)

save(
  ccc_error_propagation_coverage_summary,
  file = here("Datasets", "paper_text_ccc_error_propagation_numbers.RData")
)



# ── Helpers ───────────────────────────────────────────────────────────────────

transform_bp_into_pp <- function(x) exp(x) - 1

# Toggle whether horizon-specific error draws are recentered so their median is zero.
center_error_quantiles <- TRUE

# Draw from the error distribution at horizon h.
# Optionally subtract q50 from all quantiles before constructing the inverse CDF.
get_error_draws <- function(qtab, h, n, center_at_zero = FALSE) {
  target <- dplyr::filter(qtab, horizon == h)
  if (nrow(target) == 0) stop("No quantiles for horizon ", h)

  probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  qvals <- as.numeric(target[, c("q10", "q25", "q50", "q75", "q90")])
  if (isTRUE(center_at_zero)) {
    qvals <- qvals - qvals[3]
  }

  lo <- qvals[1] - (qvals[2] - qvals[1])
  hi <- qvals[5] + (qvals[5] - qvals[4])

  inv_cdf <- stats::approxfun(c(0, probs, 1), c(lo, qvals, hi),
                               method = "linear", rule = 2)
  inv_cdf(stats::runif(n))
}

# ── Simulation function ────────────────────────────────────────────────────────

# Simulate forecast error bands for a CCC scenario object.
#
# domar_scale: aggregate Domar error scaling (set to 0 for sector-level runs
#              to avoid double-counting the aggregation term).
simulate_ccc_error <- function(ccc_obj,
                               zeta_err,
                               domar_err,
                               domar_scale         = 0,
                               sector_scale_df,
                               forecast_start_year,
                               level               = c("agg", "sector"),
                               center_error_quantiles = FALSE,
                               n_draws             = 10000) {

  level <- match.arg(level)

  # Input checks
  stopifnot(
    all(c("scenario", "year", "cumAhat_agg_raw", "cumAhat_agg") %in% names(ccc_obj$agg)),
    all(c("scenario", "year", "sector", "cumAhat_raw", "cumAhat") %in% names(ccc_obj$sector)),
    all(c("sector", "domar_weight") %in% names(sector_scale_df))
  )

  scale_tbl <- dplyr::select(sector_scale_df, sector, sector_scale = domar_weight)

  sector_h <- ccc_obj$sector %>%
    dplyr::mutate(horizon = year - forecast_start_year + 1L) %>%
    dplyr::filter(horizon >= 1L) %>%
    dplyr::left_join(scale_tbl, by = "sector") %>%
    dplyr::mutate(sector_scale = dplyr::coalesce(sector_scale, 0))

  unmatched <- sector_h %>%
    dplyr::filter(sector_scale == 0) %>%
    dplyr::distinct(sector) %>%
    dplyr::pull(sector)
  if (length(unmatched) > 0)
    warning("Sectors with no Domar weight (treated as zero): ",
            paste(unmatched, collapse = ", "))

  agg_h <- ccc_obj$agg %>%
    dplyr::mutate(horizon = year - forecast_start_year + 1L) %>%
    dplyr::filter(horizon >= 1L) %>%
    dplyr::select(scenario, year, horizon,
                  base_cumAhat_raw = cumAhat_agg_raw)

  needed_h  <- seq_len(max(agg_h$horizon))
  missing_z <- setdiff(needed_h, unique(zeta_err$horizon))
  if (length(missing_z) > 0)
    warning("zeta_err missing horizons: ", paste(missing_z, collapse = ", "))
  if (domar_scale != 0) {
    missing_d <- setdiff(needed_h, unique(domar_err$horizon))
    if (length(missing_d) > 0)
      warning("domar_err missing horizons: ", paste(missing_d, collapse = ", "))
  }

  keys <- dplyr::distinct(sector_h, scenario, year, horizon) %>%
    dplyr::arrange(scenario, year, horizon)

  # ── Sector-level path ────────────────────────────────────────────────────

  if (level == "sector") {
    sim_draws <- purrr::pmap_dfr(
      list(keys$scenario, keys$year, keys$horizon),
      function(scn, yr, h) {
        df_sy <- dplyr::filter(sector_h, scenario == scn, year == yr, horizon == h) %>%
          dplyr::arrange(sector)
        n_sec <- nrow(df_sy)

        zeta_mat <- matrix(get_error_draws(
                             zeta_err,
                             h,
                             n_sec * n_draws,
                             center_at_zero = center_error_quantiles
                           ),
                           nrow = n_draws, ncol = n_sec)
        sim_mat  <- sweep(zeta_mat, 2, df_sy$sector_scale, `*`) +
                    matrix(df_sy$cumAhat_raw, nrow = n_draws, ncol = n_sec, byrow = TRUE)

        tibble::tibble(
          scenario         = scn,
          year             = yr,
          horizon          = h,
          draw             = rep(seq_len(n_draws), times = n_sec),
          sector           = rep(df_sy$sector,       each = n_draws),
          sector_scale     = rep(df_sy$sector_scale, each = n_draws),
          base_cumAhat_raw = rep(df_sy$cumAhat_raw,  each = n_draws),
          sim_cumAhat_raw  = as.vector(sim_mat)
        )
      }
    )

    return(
      sim_draws %>%
        dplyr::group_by(scenario, year, horizon, sector, sector_scale) %>%
        dplyr::summarise(
          base_cumAhat_raw = dplyr::first(base_cumAhat_raw),
          q10_raw = quantile(sim_cumAhat_raw, 0.10),
          q25_raw = quantile(sim_cumAhat_raw, 0.25),
          q50_raw = quantile(sim_cumAhat_raw, 0.50),
          q75_raw = quantile(sim_cumAhat_raw, 0.75),
          q90_raw = quantile(sim_cumAhat_raw, 0.90),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          level        = "sector",
          base_cumAhat = transform_bp_into_pp(base_cumAhat_raw),
          q10          = transform_bp_into_pp(q10_raw),
          q25          = transform_bp_into_pp(q25_raw),
          q50          = transform_bp_into_pp(q50_raw),
          q75          = transform_bp_into_pp(q75_raw),
          q90          = transform_bp_into_pp(q90_raw)
        )
    )
  }

  # ── Aggregate path ────────────────────────────────────────────────────────

  sim_draws <- purrr::pmap_dfr(
    list(keys$scenario, keys$year, keys$horizon),
    function(scn, yr, h) {
      df_sy <- dplyr::filter(sector_h, scenario == scn, year == yr, horizon == h) %>%
        dplyr::arrange(sector)
      n_sec <- nrow(df_sy)

      zeta_mat       <- matrix(get_error_draws(
                                 zeta_err,
                                 h,
                                 n_sec * n_draws,
                                 center_at_zero = center_error_quantiles
                               ),
                               nrow = n_draws, ncol = n_sec)
      zeta_component <- rowSums(sweep(zeta_mat, 2, df_sy$sector_scale, `*`),
                                na.rm = TRUE)

      domar_component <- if (domar_scale != 0)
        domar_scale * get_error_draws(
          domar_err,
          h,
          n_draws,
          center_at_zero = center_error_quantiles
        )
      else
        rep(0, n_draws)

      base_raw <- agg_h %>%
        dplyr::filter(scenario == scn, year == yr, horizon == h) %>%
        dplyr::pull(base_cumAhat_raw)

      tibble::tibble(
        scenario         = scn,
        year             = yr,
        horizon          = h,
        draw             = seq_len(n_draws),
        base_cumAhat_raw = base_raw,
        sim_agg_raw      = base_raw + zeta_component + domar_component
      )
    }
  )

  sim_draws %>%
    dplyr::group_by(scenario, year, horizon) %>%
    dplyr::summarise(
      base_cumAhat_raw = dplyr::first(base_cumAhat_raw),
      q10_raw = quantile(sim_agg_raw, 0.10),
      q25_raw = quantile(sim_agg_raw, 0.25),
      q50_raw = quantile(sim_agg_raw, 0.50),
      q75_raw = quantile(sim_agg_raw, 0.75),
      q90_raw = quantile(sim_agg_raw, 0.90),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      level        = "agg",
      base_cumAhat = transform_bp_into_pp(base_cumAhat_raw),
      q10_cumAhat  = transform_bp_into_pp(q10_raw),
      q25_cumAhat  = transform_bp_into_pp(q25_raw),
      q50_cumAhat  = transform_bp_into_pp(q50_raw),
      q75_cumAhat  = transform_bp_into_pp(q75_raw),
      q90_cumAhat  = transform_bp_into_pp(q90_raw)
    )
}

# ── Run ───────────────────────────────────────────────────────────────────────

set.seed(123)

ccc_error_bands_cb7 <- simulate_ccc_error(
  ccc_obj             = ccc_cb7,
  zeta_err            = df_quantiles_error_extended,
  domar_err           = df_quantiles_hulten_extented,
  domar_scale         = domar_scale_ccc,
  sector_scale_df     = domar_weight_ccc_sectors,
  forecast_start_year = forecast_start_year_cb7,
  level               = "agg",
  center_error_quantiles = center_error_quantiles,
  n_draws             = 10000
)

ccc_error_bands_cb7_sector <- simulate_ccc_error(
  ccc_obj             = ccc_cb7,
  zeta_err            = df_quantiles_error_extended,
  domar_err           = df_quantiles_hulten_extented,
  domar_scale         = 0,
  sector_scale_df     = domar_weight_ccc_sectors,
  forecast_start_year = forecast_start_year_cb7,
  level               = "sector",
  center_error_quantiles = center_error_quantiles,
  n_draws             = 10000
)

ccc_error_bands_cb6 <- simulate_ccc_error(
  ccc_obj             = ccc_cb6,
  zeta_err            = df_quantiles_error_extended,
  domar_err           = df_quantiles_hulten_extented,
  domar_scale         = domar_scale_ccc,
  sector_scale_df     = domar_weight_ccc_sectors,
  forecast_start_year = forecast_start_year_cb6,
  level               = "agg",
  center_error_quantiles = center_error_quantiles,
  n_draws             = 10000
)

ccc_error_bands_cb6_sector <- simulate_ccc_error(
  ccc_obj             = ccc_cb6,
  zeta_err            = df_quantiles_error_extended,
  domar_err           = df_quantiles_hulten_extented,
  domar_scale         = 0,
  sector_scale_df     = domar_weight_ccc_sectors,
  forecast_start_year = forecast_start_year_cb6,
  level               = "sector",
  center_error_quantiles = center_error_quantiles,
  n_draws             = 10000
)

ccc_error_bands_cb7_compare_centering <-
  dplyr::bind_rows(
    simulate_ccc_error(
      ccc_obj             = ccc_cb7,
      zeta_err            = df_quantiles_error_extended,
      domar_err           = df_quantiles_hulten_extented,
      domar_scale         = domar_scale_ccc,
      sector_scale_df     = domar_weight_ccc_sectors,
      forecast_start_year = forecast_start_year_cb7,
      level               = "agg",
      center_error_quantiles = FALSE,
      n_draws             = 10000
    ) %>%
      dplyr::mutate(error_centering = "Raw distribution"),
    simulate_ccc_error(
      ccc_obj             = ccc_cb7,
      zeta_err            = df_quantiles_error_extended,
      domar_err           = df_quantiles_hulten_extented,
      domar_scale         = domar_scale_ccc,
      sector_scale_df     = domar_weight_ccc_sectors,
      forecast_start_year = forecast_start_year_cb7,
      level               = "agg",
      center_error_quantiles = TRUE,
      n_draws             = 10000
    ) %>%
      dplyr::mutate(error_centering = "Centered at Q50")
  ) %>%
  dplyr::mutate(error_centering = factor(
    error_centering,
    levels = c("Raw distribution", "Centered at Q50")
  ))

ccc_error_bands_cb6_compare_centering <-
  dplyr::bind_rows(
    simulate_ccc_error(
      ccc_obj             = ccc_cb6,
      zeta_err            = df_quantiles_error_extended,
      domar_err           = df_quantiles_hulten_extented,
      domar_scale         = domar_scale_ccc,
      sector_scale_df     = domar_weight_ccc_sectors,
      forecast_start_year = forecast_start_year_cb6,
      level               = "agg",
      center_error_quantiles = FALSE,
      n_draws             = 10000
    ) %>%
      dplyr::mutate(error_centering = "Raw distribution"),
    simulate_ccc_error(
      ccc_obj             = ccc_cb6,
      zeta_err            = df_quantiles_error_extended,
      domar_err           = df_quantiles_hulten_extented,
      domar_scale         = domar_scale_ccc,
      sector_scale_df     = domar_weight_ccc_sectors,
      forecast_start_year = forecast_start_year_cb6,
      level               = "agg",
      center_error_quantiles = TRUE,
      n_draws             = 10000
    ) %>%
      dplyr::mutate(error_centering = "Centered at Q50")
  ) %>%
  dplyr::mutate(error_centering = factor(
    error_centering,
    levels = c("Raw distribution", "Centered at Q50")
  ))

# ── Save ──────────────────────────────────────────────────────────────────────

save(ccc_error_bands_cb7,        file = here("Datasets", "CCC_CB7_aggregate_error_bands.RData"))
save(ccc_error_bands_cb7_sector, file = here("Datasets", "CCC_CB7_aggregate_error_bands_sector.RData"))
save(ccc_error_bands_cb6,        file = here("Datasets", "CCC_CB6_aggregate_error_bands.RData"))
save(ccc_error_bands_cb6_sector, file = here("Datasets", "CCC_CB6_aggregate_error_bands_sectors.RData"))

# ── Plots ─────────────────────────────────────────────────────────────────────

cb7_colour <- "#1b9e77"

g_cumAhat_CB7_errorbands <-
  ccc_error_bands_cb7 %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat, group = scenario),
              fill = cb7_colour, alpha = 0.12) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat, group = scenario),
              fill = cb7_colour, alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, group = scenario),
            colour = cb7_colour, lwd = 0.9) +
  geom_text_repel(
    data = ccc_error_bands_cb7 %>% dplyr::filter(year == max(year)),
    aes(y     = base_cumAhat,
        label = paste0(scenario, "\n",
                       round(base_cumAhat * 100, 1), "% [",
                       round(q25_cumAhat  * 100, 1), "–",
                       round(q75_cumAhat  * 100, 1), "%]")),
    colour  = cb7_colour,
    nudge_x = 2, size = 3, hjust = 0, lineheight = 0.9
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb7, max(ccc_error_bands_cb7$year) + 6) +
  labs(x = "", y = "Cumulative TFP growth",
       caption = "Shaded bands: [Q25, Q75] and [Q10, Q90] forecast uncertainty.") +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB7_errorbands

graph_path <- here("Graphs/CCC")
save_graph("g_cumAhat_CB7_errorbands", width = 9, height = 5)

g_cumAhat_CB7_errorbands_compare_centering <-
  ccc_error_bands_cb7_compare_centering %>%
  ggplot(aes(x = year,group=scenario)) +
  facet_wrap(~error_centering) +
  geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat),
              fill = cb7_colour,
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat),
              fill = cb7_colour,
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat),
            colour = cb7_colour,
            lwd = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb7, max(ccc_error_bands_cb7_compare_centering$year) + 3) +
  labs(
    x = "",
    y = "Cumulative TFP growth",
    caption = "Comparison of aggregate CB7 validation bands with empirical-median and Q50-centered error draws."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB7_errorbands_compare_centering
save_graph("g_cumAhat_CB7_errorbands_compare_centering", width = 9, height = 5)


g_cumAhat_CB7_errorbands_sector <-
  ccc_error_bands_cb7_sector %>%
  ggplot(aes(x = year)) +
  facet_wrap(~sector, scales = "free") +
  geom_ribbon(aes(ymin = q10, ymax = q90, group = scenario, fill = sector),
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25, ymax = q75, group = scenario, fill = sector),
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, group = scenario, colour = sector), lwd = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb7, max(ccc_error_bands_cb7$year) + 6) +
  labs(x = "", y = "Cumulative TFP growth",
       caption = "Shaded bands: [Q25, Q75] and [Q10, Q90] forecast uncertainty.") +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB7_errorbands_sector


g_cumAhat_CB6_errorbands <-
  ccc_error_bands_cb6 %>%
  ggplot(aes(x = year)) +
  facet_wrap(~scenario) +
  geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat,
                  fill = scenario, group = scenario), alpha = 0.12) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat,
                  fill = scenario, group = scenario), alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, colour = scenario, group = scenario), lwd = 0.9) +
  geom_text_repel(
    data = ccc_error_bands_cb6 %>% dplyr::filter(year == max(year)),
    aes(y      = base_cumAhat, colour = scenario,
        label  = paste0(scenario, "\n",
                        round(base_cumAhat * 100, 1), "% [",
                        round(q25_cumAhat  * 100, 1), "–",
                        round(q75_cumAhat  * 100, 1), "%]")),
    nudge_x = 2, size = 3, hjust = 0, lineheight = 0.9
  ) +
  scale_colour_manual(values = scenarios_palette) +
  scale_fill_manual(values = scenarios_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb6, max(ccc_error_bands_cb6$year) + 6) +
  labs(x = "", y = "Cumulative TFP growth",
       caption = "Shaded bands: [Q25, Q75] and [Q10, Q90] forecast uncertainty.") +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands

g_cumAhat_CB6_errorbands_compare_centering <-
  ccc_error_bands_cb6_compare_centering %>%
  ggplot(aes(x = year)) +
  facet_grid( scenario~error_centering) +
  geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat,
                  fill = scenario, group = interaction(scenario, error_centering)),
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat,
                  fill = scenario, group = interaction(scenario, error_centering)),
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, colour = scenario,
                group = interaction(scenario, error_centering)),
            lwd = 0.9) +
  scale_colour_manual(values = scenarios_palette) +
  scale_fill_manual(values = scenarios_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb6, max(ccc_error_bands_cb6_compare_centering$year) + 3) +
  labs(
    x = "",
    y = "Cumulative TFP growth",
    caption = "Comparison of aggregate CB6 validation bands with empirical-median and Q50-centered error draws."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands_compare_centering+canvas(6,14,"in")
save_graph("g_cumAhat_CB6_errorbands_compare_centering", width = 6, height = 14)



g_cumAhat_CB6_BNZ_errorbands_compare_centering <-
  ccc_error_bands_cb6_compare_centering %>%
  filter(scenario=="Balanced Net Zero Pathway")%>%
  ggplot(aes(x = year)) +
  facet_grid( scenario~error_centering) +
  geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat,
                  fill = scenario, group = interaction(scenario, error_centering)),
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat,
                  fill = scenario, group = interaction(scenario, error_centering)),
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, colour = scenario,
                group = interaction(scenario, error_centering)),
            lwd = 0.9) +
  scale_colour_manual(values = scenarios_palette) +
  scale_fill_manual(values = scenarios_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb6, max(ccc_error_bands_cb6_compare_centering$year) + 3) +
  labs(
    x = "",
    y = "Cumulative TFP growth",
    caption = "Comparison of aggregate CB6 validation bands with empirical-median and Q50-centered error draws."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_BNZ_errorbands_compare_centering+canvas(9,5,"in")
save_graph("g_cumAhat_CB6_BNZ_errorbands_compare_centering", width = 9, height = 5)



g_cumAhat_CB6_errorbands_no_facet <-
  ccc_error_bands_cb6 %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat,
                  fill = scenario, group = scenario), alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, colour = scenario, group = scenario), lwd = 0.9) +
  scale_colour_manual(values = scenarios_palette) +
  scale_fill_manual(values = scenarios_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb6, max(ccc_error_bands_cb6$year) + 6) +
  labs(x = "", y = "Cumulative TFP growth",
       caption = "Shaded bands: [Q25, Q75] forecast uncertainty.") +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands_no_facet


g_cumAhat_CB6_errorbands_sector <-
  ccc_error_bands_cb6_sector %>%
  dplyr::filter(scenario == "Balanced Net Zero Pathway") %>%
  ggplot(aes(x = year)) +
  facet_wrap(~sector, scales = "free") +
  geom_ribbon(aes(ymin = q10, ymax = q90, group = scenario, fill = sector),
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25, ymax = q75, group = scenario, fill = sector),
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, group = scenario, colour = sector), lwd = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year_cb6, max(ccc_error_bands_cb6$year) + 6) +
  labs(x = "", y = "Cumulative TFP growth",
       caption = "Shaded bands: [Q25, Q75] and [Q10, Q90] forecast uncertainty.") +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands_sector
