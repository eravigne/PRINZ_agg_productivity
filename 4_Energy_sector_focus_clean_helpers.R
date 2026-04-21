# Helper functions for 4_Energy_sector_focus_CLEAN.Rmd

read_CCC_scenario_sheet <- function(filepath, sheet_name) {
  read_excel(path = filepath, sheet = sheet_name)
}

read_way_scenario_sheet <- function(sheet_name) {
  read_excel(
    path = file.path(energy_path, "scenario_energy_mix_Way.xlsx"),
    sheet = sheet_name
  ) %>%
    select(-energy_way) %>%
    gather(key = Year, value = q, -c("Scenario_q", "energy")) %>%
    mutate(Year = as.numeric(Year))
}

read_conversion_factor <- function() {
  df_x_final_useful <<-
    read_excel(
      file.path(data_path, "Energy_modelling/table_S15_conversionfactor.xlsx"),
      sheet = "x_final_useful"
    )

  df_x_primary_final <<-
    read_excel(
      file.path(data_path, "Energy_modelling/table_conversionfactor_primary_final.xlsx"),
      sheet = "x_primary_final"
    )
}

rename_energy <- function(df) {
  df %>%
    mutate(
      energy = factor(
        energy,
        levels = rev(c(
          "gas", "oil", "solid", "non_bio_waste", "hydrogen", "bioenergy",
          "elec_unabated_gas", "elec_gas_CCS", "elec_other_gasCHP", "elec_biomass",
          "elec_biomass_CCS", "elec_coal", "elec_hydrogen", "elec_other_hydro",
          "Nuclear", "wind_offshore", "wind_onshore", "solar", "storage"
        )),
        labels = rev(c(
          "Gas", "Oil", "Solid", "Waste", "Hydrogen", "Bioenergy",
          "Gas electricity", "Gas+CCS electricity", "Gas+coal electricity",
          "Biomass electricity", "Biomass+CCS electricity", "Coal electricity",
          "Hydrogen electricity", "Hydro-electricity", "Nuclear",
          "Offshore Wind electricity", "Onshore Wind electricity",
          "Solar PV electricity", "Storage"
        ))
      )
    )
}

convert_EJ_to_TWh <- function(df) {
  df %>%
    mutate(value = ifelse(variable == "q", value / 0.0036, value))
}

convert_p_final_to_useful <- function(df_merged) {
  if ("x_final_useful" %in% colnames(df_merged)) {
    df_merged <- df_merged %>% select(-x_final_useful)
  }

  df_merged %>%
    left_join(df_x_final_useful, by = "energy") %>%
    mutate(value = ifelse(variable == "p", value / x_final_useful, value))
}

convert_final_to_useful <- function(df_merged) {
  if ("x_final_useful" %in% colnames(df_merged)) {
    df_merged <- df_merged %>% select(-x_final_useful)
  }

  df_merged %>%
    left_join(df_x_final_useful, by = "energy") %>%
    mutate(
      value = ifelse(variable == "p", value / x_final_useful, value),
      value = ifelse(variable == "q", value * x_final_useful, value)
    )
}

convert_useful_to_final <- function(df_merged) {
  if ("x_final_useful" %in% colnames(df_merged)) {
    df_merged <- df_merged %>% select(-x_final_useful)
  }

  df_merged %>%
    left_join(df_x_final_useful, by = "energy") %>%
    mutate(
      value = ifelse(variable == "p", value * x_final_useful, value),
      value = ifelse(variable == "q", value / x_final_useful, value)
    )
}

prepare_price_data <- function(dp_raw) {
  df <- dp_raw %>% spread(key = energy, value = value)
  df$wind_onshore <- df$wind
  df$wind_offshore <- df$wind

  df %>%
    gather(key = "energy", value = "p", -c("Year", "Scenario_p")) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(p = as.numeric(p))
}

compute_vintage_price <- function(df_q, df_p,
                                  match_p_q = TRUE,
                                  keep_components = FALSE,
                                  clamp_negative_k = TRUE,
                                  anchor_year = 2022L) {
  dq <- df_q %>%
    select(Year, energy, Scenario_q, q) %>%
    mutate(
      energy = as.character(energy),
      q = as.numeric(q)
    )

  dp <- df_p %>%
    select(Year, energy, any_of("Scenario_p"), p) %>%
    mutate(
      energy = as.character(energy),
      p = as.numeric(p)
    )

  L <- dq %>%
    distinct(energy) %>%
    left_join(
      df_lifespan %>%
        transmute(
          energy = as.character(energy),
          lifespan = as.integer(lifespan)
        ),
      by = "energy"
    ) %>%
    mutate(lifespan = ifelse(is.na(lifespan), 0L, lifespan))

  if (!is.null(anchor_year)) {
    anchor_year <- as.integer(anchor_year)
  }

  L0 <- L %>% filter(lifespan == 0L) %>% distinct(energy)
  Lpos <- L %>% filter(lifespan > 0L) %>% distinct(energy, lifespan)

  infer_k <- function(q_vec, L) {
    n <- length(q_vec)
    k <- numeric(n)
    for (t in seq_len(n)) {
      lookback <- max(0, min(L - 1, t - 1))
      if (lookback > 0) {
        k[t] <- q_vec[t] - sum(k[(t - lookback):(t - 1)])
      } else {
        k[t] <- q_vec[t]
      }
      if (clamp_negative_k && !is.na(k[t]) && k[t] < 0) {
        k[t] <- 0
      }
    }
    k
  }

  marginal_price_from_dfq <- function(dfq_subset) {
    if (nrow(dfq_subset) == 0) {
      return(tibble(
        Year = integer(),
        Scenario_q = character(),
        energy = character(),
        p_vintage = double()
      ))
    }

    if (match_p_q) {
      dfq_join <- dfq_subset %>%
        left_join(dp, by = c("Year", "energy", "Scenario_q" = "Scenario_p"))
    } else {
      dfq_join <- dfq_subset %>%
        left_join(dp, by = c("Year", "energy"))
    }

    dfq_join %>%
      filter(is.finite(p), is.finite(q)) %>%
      group_by(Year, Scenario_q, energy) %>%
      summarise(p_vintage = weighted.mean(p, q), .groups = "drop")
  }

  dq_pos <- dq %>% semi_join(Lpos, by = "energy")
  dq_pre_anchor <- if (is.null(anchor_year)) {
    dq_pos %>% slice(0)
  } else {
    dq_pos %>% filter(Year < anchor_year)
  }
  dq_stock_period <- if (is.null(anchor_year)) {
    dq_pos
  } else {
    dq_pos %>% filter(Year >= anchor_year)
  }

  k_long <- dq_stock_period %>%
    left_join(Lpos, by = "energy") %>%
    group_by(energy, Scenario_q) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(k_build = infer_k(q, unique(lifespan))) %>%
    ungroup()

  if (nrow(k_long) == 0) {
    w_alive <- tibble(
      Year = integer(),
      energy = character(),
      Scenario_q = character(),
      BuildYear = integer(),
      w = double()
    )
  } else {
    w_alive <- k_long %>%
      rename(BuildYear = Year) %>%
      rowwise() %>%
      mutate(UseYears = list(seq.int(BuildYear, BuildYear + lifespan - 1L))) %>%
      ungroup() %>%
      unnest(UseYears) %>%
      transmute(Year = UseYears, energy, Scenario_q, BuildYear, w = k_build)
  }

  if (nrow(w_alive) > 0) {
    if (match_p_q) {
      w_alive <- w_alive %>%
        left_join(
          dp %>% rename(BuildYear = Year),
          by = c("BuildYear", "energy", "Scenario_q" = "Scenario_p")
        )
    } else {
      w_alive <- w_alive %>%
        left_join(
          dp %>% rename(BuildYear = Year),
          by = c("BuildYear", "energy")
        )
    }

    p_vint_pos <- w_alive %>%
      filter(is.finite(p), is.finite(w)) %>%
      group_by(Year, Scenario_q, energy) %>%
      summarise(
        p_vintage = if (sum(w) == 0) NA_real_ else weighted.mean(p, w),
        .groups = "drop"
      )
  } else {
    p_vint_pos <- tibble(
      Year = integer(),
      Scenario_q = character(),
      energy = character(),
      p_vintage = double()
    )
  }

  dq_0 <- dq %>% semi_join(L0, by = "energy")
  p_vint_0 <- marginal_price_from_dfq(bind_rows(dq_0, dq_pre_anchor))

  out <- bind_rows(p_vint_pos, p_vint_0) %>%
    arrange(Year, Scenario_q, energy) %>%
    rename(Scenario_p = Scenario_q)

  if (keep_components) {
    list(
      p_vintage = out,
      implied_cohorts = k_long %>% select(energy, Scenario_q, BuildYear = Year, k_build),
      weights_alive = w_alive
    )
  } else {
    out
  }
}

calculate_wind_solar_ratio <- function(df_q) {
  df_q2022 <- df_q %>%
    filter(Year == 2022, energy %in% c("solar", "wind_offshore", "wind_onshore")) %>%
    group_by(Scenario_q) %>%
    summarise(q2022 = sum(q), .groups = "drop")

  df_q %>%
    filter(energy %in% c("solar", "wind_offshore", "wind_onshore")) %>%
    group_by(Year, Scenario_q) %>%
    summarise(q = sum(q), .groups = "drop") %>%
    left_join(df_q2022, by = "Scenario_q") %>%
    rowwise() %>%
    mutate(ratio_2022 = max(0, (q - q2022) / q)) %>%
    ungroup() %>%
    select(Scenario_q, Year, ratio_2022)
}

merge_price_quantity_and_add_network <- function(df_q, df_p, cost_add = 7,
                                                 match_p_q = FALSE,
                                                 overgeneration = 1.5,
                                                 use_vintage = TRUE) {
  df_p_marginal <- df_p %>%
    mutate(
      p = ifelse(
        energy %in% c("solar", "wind_offshore", "wind_onshore", "wind"),
        p * overgeneration,
        p
      )
    )

  df_p_used <- df_p_marginal
  if (use_vintage) {
    p_vint <- compute_vintage_price(df_q, df_p, match_p_q = match_p_q)
    df_p_used <- p_vint %>%
      rename(p = p_vintage) %>%
      mutate(
        p = ifelse(
          energy %in% c("solar", "wind_offshore", "wind_onshore", "wind"),
          p * overgeneration,
          p
        )
      )
  }

  df_ratio <- calculate_wind_solar_ratio(df_q)

  if (match_p_q) {
    df_pq <- df_q %>%
      left_join(
        df_p_used %>% rename(p_used = p),
        by = c("Year", "energy", "Scenario_q" = "Scenario_p")
      ) %>%
      left_join(
        df_p_marginal %>%
          select(Year, energy, Scenario_p, p) %>%
          rename(p_marginal = p),
        by = c("Year", "energy", "Scenario_q" = "Scenario_p")
      )
  } else {
    df_pq <- df_q %>%
      left_join(
        df_p_used %>% rename(p_used = p),
        by = c("Year", "energy")
      ) %>%
      left_join(
        df_p_marginal %>%
          select(Year, energy, p) %>%
          rename(p_marginal = p),
        by = c("Year", "energy")
      )
  }

  df_pq %>%
    mutate(p = dplyr::coalesce(p_used, ifelse(q <= 0, p_marginal, NA_real_))) %>%
    left_join(df_ratio, by = c("Year", "Scenario_q")) %>%
    mutate(ratio_2022 = ifelse(is.na(ratio_2022), 0, ratio_2022)) %>%
    mutate(
      p = ifelse(
        energy %in% c("solar", "wind_offshore", "wind_onshore"),
        p + cost_add * ratio_2022,
        p
      )
    ) %>%
    select(-p_used, -p_marginal) %>%
    gather(key = "variable", value = "value", c("p", "q"))
}

prepare_quantity_data_way <- function(dq_raw) {
  df <- dq_raw %>%
    left_join(final_elec_from_hydrogen_share, by = c("Year" = "year", "Scenario_q")) %>%
    spread(key = energy, value = q) %>%
    mutate(
      storage = (solar + wind_offshore) * 1.5 * 0.3 / 365,
      elec_hydrogen = final_elec_from_hydrogen_share * (solar + wind_offshore)
    ) %>%
    select(-final_elec_from_hydrogen_share) %>%
    gather(key = "energy", value = "q", -c("Scenario_q", "Year")) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(q = as.numeric(q))

  cap_storage_growth(df) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(q = as.numeric(q)) %>%
    spread(key = energy, value = q) %>%
    mutate(storage = storage * 365) %>%
    gather(key = "energy", value = "q", -c("Scenario_q", "Year"))
}

prepare_quantity_data_CCC <- function(dq_raw, add_storage_h2 = FALSE) {
  df <- dq_raw

  if (add_storage_h2) {
    df <- df %>%
      left_join(final_elec_from_hydrogen_share_Fast_Transition, by = c("Year" = "year")) %>%
      mutate(
        storage = (solar + wind_offshore + wind_onshore) * 1.5 * 0.3 / 365,
        elec_hydrogen = final_elec_from_hydrogen_share * (solar + wind_offshore + wind_onshore)
      ) %>%
      select(-final_elec_from_hydrogen_share)
  }

  df <- df %>%
    gather(key = "energy", value = "q", -c("Scenario_q", "Year", "Unit", "variable")) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(q = as.numeric(q))

  if (add_storage_h2) {
    df <- cap_storage_growth(df) %>%
      filter(!energy %in% c("all_elec", "total")) %>%
      mutate(q = as.numeric(q)) %>%
      spread(key = energy, value = q) %>%
      mutate(storage = storage * 365) %>%
      gather(key = "energy", value = "q", -c("Scenario_q", "Year", "Unit", "variable")) %>%
      mutate(q = as.numeric(q))
  }

  df
}

elec_storage_batt_ST_electricity_0 <- 2.5 / 1000 * 0.0036
elec_storage_batt_LT_electricity_0 <- 0.12 / 1000 * 0.0036

cap_storage_growth <- function(df_q) {
  years_range <- seq(2018, 2070)
  q_ST <- tibble(
    Year = years_range,
    q_ST = elec_storage_batt_ST_electricity_0 * (1 + 0.6)^(c(1:53) + 1)
  )
  q_LT <- tibble(
    Year = years_range,
    q_LT = elec_storage_batt_LT_electricity_0 * (1 + 0.8)^(c(1:53) + 1)
  )

  cols_fin <- colnames(df_q %>% select(-energy, -q))

  q_storage <- df_q %>%
    ungroup() %>%
    filter(energy == "storage") %>%
    spread(key = energy, value = q) %>%
    mutate(
      q_fraction_ST = storage / 3,
      q_fraction_LT = 2 * storage / 3
    ) %>%
    left_join(q_LT, by = "Year") %>%
    left_join(q_ST, by = "Year") %>%
    rowwise() %>%
    mutate(
      q_fraction_ST = min(q_fraction_ST, q_ST),
      q_fraction_LT = min(q_fraction_LT, q_LT),
      storage = q_fraction_LT + q_fraction_ST
    ) %>%
    ungroup() %>%
    select(all_of(cols_fin), storage)

  df_q %>%
    spread(key = energy, value = q) %>%
    select(-storage) %>%
    left_join(q_storage, by = cols_fin) %>%
    gather(key = energy, value = q, -all_of(cols_fin))
}

add_is_elec <- function(df) {
  df %>%
    mutate(
      is_elec = ifelse(
        energy %in% c(
          "elec_unabated_gas", "elec_hydrogen", "elec_gas_CCS", "elec_biomass_CCS",
          "Nuclear", "elec_biomass", "elec_other_gasCHP", "wind_offshore",
          "wind_onshore", "solar", "elec_other_hydro", "elec_coal", "storage",
          "Gas electricity", "Gas+CCS electricity", "Gas+coal electricity",
          "Biomass electricity", "Biomass+CCS electricity", "Coal electricity",
          "Hydrogen electricity", "Hydro-electricity", "Offshore Wind electricity",
          "Onshore Wind electricity", "Solar PV electricity", "Storage"
        ),
        "elec",
        "non_elec"
      )
    )
}

transform_bp_into_pp <- function(x) {
  exp(x) - 1
}

compute_sales <- function(df_merged, exch_rate) {
  df_merged %>%
    ungroup() %>%
    spread(key = "variable", value = "value") %>%
    group_by(Scenario_q, Year) %>%
    filter(!is.na(p) & !is.na(q)) %>%
    summarise(sales = sum(p * q * 1e6 / exch_rate), .groups = "drop")
}

compute_domar <- function(df_merged,
                          Year_select = 2022,
                          gdp_year = GDP_UK,
                          exch_rate = 1) {
  df_sales <- compute_sales(df_merged %>% ungroup(), exch_rate)

  df_sales %>%
    mutate(domar = sales / gdp_year) %>%
    filter(Year == Year_select) %>%
    select(domar)
}

compute_Ahat <- function(df_merged, zeta_value = zeta, start_year = 2022) {
  df_merged %>%
    filter(Year >= start_year) %>%
    spread(key = "variable", value = "value") %>%
    group_by(Year, Scenario_q) %>%
    summarise(
      p = weighted.mean(p, q, na.rm = TRUE),
      q = sum(q, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Scenario_q) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
      qhat = log(q) - log(dplyr::lag(q)),
      phat = log(p) - log(dplyr::lag(p)),
      Ahat = zeta_value * qhat + (zeta_value - 1) * phat
    )
}

compute_cumulative_Ahat <- function(df_ah) {
  df_ah %>%
    filter(!is.na(Ahat)) %>%
    group_by(Scenario_q) %>%
    mutate(cum_Ahat = cumsum(Ahat)) %>%
    ungroup()
}

compute_ahat_decomposition <- function(df_merged,
                                       zeta_value = zeta,
                                       start_year = 2022,
                                       year_stop = 2050) {
  df_merged %>%
    filter(Year >= start_year) %>%
    spread(key = "variable", value = "value") %>%
    group_by(Year, Scenario_q) %>%
    summarise(
      p = weighted.mean(p, q, na.rm = TRUE),
      q = sum(q, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Scenario_q) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
      qhat = log(q) - log(dplyr::lag(q)),
      phat = log(p) - log(dplyr::lag(p)),
      Ahat = zeta_value * qhat + (zeta_value - 1) * phat,
      Ahat_p = (zeta_value - 1) * phat,
      Ahat_q = zeta_value * qhat
    ) %>%
    filter(!is.na(Ahat) & !is.na(Ahat_p)) %>%
    mutate(
      cum_Ahat_p = cumsum(Ahat_p),
      cum_Ahat_q = cumsum(Ahat_q),
      cum_Ahat = cumsum(Ahat)
    ) %>%
    ungroup() %>%
    filter(Year == year_stop) %>%
    select(Scenario_q, Year, cum_Ahat_q, cum_Ahat_p, cum_Ahat)
}

extract_TFP_energy <- function(df,
                               Year_TFP = 2050,
                               quartiles = FALSE,
                               df_error = df_quantiles_error_extended) {
  df_aihat <- compute_Ahat(df)

  out <- df_aihat %>%
    filter(!is.na(Ahat), Year <= Year_TFP) %>%
    mutate(cum_Ahat = cumsum(Ahat)) %>%
    left_join(df_error %>% mutate(Year = 2022 + horizon), by = "Year") %>%
    mutate(
      q10 = cum_Ahat + q10,
      q25 = cum_Ahat + q25,
      q75 = cum_Ahat + q75,
      q90 = cum_Ahat + q90
    ) %>%
    filter(Year == Year_TFP) %>%
    mutate(
      cum_hat_pp = transform_bp_into_pp(cum_Ahat),
      q10_pp = transform_bp_into_pp(q10),
      q25_pp = transform_bp_into_pp(q25),
      q75_pp = transform_bp_into_pp(q75),
      q90_pp = transform_bp_into_pp(q90)
    )

  if (quartiles) {
    out %>% select(Scenario_q, cum_hat_pp, q10_pp, q25_pp, q75_pp, q90_pp)
  } else {
    out %>% select(Scenario_q, cum_hat_pp)
  }
}

compute_impact_agg_economy_sub <- function(df, df_domar, Year_TFP = 2050) {
  impact <- df %>%
    ungroup() %>%
    filter(Year == Year_TFP) %>%
    select(Scenario_q, cum_Ahat) %>%
    mutate(
      domar = df_domar$domar,
      cum_hat_agg = cum_Ahat * domar,
      cum_hat_agg_year = cum_hat_agg / (Year_TFP - 2022),
      cum_hat_pp_agg = transform_bp_into_pp(cum_hat_agg),
      cum_hat_pp_agg_year = transform_bp_into_pp(cum_hat_agg_year)
    )

  c(
    "Agg impact %" = impact$cum_hat_pp_agg,
    "Agg impact pp" = impact$cum_hat_pp_agg * 100,
    "Agg impact pp/year" = impact$cum_hat_pp_agg_year * 100
  )
}

compute_impact_agg_economy <- function(df, GDP, exchange_rate, Year_TFP = 2050) {
  df_domar <- compute_domar(df, gdp_year = GDP, exch_rate = exchange_rate)
  df_cumAihat <- compute_cumulative_Ahat(compute_Ahat(df))
  compute_impact_agg_economy_sub(df_cumAihat, df_domar, Year_TFP)
}

compute_c_agg_impact <- function(df) {
  c_impacts <- c()

  for (x in unique(df$Scenario_q)) {
    data <- df %>% filter(Scenario_q == x)

    if (x %in% c("Balanced Net Zero Pathway", "UK fast transition")) {
      gdp <- GDP_UK
      er <- exchange_rate_UK_US
    } else {
      gdp <- GDP_world
      er <- 1
      data <- convert_EJ_to_TWh(data)
    }

    c_agg_impacts <- log(
      compute_impact_agg_economy(
        df = data,
        GDP = gdp,
        exchange_rate = er,
        Year_TFP = 2050
      )[1] + 1
    )

    c_impacts <- rbind(
      c_impacts,
      c("Scenario_q" = x, "Year" = 2050, agg_impact = as.numeric(c_agg_impacts))
    )
  }

  as.data.frame(c_impacts) %>%
    mutate(Year = as.numeric(Year), agg_impact = as.numeric(agg_impact))
}

save_tab_Ahat_decomposition <- function(df) {
  tab_log_pp <- compute_ahat_decomposition(df) %>%
    left_join(compute_c_agg_impact(df))

  tab_pp <- tab_log_pp %>%
    mutate(across(c(cum_Ahat_p, cum_Ahat_q, cum_Ahat, agg_impact), transform_bp_into_pp))

  tab_joined <- tab_log_pp %>%
    left_join(tab_pp, by = c("Year", "Scenario_q"), suffix = c("_log", "_pp"))

  columns_to_format <- c("cum_Ahat_q", "cum_Ahat_p", "cum_Ahat", "agg_impact")

  for (col in columns_to_format) {
    log_col <- paste0(col, "_log")
    pp_col <- paste0(col, "_pp")
    if (col == "agg_impact") {
      tab_joined[[col]] <- sprintf("%.3f (%.3f)", tab_joined[[log_col]], tab_joined[[pp_col]])
    } else {
      tab_joined[[col]] <- sprintf("%.2f (%.2f)", tab_joined[[log_col]], tab_joined[[pp_col]])
    }
  }

  tab_combined_logpp_pp <- tab_joined %>%
    select(Scenario_q, all_of(columns_to_format)) %>%
    mutate(Scenario_q = stringr::str_replace_all(Scenario_q, "_", " "))

  colnames(tab_combined_logpp_pp) <- c(
    "Scenario",
    "$\\zeta\\hat{X}_{it}$",
    "$(\\zeta-1)\\hat{p}_{it}$",
    "$\\hat{A}_{it}$",
    "$\\hat{A}_t=\\lambda_i\\hat{A}_{it}$"
  )

  tab_combined_logpp_pp
}

plot_energy_mix <- function(df_merged,
                            scenario_filter = "Balanced Net Zero Pathway",
                            start_year = 2021,
                            end_year = 2050,
                            max_q = NA,
                            is_useful = FALSE) {
  df_q <- df_merged %>%
    ungroup() %>%
    filter(variable == "q", Scenario_q == scenario_filter, Year >= start_year, Year <= end_year) %>%
    add_is_elec() %>%
    rename_energy()

  present_colours <- energy_colours[names(energy_colours) %in% unique(df_q %>% filter(value > 0) %>% pull(energy))]
  nb_elec <- n_distinct(df_q %>% filter(value > 0, is_elec == "elec") %>% select(energy))
  nb_notelec <- n_distinct(df_q %>% filter(value > 0, is_elec != "elec") %>% select(energy))

  unit <- unique(df_q$Unit)
  y_label <- if (is_useful) {
    paste0("Useful energy (", unit, ")")
  } else {
    paste0("Final energy (", unit, ")")
  }

  g <- df_q %>%
    filter(!is.na(value)) %>%
    ggplot() +
    geom_area_pattern(
      aes(x = Year, y = value, fill = energy, pattern = is_elec),
      pattern_density = 0.01
    ) +
    scale_pattern_manual(values = c("non_elec" = "stripe", "elec" = "none")) +
    mytheme +
    scale_fill_manual(values = present_colours) +
    theme(
      legend.box = "horizontal",
      legend.position = "bottom",
      panel.spacing.x = unit(0.8, "cm")
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          pattern_spacing = 0.01,
          pattern = c(rep("none", nb_elec), rep("stripe", nb_notelec))
        ),
        ncol = 4
      ),
      pattern = "none"
    ) +
    scale_x_continuous(breaks = c(2020, 2030, 2040, 2050)) +
    labs(fill = "", y = y_label, x = "")

  if (!is.na(max_q)) {
    g <- g + coord_cartesian(ylim = c(0, max_q))
  }

  g
}

plot_average_price_energy <- function(df,
                                      scenario_filter = "Balanced Net Zero Pathway",
                                      start_year = 2021,
                                      end_year = 2050,
                                      p_2022 = NA,
                                      p_2050 = NA,
                                      max_p = NA,
                                      min_p = NA,
                                      is_useful = FALSE) {
  g_p <- df %>%
    filter(Year >= start_year, Year <= end_year, Scenario_q == scenario_filter) %>%
    ggplot() +
    geom_line(aes(x = Year, y = p)) +
    mytheme +
    labs(y = "Average price (USD 2020/MWh)", x = "", fill = "", colour = "")

  if (!is.na(p_2022) & !is.na(p_2050)) {
    g_p <- g_p +
      geom_hline(yintercept = p_2022$p, colour = "black", linetype = 2) +
      geom_hline(yintercept = p_2050$p, colour = "black", linetype = 2)
  }

  if (!is.na(max_p) & !is.na(min_p)) {
    g_p <- g_p + ylim(min_p, max_p)
  }

  if (is_useful) {
    g_p <- g_p + labs(y = "Average price (USD 2020/useful MWh)")
  }

  g_p
}

plot_cumulative_ahat <- function(df,
                                 df_error = df_quantiles_error_extended,
                                 scenario_filter = "Balanced Net Zero Pathway",
                                 start_year = 2021,
                                 end_year = 2050) {
  df %>%
    filter(Scenario_q == scenario_filter, !is.na(Ahat), Year >= start_year, Year <= end_year) %>%
    mutate(cum_Ahat = cumsum(Ahat)) %>%
    left_join(df_error %>% mutate(Year = 2022 + horizon), by = "Year") %>%
    mutate(
      q10 = cum_Ahat + q10,
      q25 = cum_Ahat + q25,
      q75 = cum_Ahat + q75,
      q90 = cum_Ahat + q90,
      cum_Ahat = transform_bp_into_pp(cum_Ahat),
      q10 = transform_bp_into_pp(q10),
      q25 = transform_bp_into_pp(q25),
      q75 = transform_bp_into_pp(q75),
      q90 = transform_bp_into_pp(q90)
    ) %>%
    ggplot(aes(x = Year, y = cum_Ahat)) +
    geom_line() +
    geom_hline(yintercept = 0, colour = "black", linetype = 2) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = "[q10, q90]"), alpha = 0.3) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = "[q25, q75]"), alpha = 0.3) +
    scale_fill_manual(values = c("[q10, q90]" = "lightblue", "[q25, q75]" = "navy")) +
    mytheme +
    scale_y_continuous(labels = scales::number_format(scale = 100, suffix = "%")) +
    labs(y = "Cumulative Productivity Growth \n2022-2050", x = "", fill = "", colour = "")
}

create_combined_legend <- function(df_merged, scenario_filter, ncol_exo = 6) {
  df_q <- df_merged %>%
    filter(variable == "q", Scenario_q == scenario_filter) %>%
    add_is_elec() %>%
    rename_energy()

  legend_energy <- levels(droplevels(df_q$energy))
  present_colours <- energy_colours[legend_energy]

  energy_elec_map <- df_q %>%
    distinct(energy, is_elec) %>%
    { setNames(.$is_elec, .$energy) }

  energy_patterns <- ifelse(
    legend_energy %in% names(energy_elec_map[energy_elec_map == "elec"]),
    "none",
    "stripe"
  )

  fill_breaks <- c(legend_energy, "[q10, q90]", "[q25, q75]")
  pattern_override <- c(energy_patterns, "none", "none")

  plot_legend <- df_q %>%
    filter(!is.na(value)) %>%
    ggplot() +
    geom_ribbon(aes(x = Year, ymin = 0, ymax = 500, fill = "[q10, q90]"), alpha = 0.3) +
    geom_ribbon(aes(x = Year, ymin = 500, ymax = 1000, fill = "[q25, q75]"), alpha = 0.3) +
    geom_area_pattern(aes(x = Year, y = value, fill = energy, pattern = is_elec), pattern_density = 0.01) +
    scale_pattern_manual(values = c("non_elec" = "stripe", "elec" = "none")) +
    mytheme +
    scale_fill_manual(
      values = c(present_colours, "[q10, q90]" = "lightblue", "[q25, q75]" = "navy"),
      breaks = fill_breaks
    ) +
    theme(
      legend.box = "horizontal",
      legend.position = "bottom",
      panel.spacing.x = unit(0.8, "cm")
    ) +
    guides(
      fill = guide_legend(
        ncol = ncol_exo,
        override.aes = list(pattern_spacing = 0.01, pattern = pattern_override)
      ),
      pattern = "none"
    ) +
    scale_x_continuous(breaks = c(2020, 2030, 2040, 2050)) +
    labs(fill = "", y = "", x = "")

  get_legend(plot_legend)
}

plot_multiple_scenarios <- function(df_plot_energy,
                                    scenario_ref_p,
                                    name_file,
                                    subtitle_from_y_axis = FALSE,
                                    remove_y_axis_titles = FALSE,
                                    compact_height = FALSE) {
  df_aihat_all <- compute_Ahat(df_plot_energy)
  df_cumaihat_all <- compute_cumulative_Ahat(df_aihat_all) %>%
    filter(Year <= 2050) %>%
    left_join(df_quantiles_error_extended %>% mutate(Year = 2022 + horizon), by = "Year") %>%
    mutate(
      q10 = cum_Ahat + q10,
      q25 = cum_Ahat + q25,
      q75 = cum_Ahat + q75,
      q90 = cum_Ahat + q90,
      cum_Ahat = transform_bp_into_pp(cum_Ahat),
      q10 = transform_bp_into_pp(q10),
      q25 = transform_bp_into_pp(q25),
      q75 = transform_bp_into_pp(q75),
      q90 = transform_bp_into_pp(q90)
    ) %>%
    select(Year, Scenario_q, cum_Ahat, q10, q25, q75, q90) %>%
    gather(key = var, value = value, -c(1:2))

  p_min <- min(df_aihat_all$p)
  p_max <- max(df_aihat_all$p)
  cumAhat_min <- min(df_cumaihat_all$value)
  cumAhat_max <- max(df_cumaihat_all$value)

  p_2022_way <- df_aihat_all %>%
    filter(Scenario_q == scenario_ref_p, Year == 2022) %>%
    summarise(p = mean(p))

  p_2050_way <- df_aihat_all %>%
    filter(Scenario_q == scenario_ref_p, Year == 2050) %>%
    group_by(Scenario_q, Year) %>%
    summarise(p = p, .groups = "drop")

  for (x in unique(df_plot_energy$Scenario_q)) {
    data <- df_plot_energy %>% filter(Scenario_q == x)
    name_x <- unique(data$Scenario_q_name)
    unit_x <- unique(data$Unit)

    if (subtitle_from_y_axis) {
      subtitle_q <- paste0("Useful energy (", unit_x[1], ")")
      subtitle_p <- "Average price (USD 2020/useful MWh)"
      subtitle_ahat <- "Cumulative productivity growth (2022-2050)"
    } else {
      subtitle_q <- "Useful energy mix"
      subtitle_p <- "Average price USD/useful MWh"
      subtitle_ahat <- "Cumulative productivity growth"
    }

    g_q <- plot_energy_mix(data, is_useful = TRUE, scenario_filter = x) +
      labs(subtitle = subtitle_q)

    df_aihat <- compute_Ahat(data)
    df_cumaihat <- compute_cumulative_Ahat(df_aihat)

    g_ahat <- plot_cumulative_ahat(df_cumaihat, scenario_filter = x) +
      labs(subtitle = subtitle_ahat)

    g_p <- plot_average_price_energy(df_aihat, is_useful = TRUE, scenario_filter = x) +
      labs(subtitle = subtitle_p) +
      ylim(p_min, p_max) +
      geom_hline(yintercept = p_2022_way$p, colour = "black", linetype = 2) +
      geom_hline(yintercept = p_2050_way$p, colour = "black", linetype = 2)

    g_q <- g_q + theme(legend.position = "none")
    g_ahat <- g_ahat +
      theme(legend.position = "none") +
      scale_y_continuous(
        labels = scales::number_format(scale = 100, suffix = "%"),
        limits = c(cumAhat_min, cumAhat_max)
      )

    if (remove_y_axis_titles) {
      g_q <- g_q + labs(y = NULL) + theme(axis.title.y = element_blank())
      g_p <- g_p + labs(y = NULL) + theme(axis.title.y = element_blank())
      g_ahat <- g_ahat + labs(y = NULL) + theme(axis.title.y = element_blank())
    }

    if (subtitle_from_y_axis || compact_height) {
      compact_theme <- theme(
        plot.subtitle = element_text(size = 8.5, hjust = 0, margin = margin(b = 4)),
        plot.margin = margin(8, 4, 2, 2)
      )

      g_q <- g_q + compact_theme
      g_p <- g_p + compact_theme
      g_ahat <- g_ahat + compact_theme
    }

    legend_combined <- create_combined_legend(data, scenario_filter = x, ncol_exo = 8)

    g_x <- ggarrange(g_q, g_p, g_ahat, widths = c(1, 1, 1), nrow = 1)
    g_x <- annotate_figure(
      g_x,
      top = text_grob(name_x, color = "black", face = "bold", size = 12)
    )

    assign(paste("g", x, sep = "_"), g_x)
  }

  list_scenario <- unique(df_plot_energy$Scenario_q)
  plot_list <- mget(paste0("g_", list_scenario))

  g_useful_energy <- do.call(
    ggarrange,
    append(plot_list, list(ncol = 1, legend = "bottom", legend.grob = legend_combined))
  )

  h <- if (compact_height) {
    9.9 / 3 * n_distinct(df_plot_energy$Scenario_q) + 0.35
  } else {
    11.5 / 3 * n_distinct(df_plot_energy$Scenario_q) + 0.5
  }
  print(g_useful_energy + canvas(12, h, "in"))

  ggsave(
    filename = file.path(graph_path, name_file),
    plot = g_useful_energy,
    width = 12,
    height = h
  )
}
