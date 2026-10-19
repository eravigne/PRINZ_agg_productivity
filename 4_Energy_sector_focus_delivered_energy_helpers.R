# Helper functions for the delivered-useful-energy accounting notebook.

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
  df_x_final_useful_base <- read_excel(
    file.path(data_path, "Energy_modelling/table_S15_conversionfactor.xlsx"),
    sheet = "x_final_useful"
  )

  df_x_final_useful <<- bind_rows(
    df_x_final_useful_base,
    df_x_final_useful_base %>%
      filter(energy == "storage") %>%
      mutate(energy = "storage_ST"),
    df_x_final_useful_base %>%
      filter(energy == "storage") %>%
      mutate(energy = "storage_LT"),
    df_x_final_useful_base %>%
      filter(energy == "elec_hydrogen") %>%
      mutate(energy = "hydrogen_backup_service")
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
          "elec_biomass_CCS", "elec_coal", "elec_hydrogen", "hydrogen_backup_service", "elec_other_hydro",
          "Nuclear", "wind_offshore", "wind_onshore", "solar",
          "storage_wind", "storage_solar", "storage"
        )),
        labels = rev(c(
          "Gas", "Oil", "Solid", "Waste", "Hydrogen", "Bioenergy",
          "Gas electricity", "Gas+CCS electricity", "Gas+coal electricity",
          "Biomass electricity", "Biomass+CCS electricity", "Coal electricity",
          "Hydrogen electricity", "Hydrogen backup service", "Hydro-electricity", "Nuclear",
          "Offshore Wind electricity", "Onshore Wind electricity",
          "Solar PV electricity", "Wind via battery storage",
          "Solar PV via battery storage", "Storage"
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

  out <- df %>%
    gather(key = "energy", value = "p", -c("Year", "Scenario_p")) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(p = as.numeric(p))

  bind_rows(
    out,
    out %>%
      filter(energy == "elec_hydrogen") %>%
      mutate(energy = "hydrogen_backup_service")
  )
}

# Read the grid-specific battery point forecasts exported by the system-cost
# replication. The files contain levelised capital costs in bn USD/EJ-year.
# Multiplication by 3.6 converts bn USD/EJ to USD/MWh; division by 365 places
# the annual capacity service on the same daily-cycling throughput convention
# used for the storage quantities below.
read_grid_storage_prices <- function(forecast_path) {
  files <- tibble(
    Scenario_p = c("Fast_Transition", "Slow_Transition", "No_Transition"),
    filename = c(
      "grid_storage_prices_fast_transition_levelized.csv",
      "grid_storage_prices_slow_transition_levelized.csv",
      "grid_storage_prices_no_transition_levelized.csv"
    )
  )

  purrr::map2_dfr(files$filename, files$Scenario_p, function(filename, scenario_name) {
    readr::read_csv(file.path(forecast_path, filename), show_col_types = FALSE) %>%
      rename(technology = 1) %>%
      filter(technology %in% c("batteries_ST_electricity", "batteries_LT_electricity")) %>%
      select(-units) %>%
      pivot_longer(-technology, names_to = "Year", values_to = "levelised_bn_usd_per_ej_year") %>%
      transmute(
        Year = as.numeric(Year),
        energy = dplyr::recode(
          technology,
          "batteries_ST_electricity" = "storage_ST",
          "batteries_LT_electricity" = "storage_LT"
        ),
        Scenario_p = scenario_name,
        p = levelised_bn_usd_per_ej_year * 3.6 / 365
      )
  })
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
      all_elec_for_backup = rowSums(
        across(any_of(c(
          "elec_unabated_gas", "elec_hydrogen", "elec_gas_CCS",
          "elec_other_gasCHP", "elec_biomass", "elec_biomass_CCS",
          "elec_coal", "elec_other_hydro", "Nuclear", "wind_offshore",
          "wind_onshore", "solar"
        ))),
        na.rm = TRUE
      ),
      storage_fraction_ST = if_else(Scenario_q == "No_Transition", 0.0001, 0.20),
      storage_fraction_LT = if_else(Scenario_q == "No_Transition", 0.0001, 0.10),
      storage_ST = (solar + wind_offshore) * 1.5 * storage_fraction_ST / 365,
      storage_LT = (solar + wind_offshore) * 1.5 * storage_fraction_LT / 365,
      hydrogen_backup_service = final_elec_from_hydrogen_share * all_elec_for_backup
    ) %>%
    select(
      -final_elec_from_hydrogen_share, -all_elec_for_backup,
      -storage_fraction_ST, -storage_fraction_LT
    ) %>%
    gather(key = "energy", value = "q", -c("Scenario_q", "Year")) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(q = as.numeric(q))

  cap_storage_growth(df) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(q = as.numeric(q)) %>%
    spread(key = energy, value = q) %>%
    mutate(
      storage_ST = storage_ST * 365,
      storage_LT = storage_LT * 365
    ) %>%
    gather(key = "energy", value = "q", -c("Scenario_q", "Year"))
}

# CCC fuel-demand totals include the primary fuels used by the electricity
# sector. Electricity generation is already valued separately below, so those
# fuel inputs must be removed before the quantities are interpreted as final
# energy delivered to users. This restores the accounting convention used in
# the original replication: gas and hydrogen generation have an efficiency of
# 0.5, while biomass generation has an efficiency of 0.4.
remove_double_counting_primary_q_CCC <- function(dq_raw) {
  required_quantity_columns <- c(
    "gas", "bioenergy", "hydrogen",
    "elec_unabated_gas", "elec_gas_CCS", "elec_other_gasCHP",
    "elec_biomass", "elec_biomass_CCS", "elec_hydrogen"
  )
  missing_quantity_columns <- setdiff(required_quantity_columns, names(dq_raw))
  if (length(missing_quantity_columns) > 0) {
    stop(
      "CCC fuel netting requires the following missing columns: ",
      paste(missing_quantity_columns, collapse = ", ")
    )
  }

  conversion_factors <- stats::setNames(
    as.numeric(df_x_primary_final[[2]]),
    as.character(df_x_primary_final[[1]])
  )
  required_factors <- c("gas", "bioenergy", "hydrogen")
  if (any(!required_factors %in% names(conversion_factors)) ||
      any(!is.finite(conversion_factors[required_factors])) ||
      any(conversion_factors[required_factors] <= 0)) {
    stop("CCC fuel netting requires positive conversion factors for gas, bioenergy and hydrogen.")
  }

  out <- dq_raw %>%
    mutate(
      gas = gas -
        (elec_unabated_gas + elec_gas_CCS + elec_other_gasCHP) /
          conversion_factors[["gas"]],
      bioenergy = bioenergy -
        (elec_biomass + elec_biomass_CCS) /
          conversion_factors[["bioenergy"]],
      hydrogen = hydrogen -
        elec_hydrogen / conversion_factors[["hydrogen"]]
    )

  if (any(out$gas < -1e-10, na.rm = TRUE) ||
      any(out$bioenergy < -1e-10, na.rm = TRUE) ||
      any(out$hydrogen < -1e-10, na.rm = TRUE)) {
    stop("CCC fuel netting produced a negative final-fuel quantity.")
  }

  out
}

prepare_quantity_data_CCC <- function(dq_raw, add_storage_h2 = FALSE) {
  df <- dq_raw

  if (add_storage_h2) {
    df <- df %>%
      left_join(final_elec_from_hydrogen_share_Fast_Transition, by = c("Year" = "year")) %>%
      mutate(
        all_elec_for_backup = rowSums(
          across(any_of(c(
            "elec_unabated_gas", "elec_hydrogen", "elec_gas_CCS",
            "elec_other_gasCHP", "elec_biomass", "elec_biomass_CCS",
            "elec_coal", "elec_other_hydro", "Nuclear", "wind_offshore",
            "wind_onshore", "solar"
          ))),
          na.rm = TRUE
        ),
        storage_ST = (solar + wind_offshore + wind_onshore) * 1.5 * 0.20 / 365,
        storage_LT = (solar + wind_offshore + wind_onshore) * 1.5 * 0.10 / 365,
        hydrogen_backup_service = final_elec_from_hydrogen_share * all_elec_for_backup
      ) %>%
      select(-final_elec_from_hydrogen_share, -all_elec_for_backup)
  }

  df <- df %>%
    gather(key = "energy", value = "q", -c("Scenario_q", "Year", "Unit", "variable")) %>%
    filter(!energy %in% c("all_elec", "total")) %>%
    mutate(
      q = as.numeric(q),
      # CCC reports electricity sent to storage as a negative withdrawal.
      # Expenditure is positive, while delivered-output accounting below
      # excludes this intermediate flow from useful energy.
      q = if_else(energy == "storage", abs(q), q)
    )

  # CCC reports a single annual storage-flow series rather than separate
  # battery technologies. Under the constant-daily-use interpretation, retain
  # the reported total but allocate it using Way's capacity mix: 20 percentage
  # points short-duration and 10 percentage points long-duration. This gives a
  # 2/3 ST and 1/3 LT split and lets each component use its grid-specific price.
  if (!add_storage_h2) {
    storage_rows <- df %>% filter(energy == "storage")
    df <- bind_rows(
      df %>% filter(energy != "storage"),
      storage_rows %>% mutate(energy = "storage_ST", q = q * 2 / 3),
      storage_rows %>% mutate(energy = "storage_LT", q = q / 3)
    )
  }

  if (add_storage_h2) {
    df <- cap_storage_growth(df) %>%
      filter(!energy %in% c("all_elec", "total")) %>%
      mutate(q = as.numeric(q)) %>%
      spread(key = energy, value = q) %>%
      mutate(
        storage_ST = storage_ST * 365,
        storage_LT = storage_LT * 365
      ) %>%
      gather(key = "energy", value = "q", -c("Scenario_q", "Year", "Unit", "variable")) %>%
      mutate(q = as.numeric(q))
  }

  df
}

# Global grid-battery stocks in 2020 from Way et al.'s tech_params.csv, in EJ
# of storage capacity: 50 GWh short-duration and 0.3 GWh multi-day.
elec_storage_batt_ST_electricity_0 <- 0.00018
elec_storage_batt_LT_electricity_0 <- 0.00000108

# Way et al. let installed capacity grow at most 60% (short-duration) and 80%
# (multi-day) per year from the 2020 stock, i.e. cap_t = z_0 (1 + g)^(t - 2020).
# The stocks are global and in EJ; `scale` converts them to another pathway's
# size and units (e.g. UK TWh), so the cap is applied consistently.
cap_storage_growth <- function(df_q, scale = 1) {
  years_range <- seq(2018, 2070)
  q_ST <- tibble(
    Year = years_range,
    q_ST = scale * elec_storage_batt_ST_electricity_0 * (1 + 0.6)^(years_range - 2020)
  )
  q_LT <- tibble(
    Year = years_range,
    q_LT = scale * elec_storage_batt_LT_electricity_0 * (1 + 0.8)^(years_range - 2020)
  )

  cols_fin <- colnames(df_q %>% select(-energy, -q))

  q_storage <- df_q %>%
    ungroup() %>%
    filter(energy %in% c("storage_ST", "storage_LT")) %>%
    spread(key = energy, value = q) %>%
    left_join(q_LT, by = "Year") %>%
    left_join(q_ST, by = "Year") %>%
    rowwise() %>%
    mutate(
      storage_ST = min(storage_ST, q_ST),
      storage_LT = min(storage_LT, q_LT)
    ) %>%
    ungroup() %>%
    select(all_of(cols_fin), storage_ST, storage_LT)

  df_q %>%
    spread(key = energy, value = q) %>%
    select(-storage_ST, -storage_LT) %>%
    left_join(q_storage, by = cols_fin) %>%
    gather(key = energy, value = q, -all_of(cols_fin))
}

is_storage_energy <- function(energy) {
  as.character(energy) %in% c("storage", "storage_ST", "storage_LT")
}

is_delivery_service <- function(energy) {
  is_storage_energy(energy) | as.character(energy) == "hydrogen_backup_service"
}

is_way_based_scenario <- function(scenario) {
  as.character(scenario) %in% c(
    "Fast_Transition", "Slow_Transition", "No_Transition",
    "Fast_Transition_UK", "UK fast transition"
  )
}

# Construct the two quantities needed by the accounting framework.
# q_cost is the engineering quantity used to value each cost component.
# q_output is useful energy delivered to final users. Storage is an
# intermediate service, and Way's hydrogen-backup electricity is already
# contained in the imported electricity-generation rows.
build_energy_accounts <- function(df_merged) {
  df_merged %>%
    distinct() %>%
    ungroup() %>%
    spread(key = "variable", value = "value") %>%
    mutate(
      q_cost = if_else(is_storage_energy(energy), abs(q), q),
      q_output = case_when(
        is_delivery_service(energy) ~ 0,
        TRUE ~ q
      ),
      cost = p * q_cost
    )
}

# Add the Way storage-capacity and hydrogen-backup cost quantities to an
# arbitrary delivered-energy pathway. The input must be long data with columns
# Scenario_q, energy, Year and q and must not already contain these services.
# `share_scenario` selects the Way hydrogen-backup share; `cap_scale` rescales
# the global battery growth cap to the pathway (see cap_storage_growth).
add_way_delivery_services <- function(df_q, share_scenario = "Fast_Transition",
                                      cap_scale = 1) {
  electricity_technologies <- c(
    "elec_unabated_gas", "elec_hydrogen", "elec_gas_CCS",
    "elec_other_gasCHP", "elec_biomass", "elec_biomass_CCS",
    "elec_coal", "elec_other_hydro", "Nuclear", "wind_offshore",
    "wind_onshore", "solar"
  )

  wide <- df_q %>%
    filter(!is_delivery_service(energy)) %>%
    select(Scenario_q, Year, energy, q) %>%
    spread(key = energy, value = q, fill = 0) %>%
    mutate(
      renewable_generation = rowSums(
        across(any_of(c("solar", "wind_offshore", "wind_onshore"))),
        na.rm = TRUE
      ) * 1.5,
      all_elec = rowSums(across(any_of(electricity_technologies)), na.rm = TRUE),
      storage_ST = renewable_generation * 0.20 / 365,
      storage_LT = renewable_generation * 0.10 / 365
    ) %>%
    left_join(
      final_elec_from_hydrogen_share %>%
        filter(Scenario_q == share_scenario) %>%
        select(Year = year, final_elec_from_hydrogen_share),
      by = "Year"
    ) %>%
    mutate(
      hydrogen_backup_service = final_elec_from_hydrogen_share * all_elec
    )

  service_rows <- wide %>%
    select(Scenario_q, Year, storage_ST, storage_LT, hydrogen_backup_service) %>%
    gather(key = energy, value = q, -Scenario_q, -Year) %>%
    cap_storage_growth(scale = cap_scale) %>%
    spread(key = energy, value = q) %>%
    mutate(
      storage_ST = storage_ST * 365,
      storage_LT = storage_LT * 365
    ) %>%
    gather(key = energy, value = q, -Scenario_q, -Year)

  bind_rows(df_q, service_rows)
}

extract_output_quantities <- function(df_merged) {
  build_energy_accounts(df_merged) %>%
    transmute(
      across(-c(p, q, q_cost, q_output, cost)),
      variable = "q",
      value = q_output
    )
}

# Split the displayed wind and solar areas without changing total delivered
# useful energy. For the two transition pathways, the 20% short-duration and
# 10% multi-day capacity assumptions sum to 30% of one day's gross wind and
# solar generation. The two orange categories are graphical capacity markers,
# not annual storage throughput.
prepare_output_mix <- function(df_merged) {
  df_q <- extract_output_quantities(df_merged) %>%
    filter(value != 0) %>%
    add_is_elec()

  split_rows <- df_q %>%
    filter(
      is_way_based_scenario(Scenario_q),
      Scenario_q != "No_Transition",
      energy %in% c("solar", "wind_offshore", "wind_onshore")
    )

  unsplit_rows <- df_q %>% anti_join(
    split_rows %>% select(Scenario_q, Year, energy) %>% distinct(),
    by = c("Scenario_q", "Year", "energy")
  )

  direct_rows <- split_rows %>% mutate(value = 0.70 * value)
  storage_rows <- split_rows %>%
    mutate(
      value = 0.30 * value,
      energy = if_else(energy == "solar", "storage_solar", "storage_wind"),
      is_elec = "elec"
    ) %>%
    group_by(across(-value)) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  bind_rows(unsplit_rows, direct_rows, storage_rows)
}

# Fail fast if the cost/output boundary or the graphical renewable split is
# violated. This is deliberately separate from the calculations so it can be
# reused for every reported scenario collection.
validate_delivered_energy_account <- function(df_merged, label = "energy account") {
  account <- build_energy_accounts(df_merged)
  services <- is_delivery_service(account$energy)

  if (any(abs(account$q_output[services]) > 1e-10, na.rm = TRUE)) {
    stop(label, ": a delivery service has been counted as final output.")
  }
  if (any(account$q_cost[services] < -1e-10, na.rm = TRUE)) {
    stop(label, ": a delivery-service cost quantity is negative.")
  }

  account_totals <- account %>%
    group_by(Scenario_q, Year) %>%
    summarise(account_output = sum(q_output, na.rm = TRUE), .groups = "drop")

  display_totals <- prepare_output_mix(df_merged) %>%
    group_by(Scenario_q, Year) %>%
    summarise(display_output = sum(value, na.rm = TRUE), .groups = "drop")

  comparison <- account_totals %>%
    full_join(display_totals, by = c("Scenario_q", "Year")) %>%
    mutate(
      scale = pmax(abs(account_output), abs(display_output), 1),
      relative_gap = abs(account_output - display_output) / scale
    )

  if (any(is.na(comparison$relative_gap)) ||
      any(comparison$relative_gap > 1e-10)) {
    stop(label, ": the graphical split does not preserve delivered output.")
  }

  invisible(TRUE)
}

add_is_elec <- function(df) {
  df %>%
    mutate(
      is_elec = ifelse(
        energy %in% c(
          "elec_unabated_gas", "elec_hydrogen", "hydrogen_backup_service", "elec_gas_CCS", "elec_biomass_CCS",
          "Nuclear", "elec_biomass", "elec_other_gasCHP", "wind_offshore",
          "wind_onshore", "solar", "elec_other_hydro", "elec_coal", "storage",
          "storage_wind", "storage_solar",
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
  build_energy_accounts(df_merged) %>%
    group_by(Scenario_q, Year) %>%
    filter(!is.na(cost)) %>%
    summarise(sales = sum(cost * 1e6 / exch_rate), .groups = "drop")
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
  build_energy_accounts(df_merged) %>%
    filter(Year >= start_year) %>%
    group_by(Year, Scenario_q) %>%
    summarise(
      cost = sum(cost, na.rm = TRUE),
      q = sum(q_output, na.rm = TRUE),
      p = cost / q,
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
  build_energy_accounts(df_merged) %>%
    filter(Year >= start_year) %>%
    group_by(Year, Scenario_q) %>%
    summarise(
      cost = sum(cost, na.rm = TRUE),
      q = sum(q_output, na.rm = TRUE),
      p = cost / q,
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
  df_q <- prepare_output_mix(df_merged) %>%
    ungroup() %>%
    filter(Scenario_q == scenario_filter, Year >= start_year, Year <= end_year) %>%
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

create_combined_legend <- function(df_merged,
                                   scenario_filter = NULL,
                                   ncol_exo = 6,
                                   include_diagnostic = FALSE,
                                   diagnostic_label = "No storage costs",
                                   diagnostic_colour = "#D55E00") {
  df_q <- prepare_output_mix(df_merged)
  if (!is.null(scenario_filter)) {
    df_q <- df_q %>% filter(Scenario_q == scenario_filter)
  }
  df_q <- df_q %>% rename_energy()

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

  if (include_diagnostic) {
    diagnostic_legend_data <- tibble(
      Year = c(2022, 2050),
      value = c(0, 0),
      diagnostic = diagnostic_label
    )

    plot_legend <- plot_legend +
      geom_line(
        data = diagnostic_legend_data,
        aes(x = Year, y = value, colour = diagnostic, linetype = diagnostic),
        inherit.aes = FALSE,
        linewidth = 0.8
      ) +
      scale_colour_manual(values = setNames(diagnostic_colour, diagnostic_label), name = NULL) +
      scale_linetype_manual(values = setNames("22", diagnostic_label), name = NULL) +
      guides(
        fill = guide_legend(
          ncol = ncol_exo,
          override.aes = list(pattern_spacing = 0.01, pattern = pattern_override),
          order = 1
        ),
        pattern = "none",
        # The linetype guide is hidden, so the dashed key must be drawn
        # explicitly to match the dashed line in the panels.
        colour = guide_legend(
          order = 2,
          override.aes = list(linetype = "22", linewidth = 0.8),
          theme = theme(legend.key.width = unit(1.2, "cm"))
        ),
        linetype = "none"
      )
  }

  get_legend(plot_legend)
}

plot_multiple_scenarios <- function(df_plot_energy,
                                    scenario_ref_p,
                                    name_file,
                                    subtitle_from_y_axis = FALSE,
                                    remove_y_axis_titles = FALSE,
                                    compact_height = FALSE,
                                    df_productivity_diagnostic = NULL,
                                    diagnostic_label = "No storage costs",
                                    diagnostic_colour = "#D55E00") {
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

  df_diagnostic_aihat <- NULL
  df_diagnostic_cumaihat <- NULL
  if (!is.null(df_productivity_diagnostic)) {
    df_diagnostic_aihat <- compute_Ahat(df_productivity_diagnostic)
    df_diagnostic_cumaihat <- compute_cumulative_Ahat(df_diagnostic_aihat) %>%
      filter(Year <= 2050) %>%
      mutate(cum_Ahat = transform_bp_into_pp(cum_Ahat)) %>%
      select(Year, Scenario_q, cum_Ahat)
  }

  p_min <- min(df_aihat_all$p)
  p_max <- max(df_aihat_all$p)
  cumAhat_min <- min(df_cumaihat_all$value)
  cumAhat_max <- max(df_cumaihat_all$value)

  if (!is.null(df_diagnostic_cumaihat)) {
    cumAhat_min <- min(cumAhat_min, df_diagnostic_cumaihat$cum_Ahat, na.rm = TRUE)
    cumAhat_max <- max(cumAhat_max, df_diagnostic_cumaihat$cum_Ahat, na.rm = TRUE)
    p_min <- min(p_min, df_diagnostic_aihat$p, na.rm = TRUE)
    p_max <- max(p_max, df_diagnostic_aihat$p, na.rm = TRUE)
  }

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

    if (!is.null(df_diagnostic_cumaihat)) {
      diagnostic_x <- df_diagnostic_cumaihat %>% filter(Scenario_q == x)

      if (nrow(diagnostic_x) > 0) {
        g_ahat <- g_ahat +
          geom_line(
            data = diagnostic_x,
            aes(x = Year, y = cum_Ahat),
            inherit.aes = FALSE,
            colour = diagnostic_colour,
            linetype = "22",
            linewidth = 0.8
          )
      }
    }

    g_p <- plot_average_price_energy(df_aihat, is_useful = TRUE, scenario_filter = x) +
      labs(subtitle = subtitle_p) +
      ylim(p_min, p_max) +
      geom_hline(yintercept = p_2022_way$p, colour = "black", linetype = 2) +
      geom_hline(yintercept = p_2050_way$p, colour = "black", linetype = 2)

    if (!is.null(df_diagnostic_aihat)) {
      diagnostic_p_x <- df_diagnostic_aihat %>%
        filter(Scenario_q == x, Year >= 2021, Year <= 2050)

      if (nrow(diagnostic_p_x) > 0) {
        g_p <- g_p +
          geom_line(
            data = diagnostic_p_x,
            aes(x = Year, y = p),
            inherit.aes = FALSE,
            colour = diagnostic_colour,
            linetype = "22",
            linewidth = 0.8
          )
      }
    }

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

    g_x <- ggarrange(g_q, g_p, g_ahat, widths = c(1, 1, 1), nrow = 1)
    g_x <- annotate_figure(
      g_x,
      top = text_grob(name_x, color = "black", face = "bold", size = 12)
    )

    assign(paste("g", x, sep = "_"), g_x)
  }

  list_scenario <- unique(df_plot_energy$Scenario_q)
  plot_list <- mget(paste0("g_", list_scenario))
  legend_combined <- create_combined_legend(
    df_plot_energy,
    ncol_exo = 7,
    include_diagnostic = !is.null(df_productivity_diagnostic),
    diagnostic_label = diagnostic_label,
    diagnostic_colour = diagnostic_colour
  )

  g_useful_energy <- do.call(
    ggarrange,
    append(plot_list, list(ncol = 1, legend = "bottom", legend.grob = legend_combined))
  )

  h <- if (compact_height) {
    9.9 / 3 * n_distinct(df_plot_energy$Scenario_q) + 0.35
  } else {
    11.5 / 3 * n_distinct(df_plot_energy$Scenario_q) + 0.5
  }
  if (interactive()) {
    print(g_useful_energy + canvas(12, h, "in"))
  }

  ggsave(
    filename = file.path(graph_path, name_file),
    plot = g_useful_energy,
    width = 12,
    height = h
  )
  ggsave(
    filename = file.path(graph_path, sub("\\.jpe?g$", ".pdf", name_file)),
    plot = g_useful_energy,
    width = 12,
    height = h,
    device = cairo_pdf
  )
}
