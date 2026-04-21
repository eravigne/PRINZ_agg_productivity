# Compute CCC forecast error bands using the hindcasting codebase
# ------------------------------------------------------------------
# This script maps the hindcasting uncertainty objects onto the CCC projections.
# It keeps the sign convention: error = predicted - actual.
#
# Main design choices:
# 1. Industry-level uncertainty is calibrated from KLEMS zeta-hindcasting residuals:
#      nu_{i,h} = predAihat - Aihat
# 2. Aggregation uncertainty is calibrated from KLEMS first-order Domar residuals:
#      epsilon_h = pred_Ahat_1st - Ahat
# 3. For CCC, sector contributions are already additive in the source scripts, so the
#    sector-to-total accounting is exact. If you want the conservative Section-3 error,
#    keep include_domar_error = TRUE. If you want the literal CCC accounting error only,
#    set include_domar_error = FALSE.
# 4. Horizons beyond the observed hindcasting support are extrapolated by the same
#    linear-trend method used in the hindcasting notebooks, and the last observed draw
#    distribution is affine-rescaled to the extrapolated median and IQR.

library(here)
source(here("0_set_up.R"))
source(here("0.1_functions_forecast.R"))

load(file=file.path(dataset_path,"cumsum_TFP_annual_CB7.RData"),verbose=T) #cumsum_TFP_annual_CB7
load(file=file.path(dataset_path,"cumsum_TFP_annual_CB6.RData"),verbose=T) #cumsum_TFP_annual_CB6
load(file=file.path(dataset_path,"cumsum_TFP_annual_sector_CB7.RData"),verbose=T) #cumsum_TFP_annual_sector_CB7
load(file=file.path(dataset_path,"cumsum_TFP_annual_sector_CB6.RData"),verbose=T) #cumsum_TFP_annual_sector_CB6


cb7_to_tab_ccc <- c(
  "Industry"                 = "M&C",
  "Land use"                 = "LULUCF sinks",
  "Residential buildings"    = "Res buildings",
  "Non-residential buildings"= "Non res buildings",
  "Engineered removals"      = "Removals"
)

ccc_cb7 <- list(
  sector = cumsum_TFP_annual_sector_CB7 %>%
    dplyr::mutate(sector = dplyr::recode(sector, !!!cb7_to_tab_ccc)) %>%
    ungroup() %>%
    tidyr::complete(
      tidyr::nesting(scenario, country, sector, variable_unit, variable),
      year = 2024,
      fill = list(value = 0, delta_annual_resource = 0, cumAhat_raw = 0, cumAhat = 0)
    ),
  agg = cumsum_TFP_annual_CB7 %>%
    ungroup() %>%
    tidyr::complete(
      tidyr::nesting(scenario),
      year = 2024,
      fill = list(cumAhat_agg = 0, cumAhat_agg_raw = 0)
    )
)
ccc_cb6<-list(sector = cumsum_TFP_annual_sector_CB6%>%rename(year=Year,scenario=Scenario), agg = cumsum_TFP_annual_CB6%>%rename(year=Year,scenario=Scenario))



# ---------- user choices -------------------------------------------------
# budget <- "CB7"               # "CB6" or "CB7"
# forecast_start_year <- 2021   # gives horizon 1 in 2021 and horizon 30 in 2050
# include_domar_error <- TRUE   # set FALSE if you do not want the extra conservative Domar term
# n_draws <- 10000
set.seed(123)

output_path <- here("Datasets", paste0("CCC_", budget, "_aggregate_error_bands.RData"))
output_csv  <- here("Datasets", paste0("CCC_", budget, "_aggregate_error_bands.csv"))

# Zeta
filePath <- file.path(dataset_path,"df_quantiles_KLEMS_error_extended.RData")
load(file=filePath,verbose=T) #df_quantiles_error_extended
# Domar
load(file=file.path(dataset_path,"df_quantiles_hulten_extented.RData"),verbose=T) #df_quantiles_hulten_extented


# Domar weight per sector -------------------------------------------------

supply_use_Products<-read_excel(file.path(data_path,"ONS_data/supplyanduseofproductsandindustrygva.xlsx"),sheet="Table 2",skip=2,col_names = T)
tab_ccc<-read_xlsx(file.path(dataset_path,"tab_sic_ccc_sectors.xlsx"))

tab_ccc_detailed <- tab_ccc %>% filter(!is.na(SIC), nchar(SIC) > 1)   # D351, H51, C29 ...
tab_ccc_letter   <- tab_ccc %>% filter(!is.na(SIC), nchar(SIC) == 1)  # A, B, C, E, F, H

supply_use_Products_SIC_ccc <-
  supply_use_Products %>%
  filter(Price == "CP") %>%
  gather(key = Year, value = value, -c(1:4)) %>%
  rename("SIC02_code" = "Industry SIC Code") %>%
  separate(col = SIC02_code, into = c("code_SIC", "code_SIC_bis"), sep = "&") %>%
  select(-code_SIC_bis) %>%
  mutate(
    code_SIC = trimws(code_SIC),
    SIC      = str_sub(code_SIC, 1, 1),
    SIC      = ifelse(SIC == "T", "TOTAL", SIC),
    value    = as.numeric(as.character(value)),
    Year     = as.numeric(Year)
  ) %>%
  # Detailed match first (D351 → Electricity, H51 → Aviation/Shipping, C29 → Surface transport)
  left_join(tab_ccc_detailed, by = c("code_SIC" = "SIC")) %>%
  # Letter fallback for rows that didn't match a detailed code
  left_join(tab_ccc_letter %>% rename(sector_letter = sector), by = c("SIC" = "SIC")) %>%
  mutate(sector = coalesce(sector, sector_letter)) %>%
  select(-sector_letter) %>%
  mutate(sector = ifelse(is.na(sector), "Non_ccc", sector))


agg_gva_p1p2_sec_cc<-supply_use_Products_SIC_ccc%>%
  group_by(Year,sector,Component)%>%
  summarise(value=sum(value,na.rm=T))%>%
  group_by(Year,Component)%>%
  mutate(total=sum(value,na.rm=T))%>%
  mutate(share=value/total)%>%
  ungroup()%>%
  filter(Year==max(Year))

agg_gva_p1p2_sec_cc


tot_Domar<-
  supply_use_Products %>%
  filter(Price=="CP")%>%
  gather(key=Year,value=value,-c(1:4))%>%
  # Convert CPA to SIC
  rename("SIC02_code"="Industry SIC Code")%>%
  separate(col=SIC02_code,into=c("code_SIC","code_SIC_bis"),sep="&")%>%
  select(-code_SIC_bis)%>%
  mutate(SIC=str_sub(code_SIC,1,1))%>%
  mutate(SIC=ifelse(SIC=="T","TOTAL",SIC))%>%
  mutate(value=as.numeric(as.character(value)),
         Year=as.numeric(Year))%>%
  filter(Year==max(Year))%>%
  group_by(Year,Component)%>%
  summarise(value=sum(value,na.rm=T))%>%
  spread(key=Component,value=value)%>%
  mutate(ratio=P1/GVA)%>%
  ungroup()%>%
  pull(ratio)


# Domar weight for CCC sectors vs non CCC
domar_weight_ccc_sectors<-agg_gva_p1p2_sec_cc %>%
  summarise(domar_weight = value[Component == "P1"] / total[Component == "GVA"][1],
            .by = c(Year, sector))%>%
  mutate(domar_weight=ifelse(str_detect(str_to_lower(sector),"res"),domar_weight/2,domar_weight))%>%
  mutate(ratio_domar=domar_weight/tot_Domar)


# Domar weight of the CCC sectors: 0.3
dw_ccc<-domar_weight_ccc_sectors%>%filter(!sector=="Non_ccc")%>%summarise(sum(domar_weight))
dw_ccc
# Ratio of the agg CCC domar and tot domar: 0.16
dw_ccc/tot_Domar



# Other sectors
supply_use_Products%>%
  filter(str_detect(str_to_lower(Industry),"cement"))%>%
  filter(Price=="CP",Component=="P1")%>%
  gather(key=Year,value=value,-c(1:4))%>%
  mutate(value=as.numeric(as.character(value)),
         Year=as.numeric(Year))%>%
  filter(Year==max(Year))
# %>%
  # pull(Industry)

# 8034/3807150
# 0.002
# tot GVA: 3807150 

# ---------- helpers ------------------------------------------------------

transform_bp_into_pp <- function(x) exp(x) - 1   # log-change → proportional pp



get_error_draws_for_horizon <- function(qtab, h, n) {
  target <- qtab %>% dplyr::filter(horizon == h)
  if (nrow(target) == 0) stop("No quantiles available for horizon ", h)

  probs  <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  qvals  <- as.numeric(target[, c("q10", "q25", "q50", "q75", "q90")])

  # Linearly extrapolate tails so draws can fall outside [q10, q90]
  lo <- qvals[1] - (qvals[2] - qvals[1])
  hi <- qvals[5] + (qvals[5] - qvals[4])

  inv_cdf <- stats::approxfun(c(0, probs, 1), c(lo, qvals, hi),
                               method = "linear", rule = 2)
  inv_cdf(stats::runif(n))
}


simulate_ccc_error <- function(ccc_obj,
                               zeta_err,
                               domar_err = NULL,
                               sector_scale_df,
                               scale_col = "ratio",
                               normalize_scale = FALSE,
                               forecast_start_year = 2021,
                               level = c("agg", "sector"),
                               include_domar_error = TRUE,
                               agg_domar_scale = 0,
                               n_draws = 10000) {
  
  level <- match.arg(level)
  
  # Basic checks
  if (!all(c("sector") %in% names(sector_scale_df))) {
    stop("sector_scale_df must contain at least a 'sector' column.")
  }
  if (!(scale_col %in% names(sector_scale_df))) {
    stop("scale_col not found in sector_scale_df.")
  }
  
  # Harmonise year column if present
  if ("Year" %in% names(sector_scale_df) && !("year" %in% names(sector_scale_df))) {
    sector_scale_df <- sector_scale_df %>%
      dplyr::rename(year = Year)
  }
  
  
  scale_tbl<-sector_scale_df%>%
    dplyr::mutate(sector_scale = .data[[scale_col]]) %>%
        dplyr::select(sector, sector_scale)
  
  sector_h <- ccc_obj$sector %>%
    dplyr::mutate(horizon = year - forecast_start_year + 1) %>%
    dplyr::filter(horizon >= 1)
  
  # Join sector scaling

    sector_h <- sector_h %>%
      dplyr::left_join(scale_tbl, by = c("sector"))
  
  # Missing scales become zero
  sector_h <- sector_h %>%
    dplyr::mutate(sector_scale = dplyr::coalesce(sector_scale, 0))
  
  agg_h <- ccc_obj$agg %>%
    dplyr::mutate(horizon = year - forecast_start_year + 1) %>%
    dplyr::filter(horizon >= 1) %>%
    dplyr::select(
      scenario,
      year,
      horizon,
      base_cumAhat_raw = cumAhat_agg_raw,
      base_cumAhat = cumAhat_agg
    )
  
  keys <- sector_h %>%
    dplyr::distinct(scenario, year, horizon) %>%
    dplyr::arrange(scenario, year, horizon)
  
  if (level == "sector") {
    
    sim_sector_draws <- purrr::pmap_dfr(
      list(keys$scenario, keys$year, keys$horizon),
      function(scn, yr, h) {
        
        df_sy <- sector_h %>%
          dplyr::filter(scenario == scn, year == yr, horizon == h) %>%
          dplyr::arrange(sector)
        
        n_sec <- nrow(df_sy)
        
        zeta_mat <- matrix(
          get_error_draws_for_horizon(zeta_err, h, n_sec * n_draws),
          nrow = n_draws,
          ncol = n_sec
        )
        
        # sector-specific zeta component only, scaled by sector Domar ratio/weight
        zeta_component_mat <- sweep(zeta_mat, 2, df_sy$sector_scale, `*`)
        
        sim_sector_raw_mat <- sweep(
          zeta_component_mat,
          2,
          df_sy$cumAhat_raw,
          `+`
        )
        
        tibble::tibble(
          scenario = scn,
          year = yr,
          horizon = h,
          draw = rep(seq_len(n_draws), times = n_sec),
          sector = rep(df_sy$sector, each = n_draws),
          sector_scale = rep(df_sy$sector_scale, each = n_draws),
          base_cumAhat_raw = rep(df_sy$cumAhat_raw, each = n_draws),
          sim_cumAhat_raw = as.vector(sim_sector_raw_mat)
        )
      }
    )
    
    out <- sim_sector_draws %>%
      dplyr::group_by(scenario, year, horizon, sector, sector_scale) %>%
      dplyr::summarise(
        base_cumAhat_raw = dplyr::first(base_cumAhat_raw),
        q10_raw = as.numeric(stats::quantile(sim_cumAhat_raw, 0.10)),
        q25_raw = as.numeric(stats::quantile(sim_cumAhat_raw, 0.25)),
        q50_raw = as.numeric(stats::quantile(sim_cumAhat_raw, 0.50)),
        q75_raw = as.numeric(stats::quantile(sim_cumAhat_raw, 0.75)),
        q90_raw = as.numeric(stats::quantile(sim_cumAhat_raw, 0.90)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        level = "sector",
        base_cumAhat = transform_bp_into_pp(base_cumAhat_raw),
        q10 = transform_bp_into_pp(q10_raw),
        q25 = transform_bp_into_pp(q25_raw),
        q50 = transform_bp_into_pp(q50_raw),
        q75 = transform_bp_into_pp(q75_raw),
        q90 = transform_bp_into_pp(q90_raw)
      )
    
    return(out)
  }
  
  # Aggregate output
  sim_agg_draws <- purrr::pmap_dfr(
    list(keys$scenario, keys$year, keys$horizon),
    function(scn, yr, h) {
      
      df_sy <- sector_h %>%
        dplyr::filter(scenario == scn, year == yr, horizon == h) %>%
        dplyr::arrange(sector)
      
      n_sec <- nrow(df_sy)
      
      zeta_mat <- matrix(
        get_error_draws_for_horizon(zeta_err, h, n_sec * n_draws),
        nrow = n_draws,
        ncol = n_sec
      )
      
      zeta_component_mat <- sweep(zeta_mat, 2, df_sy$sector_scale, `*`)
      zeta_component <- rowSums(zeta_component_mat, na.rm = TRUE)
      
      domar_component <- if (include_domar_error && !is.null(domar_err) && agg_domar_scale != 0) {
        agg_domar_scale * get_error_draws_for_horizon(domar_err, h, n_draws)
      } else {
        rep(0, n_draws)
      }
      
      base_raw <- agg_h %>%
        dplyr::filter(scenario == scn, year == yr, horizon == h) %>%
        dplyr::pull(base_cumAhat_raw)
      
      tibble::tibble(
        scenario = scn,
        year = yr,
        horizon = h,
        draw = seq_len(n_draws),
        base_cumAhat_raw = base_raw,
        sim_agg_raw = base_raw + zeta_component + domar_component
      )
    }
  )
  
  out <- sim_agg_draws %>%
    dplyr::group_by(scenario, year, horizon) %>%
    dplyr::summarise(
      base_cumAhat_raw = dplyr::first(base_cumAhat_raw),
      mean_error_raw = mean(sim_agg_raw - base_cumAhat_raw),
      q10_raw = as.numeric(stats::quantile(sim_agg_raw, 0.10)),
      q25_raw = as.numeric(stats::quantile(sim_agg_raw, 0.25)),
      q50_raw = as.numeric(stats::quantile(sim_agg_raw, 0.50)),
      q75_raw = as.numeric(stats::quantile(sim_agg_raw, 0.75)),
      q90_raw = as.numeric(stats::quantile(sim_agg_raw, 0.90)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      agg_h,
      by = c("scenario", "year", "horizon", "base_cumAhat_raw")
    ) %>%
    dplyr::mutate(
      level = "agg",
      q10_error_raw = q10_raw - base_cumAhat_raw,
      q25_error_raw = q25_raw - base_cumAhat_raw,
      q50_error_raw = q50_raw - base_cumAhat_raw,
      q75_error_raw = q75_raw - base_cumAhat_raw,
      q90_error_raw = q90_raw - base_cumAhat_raw,
      mean_error = transform_bp_into_pp(mean_error_raw),
      q10_error = transform_bp_into_pp(q10_error_raw),
      q25_error = transform_bp_into_pp(q25_error_raw),
      q50_error = transform_bp_into_pp(q50_error_raw),
      q75_error = transform_bp_into_pp(q75_error_raw),
      q90_error = transform_bp_into_pp(q90_error_raw),
      q10_cumAhat = transform_bp_into_pp(q10_raw),
      q25_cumAhat = transform_bp_into_pp(q25_raw),
      q50_cumAhat = transform_bp_into_pp(q50_raw),
      q75_cumAhat = transform_bp_into_pp(q75_raw),
      q90_cumAhat = transform_bp_into_pp(q90_raw)
    )
  
  out
}

# ---------- run ----------------------------------------------------------
# In the forecasting scripts, CCC uses the project-level zeta object.



ccc_error_bands_cb7 <- simulate_ccc_error(
  ccc_obj = ccc_cb7,
  zeta_err = df_quantiles_error_extended,
  domar_err = df_quantiles_hulten_extented,
  forecast_start_year = 2025,
  agg_domar_scale = 0.16,
  sector_scale_df = domar_weight_ccc_sectors,
  scale_col="ratio_domar",
  normalize_scale=FALSE,
  include_domar_error=T,
  level= "agg",
  n_draws = 10000
)


ccc_error_bands_cb7%>%
  select(horizon,contains("error_raw"),base_cumAhat_raw)%>%
  gather(key=var,value=value,-1)%>%
  separate(var,sep="_error_raw",into=c("var"))%>%
  ggplot(aes(x=horizon,y=value,colour=var))+
  geom_line()+
  theme_minimal()

ccc_error_bands_cb7%>%
  select(horizon,contains("_cumAhat"),base_cumAhat_raw)%>%
  gather(key=var,value=value,-1)%>%
  # separate(var,sep="_cumAhat",into=c("var"))%>%
  ggplot(aes(x=horizon,y=value,colour=var))+
  geom_line()+
  theme_minimal()


save(ccc_error_bands_cb7, file = here("Datasets", "CCC_CB7_aggregate_error_bands.RData"))

ccc_error_bands_cb7_sector <- simulate_ccc_error(
  ccc_obj = ccc_cb7,
  zeta_err = df_quantiles_error_extended,
  domar_err = df_quantiles_hulten_extented,
  forecast_start_year = 2025,
  agg_domar_scale = 0,
  sector_scale_df = domar_weight_ccc_sectors,
  scale_col="ratio_domar",
  normalize_scale=FALSE,
  include_domar_error=T,
  level= "sector",
  n_draws = 10000
)


save(ccc_error_bands_cb7_sector, file = here("Datasets", "CCC_CB7_aggregate_error_bands_sector.RData"))




ccc_error_bands_cb6 <- simulate_ccc_error(
  ccc_obj = ccc_cb6,
  zeta_err = df_quantiles_error_extended,
  domar_err = df_quantiles_hulten_extented,
  forecast_start_year = forecast_start_year,
  agg_domar_scale = 0.16,
  sector_scale_df = domar_weight_ccc_sectors,
  scale_col="ratio_domar",
  normalize_scale=FALSE,
  include_domar_error=T,
  level= "agg",
  n_draws = 10000
)
save(ccc_error_bands_cb6, file = here("Datasets", "CCC_CB6_aggregate_error_bands.RData"))




ccc_error_bands_cb6_sector <- simulate_ccc_error(
  ccc_obj = ccc_cb6,
  zeta_err = df_quantiles_error_extended,
  domar_err = df_quantiles_hulten_extented,
  forecast_start_year = forecast_start_year,
  agg_domar_scale = 0,
  sector_scale_df = domar_weight_ccc_sectors,
  scale_col="ratio_domar",
  normalize_scale=FALSE,
  include_domar_error=T,
  level= "sector",
  n_draws = 10000
)
save(ccc_error_bands_cb6_sector, file = here("Datasets", "CCC_CB6_aggregate_error_bands_sectors.RData"))



# print(ccc_error_bands)

# Plot --------------------------------------------------------------------

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
    data = ccc_error_bands %>% filter(year == max(year)),
    aes(y     = base_cumAhat,
        label = paste0(scenario, "\n",
                       round(base_cumAhat * 100, 1), "% [",
                       round(q25_cumAhat  * 100, 1), "–",
                       round(q75_cumAhat  * 100, 1), "%]")),
    colour  = cb7_colour,
    nudge_x = 2, size = 3, hjust = 0, lineheight = 0.9
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year, max(ccc_error_bands$year) + 6) +
  labs(
    x       = "",
    y       = "Cumulative TFP growth",
    caption = "Shaded bands: [q25, q75] and [q10, q90] forecast uncertainty."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB7_errorbands

graph_path <- here("Graphs/CCC")
save_graph("g_cumAhat_CB7_errorbands", width = 9, height = 5)


# CB7 sectors


g_cumAhat_CB7_errorbands_sector <-
  ccc_error_bands_cb7_sector %>%
  # filter(sector=="Industry")%>%
  ggplot(aes(x = year)) +
  facet_wrap(~sector,scales="free")+
  geom_ribbon(aes(ymin = q10, ymax = q90, group = scenario,fill=sector),
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25, ymax = q75, group = scenario,fill=sector),
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, group = scenario,colour=sector),
            lwd = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year, max(ccc_error_bands$year) + 6) +
  labs(
    x       = "",
    y       = "Cumulative TFP growth",
    caption = "Shaded bands: [q25, q75] and [q10, q90] forecast uncertainty."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB7_errorbands_sector



# CB6

# cb7_colour <- "#1b9e77"

g_cumAhat_CB6_errorbands <-
  ccc_error_bands_cb6 %>%
  ggplot(aes(x = year)) +
  facet_wrap(~scenario)+
  geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat, fill=scenario,group = scenario),
              # fill = scenarios_palette,
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat, fill=scenario,group = scenario),
              # fill = scenarios_palette,
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, colour=scenario, group = scenario),
            # colour = scenarios_palette,
            lwd = 0.9) +
  geom_text_repel(
    data = ccc_error_bands_cb6 %>% filter(year == max(year)),
    aes(y     = base_cumAhat,
        colour=scenario,
        label = paste0(scenario, "\n",
                       round(base_cumAhat * 100, 1), "% [",
                       round(q25_cumAhat  * 100, 1), "–",
                       round(q75_cumAhat  * 100, 1), "%]")),
    # colour  = scenarios_palette,
    nudge_x = 2, size = 3, hjust = 0, lineheight = 0.9
  ) +
 scale_colour_manual(values=scenarios_palette)+
  scale_fill_manual(values=scenarios_palette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year, max(ccc_error_bands$year) + 6) +
  labs(
    x       = "",
    y       = "Cumulative TFP growth",
    caption = "Shaded bands: [q25, q75] and [q10, q90] forecast uncertainty."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands



g_cumAhat_CB6_errorbands_no_facet <-
  ccc_error_bands_cb6 %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat, fill=scenario,group = scenario),
              # fill = scenarios_palette,
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, colour=scenario, group = scenario),
            # colour = scenarios_palette,
            lwd = 0.9) +
  scale_colour_manual(values=scenarios_palette)+
  scale_fill_manual(values=scenarios_palette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year, max(ccc_error_bands$year) + 6) +
  labs(
    x       = "",
    y       = "Cumulative TFP growth",
    caption = "Shaded bands: [q25, q75] and [q10, q90] forecast uncertainty."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands_no_facet





g_cumAhat_CB6_errorbands_sector <-
  ccc_error_bands_cb6_sector %>%
  filter(scenario=="Balanced Net Zero Pathway")%>%
  # filter(sector=="Industry")%>%
  ggplot(aes(x = year)) +
  facet_wrap(~sector,scales="free")+
  geom_ribbon(aes(ymin = q10, ymax = q90, group = scenario,fill=sector),
              alpha = 0.12) +
  geom_ribbon(aes(ymin = q25, ymax = q75, group = scenario,fill=sector),
              alpha = 0.25) +
  geom_line(aes(y = base_cumAhat, group = scenario,colour=sector),
            lwd = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  xlim(forecast_start_year, max(ccc_error_bands$year) + 6) +
  labs(
    x       = "",
    y       = "Cumulative TFP growth",
    caption = "Shaded bands: [q25, q75] and [q10, q90] forecast uncertainty."
  ) +
  mytheme +
  theme(legend.position = "none")

g_cumAhat_CB6_errorbands_sector


