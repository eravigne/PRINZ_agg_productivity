if (requireNamespace("here", quietly = TRUE)) {
  source(here::here("0_set_up.R"))
} else {
  source("0_set_up.R")
}

graph_path<-file.path(base_path,"Graphs/Zeta")
tables_path<-file.path(base_path,"Tables/Zeta")




# Main estimation horizon =18y---------------------------------------------------------


# Horizon =20
# coeff = 0.131

# All horizons mixed: 0.22 (even with factor horizon): 
# we should have weights to care for the overrepresentation of short term horizon

# Save
filePath <- file.path(base_path, "Datasets","df_KLEMS_growth_rate.RData")
load(file=filePath,verbose=T)
# Regression 

df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  filter(horizon==18)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)


reg_lm<-lm(D_it~E_it-1,data=df_KLEMS_reg)
summary(reg_lm)

reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)
summary(reg_lm_sector_country_year)


extract_coef <- function(model) {
  coef_summary <- summary(model)$coefficients
  estimate <- coef_summary["E_it", "Estimate"]
  std_err <- coef_summary["E_it", "Std. Error"]
  t_value <- coef_summary["E_it", "t value"]
  
  # Determine significance level
  stars <- if (t_value > 2.58) {
    "***"
  } else if (t_value > 1.96) {
    "**"
  } else if (t_value > 1.64) {
    "*"
  } else {
    ""
  }
  
  coef_formatted <- sprintf("%.3f%s", estimate, stars)
  std_err_formatted <- sprintf("(%.3f)", std_err)
  
  return(c(coef_formatted, std_err_formatted))
}

# Run all combinations of regressions
models <- list(
  lm(D_it ~ E_it - 1, data = df_KLEMS_reg),  # No fixed effects
  lm(D_it ~ E_it - 1 + factor(country), data = df_KLEMS_reg),  # Country FE
  lm(D_it ~ E_it - 1 + factor(sector), data = df_KLEMS_reg),  # Sector FE
  lm(D_it ~ E_it - 1 + factor(Year), data = df_KLEMS_reg),  # Year FE
  lm(D_it ~ E_it - 1 + factor(country) + factor(sector), data = df_KLEMS_reg),  # Country and Sector FE
  lm(D_it ~ E_it - 1 + factor(country) + factor(Year), data = df_KLEMS_reg),  # Country and Year FE
  lm(D_it ~ E_it - 1 + factor(sector) + factor(Year), data = df_KLEMS_reg),  # Sector and Year FE
  lm(D_it ~ E_it - 1 + factor(country) + factor(sector) + factor(Year), data = df_KLEMS_reg)  # All FEs
)

# Apply the coefficient extraction function to all models
model_labels <- c("No FE", "Country", "Sector", "Year", "Country and Sector", "Country and Year", "Sector and Year", "All")
coefs <- lapply(models, extract_coef)
# Extract R2 and n from each model
r2_values <- sapply(models, function(m) sprintf("%.3f", summary(m)$r.squared))
n_values  <- sapply(models, function(m) as.character(nobs(m)))

# Convert to a data frame, each model is a column
results <- as.data.frame(do.call(cbind, coefs))
colnames(results) <- model_labels
rownames(results) <- c("E\\_it", "Std.Error")

# Add fixed effects indicators as separate rows
fe_indicators <- data.frame(
  `No FE` = c("", "", ""),
  `Country` = c("\\checkmark", "", ""),
  `Sector` = c("", "\\checkmark", ""),
  `Year` = c("", "", "\\checkmark"),
  `Country & Sector` = c("\\checkmark", "\\checkmark", ""),
  `Country & Year` = c("\\checkmark", "", "\\checkmark"),
  `Sector & Year` = c("", "\\checkmark", "\\checkmark"),
  `All` = c("\\checkmark", "\\checkmark", "\\checkmark")
)
# fe_indicators <- data.frame(
#   # `No FE` = c("", "", "", "", "", "", ""),
#   `Country` = c("","\\checkmark", "", "", "\\checkmark", "\\checkmark", "", "\\checkmark"),
#   `Sector` = c("","", "\\checkmark", "", "\\checkmark", "", "\\checkmark", "\\checkmark"),
#   `Year` = c("","", "", "\\checkmark", "", "\\checkmark", "\\checkmark", "\\checkmark")
# )

rownames(fe_indicators) <- c("Country FE", "Sector FE", "Year FE")
colnames(fe_indicators)<-colnames(results)
# Combine coefficient results and FE indicators into one table
results_combined <- rbind(results, fe_indicators,"R2" = r2_values, "N" = n_values)
nb_col<-dim(results_combined)[2]
colnames(results_combined)<-c("",paste0("(",seq(1,nb_col-1),")"))
# Create an xtable object for LaTeX export
results_xtable <- xtable(results_combined, 
                         caption = "Regression Models with Different Fixed Effects, KLEMS at horizon 18 years",
                         label = "tab:regression_models")


# Print the xtable as LaTeX code
print.xtable(results_xtable, 
             # include.rownames = FALSE, 
             comment = FALSE,
      file=paste0(tables_path,"/regression_models_KLEMS_FE.tex"),
      floating =FALSE,
      tabular.environment="longtable",
      caption.placement = "top",
      sanitize.text.function = identity,
      print.results=T)


# Inverse relation 1/zeta -------------------------------------------------

filePath <- file.path(base_path, "Datasets","df_KLEMS_growth_rate.RData")
load(file=filePath,verbose=T)
# Regression 

df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  filter(horizon<15)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)



extract_coef_D <- function(model) {
  coef_summary <- summary(model)$coefficients
  estimate <- coef_summary["D_it", "Estimate"]
  std_err <- coef_summary["D_it", "Std. Error"]
  t_value <- coef_summary["D_it", "t value"]
  
  # Determine significance level
  stars <- if (t_value > 2.58) {
    "***"
  } else if (t_value > 1.96) {
    "**"
  } else if (t_value > 1.64) {
    "*"
  } else {
    ""
  }
  
  coef_formatted <- sprintf("%.3f%s", estimate, stars)
  std_err_formatted <- sprintf("(%.3f)", std_err)
  
  return(c(coef_formatted, std_err_formatted))
}

# Run all combinations of regressions
models <- list(
  lm(E_it ~ D_it - 1, data = df_KLEMS_reg),  # No fixed effects
  lm(E_it ~ D_it - 1 + factor(country), data = df_KLEMS_reg),  # Country FE
  lm(E_it ~ D_it - 1 + factor(sector), data = df_KLEMS_reg),  # Sector FE
  lm(E_it ~ D_it - 1 + factor(Year), data = df_KLEMS_reg),  # Year FE
  lm(E_it ~ D_it - 1 + factor(country) + factor(sector), data = df_KLEMS_reg),  # Country and Sector FE
  lm(E_it ~ D_it - 1 + factor(country) + factor(Year), data = df_KLEMS_reg),  # Country and Year FE
  lm(E_it ~ D_it - 1 + factor(sector) + factor(Year), data = df_KLEMS_reg),  # Sector and Year FE
  lm(E_it ~ D_it - 1 + factor(country) + factor(sector) + factor(Year), data = df_KLEMS_reg)  # All FEs
)

# Apply the coefficient extraction function to all models
model_labels <- c("No FE", "Country", "Sector", "Year", "Country and Sector", "Country and Year", "Sector and Year", "All")
coefs <- lapply(models, extract_coef_D)
# Extract R2 and n from each model
r2_values <- sapply(models, function(m) sprintf("%.3f", summary(m)$r.squared))
n_values  <- sapply(models, function(m) as.character(nobs(m)))

# Convert to a data frame, each model is a column
results <- as.data.frame(do.call(cbind, coefs))
colnames(results) <- model_labels
rownames(results) <- c("D\\_it", "Std.Error")

# Add fixed effects indicators as separate rows
fe_indicators <- data.frame(
  `No FE` = c("", "", ""),
  `Country` = c("\\checkmark", "", ""),
  `Sector` = c("", "\\checkmark", ""),
  `Year` = c("", "", "\\checkmark"),
  `Country & Sector` = c("\\checkmark", "\\checkmark", ""),
  `Country & Year` = c("\\checkmark", "", "\\checkmark"),
  `Sector & Year` = c("", "\\checkmark", "\\checkmark"),
  `All` = c("\\checkmark", "\\checkmark", "\\checkmark")
)


rownames(fe_indicators) <- c("Country FE", "Sector FE", "Year FE")
colnames(fe_indicators)<-colnames(results)
# Combine coefficient results and FE indicators into one table
results_combined <- rbind(results, fe_indicators,"R2" = r2_values, "N" = n_values)
nb_col<-dim(results_combined)[2]
colnames(results_combined)<-c("",paste0("(",seq(1,nb_col-1),")"))

results_combined

# NB 1/0.689=1.45

# Main estimation horizon<15y---------------------------------------------------------



# Load
filePath <- file.path(base_path, "Datasets","df_KLEMS_growth_rate.RData")
load(file=filePath,verbose=T)
# Regression 

df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  # filter(horizon<15)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)


# Run all combinations of regressions
models <- list(
  lm(D_it ~ E_it - 1, data = df_KLEMS_reg),  # No fixed effects
  lm(D_it ~ E_it - 1 + factor(country), data = df_KLEMS_reg),  # Country FE
  lm(D_it ~ E_it - 1 + factor(sector), data = df_KLEMS_reg),  # Sector FE
  lm(D_it ~ E_it - 1 + factor(Year), data = df_KLEMS_reg),  # Year FE
  lm(D_it ~ E_it - 1 + factor(horizon), data = df_KLEMS_reg),  # horizon FE
  lm(D_it ~ E_it - 1 + factor(country) + factor(sector), data = df_KLEMS_reg),  # Country and Sector FE
  lm(D_it ~ E_it - 1 + factor(country) + factor(Year), data = df_KLEMS_reg),  # Country and Year FE
  lm(D_it ~ E_it - 1 + factor(sector) + factor(Year), data = df_KLEMS_reg),  # Sector and Year FE
  lm(D_it ~ E_it - 1 + factor(country) + factor(sector) + factor(Year)+ factor(horizon), data = df_KLEMS_reg)  # All FEs
)

# Apply the coefficient extraction function to all models
model_labels <- c("No FE", "Country", "Sector", "Year","Horizon", "Country and Sector", "Country and Year", "Sector and Year", "All")
coefs <- lapply(models, extract_coef)
# Extract R2 and n from each model
r2_values <- sapply(models, function(m) sprintf("%.3f", summary(m)$r.squared))
n_values  <- sapply(models, function(m) as.character(nobs(m)))

# Convert to a data frame, each model is a column
results <- as.data.frame(do.call(cbind, coefs))
colnames(results) <- model_labels
rownames(results) <- c("E\\_it", "Std.Error")

# Add fixed effects indicators as separate rows
fe_indicators <- data.frame(
  `No FE` = c("", "", "", ""),
  `Country` = c("\\checkmark", "", "", ""),
  `Sector` = c("", "\\checkmark", "", ""),
  `Year` = c("", "", "\\checkmark", ""),
  `Horizon` = c("", "", "", "\\checkmark"),
  `Country & Sector` = c("\\checkmark", "\\checkmark", "", ""),
  `Country & Year` = c("\\checkmark", "", "\\checkmark", ""),
  `Sector & Year` = c("", "\\checkmark", "\\checkmark", ""),
  `All` = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")
)
# fe_indicators <- data.frame(
#   # `No FE` = c("", "", "", "", "", "", ""),
#   `Country` = c("","\\checkmark", "", "", "\\checkmark", "\\checkmark", "", "\\checkmark"),
#   `Sector` = c("","", "\\checkmark", "", "\\checkmark", "", "\\checkmark", "\\checkmark"),
#   `Year` = c("","", "", "\\checkmark", "", "\\checkmark", "\\checkmark", "\\checkmark")
# )

rownames(fe_indicators) <- c("Country FE", "Sector FE", "Year FE","Horizon FE")
colnames(fe_indicators)<-colnames(results)
# Combine coefficient results and FE indicators into one table
results_combined <- rbind(results, fe_indicators,"R2" = r2_values, "N" = n_values)
nb_col<-dim(results_combined)[2]
colnames(results_combined)<-c("",paste0("(",seq(1,nb_col-1),")"))
results_combined
# Create an xtable object for LaTeX export
results_xtable <- xtable(results_combined, 
                         caption = "Regression Models with Different Fixed Effects, KLEMS at horizon 18 years",
                         label = "tab:regression_models")



# Various horizons -------------------------------------------------------

build_regression_zeta_by_horizon <- function(df_growth_rate) {
  horizons <- sort(unique(df_growth_rate$horizon))

  results <- map_dfr(horizons, function(i) {
    df_reg <- df_growth_rate %>%
      filter(horizon == i) %>%
      mutate(
        D_it = Aihat + (pihat - hat_infl),
        E_it = yihat + pihat - hat_infl
      ) %>%
      select(country, sector, Year, horizon, D_it, E_it) %>%
      filter(!is.na(D_it), !is.na(E_it))

    if (nrow(df_reg) == 0) {
      return(tibble(
        horizon = i,
        E_it_coefficient = NA_real_,
        E_it_p_value = NA_real_,
        n_obs = 0L
      ))
    }

    # Sparse long-horizon slices can collapse one FE dimension to a single level.
    available_fe <- c("sector", "country", "Year") %>%
      keep(~ n_distinct(df_reg[[.x]], na.rm = TRUE) > 1)

    reg_formula_rhs <- c(
      "E_it - 1",
      map_chr(available_fe, ~ sprintf("factor(%s)", .x))
    ) %>%
      paste(collapse = " + ")

    reg_lm_sector_country_year_horizoni <-
      lm(as.formula(paste("D_it ~", reg_formula_rhs)), data = df_reg)
    summary_model <- summary(reg_lm_sector_country_year_horizoni)
    coef_table <- summary_model$coefficients
    has_eit_row <- "E_it" %in% rownames(coef_table)

    tibble(
      horizon = i,
      E_it_coefficient = if (has_eit_row) coef_table["E_it", "Estimate"] else NA_real_,
      E_it_p_value = if (has_eit_row) coef_table["E_it", "Pr(>|t|)"] else NA_real_,
      n_obs = nrow(df_reg)
    )
  })

  results %>%
    mutate(significance = case_when(
      E_it_p_value < 0.001 ~ "***",
      E_it_p_value < 0.01 ~ "**",
      E_it_p_value < 0.05 ~ "*",
      E_it_p_value < 0.1 ~ ".",
      TRUE ~ ""
    ))
}

build_rmse_optimal_zeta_by_horizon <- function(df_growth_rate) {
  horizons <- sort(unique(df_growth_rate$horizon))

  map_dfr(horizons, function(i) {
    df_h <- df_growth_rate %>%
      filter(horizon == i) %>%
      filter(!is.na(Aihat), !is.na(yihat), !is.na(pihat), !is.na(hat_infl))

    if (nrow(df_h) == 0) {
      return(tibble(
        horizon = i,
        zeta_rmse = NA_real_,
        rmse = NA_real_,
        n_obs = 0L
      ))
    }

    compute_rmse_h <- function(zeta) {
      predAihat <- zeta * df_h$yihat + (zeta - 1) * (df_h$pihat - df_h$hat_infl)
      sqrt(mean((predAihat - df_h$Aihat)^2, na.rm = TRUE))
    }

    result <- optimize(compute_rmse_h, lower = 0, upper = 1)

    tibble(
      horizon = i,
      zeta_rmse = result$minimum,
      rmse = result$objective,
      n_obs = nrow(df_h)
    )
  })
}


build_mae_optimal_zeta_by_horizon <- function(df_growth_rate) {
  horizons <- sort(unique(df_growth_rate$horizon))
  
  map_dfr(horizons, function(i) {
    df_h <- df_growth_rate %>%
      filter(horizon == i) %>%
      filter(!is.na(Aihat), !is.na(yihat), !is.na(pihat), !is.na(hat_infl))
    
    if (nrow(df_h) == 0) {
      return(tibble(
        horizon = i,
        zeta_mae = NA_real_,
        rmse = NA_real_,
        n_obs = 0L
      ))
    }
    
    compute_mae_h <- function(zeta) {
      predAihat <- zeta * df_h$yihat + (zeta - 1) * (df_h$pihat - df_h$hat_infl)
      (mean(abs(predAihat - df_h$Aihat), na.rm = TRUE))
    }
    
    result <- optimize(compute_mae_h, lower = 0, upper = 1)
    
    tibble(
      horizon = i,
      zeta_mae = result$minimum,
      rmse = result$objective,
      n_obs = nrow(df_h)
    )
  })
}

load(file.path(base_path, "Datasets", "df_STAN_growthrate.RData"), verbose = TRUE)
load(file.path(base_path, "Datasets", "df_nber_growthrate.RData"), verbose = TRUE)

results_eit_horizon_klems <- build_regression_zeta_by_horizon(df_KLEMS_growth_rate)
results_eit_horizon_stan <- build_regression_zeta_by_horizon(df_STAN_growthrate)
results_eit_horizon_nber <- build_regression_zeta_by_horizon(df_nber_growthrate)

results_rmse_horizon_klems <- build_rmse_optimal_zeta_by_horizon(df_KLEMS_growth_rate)
results_rmse_horizon_stan <- build_rmse_optimal_zeta_by_horizon(df_STAN_growthrate)
results_rmse_horizon_nber <- build_rmse_optimal_zeta_by_horizon(df_nber_growthrate)

results_mae_horizon_klems <- build_mae_optimal_zeta_by_horizon(df_KLEMS_growth_rate)
results_mae_horizon_stan <- build_mae_optimal_zeta_by_horizon(df_STAN_growthrate)
results_mae_horizon_nber <- build_mae_optimal_zeta_by_horizon(df_nber_growthrate)




db_colors <- c("KLEMS" = "#00CC00", "NBER CES" = "#FF6600", "STAN" = "#0000FF")
comparison_horizon_max <- max(results_eit_horizon_klems$horizon, na.rm = TRUE)

g_Eit_klems <- results_eit_horizon_klems %>%
  ggplot(aes(x = horizon, y = E_it_coefficient)) +
  geom_line(colour = db_colors["KLEMS"]) +
  geom_point(colour = db_colors["KLEMS"]) +
  mytheme +
  labs(
    y = expression("Estimated " * zeta),
    x = "Horizon (years)",
    caption = "KLEMS, regression by horizon-growth rates"
  )
g_Eit_klems

save_graph("g_Eit_klems", width = 4, height = 4)


# Compare it across databases ---------------------------------------------

df_eit_alldb <- bind_rows(
  results_eit_horizon_klems %>% mutate(db = "KLEMS"),
  results_eit_horizon_stan %>% mutate(db = "STAN"),
  results_eit_horizon_nber %>% mutate(db = "NBER CES")
) %>%
  filter(horizon <= comparison_horizon_max)

df_rmse_zeta_alldb <- bind_rows(
  results_rmse_horizon_klems %>% mutate(db = "KLEMS"),
  results_rmse_horizon_stan %>% mutate(db = "STAN"),
  results_rmse_horizon_nber %>% mutate(db = "NBER CES")
) %>%
  filter(horizon <= comparison_horizon_max)


df_mae_zeta_alldb <- bind_rows(
  results_mae_horizon_klems %>% mutate(db = "KLEMS"),
  results_mae_horizon_stan %>% mutate(db = "STAN"),
  results_mae_horizon_nber %>% mutate(db = "NBER CES")
) %>%
  filter(horizon <= comparison_horizon_max)



max_y <- max(df_eit_alldb$E_it_coefficient,
             df_rmse_zeta_alldb$zeta_rmse,
             na.rm = TRUE)

min_y <- min(df_eit_alldb$E_it_coefficient,
             df_rmse_zeta_alldb$zeta_rmse,
             na.rm = TRUE)

ylims <- c(min_y * 0.95, max_y * 1.05)

base_theme <- mytheme +
  theme(
    legend.position = c(0.72, 0.82),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_blank()
  )

g_Eit_alldb <- df_eit_alldb %>%
  ggplot(aes(x = horizon, y = E_it_coefficient, colour = db)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0.13, colour = "grey60", linetype = "dashed") +
  scale_colour_manual(values = db_colors) +
  scale_y_continuous(
    breaks = c(seq(0.1, 0.4, 0.1), 0.13),
    limits = ylims
  ) +
  labs(
    title = "A. Estimated by Database",
    y = expression(zeta),
    x = "Horizon (years)",
    colour = ""
  ) +
  base_theme

g_zeta_rmse_horizon_alldb <- df_rmse_zeta_alldb %>%
  ggplot(aes(x = horizon, y = zeta_rmse, colour = db)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0.13, colour = "grey60", linetype = "dashed") +
  scale_colour_manual(values = db_colors) +
  scale_y_continuous(
    breaks = c(seq(0.1, 0.4, 0.1), 0.13),
    limits = ylims
  ) +
  labs(
    # title = paste0("B. RMSE-Optimal ",expression(zeta)," by Database"),
    title = bquote(bold("B. RMSE-Optimal " * zeta * " by Database")),
    y = expression(zeta),
    x = "Horizon (years)",
    colour = ""
  ) +
  base_theme


g_zeta_mae_horizon_alldb <- df_mae_zeta_alldb %>%
  ggplot(aes(x = horizon, y = zeta_mae, colour = db)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0.13, colour = "grey60", linetype = "dashed") +
  scale_colour_manual(values = db_colors) +
  scale_y_continuous(
    breaks = c(seq(0.1, 0.4, 0.1), 0.13),
    limits = ylims
  ) +
  labs(
    # title = paste0("B. mae-Optimal ",expression(zeta)," by Database"),
    title = bquote(bold("B. MAE-Optimal " * zeta * " by Database")),
    y = expression(zeta),
    x = "Horizon (years)",
    colour = ""
  ) +
  base_theme

g_zeta_compare_2panel <-
  g_Eit_alldb +
  g_zeta_mae_horizon_alldb +
  plot_layout(ncol = 2)

g_zeta_compare_2panel + canvas(10, 5, "in")

save_graph("g_zeta_compare_2panel", width = 10, height = 5)

# Horizon 10 Y ------------------------------------------------------------

df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  filter(horizon==10)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)


reg_lm<-lm(D_it~E_it-1,data=df_KLEMS_reg)
summary(reg_lm)

reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)
summary(reg_lm_sector_country_year)

# Various FE combinations -------------------------------------------------


reg_lm_country<-lm(D_it~E_it-1+factor(country),data=df_KLEMS_reg)
summary(reg_lm_country)

reg_lm_sector<-lm(D_it~E_it-1+factor(sector),data=df_KLEMS_reg)
summary(reg_lm_sector)

reg_lm_sector_country<-lm(D_it~E_it-1+factor(sector)+factor(country),data=df_KLEMS_reg)
summary(reg_lm_sector_country)

reg_lm_sector<-lm(D_it~E_it-1+factor(sector),data=df_KLEMS_reg)
summary(reg_lm_sector)

reg_lm_comb<-lm(D_it~E_it-1+
                  factor(sector)+factor(Year),data=df_KLEMS_reg)
summary(reg_lm_comb)

reg_lm_comb<-lm(D_it~E_it-1+
                  factor(country)+factor(Year),data=df_KLEMS_reg)
summary(reg_lm_comb)

# reg_lm_comb<-lm(D_it~E_it-1+
                  # factor(country)*factor(sector),data=df_KLEMS_reg)
# summary(reg_lm_comb)





# Multicolinearity --------------------------------------------------------
# Multicollinearity can be diagnosed using the Variance Inflation Factor (VIF), 
# where a VIF value greater than 10 typically indicates high multicollinearity.

# NB: no multicolinerary in D~E

# Limited colinerarity between the factor and the regressor
vif_values <- vif(reg_lm_country)
print(vif_values)

# sector, year and country are correlated, but it's expected
print(vif(reg_lm_sector_country))
print(vif(reg_lm_sector_country_year))


# Heteroscedasticity ------------------------------------------------------------------
# Heteroscedasticity can be checked using the Breusch-Pagan test, which tests for non-constant variance.
# Null Hypothesis: The null hypothesis for the Breusch-Pagan test is that the residuals 
# are homoscedastic, which means that they have constant variance across different 
# levels of the independent variables.
# H 0: The residuals are homoscedastic

# If the p-value is small (typically less than 0.05 or 0.1), you reject the null 
# hypothesis and conclude that heteroscedasticity exists. The residuals' 
# variance is not constant, indicating that your regression model may not be well specified.


df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  filter(horizon==18)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)

reg_lm<-lm(D_it~E_it-1,data=df_KLEMS_reg)
reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)

# Conduct Breusch-Pagan Test for heteroscedasticity
bp_test <- bptest(reg_lm_sector_country_year)
print(bp_test)

# Conduct White test: we reject the null of homoscedasticity
ncvTest(reg_lm)
ncvTest(reg_lm_sector_country_year)

# We are hetereoscedastic
model<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)
summary(model)
# Robust standard errors
robust_se <- vcovHC(model, type = "HC1")
coeftest(model, vcov = robust_se)
# the coefficient of Eit is robust to heteroscedasticity


# Residual sigma square ---------------------------------------------------

# Extract residuals
residuals <- resid(reg_lm_sector_country_year)

# Create a data frame with E_it and residuals
residuals_eit <- data.frame(E_it = df_KLEMS_reg$E_it, Residuals = residuals)

# Create bins for E_it
residuals_eit <- residuals_eit %>%
  mutate(E_it_bin = cut(E_it, breaks = 30)) %>% # Adjust the number of bins if necessary
  group_by(E_it_bin) %>%
  summarize(E_it_mean = mean(E_it), SD_Residuals = sd(Residuals))
save_graph("g_residuals_eit",width=4,height=4)


# Plot the standard deviation of the residuals against the mean of E_it bins
g_residuals_eit<-ggplot(residuals_eit, aes(x = E_it_mean, y = SD_Residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    # title = "Standard Deviation of Residuals vs RHS",
       x = "RHS",
       y = "Standard Deviation of Residuals") +
  mytheme
save_graph("g_residuals_eit",width=4,height=4)



# Create a data frame with D_it and residuals
residuals_df <- data.frame(D_it = df_KLEMS_reg$D_it, Residuals = residuals)

# Create bins for D_it
residuals_df <- residuals_df %>%
  mutate(D_it_bin = cut(D_it, breaks = 30)) %>% # Adjust the number of bins if necessary
  group_by(D_it_bin) %>%
  summarize(D_it_mean = mean(D_it), SD_Residuals = sd(Residuals))

# Plot the standard deviation of the residuals against the mean of D_it bins
g_residuals_dit<-ggplot(residuals_df, aes(x = D_it_mean, y = SD_Residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
  # title = "Standard Deviation of Residuals vs LHS",
       x = "LHS",
       y = "Standard Deviation of Residuals") +
  mytheme
g_residial_dit
save_graph("g_residuals_dit",width=4,height=4)


# Residual Plot Analysis -------------------------------------------------


# Residual Plot Analysis: Inspect residuals graphically for patterns t
# hat may indicate model misspecification or non-linearity.
df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  filter(horizon==18)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)

reg_lm<-lm(D_it~E_it-1,data=df_KLEMS_reg)
reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)


# Extract residuals and fitted values for the first model
residuals_lm <- resid(reg_lm)
fitted_lm <- fitted(reg_lm)

df_residuals_lm_noFE<-
  data.frame("fitted"=fitted_lm,
             "residual"=residuals_lm)
g_residual_lm_noFE<-
  df_residuals_lm_noFE%>%
  ggplot(aes(x=fitted,y=residual))+
  geom_point()+
  mytheme+
  geom_hline(yintercept = 0,colour="grey80",lwd=1.2)+
  labs(x="Fitted value",y="Residuals",caption="KLEMS, horizon 10Y, no fixed effects")
g_residual_lm_noFE
save_graph("g_residual_lm_noFE",width=4,height=4)


# Extract residuals and fitted values for the second model
residuals_lm_scy <- resid(reg_lm_sector_country_year)
fitted_lm_scy <- fitted(reg_lm_sector_country_year)
df_residuals_lm_allFE<-
  data.frame("fitted"=fitted_lm_scy,
             "residual"=residuals_lm_scy)
g_residual_lm_allFE<-
  df_residuals_lm_allFE%>%
  ggplot(aes(x=fitted,y=residual))+
  geom_point()+
  mytheme+
  geom_hline(yintercept = 0,colour="grey80",lwd=1.2)+
  labs(x="Fitted value",y="Residuals",caption="KLEMS, horizon 10Y, \\Country, sector, and year fixed effects")
g_residual_lm_allFE
save_graph("g_residual_lm_allFE",width=4,height=4)




# Plot residuals vs. fitted values for the second model
plot(fitted_lm_scy, residuals_lm_scy,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot for Model: D_it ~ E_it -1 + factor(sector) + factor(country) + factor(Year)",
     col = "green", pch = 20)
abline(h = 0, col = "red", lwd = 2)



# Cook outliers -----------------------------------------------------------
# Influence Measures: Calculate Cook's distance or leverage statistics to identify 
# influential data points that could disproportionately affect the results.



# Number of observations and predictors for your first model
n_lm <- nrow(df_KLEMS_reg)
k_lm <- length(coefficients(reg_lm)) - 1

# Calculate Cook's Distance for the first model
cooks_lm <- cooks.distance(reg_lm)

# Threshold for influential points
threshold_lm <- 4 / (n_lm - k_lm - 1)

# Plot Cook's Distance
plot(cooks_lm,
     type = "h",
     ylab = "Cook's Distance",
     main = "Cook's Distance for Model: D_it ~ E_it -1")
abline(h = threshold_lm, col = "red")

# Find indices of influential points
influential_points_lm <- which(cooks_lm > threshold_lm)
print(influential_points_lm)

# Repeat the process for the second model
n_scy <- nrow(df_KLEMS_reg)
k_scy <- length(coefficients(reg_lm_sector_country_year)) - 1

cooks_scy <- cooks.distance(reg_lm_sector_country_year)
# We use D>4/n (nn bb of degrees of freedom) as threshold for the Cook's distance
threshold_scy <- 4 / (n_scy - k_scy - 1)

plot(cooks_scy,
     type = "h",
     ylab = "Cook's Distance",
     main = "Cook's Distance for Model: D_it ~ E_it -1 + factor(sector) + factor(country) + factor(Year)")
abline(h = threshold_scy, col = "red")

influential_points_scy <- which(cooks_scy > threshold_scy)
print(influential_points_scy)

length(influential_points_scy) #439 points
dim(df_KLEMS_reg) # out of 8681 points


# New model without outliers ----------------------------------------------



df_KLEMS_reg<-
  df_KLEMS_growth_rate%>%
  filter(horizon==18)%>%
  mutate(D_it=Aihat+(pihat-hat_infl),
         E_it=yihat+pihat-hat_infl)%>%
  select(country,sector,Year,horizon,D_it,E_it)


reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)
cooks_scy <- cooks.distance(reg_lm_sector_country_year)
df_KLEMS_reg$CooksDistance_scy <- cooks_scy

# Define threshold (example using 4/(n-k-1))
n_scy <- nrow(df_KLEMS_reg)
k_scy <- length(coefficients(reg_lm_sector_country_year)) - 1
threshold_scy <- 4 / (n_scy - k_scy - 1)

# Filter the data frame to exclude influential points
df_KLEMS_filtered <- 
  df_KLEMS_reg %>% 
  filter(CooksDistance_scy <= threshold_scy)
dim(df_KLEMS_filtered)
dim(df_KLEMS_reg)
model<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_filtered)
# Robust standard errors
robust_se <- vcovHC(model, type = "HC1")
coeftest(model, vcov = robust_se)

# Autocorrelation ---------------------------------------------------------

Nc=sample(df_KLEMS_filtered$country,1)
Ni=sample((df_KLEMS_filtered%>%ungroup()%>%filter(country==Nc)%>%select(sector)%>%distinct())$sector,1)

df_KLEMS_filtered%>%
  select(-CooksDistance_scy)%>%
  filter(country==Nc & sector==Ni)%>%
  gather(key=var,value=value,-c(1:4))%>%
  ggplot()+
  geom_line(aes(x=Year,y=value,colour=var,group=interaction(country,sector)))+
  facet_wrap(~var,scales="free")

# Autocorrelation: If time is involved, use Durbin-Watson or Breusch-Godfrey tests for autocorrelation in residuals.

# The test statistic ranges from 0 to 4, where:
# A value close to 2 indicates little or no autocorrelation.
# A value closer to 0 indicates strong positive autocorrelation.
# A value closer to 4 indicates strong negative autocorrelation.

# Durbin-Watson test for the first model (basic regression)
dw_test_basic <- dwtest(reg_lm)
print(dw_test_basic)

# Durbin-Watson test for the second model (with fixed effects)
dw_test_fe <- dwtest(reg_lm_sector_country_year)
print(dw_test_fe)


# first difference
df_KLEMS_filtered <- df_KLEMS_filtered %>%
  arrange(country, sector, Year) %>%
  group_by(country, sector) %>%
  mutate(D_it_diff = D_it-lag(D_it),
         E_it_diff = E_it-lag(E_it)) %>%
  ungroup()
reg_lm_sector_country_year_diff<-lm(D_it_diff~E_it_diff-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_filtered)
summary(reg_lm_sector_country_year_diff)
# coeff O.41 ***
dw_test_fe <- dwtest(reg_lm_sector_country_year_diff)
print(dw_test_fe)
# close to 2: no autocorrelation


# Create lagged dependent variable (D_it_lag)
df_KLEMS_reg <- df_KLEMS_reg %>%
  arrange(country, sector, Year) %>%
  group_by(country, sector) %>%
  mutate(D_it_lag = lag(D_it)) %>%
  ungroup()


# Apply Newey-West HAC standard errors to the model with fixed effects
coeftest(reg_lm_sector_country_year, vcov = NeweyWest(reg_lm_sector_country_year, lag = 1, prewhite = FALSE))


# Run the regression with lagged dependent variable
reg_lm_lag <- lm(D_it ~ E_it - 1 + D_it_lag + factor(sector) + factor(country) + factor(Year), data = df_KLEMS_reg)
summary(reg_lm_lag)
# Durbin-Watson test for the second model (with fixed effects)
dw_test_fe <- dwtest(reg_lm_lag)
print(dw_test_fe)

# Fit a GLS model assuming an autoregressive (AR) process of order 1
gls_model <- gls(D_it ~ E_it - 1 + factor(sector) + factor(country) + factor(Year),
                 correlation = corAR1(), data = df_KLEMS_reg)
summary(gls_model)
# model is AR(1)
# coeff is still significant => value is 0.198. Residual standard error is 0.128.

# Sub-Sample Analysis: Run the regression separately for different sub-groups (e.g., countries, regions) to confirm consistent results across subsets of data.

# Multicollinearity: Examine the variance inflation factors (VIF) or correlation matrices to detect high correlations between independent variables.

# Heteroscedasticity: Test for non-constant variance in residuals using Breusch-Pagan or White tests.
# Heteroscedasticity means that the error variance isn't constant, leading to more significant forecast errors in certain conditions.
# Forecast intervals could be misleading if heteroscedasticity is not properly addressed, as the confidence intervals for your forecast may not accurately reflect the true prediction uncertainty.



# GLS ---------------------------------------------------------------------

# data
# df_KLEMS_filtered

# Manual GLS: but does not work for autocorrelation

# 1. OLS
reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_filtered)
summary(reg_lm_sector_country_year)
residuals_ols <- residuals(reg_lm_sector_country_year)

# 2. Estimate the Error Variance Structure
residuals_squared <- residuals_ols^2
heteroscedasticity_model <-lm(residuals_squared~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_filtered)
variance_fitted <- fitted(heteroscedasticity_model)

# 3. Calculate the GLS Weights
weights_gls <- 1 / sqrt(variance_fitted)

# 4. GLS
gls_model <- lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_filtered, weights = weights_gls)
summary(gls_model)
# still 0.24


# GLS (in nlme) -----------------------------------------------------------

# With AR(1) and heteroscedasticity
gls_model_structured <- gls(D_it ~ E_it - 1 + factor(sector) + factor(country) + factor(Year),
                            data = df_KLEMS_filtered,
                            correlation = corAR1(form = ~ 1 | sector),
                            weights = varIdent(form = ~ 1 | sector))

# View the summary of the GLS model with structures
summary(gls_model_structured)


# With AR(1) and heteroscedasticity
gls_model_structured <- gls(D_it ~ E_it - 1 + factor(sector) + factor(country) + factor(Year),
                            data = df_KLEMS_filtered,
                            correlation = corAR1(form = ~ 1 | sector+country),
                            weights = varIdent(form = ~ 1 | sector+country))

# View the summary of the GLS model with structures
summary(gls_model_structured)


# Fixed effects -----------------------------------------------------------


reg_lm_sector_country_year<-lm(D_it~E_it-1+factor(sector)+factor(country)+factor(Year),data=df_KLEMS_reg)

# Extract model summary
model_summary <- summary(reg_lm_sector_country_year)

# Extract coefficients for fixed effects
coefficients <- model_summary$coefficients

# Function to process fixed effects coefficients and standard errors
process_fixed_effects <- function(pattern, label) {
  # Extract relevant rows for the fixed effect pattern
  effect_indices <- grep(pattern, rownames(coefficients))
  effect_data <- coefficients[effect_indices, , drop = FALSE]
  
  # Prepare a data frame with estimate and standard errors
  data.frame(
    group = sub(pattern, "", rownames(effect_data)),
    estimate = effect_data[, 1],
    std_error = effect_data[, 2],
    label = label
  )
}

# Process fixed effects for country, sector, and year
df_country <- process_fixed_effects("factor\\(country\\)", "Country")
df_sector <- process_fixed_effects("factor\\(sector\\)", "Sector")
df_year <- process_fixed_effects("factor\\(Year\\)", "Year")

# Combine all fixed effects data frames
df_all <- bind_rows(df_country, df_sector, df_year)

# Plot function
plot_fixed_effects <- function(data) {
  order<-data%>%arrange(desc(group))  
   ggplot(data%>%
           mutate(group=factor(group,level=order$group))%>%
           mutate(newgroup=str_replace(group,"-","-\n")), aes(x = group, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96*std_error, ymax = estimate + 1.96*std_error), width = 0.2) +
    facet_wrap(~label, scales = "free", ncol = 1) +
    labs(x = "",
         y = "") +
    coord_flip()+
    mytheme+
    geom_hline(yintercept = 0,colour="grey60")+ylim(c(-0.3,0.5))+
    theme(axis.text.y = element_text(size=8))
    
}

g_fixed_effects_KLEMS_h18<-
  ggarrange(plot_fixed_effects(df_country),
          plot_fixed_effects(df_sector),
          plot_fixed_effects(df_year),ncol=1,heights =c(2,4,1),
          align="v")
g_fixed_effects_KLEMS_h18
save_graph("g_fixed_effects_KLEMS_h18",width=8,height=10)

# Plot the fixed effects estimates
g_fixed_effects_KLEMS_h18<-
  plot_fixed_effects(df_all)+
  theme(axis.text.y = element_text(size=6))
g_fixed_effects_KLEMS_h18
save_graph("g_fixed_effects_KLEMS_h18",width=8,height=10)

