# Joint KLEMS hindcast: zeta mapping and Domar aggregation in one step.
#
# This script validates the combined equation directly:
#   Ahat_cth ~= sum_i lambda_i,t * (zeta * Xhat_i,cth + (zeta - 1) * phat_i,cth)
#
# It differs from the previous two-step hindcasts by choosing zeta from the
# aggregate prediction error after applying Domar weights.


# Setup -------------------------------------------------------------------

library(here)
source(here("0_set_up.R"))

dataset_path <- file.path(base_path, "Datasets")
graph_path <- file.path(base_path, "Graphs", "Joint_hindcasting")
tables_path <- file.path(base_path, "Tables", "Joint_hindcasting")

dir.create(graph_path, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_path, recursive = TRUE, showWarnings = FALSE)

load(file.path(dataset_path, "df_KLEMS_growth_rate.RData"))
load(file.path(dataset_path, "df_KLEMS_Hulten_growth.RData"))


# User-facing choices ------------------------------------------------------

# Keep the zeta search range aligned with the previous zeta-only validation.
zeta_lower <- 0
zeta_upper <- 1

# For the reported CI, use the same long-horizon calibration convention as
# the KLEMS zeta-only exercise.
long_horizon_cutoff <- 15
ci_sample <- "Long horizons (h >= 15)"
ci_metric <- "Mean absolute error"
ci_empirical_cutoff <- 20
ci_extend_to <- 30


# KLEMS country coverage ---------------------------------------------------

# Reuse the complete-economy sector classifications from the Domar hindcast.
# A1 has separate D and E sectors; B1 has the combined D-E sector.
set_A1 <- strsplit("A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S", ",")[[1]]
set_B1 <- strsplit("A,B,C,D,E,F,G,H,I,J,K,L,M-N,O,P,Q,R,S", ",")[[1]]

classify_country <- function(sectors) {
  sectors <- sort(unique(sectors))
  if (all(set_A1 %in% sectors)) return("A1")
  if (all(set_B1 %in% sectors)) return("B1")
  "other"
}

country_sectors <- split(df_KLEMS_Hulten_growth$sector, df_KLEMS_Hulten_growth$country)
country_classification <- data.frame(
  country = names(country_sectors),
  classification = vapply(country_sectors, classify_country, character(1)),
  stringsAsFactors = FALSE
)
country_classification$expected_n <- ifelse(
  country_classification$classification == "A1", length(set_A1),
  ifelse(country_classification$classification == "B1", length(set_B1), NA_integer_)
)


# Build sector-level joint panel ------------------------------------------

# zeta_panel contains the industry price-output objects.
zeta_panel <- df_KLEMS_growth_rate[, c(
  "Year", "country", "sector", "horizon", "Aihat", "pihat", "yihat",
  "hat_infl", "si", "s"
)]
names(zeta_panel)[names(zeta_panel) == "Aihat"] <- "industry_Ahat"

# hulten_panel supplies the actual aggregate TFP growth and initial Domar
# weights on the same country-sector-year-horizon grid.
hulten_panel <- df_KLEMS_Hulten_growth[, c(
  "Year", "country", "sector", "horizon", "Ahat", "domar_weight_initial",
  "Year_initial"
)]

joint_sector_panel <- merge(
  zeta_panel,
  hulten_panel,
  by = c("Year", "country", "sector", "horizon")
)
joint_sector_panel <- merge(
  joint_sector_panel,
  country_classification,
  by = "country",
  all.x = TRUE
)

# Keep only finite observations in countries with complete A1/B1 coverage.
joint_sector_panel <- joint_sector_panel[
  joint_sector_panel$classification %in% c("A1", "B1") &
    is.finite(joint_sector_panel$pihat) &
    is.finite(joint_sector_panel$yihat) &
    is.finite(joint_sector_panel$hat_infl) &
    is.finite(joint_sector_panel$Ahat) &
    is.finite(joint_sector_panel$domar_weight_initial),
]

# Write the aggregate prediction as intercept + zeta * slope. This makes the
# optimisation cheap and keeps the formula visible:
#   lambda * (zeta * Xhat + (zeta - 1) * phat)
joint_sector_panel$real_pihat <- joint_sector_panel$pihat - joint_sector_panel$hat_infl
joint_sector_panel$zeta_slope <- joint_sector_panel$domar_weight_initial *
  (joint_sector_panel$yihat + joint_sector_panel$real_pihat)
joint_sector_panel$zeta_intercept <- -joint_sector_panel$domar_weight_initial *
  joint_sector_panel$real_pihat


# Aggregate to country-year-horizon cells ---------------------------------

group_vars <- c("country", "Year_initial", "Year", "horizon", "expected_n")

# Sum the sector-level zeta slopes and intercepts with Domar weights already
# embedded in each sector term.
agg_terms <- aggregate(
  joint_sector_panel[, c("zeta_slope", "zeta_intercept", "domar_weight_initial")],
  joint_sector_panel[group_vars],
  sum
)
names(agg_terms)[names(agg_terms) == "domar_weight_initial"] <- "domar_weight_sum"

# Actual aggregate TFP growth is identical across sectors within each cell.
agg_actual <- aggregate(
  joint_sector_panel$Ahat,
  joint_sector_panel[group_vars],
  function(x) x[1]
)
names(agg_actual)[names(agg_actual) == "x"] <- "actual_Ahat"

agg_n <- aggregate(
  joint_sector_panel$sector,
  joint_sector_panel[group_vars],
  function(x) length(unique(x))
)
names(agg_n)[names(agg_n) == "x"] <- "n_sector"

# A single row is now one aggregate hindcast target: country x start year x
# end year x horizon.
joint_aggregate_panel <- Reduce(
  function(x, y) merge(x, y, by = group_vars),
  list(agg_terms, agg_actual, agg_n)
)

# Drop any cell that does not contain the full expected sector set.
joint_aggregate_panel <- joint_aggregate_panel[
  joint_aggregate_panel$n_sector == joint_aggregate_panel$expected_n,
]


# Optimise zeta in the joint aggregate equation ---------------------------

prediction_error <- function(zeta, panel = joint_aggregate_panel) {
  panel$zeta_intercept + zeta * panel$zeta_slope - panel$actual_Ahat
}

# Metrics mirror the aggregate nature of the joint validation.
metrics <- list(
  "Absolute mean error" = function(e) abs(mean(e)),
  "Mean absolute error" = function(e) mean(abs(e)),
  "Absolute median error" = function(e) abs(median(e)),
  "Median absolute error" = function(e) median(abs(e)),
  "Root mean square error" = function(e) sqrt(mean(e^2))
)

optimise_metric <- function(panel, metric_name) {
  metric_fun <- metrics[[metric_name]]
  opt <- optimize(
    function(zeta) metric_fun(prediction_error(zeta, panel)),
    lower = zeta_lower,
    upper = zeta_upper
  )
  e <- prediction_error(opt$minimum, panel)
  data.frame(
    metric = metric_name,
    zeta = opt$minimum,
    objective_log_points = opt$objective,
    n_aggregate_obs = length(e),
    mean_error_pp = 100 * (exp(mean(e)) - 1),
    median_error_pp = 100 * (exp(median(e)) - 1),
    mae_log_points = mean(abs(e)),
    rmse_log_points = sqrt(mean(e^2)),
    stringsAsFactors = FALSE
  )
}

# Report both all horizons and the long-horizon sample used for calibration.
samples <- list(
  "All horizons" = rep(TRUE, nrow(joint_aggregate_panel)),
  "Long horizons (h >= 15)" = joint_aggregate_panel$horizon >= long_horizon_cutoff
)

zeta_results <- do.call(rbind, lapply(names(samples), function(sample_name) {
  panel <- joint_aggregate_panel[samples[[sample_name]],]
  out <- do.call(rbind, lapply(names(metrics), function(metric_name) {
    optimise_metric(panel, metric_name)
  }))
  out$sample <- sample_name
  out
}))

zeta_results <- zeta_results[, c(
  "sample", "metric", "zeta", "objective_log_points", "n_aggregate_obs",
  "mean_error_pp", "median_error_pp", "mae_log_points", "rmse_log_points"
)]

write.csv(
  zeta_results,
  file.path(tables_path, "tab_joint_zeta_KLEMS.csv"),
  row.names = FALSE
)


# Unit-Domar-weight zeta sanity check -------------------------------------

# This keeps the same aggregate target Ahat_cth as the main validation, but
# replaces every Domar weight by one before summing sector predictions:
#   Ahat_cth ~= sum_i 1 * (zeta * Xhat_i,cth + (zeta - 1) * phat_i,cth)
#
# Because the comparison remains at the aggregate level, this is no longer
# algebraically identical to the old sector-level zeta validation. It is a
# diagnostic for how much the Domar scaling matters in the aggregate fit.
unit_domar_sector_panel <- joint_sector_panel
unit_domar_sector_panel$unit_domar_weight <- 1
unit_domar_sector_panel$unit_zeta_slope <-
  unit_domar_sector_panel$yihat + unit_domar_sector_panel$real_pihat
unit_domar_sector_panel$unit_zeta_intercept <- -unit_domar_sector_panel$real_pihat

unit_agg_terms <- aggregate(
  unit_domar_sector_panel[, c("unit_zeta_slope", "unit_zeta_intercept", "unit_domar_weight")],
  unit_domar_sector_panel[group_vars],
  sum
)
names(unit_agg_terms)[names(unit_agg_terms) == "unit_zeta_slope"] <- "zeta_slope"
names(unit_agg_terms)[names(unit_agg_terms) == "unit_zeta_intercept"] <- "zeta_intercept"
names(unit_agg_terms)[names(unit_agg_terms) == "unit_domar_weight"] <- "unit_weight_sum"

unit_agg_actual <- aggregate(
  unit_domar_sector_panel$Ahat,
  unit_domar_sector_panel[group_vars],
  function(x) x[1]
)
names(unit_agg_actual)[names(unit_agg_actual) == "x"] <- "actual_Ahat"

unit_agg_n <- aggregate(
  unit_domar_sector_panel$sector,
  unit_domar_sector_panel[group_vars],
  function(x) length(unique(x))
)
names(unit_agg_n)[names(unit_agg_n) == "x"] <- "n_sector"

unit_domar_aggregate_panel <- Reduce(
  function(x, y) merge(x, y, by = group_vars),
  list(unit_agg_terms, unit_agg_actual, unit_agg_n)
)

unit_domar_aggregate_panel <- unit_domar_aggregate_panel[
  unit_domar_aggregate_panel$n_sector == unit_domar_aggregate_panel$expected_n,
]

unit_domar_samples <- list(
  "All horizons" = rep(TRUE, nrow(unit_domar_aggregate_panel)),
  "Long horizons (h >= 15)" =
    unit_domar_aggregate_panel$horizon >= long_horizon_cutoff
)

zeta_unit_domar_results <- do.call(rbind, lapply(names(unit_domar_samples), function(sample_name) {
  panel <- unit_domar_aggregate_panel[unit_domar_samples[[sample_name]],]
  out <- do.call(rbind, lapply(names(metrics), function(metric_name) {
    optimise_metric(panel, metric_name)
  }))
  out$sample <- sample_name
  out$domar_weight <- "unit weights"
  out
}))

zeta_unit_domar_results <- zeta_unit_domar_results[, c(
  "sample", "domar_weight", "metric", "zeta", "objective_log_points",
  "n_aggregate_obs", "mean_error_pp", "median_error_pp", "mae_log_points",
  "rmse_log_points"
)]

write.csv(
  zeta_unit_domar_results,
  file.path(tables_path, "tab_unit_domar_weight_aggregate_zeta_KLEMS.csv"),
  row.names = FALSE
)


# LaTeX table: joint and unit-weight zeta results --------------------------

# Compact manuscript table: first the main joint aggregate zeta results, then
# a separator row and the aggregate counterfactual where all lambdas are one.
display_sample <- function(x) {
  ifelse(x == "Long horizons (h >= 15)", "Long horizons ($h \\geq 15$)", x)
}

format_zeta_rows <- function(df) {
  paste0(
    display_sample(df$sample), " & ",
    df$metric, " & ",
    sprintf("%.3f", df$zeta), " \\\\"
  )
}

unit_domar_latex_rows <- zeta_unit_domar_results[
  zeta_unit_domar_results$metric %in% c("Mean absolute error", "Root mean square error"),
  c("sample", "metric", "zeta")
]

latex_lines <- c(
  "\\begin{longtable}{llr}",
  "\\caption{Values of $\\zeta$ minimising aggregate KLEMS hindcasting errors. The first panel uses observed initial Domar weights. The second panel is an aggregate counterfactual in which every Domar weight is set to one before summing sector predictions.} \\\\",
  "  \\hline",
  "Sample & Metric & $\\zeta$ \\\\",
  "  \\hline",
  format_zeta_rows(zeta_results[, c("sample", "metric", "zeta")]),
  "  \\hline",
  "\\multicolumn{3}{l}{\\emph{Aggregate counterfactual with all $\\lambda_i=1$}} \\\\",
  "  \\hline",
  format_zeta_rows(unit_domar_latex_rows),
  "  \\hline",
  "\\hline",
  "\\label{tab_joint_zeta_klems_unit_lambda}",
  "\\end{longtable}"
)

writeLines(
  latex_lines,
  con = file.path(tables_path, "tab_joint_zeta_KLEMS_with_unit_lambda.tex")
)


# Confidence interval for joint aggregate errors --------------------------

# Use the chosen long-horizon zeta for the point forecast, then build empirical
# aggregate prediction-error quantiles by horizon.
zeta_ci <- zeta_results$zeta[
  zeta_results$sample == ci_sample & zeta_results$metric == ci_metric
][1]

joint_aggregate_panel$pred_Ahat <- joint_aggregate_panel$zeta_intercept +
  zeta_ci * joint_aggregate_panel$zeta_slope
joint_aggregate_panel$error <- joint_aggregate_panel$pred_Ahat -
  joint_aggregate_panel$actual_Ahat

save(
  joint_sector_panel,
  joint_aggregate_panel,
  unit_domar_aggregate_panel,
  zeta_results,
  zeta_unit_domar_results,
  file = file.path(dataset_path, "df_joint_KLEMS_zeta_Domar_hindcast.RData")
)

summarise_ci <- function(x) {
  c(
    n = length(x),
    mean_error = mean(x),
    median_error = median(x),
    q10 = unname(quantile(x, 0.10)),
    q25 = unname(quantile(x, 0.25)),
    q75 = unname(quantile(x, 0.75)),
  q90 = unname(quantile(x, 0.90))
  )
}

# Empirical quantiles are observed up to the support of the KLEMS panel.
ci_empirical <- do.call(
  rbind,
  lapply(split(joint_aggregate_panel$error, joint_aggregate_panel$horizon), summarise_ci)
)
ci_empirical <- data.frame(
  horizon = as.integer(rownames(ci_empirical)),
  ci_empirical,
  row.names = NULL
)
ci_empirical <- ci_empirical[order(ci_empirical$horizon),]
ci_empirical$is_extrapolated <- FALSE


# Plot joint aggregate CI --------------------------------------------------

to_pct <- function(x) 100 * (exp(x) - 1)

# Convert log-point errors to percent only for display.
ci_plot <- ci_empirical
ci_plot$mean_error_pct <- to_pct(ci_plot$mean_error)
ci_plot$median_error_pct <- to_pct(ci_plot$median_error)
ci_plot$q10_pct <- to_pct(ci_plot$q10)
ci_plot$q25_pct <- to_pct(ci_plot$q25)
ci_plot$q75_pct <- to_pct(ci_plot$q75)
ci_plot$q90_pct <- to_pct(ci_plot$q90)

g_joint_KLEMS_CI <- 
  ci_plot%>%
  filter(horizon<= ci_empirical_cutoff)%>%
  ggplot(aes(x = horizon)) +
  geom_ribbon(
    aes(ymin = q10_pct, ymax = q90_pct, fill = "10-90 percentile"),
    alpha = 0.35
  ) +
  geom_ribbon(
    aes(ymin = q25_pct, ymax = q75_pct, fill = "25-75 percentile"),
    alpha = 0.35
  ) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = 3) +
  geom_line(aes(y = mean_error_pct, linetype = "Mean error"), linewidth = 0.9) +
  geom_line(aes(y = median_error_pct, linetype = "Median error"), linewidth = 0.9) +
  scale_fill_manual(
    values = c("10-90 percentile" = "lightblue", "25-75 percentile" = "navy")
  ) +
  scale_linetype_manual(values = c("Mean error" = "solid", "Median error" = "longdash")) +
  labs(
  #   title = sprintf("Joint KLEMS zeta-Domar confidence interval (zeta = %.3f)", zeta_ci),
  #   subtitle = sprintf(
  #     "Calibration: %s, %s\nDashed vertical line: linear extension after h = %s",
  #     ci_sample, ci_metric, ci_cutoff
  #   ),
    x = "Forecast horizon (years)",
    y = "Aggregate prediction error (%)",
    caption=sprintf("Joint KLEMS zeta-Domar confidence interval (zeta = %.3f)", zeta_ci),
    fill = "",
    linetype = ""
  ) +
  mytheme +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )
g_joint_KLEMS_CI

ggsave(
  filename = file.path(graph_path, "g_joint_KLEMS_CI.jpeg"),
  plot = g_joint_KLEMS_CI,
  width = 7,
  height = 4.7,
  dpi = 300
)


# Console summary ----------------------------------------------------------

cat("\nOptimal zeta, joint KLEMS aggregate hindcast\n")
print(zeta_results, row.names = FALSE)
cat("\nOptimal zeta, aggregate hindcast with all Domar weights set to one\n")
print(zeta_unit_domar_results, row.names = FALSE)
cat("\nSaved table: ", file.path(tables_path, "tab_joint_zeta_KLEMS.csv"), "\n", sep = "")
cat("Saved aggregate unit-weight zeta check: ", file.path(tables_path, "tab_unit_domar_weight_aggregate_zeta_KLEMS.csv"), "\n", sep = "")
cat("Saved LaTeX zeta table: ", file.path(tables_path, "tab_joint_zeta_KLEMS_with_unit_lambda.tex"), "\n", sep = "")
cat("Saved CI table: ", file.path(tables_path, "tab_joint_KLEMS_CI.csv"), "\n", sep = "")
cat("Saved CI graph: ", file.path(graph_path, "g_joint_KLEMS_CI.jpeg"), "\n", sep = "")
