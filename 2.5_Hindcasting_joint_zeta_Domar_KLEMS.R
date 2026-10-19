# Joint KLEMS hindcast: zeta mapping and Domar aggregation in one step.
#
# This script validates the combined equation directly:
#   Ahat_cth ~= sum_i lambda_i,t * (zeta * Xhat_i,cth + (zeta - 1) * phat_i,cth)
#
# It differs from the previous two-step hindcasts by choosing zeta from the
# aggregate prediction error after applying Domar weights. A second block moves
# from the sector-level baseline to this full-pipeline value one change at a
# time (sample, loss, weights, target) on a common sample.


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


# Same-sample steps from the sector-level baseline to the full pipeline ----

# The sector-level baseline (2.1 Hindcasting_zeta.Rmd) and the full-pipeline
# values above differ in four ways: sample, loss, weights and target. Each step
# below changes one of them, so consecutive rows isolate one shift in zeta:
#   (0) error of each sector prediction against its own TFP, all KLEMS obs
#   (a) as (0), restricted to the complete-coverage cells used above  -> sample
#   (b) sector errors summed within each cell, unit weights           -> loss
#   (c) sector errors summed within each cell, Domar weights          -> weights
#   (d) Domar-weighted prediction against observed aggregate TFP      -> target
# In (b) and (c) the target is reweighted together with the prediction, so the
# cell error is exactly sum_i w_i * (sector prediction error). Comparing an
# unweighted sum of sector predictions with aggregate TFP instead would add the
# gap sum_i Ahat_i - Ahat_agg, which is several times larger than Ahat_agg.

# (0) Baseline sample, built as in 2.1 Hindcasting_zeta.Rmd.
baseline_sector_panel <- df_KLEMS_growth_rate[
  is.finite(df_KLEMS_growth_rate$Aihat) &
    is.finite(df_KLEMS_growth_rate$yihat) &
    is.finite(df_KLEMS_growth_rate$pihat) &
    is.finite(df_KLEMS_growth_rate$hat_infl),
  c("horizon", "Aihat", "yihat", "pihat", "hat_infl")
]
baseline_real_pihat <- baseline_sector_panel$pihat - baseline_sector_panel$hat_infl

# (a)-(d) share one set of cells: the complete-coverage cells above in which
# every sector also has a finite industry TFP.
decomp_sector_panel <- merge(
  joint_sector_panel,
  joint_aggregate_panel[group_vars],
  by = group_vars
)
cell_has_tfp <- aggregate(
  list(all_finite = is.finite(decomp_sector_panel$industry_Ahat)),
  decomp_sector_panel[group_vars],
  all
)
decomp_cells <- cell_has_tfp[cell_has_tfp$all_finite, group_vars]
if (nrow(decomp_cells) < nrow(joint_aggregate_panel)) {
  warning("Some full-pipeline cells lack industry TFP; step (d) will differ from the main results.")
}
decomp_sector_panel <- merge(decomp_sector_panel, decomp_cells, by = group_vars)

decomp_slope <- decomp_sector_panel$yihat + decomp_sector_panel$real_pihat
decomp_weight <- decomp_sector_panel$domar_weight_initial
decomp_cell_panel <- aggregate(
  data.frame(
    unit_slope = decomp_slope,
    unit_intercept = -decomp_sector_panel$real_pihat,
    unit_target = decomp_sector_panel$industry_Ahat,
    domar_slope = decomp_weight * decomp_slope,
    domar_intercept = -decomp_weight * decomp_sector_panel$real_pihat,
    domar_target = decomp_weight * decomp_sector_panel$industry_Ahat
  ),
  decomp_sector_panel[group_vars],
  sum
)
decomp_cell_panel <- merge(
  decomp_cell_panel,
  joint_aggregate_panel[c(group_vars, "actual_Ahat")],
  by = group_vars
)

# Each step is written in the intercept + zeta * slope form used by
# prediction_error(), with the step's target in actual_Ahat.
zeta_step_panel <- function(horizon, slope, intercept, target) {
  data.frame(
    horizon = horizon,
    zeta_slope = slope,
    zeta_intercept = intercept,
    actual_Ahat = target
  )
}

decomposition_steps <- data.frame(
  step = c("(0)", "(a)", "(b)", "(c)", "(d)"),
  description = c(
    "Sector-level loss, full KLEMS sample",
    "Sector-level loss, complete-coverage sample",
    "Summed sector errors, unit weights",
    "Summed sector errors, Domar weights",
    "Full pipeline, observed aggregate TFP"
  ),
  change = c("", "sample", "loss", "weights", "target"),
  stringsAsFactors = FALSE
)

decomposition_panels <- list(
  "(0)" = zeta_step_panel(
    baseline_sector_panel$horizon,
    baseline_sector_panel$yihat + baseline_real_pihat,
    -baseline_real_pihat,
    baseline_sector_panel$Aihat
  ),
  "(a)" = zeta_step_panel(
    decomp_sector_panel$horizon,
    decomp_slope,
    -decomp_sector_panel$real_pihat,
    decomp_sector_panel$industry_Ahat
  ),
  "(b)" = zeta_step_panel(
    decomp_cell_panel$horizon,
    decomp_cell_panel$unit_slope,
    decomp_cell_panel$unit_intercept,
    decomp_cell_panel$unit_target
  ),
  "(c)" = zeta_step_panel(
    decomp_cell_panel$horizon,
    decomp_cell_panel$domar_slope,
    decomp_cell_panel$domar_intercept,
    decomp_cell_panel$domar_target
  ),
  "(d)" = zeta_step_panel(
    decomp_cell_panel$horizon,
    decomp_cell_panel$domar_slope,
    decomp_cell_panel$domar_intercept,
    decomp_cell_panel$actual_Ahat
  )
)

decomposition_metrics <- c("Mean absolute error", "Root mean square error")

zeta_decomposition_results <- do.call(rbind, lapply(decomposition_steps$step, function(step) {
  step_panel <- decomposition_panels[[step]]
  step_samples <- list(
    "All horizons" = rep(TRUE, nrow(step_panel)),
    "Long horizons (h >= 15)" = step_panel$horizon >= long_horizon_cutoff
  )
  do.call(rbind, lapply(names(step_samples), function(sample_name) {
    panel <- step_panel[step_samples[[sample_name]],]
    out <- do.call(rbind, lapply(decomposition_metrics, function(metric_name) {
      optimise_metric(panel, metric_name)
    }))
    out$sample <- sample_name
    out$step <- step
    out
  }))
}))
zeta_decomposition_results <- merge(
  zeta_decomposition_results,
  decomposition_steps,
  by = "step"
)
names(zeta_decomposition_results)[
  names(zeta_decomposition_results) == "n_aggregate_obs"
] <- "n_obs"

zeta_decomposition_results <- zeta_decomposition_results[
  order(zeta_decomposition_results$sample, zeta_decomposition_results$metric,
        zeta_decomposition_results$step),
  c("sample", "metric", "step", "description", "change", "zeta",
    "objective_log_points", "n_obs", "mean_error_pp", "median_error_pp",
    "mae_log_points", "rmse_log_points")
]

write.csv(
  zeta_decomposition_results,
  file.path(tables_path, "tab_joint_zeta_KLEMS_decomposition.csv"),
  row.names = FALSE
)


# LaTeX table: full-pipeline zeta and same-sample steps --------------------

# Panel A: full-pipeline zeta by metric, one column per horizon sample.
# Panel B: long-horizon steps (0)-(d), MAE and RMSE side by side.
long_sample <- "Long horizons (h >= 15)"

zeta_for <- function(df, sample_name, metric_name) {
  df$zeta[df$sample == sample_name & df$metric == metric_name][1]
}

panel_a_rows <- vapply(names(metrics), function(metric_name) {
  sprintf(
    "%s & %.3f & %.3f \\\\",
    metric_name,
    zeta_for(zeta_results, "All horizons", metric_name),
    zeta_for(zeta_results, long_sample, metric_name)
  )
}, character(1))

long_decomposition <- zeta_decomposition_results[
  zeta_decomposition_results$sample == long_sample,
]
panel_b_rows <- vapply(seq_len(nrow(decomposition_steps)), function(k) {
  step_rows <- long_decomposition[long_decomposition$step == decomposition_steps$step[k],]
  sprintf(
    "%s %s & %.3f & %.3f \\\\",
    decomposition_steps$step[k],
    decomposition_steps$description[k],
    zeta_for(step_rows, long_sample, "Mean absolute error"),
    zeta_for(step_rows, long_sample, "Root mean square error")
  )
}, character(1))

format_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
n_long <- function(step) {
  long_decomposition$n_obs[
    long_decomposition$step == step &
      long_decomposition$metric == "Mean absolute error"
  ][1]
}

table_caption <- paste0(
  "Values of $\\zeta$ minimising KLEMS backtesting errors. ",
  "Panel A: full-pipeline calibration, in which sector predictions are summed with observed initial Domar weights and compared with observed aggregate TFP. ",
  "Panel B: long-horizon steps from the sector-level baseline to the full pipeline, each changing one element of the previous row. ",
  "(0) is the baseline of Table~\\ref{tab:zeta_calibration_code}: each sector prediction is compared with that sector's TFP, pooling all KLEMS observations (",
  format_n(n_long("(0)")), "). ",
  "(a) applies the same loss to the country-year cells used in Panel A, in which every sector is observed (",
  format_n(n_long("(a)")), " sector observations in ", format_n(n_long("(b)")), " cells). ",
  "(b) sums sector prediction errors within each cell, i.e.\\ compares the unweighted sum of sector predictions with the unweighted sum of sector TFP. ",
  "(c) uses initial Domar weights for both sums. ",
  "(d) replaces the Domar-weighted sum of sector TFP by observed aggregate TFP, as in Panel A."
)

latex_lines <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\begin{tabular}{lrr}",
  "  \\hline",
  "\\multicolumn{3}{l}{\\emph{A. Full pipeline: Domar weights, observed aggregate TFP}} \\\\",
  "  \\hline",
  "Metric & All horizons & Long horizons ($h \\geq 15$) \\\\",
  "  \\hline",
  panel_a_rows,
  "  \\hline",
  "\\multicolumn{3}{l}{\\emph{B. From the sector-level baseline to the full pipeline, long horizons ($h \\geq 15$)}} \\\\",
  "  \\hline",
  "Step & MAE & RMSE \\\\",
  "  \\hline",
  panel_b_rows,
  "  \\hline",
  "\\end{tabular}",
  paste0("\\caption{", table_caption, "}"),
  "\\label{tab_joint_zeta_klems_unit_lambda}",
  "\\end{table}"
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
  decomp_cell_panel,
  zeta_results,
  zeta_decomposition_results,
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
ggsave(
  filename = file.path(graph_path, "g_joint_KLEMS_CI.pdf"),
  plot = g_joint_KLEMS_CI,
  width = 7,
  height = 4.7,
  device = cairo_pdf
)


# Console summary ----------------------------------------------------------

cat("\nOptimal zeta, joint KLEMS aggregate hindcast\n")
print(zeta_results, row.names = FALSE)
cat("\nOptimal zeta, same-sample steps from sector-level baseline to full pipeline\n")
print(zeta_decomposition_results[, c("sample", "metric", "step", "change", "zeta", "n_obs")], row.names = FALSE)
cat("\nSaved table: ", file.path(tables_path, "tab_joint_zeta_KLEMS.csv"), "\n", sep = "")
cat("Saved decomposition table: ", file.path(tables_path, "tab_joint_zeta_KLEMS_decomposition.csv"), "\n", sep = "")
cat("Saved LaTeX zeta table: ", file.path(tables_path, "tab_joint_zeta_KLEMS_with_unit_lambda.tex"), "\n", sep = "")
cat("Saved CI table: ", file.path(tables_path, "tab_joint_KLEMS_CI.csv"), "\n", sep = "")
cat("Saved CI graph: ", file.path(graph_path, "g_joint_KLEMS_CI.jpeg"), "\n", sep = "")
