## Cross-sector pairwise correlations in EU KLEMS
##
## Narrow version of 2.4_within_country_sector_dependence.R.
## This script produces only the diagnostics used in the appendix section on
## within-country pairwise sector correlations:
##   - tab_within_country_pairwise_summary.tex
##   - g_pairwise_corr_3vars_long_horizon.jpeg
library(here)
source(here("0_set_up.R"))


dataset_path <- file.path(base_path, "Datasets")
graph_path   <- file.path(base_path, "Graphs/Correlation")
tables_path  <- file.path(base_path, "Tables/Correlation")

dir.create(graph_path, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_path, recursive = TRUE, showWarnings = FALSE)

col_A  <- "#1b9e77"
col_pr <- "#d95f02"
col_X  <- "#7570b3"
col_gr <- "grey60"

main_horizon_min <- 15L
short_horizon    <- 1L
min_obs_pair     <- 8L

load_named <- function(path, name) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!exists(name, envir = env, inherits = FALSE)) {
    stop("Object '", name, "' not found in ", path)
  }
  get(name, envir = env, inherits = FALSE)
}

fmt <- function(x, digits = 2L) {
  ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
}

fmt_int <- function(x) {
  ifelse(is.na(x), "--", format(as.integer(x), big.mark = ",", scientific = FALSE))
}

write_latex_table <- function(df, file, caption, label, align = NULL) {
  if (is.null(align)) {
    align <- paste(rep("l", ncol(df)), collapse = "")
  }

  lines <- c(
    "\\begin{table}[t]",
    "\\centering",
    "\\small",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste(names(df), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    lines <- c(lines, paste(as.character(df[i, ]), collapse = " & "), "\\\\")
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(lines, file)
}

pairwise_within_country <- function(df, value_col, min_obs = min_obs_pair) {
  out_pair <- list()
  out_cty  <- list()

  for (cty in sort(unique(df$country))) {
    d <- df[df$country == cty, c("Year", "horizon", "sector", value_col)]
    names(d)[names(d) == value_col] <- "value"

    d <- d[is.finite(d$value), , drop = FALSE]
    if (nrow(d) == 0L) {
      next
    }

    sectors <- sort(unique(d$sector))
    time_id <- sort(unique(paste(d$Year, d$horizon, sep = "__")))

    mat <- matrix(
      NA_real_,
      nrow = length(time_id),
      ncol = length(sectors),
      dimnames = list(time_id, sectors)
    )

    idx <- paste(d$Year, d$horizon, sep = "__")
    mat[cbind(match(idx, time_id), match(d$sector, sectors))] <- d$value

    if (ncol(mat) < 2L) {
      next
    }

    pair_rows <- list()
    k <- 0L
    for (a in seq_len(ncol(mat) - 1L)) {
      for (b in seq.int(a + 1L, ncol(mat))) {
        ok <- is.finite(mat[, a]) & is.finite(mat[, b])
        n_ok <- sum(ok)
        if (n_ok >= min_obs) {
          corr <- suppressWarnings(stats::cor(mat[ok, a], mat[ok, b]))
          if (is.finite(corr)) {
            k <- k + 1L
            pair_rows[[k]] <- data.frame(
              country = cty,
              sector_i = colnames(mat)[a],
              sector_j = colnames(mat)[b],
              n_obs = n_ok,
              correlation = corr,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    if (length(pair_rows) > 0L) {
      cty_pairs <- do.call(rbind, pair_rows)
      out_pair[[length(out_pair) + 1L]] <- cty_pairs
      out_cty[[length(out_cty) + 1L]] <- data.frame(
        country = cty,
        n_pairs = nrow(cty_pairs),
        mean_corr = mean(cty_pairs$correlation, na.rm = TRUE),
        median_corr = stats::median(cty_pairs$correlation, na.rm = TRUE),
        p10_corr = unname(stats::quantile(cty_pairs$correlation, 0.10, na.rm = TRUE)),
        p90_corr = unname(stats::quantile(cty_pairs$correlation, 0.90, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    pairwise = if (length(out_pair) > 0L) do.call(rbind, out_pair) else data.frame(),
    country  = if (length(out_cty) > 0L) do.call(rbind, out_cty) else data.frame()
  )
}

demean_within_cyh <- function(df, value_col) {
  cell <- interaction(df$country, df$Year, df$horizon, drop = TRUE)
  df[[paste0(value_col, "_dm")]] <- df[[value_col]] -
    ave(df[[value_col]], cell, FUN = function(x) mean(x, na.rm = TRUE))
  df
}

summarise_pairwise <- function(x) {
  if (nrow(x) == 0L) {
    return(rep(NA_real_, 4L))
  }

  c(
    mean = mean(x$correlation, na.rm = TRUE),
    median = stats::median(x$correlation, na.rm = TRUE),
    p10 = unname(stats::quantile(x$correlation, 0.10, na.rm = TRUE)),
    p90 = unname(stats::quantile(x$correlation, 0.90, na.rm = TRUE))
  )
}

df_growth <- load_named(
  file.path(dataset_path, "df_KLEMS_growth_rate.RData"),
  "df_KLEMS_growth_rate"
)

## The appendix text describes one observation per single-letter NACE sector.
df <- df_growth[nchar(df_growth$sector) == 1L, , drop = FALSE]

df$pihat_real <- df$pihat - df$hat_infl

needed_cols <- c("country", "Year", "horizon", "sector", "Aihat", "pihat_real", "yihat")
df <- df[complete.cases(df[, needed_cols]), needed_cols]

df_long  <- df[df$horizon >= main_horizon_min, , drop = FALSE]
df_short <- df[df$horizon == short_horizon, , drop = FALSE]

vars <- list(
  list(var = "Aihat",      label = "Industry TFP $\\hat A_i$",              facet = "Industry TFP"),
  list(var = "pihat_real", label = "Real output price $\\hat p^r_i$",       facet = "Real output price"),
  list(var = "yihat",      label = "Real gross output $\\hat X_i$",         facet = "Real gross output")
)

samples <- list(
  list(name = paste0("h>=", main_horizon_min), df = df_long),
  list(name = paste0("h=", short_horizon),     df = df_short)
)

pair_summaries <- list()

for (v in vars) {
  for (s in samples) {
    d_raw <- s$df
    d_dm  <- demean_within_cyh(s$df, v$var)

    raw <- pairwise_within_country(d_raw, v$var)
    dm  <- pairwise_within_country(d_dm, paste0(v$var, "_dm"))

    pair_summaries[[length(pair_summaries) + 1L]] <- list(
      var_id = v$var,
      variable = v$label,
      facet = v$facet,
      sample = s$name,
      raw = raw,
      demeaned = dm
    )
  }
}

summary_rows <- list()
for (p in pair_summaries) {
  raw_s <- summarise_pairwise(p$raw$pairwise)
  dm_s  <- summarise_pairwise(p$demeaned$pairwise)

  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    Variable = p$variable,
    Sample = p$sample,
    `Country pairs` = sum(p$raw$country$n_pairs, na.rm = TRUE),
    `Mean corr (raw)` = fmt(raw_s[["mean"]]),
    `Med. corr (raw)` = fmt(raw_s[["median"]]),
    `P10 (raw)` = fmt(raw_s[["p10"]]),
    `P90 (raw)` = fmt(raw_s[["p90"]]),
    `Mean corr (demeaned)` = fmt(dm_s[["mean"]]),
    `Med. corr (demeaned)` = fmt(dm_s[["median"]]),
    `P10 (demeaned)` = fmt(dm_s[["p10"]]),
    `P90 (demeaned)` = fmt(dm_s[["p90"]]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

pairwise_summary <- do.call(rbind, summary_rows)
pairwise_summary$`Country pairs` <- fmt_int(pairwise_summary$`Country pairs`)

write.csv(
  pairwise_summary,
  file.path(tables_path, "tab_within_country_pairwise_summary.csv"),
  row.names = FALSE
)

write_latex_table(
  pairwise_summary,
  file = file.path(tables_path, "tab_within_country_pairwise_summary.tex"),
  caption = paste0(
    "Pooled within-country pairwise sector correlations in EU KLEMS, ",
    "single-letter NACE sectors."
  ),
  label = "tab_within_country_pairwise_summary",
  align = "llrrrrrrrrr"
)

picked <- Filter(
  function(p) p$sample == paste0("h>=", main_horizon_min),
  pair_summaries
)

density_df <- do.call(
  rbind,
  lapply(picked, function(p) {
    rbind(
      data.frame(
        variable = p$facet,
        series = "Raw",
        correlation = p$raw$pairwise$correlation,
        stringsAsFactors = FALSE
      ),
      data.frame(
        variable = p$facet,
        series = "Demeaned within cell",
        correlation = p$demeaned$pairwise$correlation,
        stringsAsFactors = FALSE
      )
    )
  })
)

density_df <- density_df[is.finite(density_df$correlation), , drop = FALSE]
density_df$variable <- factor(density_df$variable, levels = vapply(vars, `[[`, character(1L), "facet"))
density_df$series <- factor(density_df$series, levels = c("Raw", "Demeaned within cell"))

figure_file <- file.path(graph_path, "g_pairwise_corr_3vars_long_horizon.jpeg")

  g_pairwise_density <- ggplot2::ggplot(
    density_df,
    ggplot2::aes(x = correlation, colour = series, fill = series)
  ) +
    ggplot2::geom_density(alpha = 0.20, linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, colour = col_gr, linewidth = 0.4) +
    ggplot2::facet_wrap(ggplot2::vars(variable), nrow = 1L) +
    ggplot2::coord_cartesian(xlim = c(-1, 1)) +
    ggplot2::scale_colour_manual(values = c("Raw" = col_A, "Demeaned within cell" = col_pr)) +
    ggplot2::scale_fill_manual(values = c("Raw" = col_A, "Demeaned within cell" = col_pr)) +
    ggplot2::labs(
      title = paste0(
        "Pairwise within-country sector correlations, KLEMS long-horizon ",
        "(h >= ", main_horizon_min, ")"
      ),
      x = "Pairwise correlation",
      y = "Density",
      colour = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "plain"),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "plain")
    )

  ggplot2::ggsave(
    filename = figure_file,
    plot = g_pairwise_density,
    width = 11.25,
    height = 5,
    units = "in",
    dpi = 320
  )


cat("\nWrote pairwise correlation outputs:\n")
cat(" - ", file.path(tables_path, "tab_within_country_pairwise_summary.tex"), "\n", sep = "")
cat(" - ", figure_file, "\n", sep = "")
