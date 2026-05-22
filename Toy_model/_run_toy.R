options(width = 240)
local_lib <- file.path(getwd(), "R_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readxl); library(tibble); library(knitr)
})

script_path <- commandArgs(trailingOnly = FALSE)
file_arg <- script_path[grepl("--file=", script_path)]
toy_dir <- normalizePath(dirname(sub("--file=", "", file_arg)), winslash = "/", mustWork = TRUE)
old_wd <- getwd()
setwd(toy_dir)
on.exit(setwd(old_wd), add = TRUE)

src <- readLines(file.path(toy_dir, "_toy_model_clean_extracted.R"))
src <- sub("knitr::current_input\\(\\)", 'character(0)', src)
src <- src[!grepl("knitr::opts_knit", src)]
src <- src[!grepl("knitr::opts_chunk", src)]

tmp <- tempfile(fileext = ".R")
writeLines(src, tmp)
source(tmp, echo = FALSE)

cat("\n### Aggregate summary (log-growth) ###\n")
print(as.data.frame(aggregate_summary))

cat("\n### HH vs industry incidence ###\n")
print(as.data.frame(incidence_summary))

cat("\n### Transport sector TFP (HH vs industry) ###\n")
print(as.data.frame(transport_sector_comparison))

cat("\n### Sector-specific zeta by scenario ###\n")
zeta_table <- do.call(rbind, lapply(names(scenario_results), function(nm) {
  z <- scenario_results[[nm]]$model$zeta
  data.frame(scenario = nm, sector = names(z), zeta = round(as.numeric(z), 4))
}))
zeta_wide <- as.data.frame(tidyr::pivot_wider(zeta_table, names_from = "sector", values_from = "zeta"))
print(zeta_wide)

cat("\n### Checks summary ###\n")
print(as.data.frame(checks_summary |> mutate(passed = case_when(is.na(passed)~"n/a", passed~"pass", TRUE~"fail"))))
