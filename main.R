#!/usr/bin/env Rscript

# ---- 0) Strict, deterministic setup
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressPackageStartupMessages({
  if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
  renv::restore()  # installs exact package versions from renv.lock
})

# ---- 1) Folders
dir.create("Datasets",   recursive = TRUE, showWarnings = FALSE)
dir.create("figures",   recursive = TRUE, showWarnings = FALSE)
dir.create("output",    recursive = TRUE, showWarnings = FALSE)

# ---- 2) Source your functions
# source("code/functions.R")
# source("code/01_download.R")
# source("code/02_clean.R")
# source("code/03_estimate_zeta.R")
# source("code/04_figures_tables.R")

# Example placeholders:
# download_data()             # writes into data_raw/
# clean_data()                # writes into data_clean/
# results <- estimate_zeta()  # returns list/data.frame
# make_figures(results)       # writes into figures/
# write.csv(results, "output/zeta_results.csv", row.names = FALSE)

# ---- 3) (Optional) render appendix Rmd to HTML/PDF
if (file.exists("Hindcasting_zeta.Rmd")) {
  rmarkdown::render("Hindcasting_zeta.Rmd",
                    output_format = "html_document",
                    output_file = "output/Hindcasting_zeta.html",
                    clean = TRUE, envir = new.env())
}

# ---- 4) Session info for audit trail
if (requireNamespace("sessioninfo", quietly = TRUE)) {
  writeLines(capture.output(sessioninfo::session_info()),
             con = "output/session_info.txt")
} else {
  writeLines(capture.output(sessionInfo()),
             con = "output/session_info.txt")
}