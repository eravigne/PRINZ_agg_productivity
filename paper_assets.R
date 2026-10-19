# Register of every figure and table in the PNAS main text and SI.
#
# `paper_file` is the path inside the paper (LaTeX) folder, `repo_file` the
# output written by `script`. Items with `repo_file = NA` are written by hand in
# the paper folder; `note` says where.
#
# Used by main.R (copy outputs into a paper folder) and PAPER_REPRODUCTION.Rmd
# (print the register and flag paper copies that differ from the repo outputs).

paper_assets <- read.csv(text = '
item,paper_file,repo_file,script,note
Fig. 1,figures/fig1.pdf,NA,NA,Compiled from fig1.tex in the paper folder; its panels are the four subfigures below
Fig. 1A panel,figures/subfigures_1/schematic_q.pdf,Schema/schematic_q.pdf,Schema/0_plot_schematic.R,
Fig. 1A panel,figures/subfigures_1/schematic_p.pdf,Schema/schematic_p.pdf,Schema/0_plot_schematic.R,
Fig. 1A panel,figures/subfigures_1/schematic_tfp.pdf,Schema/schematic_tfp.pdf,Schema/0_plot_schematic.R,
Fig. 1B,figures/subfigures_1/g_ribbon_intervals_KLEMS_bis.jpeg,Graphs/Zeta/g_ribbon_intervals_KLEMS_bis.jpeg,2.1 Hindcasting_zeta.Rmd,
Fig. 1C,figures/subfigures_1/g_error_KLEMS_hulten_quantiles_bis.jpeg,Graphs/Domar/g_error_KLEMS_hulten_quantiles_bis.jpeg,2.2 Hindcasting_Domar_agg.Rmd,
Table 1,NA,NA,NA,Cement table typed in 3-results-CCC.tex
Fig. 2,figures/g_composite_CCC.jpeg,Graphs/CCC/g_composite_CCC.jpeg,3.3_plot_CB7_CB6.R,
Fig. 3,figures/g_useful_energy.jpeg,Graphs/Energy/delivered_energy/g_useful_energy.jpeg,4_Energy_sector_focus_delivered_energy.Rmd,
Table 2,tables/tab_main5results_break_down_q_p.tex,Tables/Energy/delivered_energy/tab_main5results_break_down_q_p.tex,4_Energy_sector_focus_delivered_energy.Rmd,
Fig. S1,figures/g_error_hulten_quantiles.jpeg,Graphs/Domar/g_error_hulten_quantiles.jpeg,2.2 Hindcasting_Domar_agg.Rmd,
Fig. S2,figures/g_error_hulten_1st_vs_2nd_q1090.jpeg,Graphs/Domar/g_error_hulten_1st_vs_2nd_q1090.jpeg,2.2 Hindcasting_Domar_agg.Rmd,
Fig. S3,figures/g_hulten_support_intervals_tau25.jpeg,Graphs/Domar/g_hulten_support_intervals_tau25.jpeg,2.2 Hindcasting_Domar_agg.Rmd,
Fig. S4,figures/g_quantile_extension_Hulten_tau20.jpeg,Graphs/Domar/g_quantile_extension_Hulten_tau20.jpeg,2.2 Hindcasting_Domar_agg.Rmd,
Fig. S5,figures/g_zeta_support_intervals_tau25.jpeg,Graphs/Zeta/g_zeta_support_intervals_tau25.jpeg,2.1 Hindcasting_zeta.Rmd,
Fig. S6,figures/g_mae_rmse_by_dataset.jpeg,Graphs/Zeta/g_mae_rmse_by_dataset.jpeg,2.1 Hindcasting_zeta.Rmd,
Fig. S7,figures/g_ribbon_intervals_KLEMS_extended.jpeg,Graphs/Zeta/g_ribbon_intervals_KLEMS_extended.jpeg,2.1 Hindcasting_zeta.Rmd,
Fig. S8,figures/g_zeta_compare_2panel.jpeg,Graphs/Zeta/g_zeta_compare_2panel.jpeg,2.3 regressing_zeta.R,
Fig. S9,figures/g_joint_KLEMS_CI.jpeg,Graphs/Joint_hindcasting/g_joint_KLEMS_CI.jpeg,2.5_Hindcasting_joint_zeta_Domar_KLEMS.R,
Fig. S10,figures/g_cumAhat_CB6_CB7_allscenarios_comp.jpeg,Graphs/CCC/g_cumAhat_CB6_CB7_allscenarios_comp.jpeg,3.3_plot_CB7_CB6.R,
Fig. S11,figures/g_useful_5CCC.jpeg,Graphs/Energy/delivered_energy/g_useful_5CCC.jpeg,4_Energy_sector_focus_delivered_energy.Rmd,
Fig. S12,figures/g_useful_energy_all_way_prices.jpeg,Graphs/Energy/delivered_energy/g_useful_energy_all_way_prices.jpeg,4_Energy_sector_focus_delivered_energy.Rmd,
Table S1,tables/tab_indices_example.tex,NA,NA,Index-number example typed in the paper folder
Table S2,tables/tab_indices_examples_p_q.tex,NA,NA,Index-number example typed in the paper folder
Table S3,NA,Tables/Zeta/tab_zeta_calibration_code.tex,2.1 Hindcasting_zeta.Rmd,Values typed inline in A6-Empirical_validation.tex; compare with the repo file
Table S4,tables/tab_optimisation_zeta_3databases.tex,Tables/Zeta/tab_optimisation_zeta_3databases.tex,2.1 Hindcasting_zeta.Rmd,
Table S5,tables/regression_models_KLEMS_FE.tex,Tables/Zeta/regression_models_KLEMS_FE.tex,2.3 regressing_zeta.R,
Table S6,tables/tab_joint_zeta_KLEMS_with_unit_lambda.tex,Tables/Joint_hindcasting/tab_joint_zeta_KLEMS_with_unit_lambda.tex,2.5_Hindcasting_joint_zeta_Domar_KLEMS.R,
Table S7,tables/tab_CCC_CB6_CB7_TFP_opex_capex.tex,Tables/CCC/tab_CCC_CB6_CB7_TFP_opex_capex.tex,3.3_plot_CB7_CB6.R,
Table S8,tables/tab_conversion_factor_energy.tex,Tables/Energy/delivered_energy/tab_conversion_factor_energy.tex,4_Energy_sector_focus_delivered_energy.Rmd,
Table S9,tables/tab_summary_energy_tfp_scenarios.tex,Tables/Energy/delivered_energy/tab_summary_energy_tfp_scenarios.tex,4_Energy_sector_focus_delivered_energy.Rmd,
Table S10,tables/tab_way_unit_cost_compare.tex,Tables/Energy/delivered_energy/tab_way_unit_cost_compare.tex,4_Energy_sector_focus_delivered_energy.Rmd,
Table S11,tables/tab_main_overgeneration_sensitivity.tex,Tables/Energy/delivered_energy/tab_main_overgeneration_sensitivity.tex,4_Energy_sector_focus_delivered_energy.Rmd,
Table S12,tables/tab_storage_sensitivity.tex,Tables/Energy/delivered_energy/tab_storage_sensitivity.tex,4_Energy_sector_focus_delivered_energy.Rmd,
Table S13,tables/tab_sources_prices_energy.tex,NA,NA,Price sources typed in the paper folder
', stringsAsFactors = FALSE, na.strings = "NA", strip.white = TRUE)

paper_assets$note[is.na(paper_assets$note)] <- ""

# Table files are compared without comment lines (xtable timestamps) and
# whitespace; PDFs without their creation/modification dates; JPEGs by decoded
# pixels, ignoring re-encoding noise below 0.05 on the 0-1 scale.
same_asset <- function(a, b) {
  if (grepl("\\.tex$", a)) {
    norm <- function(f) {
      x <- readLines(f, warn = FALSE, encoding = "UTF-8")
      gsub("[[:space:]]", "", paste(x[!grepl("^%", x)], collapse = ""))
    }
    identical(norm(a), norm(b))
  } else if (grepl("\\.pdf$", a)) {
    norm <- function(f) {
      x <- readBin(f, "raw", file.size(f))
      s <- rawToChar(x[x != as.raw(0)])
      gsub("/(CreationDate|ModDate) *\\([^)]*\\)", "", s, useBytes = TRUE)
    }
    identical(norm(a), norm(b))
  } else if (identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))) {
    TRUE
  } else if (grepl("\\.jpe?g$", a) && requireNamespace("jpeg", quietly = TRUE)) {
    img_a <- jpeg::readJPEG(a)
    img_b <- jpeg::readJPEG(b)
    identical(dim(img_a), dim(img_b)) && max(abs(img_a - img_b)) < 0.05
  } else {
    FALSE
  }
}

# Status of each generated asset in `paper_dir` relative to the repo output.
check_paper_assets <- function(paper_dir, repo_root = ".") {
  generated <- paper_assets[!is.na(paper_assets$repo_file) & !is.na(paper_assets$paper_file), ]
  generated$status <- vapply(seq_len(nrow(generated)), function(i) {
    repo_f <- file.path(repo_root, generated$repo_file[i])
    paper_f <- file.path(paper_dir, generated$paper_file[i])
    if (!file.exists(repo_f)) return("repo output missing")
    if (!file.exists(paper_f)) return("missing in paper folder")
    if (same_asset(repo_f, paper_f)) "up to date" else "differs"
  }, character(1))
  generated[, c("item", "paper_file", "repo_file", "script", "status")]
}

# Copy every generated asset into `paper_dir` (figures/ and tables/).
sync_paper_assets <- function(paper_dir, repo_root = ".") {
  status <- check_paper_assets(paper_dir, repo_root)
  to_copy <- status[status$status %in% c("differs", "missing in paper folder"), ]
  for (i in seq_len(nrow(to_copy))) {
    dest <- file.path(paper_dir, to_copy$paper_file[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(repo_root, to_copy$repo_file[i]), dest, overwrite = TRUE)
  }
  cat(sprintf("Copied %d of %d generated paper assets into %s\n",
              nrow(to_copy), nrow(status), paper_dir))
  invisible(to_copy)
}
