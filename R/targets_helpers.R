normalize_existing_paths <- function(paths) {
  unique(normalizePath(paths, winslash = "/", mustWork = TRUE))
}

format_p_value_for_table <- function(x) {
  ifelse(
    is.na(x),
    "",
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

build_compact_landscape_table <- function(landscape_terms) {
  keep_rows <- (
    landscape_terms$model == "Mean bite burden" &
      landscape_terms$landscape_label %in% c("Agriculture vs urban", "Forest vs urban", "Unknown vs urban")
  ) |
    (
      landscape_terms$model == "Rash probability" &
        landscape_terms$landscape_label == "Forest vs urban"
    ) |
    (
      landscape_terms$model == "Crawl-only probability" &
        landscape_terms$landscape_label %in% c("Agriculture vs urban", "Unknown vs urban")
    )

  compact <- landscape_terms[keep_rows, c("model", "landscape_label", "estimate", "conf.low", "conf.high", "p.value")]
  names(compact) <- c("Outcome", "Contrast", "Estimate", "conf_low", "conf_high", "p_value")
  compact$Estimate <- sprintf("%.2f", compact$Estimate)
  compact$`95% CI` <- sprintf("%.2f to %.2f", compact$conf_low, compact$conf_high)
  compact$`p-value` <- format_p_value_for_table(compact$p_value)
  compact[c("Outcome", "Contrast", "Estimate", "95% CI", "p-value")]
}

build_interaction_tests_table <- function(interaction_tests) {
  data.frame(
    Outcome = interaction_tests$outcome,
    `Interaction df` = interaction_tests$df,
    `Likelihood-ratio statistic` = sprintf("%.2f", interaction_tests$statistic),
    `p-value` = format_p_value_for_table(interaction_tests$p_value),
    check.names = FALSE
  )
}

build_figure_inventory_table <- function() {
  inventory <- data.frame(
    Collection = c(
      "Primary results",
      "Supplementary analyses",
      "Landscape analyses"
    ),
    Folder = c(
      "primary_epidemiological_results",
      "supplementary_epidemiological_analyses",
      "landscape_ecological_analyses"
    ),
    `Manifest file` = c(
      "primary-epidemiological-results-manifest.csv",
      "supplementary-epidemiological-analyses-manifest.csv",
      "landscape-ecological-analyses-manifest.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  manifest_paths <- c(
    "figures/primary_epidemiological_results/primary-epidemiological-results-manifest.csv",
    "figures/supplementary_epidemiological_analyses/supplementary-epidemiological-analyses-manifest.csv",
    "figures/landscape_ecological_analyses/landscape-ecological-analyses-manifest.csv"
  )

  inventory$Figures <- vapply(
    manifest_paths,
    function(path) nrow(read.csv(path, stringsAsFactors = FALSE)),
    integer(1)
  )
  inventory[c("Collection", "Folder", "Manifest file", "Figures")]
}

primary_results_output_paths <- function(output_dir = file.path("figures", "primary_epidemiological_results")) {
  c(
    file.path(output_dir, primary_result_figure_filenames()),
    file.path(output_dir, paste0(tools::file_path_sans_ext(primary_result_figure_filenames()), "-summary.csv")),
    file.path(output_dir, "primary-epidemiological-results-manifest.csv"),
    file.path(output_dir, primary_result_table_filenames())
  )
}

supplementary_results_output_paths <- function(output_dir = file.path("figures", "supplementary_epidemiological_analyses")) {
  c(
    file.path(output_dir, supplementary_analysis_figure_filenames()),
    file.path(output_dir, paste0(tools::file_path_sans_ext(supplementary_analysis_figure_filenames()), "-summary.csv")),
    file.path(output_dir, "supplementary-epidemiological-analyses-manifest.csv")
  )
}

landscape_results_output_paths <- function(output_dir = file.path("figures", "landscape_ecological_analyses")) {
  c(
    file.path(output_dir, landscape_ecological_figure_filenames()),
    file.path(output_dir, paste0(tools::file_path_sans_ext(landscape_ecological_figure_filenames()), "-summary.csv")),
    file.path(output_dir, "landscape-ecological-analyses-manifest.csv"),
    file.path(output_dir, landscape_ecological_table_filenames())
  )
}

run_primary_results_target <- function(analysis_data) {
  manifest <- generate_primary_result_figures(analysis_data)
  logbook_path <- write_primary_result_logbook_entry(analysis_data, manifest)
  normalize_existing_paths(c(primary_results_output_paths(), logbook_path))
}

run_supplementary_results_target <- function(analysis_data) {
  manifest <- generate_supplementary_analysis_figures(analysis_data)
  logbook_path <- write_supplementary_analysis_logbook_entry(analysis_data, manifest)
  normalize_existing_paths(c(supplementary_results_output_paths(), logbook_path))
}

run_landscape_results_target <- function(analysis_data) {
  manifest <- generate_landscape_ecological_analyses(analysis_data)
  logbook_path <- write_landscape_ecological_logbook_entry(analysis_data, manifest)
  normalize_existing_paths(c(landscape_results_output_paths(), logbook_path))
}

write_manuscript_table_files <- function(
  landscape_terms,
  interaction_tests,
  output_dir = file.path("derived", "manuscript_tables")
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  landscape_table <- build_compact_landscape_table(landscape_terms)
  interaction_table <- build_interaction_tests_table(interaction_tests)
  figure_inventory <- build_figure_inventory_table()

  landscape_path <- file.path(output_dir, "landscape_effects_table.csv")
  interaction_path <- file.path(output_dir, "human_age_sex_interaction_tests_table.csv")
  inventory_path <- file.path(output_dir, "figure_inventory_table.csv")

  utils::write.csv(landscape_table, landscape_path, row.names = FALSE, na = "")
  utils::write.csv(interaction_table, interaction_path, row.names = FALSE, na = "")
  utils::write.csv(figure_inventory, inventory_path, row.names = FALSE, na = "")

  normalize_existing_paths(c(landscape_path, interaction_path, inventory_path))
}

run_academic_style_qc <- function() {
  source(file.path("R", "lint_academic_style.R"), local = new.env(parent = globalenv()))
  normalize_existing_paths(c(
    file.path("derived", "writing_qa", "academic_style_summary.csv"),
    file.path("derived", "writing_qa", "academic_style_issues.csv"),
    file.path("derived", "writing_qa", "academic_style_report.md")
  ))
}

run_thesis_build_script <- function() {
  output <- system2(
    "Rscript",
    args = c(file.path("R", "build_thesis_book.R")),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  normalize_existing_paths(c(
    file.path("thesis", "dmbg_thesis_complete.docx"),
    file.path("thesis", "dmbg_thesis_complete.pdf")
  ))
}
