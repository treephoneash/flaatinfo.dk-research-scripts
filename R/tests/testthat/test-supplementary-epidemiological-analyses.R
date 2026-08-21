analysis_data <- build_analysis_table(
  load_tick_data(testthat::test_path("..", "..", "input.xlsx"))
)

test_that("supplementary epidemiological summaries are populated and numerically sane", {
  seasonality <- summarise_monthly_human_seasonality(analysis_data)
  distributions <- summarise_distribution_diagnostics(analysis_data)
  heatmap <- summarise_age_gender_heatmap(analysis_data)
  denmark_reports <- filter_denmark_reports(analysis_data)
  weather <- summarise_meteorological_associations(analysis_data)
  delay <- summarise_reporting_delay_data(analysis_data)

  expect_identical(as.character(seasonality$bite_month_label), month_levels())
  expect_true(all(seasonality$registrations >= 0))
  expect_true(all(seasonality$crawl_only_probability >= 0 & seasonality$crawl_only_probability <= 1, na.rm = TRUE))
  expect_true(all(seasonality$rash_probability >= 0 & seasonality$rash_probability <= 1, na.rm = TRUE))

  expect_gt(nrow(distributions), 0)
  expect_true(all(distributions$count_value >= 0))

  expect_gt(nrow(heatmap), 0)
  expect_true(all(heatmap$value >= 0, na.rm = TRUE))

  expect_gt(nrow(denmark_reports), 0)
  expect_true(all(!is.na(denmark_reports$latitude)))
  expect_true(all(!is.na(denmark_reports$longitude)))

  expect_gt(nrow(weather), 0)
  expect_true(all(weather$fit >= 0, na.rm = TRUE))
  expect_true(all(weather$lower <= weather$fit & weather$fit <= weather$upper, na.rm = TRUE))
  rash_weather <- weather[weather$outcome == "Rash probability", ]
  expect_true(all(rash_weather$upper <= 1, na.rm = TRUE))

  expect_gt(nrow(delay), 0)
  expect_true(all(delay$report_delay_days >= 0))
})

test_that("supplementary epidemiological figures and summary tables are written to disk", {
  output_dir <- file.path(tempdir(), "supplementary-epidemiological-analyses-test")
  manifest <- generate_supplementary_analysis_figures(analysis_data, output_dir = output_dir)

  expected_pngs <- file.path(output_dir, supplementary_analysis_figure_filenames())
  expected_csvs <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(supplementary_analysis_figure_filenames()), "-summary.csv")
  )

  expect_equal(nrow(manifest), 6)
  expect_true(all(file.exists(expected_pngs)))
  expect_true(all(file.exists(expected_csvs)))
  expect_true(file.exists(file.path(output_dir, "supplementary-epidemiological-analyses-manifest.csv")))
})
