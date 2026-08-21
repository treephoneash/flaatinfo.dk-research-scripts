analysis_data <- build_analysis_table(
  load_tick_data(testthat::test_path("..", "..", "input.xlsx"))
)

test_that("summary tables are populated and stay within valid bounds", {
  mean_by_age_gender <- summarise_mean_bite_by_age_gender(analysis_data)
  mean_by_age <- summarise_mean_bite_by_age(analysis_data, bootstrap_replicates = 200L, seed = 20260320L)
  interaction_estimates <- summarise_human_age_sex_model_estimates(analysis_data)
  interaction_tests <- summarise_human_age_sex_model_tests(analysis_data)
  rash_by_age <- summarise_rash_by_age(analysis_data)
  rash_by_age_gender <- summarise_rash_by_age_gender(analysis_data)
  crawl_only <- summarise_crawl_only_probability(analysis_data)
  registrations <- summarise_registrations_age_gender(analysis_data)

  expect_gt(nrow(mean_by_age_gender), 0)
  expect_true(all(mean_by_age_gender$mean_bite_count >= 0, na.rm = TRUE))
  expect_gt(nrow(mean_by_age), 0)
  expect_true(all(mean_by_age$mean_bite_count >= 0, na.rm = TRUE))
  expect_true(all(mean_by_age$conf_low <= mean_by_age$conf_high, na.rm = TRUE))
  expect_gt(nrow(interaction_estimates), 0)
  expect_true(all(interaction_estimates$conf_low <= interaction_estimates$conf_high, na.rm = TRUE))
  expect_equal(nrow(interaction_tests), 2)
  expect_true(all(interaction_tests$p_value >= 0 & interaction_tests$p_value <= 1, na.rm = TRUE))

  expect_identical(as.character(rash_by_age$age_group), age_group_levels())
  expect_true(all(rash_by_age$probability >= 0 & rash_by_age$probability <= 1, na.rm = TRUE))
  expect_true(all(rash_by_age$lower <= rash_by_age$upper, na.rm = TRUE))

  expect_gt(nrow(rash_by_age_gender), 0)
  expect_true(all(rash_by_age_gender$probability >= 0 & rash_by_age_gender$probability <= 1, na.rm = TRUE))

  expect_identical(as.character(crawl_only$bite_type_clean), c("human", "pet"))
  expect_true(all(crawl_only$probability >= 0 & crawl_only$probability <= 1, na.rm = TRUE))
  expect_true(all(crawl_only$lower <= crawl_only$upper, na.rm = TRUE))

  expect_true(all(registrations$absolute_registrations >= 0))
  expect_true(all(registrations$signed_registrations[registrations$gender_clean == "female"] >= 0))
  expect_true(all(registrations$signed_registrations[registrations$gender_clean == "male"] <= 0))
})

test_that("primary epidemiological result figures and summary tables are written to disk", {
  output_dir <- file.path(tempdir(), "primary-epidemiological-results-test")
  manifest <- generate_primary_result_figures(
    analysis_data,
    output_dir = output_dir,
    bootstrap_replicates = 250L,
    seed = 20260319L
  )

  expected_pngs <- file.path(output_dir, primary_result_figure_filenames())
  expected_csvs <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(primary_result_figure_filenames()), "-summary.csv")
  )
  legacy_pngs <- file.path(output_dir, legacy_primary_result_figure_filenames())
  legacy_csvs <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(legacy_primary_result_figure_filenames()), "-summary.csv")
  )

  expect_equal(nrow(manifest), 8)
  expect_true(all(file.exists(expected_pngs)))
  expect_true(all(file.exists(expected_csvs)))
  expect_true(file.exists(file.path(output_dir, "primary-epidemiological-results-manifest.csv")))
  expect_true(file.exists(file.path(output_dir, "human_age_sex_interaction_model_tests.csv")))
  expect_false(any(file.exists(legacy_pngs)))
  expect_false(any(file.exists(legacy_csvs)))
})
