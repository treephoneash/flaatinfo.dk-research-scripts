analysis_data <- build_analysis_table(
  load_tick_data(testthat::test_path("..", "..", "input.xlsx"))
)

test_that("landscape ecological summaries and model terms are populated", {
  landscape_outcomes <- summarise_landscape_human_outcomes(
    analysis_data,
    bootstrap_replicates = 200L,
    seed = 20260320L
  )
  bite_type_composition <- summarise_landscape_bite_type_composition(analysis_data)
  monthly_profile <- summarise_landscape_monthly_profile(analysis_data)
  model_coefficients <- summarise_landscape_model_coefficients(analysis_data)
  landscape_terms <- summarise_landscape_model_terms(analysis_data)
  sensitivity_terms <- summarise_landscape_sensitivity_model_terms(analysis_data)
  sensitivity_fit <- summarise_landscape_sensitivity_model_fit(analysis_data)

  expect_identical(as.character(landscape_outcomes$characterization_clean), setdiff(landscape_levels(), "unknown"))
  expect_true(all(landscape_outcomes$registrations > 0))
  expect_true(all(landscape_outcomes$mean_bite_low <= landscape_outcomes$mean_bite_high, na.rm = TRUE))
  expect_true(all(landscape_outcomes$rash_probability >= 0 & landscape_outcomes$rash_probability <= 1, na.rm = TRUE))
  expect_true(all(landscape_outcomes$crawl_only_probability >= 0 & landscape_outcomes$crawl_only_probability <= 1, na.rm = TRUE))

  expect_gt(nrow(bite_type_composition), 0)
  expect_true(all(bite_type_composition$proportion >= 0 & bite_type_composition$proportion <= 1))

  expect_gt(nrow(monthly_profile), 0)
  expect_true(all(monthly_profile$monthly_share >= 0 & monthly_profile$monthly_share <= 1))

  expect_gt(nrow(model_coefficients), 0)
  expect_gt(nrow(landscape_terms), 0)
  expect_true(all(landscape_terms$estimate > 0, na.rm = TRUE))
  expect_true(all(landscape_terms$conf.low <= landscape_terms$estimate & landscape_terms$estimate <= landscape_terms$conf.high, na.rm = TRUE))

  expect_gt(nrow(sensitivity_terms), 0)
  expect_true(all(sensitivity_terms$estimate > 0, na.rm = TRUE))
  expect_true(all(sensitivity_terms$conf.low <= sensitivity_terms$estimate & sensitivity_terms$estimate <= sensitivity_terms$conf.high, na.rm = TRUE))
  expect_gt(nrow(sensitivity_fit), 0)
  expect_true(all(sensitivity_fit$n_obs > 0))
  expect_true(all(is.finite(sensitivity_fit$aic)))
})

test_that("landscape ecological figures and model tables are written to disk", {
  output_dir <- file.path(tempdir(), "landscape-ecological-analyses-test")
  manifest <- generate_landscape_ecological_analyses(
    analysis_data,
    output_dir = output_dir,
    bootstrap_replicates = 250L,
    seed = 20260320L
  )

  expected_pngs <- file.path(output_dir, landscape_ecological_figure_filenames())
  expected_csvs <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(landscape_ecological_figure_filenames()), "-summary.csv")
  )

  expect_equal(nrow(manifest), 5)
  expect_true(all(file.exists(expected_pngs)))
  expect_true(all(file.exists(expected_csvs)))
  expect_true(file.exists(file.path(output_dir, "landscape-ecological-analyses-manifest.csv")))
  expect_true(file.exists(file.path(output_dir, "landscape_association_model_coefficients.csv")))
  expect_true(file.exists(file.path(output_dir, "landscape_association_landscape_terms.csv")))
  expect_true(file.exists(file.path(output_dir, "landscape_sensitivity_model_terms.csv")))
  expect_true(file.exists(file.path(output_dir, "landscape_sensitivity_model_fit_metrics.csv")))
})
