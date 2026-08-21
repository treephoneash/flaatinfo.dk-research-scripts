test_that("parse_tick_count handles supported values and malformed input", {
  values <- c("false", "true", "true (7)", NA, "weird", "12", TRUE, FALSE)
  expected <- c(0L, 1L, 7L, NA_integer_, NA_integer_, 12L, 1L, 0L)

  expect_identical(parse_tick_count(values), expected)
})

test_that("bin_age_group respects the locked age boundaries", {
  ages <- c(0, 9, 10, 79, 80, 90, 109, -1, NA)
  expected <- c("0-9", "0-9", "10-19", "70-79", "80+", "80+", "80+", NA, NA)

  expect_identical(as.character(bin_age_group(ages)), expected)
})

test_that("build_analysis_table preserves the cleaned workbook contract", {
  raw_data <- tibble::tibble(
    bite_type = c("human", "pet"),
    country = c("DK", "SE"),
    characterization = c("urban", "forest"),
    age = c("44", "-"),
    gender = c("male", "-"),
    crawl = c("true (2)", "false"),
    bite = c("false", "true (3)"),
    rash = c(TRUE, FALSE),
    comments = c("a", "b"),
    bite_date = c("2025-12-10", "2025-11-01"),
    registration_date = c("2025-12-12 09:39", "2025-11-03 10:15"),
    temperature = c(7.4, 11.2),
    humidity = c(87, 75)
  )

  analysis_data <- build_analysis_table(raw_data)

  expect_true(all(c("bite_type", "age", "gender", "crawl", "bite", "rash", "comments") %in% names(analysis_data)))
  expect_true(all(c(
    "bite_type_clean",
    "country_clean",
    "gender_clean",
    "rash_flag",
    "rash_numeric",
    "bite_count_raw",
    "crawl_count_raw",
    "bite_count_winsorized",
    "crawl_count_winsorized",
    "age_years",
    "age_group",
    "temperature_c",
    "humidity_pct",
    "bite_date_parsed",
    "registration_datetime",
    "registration_date_parsed",
    "bite_month_number",
    "bite_month_label",
    "report_delay_days",
    "is_crawl_only"
  ) %in% names(analysis_data)))
  expect_identical(analysis_data$bite_count_raw, c(0L, 3L))
  expect_identical(analysis_data$crawl_count_raw, c(2L, 0L))
  expect_identical(analysis_data$country_clean, c("DK", "SE"))
  expect_identical(analysis_data$rash_numeric, c(1L, 0L))
  expect_identical(analysis_data$bite_month_number, c(12L, 11L))
  expect_identical(as.character(analysis_data$bite_month_label), c("Dec", "Nov"))
  expect_identical(analysis_data$report_delay_days, c(2, 2))
  expect_equal(attr(analysis_data, "bite_cap_p99"), 2.97, tolerance = 1e-8)
  expect_equal(attr(analysis_data, "crawl_cap_p99"), 1.98, tolerance = 1e-8)
})
