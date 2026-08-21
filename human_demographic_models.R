suppressPackageStartupMessages({
  library(MASS)
})

fit_human_age_sex_models <- function(analysis_data) {
  model_data <- filter_human_age_gender(analysis_data) |>
    filter(
      !is.na(bite_count_raw),
      !is.na(rash_numeric),
      !is.na(temperature_c),
      !is.na(humidity_pct),
      !is.na(bite_month_label)
    )

  bite_model <- MASS::glm.nb(
    bite_count_raw ~ age_group * gender_clean + temperature_c + humidity_pct + bite_month_label,
    data = model_data
  )

  rash_model <- stats::glm(
    rash_numeric ~ age_group * gender_clean + temperature_c + humidity_pct + bite_month_label,
    data = model_data,
    family = stats::binomial()
  )

  list(
    bite_model = bite_model,
    rash_model = rash_model,
    model_data = model_data
  )
}

build_human_age_sex_reference_grid <- function(model_data) {
  reference_month <- names(which.max(table(model_data$bite_month_label)))[1]

  expand.grid(
    age_group = age_group_levels(),
    gender_clean = c("female", "male"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    mutate(
      age_group = factor(age_group, levels = age_group_levels(), ordered = TRUE),
      gender_clean = factor(gender_clean, levels = c("female", "male")),
      temperature_c = stats::median(model_data$temperature_c, na.rm = TRUE),
      humidity_pct = stats::median(model_data$humidity_pct, na.rm = TRUE),
      bite_month_label = factor(reference_month, levels = month_levels(), ordered = TRUE)
    )
}

summarise_human_age_sex_model_estimates <- function(analysis_data) {
  fitted_models <- fit_human_age_sex_models(analysis_data)
  reference_grid <- build_human_age_sex_reference_grid(fitted_models$model_data)

  bite_prediction <- stats::predict(
    fitted_models$bite_model,
    newdata = reference_grid,
    type = "link",
    se.fit = TRUE
  )
  bite_fit <- as.numeric(bite_prediction$fit)
  bite_se <- as.numeric(bite_prediction$se.fit)

  bite_estimates <- reference_grid |>
    transmute(
      outcome = "Mean bite burden",
      age_group,
      gender_clean,
      estimate = exp(bite_fit),
      conf_low = exp(bite_fit - (1.96 * bite_se)),
      conf_high = exp(bite_fit + (1.96 * bite_se))
    )

  rash_prediction <- stats::predict(
    fitted_models$rash_model,
    newdata = reference_grid,
    type = "link",
    se.fit = TRUE
  )
  rash_fit <- as.numeric(rash_prediction$fit)
  rash_se <- as.numeric(rash_prediction$se.fit)

  rash_estimates <- reference_grid |>
    transmute(
      outcome = "Rash probability",
      age_group,
      gender_clean,
      estimate = plogis(rash_fit),
      conf_low = plogis(rash_fit - (1.96 * rash_se)),
      conf_high = plogis(rash_fit + (1.96 * rash_se))
    )

  bind_rows(bite_estimates, rash_estimates) |>
    mutate(
      outcome = factor(outcome, levels = c("Mean bite burden", "Rash probability")),
      gender_clean = factor(gender_clean, levels = c("female", "male")),
      age_group = factor(as.character(age_group), levels = age_group_levels(), ordered = TRUE)
    )
}

summarise_human_age_sex_model_tests <- function(analysis_data) {
  fitted_models <- fit_human_age_sex_models(analysis_data)
  model_data <- fitted_models$model_data

  bite_reduced <- MASS::glm.nb(
    bite_count_raw ~ age_group + gender_clean + temperature_c + humidity_pct + bite_month_label,
    data = model_data
  )
  bite_lr <- stats::anova(bite_reduced, fitted_models$bite_model, test = "Chisq")

  rash_reduced <- stats::glm(
    rash_numeric ~ age_group + gender_clean + temperature_c + humidity_pct + bite_month_label,
    data = model_data,
    family = stats::binomial()
  )
  bite_loglik_full <- as.numeric(stats::logLik(fitted_models$bite_model))
  bite_loglik_reduced <- as.numeric(stats::logLik(bite_reduced))
  bite_df <- attr(stats::logLik(fitted_models$bite_model), "df") - attr(stats::logLik(bite_reduced), "df")
  bite_statistic <- 2 * (bite_loglik_full - bite_loglik_reduced)
  bite_p_value <- stats::pchisq(bite_statistic, df = bite_df, lower.tail = FALSE)

  rash_loglik_full <- as.numeric(stats::logLik(fitted_models$rash_model))
  rash_loglik_reduced <- as.numeric(stats::logLik(rash_reduced))
  rash_df <- attr(stats::logLik(fitted_models$rash_model), "df") - attr(stats::logLik(rash_reduced), "df")
  rash_statistic <- 2 * (rash_loglik_full - rash_loglik_reduced)
  rash_p_value <- stats::pchisq(rash_statistic, df = rash_df, lower.tail = FALSE)

  bind_rows(
    tibble::tibble(
      outcome = "Mean bite burden",
      comparison = "Age-by-sex interaction",
      df = bite_df,
      statistic = bite_statistic,
      p_value = bite_p_value,
      n_obs = stats::nobs(fitted_models$bite_model)
    ),
    tibble::tibble(
      outcome = "Rash probability",
      comparison = "Age-by-sex interaction",
      df = rash_df,
      statistic = rash_statistic,
      p_value = rash_p_value,
      n_obs = stats::nobs(fitted_models$rash_model)
    )
  )
}
