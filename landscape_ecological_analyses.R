suppressPackageStartupMessages({
  library(broom)
  library(MASS)
})

landscape_levels <- function() {
  c("urban", "agriculture", "forest", "unknown")
}

landscape_ecological_figure_filenames <- function() {
  c(
    "landscape_associations_with_human_tick_outcomes.png",
    "landscape_bite_type_composition.png",
    "monthly_reporting_profile_by_landscape.png",
    "adjusted_landscape_effect_estimates.png",
    "landscape_sensitivity_effect_estimates.png"
  )
}

landscape_ecological_table_filenames <- function() {
  c(
    "landscape_association_model_coefficients.csv",
    "landscape_association_landscape_terms.csv",
    "landscape_sensitivity_model_terms.csv",
    "landscape_sensitivity_model_fit_metrics.csv"
  )
}

cleanup_landscape_ecological_outputs <- function(
  output_dir = file.path("figures", "landscape_ecological_analyses")
) {
  cleanup_managed_outputs(
    output_dir = output_dir,
    png_filenames = landscape_ecological_figure_filenames(),
    manifest_filenames = c(
      "landscape-ecological-analyses-manifest.csv",
      landscape_ecological_table_filenames()
    )
  )
}

filter_landscape_records <- function(analysis_data, include_unknown = TRUE) {
  valid_levels <- if (include_unknown) landscape_levels() else setdiff(landscape_levels(), "unknown")
  analysis_data |>
    filter(
      country_clean == "DK",
      characterization_clean %in% valid_levels
    ) |>
    mutate(
      characterization_clean = factor(
        characterization_clean,
        levels = valid_levels
      )
    )
}

filter_landscape_human_demographic_records <- function(analysis_data, include_unknown = TRUE) {
  filter_landscape_records(analysis_data, include_unknown = include_unknown) |>
    filter(
      bite_type_clean == "human",
      gender_clean %in% c("female", "male"),
      !is.na(age_group),
      !is.na(temperature_c),
      !is.na(humidity_pct),
      !is.na(bite_month_label)
    ) |>
    mutate(
      age_group = factor(as.character(age_group), levels = age_group_levels(), ordered = TRUE),
      gender_clean = factor(gender_clean, levels = c("female", "male"))
    )
}

summarise_landscape_human_outcomes <- function(
  analysis_data,
  bootstrap_replicates = 2000L,
  seed = 20260320L,
  include_unknown = FALSE
) {
  human_data <- filter_landscape_human_demographic_records(analysis_data, include_unknown = include_unknown)

  outcome_summary <- human_data |>
    group_by(characterization_clean) |>
    summarise(
      registrations = dplyr::n(),
      mean_bite_count = mean(bite_count_winsorized, na.rm = TRUE),
      rash_cases = sum(rash_flag, na.rm = TRUE),
      rash_probability = mean(rash_flag, na.rm = TRUE),
      crawl_only_cases = sum(is_crawl_only, na.rm = TRUE),
      crawl_only_probability = mean(is_crawl_only, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(characterization_clean)

  mean_ci <- lapply(
    seq_len(nrow(outcome_summary)),
    function(index) {
      current_values <- human_data |>
        filter(characterization_clean == outcome_summary$characterization_clean[[index]]) |>
        pull(bite_count_winsorized)

      bootstrap_mean_ci(
        current_values,
        bootstrap_replicates = bootstrap_replicates,
        seed = seed + index
      )
    }
  )

  rash_ci <- wilson_ci(outcome_summary$rash_cases, outcome_summary$registrations)
  crawl_ci <- wilson_ci(outcome_summary$crawl_only_cases, outcome_summary$registrations)

  outcome_summary$mean_bite_low <- vapply(mean_ci, `[[`, numeric(1), 1)
  outcome_summary$mean_bite_high <- vapply(mean_ci, `[[`, numeric(1), 2)
  outcome_summary$rash_low <- rash_ci$lower
  outcome_summary$rash_high <- rash_ci$upper
  outcome_summary$crawl_low <- crawl_ci$lower
  outcome_summary$crawl_high <- crawl_ci$upper
  outcome_summary
}

summarise_landscape_bite_type_composition <- function(analysis_data, include_unknown = FALSE) {
  filter_landscape_records(analysis_data, include_unknown = include_unknown) |>
    filter(bite_type_clean %in% c("human", "pet")) |>
    count(characterization_clean, bite_type_clean, name = "registrations") |>
    group_by(characterization_clean) |>
    mutate(
      proportion = registrations / sum(registrations)
    ) |>
    ungroup() |>
    mutate(
      bite_type_clean = factor(bite_type_clean, levels = c("human", "pet"))
    ) |>
    arrange(characterization_clean, bite_type_clean)
}

summarise_landscape_monthly_profile <- function(analysis_data, include_unknown = FALSE) {
  valid_levels <- if (include_unknown) landscape_levels() else setdiff(landscape_levels(), "unknown")

  filter_landscape_records(analysis_data, include_unknown = include_unknown) |>
    filter(
      bite_type_clean == "human",
      !is.na(bite_month_label)
    ) |>
    count(characterization_clean, bite_month_label, name = "registrations") |>
    group_by(characterization_clean) |>
    mutate(
      monthly_share = registrations / sum(registrations)
    ) |>
    ungroup() |>
    tidyr::complete(
      characterization_clean = factor(valid_levels, levels = valid_levels),
      bite_month_label = factor(month_levels(), levels = month_levels(), ordered = TRUE),
      fill = list(registrations = 0L, monthly_share = 0)
    ) |>
    arrange(characterization_clean, bite_month_label)
}

format_p_value_label <- function(p_value) {
  if (is.na(p_value)) {
    return("p = NA")
  }
  if (p_value < 0.001) {
    return("p < 0.001")
  }
  sprintf("p = %.3f", p_value)
}

categorical_landscape_pvalue <- function(data, outcome_column) {
  contingency <- table(data$characterization_clean, data[[outcome_column]])
  chi_fit <- suppressWarnings(stats::chisq.test(contingency, correct = FALSE))
  expected <- chi_fit$expected

  if (any(expected < 5)) {
    suppressWarnings(stats::chisq.test(contingency, simulate.p.value = TRUE, B = 5000)$p.value)
  } else {
    chi_fit$p.value
  }
}

compute_landscape_outcome_pvalues <- function(analysis_data) {
  human_data <- filter_landscape_human_demographic_records(analysis_data, include_unknown = FALSE)

  list(
    mean_bite = stats::kruskal.test(bite_count_winsorized ~ characterization_clean, data = human_data)$p.value,
    rash = categorical_landscape_pvalue(human_data, "rash_flag"),
    crawl_only = categorical_landscape_pvalue(human_data, "is_crawl_only")
  )
}

compute_landscape_bite_type_composition_pvalue <- function(analysis_data) {
  composition_data <- filter_landscape_records(analysis_data, include_unknown = FALSE) |>
    filter(bite_type_clean %in% c("human", "pet"))
  categorical_landscape_pvalue(composition_data, "bite_type_clean")
}

fit_landscape_association_models_variant <- function(
  analysis_data,
  count_response = "bite_count_winsorized",
  include_unknown = TRUE
) {
  human_data <- filter_landscape_human_demographic_records(analysis_data, include_unknown = include_unknown) |>
    filter(!is.na(.data[[count_response]]))

  all_landscape_data <- filter_landscape_records(analysis_data, include_unknown = include_unknown) |>
    filter(
      bite_type_clean %in% c("human", "pet"),
      !is.na(temperature_c),
      !is.na(humidity_pct),
      !is.na(bite_month_label)
    ) |>
    mutate(
      bite_type_clean = factor(bite_type_clean, levels = c("human", "pet"))
    )

  bite_formula <- stats::as.formula(
    paste0(count_response, " ~ characterization_clean + age_group + gender_clean + temperature_c + humidity_pct + bite_month_label")
  )
  bite_model <- MASS::glm.nb(bite_formula, data = human_data)

  rash_model <- stats::glm(
    rash_numeric ~ characterization_clean + age_group + gender_clean + temperature_c + humidity_pct + bite_month_label,
    data = human_data,
    family = stats::binomial()
  )

  crawl_model <- stats::glm(
    is_crawl_only ~ characterization_clean + bite_type_clean + temperature_c + humidity_pct + bite_month_label,
    data = all_landscape_data,
    family = stats::binomial()
  )

  list(
    bite_model = bite_model,
    rash_model = rash_model,
    crawl_model = crawl_model
  )
}

fit_landscape_association_models <- function(analysis_data) {
  fit_landscape_association_models_variant(
    analysis_data = analysis_data,
    count_response = "bite_count_raw",
    include_unknown = FALSE
  )
}

summarise_landscape_model_coefficients <- function(analysis_data) {
  fitted_models <- fit_landscape_association_models(analysis_data)

  bind_rows(
    broom::tidy(fitted_models$bite_model, conf.int = TRUE, exponentiate = TRUE) |>
      mutate(model = "Mean bite burden", effect_type = "IRR"),
    broom::tidy(fitted_models$rash_model, conf.int = TRUE, exponentiate = TRUE) |>
      mutate(model = "Rash probability", effect_type = "OR"),
    broom::tidy(fitted_models$crawl_model, conf.int = TRUE, exponentiate = TRUE) |>
      mutate(model = "Crawl-only probability", effect_type = "OR")
  ) |>
    relocate(model, effect_type, .before = term)
}

summarise_landscape_model_terms <- function(analysis_data) {
  summarise_landscape_model_coefficients(analysis_data) |>
    filter(grepl("^characterization_clean", term)) |>
    mutate(
      landscape_level = sub("^characterization_clean", "", term),
      landscape_label = dplyr::case_when(
        landscape_level == "agriculture" ~ "Agriculture vs urban",
        landscape_level == "forest" ~ "Forest vs urban",
        landscape_level == "unknown" ~ "Unknown vs urban",
        TRUE ~ landscape_level
      ),
      model = factor(
        model,
        levels = c("Mean bite burden", "Rash probability", "Crawl-only probability")
      ),
      landscape_label = factor(
        landscape_label,
        levels = c("Agriculture vs urban", "Forest vs urban", "Unknown vs urban")
      )
    ) |>
    arrange(model, landscape_label)
}

fit_landscape_sensitivity_models <- function(analysis_data) {
  count_specs <- tibble::tribble(
    ~variant_id, ~variant_label, ~count_response, ~include_unknown,
    "winsorized_all", "Winsorized, all landscape classes", "bite_count_winsorized", TRUE,
    "raw_all", "Raw, all landscape classes", "bite_count_raw", TRUE,
    "winsorized_no_unknown", "Winsorized, excluding unknown", "bite_count_winsorized", FALSE,
    "raw_no_unknown", "Raw, excluding unknown", "bite_count_raw", FALSE
  )

  binary_specs <- tibble::tribble(
    ~variant_id, ~variant_label, ~include_unknown,
    "all_classes", "All landscape classes", TRUE,
    "exclude_unknown", "Excluding unknown", FALSE
  )

  count_models <- lapply(
    seq_len(nrow(count_specs)),
    function(index) {
      spec <- count_specs[index, ]
      list(
        variant_id = spec$variant_id[[1]],
        variant_label = spec$variant_label[[1]],
        model_bundle = fit_landscape_association_models_variant(
          analysis_data = analysis_data,
          count_response = spec$count_response[[1]],
          include_unknown = spec$include_unknown[[1]]
        )
      )
    }
  )

  rash_models <- lapply(
    seq_len(nrow(binary_specs)),
    function(index) {
      spec <- binary_specs[index, ]
      list(
        variant_id = spec$variant_id[[1]],
        variant_label = spec$variant_label[[1]],
        model_bundle = fit_landscape_association_models_variant(
          analysis_data = analysis_data,
          count_response = "bite_count_winsorized",
          include_unknown = spec$include_unknown[[1]]
        )
      )
    }
  )

  crawl_models <- rash_models

  list(
    count_models = count_models,
    rash_models = rash_models,
    crawl_models = crawl_models
  )
}

summarise_landscape_sensitivity_model_terms <- function(analysis_data) {
  sensitivity_models <- fit_landscape_sensitivity_models(analysis_data)

  count_terms <- bind_rows(lapply(
    sensitivity_models$count_models,
    function(entry) {
      broom::tidy(entry$model_bundle$bite_model, conf.int = TRUE, exponentiate = TRUE) |>
        mutate(
          outcome = "Mean bite burden",
          effect_type = "IRR",
          variant_id = entry$variant_id,
          variant_label = entry$variant_label
        )
    }
  ))

  rash_terms <- bind_rows(lapply(
    sensitivity_models$rash_models,
    function(entry) {
      broom::tidy(entry$model_bundle$rash_model, conf.int = TRUE, exponentiate = TRUE) |>
        mutate(
          outcome = "Rash probability",
          effect_type = "OR",
          variant_id = entry$variant_id,
          variant_label = entry$variant_label
        )
    }
  ))

  crawl_terms <- bind_rows(lapply(
    sensitivity_models$crawl_models,
    function(entry) {
      broom::tidy(entry$model_bundle$crawl_model, conf.int = TRUE, exponentiate = TRUE) |>
        mutate(
          outcome = "Crawl-only probability",
          effect_type = "OR",
          variant_id = entry$variant_id,
          variant_label = entry$variant_label
        )
    }
  ))

  bind_rows(count_terms, rash_terms, crawl_terms) |>
    filter(grepl("^characterization_clean", term)) |>
    mutate(
      landscape_level = sub("^characterization_clean", "", term),
      landscape_label = dplyr::case_when(
        landscape_level == "agriculture" ~ "Agriculture vs urban",
        landscape_level == "forest" ~ "Forest vs urban",
        landscape_level == "unknown" ~ "Unknown vs urban",
        TRUE ~ landscape_level
      ),
      outcome = factor(outcome, levels = c("Mean bite burden", "Rash probability", "Crawl-only probability"))
    ) |>
    arrange(outcome, variant_label, landscape_label)
}

summarise_landscape_sensitivity_model_fit <- function(analysis_data) {
  sensitivity_models <- fit_landscape_sensitivity_models(analysis_data)

  extract_fit <- function(model, outcome, variant_label) {
    data.frame(
      outcome = outcome,
      variant_label = variant_label,
      n_obs = stats::nobs(model),
      aic = stats::AIC(model),
      stringsAsFactors = FALSE
    )
  }

  bind_rows(
    lapply(sensitivity_models$count_models, function(entry) extract_fit(entry$model_bundle$bite_model, "Mean bite burden", entry$variant_label)),
    lapply(sensitivity_models$rash_models, function(entry) extract_fit(entry$model_bundle$rash_model, "Rash probability", entry$variant_label)),
    lapply(sensitivity_models$crawl_models, function(entry) extract_fit(entry$model_bundle$crawl_model, "Crawl-only probability", entry$variant_label))
  )
}

generate_landscape_ecological_analyses <- function(
  analysis_data,
  output_dir = file.path("figures", "landscape_ecological_analyses"),
  bootstrap_replicates = 2000L,
  seed = 20260320L
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cleanup_landscape_ecological_outputs(output_dir)

  palette_landscape <- c(
    urban = "#355C7D",
    agriculture = "#6C9A3F",
    forest = "#1B4332",
    unknown = "#8D99AE"
  )
  palette_type <- c(human = "#F8766D", pet = "#00BFC4")

  landscape_outcomes <- summarise_landscape_human_outcomes(
    analysis_data,
    bootstrap_replicates = bootstrap_replicates,
    seed = seed,
    include_unknown = FALSE
  )
  landscape_pvalues <- compute_landscape_outcome_pvalues(analysis_data)

  mean_bite_panel <- ggplot(
    landscape_outcomes,
    aes(x = characterization_clean, y = mean_bite_count, fill = characterization_clean)
  ) +
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_errorbar(
      aes(ymin = mean_bite_low, ymax = mean_bite_high),
      width = 0.16,
      linewidth = 0.7
    ) +
    scale_fill_manual(values = palette_landscape) +
    labs(
      x = NULL,
      y = "Mean winsorized bite count"
    ) +
    figure_theme() +
    annotate("text", x = Inf, y = Inf, label = format_p_value_label(landscape_pvalues$mean_bite), hjust = 1.05, vjust = 1.3, size = 5.1, fontface = "bold")

  rash_panel <- ggplot(
    landscape_outcomes,
    aes(x = characterization_clean, y = rash_probability, fill = characterization_clean)
  ) +
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_errorbar(
      aes(ymin = rash_low, ymax = rash_high),
      width = 0.16,
      linewidth = 0.7
    ) +
    scale_fill_manual(values = palette_landscape) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = NULL,
      y = "Probability"
    ) +
    figure_theme() +
    annotate("text", x = Inf, y = Inf, label = format_p_value_label(landscape_pvalues$rash), hjust = 1.05, vjust = 1.3, size = 5.1, fontface = "bold")

  crawl_panel <- ggplot(
    landscape_outcomes,
    aes(x = characterization_clean, y = crawl_only_probability, fill = characterization_clean)
  ) +
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_errorbar(
      aes(ymin = crawl_low, ymax = crawl_high),
      width = 0.16,
      linewidth = 0.7
    ) +
    scale_fill_manual(values = palette_landscape) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = NULL,
      y = "Probability"
    ) +
    figure_theme() +
    annotate("text", x = Inf, y = Inf, label = format_p_value_label(landscape_pvalues$crawl_only), hjust = 1.05, vjust = 1.3, size = 5.1, fontface = "bold")

  landscape_outcome_plot <- (mean_bite_panel + rash_panel + crawl_panel) +
    patchwork::plot_layout(ncol = 3)
  landscape_outcome_plot <- add_panel_tags(landscape_outcome_plot)

  bite_type_composition <- summarise_landscape_bite_type_composition(analysis_data, include_unknown = FALSE)
  bite_type_composition_p <- compute_landscape_bite_type_composition_pvalue(analysis_data)
  landscape_bite_type_plot <- ggplot(
    bite_type_composition,
    aes(x = characterization_clean, y = proportion, fill = bite_type_clean)
  ) +
    geom_col(width = 0.72, position = "fill") +
    scale_fill_manual(values = palette_type, labels = c("human", "pet"), name = "Report type") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Landscape class",
      y = "Share of registrations"
    ) +
    figure_theme() +
    annotate("text", x = Inf, y = 1.04, label = format_p_value_label(bite_type_composition_p), hjust = 1.05, vjust = 1.2, size = 5.1, fontface = "bold")

  monthly_profile <- summarise_landscape_monthly_profile(analysis_data, include_unknown = FALSE)
  landscape_monthly_plot <- ggplot(
    monthly_profile,
    aes(x = bite_month_label, y = characterization_clean, fill = monthly_share)
  ) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_viridis_c(
      option = "C",
      labels = scales::percent_format(accuracy = 1),
      name = "Within-landscape monthly share"
    ) +
    labs(
      x = "Month of bite",
      y = "Landscape class"
    ) +
    figure_theme()

  model_coefficients <- summarise_landscape_model_coefficients(analysis_data)
  landscape_terms <- summarise_landscape_model_terms(analysis_data)
  sensitivity_terms <- summarise_landscape_sensitivity_model_terms(analysis_data)
  sensitivity_fit <- summarise_landscape_sensitivity_model_fit(analysis_data)
  build_effect_panel <- function(model_name) {
    panel_data <- landscape_terms |>
      filter(model == model_name)

    ggplot(panel_data, aes(x = estimate, y = landscape_label)) +
      geom_vline(xintercept = 1, color = "#6C757D", linetype = "dashed", linewidth = 0.5) +
      geom_errorbar(
        aes(y = landscape_label, xmin = conf.low, xmax = conf.high),
        width = 0.18,
        linewidth = 0.75,
        color = "#2F3B4A",
        orientation = "y"
      ) +
      geom_point(size = 2.8, color = "#C0392B") +
      scale_x_log10() +
      labs(x = "Adjusted effect estimate (log scale)", y = NULL) +
      figure_theme()
  }

  effect_plot <- (
    build_effect_panel("Mean bite burden") +
      build_effect_panel("Rash probability") +
      build_effect_panel("Crawl-only probability")
  ) + plot_layout(ncol = 3)
  effect_plot <- add_panel_tags(effect_plot)

  build_sensitivity_panel <- function(outcome_name) {
    panel_data <- sensitivity_terms |>
      filter(outcome == outcome_name)

    ggplot(panel_data, aes(x = estimate, y = landscape_label, color = variant_label)) +
      geom_vline(xintercept = 1, color = "#6C757D", linetype = "dashed", linewidth = 0.5) +
      geom_errorbar(
        aes(y = landscape_label, xmin = conf.low, xmax = conf.high),
        width = 0.18,
        linewidth = 0.65,
        orientation = "y",
        position = position_dodge(width = 0.6)
      ) +
      geom_point(size = 2.4, position = position_dodge(width = 0.6)) +
      scale_x_log10() +
      scale_color_brewer(palette = "Dark2", name = "Model variant") +
      labs(x = "Adjusted effect estimate (log scale)", y = NULL) +
      figure_theme()
  }

  sensitivity_plot <- (
    build_sensitivity_panel("Mean bite burden") +
      build_sensitivity_panel("Rash probability") +
      build_sensitivity_panel("Crawl-only probability")
  ) + plot_layout(ncol = 3, guides = "collect") &
    theme(legend.position = "bottom")
  sensitivity_plot <- add_panel_tags(sensitivity_plot)

  artifacts <- list(
    list(
      filename = "landscape_associations_with_human_tick_outcomes.png",
      plot = landscape_outcome_plot,
      data = landscape_outcomes,
      title = "Landscape associations with human tick outcomes",
      width = 14,
      height = 5.8
    ),
    list(
      filename = "landscape_bite_type_composition.png",
      plot = landscape_bite_type_plot,
      data = bite_type_composition,
      title = "Bite type composition across landscape classes",
      width = 10,
      height = 6.2
    ),
    list(
      filename = "monthly_reporting_profile_by_landscape.png",
      plot = landscape_monthly_plot,
      data = monthly_profile,
      title = "Monthly reporting profile by landscape",
      width = 10.5,
      height = 5.8
    ),
    list(
      filename = "adjusted_landscape_effect_estimates.png",
      plot = effect_plot,
      data = landscape_terms,
      title = "Adjusted landscape effect estimates",
      width = 12,
      height = 6.8
    ),
    list(
      filename = "landscape_sensitivity_effect_estimates.png",
      plot = sensitivity_plot,
      data = sensitivity_terms,
      title = "Sensitivity of landscape effect estimates",
      width = 12,
      height = 7.2
    )
  )

  manifest <- write_artifact_manifest(
    artifacts,
    output_dir,
    "landscape-ecological-analyses-manifest.csv"
  )

  utils::write.csv(
    model_coefficients,
    file.path(output_dir, "landscape_association_model_coefficients.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    landscape_terms,
    file.path(output_dir, "landscape_association_landscape_terms.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    sensitivity_terms,
    file.path(output_dir, "landscape_sensitivity_model_terms.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    sensitivity_fit,
    file.path(output_dir, "landscape_sensitivity_model_fit_metrics.csv"),
    row.names = FALSE,
    na = ""
  )

  manifest
}

write_landscape_ecological_logbook_entry <- function(
  analysis_data,
  manifest,
  logbook_dir = "logbook",
  run_date = Sys.Date()
) {
  dir.create(logbook_dir, recursive = TRUE, showWarnings = FALSE)

  logbook_path <- file.path(
    logbook_dir,
    paste0(as.character(run_date), "-landscape-ecological-analyses.md")
  )

  lines <- c(
    paste0("# Logbook Entry - ", as.character(run_date)),
    "",
    "## Scope",
    "- Landscape ecological analysis phase for the Denmark tick-occurrence thesis.",
    "- Objective: evaluate whether urban, agricultural, and forest landscape classes contribute explanatory signal beyond basic descriptive plots, while retaining `unknown` for sensitivity checks.",
    "",
    "## Analytical Components Implemented",
    "- Human-outcome comparisons across landscape classes.",
    "- Bite-type composition across landscape classes.",
    "- Monthly reporting profile by landscape class.",
    "- Adjusted model estimates for landscape effects on bite burden, rash probability, and crawl-only probability.",
    "- Sensitivity analyses for raw versus winsorized bite counts and excluding the `unknown` landscape class.",
    "",
    "## Modeling Strategy",
    "- Negative binomial regression for mean bite burden.",
    "- Raw bite counts used in the primary count model; winsorized counts retained as a sensitivity specification.",
    "- Logistic regression for rash probability.",
    "- Logistic regression for crawl-only probability.",
    "- Denmark-only records retained for landscape analyses; landscape reference category fixed at `urban`.",
    "",
    "## Outputs Generated",
    paste0("- Figure count: ", nrow(manifest), " PNG files."),
    "- Additional model tables written as CSV outputs for direct thesis use.",
    "",
    "## Output Files",
    paste0("- `", manifest$filename, "`"),
    "- `landscape_association_model_coefficients.csv`",
    "- `landscape_association_landscape_terms.csv`",
    "- `landscape_sensitivity_model_terms.csv`",
    "- `landscape_sensitivity_model_fit_metrics.csv`"
  )

  writeLines(lines, con = logbook_path, useBytes = TRUE)
  logbook_path
}
