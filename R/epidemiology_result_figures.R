p_value_to_stars <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ "NA",
    p_value < 0.0001 ~ "****",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

compute_age_gender_mean_bite_pvalues <- function(analysis_data) {
  filter_human_age_gender(analysis_data) |>
    group_by(age_group) |>
    group_modify(function(data, key) {
      current_p <- if (length(unique(data$gender_clean)) == 2L) {
        stats::wilcox.test(bite_count_winsorized ~ gender_clean, data = data, exact = FALSE)$p.value
      } else {
        NA_real_
      }
      tibble::tibble(p_value_raw = current_p)
    }) |>
    ungroup() |>
    mutate(
      p_value = stats::p.adjust(p_value_raw, method = "BH"),
      significance = p_value_to_stars(p_value)
    )
}

compute_age_gender_rash_pvalues <- function(analysis_data) {
  filter_human_age_gender(analysis_data) |>
    group_by(age_group) |>
    group_modify(function(data, key) {
      current_p <- if (length(unique(data$gender_clean)) == 2L) {
        contingency_table <- table(data$gender_clean, data$rash_flag)
        if (all(dim(contingency_table) == c(2, 2))) stats::fisher.test(contingency_table)$p.value else NA_real_
      } else {
        NA_real_
      }
      tibble::tibble(p_value_raw = current_p)
    }) |>
    ungroup() |>
    mutate(
      p_value = stats::p.adjust(p_value_raw, method = "BH"),
      significance = p_value_to_stars(p_value)
    )
}

build_pairwise_significance_annotations <- function(summary_data, pvalue_data, value_column, dodge_offset = 0.205) {
  plotted_maxima <- summary_data |>
    group_by(age_group) |>
    summarise(group_max = max(.data[[value_column]], na.rm = TRUE), .groups = "drop")

  overall_range <- range(summary_data[[value_column]], na.rm = TRUE)
  buffer <- max((overall_range[2] - overall_range[1]) * 0.08, overall_range[2] * 0.04, 0.02)

  plotted_maxima |>
    left_join(pvalue_data, by = "age_group") |>
    mutate(
      x_index = match(as.character(age_group), age_group_levels()),
      x_start = x_index - dodge_offset,
      x_end = x_index + dodge_offset,
      y = group_max + buffer,
      y_tip = group_max + (buffer * 0.45),
      label_y = group_max + (buffer * 1.28)
    )
}

build_overall_significance_annotation <- function(summary_data, value_column, p_value, label_text) {
  overall_range <- range(summary_data[[value_column]], na.rm = TRUE)
  buffer <- max((overall_range[2] - overall_range[1]) * 0.12, overall_range[2] * 0.06, 0.03)

  data.frame(
    x_start = 1,
    x_end = nrow(summary_data),
    y = max(summary_data[[value_column]], na.rm = TRUE) + buffer,
    y_tip = max(summary_data[[value_column]], na.rm = TRUE) + (buffer * 0.45),
    label_y = max(summary_data[[value_column]], na.rm = TRUE) + (buffer * 1.25),
    p_value = p_value,
    significance = label_text
  )
}

add_significance_bracket <- function(plot_object, annotation_data, text_size = 5.2) {
  plot_object +
    geom_segment(data = annotation_data, aes(x = x_start, xend = x_end, y = y, yend = y), inherit.aes = FALSE, linewidth = 0.6) +
    geom_segment(data = annotation_data, aes(x = x_start, xend = x_start, y = y_tip, yend = y), inherit.aes = FALSE, linewidth = 0.6) +
    geom_segment(data = annotation_data, aes(x = x_end, xend = x_end, y = y_tip, yend = y), inherit.aes = FALSE, linewidth = 0.6) +
    geom_text(data = annotation_data, aes(x = (x_start + x_end) / 2, y = label_y, label = significance), inherit.aes = FALSE, size = text_size, fontface = "bold")
}

predict_gam_curve <- function(data, x_var, y_var, family = NULL, grid_points = 200L) {
  modeling_data <- data |>
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
  if (!nrow(modeling_data)) return(tibble::tibble())

  smooth_formula <- stats::as.formula(paste0(y_var, " ~ s(", x_var, ", k = 6)"))
  fitted_model <- if (is.null(family)) {
    mgcv::gam(smooth_formula, data = modeling_data, method = "REML")
  } else {
    mgcv::gam(smooth_formula, data = modeling_data, family = family, method = "REML")
  }

  prediction_grid <- tibble::tibble(x = seq(
    min(modeling_data[[x_var]], na.rm = TRUE),
    max(modeling_data[[x_var]], na.rm = TRUE),
    length.out = grid_points
  ))
  names(prediction_grid) <- x_var

  if (is.null(family)) {
    prediction <- predict(fitted_model, newdata = prediction_grid, type = "response", se.fit = TRUE)
    fit <- as.numeric(prediction$fit)
    se <- as.numeric(prediction$se.fit)
    lower <- pmax(0, fit - (1.96 * se))
    upper <- fit + (1.96 * se)
  } else {
    prediction <- predict(fitted_model, newdata = prediction_grid, type = "link", se.fit = TRUE)
    fit_link <- as.numeric(prediction$fit)
    se_link <- as.numeric(prediction$se.fit)
    fit <- plogis(fit_link)
    lower <- plogis(fit_link - (1.96 * se_link))
    upper <- plogis(fit_link + (1.96 * se_link))
  }

  prediction_grid |>
    mutate(fit = fit, lower = lower, upper = upper)
}

summarise_meteorological_associations <- function(analysis_data) {
  human_data <- filter_human_reports(analysis_data)

  bind_rows(
    predict_gam_curve(human_data, "temperature_c", "bite_count_winsorized") |>
      transmute(exposure = "Temperature (C)", outcome = "Mean bite count", exposure_value = temperature_c, fit, lower, upper),
    predict_gam_curve(human_data, "humidity_pct", "bite_count_winsorized") |>
      transmute(exposure = "Humidity (%)", outcome = "Mean bite count", exposure_value = humidity_pct, fit, lower, upper),
    predict_gam_curve(human_data, "temperature_c", "rash_numeric", family = stats::binomial()) |>
      transmute(exposure = "Temperature (C)", outcome = "Rash probability", exposure_value = temperature_c, fit, lower, upper),
    predict_gam_curve(human_data, "humidity_pct", "rash_numeric", family = stats::binomial()) |>
      transmute(exposure = "Humidity (%)", outcome = "Rash probability", exposure_value = humidity_pct, fit, lower, upper)
  ) |>
    mutate(
      outcome = factor(outcome, levels = c("Mean bite count", "Rash probability")),
      exposure = factor(exposure, levels = c("Temperature (C)", "Humidity (%)"))
    )
}

summarise_reporting_delay_data <- function(analysis_data) {
  analysis_data |>
    filter(bite_type_clean %in% c("human", "pet"), !is.na(report_delay_days), report_delay_days >= 0) |>
    transmute(bite_type_clean = factor(bite_type_clean, levels = c("human", "pet")), report_delay_days)
}

generate_primary_result_figures <- function(
  analysis_data,
  output_dir = file.path("figures", "primary_epidemiological_results"),
  bootstrap_replicates = 2000L,
  seed = 20260319L
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cleanup_primary_result_outputs(output_dir)

  palette_gender <- c(female = "#F8766D", male = "#00BFC4")
  palette_gender_diverging <- c(female = "#2E8B57", male = "#4F86B5")
  palette_type <- c(human = "#F8766D", pet = "#00BFC4")

  mean_bite_age_gender_data <- summarise_mean_bite_by_age_gender(analysis_data)
  mean_bite_age_gender_p <- compute_age_gender_mean_bite_pvalues(analysis_data)
  mean_bite_age_gender_annotations <- build_pairwise_significance_annotations(mean_bite_age_gender_data, mean_bite_age_gender_p, "mean_bite_count")
  mean_bite_age_gender_export <- mean_bite_age_gender_data |> left_join(mean_bite_age_gender_p, by = "age_group")
  plot_mean_bite_age_gender <- ggplot(mean_bite_age_gender_data, aes(x = age_group, y = mean_bite_count, fill = gender_clean)) +
    geom_col(position = position_dodge(width = 0.82), width = 0.74, na.rm = TRUE) +
    scale_fill_manual(values = palette_gender, labels = c("female", "male"), name = "Gender") +
    labs(x = "Age Group", y = "Mean Bite Count") +
    figure_theme()
  plot_mean_bite_age_gender <- add_significance_bracket(plot_mean_bite_age_gender, mean_bite_age_gender_annotations, text_size = 4.4)

  mean_bite_age_data <- summarise_mean_bite_by_age(analysis_data, bootstrap_replicates = bootstrap_replicates, seed = seed + 100L)
  mean_bite_age_p <- stats::kruskal.test(bite_count_winsorized ~ age_group, data = filter_human_age_gender(analysis_data))$p.value
  mean_bite_age_annotation <- build_overall_significance_annotation(mean_bite_age_data, "mean_bite_count", mean_bite_age_p, p_value_to_stars(mean_bite_age_p))
  mean_bite_age_export <- mean_bite_age_data |> mutate(global_p_value = mean_bite_age_p, global_significance = p_value_to_stars(mean_bite_age_p))
  plot_mean_bite_age <- ggplot(mean_bite_age_data, aes(x = age_group, y = mean_bite_count)) +
    geom_col(fill = "#4F86B5", width = 0.7, na.rm = TRUE) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.18, linewidth = 0.7, na.rm = TRUE) +
    labs(x = "Age Group", y = "Mean Bite Count") +
    figure_theme()
  plot_mean_bite_age <- add_significance_bracket(plot_mean_bite_age, mean_bite_age_annotation, text_size = 5)

  rash_age_data <- summarise_rash_by_age(analysis_data)
  rash_age_p <- stats::prop.trend.test(x = rash_age_data$rash_cases, n = rash_age_data$registrations, score = seq_len(nrow(rash_age_data)))$p.value
  rash_age_annotation <- build_overall_significance_annotation(rash_age_data, "probability", rash_age_p, p_value_to_stars(rash_age_p))
  rash_age_export <- rash_age_data |> mutate(global_p_value = rash_age_p, global_significance = p_value_to_stars(rash_age_p))
  plot_rash_age <- ggplot(rash_age_data, aes(x = age_group, y = probability)) +
    geom_col(fill = "#4F86B5", width = 0.7, na.rm = TRUE) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.18, linewidth = 0.7, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Age Group", y = "Rash Probability") +
    figure_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot_rash_age <- add_significance_bracket(plot_rash_age, rash_age_annotation, text_size = 5)

  rash_age_gender_data <- summarise_rash_by_age_gender(analysis_data)
  rash_age_gender_p <- compute_age_gender_rash_pvalues(analysis_data)
  rash_age_gender_annotations <- build_pairwise_significance_annotations(rash_age_gender_data, rash_age_gender_p, "probability")
  rash_age_gender_export <- rash_age_gender_data |> left_join(rash_age_gender_p, by = "age_group")
  plot_rash_age_gender <- ggplot(rash_age_gender_data, aes(x = age_group, y = probability, fill = gender_clean)) +
    geom_col(position = position_dodge(width = 0.82), width = 0.74, na.rm = TRUE) +
    scale_fill_manual(values = palette_gender, labels = c("female", "male"), name = "Gender") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Age Group", y = "Probability of Rash") +
    figure_theme()
  plot_rash_age_gender <- add_significance_bracket(plot_rash_age_gender, rash_age_gender_annotations, text_size = 4.4)

  mean_bite_gender_data <- summarise_mean_bite_by_gender(analysis_data, bootstrap_replicates = bootstrap_replicates, seed = seed)
  mean_bite_gender_p <- stats::wilcox.test(bite_count_winsorized ~ gender_clean, data = filter_human_age_gender(analysis_data), exact = FALSE)$p.value
  mean_bite_gender_annotation <- data.frame(
    x_start = 1,
    x_end = 2,
    y = max(mean_bite_gender_data$conf_high, na.rm = TRUE) + 0.06,
    y_tip = max(mean_bite_gender_data$conf_high, na.rm = TRUE) + 0.02,
    label_y = max(mean_bite_gender_data$conf_high, na.rm = TRUE) + 0.11,
    significance = p_value_to_stars(mean_bite_gender_p)
  )
  mean_bite_gender_export <- mean_bite_gender_data |> mutate(overall_p_value = mean_bite_gender_p, overall_significance = p_value_to_stars(mean_bite_gender_p))
  plot_mean_bite_gender <- ggplot(mean_bite_gender_data, aes(x = gender_clean, y = mean_bite_count, fill = gender_clean)) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.12, linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.2f", mean_bite_count), y = conf_high + 0.05), vjust = -0.2, size = 6.4) +
    scale_fill_manual(values = palette_gender) +
    labs(x = "Gender", y = "Mean Bite Count") +
    figure_theme()
  plot_mean_bite_gender <- add_significance_bracket(plot_mean_bite_gender, mean_bite_gender_annotation, text_size = 5)

  interaction_estimates <- summarise_human_age_sex_model_estimates(analysis_data)
  interaction_tests <- summarise_human_age_sex_model_tests(analysis_data)
  build_interaction_panel <- function(outcome_name, y_label, percent_scale = FALSE) {
    panel_data <- interaction_estimates |>
      filter(outcome == outcome_name)

    plot_object <- ggplot(
      panel_data,
      aes(x = age_group, y = estimate, color = gender_clean, group = gender_clean)
    ) +
      geom_line(linewidth = 0.95) +
      geom_point(size = 2.4) +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.14, linewidth = 0.68) +
      scale_color_manual(values = palette_gender, labels = c("female", "male"), name = "Gender") +
      labs(x = "Age Group", y = y_label) +
      figure_theme()

    if (percent_scale) {
      plot_object <- plot_object + scale_y_continuous(labels = percent_format(accuracy = 1))
    }
    plot_object
  }

  interaction_plot <- (
    build_interaction_panel("Mean bite burden", "Predicted bite burden") +
      build_interaction_panel("Rash probability", "Predicted rash probability", percent_scale = TRUE)
  ) +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "top")
  interaction_plot <- add_panel_tags(interaction_plot)

  crawl_only_data <- summarise_crawl_only_probability(analysis_data)
  plot_crawl_only <- ggplot(crawl_only_data, aes(x = bite_type_clean, y = probability, fill = bite_type_clean)) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12, linewidth = 0.7) +
    geom_text(aes(label = percent(probability, accuracy = 0.1), y = upper + 0.03), vjust = -0.2, size = 6.2) +
    scale_fill_manual(values = palette_type) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Bite Type", y = "Probability") +
    figure_theme()

  registrations_age_gender_data <- summarise_registrations_age_gender(analysis_data) |>
    mutate(age_group = factor(as.character(age_group), levels = rev(age_group_levels()), ordered = TRUE))
  plot_registrations_age_gender <- ggplot(registrations_age_gender_data, aes(x = signed_registrations, y = age_group, fill = gender_clean)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = palette_gender_diverging, labels = c("female", "male"), name = "Gender") +
    scale_x_continuous(labels = function(x) abs(x)) +
    labs(x = "Number of registrations", y = "Age group (years)") +
    figure_theme() + theme(legend.position = "top")

  artifacts <- list(
    list(filename = "mean_bite_count_by_age_group_and_gender.png", plot = plot_mean_bite_age_gender, data = mean_bite_age_gender_export, title = "Mean bite count by age group and gender with within-age significance annotations"),
    list(filename = "mean_bite_count_by_age_group.png", plot = plot_mean_bite_age, data = mean_bite_age_export, title = "Mean bite count by age group with overall age-group significance"),
    list(filename = "rash_probability_by_age_group.png", plot = plot_rash_age, data = rash_age_export, title = "Rash probability by age group with global trend significance"),
    list(filename = "rash_probability_by_age_group_and_gender.png", plot = plot_rash_age_gender, data = rash_age_gender_export, title = "Rash probability by age group and gender with within-age significance annotations"),
    list(filename = "mean_bite_count_by_gender.png", plot = plot_mean_bite_gender, data = mean_bite_gender_export, title = "Mean bite count by gender with significance annotation"),
    list(filename = "human_age_sex_model_estimates.png", plot = interaction_plot, data = interaction_estimates, title = "Model-based human age-sex estimates", width = 11, height = 6.8),
    list(filename = "crawl_only_probability_by_bite_type.png", plot = plot_crawl_only, data = crawl_only_data, title = "Probability of crawl without bite by bite type"),
    list(filename = "registrations_by_age_group_and_gender.png", plot = plot_registrations_age_gender, data = registrations_age_gender_data, title = "Registrations by gender and age group")
  )

  manifest <- write_artifact_manifest(artifacts, output_dir, "primary-epidemiological-results-manifest.csv")
  utils::write.csv(
    interaction_tests,
    file.path(output_dir, "human_age_sex_interaction_model_tests.csv"),
    row.names = FALSE,
    na = ""
  )
  manifest
}

generate_supplementary_analysis_figures <- function(analysis_data, output_dir = file.path("figures", "supplementary_epidemiological_analyses")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cleanup_supplementary_analysis_outputs(output_dir)

  palette_type <- c(human = "#F8766D", pet = "#00BFC4")
  seasonality_data <- summarise_monthly_human_seasonality(analysis_data)

  seasonality_registrations <- ggplot(seasonality_data, aes(x = bite_month_label, y = registrations, group = 1)) +
    geom_line(color = "#0B6E4F", linewidth = 1.1) + geom_point(color = "#0B6E4F", size = 2.2) +
    labs(x = NULL, y = "Registrations") +
    figure_theme() + theme(legend.position = "none")

  seasonality_bites <- ggplot(seasonality_data, aes(x = bite_month_label, y = mean_bite_count, group = 1)) +
    geom_line(color = "#D94801", linewidth = 1.1) + geom_point(color = "#D94801", size = 2.2) +
    labs(x = NULL, y = "Mean winsorized bite count") +
    figure_theme() + theme(legend.position = "none")

  seasonality_crawl <- ggplot(seasonality_data, aes(x = bite_month_label, y = crawl_only_probability, group = 1)) +
    geom_line(color = "#00798C", linewidth = 1.1) + geom_point(color = "#00798C", size = 2.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Month of bite", y = "Probability") +
    figure_theme() + theme(legend.position = "none")

  seasonality_rash <- ggplot(seasonality_data, aes(x = bite_month_label, y = rash_probability, group = 1)) +
    geom_line(color = "#C0392B", linewidth = 1.1) + geom_point(color = "#C0392B", size = 2.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Month of bite", y = "Probability") +
    figure_theme() + theme(legend.position = "none")

  seasonality_plot <- (seasonality_registrations + seasonality_bites) / (seasonality_crawl + seasonality_rash)
  seasonality_plot <- add_panel_tags(seasonality_plot)

  distribution_data <- summarise_distribution_diagnostics(analysis_data) |>
    mutate(metric = factor(metric, levels = c("Bite count", "Crawl count")), version = factor(version, levels = c("Raw", "Winsorized")))
  build_distribution_panel <- function(metric_name) {
    panel_data <- distribution_data |>
      filter(metric == metric_name)

    ggplot(panel_data, aes(x = count_value, color = version)) +
      geom_freqpoly(binwidth = 1, linewidth = 0.95) +
      scale_color_manual(values = c(Raw = "#3B3B3B", Winsorized = "#2C7FB8")) +
      scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 1), breaks = c(0, 1, 2, 3, 5, 10, 20, 50, 100, 200)) +
      labs(x = "Count value", y = "Frequency", color = "Version") +
      figure_theme()
  }

  distribution_plot <- build_distribution_panel("Bite count") +
    build_distribution_panel("Crawl count") +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "top")
  distribution_plot <- add_panel_tags(distribution_plot)

  heatmap_data <- summarise_age_gender_heatmap(analysis_data)
  build_heatmap_plot <- function(metric_name, fill_low, fill_high) {
    metric_data <- heatmap_data |> filter(metric == metric_name)
    ggplot(metric_data, aes(x = gender_clean, y = age_group, fill = value)) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = label), size = 4.5, fontface = "bold") +
      scale_fill_gradient(low = fill_low, high = fill_high) +
      labs(x = NULL, y = NULL) +
      figure_theme() + theme(legend.position = "none")
  }

  heatmap_plot <- (
    build_heatmap_plot("Registrations", "#F7FBFF", "#0B6E4F") +
      build_heatmap_plot("Mean bite count", "#FFF5EB", "#D94801") +
      build_heatmap_plot("Rash probability", "#FFF5F0", "#CB181D")
  ) + patchwork::plot_layout(ncol = 3)
  heatmap_plot <- add_panel_tags(heatmap_plot)

  denmark_country <- get_denmark_country_sf()
  denmark_reports_sf <- denmark_report_sf(analysis_data)
  denmark_reports <- filter_denmark_reports(analysis_data)
  municipality_counts <- summarise_denmark_municipality_counts(analysis_data)
  kde_surface <- summarise_denmark_kde_surface(analysis_data)

  density_map <- ggplot() +
    geom_tile(
      data = kde_surface,
      aes(x = x, y = y, fill = density),
      alpha = 0.88
    ) +
    scale_fill_gradientn(
      colours = viridisLite::magma(256),
      trans = "sqrt",
      name = "KDE density",
      breaks = function(lims) c(min(lims, na.rm = TRUE), max(lims, na.rm = TRUE)),
      labels = c("Low", "High")
    ) +
    geom_sf(data = denmark_country, fill = NA, color = "#111111", linewidth = 0.8) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    figure_theme() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_colorbar(barwidth = grid::unit(6, "cm"), barheight = grid::unit(0.4, "cm")))

  municipality_map <- ggplot(municipality_counts) +
    geom_sf(aes(fill = report_count), color = "#F7F7F7", linewidth = 0.18) +
    geom_sf(data = denmark_country, fill = NA, color = "#111111", linewidth = 0.65) +
    scale_fill_gradientn(
      colours = viridisLite::magma(256),
      trans = "sqrt",
      name = "Reports",
      breaks = function(lims) c(min(lims, na.rm = TRUE), max(lims, na.rm = TRUE))
    ) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    figure_theme() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_colorbar(barwidth = grid::unit(6, "cm"), barheight = grid::unit(0.4, "cm")))

  spatial_plot <- density_map + municipality_map + patchwork::plot_layout(ncol = 2)
  spatial_plot <- add_panel_tags(spatial_plot)

  weather_data <- summarise_meteorological_associations(analysis_data)
  build_weather_panel <- function(exposure_name, outcome_name, line_color, y_label, percent_scale = FALSE) {
    panel_data <- weather_data |> filter(exposure == exposure_name, outcome == outcome_name)
    panel_plot <- ggplot(panel_data, aes(x = exposure_value, y = fit)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(line_color, 0.18)) +
      geom_line(color = line_color, linewidth = 1.15) +
      labs(x = exposure_name, y = y_label) +
      figure_theme() + theme(legend.position = "none")
    if (percent_scale) panel_plot + scale_y_continuous(labels = percent_format(accuracy = 1)) else panel_plot
  }

  weather_plot <- (
    build_weather_panel("Temperature (C)", "Mean bite count", "#1F78B4", "Mean winsorized bite count") +
      build_weather_panel("Humidity (%)", "Mean bite count", "#33A02C", "Mean winsorized bite count")
  ) / (
    build_weather_panel("Temperature (C)", "Rash probability", "#E31A1C", "Probability", percent_scale = TRUE) +
      build_weather_panel("Humidity (%)", "Rash probability", "#6A3D9A", "Probability", percent_scale = TRUE)
  )
  weather_plot <- add_panel_tags(weather_plot)

  delay_data <- summarise_reporting_delay_data(analysis_data)
  delay_plot <- ggplot(delay_data, aes(x = report_delay_days, color = bite_type_clean)) +
    stat_ecdf(geom = "step", linewidth = 1.1) +
    scale_color_manual(values = palette_type, labels = c("human", "pet"), name = "Bite Type") +
    scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 1), breaks = c(0, 1, 3, 7, 14, 30, 90, 365, 1000)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Delay from bite date to registration (days)", y = "Cumulative share") +
    figure_theme()

  artifacts <- list(
    list(filename = "monthly_human_tick_seasonality.png", plot = seasonality_plot, data = seasonality_data, title = "Monthly seasonality of human tick interactions", width = 12, height = 8),
    list(filename = "raw_vs_winsorized_count_distributions.png", plot = distribution_plot, data = distribution_data, title = "Raw vs winsorized count distributions", width = 11, height = 6.5),
    list(filename = "age_gender_outcome_heatmap.png", plot = heatmap_plot, data = heatmap_data, title = "Age-gender outcome heatmap", width = 14, height = 5.5),
    list(filename = "denmark_spatial_comparison_maps.png", plot = spatial_plot, data = municipality_counts |> st_drop_geometry(), title = "Denmark spatial comparison maps", width = 13, height = 7.6),
    list(filename = "meteorological_associations_with_bite_and_rash.png", plot = weather_plot, data = weather_data, title = "Meteorological associations with bite burden and rash probability", width = 12, height = 8),
    list(filename = "reporting_delay_ecdf_by_bite_type.png", plot = delay_plot, data = delay_data, title = "Reporting delay ECDF by bite type", width = 10, height = 6)
  )

  write_artifact_manifest(artifacts, output_dir, "supplementary-epidemiological-analyses-manifest.csv")
}

write_primary_result_logbook_entry <- function(analysis_data, manifest, logbook_dir = "logbook", run_date = Sys.Date()) {
  dir.create(logbook_dir, recursive = TRUE, showWarnings = FALSE)
  source_path <- attr(analysis_data, "source_path")
  sheet_name <- attr(analysis_data, "sheet_name")
  bite_cap_p99 <- attr(analysis_data, "bite_cap_p99")
  crawl_cap_p99 <- attr(analysis_data, "crawl_cap_p99")

  logbook_path <- file.path(logbook_dir, paste0(as.character(run_date), "-primary-epidemiological-results.md"))
  lines <- c(
    paste0("# Logbook Entry - ", as.character(run_date)),
    "",
    "## Scope",
    "- Phase 1 implementation for the Denmark tick-occurrence thesis.",
    "- Objective: generate the primary epidemiological result figures from `input.xlsx` with a reproducible R pipeline.",
    "",
    "## Environment",
    paste0("- Source workbook: `", basename(source_path), "`"),
    paste0("- Worksheet: `", sheet_name, "`"),
    paste0("- Rows in cleaned table: ", nrow(analysis_data)),
    paste0("- R runtime: ", R.version.string),
    paste0("- Quarto available: ", if (nzchar(Sys.which("quarto"))) "yes" else "no"),
    "",
    "## Data Rules Locked In",
    "- Column names are normalized with `janitor::clean_names()` and all original workbook columns are preserved.",
    "- `Bite` and `Crawl` are parsed with `false -> 0`, `true -> 1`, `true (n) -> n`; malformed values become `NA`.",
    paste0("- Winsorization policy: global 99th percentile cap at `Bite = ", bite_cap_p99, "` and `Crawl = ", crawl_cap_p99, "` for mean-based figures."),
    "- Age bins: `0-9`, `10-19`, `20-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`.",
    "- Age/gender figures are restricted to human reports with numeric age and binary sex labels (`female`, `male`).",
    "",
    "## Outputs Generated",
    paste0("- Figure count: ", nrow(manifest), " PNG files."),
    paste0("- Summary tables: ", nrow(manifest), " CSV files plus a manifest."),
    "- Generated filenames are descriptive and map directly to the analysis they contain.",
    "",
    "## Output Files",
    paste0("- `", manifest$filename, "`")
  )
  writeLines(lines, con = logbook_path, useBytes = TRUE)
  logbook_path
}

write_supplementary_analysis_logbook_entry <- function(analysis_data, manifest, logbook_dir = "logbook", run_date = Sys.Date()) {
  dir.create(logbook_dir, recursive = TRUE, showWarnings = FALSE)
  logbook_path <- file.path(logbook_dir, paste0(as.character(run_date), "-supplementary-epidemiological-analyses.md"))
  lines <- c(
    paste0("# Logbook Entry - ", as.character(run_date)),
    "",
    "## Scope",
    "- Extended phase 1 supplementary epidemiological analysis batch for the Denmark tick-occurrence thesis.",
    "- Objective: add seasonality, distribution diagnostics, heatmap, spatial, meteorological, and reporting-delay plots on top of the primary result figures.",
    "",
    "## Data Extensions Used",
    "- Parsed bite dates and registration timestamps from the workbook to derive month-of-bite and reporting delay.",
    "- Preserved the global 99th percentile winsorization policy for bite and crawl counts.",
    "- Restricted the Denmark map to records with `country_clean == DK` and valid coordinates.",
    "",
    "## Outputs Generated",
    paste0("- Figure count: ", nrow(manifest), " PNG files."),
    paste0("- Summary tables: ", nrow(manifest), " CSV files plus a manifest."),
    "- All filenames are descriptive and aligned with thesis subsection topics.",
    "",
    "## Output Files",
    paste0("- `", manifest$filename, "`")
  )
  writeLines(lines, con = logbook_path, useBytes = TRUE)
  logbook_path
}
