suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(janitor)
  library(maps)
  library(mgcv)
  library(patchwork)
  library(readxl)
  library(scales)
  library(sf)
  library(tidyr)
})

age_group_levels <- function() {
  c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
}

month_levels <- function() {
  month.abb
}

primary_result_figure_filenames <- function() {
  c(
    "mean_bite_count_by_age_group_and_gender.png",
    "mean_bite_count_by_age_group.png",
    "rash_probability_by_age_group.png",
    "rash_probability_by_age_group_and_gender.png",
    "mean_bite_count_by_gender.png",
    "human_age_sex_model_estimates.png",
    "crawl_only_probability_by_bite_type.png",
    "registrations_by_age_group_and_gender.png"
  )
}

primary_result_table_filenames <- function() {
  c(
    "human_age_sex_interaction_model_tests.csv"
  )
}

legacy_primary_result_figure_filenames <- function() {
  c(
    "plot-1-new.png",
    "plot-2.png",
    "plot-2 (1).png",
    "plot-2-new.png",
    "plot-3-alt.png",
    "plot-4-alt.png",
    "rplot-extended-2.png"
  )
}

supplementary_analysis_figure_filenames <- function() {
  c(
    "monthly_human_tick_seasonality.png",
    "raw_vs_winsorized_count_distributions.png",
    "age_gender_outcome_heatmap.png",
    "denmark_spatial_comparison_maps.png",
    "meteorological_associations_with_bite_and_rash.png",
    "reporting_delay_ecdf_by_bite_type.png"
  )
}

legacy_supplementary_analysis_figure_filenames <- function() {
  c(
    "denmark_report_density_map.png"
  )
}

parse_tick_count <- function(value) {
  unname(vapply(
    value,
    FUN.VALUE = integer(1),
    function(single_value) {
      if (length(single_value) == 0L || is.na(single_value)) {
        return(NA_integer_)
      }
      if (is.logical(single_value)) {
        return(if (isTRUE(single_value)) 1L else 0L)
      }
      if (is.numeric(single_value)) {
        return(as.integer(single_value))
      }

      normalized <- tolower(trimws(as.character(single_value)))

      if (normalized == "false") return(0L)
      if (normalized == "true") return(1L)
      if (grepl("^true\\s*\\(\\d+\\)$", normalized)) {
        return(as.integer(sub("^true\\s*\\((\\d+)\\)$", "\\1", normalized)))
      }
      if (grepl("^\\d+$", normalized)) {
        return(as.integer(normalized))
      }
      NA_integer_
    }
  ))
}

parse_flag <- function(value) {
  unname(vapply(
    value,
    FUN.VALUE = logical(1),
    function(single_value) {
      if (length(single_value) == 0L || is.na(single_value)) {
        return(NA)
      }
      if (is.logical(single_value)) {
        return(single_value)
      }

      normalized <- tolower(trimws(as.character(single_value)))
      if (normalized %in% c("true", "1", "yes")) return(TRUE)
      if (normalized %in% c("false", "0", "no")) return(FALSE)
      NA
    }
  ))
}

parse_date_safe <- function(value) {
  suppressWarnings(as.Date(as.character(value), tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y")))
}

parse_datetime_safe <- function(value) {
  suppressWarnings(as.POSIXct(
    as.character(value),
    tz = "UTC",
    tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")
  ))
}

bin_age_group <- function(age_years) {
  cut(
    age_years,
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
    labels = age_group_levels(),
    right = FALSE,
    ordered_result = TRUE
  )
}

winsorize_upper <- function(x, threshold) {
  capped <- x
  capped[!is.na(capped)] <- pmin(capped[!is.na(capped)], threshold)
  capped
}

wilson_ci <- function(successes, n, conf.level = 0.95) {
  stopifnot(length(successes) == length(n))
  alpha <- 1 - conf.level
  z_value <- qnorm(1 - alpha / 2)
  lower <- rep(NA_real_, length(n))
  upper <- rep(NA_real_, length(n))

  valid <- !is.na(successes) & !is.na(n) & n > 0
  if (!any(valid)) {
    return(data.frame(lower = lower, upper = upper))
  }

  p_hat <- successes[valid] / n[valid]
  denominator <- 1 + (z_value^2 / n[valid])
  center <- (p_hat + (z_value^2 / (2 * n[valid]))) / denominator
  margin <- (
    z_value *
      sqrt((p_hat * (1 - p_hat) / n[valid]) + (z_value^2 / (4 * n[valid]^2)))
  ) / denominator

  lower[valid] <- pmax(0, center - margin)
  upper[valid] <- pmin(1, center + margin)
  data.frame(lower = lower, upper = upper)
}

bootstrap_mean_ci <- function(
  x,
  conf.level = 0.95,
  bootstrap_replicates = 2000L,
  seed = 20260319L
) {
  values <- x[!is.na(x)]
  if (!length(values)) return(c(lower = NA_real_, upper = NA_real_))
  if (length(values) == 1L) return(c(lower = values, upper = values))

  set.seed(seed)
  bootstrap_means <- replicate(
    bootstrap_replicates,
    mean(sample(values, size = length(values), replace = TRUE))
  )
  probs <- c((1 - conf.level) / 2, 1 - ((1 - conf.level) / 2))
  stats::quantile(bootstrap_means, probs = probs, names = FALSE, na.rm = TRUE)
}

load_tick_data <- function(path = "input.xlsx") {
  if (!file.exists(path)) stop("Input workbook not found: ", path, call. = FALSE)

  sheets <- readxl::excel_sheets(path)
  if (length(sheets) != 1L) {
    stop("Expected exactly one worksheet in ", path, ", found ", length(sheets), ".", call. = FALSE)
  }

  raw_data <- readxl::read_excel(
    path,
    sheet = sheets[[1]],
    guess_max = 20000,
    .name_repair = "minimal"
  ) |>
    janitor::clean_names()

  attr(raw_data, "source_path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  attr(raw_data, "sheet_name") <- sheets[[1]]
  raw_data
}

build_analysis_table <- function(raw_data) {
  required_columns <- c("bite_type", "age", "gender", "crawl", "bite", "rash")
  missing_columns <- setdiff(required_columns, names(raw_data))
  if (length(missing_columns)) {
    stop("Raw data is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  bite_count_raw <- parse_tick_count(raw_data$bite)
  crawl_count_raw <- parse_tick_count(raw_data$crawl)
  bite_dates <- if ("bite_date" %in% names(raw_data)) parse_date_safe(raw_data$bite_date) else rep(as.Date(NA), nrow(raw_data))
  registration_datetimes <- if ("registration_date" %in% names(raw_data)) parse_datetime_safe(raw_data$registration_date) else rep(as.POSIXct(NA, tz = "UTC"), nrow(raw_data))
  registration_dates <- as.Date(registration_datetimes)
  report_delay_days <- as.numeric(difftime(registration_dates, bite_dates, units = "days"))
  bite_month_number <- suppressWarnings(as.integer(format(bite_dates, "%m")))
  bite_cap_p99 <- as.numeric(stats::quantile(bite_count_raw, probs = 0.99, na.rm = TRUE, names = FALSE))
  crawl_cap_p99 <- as.numeric(stats::quantile(crawl_count_raw, probs = 0.99, na.rm = TRUE, names = FALSE))

  analysis_data <- raw_data |>
    mutate(
      bite_type_clean = tolower(trimws(as.character(bite_type))),
      country_clean = toupper(trimws(as.character(country))),
      characterization_clean = tolower(trimws(as.character(characterization))),
      gender_clean = tolower(trimws(as.character(gender))),
      rash_flag = parse_flag(rash),
      rash_numeric = as.integer(rash_flag),
      bite_count_raw = bite_count_raw,
      crawl_count_raw = crawl_count_raw,
      bite_count_winsorized = winsorize_upper(bite_count_raw, bite_cap_p99),
      crawl_count_winsorized = winsorize_upper(crawl_count_raw, crawl_cap_p99),
      age_years = suppressWarnings(as.numeric(age)),
      age_group = bin_age_group(age_years),
      temperature_c = suppressWarnings(as.numeric(temperature)),
      humidity_pct = suppressWarnings(as.numeric(humidity)),
      bite_date_parsed = bite_dates,
      registration_datetime = registration_datetimes,
      registration_date_parsed = registration_dates,
      bite_month_number = bite_month_number,
      bite_month_label = factor(month.abb[bite_month_number], levels = month_levels(), ordered = TRUE),
      report_delay_days = report_delay_days,
      is_crawl_only = coalesce(crawl_count_raw, 0L) > 0L & coalesce(bite_count_raw, 0L) == 0L
    )

  attr(analysis_data, "source_path") <- attr(raw_data, "source_path")
  attr(analysis_data, "sheet_name") <- attr(raw_data, "sheet_name")
  attr(analysis_data, "bite_cap_p99") <- bite_cap_p99
  attr(analysis_data, "crawl_cap_p99") <- crawl_cap_p99
  analysis_data
}

filter_human_age_gender <- function(analysis_data) {
  analysis_data |>
    filter(
      bite_type_clean == "human",
      gender_clean %in% c("female", "male"),
      !is.na(age_years),
      !is.na(age_group)
    ) |>
    mutate(
      age_group = factor(as.character(age_group), levels = age_group_levels(), ordered = TRUE),
      gender_clean = factor(gender_clean, levels = c("female", "male"))
    )
}

filter_human_reports <- function(analysis_data) {
  analysis_data |>
    filter(bite_type_clean == "human")
}

filter_denmark_reports <- function(analysis_data) {
  analysis_data |>
    filter(country_clean == "DK", !is.na(latitude), !is.na(longitude))
}

get_denmark_country_sf <- function(epsg = 3035) {
  giscoR::gisco_get_countries(country = "DK", epsg = epsg)
}

get_denmark_municipality_sf <- function(epsg = 3035, year = 2024) {
  giscoR::gisco_get_lau(country = "DK", year = year, epsg = epsg) |>
    mutate(
      municipality_name = LAU_NAME,
      municipality_id = GISCO_ID
    )
}

denmark_report_sf <- function(analysis_data, epsg = 3035) {
  filter_denmark_reports(analysis_data) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(epsg)
}

summarise_denmark_municipality_counts <- function(analysis_data, epsg = 3035, year = 2024) {
  municipality_sf <- get_denmark_municipality_sf(epsg = epsg, year = year)
  report_sf <- denmark_report_sf(analysis_data, epsg = epsg)

  report_matches <- sf::st_join(
    report_sf |> dplyr::select(report_id = id),
    municipality_sf |> dplyr::select(municipality_id, municipality_name, POP_2024, AREA_KM2),
    left = FALSE,
    join = sf::st_within
  ) |>
    st_drop_geometry()

  municipality_counts <- report_matches |>
    count(municipality_id, municipality_name, POP_2024, AREA_KM2, name = "report_count")

  municipality_sf |>
    left_join(municipality_counts, by = c("municipality_id", "municipality_name", "POP_2024", "AREA_KM2")) |>
    mutate(
      report_count = coalesce(report_count, 0L),
      report_rate_per_100k = if_else(!is.na(POP_2024) & POP_2024 > 0, (report_count / POP_2024) * 100000, NA_real_)
    )
}

summarise_denmark_kde_surface <- function(analysis_data, epsg = 3035, grid_n = 220L) {
  report_sf <- denmark_report_sf(analysis_data, epsg = epsg)
  country_sf <- get_denmark_country_sf(epsg = epsg)

  coords <- sf::st_coordinates(report_sf)
  x_range <- range(coords[, "X"], na.rm = TRUE)
  y_range <- range(coords[, "Y"], na.rm = TRUE)

  kde_fit <- MASS::kde2d(
    x = coords[, "X"],
    y = coords[, "Y"],
    n = grid_n,
    lims = c(x_range, y_range)
  )

  grid_df <- expand.grid(
    x = kde_fit$x,
    y = kde_fit$y
  ) |>
    mutate(
      density = as.vector(kde_fit$z)
    )

  grid_sf <- sf::st_as_sf(grid_df, coords = c("x", "y"), crs = epsg)
  inside_index <- lengths(sf::st_within(grid_sf, country_sf)) > 0

  grid_df[inside_index, , drop = FALSE]
}

summarise_mean_bite_by_age_gender <- function(analysis_data) {
  filter_human_age_gender(analysis_data) |>
    group_by(age_group, gender_clean) |>
    summarise(
      registrations = dplyr::n(),
      mean_bite_count = mean(bite_count_winsorized, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      age_group = factor(age_group_levels(), levels = age_group_levels(), ordered = TRUE),
      gender_clean = factor(c("female", "male"), levels = c("female", "male")),
      fill = list(registrations = 0L, mean_bite_count = NA_real_)
    ) |>
    arrange(age_group, gender_clean)
}

summarise_mean_bite_by_age <- function(analysis_data, bootstrap_replicates = 2000L, seed = 20260319L) {
  filtered_data <- filter_human_age_gender(analysis_data)

  summary_data <- filtered_data |>
    group_by(age_group) |>
    summarise(registrations = dplyr::n(), mean_bite_count = mean(bite_count_winsorized, na.rm = TRUE), .groups = "drop") |>
    arrange(age_group)

  grouped_values <- split(filtered_data$bite_count_winsorized, filtered_data$age_group)
  ci_values <- lapply(
    seq_along(grouped_values),
    function(index) bootstrap_mean_ci(grouped_values[[index]], bootstrap_replicates = bootstrap_replicates, seed = seed + index)
  )

  summary_data$conf_low <- vapply(ci_values, `[[`, numeric(1), 1)
  summary_data$conf_high <- vapply(ci_values, `[[`, numeric(1), 2)
  summary_data
}

summarise_rash_by_age <- function(analysis_data) {
  summary_data <- filter_human_age_gender(analysis_data) |>
    group_by(age_group) |>
    summarise(
      registrations = dplyr::n(),
      rash_cases = sum(rash_flag, na.rm = TRUE),
      probability = mean(rash_flag, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      age_group = factor(age_group_levels(), levels = age_group_levels(), ordered = TRUE),
      fill = list(registrations = 0L, rash_cases = 0L, probability = NA_real_)
    ) |>
    arrange(age_group)

  ci <- wilson_ci(summary_data$rash_cases, summary_data$registrations)
  bind_cols(summary_data, ci)
}

summarise_rash_by_age_gender <- function(analysis_data) {
  filter_human_age_gender(analysis_data) |>
    group_by(age_group, gender_clean) |>
    summarise(
      registrations = dplyr::n(),
      rash_cases = sum(rash_flag, na.rm = TRUE),
      probability = mean(rash_flag, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      age_group = factor(age_group_levels(), levels = age_group_levels(), ordered = TRUE),
      gender_clean = factor(c("female", "male"), levels = c("female", "male")),
      fill = list(registrations = 0L, rash_cases = 0L, probability = NA_real_)
    ) |>
    arrange(age_group, gender_clean)
}

summarise_mean_bite_by_gender <- function(analysis_data, bootstrap_replicates = 2000L, seed = 20260319L) {
  filtered_data <- filter_human_age_gender(analysis_data)
  grouped_values <- split(filtered_data$bite_count_winsorized, filtered_data$gender_clean)

  summary_data <- filtered_data |>
    group_by(gender_clean) |>
    summarise(registrations = dplyr::n(), mean_bite_count = mean(bite_count_winsorized, na.rm = TRUE), .groups = "drop") |>
    arrange(gender_clean)

  ci_values <- lapply(
    seq_along(grouped_values),
    function(index) bootstrap_mean_ci(grouped_values[[index]], bootstrap_replicates = bootstrap_replicates, seed = seed + index)
  )

  summary_data$conf_low <- vapply(ci_values, `[[`, numeric(1), 1)
  summary_data$conf_high <- vapply(ci_values, `[[`, numeric(1), 2)
  summary_data$gender_clean <- factor(summary_data$gender_clean, levels = c("female", "male"))
  summary_data
}

summarise_crawl_only_probability <- function(analysis_data) {
  summary_data <- analysis_data |>
    filter(bite_type_clean %in% c("human", "pet")) |>
    mutate(bite_type_clean = factor(bite_type_clean, levels = c("human", "pet"))) |>
    group_by(bite_type_clean) |>
    summarise(
      registrations = dplyr::n(),
      crawl_only_cases = sum(is_crawl_only, na.rm = TRUE),
      probability = mean(is_crawl_only, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(bite_type_clean)

  ci <- wilson_ci(summary_data$crawl_only_cases, summary_data$registrations)
  bind_cols(summary_data, ci)
}

summarise_registrations_age_gender <- function(analysis_data) {
  filter_human_age_gender(analysis_data) |>
    count(age_group, gender_clean, name = "registrations") |>
    tidyr::complete(
      age_group = factor(age_group_levels(), levels = age_group_levels(), ordered = TRUE),
      gender_clean = factor(c("female", "male"), levels = c("female", "male")),
      fill = list(registrations = 0L)
    ) |>
    mutate(
      signed_registrations = if_else(gender_clean == "male", -registrations, registrations),
      absolute_registrations = abs(signed_registrations)
    ) |>
    arrange(age_group, gender_clean)
}

summarise_monthly_human_seasonality <- function(analysis_data) {
  filter_human_reports(analysis_data) |>
    filter(!is.na(bite_month_label)) |>
    group_by(bite_month_label) |>
    summarise(
      registrations = dplyr::n(),
      mean_bite_count = mean(bite_count_winsorized, na.rm = TRUE),
      crawl_only_probability = mean(is_crawl_only, na.rm = TRUE),
      rash_probability = mean(rash_flag, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      bite_month_label = factor(month_levels(), levels = month_levels(), ordered = TRUE),
      fill = list(registrations = 0L, mean_bite_count = NA_real_, crawl_only_probability = NA_real_, rash_probability = NA_real_)
    ) |>
    arrange(bite_month_label)
}

summarise_distribution_diagnostics <- function(analysis_data) {
  bind_rows(
    tibble::tibble(metric = "Bite count", version = "Raw", count_value = analysis_data$bite_count_raw),
    tibble::tibble(metric = "Bite count", version = "Winsorized", count_value = analysis_data$bite_count_winsorized),
    tibble::tibble(metric = "Crawl count", version = "Raw", count_value = analysis_data$crawl_count_raw),
    tibble::tibble(metric = "Crawl count", version = "Winsorized", count_value = analysis_data$crawl_count_winsorized)
  ) |>
    filter(!is.na(count_value))
}

summarise_age_gender_heatmap <- function(analysis_data) {
  registrations <- summarise_registrations_age_gender(analysis_data) |>
    transmute(age_group, gender_clean, metric = "Registrations", value = registrations, label = as.character(registrations))
  mean_bite <- summarise_mean_bite_by_age_gender(analysis_data) |>
    transmute(age_group, gender_clean, metric = "Mean bite count", value = mean_bite_count, label = sprintf("%.2f", mean_bite_count))
  rash_probability <- summarise_rash_by_age_gender(analysis_data) |>
    transmute(age_group, gender_clean, metric = "Rash probability", value = probability, label = percent(probability, accuracy = 0.1))

  bind_rows(registrations, mean_bite, rash_probability) |>
    mutate(
      metric = factor(metric, levels = c("Registrations", "Mean bite count", "Rash probability")),
      gender_clean = factor(gender_clean, levels = c("female", "male")),
      age_group = factor(as.character(age_group), levels = age_group_levels(), ordered = TRUE)
    )
}

figure_theme <- function() {
  theme_minimal(base_size = 17) +
    theme(
      plot.title.position = "plot",
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title = element_text(face = "bold", size = 15.5),
      axis.text = element_text(color = "#2F3B4A", size = 13.2),
      legend.title = element_text(face = "bold", size = 13.5),
      legend.text = element_text(size = 12.4),
      strip.text = element_text(face = "bold", size = 13.6),
      panel.grid.minor = element_line(color = "#E9EEF4"),
      panel.grid.major = element_line(color = "#D6DEE8"),
      legend.position = "right",
      plot.margin = margin(12, 16, 12, 16)
    )
}

panel_tag_theme <- function() {
  theme(
    plot.tag = element_text(face = "bold", size = 18),
    plot.tag.position = c(0.02, 0.98)
  )
}

add_panel_tags <- function(plot_object) {
  plot_object +
    patchwork::plot_annotation(tag_levels = "A") &
    panel_tag_theme()
}

save_plot_and_summary <- function(plot_object, summary_data, output_file) {
  ggplot2::ggsave(
    filename = output_file,
    plot = plot_object,
    width = if (!is.null(attr(summary_data, "plot_width"))) attr(summary_data, "plot_width") else 9.15,
    height = if (!is.null(attr(summary_data, "plot_height"))) attr(summary_data, "plot_height") else 5.91,
    dpi = 220,
    bg = "white"
  )

  summary_path <- file.path(dirname(output_file), paste0(tools::file_path_sans_ext(basename(output_file)), "-summary.csv"))
  utils::write.csv(summary_data, summary_path, row.names = FALSE, na = "")
  list(plot_path = output_file, summary_path = summary_path)
}

cleanup_managed_outputs <- function(output_dir, png_filenames, manifest_filenames) {
  managed_csvs <- c(paste0(tools::file_path_sans_ext(png_filenames), "-summary.csv"), manifest_filenames)
  managed_files <- unique(file.path(output_dir, c(png_filenames, managed_csvs)))
  existing_files <- managed_files[file.exists(managed_files)]
  if (length(existing_files)) file.remove(existing_files)
  invisible(existing_files)
}

cleanup_primary_result_outputs <- function(output_dir) {
  cleanup_managed_outputs(
    output_dir = output_dir,
    png_filenames = c(primary_result_figure_filenames(), legacy_primary_result_figure_filenames()),
    manifest_filenames = c(
      "primary-epidemiological-results-manifest.csv",
      "reference-figure-manifest.csv",
      "reference-figures-manifest.csv",
      primary_result_table_filenames()
    )
  )
}

cleanup_supplementary_analysis_outputs <- function(output_dir) {
  cleanup_managed_outputs(
    output_dir = output_dir,
    png_filenames = c(
      supplementary_analysis_figure_filenames(),
      legacy_supplementary_analysis_figure_filenames()
    ),
    manifest_filenames = c("supplementary-epidemiological-analyses-manifest.csv", "robustness-figures-manifest.csv")
  )
}

write_artifact_manifest <- function(artifacts, output_dir, manifest_filename) {
  manifest_rows <- lapply(
    artifacts,
    function(artifact) {
      output_file <- file.path(output_dir, artifact$filename)
      if (!is.null(artifact$width)) attr(artifact$data, "plot_width") <- artifact$width
      if (!is.null(artifact$height)) attr(artifact$data, "plot_height") <- artifact$height
      saved <- save_plot_and_summary(artifact$plot, artifact$data, output_file)

      data.frame(
        filename = artifact$filename,
        title = artifact$title,
        plot_path = normalizePath(saved$plot_path, winslash = "/", mustWork = TRUE),
        summary_path = normalizePath(saved$summary_path, winslash = "/", mustWork = TRUE),
        row_count = nrow(artifact$data),
        stringsAsFactors = FALSE
      )
    }
  )

  manifest <- bind_rows(manifest_rows)
  manifest_path <- file.path(output_dir, manifest_filename)
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  manifest
}
