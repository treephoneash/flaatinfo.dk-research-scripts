suppressPackageStartupMessages({
  library(stringr)
})

chapter_paths <- c(
  "thesis/book/02-introduction.Rmd",
  "thesis/book/03-methods.Rmd",
  "thesis/book/04-results.Rmd",
  "thesis/book/05-discussion.Rmd"
)

banned_phrases <- c(
  "it is important to note that",
  "it cannot be overstated that",
  "this is a testament to",
  "at its core",
  "in today's fast-paced",
  "not only",
  "as we navigate",
  "plays a crucial role",
  "sheds light on",
  "in the realm of",
  "when it comes to",
  "in light of the above",
  "moving forward",
  "it is widely recognized that",
  "a comprehensive analysis reveals",
  "it should be pointed out that",
  "this speaks to the broader",
  "is a key driver of"
)

flagged_lexicon <- c(
  "robust",
  "framework",
  "crucial",
  "notably",
  "arguably",
  "landscape",
  "nuanced",
  "pivotal",
  "holistic",
  "multifaceted"
)

stock_transitions <- c(
  "moreover",
  "furthermore",
  "additionally"
)

read_utf8_lines <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

strip_code_chunks <- function(lines) {
  in_chunk <- FALSE
  kept <- logical(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]
    if (grepl("^```", line)) {
      in_chunk <- !in_chunk
      next
    }
    kept[i] <- !in_chunk
  }

  lines[kept]
}

collect_regex_hits <- function(lines, pattern, label, ignore_case = TRUE) {
  hit_index <- grep(pattern, lines, ignore.case = ignore_case, perl = TRUE)
  if (!length(hit_index)) {
    return(data.frame())
  }

  data.frame(
    line = hit_index,
    issue_type = label,
    text = lines[hit_index],
    stringsAsFactors = FALSE
  )
}

lint_file <- function(path) {
  lines <- strip_code_chunks(read_utf8_lines(path))
  lower_lines <- tolower(lines)

  phrase_hits <- do.call(
    rbind,
    lapply(
      banned_phrases,
      function(phrase) {
        hit_index <- which(str_detect(lower_lines, fixed(phrase)))
        if (!length(hit_index)) return(data.frame())
        data.frame(
          line = hit_index,
          issue_type = "banned_phrase",
          detail = phrase,
          text = lines[hit_index],
          stringsAsFactors = FALSE
        )
      }
    )
  )

  lexicon_hits <- do.call(
    rbind,
    lapply(
      flagged_lexicon,
      function(word) {
        pattern <- paste0("\\b", stringr::str_replace_all(word, "([\\W])", "\\\\\\1"), "\\b")
        hit_index <- grep(pattern, lower_lines, perl = TRUE)
        if (!length(hit_index)) return(data.frame())
        data.frame(
          line = hit_index,
          issue_type = "flagged_lexicon",
          detail = word,
          text = lines[hit_index],
          stringsAsFactors = FALSE
        )
      }
    )
  )

  transition_hits <- do.call(
    rbind,
    lapply(
      stock_transitions,
      function(word) {
        pattern <- paste0("^\\s*", word, "\\b")
        hit_index <- grep(pattern, lower_lines, perl = TRUE)
        if (!length(hit_index)) return(data.frame())
        data.frame(
          line = hit_index,
          issue_type = "stock_transition",
          detail = word,
          text = lines[hit_index],
          stringsAsFactors = FALSE
        )
      }
    )
  )

  pronoun_hits <- rbind(
    collect_regex_hits(lines, "\\b(I|we|our|ours|us|my|mine)\\b", "first_person"),
    collect_regex_hits(lines, "\\b(you|your|yours)\\b", "second_person")
  )
  if (nrow(pronoun_hits)) {
    pronoun_hits$detail <- ""
  }

  bullet_lines <- grep("^\\s*[-*]\\s+", lines, perl = TRUE)
  bullet_count <- length(bullet_lines)
  numbered_lines <- grep("^\\s*[0-9]+\\.\\s+", lines, perl = TRUE)
  numbered_count <- length(numbered_lines)

  sentence_candidates <- unlist(strsplit(paste(lines, collapse = " "), "(?<=[.!?])\\s+", perl = TRUE))
  sentence_candidates <- trimws(sentence_candidates)
  sentence_candidates <- sentence_candidates[nzchar(sentence_candidates)]
  sentence_lengths <- str_count(sentence_candidates, boundary("word"))

  summary_row <- data.frame(
    file = path,
    line_count = length(lines),
    banned_phrase_hits = if (is.null(nrow(phrase_hits))) 0 else nrow(phrase_hits),
    flagged_lexicon_hits = if (is.null(nrow(lexicon_hits))) 0 else nrow(lexicon_hits),
    stock_transition_hits = if (is.null(nrow(transition_hits))) 0 else nrow(transition_hits),
    pronoun_hits = if (is.null(nrow(pronoun_hits))) 0 else nrow(pronoun_hits),
    bullet_lines = bullet_count,
    numbered_lines = numbered_count,
    mean_sentence_length = if (length(sentence_lengths)) round(mean(sentence_lengths), 2) else NA_real_,
    sd_sentence_length = if (length(sentence_lengths) > 1) round(stats::sd(sentence_lengths), 2) else NA_real_,
    stringsAsFactors = FALSE
  )

  issues <- rbind(
    if (!is.null(phrase_hits)) phrase_hits else data.frame(),
    if (!is.null(lexicon_hits)) lexicon_hits else data.frame(),
    if (!is.null(transition_hits)) transition_hits else data.frame(),
    if (!is.null(pronoun_hits)) pronoun_hits else data.frame()
  )

  if (nrow(issues)) {
    issues$file <- path
    issues <- issues[, c("file", "line", "issue_type", "detail", "text")]
  }

  list(summary = summary_row, issues = issues)
}

dir.create(file.path("derived", "writing_qa"), recursive = TRUE, showWarnings = FALSE)

lint_results <- lapply(chapter_paths, lint_file)
summary_table <- do.call(rbind, lapply(lint_results, `[[`, "summary"))
issue_list <- lapply(lint_results, `[[`, "issues")
issue_list <- Filter(function(x) nrow(x) > 0, issue_list)
issues_table <- if (length(issue_list)) do.call(rbind, issue_list) else data.frame()

summary_path <- file.path("derived", "writing_qa", "academic_style_summary.csv")
issues_path <- file.path("derived", "writing_qa", "academic_style_issues.csv")
report_path <- file.path("derived", "writing_qa", "academic_style_report.md")

utils::write.csv(summary_table, summary_path, row.names = FALSE, na = "")
if (nrow(issues_table)) {
  utils::write.csv(issues_table, issues_path, row.names = FALSE, na = "")
} else {
  utils::write.csv(data.frame(), issues_path, row.names = FALSE, na = "")
}

report_lines <- c(
  "# Academic Style QA Report",
  "",
  paste0("Generated on: ", Sys.time()),
  "",
  "## Scope",
  "- English analytical chapters only (`Introduction`, `Methods`, `Results`, `Discussion`).",
  "- Greek front matter and regulation-driven declarations excluded.",
  "",
  "## Summary",
  ""
)

for (i in seq_len(nrow(summary_table))) {
  row <- summary_table[i, ]
  report_lines <- c(
    report_lines,
    paste0("### ", row$file),
    paste0("- banned phrase hits: ", row$banned_phrase_hits),
    paste0("- flagged lexicon hits: ", row$flagged_lexicon_hits),
    paste0("- stock transition hits: ", row$stock_transition_hits),
    paste0("- first/second person hits: ", row$pronoun_hits),
    paste0("- bullet lines: ", row$bullet_lines),
    paste0("- numbered lines: ", row$numbered_lines),
    paste0("- mean sentence length: ", row$mean_sentence_length),
    paste0("- sentence-length SD: ", row$sd_sentence_length),
    ""
  )
}

if (nrow(issues_table)) {
  report_lines <- c(report_lines, "## Flagged Lines", "")
  for (i in seq_len(nrow(issues_table))) {
    row <- issues_table[i, ]
    report_lines <- c(
      report_lines,
      paste0("- ", row$file, ":", row$line, " [", row$issue_type, ifelse(nchar(row$detail), paste0(" / ", row$detail), ""), "] ", row$text)
    )
  }
} else {
  report_lines <- c(report_lines, "## Flagged Lines", "", "- No flagged lines found by the current rule set.")
}

writeLines(enc2utf8(report_lines), report_path, useBytes = TRUE)

message("Academic style summary written to: ", normalizePath(summary_path, winslash = "/", mustWork = TRUE))
message("Academic style issues written to: ", normalizePath(issues_path, winslash = "/", mustWork = TRUE))
message("Academic style report written to: ", normalizePath(report_path, winslash = "/", mustWork = TRUE))
