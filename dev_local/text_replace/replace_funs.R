### new -----

# Replace text in Word paragraph(s) or PowerPoint shapes.

# get runs
# x : node or nodeset
xml_get_runs <- function(x) {
  xml2::xml_find_all(x, ".//w:p/w:r")
}


xml_get_runs_in_par <- function(x) {
  xml2::xml_find_all(x, ".//w:r")
}


# df <- data.frame(n=1:4, letter = LETTERS[1:4])
# df_row_repeat(df) # identical
# df_row_repeat(df, 1, 1) # no changes
# df_row_repeat(df, 1, 2) # 1st row twice
# df_row_repeat(df, 1, 0) # 1st row removed
df_row_repeat <- function(df, idx = NULL, times = 1) {
  if (is.null(idx) || all(is.na(idx))) {
    return(df)
  }
  ii <- seq_len(nrow(df))
  # if (all(idx %nin% ii)) {
  #   cli::cli_abort("{.arg idx} outside row index range [{.val {min(ii)}}, {.val {max(ii)}}]")
  # }
  before <- df[ii < idx, ]
  repeated <- df[rep(idx, each = times), ]
  after <- df[ii > idx, ]
  rbind(before, repeated, after)
}


find_paragraphs <- function(x, pattern, fixed = FALSE) {
  pars <- xml2::xml_find_all(x, ".//w:p")
  pars_text <- pars|> xml2::xml_text()
  ii <- grepl(pattern = pattern, x = pars_text, fixed = fixed)
  pars[ii]
}


# behaves like stringr::str_locate
str_locate_start_end <- function(x, pattern, ...) {
  match_info <- gregexpr(pattern, x, ...)
  start <- match_info[[1]]
  match_lengths <- attr(match_info[[1]], "match.length")
  end <- start + match_lengths - 1
  cbind(start, end)
}


### old -----

# xml_get_runs <- function(x) {
#   xml2::xml_find_all(x, ".//w:p/w:r")
# }


xml_shape_text_replace_all <- function(shape, pattern = NULL, replacement = NULL, ...) {
  dots <- rlang::dots_list(...)
  pattern <- c(pattern, names(dots))
  dots_replacement <- dots |>
    unlist() |>
    unname() |>
    as.character()
  replacement <- c(replacement, dots_replacement)
  if (length(pattern) != length(replacement)) {
    stop("Length of pattern and replacement must match")
  }
  for (i in seq_along(pattern)) {
    .xml_shape_text_replace_all(shape, pattern[i], replacement[i])
  }
}


xml_replace_all_text_in_par <- function(paragraph, pattern, replacement) {
  all_text <- xml_get_runs_in_par(paragraph) |>
    xml2::xml_text() |>
    paste0(collapse = "")
  n_matches <- stringr::str_count(all_text, pattern = stringr::fixed(pattern))
  cli::cli_alert_info("Replace {.val {n_matches}} times {.val {pattern}} => {.val {replacement}}")
  for (i in seq_len(n_matches)) {
    xml_replace_text_in_par(paragraph, pattern, replacement)
  }
}


xml_replace_text_in_par <- function(paragraph, pattern, replacement) {
  text <- original <- run_idx <- NULL
  # get texts from all runs, concatenate and find pattern
  runs <- xml_get_runs_in_par(paragraph)
  runs_text <- runs |>
    xml2::xml_text() |>
    stats::setNames(paste0("r_", seq_along(runs)))
  text_old <- paste0(runs_text, collapse = "")
  pattern_loc <- stringr::str_locate(text_old, pattern = stringr::fixed(pattern)) |>
    as.list() |>
    stats::setNames(c("from", "to"))
  if (all(is.na(pattern_loc))) {
    cli::cli_alert_info("No pattern to replace.")
    return(invisible(NULL))
  }

  # split up text into chars
  df_runs <- dplyr::tibble(run_idx = seq_along(runs_text), text = runs_text)
  df <- df_runs |>
    dplyr::mutate(original = strsplit(text, "")) |>
    tidyr::unnest(original)
  chars_new <- strsplit(replacement, "") |> unlist()
  n_new <- length(chars_new)
  if (replacement == "") { # handle edge case of zero length replacement
    chars_new <- ""
    n_new <- 1 # keep this row
  }
  # cli::cli_alert_info("Before: {text_old}")

  # 1. find run with start of pattern position
  # 2. delete rest of pattern from the run (and other runs)
  # 3. insert the whole replacement at the position of first pattern character
  # 4. (insert new run with defined run properties) => not implemented
  ii_pattern <- do.call(seq, pattern_loc)
  i_first <- ii_pattern |> utils::head(1)
  ii_remove <- ii_pattern |> utils::tail(-1) # remove all but first character from pattern
  .df <- df
  if (length(ii_remove) > 0) { # one char patterns need no removing of rows
    .df <- .df[-ii_remove, ]
  }
  .df <- df_row_repeat(.df, idx = i_first, times = n_new)
  ii_replace <- seq(i_first, i_first + n_new - 1)
  .df$replacement <- .df$original
  .df$replacement[ii_replace] <- chars_new
  .df_runs <- .df |> dplyr::summarise(
    .by = run_idx,
    dplyr::across(c(original, replacement), glue::glue_collapse)
  )
  df_all <- df_runs |>
    dplyr::left_join(.df_runs, by = "run_idx") |>
    dplyr::mutate(replacement = tidyr::replace_na(replacement, "")) |>
    dplyr::select(-original)
  # text_new <- paste0(df_all$replacement, collapse = "")
  # cli::cli_alert_info("After:  {text_new}")
  xml2::xml_text(runs) <- df_all$replacement # NOTE: empty runs are not deleted currently
}
