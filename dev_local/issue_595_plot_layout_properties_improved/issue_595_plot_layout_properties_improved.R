# https://github.com/davidgohel/officer/issues/595

# 1. Use current slide as default layout
# 2. Allow layout index as alternative to the layout name
# 3. Add placeholder id to plot


#' Layout selection helper
#'
#' Select a layout by name or index. Master name is inferred and only required
#' for disambiguation in case of several masters.
#'
#' @param x An `rpptx` object.
#' @param layout Layout name or index, see [layout_summary].
#' @param master Name of master. Only required if layout name is not unique across masters.
#' @keywords internal
#' @return A list with layout's index, name, and master.
#' @examples
#' x <- read_pptx()
#' get_layout(x, "Title Slide")
#'
get_layout <- function(x, layout, master = NULL) {
  if (!(is.numeric(layout) || is.character(layout))) {
    cli::cli_abort("{.arg layout} must be numeric or character, not {.val {class(layout)[1]}}")
  }
  df <- layout_summary(x)
  n_layouts <- nrow(df)

  if (n_layouts == 0) {
    cli::cli_alert_danger("No layouts available.")
    return(NULL)
  }

  if (is.numeric(layout)) {
    index <- layout
    if (!index %in% seq_len(n_layouts)) {
      cli::cli_abort("Layout with index {.val {index}} does not exist. Must be between {.val {1}} and {.val {n_layouts}}.")
    }
    l <- df[index, ] |> as.list()
    return(c(index = index, l))
  } else {
    layout_exists(x, layout, must_exist = TRUE)
    layout_is_unique(x, layout, require_unique = TRUE)
    index <- which(df$layout == layout)
  }

  l <- df[index, ] |> as.list()
  return(c(index = index, l))
}


get_layout_names <- function(x) {
  layout_summary(x)$layout  # layout_summary as also used in print.rpptx
}


# find similar name
find_similar_names <- function(name, names, n = 3) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return(NULL)
  }
  l <- stringdist::afind(names, name) # search for similar names
  d <- l$distance |> as.vector()
  ii <- order(d)
  ii <- utils::head(ii, n = n)
  # paste(ii, "=", names[ii])
  names[ii]
}


# name: layout name
layout_exists <- function(x, name, must_exist = FALSE) {
  name <- as.character(name)
  layouts <- get_layout_names(x)
  exists <- name %in% layouts
  if (!must_exist) {
    return(exists)
  }
  if (!exists) {
    similar <- find_similar_names(name, layouts)
    if (is.null(similar)) {
      cli::cli_abort("Layout {.val {name}} does not exists.")
    }
    cli::cli_abort("Layout {.val {name}} does not exists. Did you mean {.or {.val {similar}}}. See {.fn layout_summary} for available layouts")
  }
  exists
}


layout_is_unique <- function(x, name, require_unique = FALSE, check_exists = FALSE) {
  if (check_exists) {
    layout_exists(x, name, must_exist = TRUE)
  }
  df_all <- layout_summary(x)
  df <- subset(df_all, layout == name)
  name_is_unique <- nrow(df) == 1
  if (!name_is_unique && require_unique) {
    cli::cli_abort("Layout {.val {name}} is not unique. It exists in {.val {length(df$master)}} masters: {.val {df$master}}")
  }
  name_is_unique
}

