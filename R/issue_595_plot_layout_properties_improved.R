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

  masters <- x$masterLayouts$names()
  if (length(masters) == 1 && is.null(master)) {
    master <- masters
  }

  if (!is.null(master)) {
    master_exists(x, master, must_exist = TRUE)
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
    l <- layout_uniqueness(x, layout)
    if (!l$is_unique && is.null(master)) {
      cli::cli_abort("Please specify {.arg master} as layout {.val {l$layout}} is not unique. Exists in {.val {length(l$in_master)}} masters: {.val {l$in_master}}.")
    }
    if (!master %in% l$in_master) {
      cli::cli_abort("Layout {.val {l$layout}} not available in master {.val {master}}. Only in {.val {l$in_master}}.")
    }
    index <- which(df$layout == layout & df$master == master)
  }

  l <- df[index, ] |> as.list()
  return(c(index = index, l))
}



# Find most similar strings
#
# Helpful for providing info avout similar layouts or master names
# in case of a typo.
#
# get_most_similar("a", c("aa", "bb", "ba", "cab", "d"))
#
get_most_similar <- function(x, other, n = 3) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return(NULL)
  }
  other <- unique(other)
  l <- stringdist::afind(other, x) # search for similar names
  d <- l$distance |> as.vector()
  ii <- order(d)
  ii <- utils::head(ii, n = n)
  other[ii]
}


# Check if master exists
#
# x: rpptx object
# name: name of master
# must_exist: If TRUE, throws error if name does not exist
#
# return:
master_exists <- function(x, name, must_exist = FALSE) {
  masters <- x$masterLayouts$names()
  exists <- name %in% masters
  if (!must_exist) {
    return(exists)
  }
  if (!exists) {
    similar <- get_most_similar(name, masters)
    if (is.null(similar)) {
      cli::cli_abort("Master {.val {name}} does not exists. See {.fn layout_summary} for available masters.")
    }
    cli::cli_abort("Master {.val {name}} does not exists. See {.fn layout_summary} for available masters. Did you mean {.or {.val {similar}}}? ")
  }
  exists
}


# name: layout name
layout_exists <- function(x, name, must_exist = FALSE) {
  layouts <- layout_summary(x)$layout # layout_summary as also used in print.rpptx
  exists <- name %in% layouts
  if (!must_exist) {
    return(exists)
  }
  if (!exists) {
    similar <- get_most_similar(name, layouts)
    if (is.null(similar)) {
      cli::cli_abort("Layout {.val {name}} does not exists. See {.fn layout_summary} for available layouts.")
    }
    cli::cli_abort("Layout {.val {name}} does not exists. See {.fn layout_summary} for available layouts. Did you mean {.or {.val {similar}}}? ")
  }
  exists
}


# check if layout is unique
#
# may not be the case if with multiple masters and same layout names
#
# x: rpptx
# name: layout name
#
# return: list with results
# examples:
#   file <- system.file("doc_examples", "three_masters.pptx", package = "officer")
#   x <- read_pptx(file)
#   layout_uniqueness(x, "Title Slide")
#
layout_uniqueness <- function(x, name, require_unique = FALSE) {
  df_all <- layout_summary(x)
  df <- subset(df_all, layout == name)
  layout_is_unique <- nrow(df) == 1
  if (!layout_is_unique && require_unique) {
    cli::cli_abort("Layout {.val {name}} is not unique. It exists in {.val {length(df$master)}} masters: {.val {df$master}}")
  }
  list(layout = name, is_unique = layout_is_unique, in_master = df$master)
}
