# functions for ph annotation


#' Annotate a slide's phs with type, label, and id
#'
#' Draws a box with ph info (type + type index, label, and id) at one or more placeholders. While
#' [plot_layout_properties()] generates an image of the slide layout, [phs_annotate()] modifies the presentation
#' object.
#'
#' @param x A `rpptx` object.
#' @param ... Subset of phs to annotate (optional). Either short form location syntax (see [phs_with()] for details) or a
#'   [ph_location()] object.
#' @param .slide_idx Indexes of slides to process. Default (`NULL`) is the current slide. Use `"all"` to process all
#'   slides.
#' @param .font_size Font size. `NA` (default) is the ph's default font size. Use a named vector of
#'   length 3 for different font sizes (e.g. `c(label = 10, type = NA, id = 15)`). Set `0` to omit
#' @param .font_color Font color(s). `NA` (default) is the ph's default color. Use a named vector of
#'   length 3 for different colors (e.g. `c(label = "red", type = "blue", id = "darkgreen")`).
#' @param .bg Background color as hex value or valid R color.
#' @param .line Line around placeholder. Either a [sp_line()] or a single color value (hex or valid R color).
#' @param .keys Add keys to info (`id=, type=, label=`) (default is `TRUE`).
#' @example inst/examples/example_phs_annotate.R
#' @seealso [plot_layout_properties()]
#' @family ppt_info
#' @export
phs_annotate <- function(x, ..., .slide_idx = NULL, .font_size = NA, .font_color = NA,
                         .keys = TRUE, .bg = NA, .line = sp_line(lwd = 1, color = "grey", lty = "dash")) {
  stop_if_not_rpptx(x)
  dots <- list(...)

  .slide_idx <- .slide_idx %||% x$cursor # default is current slide
  if (is.character(.slide_idx) && .slide_idx == "all") {
    .slide_idx <- seq_len(length(x))
  }
  stop_if_not_in_slide_range(x, .slide_idx)

  .old_cursor <- x$cursor
  for (slide_idx in .slide_idx) {
    x$cursor <- slide_idx # ph_annotate uses current slide
    if (length(dots) == 0) {
      la <- get_layout_for_current_slide(x)
      prop <- layout_properties(x, la$layout_name, la$master_name)
      locations <- as.list(prop$id)
    } else {
      loc_strings <- as.list(dots)
      ii <- grepl("^\\d+$", loc_strings) # find integer short-forms
      loc_strings[ii] <- as.integer(loc_strings[ii])
      locations <- lapply(loc_strings, resolve_location)
    }
    for (i in seq_along(locations)) {
      x <- ph_annotate(x,
        location = locations[[i]], font_size = .font_size, font_colors = .font_color,
        line = .line, bg = .bg, keys = .keys, z_position = "back", inform = TRUE
      )
    }
  }
  x$cursor <- .old_cursor
  x
}


#' Annotate a slide ph with type, label, and id info.
#'
#' Draws a box with ph info (type + type index, label, and id) for *one* placeholder.
#'
#' @param x A `rpptx` object.
#' @param location Placeholders location to annotate. Either short form location syntax (see [phs_with()] for details) or a
#'   [ph_location()] object.
#' @param font_size Font size. `NA` (default) is the ph's default font size. Use a named vector of
#'   length 3 for different font sizes (e.g. `c(label = 10, type = NA, id = 15)`).
#' @param font_color Font color(s). `NA` (default) is the ph's default color. Use a named vector of
#'   length 3 for different colors (e.g. `c(label = "red", type = "blue", id = "darkgreen")`).
#' @param bg Background color as hex value or valid R color.
#' @param line Line around placeholder. Either a [sp_line()] or a single color value (hex or valid R color).
#' @param keys Add keys to info (`id=, type=, label=`) (default is `TRUE`).
#' @param z_position description  z_position: Place boxes in the `"back"` (default) or `"front"`. Only relevant if ph has already been filled.
#' @param inform: Inform user if a placeholder from `...` was not found.
#' @noRd
ph_annotate <- function(x, location, font_size = NULL,
                        font_colors = NULL, keys = TRUE, bg = NULL,
                        line = sp_line(lwd = 1, color = "grey", lty = "dash"),
                        z_position = "back", inform = TRUE, ...) {
  font_colors <- update_named_defaults(font_colors,
    default = list(label = NA, type = NA, id = NA), # NA to used default font color
    argname = "font_color", as_list = TRUE
  )

  font_size <- update_named_defaults(font_size,
    default = list(label = NA, type = NA, id = NA), # NB: 0 will use the ph's default font size
    argname = "font_size", as_list = TRUE
  )

  line <- line %||% NA
  bg <- bg %||% NA

  slide <- x$slide$get_slide(x$cursor)
  loc_ <- resolve_location(location)
  loc <- tryCatch(fortify_location(loc_, doc = x), error = function(e) e)
  if (inherits(loc, "error")) {
    if (inform) cli::cli_alert_info("Skipping location {.val {loc_to_text(loc_)}}. Not found on slide {x$cursor}")
    return(x)
  }

  # build info text
  fp_text_type <- fp_text_lite(color = font_colors$type, font.size = font_size$type)
  fp_text_label <- fp_text_lite(color = font_colors$label, font.size = font_size$label)
  fp_text_id <- fp_text_lite(color = font_colors$id, font.size = font_size$id)

  info_type_key <- ftext(ifelse(keys, "type=", ""), prop = fp_text_type)
  info_type <- ftext(mini_glue("{loc$type}[{loc$type_idx}]"), prop = fp_text_type)
  info_type_sep <- ftext(", ", prop = fp_text_type)

  info_label_key <- ftext(ifelse(keys, "label=", ""), prop = fp_text_label)
  info_label <- ftext(mini_glue("{loc$ph_label}"), prop = fp_text_label)
  info_label_sep <- ftext(", ", prop = fp_text_label)

  info_id_key <- ftext(ifelse(keys, "id=", ""), prop = fp_text_id)
  info_id <- ftext(mini_glue("{loc$ph_id}"), prop = fp_text_id)

  l_ft <- list(
    info_type_key, info_type, info_type_sep,
    info_label_key, info_label, info_label_sep,
    info_id_key, info_id
  )
  l_ft_null <- lapply(l_ft, function(x) if (isTRUE(x$pr$font.size == 0)) NULL else x)
  l_ft_compact <- Filter(Negate(is.null), l_ft_null)
  if (length(l_ft_compact) == 3) {
    l_ft_compact <- l_ft_compact[-3]
  }
  fp <- do.call(fpar, l_ft_compact)

  value <- block_list(fp)
  pars <- sapply(value, to_pml)
  pars <- paste0(pars, collapse = "")
  if (!inherits(line, "sp_line") && is.color(line)) {
    line <- sp_line(color = line, lty = "dash")
  }
  new_ph <- shape_properties_tags(
    left = loc$left, top = loc$top,
    width = loc$width, height = loc$height,
    label = loc$ph_label, ph = loc$ph,
    rot = 0, bg = bg, ln = line, geom = NULL
  )

  xml_elt <- paste0(psp_ns_yes, new_ph, "<p:txBody><a:bodyPr/><a:lstStyle/>", pars, "</p:txBody></p:sp>")

  node <- as_xml_document(xml_elt)
  shp_tree <- xml_find_first(slide$get(), "//p:spTree")
  if (z_position == "front") {
    xml_add_child(shp_tree, node)
  } else {
    # Place after grpSpPr:
    # => OOXML the <p:spTree> element has a strict content-model (child-order) defined by the ECMA-376 spec.
    #  One cannot drop <p:sp> (shape) in at the top – the first two children must be:
    #    <p:nvGrpSpPr> (non-visual group properties)
    #    <p:grpSpPr> (group transform properties)
    grpPr <- xml_find_first(shp_tree, "./p:grpSpPr")
    xml_add_sibling(grpPr, node, .where = "after") # xml_add_child(shp_tree, node, .where = 2)
  }
  x
}


loc_to_text <- function(location) {
  cls <- class(location)[1]
  switch(cls,
    "location_id" = paste0("ph_id = ", location$ph_id),
    "location_label" = paste0("ph_label = ", location$ph_label),
    "location_type" = paste0("ph_type = ", location$type, "[", location$type_idx, "]"),
    "location_fullsize" = "fullsize",
    "location_left" = "left",
    "location_right" = "right",
    "location_manual" = "manual",
    "location_template" = "template",
    "<unknown>"
  )
}


#' Add layout slide with annotated placeholders
#'
#' Generates a new slide with annotated placeholders (`label`, `id`, `type + type index`)
#' for each layout. This overview is helpful when calling [phs_with()], [ph_with()], or
#' the `ph_location_*()` functions.
#'
#' @param x A `rpptx` object.
#' @param layouts Names or indexes of layouts to annotate. If `NULL` (default), all layouts are used.
#' @param remove_slides Remove already existing slides? (default `FALSE`). If `TRUE`, will only keep annotated layouts.
#' @param ... Passed on to [phs_annotate()] for finetuning placeholder's appearance.
#' @return `rpptx` object with annotated layout slides added.
#' @family ppt_info
#' @export
add_annotated_layouts <- function(x, layouts = NULL, remove_slides = FALSE, ...) {
  stop_if_not_rpptx(x)

  if (remove_slides) {
    while (length(x) > 0) {
      x <- remove_slide(x, 1)
    }
  }

  df <- layout_summary(x)
  nr <- nrow(df)
  if (nr == 0) {
    cli::cli_alert_warning("No layouts. Nothing to annotate")
    return(x)
  }

  if (is.character(layouts)) {
    non_existent <- setdiff(layouts, df$layout)
    if (length(non_existent) > 0) {
      cli::cli_abort("Layout does not exist: {.val {non_existent}}")
    }
    layout_ii <- match(layouts, df$layout)
  } else {
    layout_ii <- layouts %||% seq_len(nr)
    out_of_range_index <- setdiff(layout_ii, seq_len(nr))
    if (length(out_of_range_index) > 0) {
      cli::cli_abort(c(
        "Index for {.arg layouts} out of range: {.val {out_of_range_index}}",
        "Choose an index in the range [1, {nr}]"
      ))
    }
  }

  for (i in layout_ii) {
    layout <- df$layout[i]
    master <- df$master[i]
    x <- add_slide(x, layout = layout, master = master)
    lp <- layout_properties(x = x, layout = layout, master = master)
    size <- slide_size(x)
    fpar_ <- fpar(sprintf('layout = "%s", master = "%s"', layout, master),
      fp_t = fp_text(color = "orange", font.size = 12),
      fp_p = fp_par(text.align = "center", padding = 0)
    )
    ppt <- ph_with(
      x = x, value = fpar_, ph_label = "layout_and_master",
      location = ph_location(
        left = 0, top = 0, width = size$width, height = .25,
        bg = "transparent", newlabel = "layout_and_master"
      )
    )
  }
  phs_annotate(x, .slide_idx = "all", ...)
}


#' @title Layout slides with annotated placeholders
#' @description Generates a slide with annotated placeholders (`label`, `id`, `type + type index`)
#' for each layout of the pptx file. This placeholder overview is helpful when calling [phs_with()],
#' [ph_with()], or the `ph_location_*()` functions.
#' @param path Path to pptx file. If `NULL` (default), `officer`'s default presentation is used.
#' @param output_file Path to save annotated presentation. Use `NULL` to suppress file generation.
#' @param layouts Names or indexes of layouts to annotate. If `NULL` (default), all layouts are used.
#' @param ... Passed on to [add_annotated_layouts()] for finetuning appearance.
#' @return `rpptx` object with annotated layouts.
#' @family ppt_info
#' @export
#' @example inst/examples/example_annotate_base.R
#'
annotate_base <- function(path = NULL, output_file = "annotated_layouts.pptx", layouts = NULL, ...) {
  x <- read_pptx(path = path)
  x <- add_annotated_layouts(x, layouts = layouts, remove_slides = TRUE, ...)
  if (!is.null(output_file)) {
    print(x, target = output_file)
    return(invisible(x)) # invisibly if saved
  }
  x
}
