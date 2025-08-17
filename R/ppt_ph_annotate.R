

#' Annotate a slide's phs with type, label, and id
#'
#' Draws a box with ph info (type, label, and id) at one or more placeholder position. While [plot_layout_properties()]
#' generates an image of the slide layout, [phs_annotate()] modifies the presentation object.
#'
#' Throws a warning if one or more phs cannot be found.
#'
#' @param x A `rpptx` object.
#' @param ... Placeholders to annotate. Either short form location syntax (see [phs_with()] for details) or a
#'   [ph_location()] object.
#' @param .slide_idx Indexes of slides to process. Default (`NULL`) is the current slide. `"all"` will process all
#'   slides.
#' @param .font_size Named vector with font sizes for `labels`, `type`, and `id`. Default is `12`. A single numeric
#'   value will apply to all three parameters. Matching by position and partial name matching is supported. `NA` will
#'   use the  ph's default font size.
#' @param .font_color Named vector of font colors for `labels`, `type`, and `id`. Default is `c(label = "red", type =
#'   "blue", id = "darkgreen")`.  A single numeric value will apply to all three parameters. Matching by position and
#'   partial name matching is supported. `NA` will use the  ph's default color.
#' @param .bg Background color as hex value or valid R color.
#' @param .line Line around placeholder. Either a [sp_line()] or a single color value (hex or valid R color).
#' @param .keys Add keys to info (`id=, type=, label=`) (default is `TRUE`).
#' @param .z_position Place boxes in the `"back"` (default) or `"front"`.
#' @param .inform Inform if a placeholder in [...] was not found.
#' @example inst/examples/example_phs_annotate.R
#' @seealso [plot_layout_properties()]
#' @export
phs_annotate <- function(x, ..., .slide_idx = NULL, .font_size = 12,
                         .font_color = c(label = "red", type = "blue", id = "darkgreen"),
                         .bg = "#0000FF10", .line = sp_line(lwd = 1, color = "blue", lty = "dash"),
                         .keys = TRUE, .z_position = "back",
                         .inform = TRUE) {
  stop_if_not_rpptx(x)
  dots <- list(...)
  # .all <- TRUE
  # dots <- utils::modifyList(dots_list, .dots %||% list())
  # if (length(dots) == 0 && isFALSE(.all)) {
  #   return(x)
  # }
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
        line = .line, bg = .bg, keys = .keys, z_position = .z_position, inform = .inform
      )
    }
  }
  x$cursor <- .old_cursor
  x
}


ph_annotate <- function(x, location, z_position = "back", font_size = 12,
                        font_colors = c(label = "red", type = "blue", id = "darkgreen"),
                        keys = TRUE, bg = "#0000FF10", line = sp_line(lwd = 1, color = "blue", lty = "dash"),
                        inform = TRUE, ...) {
  font_colors <- update_named_defaults(font_colors,
                                       default = list(label = "red", type = "blue", id = "darkgreen"),
                                       argname = "font_color"
  )
  font_size <- update_named_defaults(font_size,
                                     default = list(label = 12, type = 12, id = 12),
                                     argname = "font_size", as_list = FALSE
  )
  font_size <- ifelse(is.na(unlist(font_colors)), rep(0, 3), font_size) # avoid different sizes if color is NA
  font_size <- as.list(font_size)

  slide <- x$slide$get_slide(x$cursor)
  loc_ <- resolve_location(location)
  loc <- tryCatch(fortify_location(loc_, doc = x), error = function(e) e)
  if (inherits(loc, "error")) {
    if (inform) cli::cli_alert_info("Skip location {.val {.loc_to_text(loc_)}}. Not found on slide {x$cursor}")
    return(x)
  }
  font_size <- font_size %||% 0 # 0 will use the ph's default font size
  fp <- with(loc, {
    fpar(
      ftext(ifelse(keys, "type=", ""), prop = fp_text(font.size = font_size$type)),
      ftext(mini_glue("{type}[{type_idx}]"), prop = fp_text(color = font_colors$type, font.size = font_size$type)),
      ftext(ifelse(keys, ", label=", ", "), prop = fp_text(font.size = font_size$label)),
      ftext(mini_glue("{ph_label}"), prop = fp_text(color = font_colors$label, font.size = font_size$label)),
      ftext(ifelse(keys, ", id=", ", "), prop = fp_text(font.size = font_size$id)),
      ftext(mini_glue("{ph_id}"), prop = fp_text(color = font_colors$id, font.size = font_size$id))
    )
  })

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


.loc_to_text <- function(location) {
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
         "<unknown>")
}


# Presentation with all layouts and annotated placeholders
annotate_layouts <- function(path = NULL, .font_size = NA, .keys = TRUE, .bg = "#0000FF10",
                             .font_color = c(label = "red", type = "blue", id = "darkgreen"),
                             .line = sp_line(lwd = 1, color = "blue", lty = "dash")) {
  x <- read_pptx(path = path)
  df <- layout_summary(x)
  nr <- nrow(df)
  if (nr == 0) {
    cli::cli_alert_warning("No layouts. Nothing to annotate")
    return(x)
  }
  for (i in seq_len(nr)) {
    layout <- df$layout[i]
    master <- df$master[i]
    x <- add_slide(x, layout = layout, master = master)
    lp <- layout_properties(x = x, layout = layout, master = master)
    size <- slide_size(x)
    fpar_ <- fpar(sprintf('layout = "%s", master = "%s"', layout, master),
                  fp_t = fp_text(color = "orange", font.size = 12),
                  fp_p = fp_par(text.align = "right", padding = 0)
    )
    ppt <- ph_with(x = x, value = fpar_, ph_label = "layout_ph",
                   location = ph_location(left = 0, top = 0, width = size$width, height = .5,
                                          bg = "transparent", newlabel = "layout_ph"))
  }
  phs_annotate(x,
    .slide_idx = "all", .font_size = .font_size, .font_color = .font_color,
    .keys = .keys, .bg = .bg, .line = .line
  )
}
