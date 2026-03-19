#' @export
#' @title Concatenate PowerPoint presentations
#' @description Append slides from one or more source presentations to a target
#' presentation. The result keeps the target's template, properties, table styles,
#' and slide size. Slides from source presentations are appended in order.
#'
#' Source layouts are matched to the target by layout name and master name. If a
#' source slide uses a layout not present in the target, the layout and its
#' master/theme are automatically copied from the source into the target.
#'
#' @param target an `rpptx` object whose template is kept.
#' @param ... one or more `rpptx` objects whose slides are appended.
#' @return A new `rpptx` object with target slides followed by all source slides.
#'   The cursor is set to the last slide. The input objects (`target` and
#'   sources) are not modified.
#' @example inst/examples/example_pptx_concat.R
#' @seealso [read_pptx()], [add_slide()]
#' @family slide_manipulation
pptx_concat <- function(target, ...) {
  stop_if_not_rpptx(target, "target")

  sources <- list(...)
  if (length(sources) == 0L) {
    return(target)
  }

  for (i in seq_along(sources)) {
    if (!inherits(sources[[i]], "rpptx")) {
      cli::cli_abort(
        "All arguments in {.arg ...} must be {.cls rpptx} objects, but element {i} is {.cls {class(sources[[i]])[1]}}."
      )
    }
  }

  # Warn if slide sizes differ
  target_size <- slide_size(target)
  for (i in seq_along(sources)) {
    src_size <- slide_size(sources[[i]])
    if (!isTRUE(all.equal(target_size, src_size, tolerance = 1e-6))) {
      cli::cli_warn(
        "Slide size of source {i} ({round(src_size$width, 3)} x {round(src_size$height, 3)}) differs from
         target ({round(target_size$width, 3)} x {round(target_size$height, 3)}).
         Target slide size is kept."
      )
    }
  }

  # Save target and sources to temp files so in-memory XML changes are flushed
  # to disk and we work on copies (avoiding mutation of the caller's objects)
  layout_default <- target$layout_default
  tmp_target <- tempfile(fileext = ".pptx")
  print(target, target = tmp_target)
  target <- read_pptx(tmp_target)
  target$layout_default <- layout_default

  sources <- lapply(sources, function(src) {
    tmp <- tempfile(fileext = ".pptx")
    print(src, target = tmp)
    read_pptx(tmp)
  })

  # Copy missing layouts/masters/themes from sources into target
  target <- copy_missing_layouts(target, sources)

  # Build target layout mapping: (layout_name, master_name) -> layout_filename
  target_layout_meta <- target$slideLayouts$get_metadata()

  for (i in seq_along(sources)) {
    target <- concat_one_source(target, sources[[i]], target_layout_meta, source_index = i)
  }

  # Save and reload to get clean R6 state
  layout_default <- target$layout_default
  tmp <- tempfile(fileext = ".pptx")
  print(target, target = tmp)
  result <- read_pptx(tmp)
  result$cursor <- result$slide$length()
  result$layout_default <- layout_default

  result
}


#' @export
#' @method c rpptx
#' @title Concatenate rpptx objects
#' @description S3 method that allows using `c()` to concatenate `rpptx` objects.
#' The first argument is used as the target (its template and properties are kept),
#' and all subsequent presentations' slides are appended.
#' @param ... `rpptx` objects to concatenate.
#' @return A new `rpptx` object with slides from all inputs. The input objects
#'   are not modified.
#' @examples
#' pptx1 <- read_pptx()
#' pptx1 <- add_slide(pptx1, layout = "Title and Content")
#'
#' pptx2 <- read_pptx()
#' pptx2 <- add_slide(pptx2, layout = "Title and Content")
#'
#' result <- c(pptx1, pptx2)
#' length(result) # 2
#' @seealso [pptx_concat()]
c.rpptx <- function(...) {
  dots <- list(...)
  if (length(dots) < 1L) {
    cli::cli_abort("At least one {.cls rpptx} object must be provided.")
  }
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }
  do.call(pptx_concat, dots)
}


# Internal: append slides from one source to target
concat_one_source <- function(target, source, target_layout_meta, source_index) {
  n_source_slides <- length(source)
  if (n_source_slides == 0L) {
    return(target)
  }

  # Build layout mapping: source layout filename -> target layout filename
  source_layout_meta <- source$slideLayouts$get_metadata()
  layout_map <- build_layout_map(source_layout_meta, target_layout_meta, source_index)

  # Get source slides in presentation order
  source_slide_data <- source$presentation$slide_data()
  source_pkg <- source$package_dir
  target_pkg <- target$package_dir

  # Ensure target media directory exists
  dir.create(file.path(target_pkg, "ppt/media"), showWarnings = FALSE, recursive = TRUE)

  # Track slide filename mapping for notes
  slide_file_map <- character(0) # source_slide -> target_slide

  for (row_i in seq_len(nrow(source_slide_data))) {
    src_slide_file <- basename(source_slide_data$target[row_i])

    # Get new slide filename in target
    new_slide_name <- target$slide$get_new_slidename()
    new_slide_path <- file.path(target_pkg, "ppt/slides", new_slide_name)

    # Copy slide XML
    file.copy(
      file.path(source_pkg, "ppt/slides", src_slide_file),
      new_slide_path,
      copy.mode = FALSE
    )

    # Read source slide's .rels and build new .rels for target
    src_rels_path <- file.path(source_pkg, "ppt/slides/_rels", paste0(src_slide_file, ".rels"))
    new_rel <- relationship$new()

    if (file.exists(src_rels_path)) {
      src_rel <- relationship$new()$feed_from_xml(src_rels_path)
      src_rel_data <- src_rel$get_data()

      for (rel_i in seq_len(nrow(src_rel_data))) {
        rel_row <- src_rel_data[rel_i, ]
        rel_type_base <- basename(rel_row$type)
        rel_id <- paste0("rId", new_rel$get_next_id())

        if (rel_type_base == "slideLayout") {
          # Remap layout
          src_layout_file <- basename(rel_row$target)
          target_layout_file <- layout_map[[src_layout_file]]
          new_rel$add(
            id = rel_id,
            type = rel_row$type,
            target = paste0("../slideLayouts/", target_layout_file)
          )
        } else if (rel_type_base == "image") {
          # Copy media file
          src_media <- basename(rel_row$target)
          src_media_path <- file.path(source_pkg, "ppt/media", src_media)
          if (file.exists(src_media_path)) {
            dest_name <- fake_newname(src_media_path)
            dest_media_path <- file.path(target_pkg, "ppt/media", dest_name)
            if (!file.exists(dest_media_path)) {
              file.copy(src_media_path, dest_media_path, copy.mode = FALSE)
            }
            new_rel$add(
              id = rel_id,
              type = rel_row$type,
              target = paste0("../media/", dest_name)
            )
            update_media_ref_in_slide(new_slide_path, rel_row$id, rel_id)
          }
        } else if (rel_type_base == "notesSlide") {
          # Skip notes for now - handled separately
          next
        } else if (!is.na(rel_row$target_mode) && rel_row$target_mode == "External") {
          # External links (hyperlinks etc.) - keep as-is
          new_rel$add(
            id = rel_id,
            type = rel_row$type,
            target = rel_row$target,
            target_mode = "External"
          )
          update_media_ref_in_slide(new_slide_path, rel_row$id, rel_id)
        } else {
          # Other relationships (charts, etc.) - copy the referenced file
          copy_generic_rel(source_pkg, target_pkg, rel_row, new_rel, rel_id)
          update_media_ref_in_slide(new_slide_path, rel_row$id, rel_id)
        }
      }
    }

    # Write new .rels
    rels_dir <- file.path(target_pkg, "ppt/slides/_rels")
    dir.create(rels_dir, showWarnings = FALSE, recursive = TRUE)
    new_rel$write(file.path(rels_dir, paste0(new_slide_name, ".rels")))

    # Register slide in target
    target$presentation$add_slide(target = file.path("slides", new_slide_name))
    target$content_type$add_slide(partname = file.path("/ppt/slides", new_slide_name))
    target$slide$add_slide(new_slide_path, target$slideLayouts$get_xfrm_data())

    slide_file_map[[src_slide_file]] <- new_slide_name
  }

  # Copy notes slides
  copy_notes_slides(target, source, slide_file_map)

  target$cursor <- target$slide$length()
  target
}


# Build mapping from source layout filenames to target layout filenames
build_layout_map <- function(source_layout_meta, target_layout_meta, source_index) {
  # source_layout_meta has columns: name, filename, master_file, master_name
  # target_layout_meta has columns: name, filename, master_file, master_name
  layout_map <- list()

  for (i in seq_len(nrow(source_layout_meta))) {
    src_name <- source_layout_meta$name[i]
    src_master <- source_layout_meta$master_name[i]
    src_file <- source_layout_meta$filename[i]

    match_idx <- which(
      target_layout_meta$name == src_name &
        target_layout_meta$master_name == src_master
    )

    if (length(match_idx) == 0L) {
      # Try matching by layout name only (useful when master names differ but layout is the same)
      match_idx <- which(target_layout_meta$name == src_name)
    }

    if (length(match_idx) == 0L) {
      cli::cli_abort(
        c(
          "Layout {.val {src_name}} (master: {.val {src_master}}) from source {source_index} not found in target.",
          "x" = "All source layouts must exist in the target presentation.",
          "i" = "Available target layouts: {.val {paste(target_layout_meta$name, collapse = ', ')}}"
        )
      )
    }

    layout_map[[src_file]] <- target_layout_meta$filename[match_idx[1L]]
  }

  layout_map
}


# Update relationship IDs in a slide XML file
update_media_ref_in_slide <- function(slide_path, old_id, new_id) {
  if (old_id == new_id) {
    return(invisible())
  }

  slide_xml <- readLines(slide_path, warn = FALSE)
  slide_xml <- gsub(
    paste0('"', old_id, '"'),
    paste0('"', new_id, '"'),
    slide_xml,
    fixed = FALSE
  )
  writeLines(slide_xml, slide_path, useBytes = TRUE)
}


# Copy generic relationship target files
copy_generic_rel <- function(source_pkg, target_pkg, rel_row, new_rel, rel_id) {
  # For relative targets, resolve and copy
  target_rel <- rel_row$target
  if (!grepl("^https?://", target_rel) && !grepl("^mailto:", target_rel)) {
    # Resolve the path relative to ppt/slides/
    src_file <- normalizePath(
      file.path(source_pkg, "ppt/slides", target_rel),
      mustWork = FALSE
    )
    if (file.exists(src_file)) {
      dest_file <- file.path(target_pkg, "ppt/slides", target_rel)
      dir.create(dirname(dest_file), showWarnings = FALSE, recursive = TRUE)
      if (!file.exists(dest_file)) {
        file.copy(src_file, dest_file, copy.mode = FALSE)
      }
    }
  }

  new_rel$add(
    id = rel_id,
    type = rel_row$type,
    target = target_rel
  )
}


# Copy notes slides from source to target
copy_notes_slides <- function(target, source, slide_file_map) {
  source_pkg <- source$package_dir
  target_pkg <- target$package_dir

  for (src_slide_file in names(slide_file_map)) {
    target_slide_file <- slide_file_map[[src_slide_file]]

    # Check if source slide has a notesSlide relationship
    src_slide_rels_path <- file.path(
      source_pkg, "ppt/slides/_rels", paste0(src_slide_file, ".rels")
    )
    if (!file.exists(src_slide_rels_path)) next

    src_slide_rel <- relationship$new()$feed_from_xml(src_slide_rels_path)
    src_slide_rel_data <- src_slide_rel$get_data()
    notes_rows <- src_slide_rel_data[basename(src_slide_rel_data$type) == "notesSlide", ]

    if (nrow(notes_rows) == 0L) next

    # Ensure target has a notesMaster
    target <- add_notesMaster(target)

    notes_target <- basename(notes_rows$target[1L])
    src_notes_path <- file.path(source_pkg, "ppt/notesSlides", notes_target)
    if (!file.exists(src_notes_path)) next

    # Get new notesSlide name
    new_notes_name <- target$notesSlide$get_new_slidename()
    new_notes_path <- file.path(target_pkg, "ppt/notesSlides", new_notes_name)
    dir.create(dirname(new_notes_path), showWarnings = FALSE, recursive = TRUE)

    # Copy notes XML
    file.copy(src_notes_path, new_notes_path, copy.mode = FALSE)

    # Create new .rels for notes slide pointing to target's notesMaster + target slide
    notes_rel <- relationship$new()
    notes_rel$add(
      id = "rId1",
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesMaster",
      target = "../notesMasters/notesMaster1.xml"
    )
    notes_rel$add(
      id = "rId2",
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide",
      target = paste0("../slides/", target_slide_file)
    )

    rels_dir <- file.path(target_pkg, "ppt/notesSlides/_rels")
    dir.create(rels_dir, showWarnings = FALSE, recursive = TRUE)
    notes_rel$write(file.path(rels_dir, paste0(new_notes_name, ".rels")))

    # Add notesSlide relationship to the target slide's .rels
    target_slide_rels_path <- file.path(
      target_pkg, "ppt/slides/_rels", paste0(target_slide_file, ".rels")
    )
    target_slide_rel <- relationship$new()$feed_from_xml(target_slide_rels_path)
    notes_rid <- paste0("rId", target_slide_rel$get_next_id())
    target_slide_rel$add(
      id = notes_rid,
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesSlide",
      target = paste0("../notesSlides/", new_notes_name)
    )
    target_slide_rel$write(target_slide_rels_path)

    # Register in content_type
    target$content_type$add_notesSlide(
      partname = file.path("/ppt/notesSlides", new_notes_name)
    )

    # Add to notesSlide collection
    target$notesSlide$add_slide(new_notes_path)
  }

  target
}


# --- Phase 2: Cross-template layout/master/theme copying --------------------

# Get the next available integer index for files like slideLayout8.xml
get_next_index <- function(dir, prefix, suffix = "\\.xml$") {
  files <- list.files(dir, pattern = paste0("^", prefix, "[0-9]+", suffix))
  if (length(files) == 0L) {
    return(1L)
  }
  indices <- as.integer(gsub(
    paste0("^", prefix, "([0-9]+).*$"), "\\1", files
  ))
  max(indices) + 1L
}


# Pre-processing: copy layouts/masters/themes missing from target
copy_missing_layouts <- function(target, sources) {
  target_layout_meta <- target$slideLayouts$get_metadata()
  target_master_meta <- target$masterLayouts$get_metadata()
  target_pkg <- target$package_dir
  any_copied <- FALSE
  # Track which masters we've already copied (by source master name)
  copied_masters <- character(0)

  for (source in sources) {
    source_layout_meta <- source$slideLayouts$get_metadata()
    source_pkg <- source$package_dir

    # Find layouts missing from target
    for (i in seq_len(nrow(source_layout_meta))) {
      src_layout_name <- source_layout_meta$name[i]
      src_master_name <- source_layout_meta$master_name[i]
      src_layout_file <- source_layout_meta$filename[i]
      src_master_file <- source_layout_meta$master_file[i]

      # Check if layout already exists in target (exact or name-only match)
      match_exact <- which(
        target_layout_meta$name == src_layout_name &
          target_layout_meta$master_name == src_master_name
      )
      match_name <- which(target_layout_meta$name == src_layout_name)

      if (length(match_exact) > 0L || length(match_name) > 0L) next

      # Layout is missing — check if its master exists in target
      master_match <- which(target_master_meta$master_name == src_master_name)

      if (length(master_match) > 0L) {
        # Master exists: copy just this layout
        new_layout <- copy_layout_to_master(
          target, target_pkg, source_pkg,
          src_layout_file, basename(target_master_meta$filename[master_match[1L]])
        )
        target_layout_meta <- rbind(target_layout_meta, data.frame(
          master_file = basename(target_master_meta$filename[master_match[1L]]),
          name = src_layout_name,
          filename = new_layout,
          master_name = src_master_name,
          stringsAsFactors = FALSE
        ))
        any_copied <- TRUE
      } else if (!src_master_name %in% copied_masters) {
        # Master missing: copy master + theme + ALL its layouts
        src_master_layouts <- source_layout_meta[
          source_layout_meta$master_name == src_master_name, ,
          drop = FALSE
        ]
        result <- copy_master_with_layouts(
          target, target_pkg, source_pkg,
          src_master_file, src_master_name, src_master_layouts
        )
        target_layout_meta <- rbind(target_layout_meta, result$layout_meta)
        target_master_meta <- rbind(target_master_meta, result$master_meta)
        copied_masters <- c(copied_masters, src_master_name)
        any_copied <- TRUE
      }
    }
  }

  if (any_copied) {
    # Save and reload to refresh R6 state with new layouts/masters
    layout_default <- target$layout_default
    tmp <- tempfile(fileext = ".pptx")
    print(target, target = tmp)
    target <- read_pptx(tmp)
    target$layout_default <- layout_default
  }

  target
}


# Copy a single layout into an existing master in the target
copy_layout_to_master <- function(
  target, target_pkg, source_pkg, src_layout_file, target_master_file
) {
  layouts_dir <- file.path(target_pkg, "ppt/slideLayouts")
  new_idx <- get_next_index(layouts_dir, "slideLayout")
  new_layout_file <- paste0("slideLayout", new_idx, ".xml")
  new_layout_path <- file.path(layouts_dir, new_layout_file)

  # Copy layout XML
  file.copy(
    file.path(source_pkg, "ppt/slideLayouts", src_layout_file),
    new_layout_path,
    copy.mode = FALSE
  )

  # Copy media referenced by layout
  copy_part_media(source_pkg, target_pkg, "ppt/slideLayouts", src_layout_file)

  # Create layout .rels preserving original rIds (only remapping master target)
  layout_rel <- relationship$new()
  src_lo_rels_path <- file.path(
    source_pkg, "ppt/slideLayouts/_rels", paste0(src_layout_file, ".rels")
  )
  if (file.exists(src_lo_rels_path)) {
    src_lo_rel <- relationship$new()$feed_from_xml(src_lo_rels_path)
    src_lo_data <- src_lo_rel$get_data()
    for (k in seq_len(nrow(src_lo_data))) {
      row <- src_lo_data[k, ]
      if (basename(row$type) == "slideMaster") {
        layout_rel$add(
          id = row$id, type = row$type,
          target = paste0("../slideMasters/", target_master_file)
        )
      } else if (!is.na(row$target_mode) && row$target_mode == "External") {
        layout_rel$add(
          id = row$id, type = row$type,
          target = row$target, target_mode = "External"
        )
      } else {
        layout_rel$add(id = row$id, type = row$type, target = row$target)
      }
    }
  } else {
    layout_rel$add(
      id = "rId1",
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster",
      target = paste0("../slideMasters/", target_master_file)
    )
  }
  rels_dir <- file.path(layouts_dir, "_rels")
  dir.create(rels_dir, showWarnings = FALSE, recursive = TRUE)
  layout_rel$write(file.path(rels_dir, paste0(new_layout_file, ".rels")))

  # Add layout reference to the target master's .rels
  master_rels_path <- file.path(
    target_pkg, "ppt/slideMasters/_rels", paste0(target_master_file, ".rels")
  )
  master_rel <- relationship$new()$feed_from_xml(master_rels_path)
  layout_rid <- paste0("rId", master_rel$get_next_id())
  master_rel$add(
    id = layout_rid,
    type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout",
    target = paste0("../slideLayouts/", new_layout_file)
  )
  master_rel$write(master_rels_path)

  # Register in content_type
  target$content_type$add_override(setNames(
    "application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml",
    paste0("/ppt/slideLayouts/", new_layout_file)
  ))

  new_layout_file
}


# Copy a master + its theme + all its layouts from source to target
copy_master_with_layouts <- function(
  target, target_pkg, source_pkg,
  src_master_file, src_master_name, src_layouts_meta
) {
  # --- Copy theme ---
  src_master_rels_path <- file.path(
    source_pkg, "ppt/slideMasters/_rels", paste0(src_master_file, ".rels")
  )
  src_master_rel <- relationship$new()$feed_from_xml(src_master_rels_path)
  src_master_rel_data <- src_master_rel$get_data()
  theme_rows <- src_master_rel_data[basename(src_master_rel_data$type) == "theme", ]
  src_theme_file <- basename(theme_rows$target[1L])

  themes_dir <- file.path(target_pkg, "ppt/theme")
  dir.create(themes_dir, showWarnings = FALSE, recursive = TRUE)
  new_theme_idx <- get_next_index(themes_dir, "theme")
  new_theme_file <- paste0("theme", new_theme_idx, ".xml")

  file.copy(
    file.path(source_pkg, "ppt/theme", src_theme_file),
    file.path(themes_dir, new_theme_file),
    copy.mode = FALSE
  )

  target$content_type$add_override(setNames(
    "application/vnd.openxmlformats-officedocument.theme+xml",
    paste0("/ppt/theme/", new_theme_file)
  ))

  # --- Copy master ---
  masters_dir <- file.path(target_pkg, "ppt/slideMasters")
  dir.create(masters_dir, showWarnings = FALSE, recursive = TRUE)
  new_master_idx <- get_next_index(masters_dir, "slideMaster")
  new_master_file <- paste0("slideMaster", new_master_idx, ".xml")

  file.copy(
    file.path(source_pkg, "ppt/slideMasters", src_master_file),
    file.path(masters_dir, new_master_file),
    copy.mode = FALSE
  )

  # Copy media referenced by master
  copy_part_media(source_pkg, target_pkg, "ppt/slideMasters", src_master_file)

  # Renumber sldLayoutId entries to avoid collisions with existing IDs.
  # sldMasterId and sldLayoutId share a numeric ID namespace in OOXML.
  new_master_path <- file.path(masters_dir, new_master_file)
  renumber_layout_ids(new_master_path, target_pkg)

  target$content_type$add_override(setNames(
    "application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml",
    paste0("/ppt/slideMasters/", new_master_file)
  ))

  # --- Copy all layouts from this master ---
  layouts_dir <- file.path(target_pkg, "ppt/slideLayouts")
  layout_file_map <- list() # source layout file -> new layout file
  new_layout_meta <- data.frame(
    master_file = character(0), name = character(0),
    filename = character(0), master_name = character(0),
    stringsAsFactors = FALSE
  )

  for (j in seq_len(nrow(src_layouts_meta))) {
    src_lf <- src_layouts_meta$filename[j]
    new_layout_idx <- get_next_index(layouts_dir, "slideLayout")
    new_lf <- paste0("slideLayout", new_layout_idx, ".xml")
    new_layout_path <- file.path(layouts_dir, new_lf)

    file.copy(
      file.path(source_pkg, "ppt/slideLayouts", src_lf),
      new_layout_path,
      copy.mode = FALSE
    )

    # Copy media referenced by layout
    copy_part_media(source_pkg, target_pkg, "ppt/slideLayouts", src_lf)

    # Create layout .rels preserving original rIds (only remapping master target)
    lo_rel <- relationship$new()
    src_lo_rels_path <- file.path(
      source_pkg, "ppt/slideLayouts/_rels", paste0(src_lf, ".rels")
    )
    if (file.exists(src_lo_rels_path)) {
      src_lo_rel <- relationship$new()$feed_from_xml(src_lo_rels_path)
      src_lo_data <- src_lo_rel$get_data()
      for (k in seq_len(nrow(src_lo_data))) {
        row <- src_lo_data[k, ]
        if (basename(row$type) == "slideMaster") {
          lo_rel$add(
            id = row$id, type = row$type,
            target = paste0("../slideMasters/", new_master_file)
          )
        } else if (!is.na(row$target_mode) && row$target_mode == "External") {
          lo_rel$add(
            id = row$id, type = row$type,
            target = row$target, target_mode = "External"
          )
        } else {
          lo_rel$add(id = row$id, type = row$type, target = row$target)
        }
      }
    } else {
      lo_rel$add(
        id = "rId1",
        type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster",
        target = paste0("../slideMasters/", new_master_file)
      )
    }
    lo_rels_dir <- file.path(layouts_dir, "_rels")
    dir.create(lo_rels_dir, showWarnings = FALSE, recursive = TRUE)
    lo_rel$write(file.path(lo_rels_dir, paste0(new_lf, ".rels")))

    target$content_type$add_override(setNames(
      "application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml",
      paste0("/ppt/slideLayouts/", new_lf)
    ))

    layout_file_map[[src_lf]] <- new_lf
    new_layout_meta <- rbind(new_layout_meta, data.frame(
      master_file = new_master_file,
      name = src_layouts_meta$name[j],
      filename = new_lf,
      master_name = src_master_name,
      stringsAsFactors = FALSE
    ))
  }

  # --- Create master .rels preserving original rIds ---
  # The master XML references relationships by rId. We must keep the same rIds
  # and only remap the targets to the new filenames.
  master_rel <- relationship$new()
  for (k in seq_len(nrow(src_master_rel_data))) {
    row <- src_master_rel_data[k, ]
    rel_type_base <- basename(row$type)

    if (rel_type_base == "theme") {
      master_rel$add(
        id = row$id, type = row$type,
        target = paste0("../theme/", new_theme_file)
      )
    } else if (rel_type_base == "slideLayout") {
      src_lf <- basename(row$target)
      new_lf <- layout_file_map[[src_lf]]
      if (!is.null(new_lf)) {
        master_rel$add(
          id = row$id, type = row$type,
          target = paste0("../slideLayouts/", new_lf)
        )
      }
    } else {
      # Other rels (images, etc.) — keep as-is
      if (!is.na(row$target_mode) && row$target_mode == "External") {
        master_rel$add(
          id = row$id, type = row$type,
          target = row$target, target_mode = "External"
        )
      } else {
        master_rel$add(id = row$id, type = row$type, target = row$target)
      }
    }
  }

  master_rels_dir <- file.path(masters_dir, "_rels")
  dir.create(master_rels_dir, showWarnings = FALSE, recursive = TRUE)
  master_rel$write(file.path(master_rels_dir, paste0(new_master_file, ".rels")))

  # --- Register master in presentation.xml ---
  pres_rels <- target$presentation$relationship()
  master_rid <- paste0("rId", pres_rels$get_next_id())
  pres_rels$add(
    id = master_rid,
    type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster",
    target = paste0("slideMasters/", new_master_file)
  )
  pres_rels$write(file.path(target_pkg, "ppt/_rels/presentation.xml.rels"))

  # Add <p:sldMasterId> to presentation.xml
  pres_xml <- target$presentation$get()
  master_id_list <- xml_find_first(pres_xml, "//p:sldMasterIdLst")

  # Collect ALL existing numeric IDs: sldMasterId + sldLayoutId across all masters.
  # These share a namespace in OOXML and must be unique.
  existing_master_ids <- as.numeric(xml_attr(
    xml_find_all(pres_xml, "//p:sldMasterIdLst/p:sldMasterId"), "id"
  ))
  existing_layout_ids <- numeric(0)
  master_files <- list.files(
    file.path(target_pkg, "ppt/slideMasters"),
    pattern = "\\.xml$", full.names = TRUE
  )
  ns_p <- c(p = "http://schemas.openxmlformats.org/presentationml/2006/main")
  for (mf in master_files) {
    mxml <- read_xml(mf)
    lo_ids <- xml_attr(xml_find_all(mxml, "//p:sldLayoutIdLst/p:sldLayoutId", ns_p), "id")
    existing_layout_ids <- c(existing_layout_ids, as.numeric(lo_ids))
  }
  all_ids <- c(existing_master_ids, existing_layout_ids)
  new_master_id <- if (length(all_ids) > 0L) {
    max(all_ids) + 1
  } else {
    2147483648
  }

  new_node <- as_xml_document(sprintf(
    paste0(
      '<p:sldMasterId xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"',
      ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"',
      ' id="%.0f" r:id="%s"/>'
    ),
    new_master_id, master_rid
  ))
  xml_add_child(master_id_list, new_node)

  new_master_meta <- data.frame(
    master_name = src_master_name,
    filename = file.path(target_pkg, "ppt/slideMasters", new_master_file),
    stringsAsFactors = FALSE
  )

  list(layout_meta = new_layout_meta, master_meta = new_master_meta)
}


# Copy media files referenced by a part's .rels (images only)
copy_part_media <- function(source_pkg, target_pkg, part_dir, part_file) {
  rels_path <- file.path(
    source_pkg, part_dir, "_rels", paste0(part_file, ".rels")
  )
  if (!file.exists(rels_path)) {
    return(invisible())
  }

  rel <- relationship$new()$feed_from_xml(rels_path)
  rel_data <- rel$get_data()
  img_rows <- rel_data[basename(rel_data$type) == "image", ]
  if (nrow(img_rows) == 0L) {
    return(invisible())
  }

  media_dir <- file.path(target_pkg, "ppt/media")
  dir.create(media_dir, showWarnings = FALSE, recursive = TRUE)

  for (k in seq_len(nrow(img_rows))) {
    src_media <- basename(img_rows$target[k])
    src_media_path <- file.path(source_pkg, "ppt/media", src_media)
    if (file.exists(src_media_path)) {
      dest_path <- file.path(media_dir, src_media)
      if (!file.exists(dest_path)) {
        file.copy(src_media_path, dest_path, copy.mode = FALSE)
      }
    }
  }
}


# Copy non-slideMaster relationships from a layout's .rels (images, etc.)
copy_non_master_rels <- function(
  source_pkg, part_dir, src_file, new_rel, target_pkg
) {
  rels_path <- file.path(
    source_pkg, part_dir, "_rels", paste0(src_file, ".rels")
  )
  if (!file.exists(rels_path)) {
    return(invisible())
  }

  src_rel <- relationship$new()$feed_from_xml(rels_path)
  src_data <- src_rel$get_data()

  for (k in seq_len(nrow(src_data))) {
    rel_type_base <- basename(src_data$type[k])
    if (rel_type_base == "slideMaster") next # already handled

    rid <- paste0("rId", new_rel$get_next_id())
    if (!is.na(src_data$target_mode[k]) && src_data$target_mode[k] == "External") {
      new_rel$add(
        id = rid, type = src_data$type[k],
        target = src_data$target[k], target_mode = "External"
      )
    } else {
      new_rel$add(id = rid, type = src_data$type[k], target = src_data$target[k])
    }
  }
}


# Copy non-layout/non-theme relationships from a master's .rels (images, etc.)
copy_non_layout_theme_rels <- function(
  source_pkg, src_master_file, master_rel, target_pkg
) {
  rels_path <- file.path(
    source_pkg, "ppt/slideMasters/_rels", paste0(src_master_file, ".rels")
  )
  if (!file.exists(rels_path)) {
    return(invisible())
  }

  src_rel <- relationship$new()$feed_from_xml(rels_path)
  src_data <- src_rel$get_data()

  for (k in seq_len(nrow(src_data))) {
    rel_type_base <- basename(src_data$type[k])
    if (rel_type_base %in% c("slideLayout", "theme")) next # already handled

    rid <- paste0("rId", master_rel$get_next_id())
    if (!is.na(src_data$target_mode[k]) && src_data$target_mode[k] == "External") {
      master_rel$add(
        id = rid, type = src_data$type[k],
        target = src_data$target[k], target_mode = "External"
      )
    } else {
      master_rel$add(
        id = rid, type = src_data$type[k], target = src_data$target[k]
      )
    }
  }
}


# Renumber sldLayoutId entries in a copied master XML to avoid ID collisions.
# sldMasterId and sldLayoutId share a numeric namespace in OOXML.
renumber_layout_ids <- function(master_path, target_pkg) {
  ns_p <- c(p = "http://schemas.openxmlformats.org/presentationml/2006/main")

  # Collect all existing numeric IDs across the target
  existing_ids <- numeric(0)

  # From presentation.xml sldMasterIdLst
  pres_path <- file.path(target_pkg, "ppt/presentation.xml")
  if (file.exists(pres_path)) {
    pres_xml <- read_xml(pres_path)
    master_ids <- xml_attr(
      xml_find_all(pres_xml, "//p:sldMasterIdLst/p:sldMasterId", ns_p), "id"
    )
    existing_ids <- c(existing_ids, as.numeric(master_ids))
  }

  # From all existing masters' sldLayoutIdLst (excluding the one being added)
  master_files <- list.files(
    file.path(target_pkg, "ppt/slideMasters"),
    pattern = "\\.xml$", full.names = TRUE
  )
  master_files <- setdiff(master_files, master_path) # exclude current
  for (mf in master_files) {
    mxml <- read_xml(mf)
    lo_ids <- xml_attr(
      xml_find_all(mxml, "//p:sldLayoutIdLst/p:sldLayoutId", ns_p), "id"
    )
    existing_ids <- c(existing_ids, as.numeric(lo_ids))
  }

  if (length(existing_ids) == 0L) {
    return(invisible())
  }

  # Read the new master and renumber its layout IDs
  master_xml <- read_xml(master_path)
  layout_id_nodes <- xml_find_all(
    master_xml, "//p:sldLayoutIdLst/p:sldLayoutId", ns_p
  )
  if (length(layout_id_nodes) == 0L) {
    return(invisible())
  }

  next_id <- max(existing_ids) + 1
  for (node in layout_id_nodes) {
    xml_attr(node, "id") <- sprintf("%.0f", next_id)
    next_id <- next_id + 1
  }

  write_xml(master_xml, master_path)
}
