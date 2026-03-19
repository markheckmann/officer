#' @export
#' @title Concatenate PowerPoint presentations
#' @description Append slides from one or more source presentations to a target
#' presentation. The result keeps the target's template, properties, table styles,
#' and slide size. Slides from source presentations are appended in order.
#'
#' All source presentations must use layouts that exist in the target (matched by
#' layout name and master name). If a source slide references a layout not found
#' in the target, an error is raised.
#'
#' @param target an `rpptx` object whose template is kept.
#' @param ... one or more `rpptx` objects whose slides are appended.
#' @return The modified `target` as an `rpptx` object with all source slides
#'   appended. The cursor is set to the last slide.
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
        "Slide size of source {i} ({src_size$width}x{src_size$height}) differs from target ({target_size$width}x{target_size$height}). Target slide size is kept."
      )
    }
  }

  # Save sources to temp files so in-memory XML changes are flushed to disk
  sources <- lapply(sources, function(src) {
    tmp <- tempfile(fileext = ".pptx")
    print(src, target = tmp)
    read_pptx(tmp)
  })

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
#' @return An `rpptx` object with slides from all inputs.
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
          "i" = "All source layouts must exist in the target presentation.",
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
  if (old_id == new_id) return(invisible())

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
