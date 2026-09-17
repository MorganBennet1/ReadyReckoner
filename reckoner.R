## ARR Ready Reckoner (climate-change adjusted design rainfall) - core calculation engine.
##
## This module re-implements, in plain R, the logic of the DCCEEW "Ready reckoner
## based on rainfall" spreadsheet tool (2026 edition), which itself implements the
## Australian Rainfall and Runoff (ARR) 2019 (v4.2) approach to adjusting Bureau of
## Meteorology (BoM) Intensity-Frequency-Duration (IFD) design rainfall depths for
## future climate conditions.
##
## It reproduces, for a single user-selected "critical storm duration" and target
## Annual Exceedance Probability (AEP), the same headline numbers shown in the
## spreadsheet's "SUMMARY RESULTS" panel:
##
##   - the historical rainfall depth and AEP
##   - the climate-adjusted ("projected") rainfall depth for the chosen scenario
##   - the percentage change between the two
##   - the AEP that the projected depth would represent under *historical*
##     (unadjusted) conditions -- i.e. "this used to be a 1-in-100 year event,
##     it is now more like a 1-in-200 year event".
##
## Data sources reproduced here:
##   - Global temperature projections: ARR v4.2 Table 1.6.2 (TEMPERATURE_PROJECTIONS)
##   - IFD rate-of-change factors: ARR v4.2 Tables 1.6.1 / 1.6.5 (RATE_OF_CHANGE_TABLE)
##
## The BoM historical IFD table itself is NOT hardcoded -- it is parsed from a BoM
## "design rainfalls" CSV export (http://www.bom.gov.au/water/designRainfalls/revised-ifd/),
## which is what the original spreadsheet asks the user to paste in. See `parse_bom_ifd_csv`.

# ---------------------------------------------------------------------------
# Static reference data (ARR v4.2)
# ---------------------------------------------------------------------------

# ARR v4.2 Table 1.6.2 - degrees of global warming relative to the 1961-1990
# baseline, for each SSP scenario / time horizon / percentile. The original
# 3 ARR time horizons ("Current and near-term (2021-2040)", "Medium-term
# (2041-2060)", "Long-term (2081-2100)") are left exactly as published.
#
# 5 additional rolling 20-year windows ("2011-2030", "2031-2050",
# "2051-2070", "2061-2080", "2071-2090") were added on 2026-09-17, at
# Morgan's request for finer time-horizon coverage. These are *not* from
# the ARR table (ARR only ever published the 3 above) -- they were derived
# from the same underlying assessment ARR's own table is built on: the
# IPCC AR6 WG1 Chapter 4 "assessed GSAT projections" dataset
# (https://github.com/IPCC-WG1/Chapter-4_Figure4.11), which publishes
# exactly these rolling 20-year-mean windows (Q05/Q50/Q95) per SSP,
# relative to a 1995-2014 baseline. To slot into this table on the same
# 1961-1990 basis as ARR's 3 original entries, each raw window value was
# shifted by a constant baseline-conversion offset per percentile (5th:
# +0.50 degC, median: +0.56 degC, 95th: +0.59 degC) -- these offsets were
# not assumed, they were *fitted* by comparing the raw dataset's own
# 2021-2040 / 2041-2060 / 2081-2100 windows against ARR's already-published
# values for those same 3 periods (36 data points across all 4 scenarios x
# 3 percentiles), which matched a single per-percentile additive constant
# to within 0.06 degC everywhere (well inside the table's own 0.1 degC
# rounding) -- i.e. the fit reproduces ARR's own numbers almost exactly,
# which is what justifies applying the same offset to the 5 new windows.
# See the project doc for the fitting workflow and full residual table.
TEMPERATURE_PROJECTIONS <- list(
  "SSP1-2.6" = list(
    "2011-2030"                         = c("5th percentile" = 0.8, "median" = 1.0, "95th percentile" = 1.2),
    "Current and near-term (2021-2040)" = c("5th percentile" = 0.9, "median" = 1.2, "95th percentile" = 1.5),
    "2031-2050"                         = c("5th percentile" = 1.0, "median" = 1.3, "95th percentile" = 1.7),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.0, "median" = 1.4, "95th percentile" = 1.9),
    "2051-2070"                         = c("5th percentile" = 1.1, "median" = 1.5, "95th percentile" = 2.0),
    "2061-2080"                         = c("5th percentile" = 1.1, "median" = 1.5, "95th percentile" = 2.0),
    "2071-2090"                         = c("5th percentile" = 1.0, "median" = 1.5, "95th percentile" = 2.1),
    "Long-term (2081-2100)"             = c("5th percentile" = 1.0, "median" = 1.5, "95th percentile" = 2.1)
  ),
  "SSP2-4.5" = list(
    "2011-2030"                         = c("5th percentile" = 0.8, "median" = 1.0, "95th percentile" = 1.2),
    "Current and near-term (2021-2040)" = c("5th percentile" = 0.9, "median" = 1.2, "95th percentile" = 1.5),
    "2031-2050"                         = c("5th percentile" = 1.1, "median" = 1.4, "95th percentile" = 1.8),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.3, "median" = 1.7, "95th percentile" = 2.2),
    "2051-2070"                         = c("5th percentile" = 1.4, "median" = 1.9, "95th percentile" = 2.5),
    "2061-2080"                         = c("5th percentile" = 1.5, "median" = 2.1, "95th percentile" = 2.7),
    "2071-2090"                         = c("5th percentile" = 1.7, "median" = 2.2, "95th percentile" = 3.0),
    "Long-term (2081-2100)"             = c("5th percentile" = 1.8, "median" = 2.4, "95th percentile" = 3.2)
  ),
  "SSP3-7.0" = list(
    "2011-2030"                         = c("5th percentile" = 0.8, "median" = 1.0, "95th percentile" = 1.1),
    "Current and near-term (2021-2040)" = c("5th percentile" = 0.9, "median" = 1.2, "95th percentile" = 1.5),
    "2031-2050"                         = c("5th percentile" = 1.2, "median" = 1.5, "95th percentile" = 1.9),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.4, "median" = 1.8, "95th percentile" = 2.3),
    "2051-2070"                         = c("5th percentile" = 1.7, "median" = 2.2, "95th percentile" = 2.8),
    "2061-2080"                         = c("5th percentile" = 1.9, "median" = 2.5, "95th percentile" = 3.3),
    "2071-2090"                         = c("5th percentile" = 2.2, "median" = 2.9, "95th percentile" = 3.8),
    "Long-term (2081-2100)"             = c("5th percentile" = 2.5, "median" = 3.3, "95th percentile" = 4.3)
  ),
  "SSP5-8.5" = list(
    "2011-2030"                         = c("5th percentile" = 0.8, "median" = 1.0, "95th percentile" = 1.2),
    "Current and near-term (2021-2040)" = c("5th percentile" = 1.0, "median" = 1.3, "95th percentile" = 1.6),
    "2031-2050"                         = c("5th percentile" = 1.3, "median" = 1.7, "95th percentile" = 2.1),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.6, "median" = 2.1, "95th percentile" = 2.7),
    "2051-2070"                         = c("5th percentile" = 1.9, "median" = 2.6, "95th percentile" = 3.3),
    "2061-2080"                         = c("5th percentile" = 2.2, "median" = 3.0, "95th percentile" = 4.0),
    "2071-2090"                         = c("5th percentile" = 2.6, "median" = 3.5, "95th percentile" = 4.7),
    "Long-term (2081-2100)"             = c("5th percentile" = 3.0, "median" = 4.1, "95th percentile" = 5.4)
  )
)

SCENARIO_OPTIONS <- c(names(TEMPERATURE_PROJECTIONS), "user specified degrees of global warming")
TIME_HORIZON_OPTIONS <- c("2011-2030", "Current and near-term (2021-2040)", "2031-2050",
                           "Medium-term (2041-2060)", "2051-2070", "2061-2080", "2071-2090",
                           "Long-term (2081-2100)")
WARMING_UNCERTAINTY_OPTIONS <- c("5th percentile (low)", "median", "95th percentile (high)")
.WARMING_UNCERTAINTY_KEY <- c(
  "5th percentile (low)" = "5th percentile",
  "median" = "median",
  "95th percentile (high)" = "95th percentile"
)

# ARR v4.2 Tables 1.6.1 / 1.6.5 - rate of change (%) in rainfall intensity per
# degree of warming, by storm duration and by uncertainty tercile.
RATE_OF_CHANGE_TABLE <- data.frame(
  duration_min = c(60, 90, 120, 180, 270, 360, 540, 720, 1080, 1440),
  `Low (lower tercile)` = c(7.0, 6.1, 5.5, 4.7, 4.0, 3.6, 3.1, 2.7, 2.3, 2.0),
  `Central (median)` = c(15.0, 13.7, 12.8, 11.8, 10.8, 10.2, 9.5, 9.0, 8.4, 8.0),
  `High (upper tercile)` = c(28.0, 25.6, 24.0, 22.0, 20.3, 19.2, 17.8, 16.9, 15.7, 15.0),
  check.names = FALSE
)

RATE_OF_CHANGE_UNCERTAINTY_OPTIONS <- c("Low (lower tercile)", "Central (median)", "High (upper tercile)")

AEP_TARGET_OPTIONS <- c("1 in 2", "1 in 5", "1 in 10", "1 in 20", "1 in 50", "1 in 100", "1 in 200", "1 in 500", "1 in 1000", "1 in 2000")

# The ARR rate-of-change factors only apply from 0.632 AEP (~1 EY, i.e.
# ARI >= 1 year) and rarer -- there is "insufficient evidence" to scale more
# frequent, sub-annual events, so the spreadsheet (and this module) leaves
# them at their historical value. See compute_summary() below, which checks
# this per-result from the fitted curve rather than a fixed column count.

# Rounding precision applied to a climate-adjusted depth, matching the
# spreadsheet's nested IF() in the "IFD climate change adjusted" sheet.
round_depth <- function(value) {
  if (value < 1) {
    round(value, 3)
  } else if (value < 10) {
    round(value, 2)
  } else if (value < 100) {
    round(value, 1)
  } else {
    round(value, 0)
  }
}

# Extra rounding applied to the *equivalent historical AEP* return period,
# depending on how rare the target AEP is (matches the "N" column tiers in
# the "IFD AEP projection" sheet: columns/targets get coarser rounding as the
# return period gets longer).
tiered_round <- function(value, col_index) {
  # col_index is 1-based position within the 18 frequency columns.
  tier <- if (col_index <= 12) 1 else if (col_index <= 14) 10 else if (col_index <= 16) 50 else 100
  as.integer(round(value / tier) * tier)
}

# ---------------------------------------------------------------------------
# BoM IFD CSV parsing
# ---------------------------------------------------------------------------

#' Classify a BoM 'design rainfalls' CSV export from its title line (line 3
#' in every export seen so far), e.g. "Rare Design Rainfall Depth (mm)" or
#' "Very Frequent Design Rainfall Coefficients". BoM's download now splits
#' each site's data into several files: one per frequency *range* (Very
#' Frequent / IFD / Rare) and, within each range, one per *data type*
#' (Depth / Intensity / Coefficients) -- typically 9 files for one site.
#' Only the Depth files are needed by this app; this classification lets a
#' batch upload sort that out automatically instead of relying on filenames
#' (which the user may have renamed).
classify_bom_export <- function(lines) {
  title <- if (length(lines) >= 3) trimws(lines[3]) else ""
  title_lc <- tolower(title)
  data_type <- if (grepl("coefficient", title_lc)) {
    "coefficients"
  } else if (grepl("intensity", title_lc)) {
    "intensity"
  } else if (grepl("depth", title_lc)) {
    "depth"
  } else {
    "unknown"
  }
  range <- if (grepl("very frequent", title_lc)) {
    "very_frequent"
  } else if (grepl("^frequent", title_lc)) {
    "frequent"
  } else if (grepl("^rare", title_lc)) {
    "rare"
  } else if (grepl("^ifd", title_lc)) {
    "ifd"
  } else if (grepl("^all", title_lc)) {
    "all"
  } else {
    "unknown"
  }
  list(title = title, data_type = data_type, range = range)
}

#' Parse a BoM 'design rainfalls' CSV export into an IfdTable (a list).
#'
#' Handles both layouts BoM has issued: the older single "All Design
#' Rainfall Depth" export covering all 18 frequencies in one file, and the
#' current split-by-range export (separate Very Frequent / IFD / Rare
#' files). A file is identified by its title line (see classify_bom_export);
#' Intensity and Coefficients files are recognised but not fully parsed
#' (their table isn't needed and, for Coefficients, isn't even shaped like
#' the Duration-by-frequency table below) -- they come back with an empty
#' table and the caller (build_ifd_registry) skips them.
#'
#' For a Depth (or unrecognised) file, this mirrors the layout the original
#' spreadsheet's "BoM IFD-historical" sheet expects: a metadata block, then
#' a header row whose first cell is literally "Duration" and second cell is
#' "Duration in min", followed by the frequency columns for that file, then
#' one row per storm duration. The header row is located dynamically rather
#' than assumed to be at a fixed row number, so minor variations in the
#' metadata block are tolerated.
parse_bom_ifd_csv <- function(path_or_connection) {
  lines <- readLines(path_or_connection, warn = FALSE, encoding = "UTF-8")
  # strip a UTF-8 BOM if present
  lines[1] <- sub("^\xef\xbb\xbf", "", lines[1])
  cls <- classify_bom_export(lines)

  split_csv_line <- function(line) {
    # a simple CSV splitter; the BoM export has no embedded commas/quotes
    # within fields, so a plain strsplit is sufficient and avoids pulling
    # in an extra CSV-parsing dependency.
    if (nchar(line) == 0) return(character(0))
    strsplit(line, ",", fixed = TRUE)[[1]]
  }

  rows <- lapply(lines, split_csv_line)
  width <- max(vapply(rows, length, integer(1)), 0)
  rows <- lapply(rows, function(r) {
    length(r) <- width
    r[is.na(r)] <- ""
    r
  })

  # metadata (best-effort; not required for calculations). Scanned up front
  # (rather than "everything above the header row") so it's still picked up
  # for Coefficients files, which return before a header row is looked for.
  location_label <- NULL
  latitude <- longitude <- NA_real_
  for (i in seq_len(min(length(rows), 10))) {
    c0 <- trimws(tolower(rows[[i]][1]))
    if (identical(c0, "location label:") && width > 1 && nzchar(trimws(rows[[i]][2]))) {
      location_label <- trimws(rows[[i]][2])
    }
    if (identical(c0, "requested coordinate:")) {
      lat <- suppressWarnings(as.numeric(rows[[i]][3]))
      lon <- suppressWarnings(as.numeric(rows[[i]][5]))
      if (!is.na(lat)) latitude <- lat
      if (!is.na(lon)) longitude <- lon
    }
  }

  if (cls$data_type %in% c("coefficients", "intensity")) {
    return(list(
      title = cls$title, data_type = cls$data_type, range = cls$range,
      location_label = location_label, latitude = latitude, longitude = longitude,
      columns = character(0), durations = character(0), duration_minutes = numeric(0),
      depths = matrix(numeric(0), nrow = 0, ncol = 0)
    ))
  }

  header_row_idx <- NA_integer_
  for (i in seq_along(rows)) {
    cell_a <- trimws(tolower(rows[[i]][1]))
    cell_b <- if (width > 1) trimws(tolower(rows[[i]][2])) else ""
    if (identical(cell_a, "duration") && identical(cell_b, "duration in min")) {
      header_row_idx <- i
      break
    }
  }
  if (is.na(header_row_idx)) {
    stop(
      "Could not find the IFD table header row (a row starting with ",
      "'Duration', 'Duration in min'). Make sure the file is a BoM ",
      "design-rainfalls CSV export, or preserves that layout."
    )
  }

  header_cells <- rows[[header_row_idx]]
  columns <- trimws(header_cells[3:width])
  columns <- columns[columns != ""]
  n_cols <- length(columns)

  durations <- character(0)
  duration_minutes <- numeric(0)
  depth_rows <- list()
  i <- header_row_idx + 1
  while (i <= length(rows)) {
    cell_a <- trimws(rows[[i]][1])
    if (identical(cell_a, "")) break
    dur_min <- suppressWarnings(as.numeric(rows[[i]][2]))
    if (is.na(dur_min)) break
    durations <- c(durations, cell_a)
    duration_minutes <- c(duration_minutes, dur_min)
    vals <- suppressWarnings(as.numeric(rows[[i]][3:(2 + n_cols)]))
    depth_rows[[length(depth_rows) + 1]] <- vals
    i <- i + 1
  }

  depths <- do.call(rbind, depth_rows)
  colnames(depths) <- columns
  rownames(depths) <- durations

  list(
    title = cls$title, data_type = cls$data_type, range = cls$range,
    location_label = location_label,
    latitude = latitude,
    longitude = longitude,
    columns = columns,
    durations = durations,
    duration_minutes = duration_minutes,
    depths = depths
  )
}

ifd_row <- function(ifd, duration_label) {
  ifd$depths[duration_label, ]
}

ifd_duration_min_for <- function(ifd, duration_label) {
  ifd$duration_minutes[match(duration_label, ifd$durations)]
}

# ---------------------------------------------------------------------------
# Multi-site support: a batch of per-site BoM IFD CSVs
# ---------------------------------------------------------------------------

#' Merge a group of same-site range-split BoM Depth exports (e.g. that
#' site's "Very Frequent" + "IFD" + "Rare" Depth files) into a single
#' IfdTable with the same shape parse_bom_ifd_csv() produces: one column
#' per distinct frequency, ordered from most frequent (shortest ARI) to
#' rarest (longest ARI). Adjacent files' frequency ranges overlap by one
#' column each (e.g. the IFD file's "1EY"/"63.2%" column and the Rare
#' file's "1 in 100" column can describe the very same event in different
#' units) -- those are detected (by near-equal Average Recurrence Interval)
#' and collapsed to a single column, so the result matches the classic
#' single-file export's 18-column layout when all 3 ranges are supplied.
merge_bom_depth_exports <- function(parts) {
  ref <- parts[[1]]
  for (p in parts[-1]) {
    if (!identical(p$durations, ref$durations) ||
        length(p$duration_minutes) != length(ref$duration_minutes) ||
        !isTRUE(all.equal(p$duration_minutes, ref$duration_minutes))) {
      stop(
        "These files don't share the same set of storm durations -- make ",
        "sure the Very Frequent / IFD / Rare Depth files you uploaded ",
        "together are all exports for the same site."
      )
    }
  }

  all_labels <- unlist(lapply(parts, function(p) p$columns))
  all_ari <- vapply(all_labels, function(l) column_ey_aep_ari(l)[["ARI"]], numeric(1))
  all_values <- do.call(cbind, lapply(parts, function(p) p$depths))

  # Two columns are treated as the same frequency if their ARI agrees to
  # within 2 decimal places -- comfortably tighter than the spacing between
  # any two genuinely distinct frequencies in these files, but loose enough
  # to absorb the small rounding difference between (e.g.) an EY-derived
  # ARI and the same event's AEP-derived ARI computed from a rounded "%"
  # label such as "63.2%".
  keep <- !duplicated(round(all_ari, 2))
  order_idx <- order(all_ari[keep])

  final_labels <- all_labels[keep][order_idx]
  final_values <- all_values[, keep, drop = FALSE][, order_idx, drop = FALSE]
  colnames(final_values) <- final_labels

  list(
    location_label = ref$location_label,
    latitude = ref$latitude,
    longitude = ref$longitude,
    columns = final_labels,
    durations = ref$durations,
    duration_minutes = ref$duration_minutes,
    depths = final_values
  )
}

#' Build a registry of {site label -> parsed IFD table} from a batch of
#' uploaded BoM CSV exports. Understands both the older single "All Design
#' Rainfall Depth" file (one file = one site) and the current split-by-
#' range export (separate Very Frequent / IFD / Rare files, each also
#' offered as Intensity / Coefficients variants -- typically 9 files per
#' site in a downloaded zip). Intensity and Coefficients files are skipped
#' (noted, not treated as errors) since this app only needs Depth; the
#' Depth files belonging to one site are matched up by their shared
#' embedded "Location Label" / coordinates (not by filename, so renaming
#' files or dropping a whole zip's worth of files in at once both work) and
#' merged into one combined table -- see merge_bom_depth_exports().
#'
#' `file_names` and `file_paths` are parallel vectors (as produced by a
#' Shiny multi-file `fileInput`: `input$ifd_files$name` / `$datapath`).
#'
#' Returns a list:
#'   $entries - list of list(label=, ifd=, source_file=)
#'   $notes   - character vector of warnings/info (e.g. a file that failed
#'              to parse, or a site with incomplete coverage) to surface to
#'              the user. Kept short even for very large uploads (hundreds+
#'              of files): skipped Intensity/Coefficients files are rolled
#'              up into a single count rather than listed one by one (their
#'              filenames aren't actionable), and any other note category is
#'              capped at a handful of lines with a "...and N more" tail.
build_ifd_registry <- function(file_names, file_paths) {
  n <- length(file_names)
  parse_error_notes <- character(0)
  skip_counts <- c(coefficients = 0L, intensity = 0L)
  parsed <- list()

  for (i in seq_len(n)) {
    ifd <- tryCatch(parse_bom_ifd_csv(file_paths[i]), error = function(e) e)
    if (inherits(ifd, "error")) {
      parse_error_notes <- c(parse_error_notes, sprintf("Could not read '%s': %s", file_names[i], conditionMessage(ifd)))
      next
    }
    if (ifd$data_type %in% c("coefficients", "intensity")) {
      skip_counts[ifd$data_type] <- skip_counts[ifd$data_type] + 1L
      next
    }
    ifd$source_file <- file_names[i]
    parsed[[length(parsed) + 1]] <- ifd
  }

  # Site grouping key: prefer the embedded Location Label, then embedded
  # coordinates (both shared by every range-file for the same site), and
  # only fall back to the filename (with a known range suffix stripped) for
  # files with neither -- so grouping works even for a whole zip's worth of
  # files dropped in at once, regardless of what they're named.
  site_key <- function(p) {
    if (!is.null(p$location_label) && nzchar(p$location_label)) {
      return(paste0("label:", tolower(trimws(p$location_label))))
    }
    if (!is.na(p$latitude) && !is.na(p$longitude)) {
      return(sprintf("coord:%.4f,%.4f", p$latitude, p$longitude))
    }
    stripped <- tools::file_path_sans_ext(p$source_file)
    stripped <- sub("_(very_frequent|frequent|rare|ifds?|all)$", "", stripped, ignore.case = TRUE)
    paste0("file:", tolower(stripped))
  }

  keys <- vapply(parsed, site_key, character(1))
  entries <- list()
  labels_used <- character(0)
  partial_notes <- character(0)
  merge_error_notes <- character(0)

  for (key in unique(keys)) {
    group <- parsed[keys == key]

    if (length(group) == 1 && group[[1]]$range %in% c("very_frequent", "frequent", "ifd", "rare")) {
      partial_notes <- c(partial_notes, sprintf(
        "'%s' looks like only part of this site's data (a '%s' range file on its own) -- upload its matching Very Frequent / IFD / Rare Depth files too for full coverage.",
        group[[1]]$source_file, group[[1]]$range
      ))
    }

    ifd <- tryCatch({
      if (length(group) == 1) group[[1]] else merge_bom_depth_exports(group)
    }, error = function(e) e)

    if (inherits(ifd, "error")) {
      files_desc <- paste(vapply(group, function(p) p$source_file, character(1)), collapse = ", ")
      merge_error_notes <- c(merge_error_notes, sprintf("Could not combine %s: %s", files_desc, conditionMessage(ifd)))
      next
    }

    label <- if (!is.null(ifd$location_label) && nzchar(ifd$location_label)) {
      ifd$location_label
    } else if (!is.na(ifd$latitude) && !is.na(ifd$longitude)) {
      sprintf("%.4f, %.4f", ifd$latitude, ifd$longitude)
    } else {
      tools::file_path_sans_ext(group[[1]]$source_file)
    }
    if (label %in% labels_used) {
      label <- sprintf("%s (%s)", label, group[[1]]$source_file)
    }
    labels_used <- c(labels_used, label)

    source_files <- paste(vapply(group, function(p) p$source_file, character(1)), collapse = ", ")
    entries[[length(entries) + 1]] <- list(label = label, ifd = ifd, source_file = source_files)
  }

  # Cap any one category of note at a handful of lines, so a batch of
  # hundreds/thousands of files can't turn the status panel into an
  # unreadable wall of text -- the count itself is shown even when the
  # detail is truncated.
  cap_notes <- function(items, max_items = 8) {
    if (length(items) <= max_items) return(items)
    c(items[seq_len(max_items)], sprintf("...and %d more.", length(items) - max_items))
  }

  notes <- character(0)
  if (skip_counts["coefficients"] > 0) {
    notes <- c(notes, sprintf(
      "Skipped %d Coefficients file%s (not needed by this tool).",
      skip_counts["coefficients"], if (skip_counts["coefficients"] == 1) "" else "s"
    ))
  }
  if (skip_counts["intensity"] > 0) {
    notes <- c(notes, sprintf(
      "Skipped %d Intensity file%s (not needed by this tool).",
      skip_counts["intensity"], if (skip_counts["intensity"] == 1) "" else "s"
    ))
  }
  notes <- c(notes, cap_notes(parse_error_notes), cap_notes(partial_notes), cap_notes(merge_error_notes))

  list(entries = entries, notes = notes)
}

# ---------------------------------------------------------------------------
# Climate change inputs
# ---------------------------------------------------------------------------

#' Degrees of global warming (delta-T) relative to the 1961-1990 baseline.
degrees_of_warming <- function(scenario, time_horizon, warming_uncertainty, user_degrees = NULL) {
  if (identical(scenario, "user specified degrees of global warming")) {
    if (is.null(user_degrees) || is.na(user_degrees)) {
      stop("user_degrees must be supplied when scenario is 'user specified degrees of global warming'")
    }
    if (user_degrees < 0.9 || user_degrees > 5.4) {
      stop("Please enter a realistic degree of global warming (between 0.9 and 5.4 deg C)")
    }
    return(as.numeric(user_degrees))
  }
  if (!scenario %in% names(TEMPERATURE_PROJECTIONS)) {
    stop("Unknown scenario: ", scenario)
  }
  key <- .WARMING_UNCERTAINTY_KEY[[warming_uncertainty]]
  if (is.null(key)) key <- warming_uncertainty
  unname(TEMPERATURE_PROJECTIONS[[scenario]][[time_horizon]][key])
}

#' Return list(rate_pct, multiplier) for a storm duration.
#'
#' The multiplier is (1 + rate/100) ^ delta_t, matching the spreadsheet's
#' "IFD rate of change" sheet. The rate is looked up for the *nearest*
#' tabulated duration (60 to 1440 minutes) -- durations shorter than 60 min
#' use the 60-min rate, and durations longer than 1440 min (24 hours) use
#' the 1440-min rate, exactly as the original spreadsheet does.
rate_of_change_multiplier <- function(duration_min, rate_uncertainty, delta_t) {
  nearest_idx <- which.min(abs(RATE_OF_CHANGE_TABLE$duration_min - duration_min))
  rate_pct <- RATE_OF_CHANGE_TABLE[nearest_idx, rate_uncertainty]
  multiplier <- (1 + rate_pct / 100) ^ delta_t
  list(rate_pct = rate_pct, multiplier = multiplier)
}

# ---------------------------------------------------------------------------
# Frequency-column classification & AEP/ARI conversions
# ---------------------------------------------------------------------------

classify_column <- function(label) {
  label <- trimws(label)
  if (grepl("EY$", toupper(label))) {
    "EY"
  } else if (grepl("%$", label)) {
    "PCT"
  } else if (!is.na(suppressWarnings(as.numeric(label)))) {
    "AEP"
  } else {
    "1inX"
  }
}

#' Return c(EY, AEP, ARI) for a frequency column header. Column headers seen
#' across BoM exports come in four shapes: "<n>EY" (Exceedances per Year,
#' the older single-file export's leading columns), a bare decimal AEP
#' fraction like "0.632" (also the older export), a percentage like "63.2%"
#' (the current split-by-range export's IFD file), and "1 in <n>" (both
#' exports' rarest columns).
column_ey_aep_ari <- function(label) {
  kind <- classify_column(label)
  if (kind == "EY") {
    ey <- as.numeric(gsub("EY", "", label, ignore.case = TRUE))
    aep <- (exp(ey) - 1) / exp(ey)  # Langbein PDS -> AMS conversion
    ari <- 1 / ey
  } else {
    if (kind == "AEP") {
      aep <- as.numeric(label)
    } else if (kind == "PCT") {
      aep <- as.numeric(sub("%$", "", trimws(label))) / 100
    } else {
      x <- as.numeric(trimws(gsub("1 in", "", label, fixed = TRUE)))
      aep <- 1 / x
    }
    ari <- 1 / (-log(1 - aep))
    ey <- 1 / ari
  }
  c(EY = ey, AEP = aep, ARI = ari)
}

#' Fit the historical rainfall-frequency curve for one storm duration, the
#' same way as the "IFD AEP projection" sheet: a quadratic fit of depth vs
#' log10(ARI) across all 18 frequency columns, plus a linear fit across the
#' 3 rarest columns for extrapolation beyond the quadratic's valid range.
#' Returns a list with $columns, $log_ari, $historical_depth, $quad_coeffs
#' (a, b, c for a*x^2 + b*x + c), $lin_coeffs (m, k for m*x + k).
fit_frequency_curve <- function(ifd, duration_label) {
  row <- as.numeric(ifd_row(ifd, duration_label))
  columns <- ifd$columns
  ari <- vapply(columns, function(c) column_ey_aep_ari(c)[["ARI"]], numeric(1))
  log_ari <- log10(ari)
  depth <- row

  quad_fit <- lm(depth ~ log_ari + I(log_ari^2))
  a <- unname(coef(quad_fit)["I(log_ari^2)"])
  b <- unname(coef(quad_fit)["log_ari"])
  c <- unname(coef(quad_fit)["(Intercept)"])

  n <- length(log_ari)
  tail_idx <- (n - 2):n
  lin_fit <- lm(depth[tail_idx] ~ log_ari[tail_idx])
  k <- unname(coef(lin_fit)["(Intercept)"])
  m <- unname(coef(lin_fit)["log_ari[tail_idx]"])

  list(
    columns = columns,
    log_ari = log_ari,
    historical_depth = depth,
    quad_coeffs = c(a = a, b = b, c = c),
    lin_coeffs = c(m = m, k = k)
  )
}

#' Invert the fitted curve to estimate the ARI (in years) that `depth` would
#' represent under the historical frequency curve. `col_index` (1-based,
#' within the 18 columns) is used only to decide whether the point falls in
#' the quadratic-extrapolation regime.
historical_aep_for_depth <- function(curve, depth, col_index) {
  a <- curve$quad_coeffs[["a"]]
  b <- curve$quad_coeffs[["b"]]
  c <- curve$quad_coeffs[["c"]]
  discriminant <- b^2 - 4 * a * (c - depth)
  if (discriminant < 0) return(NA_real_)
  x_at_col <- curve$log_ari[col_index]
  if (x_at_col < 3.31) {
    x <- (-b + sqrt(discriminant)) / (2 * a)
  } else {
    m <- curve$lin_coeffs[["m"]]
    k <- curve$lin_coeffs[["k"]]
    x <- (depth - k) / m
  }
  10^x
}

#' Find the (first) column index whose '1 in Y' label matches
#' target_aep_label, e.g. '1 in 100' -> the '0.01' AEP column. Matches the
#' spreadsheet's FLOOR(1/AEP,1) + MATCH() behaviour, including picking the
#' first of any duplicate labels (e.g. both '0.5' and '0.5EY' map to '1 in 2').
match_target_column <- function(columns, target_aep_label) {
  target_y <- as.numeric(trimws(gsub("1 in", "", target_aep_label, fixed = TRUE)))
  for (i in seq_along(columns)) {
    aep <- column_ey_aep_ari(columns[i])[["AEP"]]
    if (is.na(aep) || aep <= 0) next
    y <- floor(1 / aep)
    if (isTRUE(y == target_y)) return(i)
  }
  stop("No frequency column corresponds to target AEP ", target_aep_label)
}

# ---------------------------------------------------------------------------
# Top-level summary calculation
# ---------------------------------------------------------------------------

#' Reproduce the spreadsheet's 'SUMMARY RESULTS' panel for one storm duration
#' and one target AEP. Returns a named list.
compute_summary <- function(ifd, duration_label, target_aep_label,
                             scenario, time_horizon, warming_uncertainty,
                             rate_uncertainty, user_degrees = NULL) {

  delta_t <- degrees_of_warming(scenario, time_horizon, warming_uncertainty, user_degrees)
  duration_min <- ifd_duration_min_for(ifd, duration_label)
  roc <- rate_of_change_multiplier(duration_min, rate_uncertainty, delta_t)

  curve <- fit_frequency_curve(ifd, duration_label)
  col_index <- match_target_column(ifd$columns, target_aep_label)

  historical_depth <- curve$historical_depth[col_index]

  # ARR: there is "insufficient evidence" to scale events more frequent
  # than about 1 per year (ARI < 1 year, i.e. log10(ARI) < 0) for climate
  # change, so those are left at their historical value. This is
  # equivalent to the original spreadsheet's fixed "first 5 columns"
  # (12EY..2EY all have ARI < 1) but works for any IFD table, including
  # ones assembled from BoM's split-by-range exports where the number of
  # very-frequent columns available can vary.
  if (curve$log_ari[col_index] < 0) {
    projected_depth <- historical_depth  # too frequent to scale - insufficient evidence
  } else {
    projected_depth <- round_depth(historical_depth * roc$multiplier)
  }

  percent_change <- (projected_depth - historical_depth) / historical_depth * 100

  equiv_ari <- historical_aep_for_depth(curve, projected_depth, col_index)
  equiv_label <- NA_character_
  if (!is.na(equiv_ari)) {
    rounded_ari <- tiered_round(equiv_ari, col_index)
    equiv_label <- paste0("1 in ", rounded_ari)
  }

  list(
    duration_label = duration_label,
    target_aep_label = target_aep_label,
    degrees_of_warming = delta_t,
    rate_of_change_pct = roc$rate_pct,
    multiplier = roc$multiplier,
    historical_depth_mm = historical_depth,
    historical_aep_label = target_aep_label,
    projected_depth_mm = projected_depth,
    projected_depth_display_mm = round(projected_depth),
    percent_change = percent_change,
    percent_change_display = round(percent_change),
    equivalent_historical_ari_years = equiv_ari,
    equivalent_historical_aep_label = equiv_label
  )
}

# ---------------------------------------------------------------------------
# Multi-site export: one SUMMARY RESULTS row per loaded site
# ---------------------------------------------------------------------------

.EXPORT_COLUMNS <- c(
  "site", "duration", "target_aep",
  "historical_depth_mm", "projected_depth_mm", "percent_change_pct",
  "equivalent_historical_aep", "degrees_of_warming_c", "rate_of_change_pct_per_degree",
  "multiplier", "source_files", "error"
)

#' Compute the SUMMARY RESULTS row for every site in a batch registry's
#' $entries (see build_ifd_registry), for one shared set of climate/
#' duration/target inputs -- these are exactly the "3. Climate change
#' scenario" and "4. Storm & result" controls in the app, applied across
#' every loaded site at once rather than just the one selected in "2. Site".
#'
#' A site whose own data can't produce a result for these inputs (e.g. it
#' doesn't have the selected storm duration, or has no column for the
#' selected target AEP) gets a row with NA result columns and a message in
#' `error`, rather than aborting the whole export -- so one awkward site
#' out of hundreds doesn't block exporting the rest.
#'
#' Returns a data.frame, one row per entry, sorted by site label (matching
#' the Site dropdown's own alphabetical order).
compute_summary_table <- function(entries, duration_label, target_aep_label,
                                    scenario, time_horizon, warming_uncertainty,
                                    rate_uncertainty, user_degrees = NULL) {
  if (length(entries) == 0) {
    empty <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(.EXPORT_COLUMNS)))
    colnames(empty) <- .EXPORT_COLUMNS
    return(empty)
  }

  rows <- lapply(entries, function(e) {
    res <- tryCatch(
      compute_summary(
        e$ifd,
        duration_label = duration_label,
        target_aep_label = target_aep_label,
        scenario = scenario,
        time_horizon = time_horizon,
        warming_uncertainty = warming_uncertainty,
        rate_uncertainty = rate_uncertainty,
        user_degrees = user_degrees
      ),
      error = function(err) err
    )
    if (inherits(res, "error")) {
      data.frame(
        site = e$label, duration = duration_label, target_aep = target_aep_label,
        historical_depth_mm = NA_real_, projected_depth_mm = NA_real_,
        percent_change_pct = NA_real_, equivalent_historical_aep = NA_character_,
        degrees_of_warming_c = NA_real_, rate_of_change_pct_per_degree = NA_real_,
        multiplier = NA_real_, source_files = e$source_file,
        error = conditionMessage(res), stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        site = e$label, duration = res$duration_label, target_aep = res$target_aep_label,
        historical_depth_mm = res$historical_depth_mm, projected_depth_mm = res$projected_depth_display_mm,
        percent_change_pct = res$percent_change_display, equivalent_historical_aep = res$equivalent_historical_aep_label,
        degrees_of_warming_c = res$degrees_of_warming, rate_of_change_pct_per_degree = res$rate_of_change_pct,
        multiplier = round(res$multiplier, 3), source_files = e$source_file,
        error = "", stringsAsFactors = FALSE
      )
    }
  })

  out <- do.call(rbind, rows)
  out[order(out$site), , drop = FALSE]
}
