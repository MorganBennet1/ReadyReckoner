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
# baseline, for each SSP scenario / time horizon / percentile.
TEMPERATURE_PROJECTIONS <- list(
  "SSP1-2.6" = list(
    "Current and near-term (2021-2040)" = c("5th percentile" = 0.9, "median" = 1.2, "95th percentile" = 1.5),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.0, "median" = 1.4, "95th percentile" = 1.9),
    "Long-term (2081-2100)"             = c("5th percentile" = 1.0, "median" = 1.5, "95th percentile" = 2.1)
  ),
  "SSP2-4.5" = list(
    "Current and near-term (2021-2040)" = c("5th percentile" = 0.9, "median" = 1.2, "95th percentile" = 1.5),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.3, "median" = 1.7, "95th percentile" = 2.2),
    "Long-term (2081-2100)"             = c("5th percentile" = 1.8, "median" = 2.4, "95th percentile" = 3.2)
  ),
  "SSP3-7.0" = list(
    "Current and near-term (2021-2040)" = c("5th percentile" = 0.9, "median" = 1.2, "95th percentile" = 1.5),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.4, "median" = 1.8, "95th percentile" = 2.3),
    "Long-term (2081-2100)"             = c("5th percentile" = 2.5, "median" = 3.3, "95th percentile" = 4.3)
  ),
  "SSP5-8.5" = list(
    "Current and near-term (2021-2040)" = c("5th percentile" = 1.0, "median" = 1.3, "95th percentile" = 1.6),
    "Medium-term (2041-2060)"           = c("5th percentile" = 1.6, "median" = 2.1, "95th percentile" = 2.7),
    "Long-term (2081-2100)"             = c("5th percentile" = 3.0, "median" = 4.1, "95th percentile" = 5.4)
  )
)

SCENARIO_OPTIONS <- c(names(TEMPERATURE_PROJECTIONS), "user specified degrees of global warming")
TIME_HORIZON_OPTIONS <- c("Current and near-term (2021-2040)", "Medium-term (2041-2060)", "Long-term (2081-2100)")
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

# The first 5 IFD columns (12EY,6EY,4EY,3EY,2EY) are frequent, sub-annual events.
# The ARR rate-of-change factors only apply from 0.632 AEP (~1 EY) and rarer --
# there is "insufficient evidence" to scale the very frequent events, so the
# spreadsheet (and this module) leaves them at their historical value.
N_UNADJUSTED_LEADING_COLUMNS <- 5

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

#' Parse a BoM 'design rainfalls' CSV export into an IfdTable (a list).
#'
#' This mirrors the layout the original spreadsheet's "BoM IFD-historical"
#' sheet expects (as documented in its Explainer tab): a metadata block,
#' then a header row whose first cell is literally "Duration" and second
#' cell is "Duration in min", followed by 18 frequency columns (5 EY columns,
#' then AEP-based columns from 0.632 AEP out to 1 in 2000), then one row per
#' storm duration.
#'
#' Works for any location's exported CSV, not just a specific site -- the
#' header row is located dynamically rather than assumed to be at a fixed
#' row number, so minor variations in the metadata block are tolerated.
parse_bom_ifd_csv <- function(path_or_connection) {
  lines <- readLines(path_or_connection, warn = FALSE, encoding = "UTF-8")
  # strip a UTF-8 BOM if present
  lines[1] <- sub("^\xef\xbb\xbf", "", lines[1])

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

  # metadata (best-effort; not required for calculations)
  location_label <- NULL
  latitude <- longitude <- NA_real_
  if (header_row_idx > 1) {
    for (i in 1:(header_row_idx - 1)) {
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
  }

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
# Multi-site support: a site-locations CSV + a batch of per-site BoM IFD CSVs
# ---------------------------------------------------------------------------

#' Parse a site-locations CSV: one row per site, with a site name/ID column
#' and latitude/longitude columns. Column names are matched case-insensitively
#' against common variants (e.g. "Site"/"Site Name"/"Name", "Lat"/"Latitude").
parse_site_list_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- trimws(names(df))
  lower_names <- tolower(names(df))

  find_col <- function(candidates) {
    idx <- which(lower_names %in% candidates)[1]
    if (is.na(idx)) NA_character_ else names(df)[idx]
  }
  site_col <- find_col(c("site", "site name", "sitename", "site id", "name", "location", "location label"))
  lat_col <- find_col(c("latitude", "lat"))
  lon_col <- find_col(c("longitude", "long", "lon", "lng"))

  missing <- c("site name" = is.na(site_col), "latitude" = is.na(lat_col), "longitude" = is.na(lon_col))
  if (any(missing)) {
    stop(
      "Site list CSV is missing a column for: ", paste(names(missing)[missing], collapse = ", "),
      ". Expected columns like 'Site', 'Latitude', 'Longitude'."
    )
  }

  data.frame(
    site = trimws(as.character(df[[site_col]])),
    latitude = suppressWarnings(as.numeric(df[[lat_col]])),
    longitude = suppressWarnings(as.numeric(df[[lon_col]])),
    stringsAsFactors = FALSE
  )
}

#' Normalise a name for fuzzy filename<->site matching: lowercase, strip
#' anything that isn't a letter or digit (so "Site A", "site_a", "SiteA.csv"
#' all normalise to the same token).
.normalise_name <- function(s) gsub("[^a-z0-9]", "", tolower(s))

#' Build a registry of {site label -> parsed IFD table} from a batch of
#' uploaded BoM IFD CSVs, optionally matched against a site-locations table
#' by filename (case/punctuation-insensitive substring match, exact match
#' preferred). If no site list is supplied, or a file doesn't match any
#' site, that file is still included, labelled from its own embedded
#' metadata (location label, or coordinates, or filename) rather than
#' dropped -- so a batch upload never silently loses a site.
#'
#' `file_names` and `file_paths` are parallel vectors (as produced by a
#' Shiny multi-file `fileInput`: `input$ifd_files$name` / `$datapath`).
#' `sites_df` is the result of `parse_site_list_csv()`, or NULL.
#'
#' Returns a list:
#'   $entries       - list of list(label=, ifd=, source_file=)
#'   $notes         - character vector of warnings (ambiguous/unmatched
#'                    files, coordinate mismatches) to surface to the user
#'   $missing_sites - site names in sites_df with no matched file
build_ifd_registry <- function(file_names, file_paths, sites_df = NULL) {
  n <- length(file_names)
  notes <- character(0)
  matched_site_idx <- rep(NA_integer_, n)

  has_sites <- !is.null(sites_df) && nrow(sites_df) > 0
  if (has_sites) {
    stems <- .normalise_name(tools::file_path_sans_ext(file_names))
    site_norm <- .normalise_name(sites_df$site)
    for (i in seq_len(n)) {
      exact <- which(site_norm == stems[i])
      if (length(exact) == 1) {
        matched_site_idx[i] <- exact
        next
      }
      candidates <- which(nchar(site_norm) > 0 &
        (mapply(function(sn) grepl(sn, stems[i], fixed = TRUE), site_norm) |
         mapply(function(sn) grepl(stems[i], sn, fixed = TRUE), site_norm)))
      if (length(candidates) == 1) {
        matched_site_idx[i] <- candidates
      } else if (length(candidates) > 1) {
        notes <- c(notes, sprintf(
          "'%s' matches more than one site by filename (%s) - rename the file so it matches only one.",
          file_names[i], paste(sites_df$site[candidates], collapse = ", ")
        ))
      } else {
        notes <- c(notes, sprintf(
          "'%s' doesn't match any site name in the site list by filename - it will still be loaded, labelled from its own data.",
          file_names[i]
        ))
      }
    }
  }

  entries <- list()
  labels_used <- character(0)
  for (i in seq_len(n)) {
    ifd <- tryCatch(parse_bom_ifd_csv(file_paths[i]), error = function(e) e)
    if (inherits(ifd, "error")) {
      notes <- c(notes, sprintf("Could not read '%s': %s", file_names[i], conditionMessage(ifd)))
      next
    }
    if (has_sites && !is.na(matched_site_idx[i])) {
      site_row <- sites_df[matched_site_idx[i], ]
      label <- site_row$site
      if (!is.na(ifd$latitude) && !is.na(ifd$longitude) && !is.na(site_row$latitude) && !is.na(site_row$longitude)) {
        if (abs(ifd$latitude - site_row$latitude) > 0.5 || abs(ifd$longitude - site_row$longitude) > 0.5) {
          notes <- c(notes, sprintf(
            "'%s': the site list's coordinate for %s (%.3f, %.3f) is more than 0.5 deg from the coordinate embedded in the file (%.3f, %.3f) - double check this is the right file.",
            file_names[i], label, site_row$latitude, site_row$longitude, ifd$latitude, ifd$longitude
          ))
        }
      }
    } else {
      label <- if (!is.null(ifd$location_label) && nzchar(ifd$location_label)) {
        ifd$location_label
      } else if (!is.na(ifd$latitude) && !is.na(ifd$longitude)) {
        sprintf("%.4f, %.4f", ifd$latitude, ifd$longitude)
      } else {
        tools::file_path_sans_ext(file_names[i])
      }
    }
    if (label %in% labels_used) {
      label <- sprintf("%s (%s)", label, file_names[i])
    }
    labels_used <- c(labels_used, label)
    entries[[length(entries) + 1]] <- list(label = label, ifd = ifd, source_file = file_names[i])
  }

  missing_sites <- character(0)
  if (has_sites) {
    matched_names <- sites_df$site[stats::na.omit(matched_site_idx)]
    missing_sites <- setdiff(sites_df$site, matched_names)
    if (length(missing_sites) > 0) {
      notes <- c(notes, sprintf(
        "No uploaded file matched: %s. Those sites won't appear in the Site list below.",
        paste(missing_sites, collapse = ", ")
      ))
    }
  }

  list(entries = entries, notes = notes, missing_sites = missing_sites)
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
  if (grepl("EY$", toupper(trimws(label)))) {
    "EY"
  } else if (!is.na(suppressWarnings(as.numeric(label)))) {
    "AEP"
  } else {
    "1inX"
  }
}

#' Return c(EY, AEP, ARI) for a frequency column header.
column_ey_aep_ari <- function(label) {
  kind <- classify_column(label)
  if (kind == "EY") {
    ey <- as.numeric(gsub("EY", "", label, ignore.case = TRUE))
    aep <- (exp(ey) - 1) / exp(ey)  # Langbein PDS -> AMS conversion
    ari <- 1 / ey
  } else {
    if (kind == "AEP") {
      aep <- as.numeric(label)
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

  if (col_index <= N_UNADJUSTED_LEADING_COLUMNS) {
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
