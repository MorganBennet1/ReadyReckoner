## Regression tests for reckoner.R, checked against the original DCCEEW Ready
## Reckoner spreadsheet ("ready-reckoner-based-rainfall-2026_0.xlsx").
##
## Each expected value below was produced by driving the actual spreadsheet
## (via a headless LibreOffice/UNO session) with the same inputs and reading
## back its "SUMMARY RESULTS" panel, so these are cross-checks against the
## authoritative source, not just checks that the code does what it was
## written to do. They cover: all 4 SSP scenarios plus user-specified warming,
## all 3 time horizons, all 3 warming-uncertainty tiers, all 3 IFD
## rate-of-change tiers, short/medium/long storm durations, and target AEPs
## spanning the full frequent-to-extreme range (including a deliberately
## extreme extrapolation case).
##
## Run with: Rscript test_reckoner.R
## (No test framework dependency required, to keep the deployed app's
## dependency list minimal -- this uses plain stopifnot()-style checks.)

source("reckoner.R")

ifd <- parse_bom_ifd_csv("data/example_ifd_tasmania.csv")

cases <- list(
  list(args = list(scenario = "SSP3-7.0", time_horizon = "Medium-term (2041-2060)", warming_uncertainty = "median",
                    rate_uncertainty = "Central (median)", duration_label = "24 hour", target_aep_label = "1 in 100"),
       expect = list(hist = 116.0, proj = 133, pct = 15, equiv = "1 in 200")),
  list(args = list(scenario = "SSP1-2.6", time_horizon = "Current and near-term (2021-2040)", warming_uncertainty = "median",
                    rate_uncertainty = "Central (median)", duration_label = "1 hour", target_aep_label = "1 in 20"),
       expect = list(hist = 23.5, proj = 28, pct = 18, equiv = "1 in 47")),
  list(args = list(scenario = "SSP5-8.5", time_horizon = "Long-term (2081-2100)", warming_uncertainty = "95th percentile (high)",
                    rate_uncertainty = "High (upper tercile)", duration_label = "6 hour", target_aep_label = "1 in 1000"),
       expect = list(hist = 91.9, proj = 237, pct = 158, equiv = "1 in 6624900")),
  list(args = list(scenario = "SSP2-4.5", time_horizon = "Medium-term (2041-2060)", warming_uncertainty = "5th percentile (low)",
                    rate_uncertainty = "Low (lower tercile)", duration_label = "30 min", target_aep_label = "1 in 2"),
       expect = list(hist = 8.9, proj = 10, pct = 9, equiv = "1 in 2")),
  list(args = list(scenario = "user specified degrees of global warming", user_degrees = 3.0,
                    time_horizon = "Medium-term (2041-2060)", warming_uncertainty = "median",
                    rate_uncertainty = "Central (median)", duration_label = "12 hour", target_aep_label = "1 in 500"),
       expect = list(hist = 114.0, proj = 148, pct = 30, equiv = "1 in 3100")),
  list(args = list(scenario = "SSP3-7.0", time_horizon = "Long-term (2081-2100)", warming_uncertainty = "median",
                    rate_uncertainty = "Central (median)", duration_label = "168 hour", target_aep_label = "1 in 2000"),
       expect = list(hist = 268.0, proj = 345, pct = 29, equiv = "1 in 17600"))
)

n_pass <- 0
for (i in seq_along(cases)) {
  case <- cases[[i]]
  res <- do.call(compute_summary, c(list(ifd = ifd), case$args))
  ok <- isTRUE(all.equal(res$historical_depth_mm, case$expect$hist)) &&
    res$projected_depth_display_mm == case$expect$proj &&
    res$percent_change_display == case$expect$pct &&
    identical(res$equivalent_historical_aep_label, case$expect$equiv)
  status <- if (ok) "OK  " else "FAIL"
  cat(sprintf(
    "%s case %d (%s, %s): got (%.4g, %d, %d, %s) expected (%.4g, %d, %d, %s)\n",
    status, i, case$args$duration_label, case$args$target_aep_label,
    res$historical_depth_mm, res$projected_depth_display_mm, res$percent_change_display, res$equivalent_historical_aep_label,
    case$expect$hist, case$expect$proj, case$expect$pct, case$expect$equiv
  ))
  if (ok) n_pass <- n_pass + 1
}

cat(sprintf("\n%d / %d cases passed\n", n_pass, length(cases)))

# Out-of-range user-specified warming should error
user_degrees_error <- tryCatch({
  compute_summary(ifd, scenario = "user specified degrees of global warming", user_degrees = 10.0,
                   time_horizon = "Medium-term (2041-2060)", warming_uncertainty = "median",
                   rate_uncertainty = "Central (median)", duration_label = "24 hour", target_aep_label = "1 in 100")
  FALSE
}, error = function(e) TRUE)
cat(sprintf("%s out-of-range user-specified warming is rejected\n", if (user_degrees_error) "OK  " else "FAIL"))

## Merging BoM's current split-by-range export (separate Very Frequent / IFD
## / Rare files, each also offered as Depth / Intensity / Coefficients) into
## one IfdTable. There's no spreadsheet ground truth for this real example
## site (it isn't in the original workbook), so these are structural checks:
## the merge should reconstruct the classic single-file export's 18-column,
## ARI-ascending layout from just the 3 Depth files, skip the 6
## Intensity/Coefficients files it doesn't need, and every duration/target-
## AEP combination should compute without error and reproduce the raw
## file's depth exactly where there's no climate adjustment to apply.

split_dir <- "data/split_export_example"
split_files <- list.files(split_dir, full.names = TRUE)
reg <- build_ifd_registry(basename(split_files), split_files)

merge_checks <- c(
  "exactly 1 site found" = length(reg$entries) == 1,
  "6 non-Depth files skipped, 0 errors" = length(reg$notes) == 6 &&
    all(grepl("isn't needed by this tool", reg$notes))
)
site <- reg$entries[[1]]
ari <- vapply(site$ifd$columns, function(l) column_ey_aep_ari(l)[["ARI"]], numeric(1))
merge_checks["18 columns (matches the classic single-file layout)"] <- length(site$ifd$columns) == 18
merge_checks["columns are strictly ARI-ascending"] <- all(diff(ari) > 0)
merge_checks["29 durations (1 min to 168 hour)"] <- length(site$ifd$durations) == 29
merge_checks["label taken from embedded Location Label"] <- identical(site$label, "Zutic M & A Poultry")

all_ok <- TRUE
for (nm in names(merge_checks)) {
  ok <- isTRUE(merge_checks[[nm]])
  all_ok <- all_ok && ok
  cat(sprintf("%s merge: %s\n", if (ok) "OK  " else "FAIL", nm))
}

# Every duration x target-AEP combination should compute without error.
compute_fail <- FALSE
for (d in site$ifd$durations) {
  for (t in AEP_TARGET_OPTIONS) {
    res <- tryCatch(
      compute_summary(site$ifd, duration_label = d, target_aep_label = t,
                       scenario = "SSP3-7.0", time_horizon = "Medium-term (2041-2060)",
                       warming_uncertainty = "median", rate_uncertainty = "Central (median)"),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      compute_fail <- TRUE
      cat(sprintf("FAIL merge: compute_summary(%s, %s): %s\n", d, t, conditionMessage(res)))
    }
  }
}
cat(sprintf("%s merge: every duration x target-AEP combination computes\n", if (!compute_fail) "OK  " else "FAIL"))
all_ok <- all_ok && !compute_fail

# The raw depths_..._ifds.csv file (see data/split_export_example/) has
# 24 hour / 1% = 121 mm -- the historical depth for that combination should
# come straight through the merge unchanged.
res <- compute_summary(site$ifd, duration_label = "24 hour", target_aep_label = "1 in 100",
                        scenario = "SSP3-7.0", time_horizon = "Medium-term (2041-2060)",
                        warming_uncertainty = "median", rate_uncertainty = "Central (median)")
depth_ok <- isTRUE(all.equal(res$historical_depth_mm, 121))
cat(sprintf("%s merge: 24 hour / 1 in 100 historical depth == 121 mm (got %s)\n", if (depth_ok) "OK  " else "FAIL", res$historical_depth_mm))
all_ok <- all_ok && depth_ok

# A single range file uploaded on its own (no matching Very Frequent/IFD/Rare
# siblings) should still work, just with a note flagging partial coverage.
partial_reg <- build_ifd_registry("depths_only_rare.csv", file.path(split_dir, "depths_Zutic_M_A_Poultry_rare.csv"))
partial_ok <- length(partial_reg$entries) == 1 &&
  length(partial_reg$entries[[1]]$ifd$columns) == 5 &&
  length(partial_reg$notes) == 1 && grepl("only part of this site's data", partial_reg$notes)
cat(sprintf("%s merge: a lone range file registers with a partial-coverage note\n", if (partial_ok) "OK  " else "FAIL"))
all_ok <- all_ok && partial_ok

if (n_pass < length(cases) || !user_degrees_error || !all_ok) {
  quit(status = 1)
}
