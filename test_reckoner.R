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

if (n_pass < length(cases) || !user_degrees_error) {
  quit(status = 1)
}
