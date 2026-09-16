# ARR Ready Reckoner (Shiny app, R)

An R re-implementation of DCCEEW's "Ready reckoner based on rainfall"
spreadsheet, as an interactive Shiny app. It reproduces the spreadsheet's
headline "SUMMARY RESULTS": given a Bureau of Meteorology (BoM) design-
rainfall (IFD) table for a site, a climate scenario/time-horizon/uncertainty
selection, a critical storm duration and a target Annual Exceedance
Probability (AEP), it reports the historical depth/AEP, the climate-adjusted
depth and % change, and the AEP that adjusted depth would represent under
historical conditions.

This implementation has been made solely using Claude AI. The Ready reckoner 
spreadsheet was obtained and provided to Claude with the instructions to turn 
into a Shiny app using R.

## Inputs

Only one thing actually drives a result: **a site's BoM design rainfall
data** -- everything else (scenario, time horizon, uncertainty, storm
duration, target AEP) is just a selection in the app, not a data file.
Download it from http://www.bom.gov.au/water/designRainfalls/revised-ifd/
and upload it (there's no separate site list to prepare or keep in sync).
BoM has issued this data in two layouts over time, and the app accepts
both, in any mixture, in one batch upload:

- **The current layout**: a zip with one file per frequency *range* (Very
  Frequent / IFD / Rare) times one per *data type* (Depth / Intensity /
  Coefficients) -- 9 files per site. Only the 3 **Depth** files are needed;
  drop the whole zip's contents in and the app sorts them out on its own
  (by reading each file's own title line, not its filename): Intensity and
  Coefficients files are recognised and skipped, and the 3 Depth files for
  one site are matched up by their shared embedded "Location Label" /
  coordinates and combined into that site's full frequency range -- this
  works regardless of how the files happen to be named.
- **The older layout**: a single "All Design Rainfall Depth" CSV covering
  one site's full frequency range in one file. Still supported as-is, one
  file per site.

Upload one site's worth of files, or several sites' at once. Each site
becomes one entry in the Site dropdown, labelled from its embedded
"Location Label:" if it has one, otherwise from its embedded coordinates,
otherwise from its filename. The status panel above the Site dropdown
reports how many sites loaded and flags anything skipped, unparseable, or
only partially uploaded (e.g. a Rare file with no matching Very Frequent /
IFD files for the same site).

See `data/split_export_example/` for a real example site's full 9-file
current-layout download, and `data/multi_site_example/` for two example
older-layout per-site CSVs (reusing the same real Tasmania rainfall
numbers under generic labels, purely to demonstrate uploading more than
one file at once).

## Files

- `reckoner.R` -- the calculation engine (data + logic), independent of the
  UI. Includes comments on the ARR v4.2 methodology and data sources.
- `app.R` -- the Shiny app (UI + reactive wiring around `reckoner.R`).
- `test_reckoner.R` -- regression tests (plain `Rscript`, no test-framework
  dependency needed). Every expected value was cross-checked against the
  original spreadsheet (opened headlessly with LibreOffice and driven with
  the same inputs), across all 4 SSP scenarios, user-specified warming, all
  time horizons and uncertainty tiers, short through long storm durations,
  and AEPs from 1-in-2 to 1-in-2000 (including a deliberately extreme
  extrapolation case).
- `data/example_ifd_tasmania.csv` -- a real BoM IFD export (Tasmania, the
  location in the originally supplied spreadsheet), in the older single-
  file layout. Bundled as the app's single-site demo/default data.
- `data/multi_site_example/` -- two example per-site CSVs in the older
  single-file layout, to demonstrate uploading more than one site at once.
  These reuse the same real Tasmania rainfall numbers under two generic
  site labels -- they are not two distinct real locations.
- `data/split_export_example/` -- one real site's full current-layout
  download (9 files: Very Frequent / IFD / Rare, each as Depth / Intensity
  / Coefficients), to demonstrate and regression-test the split-file merge
  and the Intensity/Coefficients skip logic.

Dependencies: base R (>= 4.0) plus the `shiny` package. No other packages
required to run the app or the tests.

## What carried over from the spreadsheet, and what's generalised

- **Global temperature projections** (ARR v4.2 Table 1.6.2) and **IFD
  rate-of-change factors** (ARR v4.2 Tables 1.6.1/1.6.5) are hardcoded in
  `reckoner.R`, exactly as they were hardcoded as lookup tables in the
  spreadsheet.
- **The BoM historical IFD table is *not* hardcoded, and it's not limited to
  one site.** The spreadsheet asked the user to paste one site's IFD data
  into a fixed cell layout; this app instead accepts a batch upload of BoM
  "design rainfalls" CSVs for any number of sites (see Inputs above), each
  parsed by locating the `Duration / Duration in min` header row rather than
  assuming a fixed row number -- so it works for any site's export, not just
  the one in the example file. It also copes with BoM having split what
  used to be one file into several (see Inputs above) by classifying and
  re-merging them at upload time, rather than requiring a separate
  preprocessing step to reassemble them first.
- This app deliberately reproduces only the **summary results** (the numbers
  people actually read), not the spreadsheet's full 39-duration x
  18-frequency adjusted-depths table. It still computes the full 18-point
  frequency curve fit needed to answer "what historical AEP does this
  projected depth correspond to", for the one duration you select -- that
  part of the method can't be simplified away without changing the answer.

## Known spreadsheet quirk *not* reproduced

The original spreadsheet's "Current and near-term (2021-2040)" dropdown
option has a trailing non-breaking space baked into its underlying cell,
which occasionally breaks its own VLOOKUP if you retype the option instead of
selecting it from the dropdown. This app's dropdown values don't have that
issue -- selecting "Current and near-term (2021-2040)" always resolves
correctly.

## A portability note for future edits

Every string in `app.R` / `reckoner.R` is plain ASCII on purpose (no em
dashes, degree signs, curly quotes, etc.), even though R and Shiny normally
handle UTF-8 fine. Under a non-UTF-8 locale (`LC_CTYPE=C`/`POSIX` -- easy to
end up with on a minimal server or container), those characters can get
silently mangled into things like `<e2><80><94>` when Shiny renders them,
regardless of the source file's own encoding. Sticking to ASCII in these two
files sidesteps that entirely. If you add new user-facing text, keep it
ASCII (spell out "deg C" instead of using the degree sign, use a plain
hyphen instead of an em dash, and so on) rather than relying on the
deployment server having a UTF-8 locale configured.

## Running locally

```r
install.packages("shiny")   # if not already installed
shiny::runApp(".")
```

Then open the URL it prints (typically http://127.0.0.1:xxxx).

To run the regression tests:

```bash
Rscript test_reckoner.R
```

## Deploying to Posit Connect

The simplest path is the `rsconnect` R package:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(server = "<your-posit-connect-server>",
                          name = "<account-name>",
                          apiKey = "<your-api-key>")
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "reckoner.R", "data/example_ifd_tasmania.csv"),
  appTitle = "ARR Ready Reckoner"
)
```

Run that from inside this folder (the one containing `app.R`). If your
organisation deploys via the Connect web UI instead (drag-and-drop /
git-backed content), just make sure `reckoner.R` and the `data/` folder are
both included alongside `app.R` -- they're both required at runtime, not
just for local testing. `test_reckoner.R`, `data/multi_site_example/` and
`data/split_export_example/` don't need to be deployed; they're dev-time/
documentation only (though `test_reckoner.R` does need
`data/split_export_example/` present if you run it).

## Extending it

- To surface more of the spreadsheet's detail (e.g. the full adjusted-depths
  table across all durations, or the quadratic-fit diagnostic), the building
  blocks are already in `reckoner.R` (`fit_frequency_curve`,
  `rate_of_change_multiplier`, etc.) -- only `app.R` would need new UI to
  expose them.
- To support a different rate-of-change or temperature-projection dataset
  (e.g. a future ARR revision), edit the `TEMPERATURE_PROJECTIONS` and
  `RATE_OF_CHANGE_TABLE` objects at the top of `reckoner.R`.
- If BoM changes the split-file layout again (a new range tier, a renamed
  title line, etc.), `classify_bom_export()` in `reckoner.R` is the one
  place that reads a file's title line to work out its data type (Depth /
  Intensity / Coefficients) and range (Very Frequent / IFD / Rare / All) --
  update the keyword matching there first.
- If you ever want to reintroduce a separate site-locations file (e.g. to
  rename sites without renaming the underlying BoM files, or to cross-check
  an uploaded file's embedded coordinates against a project register),
  `build_ifd_registry()` in `reckoner.R` is the natural place to add it back
  -- it already parses each file's embedded location label and coordinates,
  just doesn't currently match them against anything external.
