## ARR Ready Reckoner - climate-change adjusted design rainfall (Shiny app).
##
## Re-implements the DCCEEW/BoM "Ready reckoner based on rainfall" spreadsheet
## tool as an interactive web app, suitable for deployment to Posit Connect.
##
## Usage:
##   - Upload one or more BoM design-rainfalls CSVs (from
##     http://www.bom.gov.au/water/designRainfalls/revised-ifd/) -- one file per
##     site. Each file becomes a site, labelled from its own embedded location
##     label (or coordinates, or filename if neither is present) -- there's no
##     separate site list to maintain. Or leave the "use bundled example" box
##     ticked to use the bundled single-site example data.
##   - Pick a site, a climate scenario (or specify degrees of warming directly),
##     a time horizon, uncertainty settings, a critical storm duration, and a
##     target AEP.
##   - The summary panel reproduces the spreadsheet's headline numbers: historical
##     depth/AEP, the climate-adjusted depth and % change, and what AEP that
##     adjusted depth would correspond to under historical conditions.
##
## See reckoner.R for the calculation engine and its header comment for
## methodology notes and data sources (ARR v4.2).

library(shiny)

source("reckoner.R")

EXAMPLE_IFD_PATH <- "data/example_ifd_tasmania.csv"

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { max-width: 980px; margin: 0 auto; padding: 1.5rem; }
    .summary-card { background: #f4f7fb; border: 1px solid #d7e2ee; border-radius: 10px;
                    padding: 1.25rem 1.5rem; margin-top: 1rem; }
    .summary-card h4 { margin-top: 0; }
    .big-number { font-size: 1.6rem; font-weight: 600; }
    .muted { color: #667; font-size: 0.9rem; }
    .change-up { color: #b3261e; }
    .change-down { color: #1b6f4a; }
    .status-panel { background: #fff8e6; border: 1px solid #f0dca0; border-radius: 8px;
                     padding: 0.75rem 1rem; margin-top: 0.5rem; font-size: 0.85rem; }
    .status-panel ul { margin: 0.25rem 0 0 0; padding-left: 1.2rem; }
    footer.app-footer { margin-top: 2rem; font-size: 0.8rem; color: #778; }
  "))),
  tags$script(HTML("
    // 'Choose a whole folder' convenience: a plain (non-Shiny) hidden file
    // input with webkitdirectory picks every file in a chosen folder, then
    // this hands that FileList to the real fileInput (#ifd_files) as if the
    // user had selected those files directly, so no server-side code needs
    // to know this path exists. Filtered to .csv up front so OS clutter
    // (Thumbs.db, .DS_Store, a stray README) in the folder doesn't get
    // uploaded and reported as an unreadable file.
    $(document).on('click', '#ifd_folder_trigger', function (e) {
      e.preventDefault();
      document.getElementById('ifd_folder_input').click();
    });
    $(document).on('change', '#ifd_folder_input', function () {
      var csvFiles = Array.prototype.filter.call(this.files, function (f) {
        return /\\.csv$/i.test(f.name);
      });
      if (csvFiles.length === 0) return;
      var dt = new DataTransfer();
      csvFiles.forEach(function (f) { dt.items.add(f); });
      var target = document.getElementById('ifd_files');
      target.files = dt.files;
      $(target).trigger('change');
    });
  ")),
  titlePanel("ARR Ready Reckoner - Climate-Adjusted Design Rainfall"),
  p(
    "Reproduces the DCCEEW / Australian Rainfall and Runoff (ARR v4.2) climate-change ",
    "adjustment method for Bureau of Meteorology Intensity-Frequency-Duration (IFD) ",
    "design rainfalls.",
    class = "muted"
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h5("1. Site data"),
      checkboxInput("use_example", "Use bundled single-site example (Tasmania)", value = TRUE),
      conditionalPanel(
        condition = "!input.use_example",
        fileInput("ifd_files", "BoM design rainfall CSV(s)", accept = ".csv", multiple = TRUE),
        tags$input(id = "ifd_folder_input", type = "file", webkitdirectory = NA, directory = NA, multiple = NA, style = "display:none;"),
        tags$a(id = "ifd_folder_trigger", href = "#", "Or choose a whole folder instead", style = "font-size: 0.85rem;"),
        p(
          "Drop in a whole downloaded set at once - a single all-in-one CSV per site, ",
          "or BoM's current 'Very Frequent' / 'IFD' / 'Rare' split (Depth files only; ",
          "Intensity and Coefficients files are recognised and skipped automatically). ",
          "Files for the same site are matched up by their own embedded location and ",
          "combined, no matter how they're named - just upload everything, or (in ",
          "Chrome/Edge/Firefox) pick the folder they're all sitting in instead.",
          class = "muted"
        )
      ),
      uiOutput("data_status"),
      hr(),
      h5("2. Site"),
      uiOutput("site_select"),
      hr(),
      h5("3. Climate change scenario"),
      selectInput("scenario", "Scenario", choices = SCENARIO_OPTIONS, selected = "SSP3-7.0"),
      conditionalPanel(
        condition = "input.scenario != 'user specified degrees of global warming'",
        selectInput("time_horizon", "Time horizon", choices = TIME_HORIZON_OPTIONS, selected = "Medium-term (2041-2060)"),
        selectInput("warming_uncertainty", "Projection uncertainty", choices = WARMING_UNCERTAINTY_OPTIONS, selected = "median")
      ),
      conditionalPanel(
        condition = "input.scenario == 'user specified degrees of global warming'",
        numericInput("user_degrees", "Degrees of global warming (deg C, relative to 1961-1990)", value = 2.0, min = 0.9, max = 5.4, step = 0.1)
      ),
      selectInput("rate_uncertainty", "IFD rate-of-change uncertainty", choices = RATE_OF_CHANGE_UNCERTAINTY_OPTIONS, selected = "Central (median)"),
      hr(),
      h5("4. Storm & result"),
      uiOutput("duration_select"),
      selectInput("target_aep", "Interpret results for AEP of", choices = AEP_TARGET_OPTIONS, selected = "1 in 100")
    ),
    mainPanel(
      width = 8,
      uiOutput("error_panel"),
      uiOutput("summary_panel"),
      tags$footer(
        class = "app-footer",
        HTML(paste(
          "Data sources: Bureau of Meteorology design rainfalls (user-supplied CSV);",
          "global temperature projections from ARR v4.2 Table 1.6.2; IFD rate-of-change",
          "factors from ARR v4.2 Tables 1.6.1 / 1.6.5. This tool provides indicative",
          "estimates only - refer to the DCCEEW explainer report and ARR v4.2 Book 1",
          "for guidance on appropriate use."
        ))
      )
    )
  )
)

server <- function(input, output, session) {

  # A registry of {label -> parsed IFD table}, built either from the bundled
  # single-site example, or from the uploaded batch of per-site BoM IFD CSVs.
  registry <- reactive({
    if (isTRUE(input$use_example)) {
      ifd <- parse_bom_ifd_csv(EXAMPLE_IFD_PATH)
      return(list(
        entries = list(list(label = "Tasmania example", ifd = ifd, source_file = EXAMPLE_IFD_PATH)),
        notes = character(0)
      ))
    }

    req(input$ifd_files)
    build_ifd_registry(input$ifd_files$name, input$ifd_files$datapath)
  })

  output$data_status <- renderUI({
    reg <- registry()
    n_ok <- length(reg$entries)
    parts <- list()
    parts[[length(parts) + 1]] <- p(sprintf("%d site%s loaded.", n_ok, if (n_ok == 1) "" else "s"), class = "muted")
    if (length(reg$notes) > 0) {
      parts[[length(parts) + 1]] <- div(
        class = "status-panel",
        tags$strong("Check this before relying on the results:"),
        tags$ul(lapply(reg$notes, tags$li))
      )
    }
    do.call(tagList, parts)
  })

  output$site_select <- renderUI({
    reg <- registry()
    if (length(reg$entries) == 0) {
      return(p("No sites loaded yet - upload at least one BoM IFD CSV.", class = "muted"))
    }
    labels <- vapply(reg$entries, function(e) e$label, character(1))
    selectInput("site", "Site", choices = sort(labels), selected = sort(labels)[1])
  })

  selected_ifd <- reactive({
    reg <- registry()
    req(input$site, length(reg$entries) > 0)
    idx <- which(vapply(reg$entries, function(e) e$label, character(1)) == input$site)
    req(length(idx) == 1)
    reg$entries[[idx[1]]]$ifd
  })

  output$duration_select <- renderUI({
    ifd <- tryCatch(selected_ifd(), error = function(e) NULL)
    if (is.null(ifd)) {
      return(selectInput("duration", "Critical storm duration", choices = c("24 hour")))
    }
    default <- if ("24 hour" %in% ifd$durations) "24 hour" else ifd$durations[ceiling(length(ifd$durations) / 2)]
    selectInput("duration", "Critical storm duration", choices = ifd$durations, selected = default)
  })

  result <- reactive({
    req(input$duration, input$target_aep, input$scenario, input$rate_uncertainty)
    ifd <- selected_ifd()
    is_user_specified <- identical(input$scenario, "user specified degrees of global warming")
    compute_summary(
      ifd,
      duration_label = input$duration,
      target_aep_label = input$target_aep,
      scenario = input$scenario,
      time_horizon = if (is_user_specified) "Medium-term (2041-2060)" else input$time_horizon,
      warming_uncertainty = if (is_user_specified) "median" else input$warming_uncertainty,
      rate_uncertainty = input$rate_uncertainty,
      user_degrees = if (is_user_specified) input$user_degrees else NULL
    )
  })

  output$error_panel <- renderUI({
    res <- tryCatch({ result(); NULL }, error = function(e) e$message)
    if (is.null(res)) return(NULL)
    div(tags$strong("Could not compute a result: "), res, class = "summary-card")
  })

  output$summary_panel <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)

    change_class <- if (res$percent_change_display >= 0) "change-up" else "change-down"
    sign <- if (res$percent_change_display >= 0) "+" else ""

    equiv_text <- if (!is.na(res$equivalent_historical_aep_label)) {
      sprintf("This is equivalent to a rainfall AEP of %s under historical conditions.", res$equivalent_historical_aep_label)
    } else {
      "Could not estimate an equivalent historical AEP for this combination."
    }

    div(
      class = "summary-card",
      h4(sprintf("%s - %s historical and projected rainfall - %s AEP", input$site, res$duration_label, res$target_aep_label)),
      fluidRow(
        column(
          6,
          p("Under historical conditions", class = "muted"),
          p(sprintf("AEP: %s", res$historical_aep_label)),
          p(sprintf("%s mm", format(res$historical_depth_mm, trim = TRUE)), class = "big-number")
        ),
        column(
          6,
          p("Under the selected scenario", class = "muted"),
          p(sprintf("AEP: %s", res$target_aep_label)),
          p(
            sprintf("%s mm  ", format(res$projected_depth_display_mm, trim = TRUE)),
            tags$span(sprintf("(%s%d%%)", sign, res$percent_change_display), class = change_class),
            class = "big-number"
          )
        )
      ),
      hr(),
      p(equiv_text),
      p(
        sprintf(
          "Degrees of warming applied: %s deg C | IFD rate of change: %s%% per deg C (nearest tabulated duration) | Multiplier: x%.3f",
          format(res$degrees_of_warming, trim = TRUE), format(res$rate_of_change_pct, trim = TRUE), res$multiplier
        ),
        class = "muted"
      )
    )
  })
}

shinyApp(ui, server)
