mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, wellPanel(
        h4("1. Classifier Keywords"),
        uiOutput(ns("download_keywords_ui"))
      )),
      column(3, wellPanel(
        h4("2. Training Data"),
        uiOutput(ns("download_training_ui"))
      )),
      column(3, wellPanel(
        h4("3. Fully Coded Data"),
        uiOutput(ns("download_coded_ui")),
        helpText("Apply keywords to the entire dataset.")
      )),
      column(3, wellPanel(
        h4("4. Validated Package"),
        uiOutput(ns("full_data_ui"))
      ))
    ),
    hr(),
    actionButton(ns("prev_tab"), "← Back to Validation", class = "btn-secondary")
  )
}

mod_download_server <- function(id, state, parent_session) { # Added parent_session for navigation
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Keywords UI ---
    output$download_keywords_ui <- renderUI({
      clf <- state$classifiers$Keywords
      if (is.null(clf) || length(clf[clf != "" & !is.na(clf)]) == 0) {
        actionButton(ns("kw_disabled"), "No Keywords", icon = icon("ban"), class = "btn-secondary disabled")
      } else {
        downloadButton(ns("download_keywords"), "Download CSV", class = "btn-info")
      }
    })

    # --- 2. Training UI ---
    output$download_training_ui <- renderUI({
      if (is.null(state$training_results) || nrow(state$training_results) == 0) {
        actionButton(ns("train_disabled"), "No Training Data", icon = icon("ban"), class = "btn-secondary disabled")
      } else {
        downloadButton(ns("download_training"), "Download CSV", class = "btn-primary")
      }
    })

    # --- 3. NEW: Coded Data UI (Unlocked once keywords exist) ---
    output$download_coded_ui <- renderUI({
      clf <- state$classifiers$Keywords
      if (is.null(clf) || length(clf[clf != "" & !is.na(clf)]) == 0) {
        actionButton(ns("coded_disabled"), "Keywords Required", icon = icon("lock"), class = "btn-secondary disabled")
      } else {
        downloadButton(ns("download_coded_only"), "Download Coded CSV", class = "btn-warning")
      }
    })

    # --- 4. Validated Package UI ---
    output$full_data_ui <- renderUI({
      if (is.null(state$val_complete) || state$val_complete == FALSE) {
        tagList(
          actionButton(ns("full_disabled"), "Locked", icon = icon("lock"), class = "btn-secondary disabled"),
          helpText("Requires Validation cycle.")
        )
      } else {
        downloadButton(ns("download_full"), "Download Zip", class = "btn-success")
      }
    })

    # --- DOWNLOAD HANDLERS ---

    # 1. Keywords
    output$download_keywords <- downloadHandler(
      filename = function() { paste0("keywords_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(state$classifiers, file, row.names = FALSE) }
    )

    # 2. Training Data
    output$download_training <- downloadHandler(
      filename = function() { paste0("training_data_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(state$training_results, file, row.names = FALSE) }
    )

    # 3. NEW: Coded Data Handler
    output$download_coded_only <- downloadHandler(
      filename = function() { paste0("coded_full_dataset_", Sys.Date(), ".csv") },
      content = function(file) {
        df <- state$dataset
        t_col <- state$text_column
        clf <- state$classifiers$Keywords
        pattern <- paste(clf[clf != "" & !is.na(clf)], collapse = "|")

        # Apply coding logic
        df$Predicted_Positive <- ifelse(grepl(pattern, df[[t_col]], ignore.case = TRUE, perl = TRUE), 1, 0)
        write.csv(df, file, row.names = FALSE)
      }
    )

    # 4. Full Validated Package (ZIP)
    output$download_full <- downloadHandler(
      filename = function() { paste0("validated_results_", Sys.Date(), ".zip") },
      content = function(file) {
        tmpdir <- tempdir()
        orig_wd <- getwd()
        setwd(tmpdir)
        on.exit(setwd(orig_wd)) # Ensure we return to original WD

        # Generate CSV
        df <- state$dataset
        t_col <- state$text_column
        clf <- state$classifiers$Keywords
        pattern <- paste(clf[clf != "" & !is.na(clf)], collapse = "|")
        df$Predicted_Positive <- ifelse(grepl(pattern, df[[t_col]], ignore.case = TRUE, perl = TRUE), 1, 0)

        data_file <- "coded_dataset.csv"
        write.csv(df, data_file, row.names = FALSE)

        # Generate Report
        report_file <- "validation_report.txt"
        writeLines(c(
          "===========================================",
          "      CAI'S N VALIDATION REPORT",
          "===========================================",
          paste("Date:", Sys.Date()),
          paste("Validation Status: SUCCESS"),
          paste("Cai's N:", state$val_cais_n)
        ), report_file)

        zip(zipfile = file, files = c(data_file, report_file), extras = "-j")
      }
    )

    # Navigation Logic
    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "validation")
    })
  })
}
