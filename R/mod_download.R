mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, wellPanel(
        h4("1. Classifier Keywords"),
        uiOutput(ns("download_keywords_ui")) # Dynamic UI
      )),
      column(4, wellPanel(
        h4("2. Training/Gold Standard"),
        uiOutput(ns("download_training_ui")) # Dynamic UI
      )),
      column(4, wellPanel(
        h4("3. Validated Results Package"),
        uiOutput(ns("full_data_ui"))         # Dynamic UI
      ))
    ),
    hr(),
    actionButton(ns("prev_tab"), "← Back to Validation", class = "btn-secondary")
  )
}

mod_download_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ns <- session$ns

    # --- 1. Keywords Download (Conditional) ---
    output$download_keywords_ui <- renderUI({
      clf <- state$classifiers$Keywords
      valid_kw <- clf[clf != "" & !is.na(clf)]

      if (length(valid_kw) == 0) {
        actionButton(ns("kw_disabled"), "No Keywords Found", icon = icon("ban"), class = "btn-secondary disabled")
      } else {
        downloadButton(ns("download_keywords"), "Download CSV", class = "btn-info")
      }
    })

    # --- 2. Training Data Download (Conditional) ---
    output$download_training_ui <- renderUI({
      if (is.null(state$training_results) || nrow(state$training_results) == 0) {
        actionButton(ns("train_disabled"), "No Training Data", icon = icon("ban"), class = "btn-secondary disabled")
      } else {
        downloadButton(ns("download_training"), "Download CSV", class = "btn-primary")
      }
    })

    # --- 3. Full Data & Report Download (Conditional) ---
    output$full_data_ui <- renderUI({
      if (is.null(state$val_complete) || state$val_complete == FALSE) {
        # Show a locked status
        tagList(
          actionButton(ns("full_disabled"), "Validation Incomplete", icon = icon("lock"), class = "btn-secondary disabled"),
          helpText("Requires a successful Perfect Sampling cycle.")
        )
      } else {
        downloadButton(ns("download_full"), "Download Validated Package", class = "btn-success")
      }
    })
    # 1. Keywords Download
    output$download_keywords <- downloadHandler(
      filename = function() { paste0("keywords_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(state$classifiers, file, row.names = FALSE)
      }
    )

    # 2. Training Data Download
    output$download_training <- downloadHandler(
      filename = function() { paste0("training_data_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(state$training_results, file, row.names = FALSE)
      }
    )

    # 3. Full Data Download (Conditional UI)
    output$full_data_ui <- renderUI({
      if (is.null(state$val_complete) || state$val_complete == FALSE) {
        helpText("⚠️ Complete a validation cycle (Cai's N) to unlock full dataset export.")
      } else {
        downloadButton(ns("download_full"), "Download Full Results", class = "btn-success")
      }
    })

    output$download_full <- downloadHandler(
      filename = function() {
        paste0("validated_results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        # Create a temporary directory to store files
        tmpdir <- tempdir()
        setwd(tempdir())

        # --- File 1: The Fully Coded CSV ---
        df <- state$dataset
        t_col <- state$text_column
        clf <- state$classifiers$Keywords
        pattern <- paste(clf[clf != "" & !is.na(clf)], collapse = "|")

        df$Predicted_Positive <- ifelse(grepl(pattern, df[[t_col]], ignore.case = TRUE, perl = TRUE), 1, 0)

        data_file <- "coded_dataset.csv"
        write.csv(df, data_file, row.names = FALSE)

        # --- File 2: The Validation Report (TXT) ---
        report_file <- "validation_report.txt"
        report_conn <- file(report_file)

        writeLines(c(
          "===========================================",
          "     CAI'S N VALIDATION REPORT",
          "===========================================",
          paste("Date:", Sys.Date()),
          paste("Project Code:", ifelse(is.null(state$current_code), "N/A", state$current_code)),
          "",
          "--- CLASSIFIER DETAILS ---",
          paste("Keywords used:", paste(clf, collapse = ", ")),
          paste("Target Text Column:", t_col),
          "",
          "--- STATISTICAL PARAMETERS ---",
          paste("Target Kappa Threshold:", "0.80"),
          paste("Alpha Level:", "0.025"),
          paste("Observed Base Rate (b1):", round(state$val_b1_adj, 4)),
          paste("Required Accuracy (a_max):", round(state$val_a_max, 4)),
          paste("Calculated Cai's N:", state$val_cais_n),
          "",
          "--- RESULTS ---",
          paste("Total Dataset Rows:", nrow(df)),
          paste("Predicted Positive Cases:", sum(df$Predicted_Positive)),
          paste("Validation Status:", "SUCCESS (Perfect Sampling Achieved)"),
          "",
          "Reference: Shaffer, D.W., & Cai, Z. (2012). Perfect Sampling.",
          "==========================================="
        ), report_conn)
        close(report_conn)

        # --- Bundle into ZIP ---
        zip(zipfile = file, files = c(data_file, report_file), extras = "-j")
      }
    )

    # 4. Metadata Table
    output$meta_table <- renderTable({
      data.frame(
        "Metric" = c("Total Dataset Rows", "Training Items Coded", "Validation Cai's N", "Status"),
        "Value" = c(
          nrow(state$dataset),
          nrow(state$training_results),
          ifelse(is.null(state$val_cais_n), "Not Calculated", state$val_cais_n),
          ifelse(state$val_complete, "Validated", "In Progress")
        )
      )
    })

    # 5. Navigation
    observe({
      kw_list <- state$classifiers$Keywords
      is_ready <- !is.null(kw_list) && length(kw_list) > 0 && any(kw_list != "")

      # 1. Handle the "Next" button within the module
      if (is_ready) {
        shinyjs::enable("prev_tab")
      } else {
        shinyjs::disable("prev_tab")
      }

      # 2. Handle the Tab in the Top Navbar
      # We use shinyjs::runjs to target the CSS selector of the tab link
      if (is_ready) {
        shinyjs::runjs("$('#tabs li a[data-value=\"validation\"]').removeClass('disabled').css('pointer-events', 'auto').css('opacity', '1');")
      } else {
        # We add a 'disabled' class and stop pointer events (clicks)
        shinyjs::runjs("$('#tabs li a[data-value=\"validation\"]').addClass('disabled').css('pointer-events', 'none').css('opacity', '0.5');")
      }
    })
    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "validation")
    })
  })
}
