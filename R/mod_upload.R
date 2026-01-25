mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      h4("Upload Data"),
      fluidRow(
        column(4,
               fileInput(ns("data_upload"), "Upload CSV or Excel File",
                         accept = c(".csv", ".xls", ".xlsx")),
               # This will display the persistent filename
               uiOutput(ns("file_name_display"))
        ),
        column(2),
        column(4,
               selectInput(ns("textColumn"), "Select the column with text data", choices = "")
        )
      ),
      fluidRow(
        column(12, DT::dataTableOutput(ns("uploaded_data")))
      )
    ),
    div(class = "alert alert-info",
        actionButton(ns("goto_create_code"), "Go to Create Code →", class = "btn-primary")
    )
  )
}

mod_upload_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. UI Rehydration: Filename and Column Dropdown ---

    # Render the filename message dynamically
    output$file_name_display <- renderUI({
      req(state$file_name)
      p(tags$strong("Current file: "), state$file_name,
        style = "color: #2c3e50; margin-top: -10px; font-size: 0.9em;")
    })

    # Sync the dropdown choices on startup
    observe({
      req(state$dataset)
      updateSelectInput(session, "textColumn",
                        choices = names(state$dataset),
                        selected = state$text_column)
    })

    # --- 2. Handling New File Uploads ---
    observeEvent(input$data_upload, {
      req(input$data_upload)

      file <- input$data_upload
      ext <- tools::file_ext(file$name)

      # Load the data
      df <- if(ext == "csv") {
        readr::read_csv(file$datapath)
      } else {
        readxl::read_excel(file$datapath)
      }

      # Update State: Data and the new Filename
      state$dataset <- df
      state$file_name <- file$name # This triggers the UI output to update

      # Update UI choices
      updateSelectInput(session, "textColumn",
                        choices = names(df),
                        selected = names(df)[1])
    })

    # --- 3. Text Column Selection ---
    observeEvent(input$textColumn, {
      # Prevent overwriting state with empty string during initialization
      if (!is.null(input$textColumn) && input$textColumn != "") {
        state$text_column <- input$textColumn
      }
    })

    # --- 4. Navigation ---
    observeEvent(input$goto_create_code, {
      updateNavbarPage(session = parent_session,
                       inputId = "tabs",
                       selected = "create_codebook")
    })

    # --- 5. Data Preview Table ---
    output$uploaded_data <- DT::renderDataTable({
      req(state$dataset)
      DT::datatable(
        head(state$dataset, 10),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          columnDefs = list(list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display' && data != null && data.length > 50) {",
              "    return '<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>';",
              "  } else { return data; }",
              "}"
            )
          ))
        )
      )
    })
  })
}
