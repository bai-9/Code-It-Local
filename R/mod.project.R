mod_project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(4,
               fileInput(ns("data_upload"), "1. Data Source", accept = c(".csv", ".xls", ".xlsx")),
               uiOutput(ns("file_status"))
        ),
        column(4,
               selectInput(ns("textColumn"), "2. Text Column", choices = "")
        ),
        column(4,
               selectInput(ns("code_selector"), "3. Select/Load Code", choices = c("Create New...")),
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'Create New...'", ns("code_selector")),
                 div(style = "display: flex; gap: 5px;",
                     textInput(ns("new_code_name"), NULL, placeholder = "Enter New Code Name"),
                     actionButton(ns("create_code_btn"), "Create", class = "btn-success")
                 )
               )
        )
      )
    ),
    # A clear visual confirmation of what is currently active
    uiOutput(ns("active_project_summary")),
    # Inside mod_project_ui at the bottom
    div(align = "right",
        shinyjs::disabled(
          actionButton(ns("next_tab"), "Go to Classifiers →", class = "btn-primary")
        )
    )
  )
}

mod_project_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Registry Reactive Poll ---
    # Watches the projects.rds file for changes every second
    registry <- reactivePoll(1000, session,
                             checkFunc = function() {
                               path <- get_registry_path()
                               if(file.exists(path)) file.info(path)$mtime else 1
                             },
                             valueFunc = load_project_registry)

    # --- 2. Data Upload & Column Population ---
    observeEvent(input$data_upload, {
      req(input$data_upload)
      file <- input$data_upload

      # Process file based on extension
      ext <- tools::file_ext(file$name)
      df <- tryCatch({
        if(ext == "csv") readr::read_csv(file$datapath)
        else readxl::read_excel(file$datapath)
      }, error = function(e) {
        showNotification("Error reading file. Please check format.", type = "error")
        return(NULL)
      })

      req(df)
      state$dataset <- df
      state$file_name <- file$name

      # Update column selector
      updateSelectInput(session, "textColumn", choices = names(df))
    })

    # --- 3. Dynamic Code List Management ---
    # Filters existing codes based on selected File + Column
    observe({
      req(state$file_name, input$textColumn)
      reg <- registry()

      # Find codes matching this specific data/column combination
      existing_codes <- reg$code_name[reg$data_file == state$file_name &
                                        reg$text_column == input$textColumn]

      updateSelectInput(session, "code_selector",
                        choices = c("Create New...", existing_codes),
                        selected = isolate(input$code_selector))
    })

    # --- 4. AUTO-LOAD LOGIC (Existing Projects) ---
    # Triggered immediately when an existing code is selected
    observe({
      req(state$file_name, input$textColumn, input$code_selector)
      req(input$code_selector != "Create New...")

      # Prevent redundant loading if this is already the active project
      if (!is.null(state$current_code) && state$current_code == input$code_selector) return()

      # 1. Get the state file name from registry
      state_file <- save_project_to_registry(state$file_name, input$textColumn, input$code_selector)

      # 2. Load the RDS into global state
      full_path <- file.path("./tmp/user_data", state_file)
      if (file.exists(full_path)) {
        saved_data <- readRDS(full_path)
        # Update every key in the reactiveValues object
        for(n in names(saved_data)) state[[n]] <- saved_data[[n]]

        showNotification(paste("Project Restored:", state$current_code), type = "message")


      }
    })

    # --- 5. MANUAL CREATE LOGIC (New Projects) ---
    # Triggered only when the 'Create' button is clicked
    observeEvent(input$create_code_btn, {
      req(input$new_code_name, state$file_name, input$textColumn)

      new_name <- trimws(input$new_code_name)
      if (new_name == "") {
        showNotification("Please enter a valid code name.", type = "warning")
        return()
      }

      # 1. Register the new project and get a filename
      state_file <- save_project_to_registry(state$file_name, input$textColumn, new_name)

      # 2. Initialize a clean state for this new project
      state$current_state_file <- state_file
      state$current_code <- new_name
      state$text_column <- input$textColumn
      state$file_name <- state$file_name

      # Reset progress data
      state$classifiers <- data.frame(Keywords = character(), stringsAsFactors = FALSE)
      state$training_results <- data.frame(TextData = character(), user.coding = numeric(), stringsAsFactors = FALSE)
      state$val_results_cycle <- data.frame()
      state$val_complete <- FALSE
      state$val_failed <- FALSE

      # 3. UI Updates
      updateSelectInput(session, "code_selector", selected = new_name)
      updateTextInput(session, "new_code_name", value = "")

      showNotification("New Project Environment Created!", type = "warning")

      # 4. Auto-navigate to Classifiers
      updateNavbarPage(parent_session, "tabs", selected = "create_classifiers")
    })

    # --- 6. Visual Feedback ---
    output$file_status <- renderUI({
      req(state$file_name)
      span(icon("check-circle"), state$file_name, style = "color: #28a745; font-size: 0.9em;")
    })

    output$active_project_summary <- renderUI({
      req(state$current_state_file)
      div(class = "alert alert-success",
          style = "margin-top: 25px; border-left: 5px solid #28a745;",
          h4(icon("rocket"), "Environment Initialized"),
          tags$ul(
            tags$li(strong("Dataset: "), state$file_name),
            tags$li(strong("Variable: "), state$text_column),
            tags$li(strong("Codebook Label: "), state$current_code)
          ),
          p(em("All progress is being automatically saved to: "), code(state$current_state_file))
      )
    })
    # navigation

    observe({
      # A code is only 'determined' if the global state has a value AND
      # that value isn't just an empty string or the default placeholder.
      code_is_set <- !is.null(state$current_code) &&
        state$current_code != "" &&
        state$current_code != "New Code"

      # Comprehensive "Ready" check
      is_ready <- !is.null(state$dataset) &&
        !is.null(state$text_column) &&
        code_is_set &&
        !is.null(state$current_state_file)

      # Toggle Next Button
      if (is_ready) {
        shinyjs::enable("next_tab")
      } else {
        shinyjs::disable("next_tab")
      }

      # Toggle Navbar Tab
      if (is_ready) {
        shinyjs::runjs("$('#tabs li a[data-value=\"create_classifiers\"]').removeClass('disabled').css('pointer-events', 'auto').css('opacity', '1');")
      } else {
        shinyjs::runjs("$('#tabs li a[data-value=\"create_classifiers\"]').addClass('disabled').css('pointer-events', 'none').css('opacity', '0.5');")
      }
    })
    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "create_classifiers")
    })
  })
}
