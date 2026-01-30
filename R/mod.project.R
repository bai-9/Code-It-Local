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

mod_project_server <- function(id, state,reset_state_callback, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Helper: Model Path Generation ---
    get_shared_model_path <- function() {
      shiny::req(state$file_name, input$textColumn)

      model_dir <- "./tmp/shared_models"
      if (!dir.exists(model_dir)) {
        dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      }

      clean_name <- gsub("[^[:alnum:]]", "_", state$file_name)
      model_hash <- digest::digest(list(state$file_name, input$textColumn))
      model_name <- paste0(clean_name, "_", model_hash, ".bin")

      return(file.path(model_dir, model_name))
    }

    # Update model path reactively
    observe({
      req(state$file_name, input$textColumn)
      state$shared_model_path <- get_shared_model_path()
    })

    # --- 1. Registry Reactive Poll ---
    registry <- reactivePoll(1000, session,
                             checkFunc = function() {
                               path <- get_registry_path()
                               if(file.exists(path)) file.info(path)$mtime else 1
                             },
                             valueFunc = load_project_registry)

    # --- 2. Data Upload (With Warning Suppression) ---
    observeEvent(input$data_upload, {
      req(input$data_upload)
      file <- input$data_upload

      ext <- tools::file_ext(file$name)
      df <- tryCatch({
        if(ext == "csv") {
          # Added show_col_types = FALSE to silence the parsing message
          readr::read_csv(file$datapath, show_col_types = FALSE, guess_max = 2000)
        } else {
          readxl::read_excel(file$datapath)
        }
      }, error = function(e) {
        showNotification("Error reading file. Please check format.", type = "error")
        return(NULL)
      })

      req(df)
      reset_state_callback()
      state$dataset <- df
      state$file_name <- file$name

      # Update column selector choices
      updateSelectInput(session, "textColumn", choices = names(df))
    })

    # --- 3. Dynamic Code List Management ---
    observe({
      req(state$file_name, input$textColumn)
      reg <- registry()

      # Filter codes matching current File + Column
      existing_codes <- reg$code_name[reg$data_file == state$file_name &
                                        reg$text_column == input$textColumn]

      updateSelectInput(session, "code_selector",
                        choices = c("Create New...", existing_codes),
                        selected = isolate(input$code_selector))
    })

    # --- 4. Auto-Load Logic (Safe Loading) ---
    observe({
      req(state$file_name, input$textColumn, input$code_selector)
      if (input$code_selector == "Create New...") return()

      # Prevent redundant loading if already active
      if (!is.null(state$current_code) && state$current_code == input$code_selector) return()

      # Safety Check: Ensure the selected code actually belongs to the current file/column
      # This prevents the app from trying to load a project before the registry updates
      reg <- registry()
      valid_match <- any(reg$data_file == state$file_name &
                           reg$text_column == input$textColumn &
                           reg$code_name == input$code_selector)
      req(valid_match)

      # 1. Get state file name
      state_file <- save_project_to_registry(state$file_name, input$textColumn, input$code_selector)

      # 2. Load the RDS
      full_path <- file.path("./tmp/user_data/local_user", state_file)
      if (file.exists(full_path)) {
        saved_data <- readRDS(full_path)
        # Update reactive state
        for(n in names(saved_data)) state[[n]] <- saved_data[[n]]
        state$shared_model_path <- get_shared_model_path()

        showNotification(paste("Project Restored:", state$current_code), type = "message")
      }else
      {
        reset_state_callback(type="partial")
      }
    })

    # --- 5. Manual Create Logic ---
    observeEvent(input$create_code_btn, {
      req(input$new_code_name, state$file_name, input$textColumn)

      new_name <- trimws(input$new_code_name)
      if (new_name == "" || new_name == "Create New...") {
        showNotification("Please enter a valid code name.", type = "warning")
        return()
      }

      # 1. Register new project
      state_file <- save_project_to_registry(state$file_name, input$textColumn, new_name)

      # 2. Initialize clean state
      state$current_state_file <- state_file
      state$current_code <- new_name
      state$text_column <- input$textColumn
      state$file_name <- state$file_name
      state$shared_model_path <- get_shared_model_path()

      # Reset logic data

      #state$classifiers <- data.frame(Keywords = character(), stringsAsFactors = FALSE)
      #state$training_results <- data.frame(TextData = character(), user.coding = numeric(), stringsAsFactors = FALSE)
      #state$val_results_cycle <- data.frame()
      #state$val_complete <- FALSE
      #state$val_failed <- FALSE
      reset_state_callback(type="partial")
      # 3. UI Updates
      updateSelectInput(session, "code_selector", selected = new_name)
      updateTextInput(session, "new_code_name", value = "")

      showNotification("New Project Environment Created!", type = "warning")
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
            tags$li(strong("Code Label: "), state$current_code)
          ),
          p(em("Auto-saving to: "), code(state$current_state_file))
      )
    })

    # --- 7. Navigation & Button State ---
    observe({
      code_is_set <- !is.null(state$current_code) &&
        state$current_code != "" &&
        state$current_code != "Create New..."

      is_ready <- !is.null(state$dataset) &&
        !is.null(state$text_column) &&
        code_is_set &&
        !is.null(state$current_state_file)

      # Toggle UI availability
      if (is_ready) {
        shinyjs::enable("next_tab")
        shinyjs::runjs("$('#tabs li a[data-value=\"create_classifiers\"]').removeClass('disabled').css('pointer-events', 'auto').css('opacity', '1');")
      } else {
        shinyjs::disable("next_tab")
        shinyjs::runjs("$('#tabs li a[data-value=\"create_classifiers\"]').addClass('disabled').css('pointer-events', 'none').css('opacity', '0.5');")
      }
    })

    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "create_classifiers")
    })
  })
}
