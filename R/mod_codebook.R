mod_codebook_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Create Your Code"),
      textInput(ns("code_name_input"), "Code Name:", value = "New Code"),
      actionButton(ns("save_code_btn"), "Save Code Name", class = "btn-success"),
      # Added a small feedback message
      uiOutput(ns("save_feedback"))
    ),
    div(class = "alert alert-info",
        actionButton(ns("goto_classifiers"), "Go to Create Classifiers →", class = "btn-primary")
    )
  )
}

mod_codebook_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Rehydration Logic (Startup) ---
    # When the module starts, push the saved state back into the UI
    observe({
      # Use isolate to ensure this only runs once on startup or when state is loaded
      # but doesn't create a reactive loop with the text input itself
      if (!is.null(state$current_code)) {
        updateTextInput(session, "code_name_input", value = state$current_code)
      }
    })

    # --- 2. Save Logic ---
    observeEvent(input$save_code_btn, {
      req(input$code_name_input)
      state$current_code <- input$code_name_input

      # Visual feedback for the user
      showNotification(paste("Code name saved as:", state$current_code), type = "message")
    })

    # Optional feedback text below the button
    output$save_feedback <- renderUI({
      req(state$current_code)
      p(em(paste("Current active code:", state$current_code)),
        style = "margin-top: 10px; color: #2c3e50; font-size: 0.9em;")
    })

    # --- 3. Navigation ---
    observeEvent(input$goto_classifiers, {
      updateNavbarPage(session = parent_session,
                       inputId = "tabs",
                       selected = "create_classifiers")
    })
  })
}
