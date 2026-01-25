mod_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # LEFT: Regex Management
      column(4,
             wellPanel(
               h4("1. Refine Keywords"),
               div(style = "display: flex; gap: 5px;",
                   textInput(ns("new_kw"), NULL, placeholder = "Add new keyword..."),
                   actionButton(ns("add_kw_btn"), "", icon = icon("plus"), class = "btn-success")
               ),
               DT::DTOutput(ns("kw_table"))
             )
      ),

      # RIGHT: Coded Items Review
      column(8,
             wellPanel(
               h4("2. Review Coded Items"),
               radioButtons(ns("view_mode"), "Display Mode:",
                            choices = c("Conflicts Only" = "conflicts", "All Items" = "all"),
                            inline = TRUE),
               br(),
               div(id = ns("text_viewer_container"),
                   verbatimTextOutput(ns("full_text_display"), placeholder = TRUE)
               ),
               br(),
               DT::DTOutput(ns("review_table"))
             )
      )
    ),

    # NAVIGATION
    hr(),
    fluidRow(
      column(6, actionButton(ns("prev_tab"), "← Back to Training", class = "btn-secondary")),
      column(6, div(align = "right",
                    actionButton(ns("next_tab"), "Go to Validation →", class = "btn-primary")))
    )
  )
}

mod_review_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. DATA PREP HELPER ---
    # This reactive helps keep the logic consistent across the table and the viewer
    # --- 1. DATA PREP HELPER (Corrected) ---
    processed_data <- reactive({
      # Requirement: training_results must exist
      req(state$training_results)
      df <- state$training_results

      # FIX: If the dataframe is empty, return it immediately before
      # trying to add calculated columns
      if (nrow(df) == 0) {
        # Ensure the columns exist so the table doesn't crash
        df$classifier_rating <- numeric()
        df$status <- character()
        return(df)
      }

      # Build pattern from current classifiers
      patterns <- state$classifiers$Keywords
      pattern_string <- if(!is.null(patterns) && length(patterns) > 0) {
        paste(patterns, collapse = "|")
      } else {
        ""
      }

      # Calculate Classifier Rating
      if (pattern_string == "" || is.na(pattern_string)) {
        df$classifier_rating <- 0
      } else {
        df$classifier_rating <- tryCatch({
          as.numeric(grepl(pattern_string, df$TextData, ignore.case = TRUE, perl = TRUE))
        }, error = function(e) rep(0, nrow(df)))
      }

      # Determine Match Status
      df$status <- ifelse(df$user.coding == df$classifier_rating, "Match", "Conflict")

      # Apply Filter View
      if (input$view_mode == "conflicts") {
        df <- df[df$status == "Conflict", ]
      }

      return(df)
    })

    # --- 2. REGEX MANAGEMENT (LEFT SIDE) ---

    # Render Keyword Table
    output$kw_table <- DT::renderDT({
      df <- if (!is.null(state$classifiers)) state$classifiers else data.frame(Keywords = character())

      if (nrow(df) > 0) {
        df$Action <- sapply(1:nrow(df), function(i) {
          as.character(actionButton(ns(paste0("del_", i)), "",
                                    icon = icon("trash"),
                                    class = "btn-danger btn-xs",
                                    onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                                                      ns("delete_kw_idx"), i)))
        })
      }

      DT::datatable(df, escape = FALSE, selection = 'none', rownames = FALSE,
                    options = list(dom = 't', pageLength = 100, scrollY = "300px"))
    })

    # Add Keyword

    observeEvent(input$add_kw_btn, {
      req(input$new_kw)
      new_val <- trimws(input$new_kw)

      if(new_val != "") {
        # Check if state$classifiers is NULL or doesn't have the column
        if (is.null(state$classifiers) || ncol(state$classifiers) == 0) {
          state$classifiers <- data.frame(Keywords = new_val, stringsAsFactors = FALSE)
        } else {
          # Standard append
          new_row <- data.frame(Keywords = new_val, stringsAsFactors = FALSE)
          state$classifiers <- rbind(state$classifiers, new_row)
        }

        # Clear the input box after adding
        updateTextInput(session, "new_kw", value = "")
      }
    })

    # Delete Keyword by Index
    observeEvent(input$delete_kw_idx, {
      idx <- as.numeric(input$delete_kw_idx)
      if (!is.na(idx) && idx <= nrow(state$classifiers)) {
        state$classifiers <- state$classifiers[-idx, , drop = FALSE]
      }
    })

    # --- 3. REVIEW TABLE (RIGHT SIDE) ---

    output$review_table <- DT::renderDT({
      df <- processed_data()

      # Ensure we have data
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No items to review.")))
      }

      # Columns in df are:
      # [1] TextData, [2] user.coding, [3] classifier_rating, [4] status

      DT::datatable(df,
                    selection = 'single',
                    rownames = FALSE, # If this is FALSE, TextData is index 0
                    editable = list(
                      target = "cell",
                      # We want to DISABLE editing for columns 0, 2, and 3
                      # This leaves index 1 (user.coding) as the ONLY editable column
                      disable = list(columns = c(0, 2, 3))
                    ),
                    options = list(
                      pageLength = 10,
                      dom = 'tp',
                      columnDefs = list(list(
                        targets = 0,
                        render = JS("function(data, type, row) {
              return type === 'display' && data.length > 80 ?
                data.substr(0, 80) + '...' : data;
            }")
                      ))
                    ),
                    colnames = c("Text Data", "User Rating", "Classifier Rating", "Match Status")
      ) %>%
        DT::formatStyle(
          'status', target = 'row',
          backgroundColor = DT::styleEqual(c("Match", "Conflict"), c("#d4edda", "#f8d7da"))
        )
    })

    # Handle Manual User Rating Edits
    observeEvent(input$review_table_cell_edit, {
      info <- input$review_table_cell_edit
      df_current <- processed_data()

      # Use the unique text from the edited row to update the main state
      row_text <- df_current$TextData[info$row]
      state$training_results$user.coding[state$training_results$TextData == row_text] <- as.numeric(info$value)
    })

    # --- 4. FULL TEXT VIEWER ---

    output$full_text_display <- renderText({
      s <- input$review_table_rows_selected
      df <- processed_data()

      if (length(s) == 0 || nrow(df) == 0) {
        return("Click a row in the table above to view the full text content.")
      }

      df$TextData[s]
    })

    # --- 5. NAVIGATION ---

    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "training")
    })

    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "validation")
    })

    # Gatekeeping: Disable next tab if there are still conflicts (Optional)
    observe({
      df <- processed_data()
      conflicts_exist <- any(df$status == "Conflict")
      # You can decide if you want to strictly block them or just warn them
    })
  })
}
