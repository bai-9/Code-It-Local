mod_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # LEFT: Regex Management
      column(4,
             wellPanel(
               h4("1. Refine Keywords"),
               div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
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
               #radioButtons(ns("view_mode"), "Display Mode:",
               radioButtons(ns("view_mode"), "",
                            choices = c("Conflicts Only" = "conflicts", "All Items" = "all"),
                            inline = TRUE),
               #br(),
               div(id = ns("text_viewer_container"),
                   #h5(icon("eye"), "Full Text Viewer"),
                   htmlOutput(ns("full_text_display"),
                              style = "background: #f9f9f9; padding: 10px; border: 1px solid #ddd; border-radius: 4px; max-height: 200px; overflow-y: auto;")
               ),
               #br(),
               # --- NEW ELEMENT START ---
               # --- Inside mod_review_ui ---
               div(id = ns("hits_container"),
                   fluidRow(
                     column(6,
                            #h5(icon("filter"), "Matched Keywords"),
                            uiOutput(ns("matching_keywords_display"))
                     ),
                     column(6,
                            #h5(icon("check-circle"), "Set User Rating"),
                            radioButtons(ns("set_user_rating"), NULL,
                                         choices = c("0" = 0, "1" = 1),
                                         inline = TRUE, selected = character(0))
                     )
                   ),
                   style = "margin-bottom: 15px; padding: 10px; border: 1px solid #eee; border-radius: 5px; background: #fcfcfc;"
               ),
               # --- NEW ELEMENT END ---
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

    # Create a proxy to manipulate the table (allows re-selecting rows)
    proxy <- DT::dataTableProxy("review_table")

    # --- 1. DATA PREPARATION ---
    processed_data <- reactive({
      req(state$training_results)
      df <- state$training_results

      if (nrow(df) == 0) {
        df$classifier_rating <- numeric()
        df$status <- character()
        return(df)
      }

      # Force binary safety: 0 stays 0, anything else (or NA) becomes 1
      df$user.coding <- ifelse(as.numeric(df$user.coding) <= 0 | is.na(df$user.coding), 0, 1)

      # Build regex pattern from keyword list
      patterns <- state$classifiers$Keywords
      pattern_string <- if(!is.null(patterns) && length(patterns) > 0) {
        paste(patterns, collapse = "|")
      } else { "" }

      # Calculate Classifier Rating via regex
      df$classifier_rating <- if (pattern_string == "" || is.na(pattern_string)) {
        0
      } else {
        tryCatch({
          as.numeric(grepl(pattern_string, df$TextData, ignore.case = TRUE, perl = TRUE))
        }, error = function(e) rep(0, nrow(df)))
      }

      # Determine Match Status
      df$status <- ifelse(df$user.coding == df$classifier_rating, "Match", "Conflict")

      # Apply Filter View (Conflicts vs All)
      if (input$view_mode == "conflicts") {
        df <- df[df$status == "Conflict", ]
      }

      return(df)
    })

    # --- 2. UI STATE CONTROL (SHINYJS) ---
    # Enable/Disable radio buttons based on selection
    observe({
      if (length(input$review_table_rows_selected) > 0) {
        shinyjs::enable("set_user_rating")
      } else {
        shinyjs::disable("set_user_rating")
        # Clear radio selection if no row is picked
        updateRadioButtons(session, "set_user_rating", selected = character(0))
      }
    })

    # --- 3. SYNC RADIO BUTTON WITH TABLE SELECTION ---
    observeEvent(input$review_table_rows_selected, {
      s <- input$review_table_rows_selected
      df <- processed_data()
      req(length(s) > 0)

      # When a row is clicked, set the radio button to that row's rating
      current_val <- df$user.coding[s]
      updateRadioButtons(session, "set_user_rating", selected = as.character(current_val))
    })

    # --- 4. UPDATE MASTER DATA & PERSIST SELECTION ---
    observeEvent(input$set_user_rating, {
      s <- input$review_table_rows_selected
      req(length(s) > 0)

      df_current <- processed_data()
      selected_text <- df_current$TextData[s]
      new_rating <- as.numeric(input$set_user_rating)

      # Update the master dataframe in the reactive 'state'
      # We match by text string to ensure we update the right row even if filtered/sorted
      state$training_results$user.coding[state$training_results$TextData == selected_text] <- new_rating

      # CRITICAL: Re-select the row so the focus doesn't jump
      DT::selectRows(proxy, s)
    }, ignoreInit = TRUE)

    # --- 5. FULL TEXT DISPLAY ---
    output$full_text_display <- renderUI({
      s <- input$review_table_rows_selected
      df <- processed_data()
      if (length(s) == 0 || nrow(df) == 0) return(tags$i("Click a row in the table below to view content."))

      tags$div(style = "white-space: pre-wrap; background: #fff; padding: 10px; border-radius: 4px;",
               df$TextData[s])
    })

    # --- 6. MATCHING KEYWORDS DISPLAY ---
    output$matching_keywords_display <- renderUI({
      s <- input$review_table_rows_selected
      df <- processed_data()
      kws <- state$classifiers$Keywords

      if (length(s) == 0 || is.null(kws) || length(kws) == 0) {
        return(tags$small(class = "text-muted", "No hits to display."))
      }

      selected_text <- df$TextData[s]
      hits <- kws[sapply(kws, function(k) grepl(k, selected_text, ignore.case = TRUE, perl = TRUE))]

      if (length(hits) == 0) return(tags$span(class = "label label-default", "No regex matches"))

      tagList(
        lapply(hits, function(h) {
          span(h, class = "label label-info",
               style = "margin-right: 5px; display: inline-block; padding: 5px; border-radius: 3px;")
        })
      )
    })

    # --- 7. KEYWORD MANAGEMENT TABLE (LEFT) ---
    output$kw_table <- DT::renderDT({
      df <- if (!is.null(state$classifiers)) state$classifiers else data.frame(Keywords = character())
      if (nrow(df) > 0) {
        df$Action <- sapply(1:nrow(df), function(i) {
          as.character(actionButton(ns(paste0("del_", i)), "", icon = icon("trash"), class = "btn-danger btn-xs",
                                    onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("delete_kw_idx"), i)))
        })
      }
      DT::datatable(df, escape = FALSE, selection = 'none', rownames = FALSE,
                    options = list(dom = 't', pageLength = 100, scrollY = "300px"))
    })

    observeEvent(input$add_kw_btn, {
      req(input$new_kw)
      new_val <- trimws(input$new_kw)
      if(new_val != "") {
        new_row <- data.frame(Keywords = new_val, stringsAsFactors = FALSE)
        if (is.null(state$classifiers) || ncol(state$classifiers) == 0) {
          state$classifiers <- new_row
        } else {
          state$classifiers <- rbind(state$classifiers, new_row)
        }
        updateTextInput(session, "new_kw", value = "")
      }
    })

    observeEvent(input$delete_kw_idx, {
      idx <- as.numeric(input$delete_kw_idx)
      if (!is.na(idx)) state$classifiers <- state$classifiers[-idx, , drop = FALSE]
    })

    # --- 8. REVIEW DATA TABLE (RIGHT) ---
    output$review_table <- DT::renderDT({
      df <- processed_data()
      req(nrow(df) > 0)

      DT::datatable(
        df,
        class = "cell-border",
        selection = 'single',
        rownames = FALSE,
        editable = FALSE,
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

    # --- 9. NAVIGATION ---
    observeEvent(input$prev_tab, { updateNavbarPage(parent_session, "tabs", selected = "training") })
    observeEvent(input$next_tab, { updateNavbarPage(parent_session, "tabs", selected = "validation") })
  })
}
