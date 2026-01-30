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
               radioButtons(ns("view_mode"), "Display Mode:",
                            choices = c("Conflicts Only" = "conflicts", "All Items" = "all"),
                            inline = TRUE),
               br(),
               div(id = ns("text_viewer_container"),
                   h5(icon("eye"), "Full Text Viewer"),
                   htmlOutput(ns("full_text_display"),
                              style = "background: #f9f9f9; padding: 10px; border: 1px solid #ddd; border-radius: 4px; max-height: 200px; overflow-y: auto;")
               ),
               br(),
               # --- NEW ELEMENT START ---
               div(id = ns("hits_container"),
                   h5(icon("filter"), "Matched Keywords in Selection"),
                   uiOutput(ns("matching_keywords_display")),
                   style = "margin-bottom: 15px;"
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

    # (Processed_data reactive remains the same as your current code)
    processed_data <- reactive({
      req(state$training_results)
      df <- state$training_results
      if (nrow(df) == 0) {
        df$classifier_rating <- numeric(); df$status <- character()
        return(df)
      }
      df$user.coding <- ifelse(df$user.coding <= 0, 0, 1)
      patterns <- state$classifiers$Keywords
      pattern_string <- if(!is.null(patterns) && length(patterns) > 0) paste(patterns, collapse = "|") else ""

      if (pattern_string == "" || is.na(pattern_string)) {
        df$classifier_rating <- 0
      } else {
        df$classifier_rating <- tryCatch({
          as.numeric(grepl(pattern_string, df$TextData, ignore.case = TRUE, perl = TRUE))
        }, error = function(e) rep(0, nrow(df)))
      }
      df$status <- ifelse(df$user.coding == df$classifier_rating, "Match", "Conflict")
      if (input$view_mode == "conflicts") df <- df[df$status == "Conflict", ]
      return(df)
    })

    # --- NEW: MATCHING KEYWORDS LOGIC ---
    output$matching_keywords_display <- renderUI({
      s <- input$review_table_rows_selected
      df <- processed_data()
      kws <- state$classifiers$Keywords

      # Validation
      if (length(s) == 0 || nrow(df) == 0 || is.null(kws) || length(kws) == 0) {
        return(tags$small(class = "text-muted", "No matches or no row selected."))
      }

      # Identify which specific regexes hit the selected text
      selected_text <- df$TextData[s]
      hits <- kws[sapply(kws, function(k) grepl(k, selected_text, ignore.case = TRUE, perl = TRUE))]

      if (length(hits) == 0) {
        return(tags$span(class = "label label-default", "No regex matches found"))
      }

      # Render matches as small badges/tags
      tagList(
        lapply(hits, function(h) {
          span(h, class = "label label-info", style = "margin-right: 5px; display: inline-block; padding: 5px;")
        })
      )
    })

    # --- REMAINING OUTPUTS (kw_table, review_table, etc. stay as they were) ---

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
        if (is.null(state$classifiers) || ncol(state$classifiers) == 0) {
          state$classifiers <- data.frame(Keywords = new_val, stringsAsFactors = FALSE)
        } else {
          state$classifiers <- rbind(state$classifiers, data.frame(Keywords = new_val))
        }
        updateTextInput(session, "new_kw", value = "")
      }
    })

    observeEvent(input$delete_kw_idx, {
      idx <- as.numeric(input$delete_kw_idx)
      if (!is.na(idx)) state$classifiers <- state$classifiers[-idx, , drop = FALSE]
    })

    output$review_table <- DT::renderDT({
      df <- processed_data()
      req(nrow(df) > 0)

      DT::datatable(
        df,
        class = "cell-border editable-table",
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
            }")))),
            colnames = c("Text Data", "User Rating", "Classifier Rating", "Match Status")) %>%
          DT::formatStyle(
            'status', target = 'row',
            backgroundColor = DT::styleEqual(c("Match", "Conflict"), c("#d4edda", "#f8d7da"))
          )
    })

    observeEvent(input$review_table_cell_edit, {
      info <- input$review_table_cell_edit
      df_current <- processed_data()
      row_text <- df_current$TextData[info$row]
      state$training_results$user.coding[state$training_results$TextData == row_text] <- as.numeric(info$value)
    })

    output$full_text_display <- renderUI({
      s <- input$review_table_rows_selected
      df <- processed_data()
      if (length(s) == 0 || nrow(df) == 0) return(tags$i("Click a row in the table above to view the full text content."))
      tags$div(df$TextData[s])
    })

    observeEvent(input$prev_tab, { updateNavbarPage(parent_session, "tabs", selected = "training") })
    observeEvent(input$next_tab, { updateNavbarPage(parent_session, "tabs", selected = "validation") })
  })
}
