mod_classifier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               h4("Current Keywords"),
               textInput(ns("classifier_keywords"), "Enter New Keyword"),
               actionButton(ns("add_classifier_submit"), "Add Manually", class = "btn-primary"),
               hr(),
               DT::dataTableOutput(ns("classifier_list"))
             )
      ),
      column(8,
             wellPanel(
               h4("Keyword Suggester (Naive Bayes)"),
               p("Analyze training data to find predictive words based on your YES/NO coding."),
               actionButton(ns("predict_classifiers"), "Generate Suggestions", class = "btn-info"),
               br(), br(),
               conditionalPanel(
                 condition = sprintf("output['%s']", ns("has_suggestions")),
                 DT::dataTableOutput(ns("suggested_keywords_table"))
               )
             )
      )
    ),
    hr(),
    fluidRow(
      column(6, actionButton(ns("prev_tab"), "← Back to Project", class = "btn-secondary")),
      column(6, div(align = "right",
                    shinyjs::disabled(
                      actionButton(ns("next_tab"), "Go to Training →", class = "btn-primary")
                    )))
    )
  )
}

mod_classifier_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Manual Entry ---
    observeEvent(input$add_classifier_submit, {
      word <- trimws(input$classifier_keywords)
      req(word != "")
      # Initialize dataframe if it somehow became NULL
      if (is.null(state$classifiers)) {
        state$classifiers <- data.frame(Keywords = character(), stringsAsFactors = FALSE)
      }
      state$classifiers <- rbind(state$classifiers, data.frame(Keywords = word, stringsAsFactors = FALSE))
      updateTextInput(session, "classifier_keywords", value = "")
    })

    # --- 2. Naive Bayes Suggester ---
    observeEvent(input$predict_classifiers, {
      results <- state$training_results
      if (is.null(results) || nrow(results) < 10 || length(unique(results$user.coding)) < 2) {
        showNotification("Need more diverse training data (min 10 items, both classes).", type = "warning")
        return()
      }

      withProgress(message = 'Analyzing training data...', value = 0, {
        tryCatch({
          nb_data <- data.frame(text = results$TextData, class = results$user.coding)
          temp_file <- tempfile(fileext = ".csv")
          write.csv(nb_data, temp_file, row.names = FALSE)

          model_results <- train_and_analyze_model(temp_file)
          if(file.exists(temp_file)) unlink(temp_file)

          # Filter using state (Existing + Blacklist)
          top_keywords <- get_filtered_suggestions(
            model_results,
            state$classifiers,
            blacklist = state$keyword_blacklist
          )

          # PERSISTENCE: Save suggestions directly to global state
          state$suggested_keywords <- top_keywords
          showNotification("Suggestions updated and saved!", type = "message")
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })

    # --- 3. Rendering Suggested Table ---
    output$suggested_keywords_table <- DT::renderDataTable({
      # PERSISTENCE: Check global state instead of local reactive
      req(state$suggested_keywords)
      raw_df <- state$suggested_keywords

      char_cols <- sapply(raw_df, is.character)
      num_cols <- sapply(raw_df, is.numeric)
      kw_col <- if(any(char_cols)) names(raw_df)[which(char_cols)[1]] else names(raw_df)[1]
      freq_col <- if(any(num_cols)) names(raw_df)[which(num_cols)[1]] else names(raw_df)[2]

      df <- data.frame(
        Keyword = raw_df[[kw_col]],
        Weight = round(raw_df[[freq_col]],4),
        stringsAsFactors = FALSE
      )

      df$Add <- sapply(seq_len(nrow(df)), function(i) {
        paste0('<button class="btn btn-success btn-sm" onclick="addSuggestedIdx(', i, ')">Add</button>')
      })

      df$Delete <- sapply(seq_len(nrow(df)), function(i) {
        paste0('<button class="btn btn-link" style="color: #dc3545; padding: 0;" onclick="trashSuggestedIdx(', i, ')">
                  <i class="fa fa-trash"></i>
                </button>')
      })

      DT::datatable(df,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 'tp', pageLength = 10),
                    callback = DT::JS(paste0("
          window.addSuggestedIdx = function(idx) {
            Shiny.setInputValue('", ns("add_suggested_index"), "', idx, {priority: 'event'});
          };
          window.trashSuggestedIdx = function(idx) {
            Shiny.setInputValue('", ns("trash_suggested_index"), "', idx, {priority: 'event'});
          };
        "))
      )
    })

    # --- 4. Handle Add/Trash from Suggestions ---
    observeEvent(input$add_suggested_index, {
      idx <- as.numeric(input$add_suggested_index)
      req(idx, state$suggested_keywords)

      word <- state$suggested_keywords$Keyword[idx]
      state$classifiers <- rbind(state$classifiers, data.frame(Keywords = word, stringsAsFactors = FALSE))

      # Update global state suggestions
      state$suggested_keywords <- state$suggested_keywords[-idx, , drop = FALSE]
      showNotification(paste("Added:", word), type = "message")
    })

    observeEvent(input$trash_suggested_index, {
      idx <- as.numeric(input$trash_suggested_index)
      req(idx, state$suggested_keywords)

      word <- state$suggested_keywords$Keyword[idx]
      state$keyword_blacklist <- unique(c(state$keyword_blacklist, tolower(word)))

      # Update global state suggestions
      state$suggested_keywords <- state$suggested_keywords[-idx, , drop = FALSE]
      showNotification(paste("Blacklisted:", word), type = "warning")
    })

    # --- 5. Main Keyword List ---
    output$classifier_list <- DT::renderDataTable({
      req(state$classifiers)
      df <- state$classifiers

      if (nrow(df) == 0) {
        return(DT::datatable(data.frame(Keywords = character(), Delete = character()),
                             options = list(dom = 't'), rownames = FALSE))
      }

      df$Delete <- sapply(seq_len(nrow(df)), function(i) {
        paste0('<button class="btn btn-link" style="color: #dc3545; padding: 0;" onclick="removeActive(', i, ')">
                  <i class="fa fa-trash"></i>
                </button>')
      })

      DT::datatable(df,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(
                      dom = 'tp',
                      pageLength = 5,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                    callback = DT::JS(paste0("
          window.removeActive = function(idx) {
            Shiny.setInputValue('", ns("remove_active_index"), "', idx, {priority: 'event'});
          };
        "))
      )
    })

    observeEvent(input$remove_active_index, {
      idx <- as.numeric(input$remove_active_index)
      req(idx)
      current_list <- state$classifiers
      if (idx > 0 && idx <= nrow(current_list)) {
        removed_word <- current_list$Keywords[idx]
        state$classifiers <- current_list[-idx, , drop = FALSE]
        showNotification(paste("Removed:", removed_word), type = "message")
      }
    })

    # --- 6. Helper Outputs ---
    output$has_suggestions <- reactive({ !is.null(state$suggested_keywords) && nrow(state$suggested_keywords) > 0 })
    outputOptions(output, "has_suggestions", suspendWhenHidden = FALSE)

    # --- 7. Navigation
    observe({
      kw_list <- state$classifiers$Keywords
      is_ready <- !is.null(kw_list) && length(kw_list) > 0 && any(kw_list != "")

      # 1. Handle the "Next" button within the module
      if (is_ready) {
        shinyjs::enable("next_tab")
      } else {
        shinyjs::disable("next_tab")
      }

      # 2. Handle the Tab in the Top Navbar
      # We use shinyjs::runjs to target the CSS selector of the tab link
      if (is_ready) {
        shinyjs::runjs("$('#tabs li a[data-value=\"training\"]').removeClass('disabled').css('pointer-events', 'auto').css('opacity', '1');")
      } else {
        # We add a 'disabled' class and stop pointer events (clicks)
        shinyjs::runjs("$('#tabs li a[data-value=\"training\"]').addClass('disabled').css('pointer-events', 'none').css('opacity', '0.5');")
      }
    })

    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "project")
    })

    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "training")
    })
  })
}
