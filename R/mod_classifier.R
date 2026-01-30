mod_classifier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               h4("Current Regex"),
               textInput(ns("classifier_keywords"), "Enter New Regex"),
               actionButton(ns("add_classifier_submit"), "Add Manually", class = "btn-primary"),
               hr(),
               DT::dataTableOutput(ns("classifier_list"))
             )
      ),
      column(8,
             wellPanel(
               h4("Keyword Suggester"),
               # Requirement 1: Method Selection
               shinyjs::hidden(
               radioButtons(ns("suggest_method"), "Suggestion Engine:",
                             choices = c("Naive Bayesian", "Embedding"),
                             selected = "Naive Bayesian", inline = TRUE)),
               p("Find predictive words based on your coding (Bayesian) or semantic relationships (Embedding)."),
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
      if (is.null(state$classifiers)) {
        state$classifiers <- data.frame(Keywords = character(), stringsAsFactors = FALSE)
      }
      state$classifiers <- rbind(state$classifiers, data.frame(Keywords = word, stringsAsFactors = FALSE))
      updateTextInput(session, "classifier_keywords", value = "")
    })

    # --- 2. Keyword Suggestion Engine ---
    # --- 2. Keyword Suggestion Engine ---
    observeEvent(input$predict_classifiers, {
      results <- state$training_results
      if (is.null(results) || nrow(results) < 10 || length(unique(results$user.coding)) < 2) {
        showNotification("Need more diverse training data.", type = "warning")
        return()
      }

      # --- NEW PENALTY LOGIC BLOCK ---
      # 1) Find the ignored list from previous suggestions
      if (!is.null(state$suggested_keywords) && nrow(state$suggested_keywords) > 0) {
        prev_suggestions <- state$suggested_keywords$Keyword
        active_keywords <- state$classifiers$Keywords

        # Ignored words = suggested last time but NOT added to the manual list
        ignored_this_round <- prev_suggestions[!(prev_suggestions %in% active_keywords)]

        if (length(ignored_this_round) > 0) {
          # Ensure state$panelty is initialized
          if (is.null(state$panelty)) {
            state$panelty <- data.frame(Word = character(), value = numeric(), stringsAsFactors = FALSE)
          }

          # 2) Penalize existing words in the penalty list (multiply by 0.9)
          existing_idx <- state$panelty$Word %in% ignored_this_round
          #state$panelty$value[existing_idx] <- state$panelty$value[existing_idx] * 0.9
          state$panelty$value[existing_idx] <- 0.1

          # 3) Add new ignored words with initial value 0.9
          new_ignores <- ignored_this_round[!(ignored_this_round %in% state$panelty$Word)]
          if (length(new_ignores) > 0) {
            new_rows <- data.frame(
              Word = new_ignores,
              value = 0.9,
              stringsAsFactors = FALSE
            )
            state$panelty <- rbind(state$panelty, new_rows)
          }
        }
      }
      # --- END PENALTY LOGIC ---

      #bin_path <- get_bin_path(state$current_state_file)
      bin_path <- state$shared_model_path

      # A. Lazy Build with Blocking Modal
      if (input$suggest_method == "Embedding" && !file.exists(bin_path)) {
        showModal(modalDialog(
          title = "Building Embedding Space",
          div(style = "text-align: center;",
              p("Analyzing your corpus..."),
              icon("gear", class = "fa-spin fa-3x", style = "margin: 20px 0; color: #17a2b8;")
          ),
          footer = NULL, easyClose = FALSE
        ))

        tryCatch({
          build_text_space(state$dataset[[state$text_column]], bin_path)
          removeModal()
        }, error = function(e) {
          removeModal(); showNotification(paste("Build Error:", e$message), type = "error")
          return()
        })
      }

      # B. Generation Step
      withProgress(message = 'Generating Suggestions...', value = 0.5, {
        tryCatch({
          if (input$suggest_method == "Embedding") {
            # Pass state$panelty instead of a simple blacklist vector
            top_keywords <- fasttext_suggest_keywords(
              model_path = bin_path,
              training_df = state$training_results %>% rename(text = TextData, label = user.coding),
              full_corpus = state$dataset[[state$text_column]],
              current_regex = state$classifiers$Keywords,
              penalty_df = state$panelty, # Pass the new dataframe
              top_n = 10
            )
          } else {
            # NAIVE BAYESIAN METHOD
            # You can adapt your NB function to use state$panelty value similarly
            nb_data <- data.frame(text = results$TextData, class = results$user.coding)
            temp_file <- tempfile(fileext = ".csv")
            write.csv(nb_data, temp_file, row.names = FALSE)

            model_results <- train_and_analyze_model(temp_file)
            if(file.exists(temp_file)) unlink(temp_file)

            # Pass penalty data to your NB filter
            top_keywords <- get_filtered_suggestions(
              model_results,
              state$classifiers,
              penalty_df = state$panelty
            ) %>% head(10)

            colnames(top_keywords)[1:2] <- c("Keyword", "Weight")
          }

          state$suggested_keywords <- top_keywords
          showNotification(paste(input$suggest_method, "suggestions ready!"), type = "message")

        }, error = function(e) {
          showNotification(paste("Suggestion Error:", e$message), type = "error")
        })
      })
    })

    # --- 3. Rendering Suggested Table (Cleaned: No Delete Column) ---
    output$suggested_keywords_table <- DT::renderDataTable({
      req(state$suggested_keywords)
      df <- state$suggested_keywords
      if (nrow(df) == 0) return(NULL)

      display_df <- data.frame(
        Keyword = df$Keyword,
        Weight = if(is.numeric(df$Weight)) round(df$Weight, 2) else df$Weight,
        Frequency = if(is.numeric(df$Frequency)) round(df$Frequency, 0) else df$Frequency,
        stringsAsFactors = FALSE
      )

      display_df$Add <- sapply(seq_len(nrow(display_df)), function(i) {
        paste0('<button class="btn btn-success btn-sm" onclick="addSuggestedIdx(', i, ')">Add</button>')
      })

      DT::datatable(display_df, rownames = FALSE, escape = FALSE,
                    options = list(dom = 'tp', pageLength = 10,
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    callback = DT::JS(paste0("
                      window.addSuggestedIdx = function(idx) {
                        Shiny.setInputValue('", ns("add_suggested_index"), "', idx, {priority: 'event'});
                      };
                    ")))
    })

    # --- 4. Handle Add from Suggestions ---
    observeEvent(input$add_suggested_index, {
      idx <- as.numeric(input$add_suggested_index)
      req(idx, state$suggested_keywords)
      word <- state$suggested_keywords$Keyword[idx]

      state$classifiers <- rbind(state$classifiers, data.frame(Keywords = word, stringsAsFactors = FALSE))
      state$suggested_keywords <- state$suggested_keywords[-idx, , drop = FALSE]
      showNotification(paste("Added:", word))
    })

    # --- 5. Main Active Keyword List ---
    output$classifier_list <- DT::renderDataTable({
      req(state$classifiers)
      df <- state$classifiers

      # Ensure there is training data to check against
      training_texts <- state$training_results$TextData

      if (nrow(df) == 0) {
        return(DT::datatable(data.frame(Keywords = character(), Frequency = numeric()), options = list(dom = 't')))
      }

      # --- NEW LOGIC: Calculate Frequency ---
      # For each keyword in the table, count hits in the training set
      df$Frequency <- sapply(df$Keywords, function(kw) {
        if (is.null(training_texts) || length(training_texts) == 0) return(0)
        # Use tryCatch in case the user enters an invalid regex string
        tryCatch({
          sum(grepl(kw, training_texts, ignore.case = TRUE, perl = TRUE))
        }, error = function(e) 0)
      })

      # Add Delete button
      df$Delete <- sapply(seq_len(nrow(df)), function(i) {
        paste0('<button class="btn btn-link" style="color: #dc3545; padding: 0;" onclick="removeActive(', i, ')">
                  <i class="fa fa-trash"></i></button>')
      })

      # Reorder columns to put Keywords and Frequency first
      df <- df[, c("Keywords", "Frequency", "Delete")]
      colnames(df)[1]="Regex"

      DT::datatable(df, rownames = FALSE, escape = FALSE,
                    options = list(dom = 'tp', pageLength = 10,
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    callback = DT::JS(paste0("window.removeActive = function(idx) {
                      Shiny.setInputValue('", ns("remove_active_index"), "', idx, {priority: 'event'}); };")))
    })

    observeEvent(input$remove_active_index, {
      idx <- as.numeric(input$remove_active_index)
      req(idx)
      state$classifiers <- state$classifiers[-idx, , drop = FALSE]
    })

    # --- 6. Helpers & Navigation ---
    output$has_suggestions <- reactive({ !is.null(state$suggested_keywords) && nrow(state$suggested_keywords) > 0 })
    outputOptions(output, "has_suggestions", suspendWhenHidden = FALSE)

    observe({
      is_ready <- !is.null(state$classifiers) && nrow(state$classifiers) > 0
      if (is_ready) shinyjs::enable("next_tab") else shinyjs::disable("next_tab")
    })

    observeEvent(input$prev_tab, { updateNavbarPage(parent_session, "tabs", selected = "project") })
    observeEvent(input$next_tab, { updateNavbarPage(parent_session, "tabs", selected = "training") })
  })
}
