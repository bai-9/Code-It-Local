mod_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, wellPanel(h4("Perfect Sampling"), p("Maintain consecutive agreement until Cai's N is reached."))),
      column(6, wellPanel(h4("Target Code:"), h3(textOutput(ns("code_name_display")))))
    ),
    fluidRow(
      column(6, DT::dataTableOutput(ns("cycle_metrics"))),
      column(6, DT::dataTableOutput(ns("overall_metrics")))
    ),
    fluidRow(
      column(8,
             wellPanel(
               h4("Validation Item"),
               div(style = "background: #f8f9fa; padding: 15px; min-height: 200px; border-left: 5px solid #007bff;",
                   uiOutput(ns("validation_text"))),
               br(),
               div(align = "center",
                   actionButton(ns("validate_yes"), "YES (Agrees)", class = "btn-success", width = "120px"),
                   actionButton(ns("validate_no"), "NO (Agrees)", class = "btn-danger", width = "120px")
               ),
               uiOutput(ns("completion_alert"))
             )
      ),
      column(4,
             wellPanel(
               h4("Controls"),
               actionButton(ns("restart_validation"), "Restart Cycle", class = "btn-warning btn-block"),
               hr(),
               # Button only appears when val_complete is TRUE in global state
               uiOutput(ns("finalize_ui")),
               downloadButton(ns("download_val"), "Download Val Log", class = "btn-secondary btn-block")
             )
      )
    ),
    hr(),
    fluidRow(
      column(6, actionButton(ns("prev_tab"), "← Back to Training", class = "btn-secondary")),
      column(6, div(align = "right",
                    actionButton(ns("next_tab"), "Go to Download →", class = "btn-primary")))
    )
  )
}

mod_validation_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Sync UI Elements ---
    output$code_name_display <- renderText({ state$current_code })

    # --- 2. Startup & Rehydration Logic ---
    observeEvent(parent_session$input$tabs, {
      req(parent_session$input$tabs == "validation")

      # If no pool exists, or if we have keywords but haven't started, initialize.
      if (is.null(state$val_pool)) {
        setup_validation_cycle()
      }
    }, ignoreInit = TRUE)

    setup_validation_cycle <- function() {
      req(state$dataset, state$text_column)

      # Extract Classifiers
      clf <- state$classifiers$Keywords
      valid_kw <- clf[clf != "" & !is.na(clf)]
      if (length(valid_kw) == 0) {
        showNotification("Add keywords before starting validation.", type = "warning")
        return()
      }

      pattern <- paste(valid_kw, collapse = "|")
      t_col <- state$text_column

      # Calculate Base Rate
      preds <- grepl(pattern, state$dataset[[t_col]], ignore.case = TRUE, perl = TRUE)
      obs_base_rate <- sum(preds) / nrow(state$dataset)
      prev_coded <- if(!is.null(state$training_results)) nrow(state$training_results) else 0

      # Perfect Sampling Logic
      cais_calc <- calculate_cais_n_logic(
        base_rate = obs_base_rate,
        previously_coded = prev_coded,
        data_size = nrow(state$dataset)
      )

      # Push to GLOBAL state for persistence
      state$val_cais_n <- cais_calc$n
      state$val_a_max <- cais_calc$a_max
      state$val_b1_adj <- cais_calc$b1
      state$val_results_cycle <- data.frame() # Reset cycle
      state$val_complete <- FALSE
      state$val_failed <- FALSE

      # Prepare Pool
      pool <- state$dataset
      pool$auto_coding <- ifelse(preds, 1, 0)
      pool$TextData <- state$dataset[[t_col]]
      state$val_pool <- pool

      load_next_val_item()
    }

    # --- 3. Item Navigation ---
    load_next_val_item <- function() {
      req(state$val_pool)

      # Exclude items already coded in this specific cycle
      coded_indices <- if(nrow(state$val_results_cycle) > 0) state$val_results_cycle$idx else NULL
      available_indices <- setdiff(1:nrow(state$val_pool), coded_indices)

      if (length(available_indices) == 0) {
        showNotification("Pool exhausted.", type = "warning")
        return()
      }

      selected_idx <- sample(available_indices, 1)
      # Save current item index to state so it persists on refresh
      state$val_current_idx <- selected_idx
    }

    output$validation_text <- renderUI({
      if (state$val_failed) {
        return(div(class = "alert alert-danger", h4("❌ Cycle Terminated"), p("Disagreement found. Refine keywords and restart.")))
      }
      if (state$val_complete) {
        return(div(class = "alert alert-success", h4("✅ Perfect Sampling Achieved!"), p("You have reached Cai's N.")))
      }

      req(state$val_current_idx, state$val_pool)
      text <- state$val_pool$TextData[state$val_current_idx]
      div(style = "font-size: 16px; line-height: 1.6;", text)
    })

    # --- 4. Processing Coding ---
    observeEvent(input$validate_yes, { process_coding(1) })
    observeEvent(input$validate_no, { process_coding(0) })

    process_coding <- function(user_val) {
      req(state$val_current_idx, !state$val_complete, !state$val_failed)

      item_auto <- state$val_pool$auto_coding[state$val_current_idx]
      item_text <- state$val_pool$TextData[state$val_current_idx]

      if (item_auto != user_val) {
        # FAILURE
        state$val_failed <- TRUE
        # Move bad item to training results for learning
        state$training_results <- rbind(state$training_results,
                                        data.frame(TextData = item_text, user.coding = user_val))
      } else {
        # SUCCESS
        state$val_results_cycle <- rbind(state$val_results_cycle, data.frame(idx = state$val_current_idx))

        if (nrow(state$val_results_cycle) >= state$val_cais_n) {
          state$val_complete <- TRUE
        } else {
          load_next_val_item()
        }
      }
    }

    # --- 5. Metrics Rendering ---
    output$cycle_metrics <- DT::renderDataTable({
      req(state$val_cais_n)
      count <- nrow(state$val_results_cycle)
      progress <- round((count / state$val_cais_n) * 100, 1)

      df <- data.frame(
        "Metric" = c("Progress", "Cai's N", "Consecutive Count", "Status"),
        "Value" = c(paste0(progress, "%"), state$val_cais_n, count,
                    ifelse(state$val_failed, "❌ FAILED", ifelse(state$val_complete, "✅ READY", "In Progress")))
      )
      DT::datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
    })

    output$overall_metrics <- DT::renderDataTable({
      df <- data.frame(
        "Parameter" = c("Base Rate (b1)", "Accuracy (a_max)", "Alpha", "Dataset Size"),
        "Value" = c(round(state$val_b1_adj, 4), round(state$val_a_max, 4), "0.025", nrow(state$dataset))
      )
      DT::datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
    })

    # --- 6. Finalization UI ---
    output$finalize_ui <- renderUI({
      if (state$val_complete) {
        actionButton(ns("code_entire_dataset"), "Finalize & Code Dataset", class = "btn-success btn-lg btn-block")
      }
    })

    observeEvent(input$code_entire_dataset, {
      showNotification("Applying validated classifier to full dataset...", type = "message")
      # Logical completion: the download tab is now unlocked
      state$val_finalized_timestamp <- Sys.time()
    })

    # --- 7. Downloads & Restarts ---
    output$download_val <- downloadHandler(
      filename = function() { paste0("validation_log_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(state$val_results_cycle, file, row.names = FALSE) }
    )

    observeEvent(input$restart_validation, { setup_validation_cycle() })

    # Navigation

    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "training")
    })

    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "download")
    })
  })
}
