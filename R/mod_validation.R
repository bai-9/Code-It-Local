mod_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        6,
        wellPanel(
          h4("Perfect Sampling"),
          p("Maintain consecutive agreement until Cai's N is reached.")
        )
      ),
      column(
        6,
        wellPanel(
          h4("Cohen's Kappa Threshold:"),
          selectInput(
            ns("kappa_threshold"),
            label = NULL,
            choices = c(0.65, 0.70, 0.80, 0.90),
            selected = 0.80,
            width = "100%"
          )
        )
      )
    ),
    fluidRow(
      column(6, DT::dataTableOutput(ns("cycle_metrics"))),
      column(6, DT::dataTableOutput(ns("overall_metrics")))
    ),
    fluidRow(
      column(
        8,
        wellPanel(
          h4("Validation Item"),
          div(
            style = "background: #f8f9fa; padding: 15px; min-height: 200px; border-left: 5px solid #007bff;",
            uiOutput(ns("validation_text"))
          ),
          br(),
          div(
            align = "center",
            actionButton(ns("validate_yes"), "YES (Agrees)", class = "btn-success", width = "120px"),
            actionButton(ns("validate_no"), "NO (Agrees)", class = "btn-danger", width = "120px")
          ),
          uiOutput(ns("completion_alert"))
        )
      ),
      column(
        4,
        wellPanel(
          h4("Controls"),
          actionButton(ns("restart_validation"), "Restart Cycle", class = "btn-warning btn-block"),
          hr(),
          uiOutput(ns("finalize_ui")),
          downloadButton(ns("download_val"), "Download Val Log", class = "btn-secondary btn-block")
        )
      )
    ),
    hr(),
    fluidRow(
      column(6, actionButton(ns("prev_tab"), "← Back to Training", class = "btn-secondary")),
      column(
        6,
        div(
          align = "right",
          actionButton(ns("next_tab"), "Go to Download →", class = "btn-primary")
        )
      )
    )
  )
}

mod_validation_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Store kappa threshold in global state ---
    observe({
      req(input$kappa_threshold)
      state$val_kappa_threshold <- as.numeric(input$kappa_threshold)
    })

    # --- 2. Startup & Rehydration Logic ---
    observeEvent(parent_session$input$tabs, {
      req(parent_session$input$tabs == "validation")

      if (is.null(state$val_pool)) {
        setup_validation_cycle()
      }
    }, ignoreInit = TRUE)

    # Re-run validation when kappa threshold changes
    observeEvent(input$kappa_threshold, {
      setup_validation_cycle()
    }, ignoreInit = TRUE)

    setup_validation_cycle <- function() {
      req(state$dataset, state$text_column, state$val_kappa_threshold)

      clf <- state$classifiers$Keywords
      valid_kw <- clf[clf != "" & !is.na(clf)]
      if (length(valid_kw) == 0) {
        showNotification("Add keywords before starting validation.", type = "warning")
        return()
      }

      pattern <- paste(valid_kw, collapse = "|")
      t_col <- state$text_column

      preds <- grepl(pattern, state$dataset[[t_col]], ignore.case = TRUE, perl = TRUE)
      obs_base_rate <- sum(preds) / nrow(state$dataset)
      prev_coded <- if (!is.null(state$training_results)) nrow(state$training_results) else 0

      # 🔴 THIS IS THE ONLY LINE YOU MAY NEED TO ADJUST
      cais_calc <- calculate_cais_n_logic(
        base_rate = obs_base_rate,
        previously_coded = prev_coded,
        data_size = nrow(state$dataset),
        kappa_threshold = state$val_kappa_threshold
      )

      state$val_cais_n <- cais_calc$n
      state$val_a_max <- cais_calc$a_max
      state$val_b1_adj <- cais_calc$b1
      state$val_results_cycle <- data.frame()
      state$val_complete <- FALSE
      state$val_failed <- FALSE

      pool <- state$dataset
      pool$auto_coding <- ifelse(preds, 1, 0)
      pool$TextData <- state$dataset[[t_col]]
      state$val_pool <- pool

      load_next_val_item()
    }

    # --- 3. Item Navigation ---
    load_next_val_item <- function() {
      coded_indices <- if (nrow(state$val_results_cycle) > 0)
        state$val_results_cycle$idx else NULL

      available_indices <- setdiff(seq_len(nrow(state$val_pool)), coded_indices)

      if (length(available_indices) == 0) {
        showNotification("Pool exhausted.", type = "warning")
        return()
      }

      state$val_current_idx <- sample(available_indices, 1)
    }

    output$validation_text <- renderUI({
      if (state$val_failed) {
        return(div(class = "alert alert-danger",
                   h4("❌ Cycle Terminated"),
                   p("Disagreement found. Refine keywords and restart.")))
      }
      if (state$val_complete) {
        return(div(class = "alert alert-success",
                   h4("✅ Perfect Sampling Achieved!"),
                   p("You have reached Cai's N.")))
      }

      req(state$val_current_idx)
      div(style = "font-size: 16px; line-height: 1.6;",
          state$val_pool$TextData[state$val_current_idx])
    })

    observeEvent(input$validate_yes, { process_coding(1) })
    observeEvent(input$validate_no, { process_coding(0) })

    process_coding <- function(user_val) {
      req(state$val_current_idx, !state$val_complete, !state$val_failed)

      item_auto <- state$val_pool$auto_coding[state$val_current_idx]
      item_text <- state$val_pool$TextData[state$val_current_idx]

      if (item_auto != user_val) {
        state$val_failed <- TRUE
        state$training_results <- rbind(
          state$training_results,
          data.frame(TextData = item_text, user.coding = user_val)
        )
      } else {
        state$val_results_cycle <- rbind(
          state$val_results_cycle,
          data.frame(idx = state$val_current_idx)
        )

        if (nrow(state$val_results_cycle) >= state$val_cais_n) {
          state$val_complete <- TRUE
        } else {
          load_next_val_item()
        }
      }
    }

    output$cycle_metrics <- DT::renderDataTable({
      req(state$val_cais_n)
      count <- nrow(state$val_results_cycle)
      progress <- round((count / state$val_cais_n) * 100, 1)

      DT::datatable(
        data.frame(
          Metric = c("Progress", "Cai's N", "Consecutive Count", "Status"),
          Value = c(
            paste0(progress, "%"),
            ceiling(state$val_cais_n),
            count,
            ifelse(state$val_failed, "❌ FAILED",
                   ifelse(state$val_complete, "✅ READY", "In Progress"))
          )
        ),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      )
    })

    output$overall_metrics <- DT::renderDataTable({
      DT::datatable(
        data.frame(
          Parameter = c("Base Rate (b1)", "Accuracy (a_max)", "Alpha", "Dataset Size"),
          Value = c(
            round(state$val_b1_adj, 2),
            round(state$val_a_max, 2),
            0.05,
            nrow(state$dataset)
          )
        ),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      )
    })

    output$finalize_ui <- renderUI({
      if (state$val_complete) {
        actionButton(ns("code_entire_dataset"),
                     "Finalize & Code Dataset",
                     class = "btn-success btn-lg btn-block")
      }
    })

    observeEvent(input$code_entire_dataset, {
      state$val_finalized_timestamp <- Sys.time()
      showNotification("Applying validated classifier to full dataset...", type = "message")
    })

    output$download_val <- downloadHandler(
      filename = function() paste0("validation_log_", Sys.Date(), ".csv"),
      content = function(file)
        write.csv(state$val_results_cycle, file, row.names = FALSE)
    )

    observeEvent(input$restart_validation, setup_validation_cycle)

    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "training")
    })

    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "download")
    })
  })
}
