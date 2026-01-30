mod_training_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, wellPanel(
        h4("Training for: ", style = "display: inline;"),
        h4(textOutput(ns("training_code_name"), inline = TRUE),
           style = "color: #007bff; display: inline; font-weight: bold;")
      ))
    ),

    # Metrics and Confusion Matrix Row
    fluidRow(
      column(6, wellPanel(
        h4("Confusion Matrix"),
        DT::dataTableOutput(ns("confusion_matrix"))
      )),
      column(6, wellPanel(
        h4("Performance Metrics"),
        DT::dataTableOutput(ns("metrics_table"))
      ))
    ),

    # Sampling and Coding Row
    fluidRow(
      column(12, wellPanel(
        h4("Sample Text for Training"),
        div(style = "background: #f8f9fa; padding: 15px; border: 1px solid #ddd;
                    border-radius: 5px; min-height: 120px; margin-bottom: 15px;",
            uiOutput(ns("current_sample_text"))),

        div(align = "center",
            actionButton(ns("sample_pos"), "Sample Positive", class = "btn-info"),
            actionButton(ns("sample_neg"), "Sample Negative", class = "btn-info"),
            hr(),
            actionButton(ns("train_yes"), "YES (Matches)", class = "btn-success", width = "150px"),
            actionButton(ns("train_no"), "NO (Doesn't Match)", class = "btn-danger", width = "150px")
        )
      ))
    ),

    # Inside mod_classifier_ui
    fluidRow(
      column(6, actionButton(ns("prev_tab"), "← Back to Classifiers", class = "btn-secondary")),
      column(6, div(align = "right",
                    shinyjs::disabled(
                      actionButton(ns("next_tab"), "Go to Review →", class = "btn-primary")
                    )))
    )
  )
}

mod_training_server <- function(id, state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Sync Code Name
    output$training_code_name <- renderText({ state$current_code })

    # 2. Reactive Machine Prediction (Cached for efficiency)
    coded_data <- reactive({
      req(state$dataset, state$text_column)
      df <- state$dataset
      clf <- state$classifiers

      col_name <- if("Keywords" %in% names(clf)) "Keywords" else if("Keyword" %in% names(clf)) "Keyword" else NULL

      if (is.null(col_name) || nrow(clf) == 0) {
        df$predicted <- 0
      } else {
        raw_keywords <- clf[[col_name]]
        valid_keywords <- raw_keywords[!is.na(raw_keywords) & trimws(raw_keywords) != ""]

        if(length(valid_keywords) == 0) {
          df$predicted <- 0
        } else {
          pattern <- paste(valid_keywords, collapse = "|")
          matches <- grepl(pattern, df[[state$text_column]], ignore.case = TRUE, perl = TRUE)
          df$predicted <- ifelse(matches, 1, 0)
        }
      }
      return(df)
    })

    # 3. Sampling Logic (Updates Global state$current_train_idx for persistence)
    observeEvent(input$sample_pos, {
      data <- coded_data()
      pos_indices <- which(data$predicted == 1)

      if(length(pos_indices) > 0) {
        state$current_train_idx <- sample(pos_indices, 1)
      } else {
        showNotification("No positive matches found for your keywords.", type = "error")
      }
    })

    observeEvent(input$sample_neg, {
      data <- coded_data()
      neg_indices <- which(data$predicted == 0)

      if(length(neg_indices) > 0) {
        state$current_train_idx <- sample(neg_indices, 1)
      } else {
        showNotification("Machine thinks everything is a positive match.", type = "error")
      }
    })

    # 4. Recording Logic
    record_choice <- function(choice) {
      req(state$current_train_idx, state$dataset, state$text_column)

      new_entry <- data.frame(
        TextData = state$dataset[[state$text_column]][state$current_train_idx],
        user.coding = choice,
        stringsAsFactors = FALSE
      )

      # Append to the global results (persisted)
      state$training_results <- rbind(state$training_results, new_entry)

      # Clear the pointer
      state$current_train_idx <- NULL
      showNotification("Rating saved. Please sample a new item.", type = "message")
    }

    observeEvent(input$train_yes, { record_choice(1) })
    observeEvent(input$train_no,  { record_choice(0) })

    # 5. UI Output for Sample Text
    output$current_sample_text <- renderUI({
      # Check the PERSISTED index
      idx <- state$current_train_idx

      if (is.null(idx)) {
        return(tags$p(style = "color: #a0a0a0; text-align: center; margin-top: 30px;",
                      "Please click 'Sample Positive' or 'Sample Negative' to begin."))
      }

      text <- state$dataset[[state$text_column]][idx]
      tagList(
        tags$p(style = "font-size: 16px; line-height: 1.5; color: #2c3e50;", text)
      )
    })

    # 6. Confusion Matrix Rendering
    output$confusion_matrix <- DT::renderDataTable({
      req(nrow(state$training_results) >0)

      level_order <- c(1, 0)
      actual <- factor(state$training_results$user.coding, levels = level_order)

      # Re-evaluate prediction based on current classifier keywords
      clf <- state$classifiers
      col_name <- if("Keywords" %in% names(clf)) "Keywords" else if("Keyword" %in% names(clf)) "Keyword" else NULL

      predicted_vals <- if (is.null(col_name) || nrow(clf) == 0) {
        rep(0, length(actual))
      } else {
        valid_keywords <- clf[[col_name]][!is.na(clf[[col_name]]) & clf[[col_name]] != ""]
        pattern <- paste(valid_keywords, collapse = "|")
        if (pattern == "") rep(0, length(actual))
        else ifelse(grepl(pattern, state$training_results$TextData, ignore.case = TRUE, perl = TRUE), 1, 0)
      }

      predicted <- factor(predicted_vals, levels = level_order)
      tab_df <- as.data.frame.matrix(table(Actual = actual, Predicted = predicted))

      rownames(tab_df) <- c("Actual: YES", "Actual: NO")
      colnames(tab_df) <- c("Pred: YES", "Pred: NO")

      DT::datatable(tab_df, options = list(dom = 't', ordering = FALSE), selection = 'none')
    })

    # 7. Performance Metrics
    output$metrics_table <- DT::renderDataTable({ # CHANGED THIS FROM renderTable
      req(nrow(state$training_results) >= 5)

      actual <- state$training_results$user.coding
      clf <- state$classifiers
      col_name <- if("Keywords" %in% names(clf)) "Keywords" else if("Keyword" %in% names(clf)) "Keyword" else NULL
      if (is.null(col_name) || nrow(clf) == 0) return(NULL)

      valid_keywords <- clf[[col_name]][!is.na(clf[[col_name]]) & clf[[col_name]] != ""]
      pattern <- paste(valid_keywords, collapse = "|")

      # Prediction on training subset
      predicted <- if (pattern == "") rep(0, length(actual))
      else ifelse(grepl(pattern, state$training_results$TextData, ignore.case = TRUE, perl = TRUE), 1, 0)

      # Confusion Matrix Elements
      tp <- sum(actual == 1 & predicted == 1)
      tn <- sum(actual == 0 & predicted == 0)
      fp <- sum(actual == 0 & predicted == 1)
      fn <- sum(actual == 1 & predicted == 0)
      total <- length(actual)

      # Sample-based Metrics
      fdr <- if ((tp + fp) == 0) 0 else fp / (tp + fp)
      for_rate <- if ((tn + fn) == 0) 0 else fn / (tn + fn)
      po <- (tp + tn) / total
      pe <- (( (tp + fp)/total * (tp + fn)/total ) + ( (tn + fn)/total * (tn + fp)/total ))
      kappa <- if (pe == 1) 0 else (po - pe) / (1 - pe)

      # --- START NEW LOGIC: True Value Estimation ---
      N_total <- nrow(state$dataset)
      full_matches <- grepl(pattern, state$dataset[[state$text_column]], ignore.case = TRUE, perl = TRUE)
      b1 <- sum(full_matches) / N_total

      # Call ps_estimates
      est <- ps_estimates(tp, fp, fn, tn, b1, N_total)
      # --- END NEW LOGIC ---

      df = data.frame(
        Metric = c("False Discovery Rate", "False Omission Rate", "Cohen's Kappa"),
        `Sample Value` = c(
          paste0(round(fdr * 100, 2), "%"),
          paste0(round(for_rate * 100, 2), "%"),
          round(kappa, 3)
        ),
        `Estimated True Value` = c(
          paste0(round(est$FDR * 100, 2), "%"),
          paste0(round(est$FOR * 100, 2), "%"),
          round(est$kappa, 2)
        ),
        check.names = FALSE
      )

      # Use DT to return the formatted table
      DT::datatable(
        df,
        class = "cell-border",
        options = list(
          dom = "t",
          ordering = FALSE,
          rowCallback = JS(
            "function(row, data) {
         if (data[0] === \"Cohen's Kappa\") {
           $('td', row).css({
             'background-color': '#fff3cd',
             'font-weight': 'bold',
             'color': '#856404'
           });
         }
       }"
          ),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        selection = "none",
        rownames = FALSE
      )


    })

    # 8. Navigation
    # Inside mod_classifier_server
    observe({
      is_ready <- nrow(state$training_results) > 0
      # 1. Handle the "Next" button within the module
      if (is_ready) {
        shinyjs::enable("next_tab")
      } else {
        shinyjs::disable("next_tab")
      }

      # 2. Handle the Tab in the Top Navbar
      # We use shinyjs::runjs to target the CSS selector of the tab link
      if (is_ready) {
        shinyjs::runjs("$('#tabs li a[data-value=\"review\"]').removeClass('disabled').css('pointer-events', 'auto').css('opacity', '1');")
      } else {
        # We add a 'disabled' class and stop pointer events (clicks)
        shinyjs::runjs("$('#tabs li a[data-value=\"review\"]').addClass('disabled').css('pointer-events', 'none').css('opacity', '0.5');")
      }
    })
    observeEvent(input$next_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "review")
    })

    observeEvent(input$prev_tab, {
      updateNavbarPage(parent_session, "tabs", selected = "create_classifiers")
    })
  })
}
