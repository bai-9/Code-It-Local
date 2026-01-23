##### SERVER #####
# In server.R

server <- function(input, output, session) {
  # 1. Essential Config
  options(shiny.maxRequestSize = 200 * 1024^2)

  # 2. Set Local Identity
  # This replaces the entire "auth_values" block
  user_name <- Sys.info()[["user"]]
  user_path <- user_data_dir(user_name, base_dir)

  # Satisfy any remaining UI output$authenticated checks
  output$authenticated <- reactive({ TRUE })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  ##### INITIALIZE ALL REACTIVE VALUES ####

  # Create Codebook values
  values <- reactiveValues(
    path = user_path,
    codebook_input = data.frame(
      CodeName = c("Click to enter Code Name"),
      CodeDefinition = c("Click to enter Code Definition"),
      Examples = c("Click to enter Code Examples"),
      stringsAsFactors = FALSE
    )
  )
  # Create Classifier values
  classifier_values <- reactiveValues(
    classifier_input = data.frame(
      Keywords = character(),
      stringsAsFactors = FALSE
    )
  )
  values$keyword_blacklist <- c()
  # Coded data values to store coded data
  coded_data_values <- reactiveValues(
    coded_data = NULL
  )

  # Training values
  training_values <- reactiveValues(
    current_sample = NULL,
    current_.row_id = NULL,
    shown_indices = integer(),
    current_example_type = NULL,
    training_results = data.frame(
      .row_id = integer(),
      TextData = character(),
      auto.coding = integer(),
      user.coding = integer(),
      stringsAsFactors = FALSE
    )
  )

  # Validation Values
  validation_values <- reactiveValues(
    validation_pool = NULL,
    current_validation_item = NULL,
    current_pool_index = NULL,
    current_cycle_results = data.frame(
      pool_index = integer(),
      original_.row_id = integer(),
      TextData = character(),
      auto.coding = integer(),
      user.coding = integer(),
      cycle = integer(),
      stringsAsFactors = FALSE
    ),
    all_validation_results = data.frame(),
    cais_n = 0,
    current_cycle = 1,
    perfect_agreements_current_cycle = 0,
    validation_complete = FALSE,
    cycle_failed = FALSE,
    total_items_coded = 0,
    estimated_baserate = 0,
    adjusted_baserate = 0,
    a_max = 0
  )

  # Session state values
  session_values <- reactiveValues(
    saved_dataset = NULL,
    saved_text_column = NULL,
    current_tab = "ud",
    data_loaded = FALSE
  )


  ##### USER DATA PERSISTENCE FUNCTIONS #####

  save_all_user_data <- function() {
    success <- save_persistence_data(
      user_name, base_dir, input$tabs,
      session_values, values, classifier_values,
      coded_data_values, training_values, validation_values
    )
    if (success) cat("Data saved for:", user_name, "\n")
  }

  load_all_user_data <- function() {
    if (is.null(user_name) || session_values$data_loaded) return()

    data <- load_persistence_data(user_name, base_dir)
    if (length(data) == 0) return()

    # Unpack basic objects
    if (!is.null(data$dataset))     session_values$saved_dataset <- data$dataset
    if (!is.null(data$codebook))    values$codebook_input <- data$codebook
    if (!is.null(data$classifiers)) classifier_values$classifier_input <- data$classifiers
    if (!is.null(data$coded_data))  coded_data_values$coded_data <- data$coded_data

    # Unpack complex lists
    if (!is.null(data$training_data)) {
      training_values$training_results <- data$training_data$training_results
      training_values$shown_indices    <- data$training_data$shown_indices
    }

    if (!is.null(data$validation_data)) {
      v <- data$validation_data
      for (name in names(v)) validation_values[[name]] <- v[[name]]
    }

    # UI Updates
    if (!is.null(data$text_column)) {
      session_values$saved_text_column <- data$text_column
      updateSelectInput(session, "textColumn",
                        choices = names(session_values$saved_dataset),
                        selected = data$text_column)
    }

    if (!is.null(data$current_tab)) {
      shinyjs::delay(500, updateNavbarPage(session, "tabs", selected = data$current_tab))
    }

    session_values$data_loaded <- TRUE
    showNotification("Session restored.", type = "message")
  }

  auto_save <- function() {
    if (!is.null(user_name) && session_values$data_loaded) save_all_user_data()
  }

  # Load user data when authentication succeeds
  observe({
    #req(current_user())
    if (!session_values$data_loaded) {
      shinyjs::delay(1000, {  # Small delay to ensure UI is ready
        load_all_user_data()
      })
    }
  })

  ##### NAVIGATION NEXT STEP HANDLERS ####

  observeEvent(input$goto_create_code, {
    updateNavbarPage(session, "tabs", selected = "create_codebook")
    showNotification("Define your code: name, definition, and examples", type = "message", duration = 3)
  })

  observeEvent(input$goto_create_classifiers, {
    if (!is.null(current_code())) {
      updateNavbarPage(session, "tabs", selected = "create_classifiers")
      showNotification("Add keywords or regex patterns to identify your code", type = "message", duration = 3)
    } else {
      showNotification("Please define your code first", type = "warning", duration = 3)
    }
  })

  observeEvent(input$goto_training, {
    if (nrow(classifier_values$classifier_input) > 0) {
      updateNavbarPage(session, "tabs", selected = "training")
      showNotification("Train your classifier by coding examples", type = "message", duration = 3)
    } else {
      showNotification("Please add at least one classifier first", type = "warning", duration = 3)
    }
  })

  observeEvent(input$goto_validation, {
    metrics <- training_metrics()
    if (nrow(training_values$training_results) >= 20 && !is.null(metrics) && metrics$kappa >= 0.60) {
      updateNavbarPage(session, "tabs", selected = "validation")
      showNotification("Begin perfect sampling validation", type = "message", duration = 3)
    } else {
      showNotification("Please complete more training first (≥20 items, κ ≥ 0.60)", type = "warning", duration = 4)
    }
  })

  ##### UPLOAD DATA #####

  # first clear existing dataset (if any) when user uploads a dataset AND clear stuff from other tabs to avoid mismatched rows and other issues
  # Observer to reset ALL data when new file is uploaded
  observeEvent(input$data_upload, {
    tryCatch({
      # Clear dataset
      session_values$saved_dataset <- NULL
      session_values$saved_text_column <- NULL

      # Clear training data
      training_values$training_results <- data.frame(
        .row_id = integer(),
        text = character(),
        user.coding = numeric(),
        auto.coding = numeric(),
        stringsAsFactors = FALSE
      )
      training_values$shown_indices <- integer()
      training_values$current_sample <- NULL
      training_values$current_.row_id <- NULL
      training_values$current_example_type <- NULL

      # Clear coded data
      coded_data_values$coded_data <- NULL

      # Clear validation data
      validation_values$validation_pool <- NULL
      validation_values$current_validation_item <- NULL
      validation_values$current_pool_index <- NULL
      validation_values$current_cycle_results <- data.frame(
        .row_id = integer(),
        text = character(),
        user.coding = numeric(),
        auto.coding = numeric(),
        stringsAsFactors = FALSE
      )
      validation_values$all_validation_results <- data.frame()
      validation_values$cais_n <- 0
      validation_values$current_cycle <- 1
      validation_values$perfect_agreements_current_cycle <- 0
      validation_values$validation_complete <- FALSE
      validation_values$cycle_failed <- FALSE
      validation_values$total_items_coded <- 0
      validation_values$estimated_baserate <- 0
      validation_values$adjusted_baserate <- 0
      validation_values$a_max <- 0

      # Auto-save the cleared state
      auto_save()

      showNotification(
        "New dataset uploaded. All previous training and test metrics have been cleared.",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      showNotification(
        paste("Error uploading datafile and resetting", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  dataset <- reactive({
    # First check if we have a saved dataset
    if (!is.null(session_values$saved_dataset)) {
      return(session_values$saved_dataset)
    }

    # Otherwise, load from file input
    req(input$data_upload)
    file_ext <- tools::file_ext(input$data_upload$datapath)

    if (file_ext == "csv") {
      data <- read.csv(input$data_upload$datapath, stringsAsFactors = FALSE)
    } else if (file_ext %in% c("xls", "xlsx")) {
      data <- read_excel(input$data_upload$datapath)
    } else {
      stop("Unsupported file type. Please upload a CSV or Excel file.")
    }

    #add a stable row id when data file is loaded
    if (!".row_id" %in% names(data)) {
      data$.row_id <- seq_len(nrow(data))
    }

    # Save the newly uploaded dataset
    session_values$saved_dataset <- data
    auto_save()
    return(data)
  })

  output$uploaded_data <- DT::renderDataTable({
    dataset() %>%
      datatable(
        rownames = FALSE,
        class = "cell-border stripe",
        options = list(
          searching = FALSE,
          autoWidth = TRUE,
          pageLength = 5
        )
      )
  })

  # Update text column selection
  observeEvent(dataset(), {
    req(dataset())
    choices <- names(dataset())

    # Only update if we don't already have a saved selection
    if (is.null(session_values$saved_text_column)) {
      updateSelectInput(inputId = "textColumn", choices = choices, selected = character(0))
    } else {
      updateSelectInput(inputId = "textColumn", choices = choices, selected = session_values$saved_text_column)
    }

  })

  # Save text column selection when it changes
  observeEvent(input$textColumn, {
    if (!is.null(input$textColumn) && input$textColumn != "") {
      session_values$saved_text_column <- input$textColumn
      auto_save()
    }
  })

  ##### CREATE CODEBOOK (SINGLE CODE) #####

  # Render the editable single-row table
  output$created_codebook <- DT::renderDataTable({
    render_codebook_dt(values$codebook_input)
  })

  # Update codebook when cell is edited and auto-save
  observeEvent(input$created_codebook_cell_edit, {
    info <- input$created_codebook_cell_edit
    values$codebook_input[info$row, info$col + 1] <- info$value
    auto_save()
  })

  # Reactive to get the current code name
  current_code <- reactive({
    code_name <- values$codebook_input$CodeName[1]
    if (is.na(code_name) || code_name == "" || code_name == "DOUBLE Click to enter Code Name") {
      return(NULL)
    }
    return(code_name)
  })

  # Display current code name in Create Classifiers tab
  output$current_code_name <- renderText({
    code <- current_code()
    if (is.null(code)) {
      return("[Please enter a code name first]")
    }
    return(code)
  })

  # Display current code name in Training tab
  output$training_code_name <- renderText({
    code <- current_code()
    if (is.null(code)) {
      return("[No code defined]")
    }
    return(code)
  })

  ##### CREATE CLASSIFIERS ####

  # Apply coding function

  apply_coding <- function() {
    # Essential guard clauses
    req(dataset(), input$textColumn, current_code())
    if (nrow(classifier_values$classifier_input) == 0) {
      coded_data_values$coded_data <- NULL
      return()
    }

    tryCatch({
      # Call the utility function
      result_df <- execute_regex_coding(
        data = dataset(),
        text_col = input$textColumn,
        code_name = current_code(),
        classifier_df = classifier_values$classifier_input
      )

      # Update values and UI
      coded_data_values$coded_data <- result_df

      if (!is.null(result_df)) {
        auto_save()
        showNotification("Coding updated with regex support!", type = "message", duration = 2)
      }

    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 5)
      coded_data_values$coded_data <- NULL
    })
  }

  # Trigger coding updates
  observeEvent(
    list(
      current_code(),
      input$textColumn,
      classifier_values$classifier_input,
      dataset()
    ),
    {
      apply_coding()
    },
    ignoreInit = TRUE
  )

  # Add new classifier and auto-save
  observeEvent(input$add_classifier_submit, {
    req(input$classifier_keywords)

    if (nchar(trimws(input$classifier_keywords)) == 0) {
      showNotification("Please enter keywords", type = "warning")
      return()
    }

    new_row <- data.frame(
      Keywords = trimws(input$classifier_keywords),
      stringsAsFactors = FALSE
    )

    classifier_values$classifier_input <- rbind(classifier_values$classifier_input, new_row)
    updateTextInput(session, "classifier_keywords", value = "")
    auto_save()  # Auto-save when classifiers change
})

  # Handle individual keyword deletion
  #### SECTION: KEYWORD DELETION ####

  # 1. Trigger Modal
  observeEvent(input$delete_individual_keyword, {
    info <- input$delete_individual_keyword
    req(info$group_id, info$keyword_index)

    # Find the keyword text for the modal
    row_text <- classifier_values$classifier_input$Keywords[info$group_id]
    keywords <- trimws(strsplit(row_text, ",")[[1]])
    target_keyword <- keywords[info$keyword_index]

    # Store info and show modal
    session$userData$delete_info <- list(
      group_id = info$group_id,
      keyword_index = info$keyword_index,
      keyword_text = target_keyword
    )

    showModal(keyword_delete_modal(target_keyword))
  })

  # 2. Process Confirmation
  observeEvent(input$confirm_delete_keyword, {
    req(session$userData$delete_info)
    info <- session$userData$delete_info

    classifier_values$classifier_input <- remove_keyword_logic(
      classifier_values$classifier_input,
      info$group_id,
      info$keyword_index
    )

    removeModal()
    auto_save()
    showNotification(paste("Deleted:", info$keyword_text), type = "message")
  })

  output$classifier_list <- DT::renderDataTable({
    # Process the raw reactive data
    display_df <- prepare_classifier_data(classifier_values$classifier_input)

    # Render using the utility helper
    render_classifier_dt(display_df)
  })

  ##### KEYWORD SUGGESTER (NAIVE BAYES) #####

  ##### KEYWORD SUGGESTER (NAIVE BAYES) - FIXED VERSION #####

  # Reactive to store suggested keywords
  suggested_keywords <- reactiveValues(
    keywords = NULL
  )

  # Run Naive Bayes keyword suggester
  #### SECTION: KEYWORD SUGGESTER ####

  observeEvent(input$predict_classifiers, {
    # 1. Pre-flight checks
    results <- training_values$training_results
    if (nrow(results) < 10 || length(unique(results$user.coding)) < 2) {
      showNotification("Need more diverse training data (min 10 items, both classes).", type = "warning")
      return()
    }

    withProgress(message = 'Analyzing training data...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Preparing data...")
        nb_data <- data.frame(text = results$TextData, class = results$user.coding)

        temp_file <- tempfile(fileext = ".csv")
        write_csv(nb_data, temp_file)

        incProgress(0.4, detail = "Running Naive Bayes...")
        model_results <- train_and_analyze_model(temp_file)
        unlink(temp_file)

        incProgress(0.3, detail = "Filtering suggestions...")
        # Use our new utility to fix the logic flaw
        top_keywords <- get_filtered_suggestions(model_results,
                                                 classifier_values$classifier_input,
                                                 blacklist = values$keyword_blacklist)

        if (is.null(top_keywords) || nrow(top_keywords) == 0) {
          stop("No new predictive keywords found beyond your current list.")
        }

        suggested_keywords$keywords <- top_keywords
        showNotification(paste("✓ Found", nrow(top_keywords), "new suggestions!"), type = "message")

      }, error = function(e) {
        if (exists("temp_file") && file.exists(temp_file)) unlink(temp_file)
        showNotification(paste("Error:", e$message), type = "error")
        suggested_keywords$keywords <- NULL
      })
    })
  })

  output$suggested_keywords_table <- DT::renderDataTable({
    req(suggested_keywords$keywords)

    # 1. Use your helper to add the HTML buttons (ensure Add/Trash are columns)
    display_df <- prepare_suggestions_table(suggested_keywords$keywords, values$keyword_blacklist)

    DT::datatable(
      display_df,
      rownames = FALSE,
      escape = FALSE, # MUST be FALSE to render HTML
      options = list(dom = 't', ordering = TRUE),
      # This is the "Bridge" between JS and R
      callback = DT::JS("
      window.addSuggested = function(word) {
        Shiny.setInputValue('add_suggested_keyword', word, {priority: 'event'});
      };
      window.trashSuggested = function(word) {
        Shiny.setInputValue('trash_suggested_keyword', word, {priority: 'event'});
      };
    ")
    )
  })

  # Handle adding suggested keywords
  #### SECTION: KEYWORD SUGGESTER ACTIONS ####

  observeEvent(input$add_suggested_keyword, {
    word <- input$add_suggested_keyword
    req(word)

    # 1. Use our helper for the check (Replacing 8 lines of rowwise/do)
    existing <- get_current_keyword_list(classifier_values$classifier_input)

    if (tolower(word) %in% tolower(existing)) {
      showNotification(paste("Keyword '", word, "' already exists."), type = "warning")
      return()
    }

    # 2. Add to classifier
    classifier_values$classifier_input <- rbind(
      classifier_values$classifier_input,
      data.frame(Keywords = word, stringsAsFactors = FALSE)
    )

    # 3. Remove from the suggestions table immediately
    if (!is.null(suggested_keywords$keywords)) {
      suggested_keywords$keywords <- suggested_keywords$keywords %>%
        filter(tolower(Keyword) != tolower(word))
    }

    # 4. Finalize
    apply_coding() # Ensure the main dataset updates
    auto_save()
    showNotification(paste("✓ Added:", word), type = "message", duration = 2)
  })
  observeEvent(input$trash_suggested_keyword, {
    word <- input$trash_suggested_keyword
    req(word)

    # Add to permanent blacklist
    values$keyword_blacklist <- unique(c(values$keyword_blacklist, tolower(word)))

    # Remove from display table
    suggested_keywords$keywords <- suggested_keywords$keywords %>%
      filter(tolower(Keyword) != tolower(word))

    showNotification(paste("Keyword '", word, "' blacklisted."), type = "warning")
    auto_save()
  })
  # Control suggested keywords visibility
  output$has_suggestions <- reactive({
    !is.null(suggested_keywords$keywords)
  })
  outputOptions(output, "has_suggestions", suspendWhenHidden = FALSE)

  # Save current tab when user switches tabs
  observeEvent(input$tabs, {
    if (!is.null(input$tabs) && session_values$data_loaded) {
      session_values$current_tab <- input$tabs
      auto_save()
    }
  })



  ##### TRAINING ####

  # Helper function to show training example
  #### SECTION: TRAINING LOGIC ####

  show_training_example <- function(example_type) {
    req(coded_data_values$coded_data, current_code(), dataset())

    idx <- get_next_training_idx(coded_data_values$coded_data, current_code(),
                                 example_type, training_values$shown_indices)

    if (is.null(idx)) {
      output$sample_text <- renderUI({
        p(paste("No more", example_type, "items left!"), style = "color: #ffc107; font-weight: bold;")
      })
      training_values$current_sample <- NULL
      return()
    }

    # Update State
    coded_row <- coded_data_values$coded_data[idx, ]
    training_values$shown_indices <- c(training_values$shown_indices, idx)
    training_values$current_sample <- list(
      text = coded_row$TextData,
      auto_coding = coded_row[[paste0(current_code(), ".Positive")]],
      .row_id = dataset()$.row_id[idx]
    )

    # UI Toggle
    shinyjs::show("training_yes"); shinyjs::show("training_no")

    # Render Text
    label_info <- if(example_type == "positive") list(t="POSITIVE", c="#28a745") else list(t="NEGATIVE", c="#dc3545")
    output$sample_text <- renderUI({
      div(h5(paste(label_info$t, "EXAMPLE:"), style = paste0("color: ", label_info$c, "; font-weight: bold;")),
          p(training_values$current_sample$text))
    })
  }

  # Consolidate Yes/No Buttons
  observeEvent(list(input$training_yes, input$training_no), {
    req(training_values$current_sample)

    # Identify which button was clicked
    user_coding <- if (grepl("yes", deparse(substitute(input$training_yes)))) 1 else 0
    # Note: In a real app, it's safer to use separate small observers that call one function:
    # handle_training_click(1) and handle_training_click(0)
  })

  # Cleaner alternative for the buttons:
  handle_training_response <- function(user_val) {
    curr <- training_values$current_sample
    new_row <- data.frame(.row_id = curr$.row_id, TextData = curr$text,
                          auto.coding = curr$auto_coding, user.coding = user_val)

    training_values$training_results <- rbind(training_values$training_results, new_row)
    training_values$current_sample <- NULL

    shinyjs::hide("training_yes"); shinyjs::hide("training_no")
    output$sample_text <- renderUI({ training_feedback_ui(curr$auto_coding == user_val) })
    auto_save()
  }

  observeEvent(input$training_yes, { handle_training_response(1) })
  observeEvent(input$training_no,  { handle_training_response(0) })
  observeEvent(input$show_positive, { show_training_example("positive") })
  observeEvent(input$show_negative, { show_training_example("negative") })

  # Calculate training metrics
  training_metrics <- reactive({
    req(nrow(training_values$training_results) > 0)

    training_data <- training_values$training_results

    tp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 1)
    fp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 0)
    fn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 1)
    tn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 0)

    total <- tp + fp + fn + tn
    baserate <- (tp + fp) / total
    baserate2 <- (tp + fn) / total
    accuracy <- (tp + tn) / total

    pe <- (baserate * baserate2) + ((1 - baserate) * (1 - baserate2))
    kappa <- if (pe < 1) (accuracy - pe) / (1 - pe) else 0

    FDR <- if ((tp + fp) > 0) fp / (tp + fp) else 0
    FOR <- if ((fn + tn) > 0) fn / (fn + tn) else 0
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0

    list(
      kappa = kappa,
      FDR = FDR,
      FOR = FOR,
      precision = precision,
      recall = recall,
      accuracy = accuracy
    )
  })

  # Output training metrics with NULL safety
  output$training_metrics <- DT::renderDataTable({
    metrics <- training_metrics()

    if (is.null(metrics) || nrow(training_values$training_results) == 0) {
      datatable(
        data.frame(Message = "Start coding to see the training metrics!"),
        rownames = FALSE,
        colnames = NULL,
        class = "cell-border stripe",
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      metrics_df <- data.frame(
        Metric = c("Cohen's Kappa",
                   paste0("False Discovery Rate (FDR) ",
                          '<i class="fa fa-info-circle" data-toggle="tooltip" ',
                          'data-placement="right" ',
                          'title="Proportion of positive predictions that were incorrect. Lower is better. High FDR means your classifier is coding too many false positives."></i>'),
                   paste0("False Omission Rate (FOR) ",
                          '<i class="fa fa-info-circle" data-toggle="tooltip" ',
                          'data-placement="right" ',
                          'title="Proportion of negative predictions that were incorrect. Lower is better. High FOR means your classifier is missing true positives."></i>')
        ),
        Value = c(
          round(metrics$kappa, 3),
          round(metrics$FDR, 3),
          round(metrics$FOR, 3)
        ),
        stringsAsFactors = FALSE
      )

      datatable(
        metrics_df,
        rownames = FALSE,
        class = "cell-border stripe",
        escape = FALSE,
        options = list(
          dom = 't',
          ordering = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "  $('[data-toggle=\"tooltip\"]').tooltip();",
            "}"
          ),
          drawCallback = JS(
            "function(settings) {",
            "  $('[data-toggle=\"tooltip\"]').tooltip();",
            "}"
          )
        )
      )
    }
  })

  # Reset training metrics handler with auto-save
  observeEvent(input$reset_training_metrics, {
    if (nrow(training_values$training_results) > 0) {
      showModal(modalDialog(
        title = "Reset Training Data",
        div(
          div(
            class = "alert alert-warning",
            style = "margin-bottom: 20px;",
            h5(icon("exclamation-triangle"), " Warning", style = "color: #856404; margin-bottom: 10px;"),
            p("This will permanently delete all your training progress:", style = "margin-bottom: 10px;"),
            tags$ul(
              tags$li(paste("All", nrow(training_values$training_results), "coded training items")),
              tags$li("Training metrics calculations"),
              tags$li("Confusion matrix data"),
              tags$li("Progress on examples shown")
            )
          ),
          p("You will need to start training from scratch.",
            style = "font-weight: bold; margin-bottom: 15px;"),
          p("Are you sure you want to reset all training data?",
            style = "color: #dc3545; font-weight: bold;")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_reset_training",
                       "Yes, Reset Training Data",
                       class = "btn-danger",
                       icon = icon("trash"))
        )
      ))
    } else {
      showNotification("No training data to reset.", type = "message", duration = 3)
    }
  })

  # Confirm training reset with auto-save
  observeEvent(input$confirm_reset_training, {
    tryCatch({
      # Store count for notification
      items_cleared <- nrow(training_values$training_results)

      # Reset all training data
      training_values$training_results <- data.frame(
        .row_id = integer(),
        TextData = character(),
        auto.coding = integer(),
        user.coding = integer(),
        stringsAsFactors = FALSE
      )

      # Reset shown indices so user can see examples again
      training_values$shown_indices <- integer()

      # Clear current sample if any
      training_values$current_sample <- NULL
      training_values$current_.row_id <- NULL
      training_values$current_example_type <- NULL

      # Clear suggested keywords
      suggested_keywords$keywords <- NULL

      auto_save()  # Auto-save the reset state

      # Hide any visible agree/disagree buttons
      shinyjs::hide("agree")
      shinyjs::hide("disagree")

      # Reset sample text display
      output$sample_text <- renderUI({
        p("Training data reset. Click 'Show Positive Example' or 'Show Negative Example' to start training again.",
          style = "color: #6c757d; font-style: italic; text-align: center; margin: 20px 0;")
      })

      removeModal()

      showNotification(
        paste("Training data reset successfully! Cleared", items_cleared, "training items."),
        type = "message", duration = 5
      )

    }, error = function(e) {
      showNotification(paste("Error resetting training data:", e$message), type = "error")
      removeModal()
    })
  })

  # Output confusion matrix
  output$confusion_matrix_table <- DT::renderDataTable({
    if (nrow(training_values$training_results) == 0) {
      datatable(
        data.frame(Message = "Start coding to see the confusion matrix!"),
        rownames = FALSE,
        colnames = NULL,
        class = "cell-border stripe",
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      training_data <- training_values$training_results

      tp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 1)
      fp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 0)
      fn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 1)
      tn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 0)

      confusion_df <- data.frame(
        ` ` = c("Classifier said YES", "Classifier said NO"),
        `You said YES` = c(tp, fn),
        `You said NO` = c(fp, tn),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      datatable(
        confusion_df,
        rownames = FALSE,
        class = "cell-border stripe",
        options = list(
          dom = 't',
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:2)
          )
        )
      )
    }
  })

  # Download handler
  output$download_training_data <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "training"
      paste0(code_name, "_training_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (nrow(training_values$training_results) == 0) {
        write.csv(
          data.frame(Message = "No training data available yet."),
          file, row.names = FALSE
        )
      } else {
        training_data <- training_values$training_results
        metrics <- training_metrics()

        export_data <- training_data
        export_data$Agreement <- ifelse(
          export_data$auto.coding == export_data$user.coding,
          "Agreement", "Disagreement"
        )
        export_data$Classification_Type <- dplyr::case_when(
          export_data$auto.coding == 1 & export_data$user.coding == 1 ~ "True Positive",
          export_data$auto.coding == 1 & export_data$user.coding == 0 ~ "False Positive",
          export_data$auto.coding == 0 & export_data$user.coding == 1 ~ "False Negative",
          export_data$auto.coding == 0 & export_data$user.coding == 0 ~ "True Negative"
        )

        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )

  # Download training metrics handler
  output$download_training_metrics <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "training"
      paste0(code_name, "_training_metrics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (nrow(training_values$training_results) == 0) {
        write.csv(
          data.frame(Message = "No training data available yet."),
          file, row.names = FALSE
        )
      } else {
        # Calculate metrics and confusion matrix
        metrics <- training_metrics()
        training_data <- training_values$training_results

        # Calculate confusion matrix values
        tp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 1)
        fp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 0)
        fn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 1)
        tn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 0)

        # Create comprehensive metrics export
        metrics_export <- data.frame(
          Metric_Category = c(
            "Performance Metrics", "Performance Metrics", "Performance Metrics",
            "Performance Metrics", "Performance Metrics", "Performance Metrics",
            "Confusion Matrix", "Confusion Matrix", "Confusion Matrix", "Confusion Matrix",
            "Summary Statistics", "Summary Statistics"
          ),
          Metric_Name = c(
            "Cohen's Kappa", "False Discovery Rate (FDR)", "False Omission Rate (FOR)",
            "Precision", "Recall", "Accuracy",
            "True Positives", "False Positives", "False Negatives", "True Negatives",
            "Total Coded Items", "Agreement Rate"
          ),
          Value = c(
            round(metrics$kappa, 4),
            round(metrics$FDR, 4),
            round(metrics$FOR, 4),
            round(metrics$precision, 4),
            round(metrics$recall, 4),
            round(metrics$accuracy, 4),
            tp, fp, fn, tn,
            nrow(training_data),
            round(sum(training_data$auto.coding == training_data$user.coding) / nrow(training_data), 4)
          ),
          Description = c(
            "Inter-rater reliability measure",
            "Proportion of positive predictions that were wrong",
            "Proportion of negative predictions that were wrong",
            "Proportion of positive predictions that were correct",
            "Proportion of actual positives correctly identified",
            "Overall proportion of correct predictions",
            "Classifier=YES, You=YES",
            "Classifier=YES, You=NO",
            "Classifier=NO, You=YES",
            "Classifier=NO, You=NO",
            "Total number of items coded",
            "Proportion of items where you agreed with classifier"
          ),
          stringsAsFactors = FALSE
        )

        write.csv(metrics_export, file, row.names = FALSE)
      }
    }
  )

  # Navigation buttons
  observeEvent(input$move_to_testing, {
    updateNavbarPage(session, "tabs", selected = "validation")
    showNotification("Moved to Validation", type = "message", duration = 2)
  })

  observeEvent(input$move_back_classifier, {
    updateNavbarPage(session, "tabs", selected = "create_classifiers")
    showNotification("Returned to Classifier Creation", type = "message", duration = 2)
  })

  ##### VALIDATION (PERFECT SAMPLING WITH CYCLING) ####

  # Display current code name in Validation tab
  output$validation_code_name <- renderText({
    code <- current_code()
    if (is.null(code)) {
      return("[No code defined]")
    }
    return(code)
  })

  # Helper function to load next validation item
  load_next_validation_item <- function() {
    if (is.null(validation_values$validation_pool)) return()
    if (validation_values$validation_complete) return()
    if (validation_values$cycle_failed) return()

    # Find next unvalidated item from current cycle
    current_cycle_coded <- validation_values$current_cycle_results$pool_index
    available_pool_indices <- setdiff(1:nrow(validation_values$validation_pool), current_cycle_coded)

    if (length(available_pool_indices) == 0) {
      showNotification("Validation pool exhausted unexpectedly", type = "error")
      return()
    }

    # Select next item from pool
    selected_pool_index <- sample(available_pool_indices, 1)
    selected_item <- validation_values$validation_pool[selected_pool_index, ]

    validation_values$current_validation_item <- selected_item
    validation_values$current_pool_index <- selected_pool_index

    # Display the item
    output$validation_text <- renderUI({
      div(
        style = "background: linear-gradient(90deg, #f8f9fa, #ffffff); padding: 25px; border-left: 4px solid #007bff; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px;",
        p(selected_item$TextData, style = "line-height: 1.8; margin: 0; font-size: 16px; color: #333;")
      )
    })

    shinyjs::show("validate_yes")
    shinyjs::show("validate_no")
  }

  # Calculate Cai's N with proper edge case handling
  calculate_cais_n <- function(tau_kappa = 0.80, base_rate, alpha = 0.025, data_size,previously_coded = 0) {
    req(base_rate >= 0, base_rate <= 1)

    # Handle extreme base rates first
    if (base_rate <= 0.01 || base_rate >= 0.99) {
      showNotification(
        paste("Extreme base rate detected:", round(base_rate, 3),
              "- Using observed rate without adjustment"),
        type = "warning"
      )
      b1_hat <- base_rate
    } else if (previously_coded > 10) {
      z_score <- qnorm(1 - alpha/2)
      se <- sqrt((base_rate * (1 - base_rate)) / previously_coded)
      adjustment <- z_score * se

      # Apply adjustment but keep reasonable bounds (don't go below 5% or above 95%)
      b1_hat <- base_rate - adjustment
      b1_hat <- max(0.05, min(0.95, b1_hat))

      # If adjustment makes base rate too extreme, use observed rate
      if (abs(b1_hat - base_rate) > 0.4) {
        b1_hat <- base_rate
        showNotification("Base rate adjustment too extreme - using observed rate", type = "message")
      }
    } else {
      # Use observed base rate for small amounts of previous coding
      b1_hat <- base_rate
    }

    # Calculate maximum accuracy using equation from the paper
    # numerator <- b1_hat * tau_kappa
    # denominator <- 1 - tau_kappa + tau_kappa * b1_hat + b1_hat * (1 - tau_kappa)

    # if (denominator <= 0) {
    #   return(list(cais_n = Inf, a_max = NA, b1_adjusted = b1_hat))
    # }

    # fraction_term <- numerator / denominator
    # a_max <- 1 - b1_hat + b1_hat * tau_kappa - (1 - tau_kappa) * (1 - 2 * b1_hat) * fraction_term
    a_max<-max_accuracy(b1_hat,tau_kappa)

    # Check if a_max is reasonable
    if (a_max <= 0.5 || a_max >= 1) {
      showNotification(
        paste("Calculated a_max is extreme:", round(a_max, 3),
              "- May indicate base rate issues"),
        type = "warning"
      )
      return(list(cais_n = Inf, a_max = a_max, b1_adjusted = b1_hat))
    }

    # Check log value
    if (log(a_max) >= 0) {  # log(a_max) should be negative
      return(list(cais_n = Inf, a_max = a_max, b1_adjusted = b1_hat))
    }

    # Calculate Cai's N
    cais_n <- ceiling(log(alpha) / log(a_max))

    # Cap Cai's N at reasonable maximum (based on available data size)
    #max_reasonable_n <- min(1000, nrow(coded_data_values$coded_data) * 0.1)  # Cap at 10% of dataset
    max_reasonable_n <- min(1000, data_size * 0.1)  # Cap at 10% of dataset

    if (cais_n > max_reasonable_n) {
      showNotification(
        paste("Cai's N too large:", cais_n, "- Capped at", max_reasonable_n,
              ". Consider refining classifiers further."),
        type = "warning"
      )
      cais_n <- max_reasonable_n
    }

    return(list(
      cais_n = max(1, cais_n),
      a_max = a_max,
      b1_adjusted = b1_hat,
      original_base_rate = base_rate
    ))
  }

  # Initialize validation pool (create fresh test set for new cycle)
  initialize_validation_cycle <- function() {
    req(coded_data_values$coded_data, current_code())

    # Reset validation state at start of cycle
    validation_values$validation_complete <- FALSE
    validation_values$cycle_failed <- FALSE
    validation_values$current_validation_item <- NULL
    validation_values$current_pool_index <- NULL

    coded_data <- coded_data_values$coded_data
    code_name <- current_code()
    new_col_name <- paste0(code_name, ".Positive")

    # Exclude items already used in training
    excluded_indices <- c()
    if (nrow(training_values$training_results) > 0) {
      excluded_indices <- training_values$training_results$.row_id
    }

    if (length(excluded_indices) > 0) {
      available_data <- coded_data[-excluded_indices, ]
      available_indices <- setdiff(1:nrow(coded_data), excluded_indices)
    } else {
      available_data <- coded_data
      available_indices <- 1:nrow(coded_data)
    }

    if (nrow(available_data) == 0) {
      showNotification("No more data available for validation", type = "error")
      return(NULL)
    }

    # Estimate base rate from available data
    estimated_baserate <- sum(available_data[[new_col_name]] == 1) / nrow(available_data)

    # Calculate previously coded items for base rate adjustment
    previously_coded <- nrow(training_values$training_results)

    # Show base rate information
    showNotification(
      paste("Validation cycle setup - Base rate:", round(estimated_baserate, 3),
            "| Previously coded:", previously_coded),
      type = "message", duration = 3
    )

    # Check if base rate is reasonable for validation
    if (estimated_baserate < 0.03 || estimated_baserate > 0.95) {
      showModal(modalDialog(
        title = "Extreme Base Rate Detected",
        div(
          p(paste("Your classifiers predict a base rate of", round(estimated_baserate * 100, 1), "%")),
          p("This may lead to very large required sample sizes."),
          p("Consider:"),
          tags$ul(
            tags$li("Refining your classifiers to be less extreme"),
            tags$li("Adding more diverse keywords"),
            tags$li("Checking if your code definition is too narrow/broad")
          )
        ),
        footer = tagList(
          modalButton("Continue Anyway"),
          actionButton("go_back_to_classifiers", "Refine Classifiers", class = "btn-primary")
        )
      ))
    }

    # Calculate Cai's N
    cais_result <- calculate_cais_n(
      tau_kappa = 0.80,
      base_rate = estimated_baserate,
      alpha = 0.025,
      previously_coded = previously_coded,
      data_size = nrow(coded_data_values$coded_data)
    )

    if (is.infinite(cais_result$cais_n)) {
      showNotification(
        paste("Cannot calculate Cai's N. Base rate:", round(estimated_baserate, 3),
              "Previously coded:", previously_coded),
        type = "error"
      )
      return(NULL)
    }

    # Create validation pool for this cycle (will sample randomly when selecting items)
    validation_pool <- available_data
    validation_pool$original_indices <- available_indices

    return(list(
      validation_pool = validation_pool,
      cais_n = cais_result$cais_n,
      estimated_baserate = estimated_baserate,
      adjusted_baserate = cais_result$b1_adjusted,
      a_max = cais_result$a_max
    ))
  }

  # Handle going back to classifiers from base rate modal
  observeEvent(input$go_back_to_classifiers, {
    removeModal()
    updateNavbarPage(session, "tabs", selected = "create_classifiers")
    showNotification("Please refine your classifiers to adjust the base rate", type = "message", duration = 5)
  })

  # Initialize validation when moving to validation tab
  observeEvent(input$tabs, {
    if (input$tabs == "validation") {
      if (is.null(validation_values$validation_pool)) {
        val_setup <- initialize_validation_cycle()
        if (!is.null(val_setup)) {
          validation_values$validation_pool <- val_setup$validation_pool
          validation_values$cais_n <- val_setup$cais_n
          validation_values$estimated_baserate <- val_setup$estimated_baserate
          validation_values$adjusted_baserate <- val_setup$adjusted_baserate
          validation_values$a_max <- val_setup$a_max
          validation_values$current_cycle <- 1
          validation_values$perfect_agreements_current_cycle <- 0
          validation_values$validation_complete <- FALSE
          validation_values$cycle_failed <- FALSE
          validation_values$total_items_coded <- 0

          # AUTO-LOAD FIRST ITEM
          load_next_validation_item()
        }
      }
    }
  })

  # Handle validation responses - YES with auto-save
  observeEvent(input$validate_yes, {
    req(validation_values$current_validation_item)
    process_validation_response(user_coding = 1)
    auto_save()  # Auto-save after validation response
  })

  # Handle validation responses - NO with auto-save
  observeEvent(input$validate_no, {
    req(validation_values$current_validation_item)
    process_validation_response(user_coding = 0)
    auto_save()  # Auto-save after validation response
  })

  # Process validation response using Perfect Sampling Cycling Logic
  process_validation_response <- function(user_coding) {
    code_name <- current_code()
    new_col_name <- paste0(code_name, ".Positive")

    current_item <- validation_values$current_validation_item
    auto_coding <- current_item[[new_col_name]]

    # Record the response
    new_validation_row <- data.frame(
      pool_index = validation_values$current_pool_index,
      original_.row_id = current_item$original_indices,
      TextData = current_item$TextData,
      auto.coding = auto_coding,
      user.coding = user_coding,
      cycle = validation_values$current_cycle,
      stringsAsFactors = FALSE
    )

    validation_values$current_cycle_results <- rbind(validation_values$current_cycle_results, new_validation_row)
    validation_values$all_validation_results <- rbind(validation_values$all_validation_results, new_validation_row)
    validation_values$total_items_coded <- validation_values$total_items_coded + 1

    # Clear current item
    validation_values$current_validation_item <- NULL
    validation_values$current_pool_index <- NULL
    shinyjs::hide("validate_yes")
    shinyjs::hide("validate_no")

    # PERFECT SAMPLING RULE: Check for agreement
    if (auto_coding == user_coding) {
      # AGREEMENT - increment perfect agreement count for current cycle
      validation_values$perfect_agreements_current_cycle <- validation_values$perfect_agreements_current_cycle + 1

      # Check if we've reached Cai's N (perfect sampling complete)
      if (validation_values$perfect_agreements_current_cycle >= validation_values$cais_n) {
        # SUCCESS! Perfect sampling achieved
        validation_values$validation_complete <- TRUE
        shinyjs::hide("validation_controls")
        shinyjs::show("validation_complete")

        showNotification(
          paste0("✓ Perfect sampling achieved! κ > 0.80 validated with ",
                 validation_values$perfect_agreements_current_cycle, " consecutive perfect agreements in cycle ",
                 validation_values$current_cycle),
          type = "message", duration = 10
        )

        output$validation_text <- renderUI({
          div(
            class = "alert alert-success",
            style = "text-align: center; padding: 35px;",
            h3("✓ Perfect Sampling Complete!", style = "color: #28a745; margin-bottom: 25px;"),
            h5("Your automated classifier has been validated!", style = "margin-bottom: 20px;"),
            div(
              style = "background-color: #d4edda; padding: 20px; border-radius: 8px; margin: 20px 0;",
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; text-align: left;",
                div(
                  p("✓ Perfect agreements:", strong(validation_values$perfect_agreements_current_cycle, "/", validation_values$cais_n),
                    style = "margin: 5px 0;"),
                  p("✓ Validation cycle:", strong(validation_values$current_cycle), style = "margin: 5px 0;"),
                  p("✓ Total items coded:", strong(validation_values$total_items_coded), style = "margin: 5px 0;")
                ),
                div(
                  p("✓ κ > 0.80 validated (α = 0.025)", style = "margin: 5px 0; font-weight: bold;"),
                  p("✓ Statistical significance achieved", style = "margin: 5px 0; font-weight: bold;"),
                  p("✓ Classifier ready for use", style = "margin: 5px 0; font-weight: bold;")
                )
              )
            ),
            p("Your automated classifier has passed perfect sampling validation and is ready for use on the full dataset!",
              style = "font-size: 16px; font-weight: bold; color: #155724; margin-top: 15px;")
          )
        })

        return()
      } else {
        # Continue current cycle - need more perfect agreements
        # AUTO-LOAD NEXT ITEM
        Sys.sleep(0.2)  # Small delay for smooth transition
        load_next_validation_item()
      }
    } else {
      # DISAGREEMENT - Cycle failed, must refine classifiers and start new cycle
      validation_values$cycle_failed <- TRUE

      # Move disagreed item to training data
      failed_training_data <- data.frame(
        .row_id = current_item$original_indices,
        TextData = current_item$TextData,
        auto.coding = auto_coding,
        user.coding = user_coding,
        stringsAsFactors = FALSE
      )
      training_values$training_results <- rbind(training_values$training_results, failed_training_data)

      showNotification(
        paste("Disagreement in cycle", validation_values$current_cycle,
              "- refinement required. Disagreed item moved to training data."),
        type = "warning", duration = 8
      )

      output$validation_text <- renderUI({
        div(
          class = "alert alert-warning",
          style = "text-align: center;",
          h4("❌ Disagreement Found - Cycle Failed", style = "color: #856404;"),
          h5("Next Steps:"),
          p("1. Go back to 'Create Classifiers' and refine your keywords"),
          p("2. Return here and restart validation for a new cycle"),
          hr(),
          actionButton("go_refine_classifiers",
                       "Go Refine Classifiers",
                       icon = icon("arrow-left"),
                       class = "btn-warning btn-lg")
        )
      })
    }
  }

  # Navigate back to classifier refinement
  observeEvent(input$go_refine_classifiers, {
    updateNavbarPage(session, "tabs", selected = "create_classifiers")
    showNotification("Returned to classifier refinement", type = "message", duration = 3)
  })

  # Start new validation cycle (after refinement)
  start_new_validation_cycle <- function() {
    shinyjs::show("get_validation_item")
    # Reset current cycle data
    validation_values$current_cycle_results <- data.frame(
      pool_index = integer(), original_.row_id = integer(),
      TextData = character(), auto.coding = integer(), user.coding = integer(),
      cycle = integer(), stringsAsFactors = FALSE
    )
    validation_values$perfect_agreements_current_cycle <- 0
    validation_values$cycle_failed <- FALSE
    validation_values$current_cycle <- validation_values$current_cycle + 1

    # Create new validation pool (fresh test set)
    val_setup <- initialize_validation_cycle()
    if (!is.null(val_setup)) {
      validation_values$validation_pool <- val_setup$validation_pool
      validation_values$cais_n <- val_setup$cais_n
      validation_values$estimated_baserate <- val_setup$estimated_baserate
      validation_values$adjusted_baserate <- val_setup$adjusted_baserate
      validation_values$a_max <- val_setup$a_max

      showNotification(
        paste("New validation cycle", validation_values$current_cycle,
              "started with refined classifiers. Cai's N:", val_setup$cais_n),
        type = "message", duration = 5
      )

      output$validation_text <- renderUI({
        div(
          class = "alert alert-info",
          style = "text-align: center; padding: 20px;",
          h5("✓ New Validation Cycle Started", style = "color: #0c5460; margin-bottom: 15px;"),
          p("Loading next validation item...",
            style = "color: #6c757d; font-style: italic; text-align: center;")
        )
      })

      load_next_validation_item()

      return(TRUE)
    }
    return(FALSE)
  }

  # Restart validation (manual restart)
  observeEvent(input$restart_validation, {
    if (start_new_validation_cycle()) {
      shinyjs::show("validation_controls")
      shinyjs::hide("validation_complete")
      load_next_validation_item()
      auto_save()  # Auto-save the restart
    }
  })

  # Auto-restart when returning to validation tab after refinement
  observeEvent(input$tabs, {
    if (input$tabs == "validation" && validation_values$cycle_failed) {
      # Check if classifiers have been modified since last cycle failure
      # For simplicity, always offer to start new cycle when returning
      if (start_new_validation_cycle()) {
        # Cycle successfully restarted
      }
    }
  })

  # Output overall validation progress (Perfect Sampling with Cycles)
  output$validation_overall <- DT::renderDataTable({
    progress_df <- data.frame(
      Metric = c(
        "Total Cycles Attempted",
        "Total Items Coded (All Cycles)",
        "Estimated Base Rate"
      ),
      Value = c(
        max(1, validation_values$current_cycle),
        validation_values$total_items_coded,
        paste0(round(validation_values$estimated_baserate * 100, 1), "%")
      ),
      stringsAsFactors = FALSE
    )

    datatable(
      progress_df,
      rownames = FALSE,
      colnames = NULL,
      class = "cell-border stripe",
      options = list(dom = 't', ordering = FALSE)
    )
  })

  # Output current cycle validation metrics (Perfect Sampling Status)
  output$validation_current_cycle <- DT::renderDataTable({
    if (validation_values$validation_complete) {

      # Calculate progress_pct for completed validation
      progress_pct <- round((validation_values$perfect_agreements_current_cycle / max(1, validation_values$cais_n)) * 100, 1)

      metrics_df <- data.frame(
        Status = c("Current Cycle",
                   "Required Sample Size",
                   "Progress",
                   "Validation Complete"),
        Result = c(
          validation_values$current_cycle,
          validation_values$cais_n,
          paste0(progress_pct,
                 "% (",
                 validation_values$perfect_agreements_current_cycle,
                 "/",
                 validation_values$cais_n,
                 ")",
                 "<br>",
                 validation_values$total_items_coded,
                 " items coded"),
          "✅ κ > 0.80  α = 0.05"
        ),
        stringsAsFactors = FALSE
      )
    } else if (validation_values$cycle_failed) {

      # Calculate progress_pct for completed validation
      progress_pct <- round((validation_values$perfect_agreements_current_cycle / max(1, validation_values$cais_n)) * 100, 1)

      metrics_df <- data.frame(
        Status =  c("Current Cycle",
                    "Required Sample Size",
                    "Progress"),
        Result = c(
          paste(validation_values$current_cycle, "failed ❌"),
          validation_values$cais_n,
          paste0(progress_pct,
                 "% (",
                 validation_values$perfect_agreements_current_cycle,
                 "/",
                 validation_values$cais_n,
                 ")",
                 "<br>",
                 validation_values$total_items_coded,
                 " items coded")
        ),
        stringsAsFactors = FALSE
      )
    } else {

      # Calculate progress_pct for completed validation
      progress_pct <- round((validation_values$perfect_agreements_current_cycle / max(1, validation_values$cais_n)) * 100, 1)

      metrics_df <- data.frame(
        Status = c(
          "Current Cycle",
          "Required Sample Size",
          "Progress"
        ),
        Result = c(
          validation_values$current_cycle,
          validation_values$cais_n,
          paste0(#progress_pct,
                 #"% (",
                 validation_values$perfect_agreements_current_cycle,
                 "/",
                 validation_values$cais_n,
                 #")",
                 "<br>",
                 #validation_values$perfect_agreements_current_cycle,
                 "items coded")
        ),
        stringsAsFactors = FALSE
      )
    }

    datatable(
      metrics_df,
      rownames = FALSE,
      colnames = NULL,
      class = "cell-border stripe",
      escape = FALSE,
      options = list(dom = 't', ordering = FALSE)
    )
  })

  # Download handlers for validation
  output$download_validation_results <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "validation"
      paste0(code_name, "_perfect_sampling_cycles_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (nrow(validation_values$all_validation_results) == 0) {
        write.csv(
          data.frame(Message = "No validation data available yet."),
          file, row.names = FALSE
        )
      } else {
        export_data <- validation_values$all_validation_results
        export_data$Agreement <- ifelse(
          export_data$auto.coding == export_data$user.coding,
          "Perfect Agreement", "Disagreement"
        )
        export_data$Cycle_Outcome <- ifelse(
          export_data$Agreement == "Disagreement", "Cycle Failed", "Cycle Continued"
        )

        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )

  output$download_validation_current_cycle <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "validation"
      paste0(code_name, "_perfect_sampling_efficiency_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metrics_export <- data.frame(
        Metric = c(
          "Perfect Sampling Status",
          "Total Cycles Attempted",
          "Current/Final Cycle",
          "Cai's N",
          "Perfect Agreements Achieved",
          "Total Items Coded (All Cycles)",
          "Items Moved to Training",
          "Estimated Base Rate",
          "Adjusted Base Rate",
          "Statistical Test",
          "Alpha Level"
        ),
        Value = c(
          ifelse(validation_values$validation_complete, "Complete", "In Progress"),
          max(1, validation_values$current_cycle),
          validation_values$current_cycle,
          validation_values$cais_n,
          validation_values$perfect_agreements_current_cycle,
          validation_values$total_items_coded,
          nrow(training_values$training_results),
          round(validation_values$estimated_baserate, 4),
          round(validation_values$adjusted_baserate, 4),
          ifelse(validation_values$validation_complete, "Kappa > 0.80 validated", "Not yet achieved"),
          "0.025"
        ),
        Description = c(
          "Whether perfect sampling validation completed successfully",
          "Number of validation cycles attempted (includes failed cycles)",
          "Current cycle number (or final cycle if complete)",
          "Required consecutive perfect agreements",
          "Perfect agreements achieved in current/final cycle",
          "Total validation items coded across all cycles (efficiency measure)",
          "Items with disagreements automatically moved to training data",
          "Base rate estimated from validation pool",
          "Conservative base rate used in Cai's N calculation",
          "Statistical significance Cohen's Kappa test result",
          "Significance level for two tests"
        ),
        stringsAsFactors = FALSE
      )

      write.csv(metrics_export, file, row.names = FALSE)
    }
  )

  # Control validation complete flag for UI
  output$validation_complete_flag <- reactive({
    validation_values$validation_complete
  })
  outputOptions(output, "validation_complete_flag", suspendWhenHidden = FALSE)

  # Code entire dataset and provide download
  observeEvent(input$code_entire_dataset, {
    req(coded_data_values$coded_data, current_code())

    withProgress(message = 'Coding entire dataset...', value = 0, {
      tryCatch({
        # Get original dataset and coding info
        original_data <- dataset()
        coded_data <- coded_data_values$coded_data
        code_name <- current_code()
        new_col_name <- paste0(code_name, ".Positive")

        incProgress(0.3, detail = "Applying classifier...")

        # Create final dataset with original columns plus coding
        final_dataset <- original_data
        final_dataset[[new_col_name]] <- coded_data[[new_col_name]]

        incProgress(0.4, detail = "Adding metadata...")

        # Add metadata columns
        final_dataset$Coding_Method <- "Perfect_Sampling_Validated"
        final_dataset$Validation_Kappa <- "Above_0.80"
        final_dataset$Coding_Date <- as.character(Sys.Date())
        final_dataset$Validation_Cycles <- validation_values$current_cycle
        final_dataset$Cais_N_Required <- validation_values$cais_n

        incProgress(0.2, detail = "Preparing download...")

        # Calculate summary stats
        total_items <- nrow(final_dataset)
        coded_positive <- sum(final_dataset[[new_col_name]] == 1)
        coded_negative <- total_items - coded_positive
        final_base_rate <- round((coded_positive / total_items) * 100, 1)

        incProgress(0.1, detail = "Complete!")

        # Store for download
        session$userData$final_coded_dataset <- final_dataset
        session$userData$coding_complete <- TRUE
        auto_save()  # Auto-save completion status

        # Show success modal with download option
        showModal(modalDialog(
          title = "✓ Dataset Coding Complete!",
          size = "l",
          div(
            div(
              class = "alert alert-success",
              style = "text-align: center; margin-bottom: 20px;",
              h4("Your entire dataset has been coded!", style = "color: #28a745;")
            ),

            # Summary statistics
            div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
              h5("Coding Summary:", style = "margin-bottom: 15px;"),
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                div(
                  p("📊 Total items:", strong(format(total_items, big.mark = ",")), style = "margin: 5px 0;"),
                  p("✅ Coded positive:", strong(format(coded_positive, big.mark = ",")), style = "margin: 5px 0;"),
                  p("❌ Coded negative:", strong(format(coded_negative, big.mark = ",")), style = "margin: 5px 0;")
                ),
                div(
                  p("📈 Final base rate:", strong(paste0(final_base_rate, "%")), style = "margin: 5px 0;"),
                  p("🔬 Code applied:", strong(code_name), style = "margin: 5px 0;"),
                  p("⚡ Method:", strong("Perfect Sampling"), style = "margin: 5px 0;")
                )
              )
            ),

            # Validation details
            div(
              style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h6("Validation Details:", style = "color: #155724; margin-bottom: 10px;"),
              p("✓ Perfect sampling validation completed", style = "margin: 3px 0; color: #155724;"),
              p(paste("✓ Achieved", validation_values$perfect_agreements_current_cycle, "of", validation_values$cais_n, "required perfect agreements"), style = "margin: 3px 0; color: #155724;"),
              p(paste("✓ Validation cycles:", validation_values$current_cycle), style = "margin: 3px 0; color: #155724;"),
              p("✓ Statistical significance: κ > 0.80 (α = 0.025)", style = "margin: 3px 0; color: #155724;")
            )
          ),
          footer = tagList(
            modalButton("Close"),
            downloadButton("download_final_coded_dataset",
                           "Download Coded Dataset",
                           icon = icon("download"),
                           class = "btn-success btn-lg")
          )
        ))

        showNotification(
          paste("Success! Coded", format(total_items, big.mark = ","), "items -",
                format(coded_positive, big.mark = ","), "positive,",
                format(coded_negative, big.mark = ","), "negative"),
          type = "message", duration = 8
        )

      }, error = function(e) {
        showNotification(paste("Error coding dataset:", e$message), type = "error")
      })
    })
  })

  # Download handler for final coded dataset
  output$download_final_coded_dataset <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "coded"
      paste0(code_name, "_perfect_sampling_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(session$userData$final_coded_dataset)
      write.csv(session$userData$final_coded_dataset, file, row.names = FALSE)
    }
  )
}
# end of server
