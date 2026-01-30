server <- function(input, output, session) {
  options(shiny.maxRequestSize = 200 * 1024^2) # Sets limit to 200MB
  # --- 1. Initialization ---
  # We start with an empty state. Data only populates when a project is
  # loaded/created in the Project tab.
  #state <- reactiveValues(
  get_default_state <- function() {
    list(
    # Project Identifiers
    current_state_file = NULL,     # Pointer to the specific .rds file
    file_name = NULL,              # Original data filename
    text_column = NULL,            # Column selected for text
    current_code = "New Code",     # Codebook label
    data_ready = FALSE,
    # Data Objects
    dataset = NULL,
    classifiers = data.frame(Keywords = character(),
                             stringsAsFactors = FALSE),
    panelty = data.frame(Word = character(),
                         value=numeric(),
                         stringsAsFactors = FALSE),
    suggested_keywords = data.frame(Keyword=character(),
                                    Weight = numeric(),
                                    Frequency = integer(),
                                    stringsAsFactors = FALSE),
    keyword_blacklist = c(),
    keyword_suggested = c(),

    # Training (Naive Bayes)
    training_results = data.frame(
      TextData = character(),
      user.coding = numeric(),
      stringsAsFactors = FALSE
    ),

    # Validation (Perfect Sampling)
    val_pool = NULL,
    val_results_cycle = data.frame(),
    val_cais_n = 0,
    val_a_max = 0,
    val_b1_adj = 0,
    val_current_idx = NULL,
    val_complete = FALSE,
    val_failed = FALSE,

    # Final Output
    val_finalized_timestamp = NULL
  )
  }

  state <- do.call(reactiveValues, get_default_state())

  # The Reset Function
  reset_state <- function(type = c("full", "partial")) {
    type <- match.arg(type)
    defaults <- get_default_state()

    if (type == "full") {
      for (name in names(defaults)) state[[name]] <- defaults[[name]]
    } else {
      keep <- c("current_state_file", "file_name", "text_column", "current_code", "data_ready", "dataset")
      for (name in setdiff(names(defaults), keep)) state[[name]] <- defaults[[name]]
    }
  }
  # --- 2. The Project-Specific Auto-Save Trigger ---
  # This observer only fires when 'state' changes AND a project is active.
  observe({
    # Ensure we have a file to save to
    req(state$current_state_file)
    current_id = state$current_state_file
    # Convert reactiveValues to a standard list
    state_list <- reactiveValuesToList(state)

    # Save to the specific path defined in global.R
    save_path <- file.path("./tmp/user_data/local_user", state$current_state_file)
    saveRDS(state_list, save_path)
    cat("\n state saved!")

    # Optional: Log to console for debugging
    # print(paste("Autosaved project to:", state$current_state_file))
  })

  # --- 3. Module Servers ---
  # Note: I've updated the first module name to 'project_1' to reflect
  # your new merged Project tab.

  mod_project_server("project_1", state, reset_state_callback = reset_state, parent_session = session)
  # mod_project_server("project_1", state, parent_session = session)
  mod_classifier_server("classifier_1", state, parent_session = session)
  mod_training_server("training_1", state, parent_session = session)
  mod_review_server("review_1", state, parent_session = session)
  mod_validation_server("validation_1", state, parent_session = session)
  mod_download_server("download_1", state, parent_session = session)

}
