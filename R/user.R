# R/user.R

#' Get or create a user-specific data directory
#' @param username The ID or name of the current user
#' @param base_path The root directory where user folders are stored
user_data_dir <- function(username, base_path) {
  # Validate that we have a username
  if (is.null(username) || username == "") return(NULL)

  # Create base directory if it doesn't exist
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE, showWarnings = FALSE)
  }

  # User-specific directory path
  user_dir <- file.path(base_path, username)

  # Create user directory if it doesn't exist
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
  }

  return(user_dir)
}


#' Save state to RDS files
save_persistence_data <- function(user_name, base_dir, tab_id,
                                  session_vals, main_vals,
                                  class_vals, coded_vals,
                                  train_vals, valid_vals) {
  if (is.null(user_name)) return(FALSE)

  user_dir <- user_data_dir(user_name, base_dir)

  tryCatch({
    # Helper to save if not null
    safe_save <- function(obj, filename) {
      if (!is.null(obj)) saveRDS(obj, file.path(user_dir, filename))
    }

    safe_save(session_vals$saved_dataset, "dataset.rds")
    safe_save(session_vals$saved_text_column, "text_column.rds")
    safe_save(main_vals$codebook_input, "codebook.rds")
    safe_save(class_vals$classifier_input, "classifiers.rds")
    safe_save(coded_vals$coded_data, "coded_data.rds")
    safe_save(tab_id, "current_tab.rds")

    saveRDS(list(
      training_results = train_vals$training_results,
      shown_indices = train_vals$shown_indices
    ), file.path(user_dir, "training_data.rds"))

    saveRDS(list(
      validation_pool = valid_vals$validation_pool,
      current_cycle_results = valid_vals$current_cycle_results,
      all_validation_results = valid_vals$all_validation_results,
      cais_n = valid_vals$cais_n,
      current_cycle = valid_vals$current_cycle,
      perfect_agreements_current_cycle = valid_vals$perfect_agreements_current_cycle,
      validation_complete = valid_vals$validation_complete,
      cycle_failed = valid_vals$cycle_failed,
      total_items_coded = valid_vals$total_items_coded,
      estimated_baserate = valid_vals$estimated_baserate,
      adjusted_baserate = valid_vals$adjusted_baserate,
      a_max = valid_vals$a_max
    ), file.path(user_dir, "validation_data.rds"))

    return(TRUE)
  }, error = function(e) {
    warning("Save error: ", e$message)
    return(FALSE)
  })
}

#' Load state from RDS files
load_persistence_data <- function(user_name, base_dir) {
  user_dir <- user_data_dir(user_name, base_dir)
  out <- list()

  files <- c("dataset", "text_column", "codebook", "classifiers",
             "coded_data", "training_data", "validation_data", "current_tab")

  for (f in files) {
    path <- file.path(user_dir, paste0(f, ".rds"))
    if (file.exists(path)) out[[f]] <- readRDS(path)
  }
  return(out)
}
