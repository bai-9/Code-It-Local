# Path where the session state will be stored
get_save_path <- function() { "autosave.rds" }

# Function to save state
save_app_state <- function(state) {
  # Convert reactiveValues to a standard list for saving
  state_list <- reactiveValuesToList(state)
  saveRDS(state_list, get_save_path())
}

# Function to load state
load_app_state <- function() {
  path <- get_save_path()
  if (file.exists(path)) {
    return(readRDS(path))
  }
  return(NULL)
}
