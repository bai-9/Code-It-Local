library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(DT)
library(shinythemes)
library(bsicons)
library(shinyjs)
library(uuid) # Optional: run install.packages("uuid") for unique IDs
library(fastTextR)
library(stringr)

base_dir <- "./tmp/user_data"

# --- Project Registry Helpers ---

# Get the path to the registry
get_registry_path <- function() {
  dir.create("./tmp/user_data/local_user", recursive = TRUE, showWarnings = FALSE)
  file.path("./tmp/user_data/local_user", "projects.rds")
}

# Load the project list
load_project_registry <- function() {
  path <- get_registry_path()
  if (file.exists(path)) {
    return(readRDS(path))
  } else {
    # Initialize an empty registry
    return(data.frame(
      project_id = character(),
      data_file = character(),
      text_column = character(),
      code_name = character(),
      state_rds = character(),
      last_modified = POSIXct(),
      stringsAsFactors = FALSE
    ))
  }
}

# Save or Update a Project in the Registry
save_project_to_registry <- function(data_file, text_column, code_name, state_rds_name = NULL) {
  registry <- load_project_registry()

  # Check if this specific combination already exists
  existing_idx <- which(registry$data_file == data_file &
                          registry$text_column == text_column &
                          registry$code_name == code_name)

  if (length(existing_idx) > 0) {
    # Update existing entry
    registry$last_modified[existing_idx] <- Sys.time()
    res <- registry$state_rds[existing_idx]
  } else {
    # Create new entry
    new_id <- if(is.null(state_rds_name)) paste0("state_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds") else state_rds_name
    new_row <- data.frame(
      project_id = as.character(nrow(registry) + 1),
      data_file = data_file,
      text_column = text_column,
      code_name = code_name,
      state_rds = new_id,
      last_modified = Sys.time(),
      stringsAsFactors = FALSE
    )

    registry <- rbind(registry, new_row)
    res <- new_id
  }

  saveRDS(registry, get_registry_path())
  return(res) # Return the filename of the state rds
}
