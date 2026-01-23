#' Select a random index for training
get_next_training_idx <- function(coded_data, code_name, example_type, shown_indices) {
  new_col_name <- paste0(code_name, ".Positive")
  target_val <- if (example_type == "positive") 1 else 0

  target_indices <- which(coded_data[[new_col_name]] == target_val)
  available_indices <- setdiff(target_indices, shown_indices)

  if (length(available_indices) == 0) return(NULL)
  return(sample(available_indices, 1))
}

#' Helper for HTML feedback messages
training_feedback_ui <- function(agreement_bool) {
  agreement_text <- if(agreement_bool) "Agreement! ✓" else "Disagreement noted."
  color <- if(agreement_bool) "#28a745" else "#ffc107"

  div(
    p(agreement_text, style = paste0("color: ", color, "; font-weight: bold; text-align: center; margin-bottom: 10px;")),
    p("Response recorded! Click a button above to see another example.",
      style = "color: #6c757d; font-style: italic; text-align: center;")
  )
}
