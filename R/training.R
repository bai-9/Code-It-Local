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
# utils.R

#' Refresh auto-coding based on current keywords
refresh_training_auto_coding <- function(training_df, current_keywords_df) {
  if (nrow(training_df) == 0) return(training_df)

  # Extract keywords into a regex pattern
  k_list <- get_current_keyword_list(current_keywords_df)
  if (length(k_list) == 0) {
    training_df$auto.coding <- 0
    return(training_df)
  }

  pattern <- paste0("\\b(", paste(k_list, collapse = "|"), ")\\b")
  training_df$auto.coding <- as.integer(grepl(pattern, training_df$TextData, ignore.case = TRUE))
  return(training_df)
}

#' Master Metric Calculator
calculate_training_performance <- function(df) {
  if (nrow(df) == 0) return(NULL)

  tp <- sum(df$auto.coding == 1 & df$user.coding == 1)
  fp <- sum(df$auto.coding == 1 & df$user.coding == 0)
  fn <- sum(df$auto.coding == 0 & df$user.coding == 1)
  tn <- sum(df$auto.coding == 0 & df$user.coding == 0)

  n <- nrow(df)
  acc <- (tp + tn) / n
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0

  # Kappa
  p_obs <- acc
  p_exp <- (((tp+fp)/n)*((tp+fn)/n)) + (((fn+tn)/n)*((fp+tn)/n))
  kappa <- if (p_exp < 1) (p_obs - p_exp) / (1 - p_exp) else 0

  list(tp=tp, fp=fp, fn=fn, tn=tn, kappa=kappa,
       fdr = if((tp+fp)>0) fp/(tp+fp) else 0,
       for_rate = if((fn+tn)>0) fn/(fn+tn) else 0,
       acc=acc, prec=precision, rec=recall, n=n)
}
# utils.R

#' Prepare training data for CSV export
prepare_export_data <- function(df) {
  if (nrow(df) == 0) return(data.frame(Message = "No data"))

  df %>%
    mutate(
      Agreement = ifelse(auto.coding == user.coding, "Agreement", "Disagreement"),
      Classification_Type = case_when(
        auto.coding == 1 & user.coding == 1 ~ "True Positive",
        auto.coding == 1 & user.coding == 0 ~ "False Positive",
        auto.coding == 0 & user.coding == 1 ~ "False Negative",
        auto.coding == 0 & user.coding == 0 ~ "True Negative"
      )
    )
}

#' Create a summary report of all performance metrics
prepare_metrics_report <- function(perf) {
  if (is.null(perf)) return(data.frame(Message = "No metrics available"))

  data.frame(
    Category = c(rep("Performance", 6), rep("Confusion Matrix", 4), "Summary"),
    Metric = c("Kappa", "FDR", "FOR", "Precision", "Recall", "Accuracy",
               "TP", "FP", "FN", "TN", "Total"),
    Value = c(perf$kappa, perf$fdr, perf$for_rate, perf$prec, perf$rec, perf$acc,
              perf$tp, perf$fp, perf$fn, perf$tn, perf$n)
  )
}
