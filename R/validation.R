
#' Get next index from the validation pool
get_next_validation_idx <- function(pool, already_coded_indices) {
  available <- setdiff(seq_len(nrow(pool)), already_coded_indices)
  if (length(available) == 0) return(NULL)
  return(sample(available, 1))
}

#' Helper for the Validation Text UI
validation_box_ui <- function(text) {
  div(
    class = "validation-item-box", # Move the long style string to your CSS file!
    p(text, style = "line-height: 1.8; margin: 0; font-size: 16px; color: #333;")
  )
}
# utils.R

#' Core Statistical Logic for Cai's N
calculate_cais_n_logic <- function(base_rate, previously_coded, data_size, alpha = 0.025) {
  # Adjustment for base rate
  b1_hat <- base_rate
  if (base_rate > 0.01 && base_rate < 0.99 && previously_coded > 10) {
    z_score <- qnorm(1 - alpha/2)
    se <- sqrt((base_rate * (1 - base_rate)) / previously_coded)
    b1_hat <- max(0.05, min(0.95, base_rate - (z_score * se)))
  }

  a_max <- max_accuracy(b1_hat, 0.80)
  if (a_max <= 0.5 || a_max >= 1) return(list(n = Inf, a_max = a_max, b1 = b1_hat))

  cais_n <- ceiling(log(alpha) / log(a_max))
  max_n <- min(1000, data_size * 0.1)

  list(n = max(1, min(cais_n, max_n)), a_max = a_max, b1 = b1_hat)
}

#' UI Template for Validation States
render_validation_ui <- function(state, v = NULL) {
  if (state == "success") {
    return(div(class = "alert alert-success", style = "text-align: center; padding: 30px;",
               h3("✓ Perfect Sampling Complete!", style = "color: #28a745;"),
               p("Classifier validated κ > 0.80", style = "font-weight: bold;"),
               div(style = "display: flex; justify-content: space-around; background: #fff; padding: 15px; border-radius: 8px;",
                   p(strong("Agreements: "), v$perfect_agreements_current_cycle),
                   p(strong("Cycle: "), v$current_cycle),
                   p(strong("Total Items: "), v$total_items_coded))
    ))
  }
  if (state == "failure") {
    return(div(class = "alert alert-warning", style = "text-align: center; padding: 20px;",
               h4("❌ Disagreement Found - Cycle Failed"),
               p("Refine your keywords and return to start a new cycle."),
               actionButton("go_refine_classifiers", "Go Refine Classifiers", class = "btn-warning btn-lg")
    ))
  }
  if (state == "new_cycle") {
    return(div(class = "alert alert-info", style = "text-align: center;", h5("✓ New Cycle Started"), p("Loading item...")))
  }
}


#' Check if the classifier is statistically ready for validation
is_ready_for_validation <- function(perf, training_nrow) {
  if (is.null(perf) || training_nrow < 10) return(FALSE)

  # Requirement: Kappa >= 0.8 AND we have enough training items to trust the stats
  return(perf$kappa >= 0.8)
}
