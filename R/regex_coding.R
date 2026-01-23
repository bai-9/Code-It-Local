# utils.R

#' Perform regex-based keyword matching on a dataset
#' @param data The dataframe to code
#' @param text_col The name of the column containing text
#' @param code_name The name of the code (used for column naming)
#' @param classifier_df The dataframe containing keywords
execute_regex_coding <- function(data, text_col, code_name, classifier_df) {
  # 1. Extract and clean keywords
  all_keywords <- classifier_df$Keywords %>%
    lapply(function(x) trimws(strsplit(x, ",")[[1]])) %>%
    unlist() %>%
    unique()

  all_keywords <- all_keywords[all_keywords != "" & !is.na(all_keywords)]

  if (length(all_keywords) == 0) return(NULL)

  # 2. Construct Regex Pattern
  regex_pattern <- paste0("(", paste(all_keywords, collapse = "|"), ")")

  # 3. Apply matches (with error handling for bad regex syntax)
  matches <- tryCatch({
    grepl(regex_pattern, data[[text_col]], ignore.case = TRUE)
  }, error = function(e) {
    stop(paste("Invalid regex syntax:", e$message))
  })

  # 4. Construct Output
  new_col_name <- paste0(code_name, ".Positive")
  coded_df <- data.frame(
    TextData = data[[text_col]],
    stringsAsFactors = FALSE
  )
  coded_df[[new_col_name]] <- as.integer(matches)

  return(coded_df)
}
