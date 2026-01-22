# --- 1. LOAD LIBRARIES ---
# Ensure these packages are installed in your Shiny environment
library(tm)
library(e1071)
library(tidyverse)
library(tidytext)
library(dplyr)

# --- 2. HELPER FUNCTIONS (from your script) ---

# (Helper for preprocess_and_tokenize)
BigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(as.character(x)), 2), paste, collapse = "_"), use.names = FALSE)
}

# (Helper for preprocess_and_tokenize)
TrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(as.character(x)), 3), paste, collapse = "_"), use.names = FALSE)
}

#' Preprocesses text data and creates n-gram DTMs
#'
#' @param data A dataframe with a 'text' column and a 'class' column.
#' @return A list containing the DTM dataframe, combined matrix, terms, and corpus.
preprocess_and_tokenize <- function(data) {
  # Create corpus
  corpus <- VCorpus(VectorSource(data$text))

  # Clean text
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)

  # Create DTMs
  dtm_unigram <- DocumentTermMatrix(corpus)
  dtm_bigram <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
  dtm_trigram <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))

  # Combine all DTMs
  dtm_unigram_mat <- as.matrix(dtm_unigram)
  dtm_bigram_mat <- as.matrix(dtm_bigram)
  dtm_trigram_mat <- as.matrix(dtm_trigram)

  # Merge all matrices
  # Handle cases where one DTM might be empty (e.g., no trigrams found)
  dtm_combined <- cbind(
    if (ncol(dtm_unigram_mat) > 0) dtm_unigram_mat else matrix(nrow = nrow(dtm_unigram_mat), ncol = 0),
    if (ncol(dtm_bigram_mat) > 0) dtm_bigram_mat else matrix(nrow = nrow(dtm_bigram_mat), ncol = 0),
    if (ncol(dtm_trigram_mat) > 0) dtm_trigram_mat else matrix(nrow = nrow(dtm_trigram_mat), ncol = 0)
  )

  # Convert to dataframe and add class labels
  dtm_df <- as.data.frame(dtm_combined)

  # Make column names valid R variable names
  colnames(dtm_df) <- make.names(colnames(dtm_df))

  dtm_df$class <- as.factor(data$class)

  # Create a combined DTM object
  combined_terms <- c(Terms(dtm_unigram), Terms(dtm_bigram), Terms(dtm_trigram))

  # Ensure combined_terms are also valid R names to match dtm_df
  combined_terms <- make.names(combined_terms)

  return(list(dtm_df = dtm_df,
              combined_matrix = dtm_combined,
              combined_terms = combined_terms,
              corpus = corpus))
}

#' Trains the Naive Bayes model
#'
#' @param dtm_df The DTM dataframe from preprocess_and_tokenize.
#' @return A trained naiveBayes model object.
train_naive_bayes <- function(dtm_df) {
  # Ensure 'class' is a factor
  dtm_df$class <- as.factor(dtm_df$class)
  model <- naiveBayes(class ~ ., data = dtm_df)
  return(model)
}

#' Analyzes token importance
#'
#' @param data The original training dataframe (must have 'class' column).
#' @param dtm A document-term matrix.
#' @return A dataframe with token statistics.
analyze_token_importance <- function(data, dtm) {
  # Get token frequencies by class
  tokens <- as.data.frame(as.matrix(dtm))
  tokens$class <- data$class

  # Calculate token statistics for each class
  token_stats <- data.frame()

  # Get all token names (except the 'class' column)
  token_names <- colnames(tokens)[colnames(tokens) != "class"]

  # Ensure there are tokens to analyze
  if (length(token_names) == 0) {
    warning("No tokens found to analyze.")
    return(data.frame())
  }

  for (token in token_names) {
    # Need to handle potential invalid column names if not sanitized
    safe_token <- make.names(token)
    if (!safe_token %in% colnames(tokens)) next # Skip if token name is problematic

    class_1_freq <- sum(tokens[tokens$class == 1, safe_token])
    class_0_freq <- sum(tokens[tokens$class == 0, safe_token])

    class_1_docs <- sum(tokens[tokens$class == 1, safe_token] > 0)
    class_0_docs <- sum(tokens[tokens$class == 0, safe_token] > 0)

    total_1 <- sum(tokens$class == 1)
    total_0 <- sum(tokens$class == 0)

    # Avoid division by zero if a class has 0 documents
    if (total_1 == 0) total_1 <- 1e-6
    if (total_0 == 0) total_0 <- 1e-6

    class_1_pct <- class_1_docs / total_1
    class_0_pct <- class_0_docs / total_0

    # Add a small epsilon to denominator to avoid division by zero
    predictive_ratio <- (class_1_pct + 0.01) / (class_0_pct + 0.01)

    token_stats <- rbind(token_stats, data.frame(
      token = token,
      class_1_freq = class_1_freq,
      class_0_freq = class_0_freq,
      class_1_pct = class_1_pct,
      class_0_pct = class_0_pct,
      predictive_ratio = predictive_ratio
    ))
  }

  return(token_stats)
}


# --- 3. MAIN WRAPPER FUNCTION ---
#'
#' This is the main function you will call from your Shiny app.
#' It runs the full training and analysis pipeline.
#'
#' @param training_filepath The path to the user's uploaded CSV file.
#' @return A list containing the trained model, a vector of training terms,
#'         and dataframes of the most predictive positive and negative tokens.
#'
train_and_analyze_model <- function(training_filepath) {

  # 1. Load Data
  train_data <- read_csv(training_filepath)

  # 2. Clean Text
  # Handle potential encoding issues
  train_data$text <- iconv(train_data$text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  # Remove NAs that might be created
  train_data <- train_data %>% filter(!is.na(text))

  # 3. Preprocess and Tokenize
  processed <- preprocess_and_tokenize(train_data)

  # Check if DTM is empty
  if (ncol(processed$dtm_df) <= 1) {
    stop("Error: No features (tokens) were generated. The input text might be empty, stopword-only, or in an unexpected format.")
  }

  # 4. Train Model
  model <- train_naive_bayes(processed$dtm_df)

  # 5. Analyze Tokens
  token_analysis <- analyze_token_importance(train_data, processed$combined_matrix)

  # 6. Get Top Tokens
  class_1_tokens <- token_analysis %>%
    arrange(desc(predictive_ratio)) %>%
    filter(predictive_ratio > 1) %>%
    head(20)

  class_0_tokens <- token_analysis %>%
    arrange(predictive_ratio) %>%
    filter(predictive_ratio < 1) %>%
    head(20)

  # 7. Return all useful objects
  return(list(
    model = model,
    training_terms = processed$combined_terms,
    positive_tokens = class_1_tokens,
    negative_tokens = class_0_tokens,
    full_analysis = token_analysis
  ))
}


# --- 4. PREDICTION FUNCTION ---
#'
#' Predicts the class of new text based on the trained model.
#'
#' @param model_object The list returned by train_and_analyze_model.
#' @param new_text_vector A character vector of new text strings to predict.
#' @return A factor vector of predictions (e.g., "0" or "1").
#'
predict_new_text <- function(model_object, new_text_vector) {

  # Get model and terms from the trained object
  model <- model_object$model
  training_terms <- model_object$training_terms

  # 1. Create and clean corpus for new text
  new_corpus <- VCorpus(VectorSource(new_text_vector))
  new_corpus <- tm_map(new_corpus, content_transformer(tolower))
  new_corpus <- tm_map(new_corpus, removePunctuation)
  new_corpus <- tm_map(new_corpus, removeNumbers)
  new_corpus <- tm_map(new_corpus, removeWords, stopwords("english"))
  new_corpus <- tm_map(new_corpus, stripWhitespace)

  # 2. Tokenize into n-grams
  # We need to apply all three tokenizers and combine
  dtm_unigram <- DocumentTermMatrix(new_corpus)
  dtm_bigram <- DocumentTermMatrix(new_corpus, control = list(tokenize = BigramTokenizer))
  dtm_trigram <- DocumentTermMatrix(new_corpus, control = list(tokenize = TrigramTokenizer))

  # 3. Create a DTM using the *exact* terms from the training data
  # This is the crucial step

  # Combine terms from the new tokenizers
  new_terms_unigram <- Terms(dtm_unigram)
  new_terms_bigram <- Terms(dtm_bigram)
  new_terms_trigram <- Terms(dtm_trigram)

  # Find which of *our* new terms were in the training set
  all_new_terms <- c(new_terms_unigram, new_terms_bigram, new_terms_trigram)
  valid_terms <- all_new_terms[all_new_terms %in% training_terms]

  # Re-create a combined DTM with only the valid, matching terms
  # We can't just use the 'dictionary' control on all three, as it's complex
  # A safer way is to re-run the DTMs and filter

  # This is tricky. A simpler, more robust way:
  # Create a combined DTM for the new data, then select only columns
  # that were in the original training_terms.

  dtm_u_mat <- as.matrix(dtm_unigram)
  dtm_b_mat <- as.matrix(dtm_bigram)
  dtm_t_mat <- as.matrix(dtm_trigram)

  # Make column names consistent
  colnames(dtm_u_mat) <- make.names(colnames(dtm_u_mat))
  colnames(dtm_b_mat) <- make.names(colnames(dtm_b_mat))
  colnames(dtm_t_mat) <- make.names(colnames(dtm_t_mat))

  new_dtm_combined <- cbind(
    if (ncol(dtm_u_mat) > 0) dtm_u_mat else matrix(nrow = nrow(dtm_u_mat), ncol = 0),
    if (ncol(dtm_b_mat) > 0) dtm_b_mat else matrix(nrow = nrow(dtm_b_mat), ncol = 0),
    if (ncol(dtm_t_mat) > 0) dtm_t_mat else matrix(nrow = nrow(dtm_t_mat), ncol = 0)
  )

  # 4. Align new DTM with training DTM
  new_df <- data.frame(matrix(0,
                              nrow = nrow(new_dtm_combined),
                              ncol = length(training_terms)))

  colnames(new_df) <- training_terms

  # Find matching columns
  common_cols <- intersect(colnames(new_dtm_combined), training_terms)

  # Fill in the counts for the matching columns
  if (length(common_cols) > 0) {
    new_df[, common_cols] <- new_dtm_combined[, common_cols]
  }

  # # 5. Make prediction
  # # Note: The 'class' column is not in new_df, which is correct for prediction
  # prediction <- predict(model, new_df)
  # return(prediction)
}
