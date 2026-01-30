# Helper to get the absolute path to the embedding binary
get_bin_path <- function(state_file) {
  if (is.null(state_file)) return(NULL)
  bin_name <- gsub(".rds$", ".bin", state_file)
  return(file.path("./tmp/user_data", bin_name))
}
#' Build a FastText Embedding Space from Corpus
#' @param all_text Character vector from df$text_column
#' @param model_path Path to save the .bin file
build_text_space <- function(all_text, model_path) {
  clean_text <- as.character(na.omit(all_text))
  tmp_file <- tempfile(fileext = ".txt")
  writeLines(clean_text, tmp_file)

  model <- fastTextR::ft_train(
    file = tmp_file,
    method = "skipgram",
    control = fastTextR::ft_control(
      word_vec_size = 100L,
      learning_rate = 0.05,
      epoch = 10L
    )
  )

  # Logic: ft_save is stripping .bin.
  # Let's save it, then check if it renamed it.
  fastTextR::ft_save(model, model_path)

  # If the file exists without the .bin, rename it TO the .bin version
  # so our 'file.exists' checks in Shiny work correctly.
  path_no_ext <- gsub("\\.bin$", "", model_path)
  if (file.exists(path_no_ext) && !file.exists(model_path)) {
    file.rename(path_no_ext, model_path)
  }

  if(file.exists(tmp_file)) unlink(tmp_file)
  return(model_path)
}

#' Suggest keywords based on embedding similarity and corpus frequency
#' @param model_path Path to the .bin file
#' @param training_df DF with 'text' and 'label' (1/0)
#' @param full_corpus The entire character vector of the dataset for frequency counts
#' @param current_regex Character vector of keywords currently in the classifier
#' @param ignored_words Character vector from state$keyword_blacklist
#' @param top_n Number of keywords to return (default 10)
fasttext_suggest_keywords <- function(model_path,
                                      training_df,
                                      full_corpus,
                                      current_regex,
                                      penalty_df = NULL,
                                      top_n = 10) {

  if (!file.exists(model_path)) return(data.frame())

  # 1. Load Model (Handling package extension quirk)
  model <- tryCatch({
    fastTextR::ft_load(model_path)
  }, error = function(e) {
    fastTextR::ft_load(gsub("\\.bin$", "", model_path))
  })

  # 2. Calculate Class Centroids
  pos_lines <- training_df$text[training_df$label == 1]
  neg_lines <- training_df$text[training_df$label == 0]

  if(length(pos_lines) == 0) return(data.frame())

  pos_vecs <- fastTextR::ft_sentence_vectors(model, pos_lines)
  neg_vecs <- fastTextR::ft_sentence_vectors(model, neg_lines)

  pv <- colMeans(pos_vecs)
  nv <- colMeans(neg_vecs)

  # 3. Get Vocabulary and Filter
  vocab <- fastTextR::ft_words(model)

  if (length(current_regex) > 0 && any(current_regex != "")) {
    combined_pattern <- paste(current_regex, collapse = "|")
    is_matched <- tryCatch({
      stringr::str_detect(vocab, combined_pattern)
    }, error = function(e) rep(FALSE, length(vocab)))
    candidates <- vocab[!is_matched]
  } else {
    candidates <- vocab
  }

  # Remove short words/numbers
  candidates <- candidates[nchar(candidates) > 2 & !grepl("^\\d+$", candidates)]
  if (length(candidates) == 0) return(data.frame())

  # 4. Rank Candidates
  word_vecs <- fastTextR::ft_word_vectors(model, candidates)

  calc_cos <- function(m, v) {
    as.vector((m %*% v) / (sqrt(rowSums(m^2)) * sqrt(sum(v^2))))
  }

  sim_to_pos <- calc_cos(word_vecs, pv)
  sim_to_neg <- calc_cos(word_vecs, nv)

  # Base Score calculation
  base_scores <- (sim_to_pos - (0.5 * sim_to_neg))

  # 5. Apply Dynamic Penalty
  # Default multiplier is 1.0
  multipliers <- rep(1.0, length(candidates))

  if (!is.null(penalty_df) && nrow(penalty_df) > 0) {
    # Match candidates to penalty list
    match_idx <- match(candidates, penalty_df$Word)
    # Replace 1.0 with the penalty value where a match exists
    multipliers[!is.na(match_idx)] <- penalty_df$value[na.omit(match_idx)]
  }

  final_scores <- base_scores * multipliers

  # 6. Compile Results
  results_df <- data.frame(
    word = candidates,
    score = final_scores,
    stringsAsFactors = FALSE
  )

  # Sort and take Top N
  top_raw <- head(results_df[order(-results_df$score), ], top_n)

  # 7. Calculate Frequency in the full corpus
  freq_counts <- sapply(top_raw$word, function(w) {
    sum(stringr::str_count(full_corpus, stringr::fixed(w)))
  })

  # Return the three requested fields
  final_output <- data.frame(
    Keyword = as.character(top_raw$word),
    Weight = as.numeric(top_raw$score), # The actual semantic score
    Frequency = as.integer(freq_counts), # The corpus count
    stringsAsFactors = FALSE
  )

  return(final_output)
}
