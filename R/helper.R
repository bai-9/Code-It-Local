
#' Create the styled, editable codebook datatable
render_codebook_dt <- function(df) {
  DT::datatable(
    df,
    rownames = FALSE,
    class = "cell-border stripe compact",
    editable = list(target = 'cell', disable = list(columns = NULL)),
    options = list(
      dom = 't',
      ordering = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-left', targets = '_all'),
        list(width = '200px', targets = 0),
        list(width = '300px', targets = 1),
        list(width = '300px', targets = 2)
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  $(this.api().table().container()).find('input').css({",
        "    'background-color': '#ffffff',",
        "    'color': '#000000',",
        "    'border': '2px solid #007bff'",
        "  });",
        "}"
      )
    ),
    callback = DT::JS(
      "table.on('click', 'td', function() {",
      "  setTimeout(function() {",
      "    $('input').css({",
      "      'background-color': '#ffffff !important',",
      "      'color': '#000000 !important',",
      "      'border': '2px solid #007bff !important'",
      "    });",
      "  }, 100);",
      "});"
    )
  )
}
# utils.R

#' Expand comma-separated keywords into a dataframe with delete buttons
prepare_classifier_data <- function(classifier_df) {
  if (nrow(classifier_df) == 0) return(NULL)

  # Expand keywords
  expanded_data <- do.call(rbind, lapply(seq_len(nrow(classifier_df)), function(i) {
    keywords <- trimws(strsplit(classifier_df$Keywords[i], ",")[[1]])
    keywords <- keywords[keywords != ""]
    if (length(keywords) > 0) {
      data.frame(
        Group_ID = i,
        Keyword = keywords,
        Keyword_Index = seq_along(keywords),
        stringsAsFactors = FALSE
      )
    }
  }))

  if (is.null(expanded_data) || nrow(expanded_data) == 0) return(NULL)

  # Add HTML Delete buttons
  expanded_data$Delete_Button <- sprintf(
    '<button class="btn btn-danger btn-xs" onclick="deleteKeyword(%d, %d)" style="padding: 2px 6px; font-size: 11px;"><i class="fa fa-trash"></i></button>',
    expanded_data$Group_ID, expanded_data$Keyword_Index
  )

  return(expanded_data[, c("Keyword", "Delete_Button")])
}

#' Render the Classifier DT
render_classifier_dt <- function(display_df) {
  if (is.null(display_df)) {
    return(DT::datatable(
      data.frame(Message = "No classifiers added yet."),
      rownames = FALSE, colnames = NULL, options = list(dom = 't')
    ))
  }

  DT::datatable(
    display_df,
    rownames = FALSE,
    colnames = NULL,
    class = "cell-border stripe",
    escape = FALSE,
    options = list(
      dom = 't',
      ordering = FALSE,
      pageLength = 25,
      columnDefs = list(
        list(width = '80%', targets = 0, className = 'dt-left'),
        list(width = '20%', targets = 1, className = 'dt-center')
      )
    )
  )
}
# utils.R

#' Generate the Delete Confirmation Modal
keyword_delete_modal <- function(keyword) {
  modalDialog(
    title = "Confirm Keyword Deletion",
    p("Are you sure you want to delete this keyword?"),
    div(
      style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 15px 0; border: 1px solid #f5c6cb; text-align: center;",
      h5(keyword, style = "color: #721c24; font-family: 'Courier New', monospace; margin: 0;")
    ),
    p("This action cannot be undone.", style = "color: #dc3545; font-size: 13px;"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_delete_keyword", "Delete Keyword", class = "btn-danger")
    )
  )
}

#' Logic to remove a keyword from a comma-separated string in a dataframe
remove_keyword_logic <- function(df, group_id, keyword_index) {
  current_string <- df$Keywords[group_id]
  keywords <- trimws(strsplit(current_string, ",")[[1]])
  keywords <- keywords[keywords != ""]

  keywords <- keywords[-keyword_index]

  if (length(keywords) > 0) {
    df$Keywords[group_id] <- paste(keywords, collapse = ", ")
  } else {
    df <- df[-group_id, , drop = FALSE]
  }
  return(df)
}
# utils.R

#' Prepare the suggested keywords table with Add and Trash buttons
#' @param suggestions_df The raw output from the NB model
#' @param blacklist Character vector of words the user never wants to see again
prepare_suggestions_table <- function(suggestions_df, blacklist) {
  if (is.null(suggestions_df) || nrow(suggestions_df) == 0) return(NULL)

  # Filter out permanently blacklisted keywords
  df <- suggestions_df[!tolower(suggestions_df$Keyword) %in% tolower(blacklist), ]
  if (nrow(df) == 0) return(NULL)

  # Create Add Button (Blue)
  df$Add <- sprintf(
    '<button class="btn btn-primary btn-xs" onclick="addSuggested(\'%s\')" style="padding: 4px 8px;"><i class="fa fa-plus"></i> Add</button>',
    df$Keyword
  )

  # Create Trash Button (Red)
  df$Remove <- sprintf(
    '<button class="btn btn-danger btn-xs" onclick="trashSuggested(\'%s\')" style="padding: 4px 8px;"><i class="fa fa-trash"></i></button>',
    df$Keyword
  )

  return(df)
}
