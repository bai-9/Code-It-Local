ui <- navbarPage(
  title = div(bs_icon("ui-checks"), "Code-- It!"),
  windowTitle = "Code It!",
  id = "tabs",
  selected = "ud",
  inverse = TRUE,

  # HEADER: Global scripts and Auth Overlay (Fixes Navbar errors)
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* navbar alignment */
        .navbar-nav > li > a { color: #9d9d9d !important; }
        .navbar-nav > li > a:hover { color: #fff !important; background-color: transparent !important; }

        /* editable datatable styling */
        .dataTables_wrapper .dataTable tbody td { padding: 12px 8px !important; border: 1px solid #dee2e6 !important; background-color: #ffffff !important; color: #333333 !important; }
        .dataTables_wrapper .dataTable tbody td:hover { background-color: #e3f2fd !important; cursor: pointer !important; border: 1px solid #2196F3 !important; }
        .dataTables_wrapper .dataTable tbody td input { width: 100% !important; padding: 8px 12px !important; border: 2px solid #007bff !important; border-radius: 4px !important; }

        /* Authentication styles */
        .auth-container { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: linear-gradient(45deg, #f8f9fa, #e9ecef); z-index: 9999; display: flex; justify-content: center; align-items: center; }
        .auth-panel { background: white; padding: 40px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); max-width: 400px; width: 90%; }
        .auth-title { text-align: center; color: #2c3e50; margin-bottom: 10px; }
        .auth-subtitle { text-align: center; color: #7f8c8d; font-size: 16px; margin-bottom: 30px; }
        .btn-auth { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; padding: 12px 20px; border-radius: 25px; color: white; width: 100%; margin-bottom: 10px; }
        .auth-links { text-align: center; margin-top: 20px; padding-top: 20px; border-top: 1px solid #dee2e6; }
      ")),
      tags$script(HTML("
        function showView(viewId) { $('.auth-view').hide(); $('#' + viewId).show(); }
        function deleteKeyword(groupId, keywordIndex) {
          Shiny.setInputValue('delete_individual_keyword', {group_id: groupId, keyword_index: keywordIndex, timestamp: new Date().getTime()}, {priority: 'event'});
        }
        $(document).on('click', '.add-keyword-btn', function() {
          var keyword = $(this).data('keyword');
          Shiny.setInputValue('add_suggested_keyword', keyword, {priority: 'event'});
        });
      "))
    ),

    # Authentication Overlay (Moved to header so it doesn't break navbar)
    conditionalPanel(
      condition = "output.authenticated == false",
      div(class = "auth-container",
          div(class = "auth-panel",
              # Login View
              div(id = "login-view", class = "auth-view",
                  h2(bs_icon("ui-checks"), "Code It!", class = "auth-title"),
                  p("Develop, Automate, and Validate", class = "auth-subtitle"),
                  div(class = "form-group", textInput("login_username", "Username", placeholder = "Enter username")),
                  div(class = "form-group", passwordInput("login_password", "Password", placeholder = "Enter password")),
                  actionButton("login_submit", "Login", class = "btn-auth"),
                  div(class = "auth-links",
                      a(href = "#", onclick = "showView('register-view')", "Create New Account"), " | ",
                      a(href = "#", onclick = "showView('forgot-view')", "Forgot Password?"))
              ),
              # Register/Forgot views would go here... (collapsed for brevity)
              div(id = "register-view", class = "auth-view", style = "display: none;", h3("Register"))
          )
      )
    )
  ),

  #### TAB 1: ABOUT ####
  tabPanel(title = " ", icon = icon("info-circle"), value = "about",
           conditionalPanel(condition = "output.authenticated == true",
                            fluidRow(column(12, wellPanel(
                              h4("Overview"),
                              p(strong("Code It!"), "streamlines automated qualitative coding."),
                              h4("Perfect Sampling Validation"),
                              p("The app uses a cycle-based perfect validation approach (Shaffer & Cai's 2024).")
                            )))
           )
  ),

  #### TAB 2: UPLOAD DATA ####
  tabPanel(title = "Upload Data", value = "ud",
           conditionalPanel(condition = "output.authenticated == true",
                            tagList(
                              fluidRow(column(12, wellPanel(
                                h4("Upload Data"),
                                fluidRow(
                                  column(4, fileInput("data_upload", "Upload CSV or Excel", accept = c(".csv", ".xls", ".xlsx"))),
                                  column(4, offset = 2, selectInput("textColumn", "Select text column", choices = ""))
                                ),
                                DT::dataTableOutput("uploaded_data")
                              ))),
                              fluidRow(column(12, div(class = "alert alert-info",
                                                      actionButton("goto_create_code", "Go to Create Code →", class = "btn-primary")
                              )))
                            )
           )
  ),

  #### TAB 3: CREATE CODE ####
  tabPanel(title = "Create Code", value = "create_codebook",
           conditionalPanel(condition = "output.authenticated == true",
                            tagList(
                              fluidRow(column(12, wellPanel(h4("Create Your Code"), DT::dataTableOutput("created_codebook")))),
                              fluidRow(column(12, div(class = "alert alert-info",
                                                      actionButton("goto_create_classifiers", "Go to Create Classifiers →", class = "btn-primary")
                              )))
                            )
           )
  ),

  #### TAB 4: TRAINING ####
  tabPanel(title = "Training", value = "training",
           conditionalPanel(condition = "output.authenticated == true",
                            tagList(
                              fluidRow(column(12, wellPanel(h4("Training for: ", textOutput("training_code_name", inline = TRUE))))),
                              fluidRow(
                                column(6, wellPanel(h4("Confusion Matrix"), DT::dataTableOutput("confusion_matrix_table"))),
                                column(6, wellPanel(h4("Training Metrics"), DT::dataTableOutput("training_metrics")))
                              ),
                              fluidRow(
                                column(9, wellPanel(
                                  h4("Train the Autocoder"),
                                  div(style = "background: #f8f9fa; padding: 15px; min-height: 200px;", uiOutput("sample_text")),
                                  br(), align = "center",
                                  actionButton("training_yes", "YES", class = "btn-success"),
                                  actionButton("training_no", "NO", class = "btn-danger")
                                )),
                                column(3, wellPanel(
                                  actionButton("move_to_testing", "Move to Validation", class = "btn-primary btn-block")
                                ))
                              ),
                              fluidRow(column(12, div(class = "alert alert-info",
                                                      actionButton("goto_validation", "Go to Validation →", class = "btn-primary")
                              )))
                            )
           )
  ),

  #### TAB 5: VALIDATION ####
  tabPanel(title = "Validation", value = "validation",
           conditionalPanel(condition = "output.authenticated == true",
                            tagList(
                              fluidRow(
                                column(6, wellPanel(style = "height: 140px;", h4("Perfect Sampling"), p("Complete when Kappa ≥ 0.80"))),
                                column(6, wellPanel(style = "height: 140px;", h4("Validation for: ", textOutput("validation_code_name", inline = TRUE))))
                              ),
                              fluidRow(
                                column(6, wellPanel(h4("Current Cycle"), DT::dataTableOutput("validation_current_cycle"))),
                                column(6, wellPanel(h4("Overall Metrics"), DT::dataTableOutput("validation_overall")))
                              ),
                              fluidRow(
                                column(8, wellPanel(
                                  h4("Validation Item"),
                                  div(style = "background: #f8f9fa; padding: 15px; min-height: 200px;", uiOutput("validation_text")),
                                  br(), align = "center",
                                  actionButton("validate_yes", "YES", class = "btn-success"),
                                  actionButton("validate_no", "NO", class = "btn-danger")
                                )),
                                column(4, wellPanel(
                                  h4("Next Steps"),
                                  conditionalPanel(condition = "output.validation_complete_flag",
                                                   actionButton("code_entire_dataset", "Code Entire Dataset", icon = icon("magic"), class = "btn-success btn-block")
                                  ),
                                  downloadButton("download_validation_results", "Download Results", class = "btn-secondary btn-block")
                                ))
                              )
                            )
           )
  )
)
