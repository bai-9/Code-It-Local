ui <- navbarPage(
  title = div(bs_icon("ui-checks"), "Code It!"),
  windowTitle = "Code It!",
  id = "tabs",
  selected = "about",
  inverse = TRUE,

  # Global dependencies
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "script.js")
    )
  ),

  #### TAB 1: ABOUT ####
  tabPanel(
    title = " ",
    icon = icon("info-circle"),
    value = "about",
    fluidRow(
      column(12,
             wellPanel(
               h4("Overview", style = "color: #34495e; margin-top: 25px;"),
               p(strong("Code It!"), "streamlines automated qualitative coding..."),

               # ... [Insert your Workflow/Acknowlegement text here] ...

               hr(),
               p(em("Local Version • Auto-saves to project folder • Private data storage"),
                 style = "text-align: center; color: #7f8c8d; font-size: 14px; margin-top: 20px;")
             )
      )
    )
  ),

  #### TAB 2: UPLOAD DATA ####
  tabPanel(
    title = "Upload Data",
    icon = icon("upload"),
    value = "ud",
    fluidRow(
      column(12,
             wellPanel(
               h4("Upload Data"),
               fluidRow(
                 column(4,
                        fileInput("data_upload", "Upload CSV or Excel File",
                                  accept = c(".csv", ".xls", ".xlsx"))
                 ),
                 column(2),
                 column(4,
                        selectInput("textColumn", "Select the column with text data", choices = "")
                 )
               ),
               fluidRow(
                 column(12, DT::dataTableOutput("uploaded_data"))
               )
             )
      )
    ),
    # Next Step Box (Inside tabPanel)
    fluidRow(
      column(12,
             div(class = "alert alert-info", style = "margin-top: 20px;",
                 h5(icon("lightbulb"), " Next Step", style = "color: #004085; display: inline; margin-left: 5px;"),
                 p("Once you've uploaded your data, proceed to define your code."),
                 actionButton("goto_create_code", "Go to Create Code →", class = "btn-primary")
             )
      )
    )
  ),

  #### TAB 3: CREATE CODE ####
  tabPanel(
    title = "Create Code",
    icon = icon("plus"),
    value = "create_codebook",
    fluidRow(
      column(12,
             wellPanel(
               h4("Create Your Code"),
               p("Define a single code for your analysis", style = "color: #6c757d; font-size: 14px;"),
               DT::dataTableOutput("created_codebook")
             )
      )
    ),
    fluidRow(
      column(12,
             div(class = "alert alert-info", style = "margin-top: 20px;",
                 h5(icon("lightbulb"), " Next Step"),
                 p("After defining your code, proceed to add classifiers/keywords."),
                 actionButton("goto_create_classifiers", "Go to Create Classifiers →", class = "btn-primary")
             )
      )
    )
  ),

  #### TAB 4: CREATE CLASSIFIERS ####
  tabPanel(
    title = "Create Classifiers",
    icon = icon("tags"),
    value = "create_classifiers",
    fluidRow(
      column(12,
             wellPanel(
               h4("Add Classifier for: ", style = "display: inline;"),
               h4(textOutput("current_code_name", inline = TRUE),
                  style = "display: inline; color: #007bff; font-weight: bold;")
             )
      )
    ),
    fluidRow(
      column(12,
             wellPanel(
               fluidRow(
                 column(8,
                        tags$label("Keywords ",
                                   bslib::tooltip(span(icon("info-circle"), style = "color: #007bff;"),
                                                  "Enter comma-separated keywords or regex patterns.", placement = "right")
                        ),
                        textInput("classifier_keywords", label = NULL, placeholder = "love, \\bdevot, etc.")
                 ),
                 column(4, br(), actionButton("add_classifier_submit", "Add Keywords", class = "btn-primary btn-block"))
               )
             )
      )
    ),
    fluidRow(
      column(12,
             wellPanel(
               h4("Classifier List"),
               DT::dataTableOutput("classifier_list")
             )
      )
    ),
    fluidRow(
      column(12,
             wellPanel(
               h4("Keyword Suggester"),
               actionButton("predict_classifiers", "Generate Keyword Suggestions", icon = icon("magic"), class = "btn-info"),
               conditionalPanel(
                 condition = "output.has_suggestions",
                 hr(),
                 DT::dataTableOutput("suggested_keywords_table")
               )
             )
      )
    ),
    fluidRow(
      column(12,
             div(class = "alert alert-info",
                 actionButton("goto_training", "Go to Training →", class = "btn-primary")
             )
      )
    )
  ),

  #### TAB 5: TRAINING ####
  tabPanel(
    title = "Training",
    icon = icon("dumbbell"),
    value = "training",
    fluidRow(
      column(12,
             wellPanel(
               h4("Training for: ", style = "display: inline;"),
               h4(textOutput("training_code_name", inline = TRUE), style = "color: #007bff; display: inline;")
             )
      )
    ),
    fluidRow(
      column(6, wellPanel(h4("Confusion Matrix"), DT::dataTableOutput("confusion_matrix_table"))),
      column(6, wellPanel(h4("Training Metrics"), DT::dataTableOutput("training_metrics")))
    ),
    fluidRow(
      column(9,
             wellPanel(
               h4("Train the Autocoder"),
               fluidRow(
                 column(6, actionButton("show_positive", "Show Positive Example", class = "btn-block")),
                 column(6, actionButton("show_negative", "Show Negative Example", class = "btn-block"))
               ),
               br(),
               div(style = "background: #f8f9fa; border: 1px solid #ddd; padding: 15px; min-height: 200px;",
                   uiOutput("sample_text")),
               br(),
               div(align = "center",
                   shinyjs::hidden(actionButton("training_yes", "YES", icon = icon("check"), class = "btn-success")),
                   shinyjs::hidden(actionButton("training_no", "NO", icon = icon("times"), class = "btn-danger"))
               )
             )
      ),
      column(3,
             wellPanel(
               h4("Controls"),
               actionButton("move_to_testing", "Move to Validation", icon = icon("arrow-right"), class = "btn-primary btn-block"),
               actionButton("move_back_classifier", "Revise Classifiers", icon = icon("arrow-left"), class = "btn-info btn-block"),
               hr(),
               downloadButton("download_training_data", "Download Results", class = "btn-secondary btn-block")
             )
      )
    )
  ),

  #### TAB 6: VALIDATION ####
  tabPanel(
    title = "Validation",
    icon = icon("medal"),
    value = "validation",
    fluidRow(
      column(6, wellPanel(h4("Perfect Sampling"), p("Complete cycle until Kappa ≥ 0.80."))),
      column(6, wellPanel(h4("Target: "), textOutput("validation_code_name")))
    ),
    fluidRow(
      column(6, DT::dataTableOutput("validation_current_cycle")),
      column(6, DT::dataTableOutput("validation_overall"))
    ),
    fluidRow(
      column(8,
             wellPanel(
               h4("Validation Item"),
               div(style = "background: #f8f9fa; padding: 15px; min-height: 200px;", uiOutput("validation_text")),
               br(),
               div(align = "center",
                   shinyjs::hidden(actionButton("validate_yes", "YES", class = "btn-success")),
                   shinyjs::hidden(actionButton("validate_no", "NO", class = "btn-danger"))
               ),
               shinyjs::hidden(div(id = "validation_complete", class = "alert alert-success", h4("Validation Complete!")))
             )
      ),
      column(4,
             wellPanel(
               conditionalPanel("!output.validation_complete_flag", actionButton("restart_validation", "Restart", class = "btn-warning")),
               conditionalPanel("output.validation_complete_flag",
                                actionButton("code_entire_dataset", "Code Entire Dataset", class = "btn-success btn-lg btn-block")
               ),
               hr(),
               downloadButton("download_validation_results", "Download Results", class = "btn-secondary btn-block")
             )
      )
    )
  )
)
