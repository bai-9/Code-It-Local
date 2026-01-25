ui <- navbarPage(
  title = div(bs_icon("ui-checks"), "Code It!"),
  id = "tabs",
  selected = "project", # Set the default to the new project tab
  inverse = TRUE,
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Scannability for long text in data tables */
        table.dataTable tbody td {
          max-width: 300px;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        /* Visual cue for the active project */
        .project-status {
          padding: 8px 15px;
          color: #ecf0f1;
          font-size: 0.9em;
          border-bottom: 1px solid #34495e;
          background: #2c3e50;
        }
      ")),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    # Global header to show current status across all tabs
    uiOutput("global_project_status")
  ),

  # 1. Project Management (Merged Tab)
  tabPanel("Project", icon = icon("gear"), value = "project",
           mod_project_ui("project_1")),

  # 2. Classifier Logic
  tabPanel("Classifiers", icon = icon("filter"), value = "create_classifiers",
           mod_classifier_ui("classifier_1")),

  # 3. Training
  tabPanel("Training", icon = icon("graduation-cap"), value = "training",
           mod_training_ui("training_1")),

  # 4. Validation
  tabPanel("Validation", icon = icon("medal"), value = "validation",
           mod_validation_ui("validation_1")),

  # 5. Finalize
  tabPanel("Download", icon = icon("download"), value = "download",
           mod_download_ui("download_1")),

  # 6. About
  tabPanel(title = "Help", icon = icon("question-circle"), value = "about",
           h2("Code It"),
           p("This tool allows you to train and validate keyword-based classifiers using Perfect Sampling logic."))
)
