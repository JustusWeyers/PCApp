#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @importFrom shiny tagList tabsetPanel
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard sidebarMenuOutput dashboardBody tabItems
#' @importFrom shinydashboard tabItem sidebarMenu
#'
#' @noRd

app_ui <- function(request, admin) {
  mytheme <- fresh::create_theme(
    fresh::adminlte_color(
      light_blue = "#4ba046"
    ),
    fresh::adminlte_sidebar(
      dark_bg = "#EDEDED",
      dark_hover_bg = "#72a36f",
      dark_color = "#2E3440"
    ),
    fresh::adminlte_global(
      content_bg = "#FFFFFF",
      box_bg = "#FFFFFF",
      info_box_bg = "#EDEDED"
    )
  )


  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Dashboard framework
    shinydashboard::dashboardPage(
      # theme = bslib::bs_theme(primary = "green"),

      # Header
      shinydashboard::dashboardHeader(
        title = "PCApp"
      ),

      # Sidebar
      shinydashboard::dashboardSidebar(
        
        br(),
        # hr(style = "border-top: 1px solid #c1c9bf;"),
        
        shiny::uiOutput("user"),
        
        hr(style = "border-top: 1px solid #c1c9bf;"),

        shinydashboard::sidebarMenu(
          id="mytabs",
          shinydashboard::sidebarMenuOutput("sidebarmenu")
        ),
        
        hr(style = "border-top: 1px solid #c1c9bf;"),
        
        shiny::fluidRow(
          col_2(
            shiny::img(
              src = 'www/github.svg',
              height="15px", width="15px",
              style = "display: block; margin-left: 20px; margin-right: auto;"
            )
          ),
          col_10(
            tags$a(
              href="https://github.com/JustusWeyers/PCApp",
              "GitHub"
            )
          )
        )
      ),

      # Body
      shinydashboard::dashboardBody(

        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "pcapp.css")
        # ),
        fresh::use_theme(mytheme),

        shinydashboard::tabItems(

          # Home
          shinydashboard::tabItem(
            tabName = "home",
            mod_home_ui("home")
          ),

          # Import
          shinydashboard::tabItem(
            tabName = "import", shiny::tabsetPanel(
              type = "tabs",
              mod_import_ui("import_timeseries"),
              mod_import_ui("import_metadata"),
              mod_import_ui("import_vectordata"),
              mod_import_ui("import_rasterdata")
            )
          ),

          # Data selection
          shinydashboard::tabItem(
            tabName = "selection",
            mod_selection_ui("select")
          ),

          # Analysis
          shinydashboard::tabItem(
            tabName = "pca",
            mod_PCA_ui("PCA")
          ),

          # Export
          shinydashboard::tabItem(
            tabName = "export",
            mod_export_ui("export")
          ),

          # Settings
          shinydashboard::tabItem(
            tabName = "settings", shiny::tabsetPanel(
              type = "tabs",
              mod_general_ui("general_settings"),
              mod_database_ui("database_tab"),
              mod_ENV_ui("ENV")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "PCApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' render_sidebar
#'
#' @description Render sidebar menu based on textelements
#'
#' @return sidebarmenu A shinydashboard::renderMenu()-menu
#'
#' @noRd
#'
#' @importFrom shinydashboard renderMenu sidebarMenu menuItem
#' @importFrom purrr pmap
#' @importFrom shiny icon

render_sidebar <- function(apptext) {
  # Define sidebar menu content
  menu_items <- data.frame(
    name = apptext,
    id = c("home", "import",  "selection", "pca", "export", "settings"),
    icon = c("house", "file-arrow-up", "filter", "wave-square", "file-arrow-down", "gear")
  )
  
  # Build sidebar menu
  sidebarmenu <- shinydashboard::sidebarMenu(
    # Iterate over menu_items name and id
    purrr::pmap(
      list(menu_items$name, menu_items$id, menu_items$icon),
      \(x, y, i) shinydashboard::menuItem(x, tabName = y, icon = shiny::icon(i))
    )
  )
  
  
  return(sidebarmenu)
}

