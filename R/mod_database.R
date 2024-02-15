#' mod_database UI Function
#'
#' @description Create the settings tab regarding database issues
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel uiOutput  tagList fluidPage fluidRow htmlOutput

mod_database_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    shiny::uiOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage(
        # Header 1
        shiny::uiOutput(ns("ui_header1")),
        # Body
        shiny::fluidRow(
          col_10(
            shiny::uiOutput(ns("ui_connection_box")),
          ),
          col_2(
            shiny::htmlOutput(ns("db_logo"))
          )
        ),
        # Header 2
        shiny::uiOutput(ns("ui_header2")),
        shiny::fluidRow(),
        # Header 3
        shiny::uiOutput(ns("ui_header3")),
        shiny::fluidRow()
      )
    )
  )
}

#' mod_database Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param r global reactive values
#' @param txt app text
#'
#' @noRd
#'
#' @importFrom shiny observeEvent renderUI textInput actionButton reactive
#' @importFrom shiny reactiveValues img tabPanel fluidRow uiOutput titlePanel
#' @importFrom shinyThings radioSwitchButtons
#' @importFrom shinydashboard tabBox
#' @importFrom DBI dbDisconnect
#' @importFrom stats setNames

mod_database_server <- function(id, r, txt) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Constants

    # Default database access (sys?)
    default_database_access = stats::setNames(
      c("localhost", "user", "mysecretpassword", "5432", "mydb"),
      c("host", "user", "password", "port", "dbname")
    )

    ### Serverlogic

    # Module servers global variables
    server = shiny::reactiveValues(
      # Supported database types
      database_types = c("RPostgres", "RSQLite"),
      # Connection
      con = connect_database(
        type = "RPostgres",
        host = getElement(default_database_access, "host"),
        port = getElement(default_database_access, "port"),
        user = getElement(default_database_access, "user"),
        dbname = getElement(default_database_access, "dbname"),
        password = getElement(default_database_access, "password")
      )
    )

    # Connect to database functionality
    shiny::observeEvent(input$dbtype, {

      # At first disconnect
      DBI::dbDisconnect(server$con)

      # Then reconnect
      if (shiny::isTruthy(input$host)) {
        server$con = connect_database(
          type = input$dbtype,
          host = input$host,
          port = input$port,
          user = input$user,
          dbname = input$dbname,
          password = input$password
        )
      } else {
        server$con = connect_database(
          type = input$dbtype,
          host = getElement(default_database_access, "host"),
          port = getElement(default_database_access, "port"),
          user = getElement(default_database_access, "user"),
          dbname = getElement(default_database_access, "dbname"),
          password = getElement(default_database_access, "password")
        )
      }
    })

    ### UI Elements

    # Tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(txt[2])
    })

    # Header 1
    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel(txt[11])
    })

    # Database system radiobuttons
    output$ui_dbtype_radiobutton <- shiny::renderUI(
      shinyThings::radioSwitchButtons(
        ns("dbtype"),
        label = NULL,
        choices = server$database_types,
        selected = attr(class(server$con), "package")
      )
    )

    # Host text input
    output$ui_host_textinput <- shiny::renderUI(
        shiny::textInput(ns("host"), label = txt[9],
                         value = default_database_access["host"])
    )

    # Port text input
    output$ui_port_textinput <- shiny::renderUI(
        shiny::textInput(ns("port"), label = txt[8],
                         value = default_database_access["port"])
    )

    # User text input
    output$ui_user_textinput <- shiny::renderUI(
        shiny::textInput(ns("user"), label = txt[6],
                         value = default_database_access["user"])
    )

    # Password text input
    output$ui_password_textinput <- shiny::renderUI(
        shiny::textInput(ns("password"), label = txt[7],
                         value = default_database_access["password"])
    )

    # Database name text input
    output$ui_dbname_textinput <- shiny::renderUI(
        shiny::textInput(ns("dbname"), label = txt[2],
                         value = default_database_access["dbname"])
    )

    # Connection tabbox
    output$ui_connection_box <- shiny::renderUI(shinydashboard::tabBox(
        id = ns("mytabbox"), title = "", width = "100%",
        shiny::tabPanel(
          txt[14],
          shiny::uiOutput(ns("ui_dbtype_radiobutton"))
        ),
        shiny::tabPanel(
          txt[15],
          shiny::fluidRow(
            col_6(
              shiny::uiOutput(ns("ui_host_textinput")),
              shiny::uiOutput(ns("ui_port_textinput")),
              shiny::uiOutput(ns("ui_user_textinput"))
            ),
            col_6(
              shiny::uiOutput(ns("ui_password_textinput")),
              shiny::uiOutput(ns("ui_dbname_textinput"))
            )
          )
        )
      )
    )

    # Database logo
    output$db_logo <- shiny::renderUI({
      path = paste0('www/', attr(class(server$con), "package"), '.svg')
      return(shiny::img(src = path, width = "100%"))
    })

    # Header 2
    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel(txt[16])
    })

    # Header 3
    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel(txt[17])
    })

    ### Cleanup routine
    cancel.onSessionEnded <- session$onSessionEnded(function() {
      shiny::reactive(DBI::dbDisconnect(server$con))
    })
  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
