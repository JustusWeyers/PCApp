#' mod_database UI Function
#'
#' @description Create the settings tab regarding database issues
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage uiOutput fluidRow

mod_database_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      # Header
      shiny::uiOutput(ns("ui_header")),
      # Body
      shiny::fluidRow(
        col_10(
          shiny::uiOutput(ns("ui_dbtype_radiobutton")),
          col_6(
            shiny::uiOutput(ns("ui_host_textinput")),
            shiny::uiOutput(ns("ui_port_textinput")),
            shiny::uiOutput(ns("ui_user_textinput"))
          ),
          col_6(
            shiny::uiOutput(ns("ui_password_textinput")),
            shiny::uiOutput(ns("ui_dbname_textinput"))
          )
        ),
        col_2(
          shiny::htmlOutput(ns("db_logo"))
        )
      ),
      shiny::uiOutput(ns("ui_connect_actionbutton"))
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
#' @importFrom shiny reactiveValues
#' @importFrom shinyThings radioSwitchButtons
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
      # Connection in spe
      con = connect_database(
        type = "RPostgres",
        host = getElement(default_database_access, "host"),
        port = getElement(default_database_access, "port"),
        user = getElement(default_database_access, "user"),
        dbname = getElement(default_database_access, "dbname"),
        password = getElement(default_database_access, "password")
      )
    )

    # Connect to database
    shiny::observeEvent(input$connect, {
      # At first disconnect
      DBI::dbDisconnect(server$con)

      # Then reconnect
      server$con = connect_database(
        type = input$dbtype,
        host = input$host,
        port = input$port,
        user = input$user,
        dbname = input$dbname,
        password = input$password
      )
    })

    ### UI Elements

    # Database system radiobuttons
    output$ui_dbtype_radiobutton <- shiny::renderUI(
      shinyThings::radioSwitchButtons(
        ns("dbtype"),
        label = txt[14],
        choices = server$database_types,
        selected = attr(class(server$con), "package")
      )
    )

    # Header
    output$ui_header <- shiny::renderUI({
      shiny::titlePanel(txt[11])
    })

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

    # Connect actionbutton
    output$ui_connect_actionbutton <- shiny::renderUI(
      shiny::actionButton(ns("connect"), txt[10])
    )

    # Database logo
    output$db_logo <- shiny::renderUI({
      path = paste0('www/', attr(class(server$con), "package"), '.svg')
      return(img(src = path, width = "100%"))
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
