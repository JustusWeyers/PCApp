#' mod_database UI Function
#'
#' @description Create the settings tab regarding database issues
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList verbatimTextOutput
#' @importFrom DBI dbWriteTable dbListTables dbDisconnect
#' @importFrom utils read.table
#' @importFrom shinyThings radioSwitchButtons

mod_database_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    col_6(
      shiny::h1(),
      shiny::htmlOutput(ns("db_logo")),
      shiny::uiOutput(ns("ui_dbtype_radiobutton")),
      shiny::uiOutput(ns("ui_connect_actionbutton")),
      shiny::verbatimTextOutput(ns("db_status")),
      shiny::uiOutput(ns("ui_host_textinput")),
      shiny::uiOutput(ns("ui_port_textinput")),
      shiny::uiOutput(ns("ui_user_textinput")),
      shiny::uiOutput(ns("ui_password_textinput")),
      shiny::uiOutput(ns("ui_dbname_textinput")),
      shiny::uiOutput(ns("ui_connect_button"))
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
#' @importFrom shiny renderText observeEvent renderUI textInput actionButton
#' @importFrom shiny reactive
#' @importFrom DBI dbDisconnect

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
    observeEvent(input$connect, {
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

    # Text output of connection status
    output$db_status <- shiny::renderText({
      summary(server$con)
    })


    # Database access parameters
    output$ui_dbtype_radiobutton <- shiny::renderUI(
      shinyThings::radioSwitchButtons(
        ns("dbtype"),
        label = txt[2],
        choices = server$database_types,
        selected = attr(class(server$con), "package")
      )
    )

    output$ui_connect_actionbutton <-
      shiny::renderUI(shiny::actionButton(ns("connect"), txt[10]))

    output$ui_host_textinput <-
      shiny::renderUI({
        shiny::textInput(ns("host"),
                         label = txt[9],
                         value = default_database_access["host"])
      })
    output$ui_port_textinput <-
      shiny::renderUI({
        shiny::textInput(ns("port"),
                         label = txt[8],
                         value = default_database_access["port"])
      })
    output$ui_user_textinput <-
      shiny::renderUI({
        shiny::textInput(ns("user"),
                         label = txt[6],
                         value = default_database_access["user"])
      })
    output$ui_password_textinput <-
      shiny::renderUI({
        shiny::textInput(ns("password"),
                         label = txt[7],
                         value = default_database_access["password"])
      })
    output$ui_dbname_textinput <-
      shiny::renderUI({
        shiny::textInput(ns("dbname"),
                         label = txt[2],
                         value = default_database_access["dbname"])
      })

    output$db_logo <- shiny::renderUI({
      path = paste0('www/', attr(class(server$con), "package"), '.svg')
      img(src = path, height = 50)
    })

    # Cleanup routine
    cancel.onSessionEnded <- session$onSessionEnded(function() {
      shiny::reactive(DBI::dbDisconnect(con()))
    })
  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
