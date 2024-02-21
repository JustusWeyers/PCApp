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
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("ui_properties_tabbox")),
          )
        ),
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
#' @importFrom DBI dbDisconnect
#' @importFrom methods new
#' @importFrom shiny observeEvent renderUI textInput actionButton reactive
#' @importFrom shiny reactiveValues img tabPanel fluidRow uiOutput titlePanel
#' @importFrom shiny isTruthy renderTable
#' @importFrom shinyThings radioSwitchButtons
#' @importFrom shinydashboard tabBox
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

    ### Functions

    connect = function(dbms, access) {

      if (dbms == "RPostgres") {
        tryCatch({
          connect.database(new("PostgreSQL",
                               host = getElement(access, "host"),
                               port = getElement(access, "port"),
                               user = getElement(access, "user"),
                               password = getElement(access, "password"),
                               dbname = getElement(access, "dbname"))
          )
        },
        error = function(e) {
          return(connect.database(new("SQLite")))
        })
      } else {
        return(connect.database(new("SQLite")))
      }
    }

    ### Serverlogic

    # Module servers global variables
    server = shiny::reactiveValues(
      # Supported database types
      database_types = stats::setNames(c("RPostgres", "RSQLite"),
                                       c("PostgreSQL", "SQLite")),

      # Initially try Postgres connection
      db = connect(dbms = "RPostgres", access = default_database_access)
    )

    # Connect to database functionality
    shiny::observeEvent(input$dbtype, {

      # At first disconnect
      DBI::dbDisconnect(server$db@con)

      # Eventually fetch user defined database access parameters
      if (all(shiny::isTruthy(c(input$host, input$user, input$password, input$port, input$dbname)))) {
        acc = stats::setNames(c(input$host, input$user, input$password, input$port, input$dbname),
                              c("host", "user", "password", "port", "dbname"))
      } else {
        acc = default_database_access
      }

      # Connect selected database type
      server$db = connect(dbms = input$dbtype, access = acc)

      create.user(server$db, newUser = "newbie", password = "1234")
    })

    ### UI Elements

    # Tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(txt[2])
    })

    # Header 1
    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel(txt[18])
    })

    # Database system radiobuttons
    output$ui_dbtype_radiobutton <- shiny::renderUI(
      shinyThings::radioSwitchButtons(
        ns("dbtype"),
        label = NULL,
        choice_labels = names(server$database_types),
        choices = unname(server$database_types),
        selected = attr(class(server$db@con), "package")
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
        id = ns("connection_tabbox"), title = "", width = "100%",
        shiny::tabPanel(
          txt[22],
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
      path = paste0('www/', attr(class(server$db@con), "package"), '.svg')
      return(shiny::img(src = path, width = "100%"))
    })

    # Header 2
    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel(txt[19])
    })

    # Table of database users
    output$ui_users <- shiny::renderUI({
      shiny::renderTable(database.users(server$db), bordered = TRUE)
    })

    # Tabbox for database properties
    output$ui_properties_tabbox <- shiny::renderUI(
      shinydashboard::tabBox(
        id = ns("properties_tabbox"), title = "", width = "100%",
        shiny::tabPanel(
          txt[21],
          shiny::uiOutput(ns("ui_users"))
        )
      )
    )

    # Header 3
    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel(txt[20])
    })

    ### Cleanup routine
    cancel.onSessionEnded <- session$onSessionEnded(function() {
      shiny::reactive(DBI::dbDisconnect(server$db@con))
    })

  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
