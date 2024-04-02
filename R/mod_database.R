#' mod_database UI Function
#'
#' @description Create the settings tab regarding database issues
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel uiOutput  tagList fluidPage fluidRow htmlOutput
#' @importFrom DT renderDataTable

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
#' @importFrom DT renderDataTable
#' @importFrom methods new
#' @importFrom shiny observeEvent renderUI textInput reactive
#' @importFrom shiny reactiveValues img tabPanel fluidRow uiOutput titlePanel
#' @importFrom shiny isTruthy renderTable moduleServer isolate renderText
#' @importFrom shiny passwordInput textOutput
#' @importFrom shinyThings radioSwitchButtons
#' @importFrom shinydashboard tabBox

mod_database_server <- function(id, r, txt) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Functions

    ## Fetch credentials from environment
    fetch_credentials = function(ENV = shiny::isolate(r$ENV), acc = internal$acc) {

      # Check if alternatively host is available from env
      if ("DBIP" %in% names(ENV)) {
        acc["host"] <- getElement(ENV, "DBIP")
      }

      # Check if alternatively username is available from env
      if ("SHINYPROXY_USERNAME" %in% names(ENV)) {
        acc["user"] <- getElement(ENV, "SHINYPROXY_USERNAME")
      } else if ("USERNAME" %in% names(ENV)) {
        acc["user"] <- getElement(ENV, "USERNAME")
      }

      # Check if alternatively password is available from env
      if (paste0(acc["user"], "_db_password") %in% names(ENV)) {
        acc["password"] <- getElement(ENV, paste0(acc["user"], "_db_password"))
      }

      # print(acc)

      return(acc)
    }

    ## Initial connection routine
    init_connect = function(access) {
      # By default test postgres connection
      tryCatch({

        # Connect as superuser
        db = connect.database(
          new("PostgreSQL",
            host = getElement(access, "host"),
            port = getElement(access, "port"),
            user = getElement(access, "superuser"),
            password = getElement(access, "superpassword"),
            dbname = getElement(access, "dbname")
          )
        )

        # Create new user if user does not exist
        if (!(getElement(access, "user") %in% database.users(db)$usename)) {
          create.user(db, newUser = getElement(access, "user"),
                      password = getElement(access, "password"))
        }

        # Create new user schema if user schema does not exist
        if (!(getElement(access, "user") %in% database.schemas(db))) {
          create.schema(db, user = getElement(access, "user"))
        }

        # Then disconnect
        DBI::dbDisconnect(db@con)

        # And reconnect as user
        db = connect.database(
          new("PostgreSQL",
            host = getElement(access, "host"),
            port = getElement(access, "port"),
            user = getElement(access, "user"),
            password = getElement(access, "password"),
            dbname = getElement(access, "dbname")
          )
        )

        # Return db object
        return(db)
      },

      # If postgres connection fails return SQLite connection
      error = function(e) {
        db = connect.database(new("SQLite"))
        return(db)
      })

    }

    ## Ordinary database connection routine
    connect = function(dbms, access) {
      # Check desired DBMS
      if (dbms == "RPostgres") {
        tryCatch({
          db = connect.database(
            new("PostgreSQL",
                host = getElement(access, "host"),
                port = getElement(access, "port"),
                user = getElement(access, "user"),
                password = getElement(access, "password"),
                dbname = getElement(access, "dbname")
                )
            )
          return(db)
        },
        error = function(e) {
          db = connect.database(new("SQLite"))
          return(db)
        })

      } else {
        db = connect.database(new("SQLite"))
        return(db)
      }
    }

    # Serverlogic

    ## Fetch database access
    database_access = fetch_credentials()

    ## Module servers global variables
    server = shiny::reactiveValues(
      # Supported database types
      database_types = c(PostgreSQL = "RPostgres", SQLite = "RSQLite"),
      # The reactive database access parameters
      database_access = database_access,
      # Initial connection
      db = init_connect(access = database_access)
    )

    ## Connect to database
    shiny::observeEvent(input$dbtype, {
      # Eventually fetch user defined database access parameters
      if (all(shiny::isTruthy(c(input$host, input$user, input$password,
                                input$port, input$dbname)))) {
        acc = c(host = input$host,
                user = input$user,
                password = input$password,
                port = input$port,
                dbname = input$dbname
        )
      } else {
        acc = server$database_access
      }
      # At first disconnect
      DBI::dbDisconnect(server$db@con)
      # Connect selected database type
      server$db = connect(dbms = input$dbtype, access = acc)
    })

    ## Globally share db and primarytable
    observeEvent(server$db, {
      # Share db object
      r$db <- server$db
      # Share primary_table
      r$primary_table = get.table(server$db, tablename = "primary_table")
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
                         value = getElement(server$database_access, "host"))
    )

    # Port text input
    output$ui_port_textinput <- shiny::renderUI(
        shiny::textInput(ns("port"), label = txt[8],
                         value = getElement(server$database_access, "port"))
    )

    # User text input
    output$ui_user_textinput <- shiny::renderUI(
        shiny::textInput(ns("user"), label = txt[6],
                         value = getElement(server$database_access, "user"))
    )

    # Password text input
    output$ui_password_textinput <- shiny::renderUI(
        shiny::passwordInput(ns("password"), label = txt[7],
                             value = getElement(server$database_access, "password"))
    )

    # Database name text input
    output$ui_dbname_textinput <- shiny::renderUI(
        shiny::textInput(ns("dbname"), label = txt[2],
                         value = getElement(server$database_access, "dbname"))
    )

    # Connection tabbox
    output$ui_connection_box <- shiny::renderUI(shinydashboard::tabBox(
        id = ns("connection_tabbox"), title = "", width = "100%",
        # First tab
        shiny::tabPanel(
          txt[22],
          shiny::uiOutput(ns("ui_dbtype_radiobutton"))
        ),
        # Second tab
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

    observeEvent(r$db, {
      # Table of database users
      output$ui_users <- shiny::renderUI({
        DT::renderDataTable(database.users(server$db), options = list(scrollX = TRUE))
      })

      # Table of database tables
      output$ui_tables <- shiny::renderUI({
        DT::renderDataTable(database.tables(r$db), options = list(scrollX = TRUE))
      })

      # List of database schemas
      output$ui_schemas <- renderText(paste(database.schemas(server$db), collapse=", "))
    })

    # Database searchpath
    output$ui_searchpath <- renderText(gsub('"user', paste0("user:", server$db@user), database.searchpath(server$db)))

    # Tabbox for database properties
    output$ui_properties_tabbox <- shiny::renderUI(
      shinydashboard::tabBox(
        id = ns("properties_tabbox"), title = "", width = "100%",
        shiny::tabPanel(
          # style="max-height: 200px; overflow-x: scroll; overflow-y: scroll",
          txt[21],
          shiny::uiOutput(ns("ui_users"))
        ),
        shiny::tabPanel(
          # style="max-height: 200px; overflow-x: scroll; overflow-y: scroll",
          txt[23],
          shiny::uiOutput(ns("ui_tables"))
        ),
        shiny::tabPanel(
          # style="max-height: 200px; overflow-x: scroll; overflow-y: scroll",
          txt[31],
          shiny::textOutput(ns("ui_schemas")),
          shiny::textOutput(ns("ui_searchpath"))
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
