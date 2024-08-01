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
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE,
            shiny::uiOutput(ns("ui_cleardb_button"))
          )
        )
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

mod_database_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive functions

    ## Initial connection routine
    init_connect = reactive({
      # By default test postgres connection
      tryCatch(
         expr = {
          # Fetch credentials
          cred = credentials()
          # Connect as superuser
          db = connect.database(instantiatePostgreSQL(cred, superuser = TRUE))
          # Check if normal user exists
          user_exists = getElement(cred, "user") %in% database.users(db)$usename
          # Create new user if user does not exist
          if (!(user_exists)) {
            create.user(
              d = db,
              newUser = getElement(cred, "user"),
              password = getElement(cred, "password")
            )
          }
          # Check if schema for user exists
          schema_exists = getElement(cred, "user") %in% database.schemas(db)
          # Create new user schema if user schema does not exist
          if (!(schema_exists)) {
            create.schema(
              d = db,
              user = getElement(cred, "user")
            )
          }
          # Then disconnect
          DBI::dbDisconnect(db@con)
          # And reconnect as user
          db = connect.database(instantiatePostgreSQL(credentials()))
          # Return db object
          return(db)
        },
        # If postgres connection fails return SQLite connection
        error = function(e) connect.database(instantiateSQLite())
      )
    })

    ## Ordinary database connection routine via try catch
    connect = reactive({
      # If input$dbtype is PostgreSQL
      if (input$dbtype == "RPostgres") {
        tryCatch(
          # Try connecting to PostgreSQL
          connect.database(instantiatePostgreSQL(credentials())),
          # Otherwise connect to SQLite
          error = function(e) connect.database(instantiateSQLite())
        )
      }
      # If input$dbtype is SQLite
      else if (input$dbtype == "RSQLite") {
        connect.database(instantiateSQLite())
      }
    })

    ## Fetch credentials from environment
    credentials = reactive({
      # Eventually fetch user defined credentials. Therefore the corresponding
      # UI elements have to be rendered. This is especially at the initial
      # attempt to conect to the database the case.
      # Check if UI elements are available
      ui_check = all(shiny::isTruthy(
        c(input$host, input$user, input$password, input$port, input$dbname))
      )
      if (ui_check) {
        cred = c(
          host = input$host,
          user = input$user,
          password = input$password,
          port = input$port,
          dbname = input$dbname
        )
      # If no userdefined credentials are available obtain credentials from
      # default app internals or even better environmental variables.
      } else {
        # At first just fetch default credentials. These credentials will be
        # updated inside this function. The internal variable 'acc' contains
        # some generic credentials for an appropriate connection attempt.
        cred = internal$acc
        # Check if alternatively host is available from env
        if ("DBIP" %in% names(r$ENV)) {
          cred["host"] <- getElement(
            object = r$ENV,
            name = "DBIP"
          )
        }
        # Check if alternatively username is available from env. This might
        # be the environmental variable SHINYPROXY_USERNAME or USERNAME. This
        # depends on the environment the application is launched in.
        if ("SHINYPROXY_USERNAME" %in% names(r$ENV)) {
          cred["user"] <- getElement(
            object = r$ENV,
            name = "SHINYPROXY_USERNAME"
          )
        } else if ("USERNAME" %in% names(r$ENV)) {
          cred["user"] <- getElement(
            object = r$ENV,
            name = "USERNAME"
          )
        }
        # Check if alternatively password is available from env. The name
        # of the environmental password variable depends on the cred user.
        if (paste0(cred["user"], "_db_password") %in% names(r$ENV)) {
          cred["password"] <- getElement(
            object = r$ENV,
            name = paste0(cred["user"], "_db_password")
          )
        }
        # Return updated credentials
        return(cred)
      }
    })

    # Serverlogic

    ## Initial connection once
    r$db <- shiny::isolate(expr = init_connect())

    ## Connect to database
    shiny::observeEvent(
      eventExpr = input$dbtype,
      handlerExpr = {
        # At first disconnect
        DBI::dbDisconnect(r$db@con)
        # Connect selected database type
        r$db = connect()
      }
    )

    ## On changes regarding the connected database stored in r$db
    ## make primary_table and datagroup_table globally available
    shiny::observeEvent(
      eventExpr = r$db,
      handlerExpr = {
        # Share primary_table
        r$primary_table = get.table(d = r$db, tablename = "primary_table")
        # Share datagroup_table
        r$datagroup_table = get.table(d = r$db, tablename = "datagroup_table")
      }
    )


    observeEvent(input$clear_button, {
      clear.db(r$db)
      session$reload()
    })


    ## Cleanup routine on session ending
    cancel.onSessionEnded <- session$onSessionEnded(function() {
      shiny::reactive(DBI::dbDisconnect(r$db@con))
    })

    # UI Elements

    ## Tab title
    output$ui_tab_title <- shiny::renderUI(
      expr = shiny::renderText(r$txt[2])
    )

    ## Header 1
    output$ui_header1 <- shiny::renderUI(
      expr = shiny::titlePanel(r$txt[18])
    )

    ## Database system radiobuttons
    output$ui_dbtype_radiobutton <- shiny::renderUI(
      expr = {
        valid_dbms = c(PostgreSQL = "RPostgres", SQLite = "RSQLite")
        ui = shinyThings::radioSwitchButtons(
          # RadioSwitchButtons parameters
          inputId = ns("dbtype"),
          label = NULL,
          choice_labels = names(valid_dbms),
          choices = unname(valid_dbms),
          selected = attr(class(r$db@con), "package")
        )
        return(ui)
      }
    )

    ## Host text input
    output$ui_host_textinput <- shiny::renderUI(
      expr = shiny::textInput(
        # Text input parameters
        inputId = ns("host"),
        label = r$txt[9],
        value = getElement(credentials(), "host")
      )
    )

    ## Port text input
    output$ui_port_textinput <- shiny::renderUI(
      expr = shiny::textInput(
        # Text input parameters
        inputId = ns("port"),
        label = r$txt[8],
        value = getElement(credentials(), "port")
      )
    )

    ## User text input
    output$ui_user_textinput <- shiny::renderUI(
      expr = shiny::textInput(
        # Text input parameters
        inputId = ns("user"),
        label = r$txt[6],
        value = getElement(credentials(), "user")
      )
    )

    ## Password text input
    output$ui_password_textinput <- shiny::renderUI(
      expr = shiny::passwordInput(
        # Text input parameters
        inputId = ns("password"),
        label = r$txt[7],
        value = getElement(credentials(), "password")
      )
    )

    # Database name text input
    output$ui_dbname_textinput <- shiny::renderUI(
      expr = shiny::textInput(
        # Text input parameters
        inputId = ns("dbname"),
        label = r$txt[2],
        value = getElement(credentials(), "dbname")
      )
    )

    # Connection tabbox
    output$ui_connection_box <- shiny::renderUI(
      expr = shinydashboard::tabBox(
        # TabBox parameters
        id = ns("connection_tabbox"),
        title = "",
        width = "100%",
        # TabBox panel 1
        shiny::tabPanel(
          # TabPanel parameters
          title = r$txt[22],
          # TabPanel content
          shiny::uiOutput(ns("ui_dbtype_radiobutton"))
        ),
        # TabBox panel 2
        shiny::tabPanel(
          # TabPanel parameters
          title = r$txt[15],
          # TabPanel content
          shiny::fluidRow(
            # FluidRow content
            col_6(
              # Column content
              shiny::uiOutput(outputId = ns("ui_host_textinput")),
              shiny::uiOutput(outputId = ns("ui_port_textinput")),
              shiny::uiOutput(outputId = ns("ui_user_textinput"))
            ),
            col_6(
              # Column content
              shiny::uiOutput(outputId = ns("ui_password_textinput")),
              shiny::uiOutput(outputId = ns("ui_dbname_textinput"))
            )
          )
        )
      )
    )

    # Database logo
    output$db_logo <- shiny::renderUI(
      expr = {
        # Paste image filepath
        path = paste0('www/', attr(class(r$db@con), "package"), '.svg')
        # Shiny image output
        img = shiny::img(
          # Image parameters
          src = path,
          width = "100%"
        )
        return(img)
      }
    )

    # Header 2
    output$ui_header2 <- shiny::renderUI(
      expr = shiny::titlePanel(
        # TitlePanel parameters
        title = r$txt[19]
      )
    )

    # Table of database users
    output$ui_users <- shiny::renderUI(
      expr = DT::renderDataTable(
        # DataTable content
        expr = database.users(r$db),
        # DataTable options
        options = list(scrollX = TRUE)
      )
    )

    # Table of database tables
    output$ui_tables <- shiny::renderUI(
      expr =  DT::renderDataTable(
        # DataTable content
        expr = database.tables(r$db),
        # DataTable options
        options = list(scrollX = TRUE)
      )
    )

    # Text with database schemas
    output$ui_schemas <- shiny::renderText(
      # Text
      expr = paste(database.schemas(r$db), collapse=", ")
    )

    # Database searchpath
    output$ui_searchpath <- shiny::renderText(
      # Text
      expr = gsub(
        pattern = '"user',
        replacement = paste0("user:", database.users(r$db)),
        x = database.searchpath(r$db)
      )
    )

    # Tabbox for database properties
    output$ui_properties_tabbox <- shiny::renderUI(
      expr = shinydashboard::tabBox(
        # TabBox parameters
        id = ns("properties_tabbox"),
        title = "",
        width = "100%",
        # TabBox panel 1
        shiny::tabPanel(
          # TabPanel parameters
          title = r$txt[21],
          # TabPanel content
          shiny::uiOutput(ns("ui_users"))
        ),
        # TabBox panel 2
        shiny::tabPanel(
          # TabPanel parameters
          title = r$txt[23],
          # TabPanel content
          shiny::uiOutput(ns("ui_tables"))
        ),
        # TabBox panel 3
        shiny::tabPanel(
          # TabPanel parameters
          title = r$txt[31],
          # TabPanel content
          shiny::textOutput(ns("ui_schemas")),
          shiny::textOutput(ns("ui_searchpath"))
        )
      )
    )

    # Header 3
    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel(r$txt[20])
    })

    output$ui_cleardb_button <- shiny::renderUI({
      shiny::actionButton(
        inputId = ns("clear_button"),
        label = r$txt[[62]]
      )
    })

  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
