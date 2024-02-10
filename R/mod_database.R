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

mod_database_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    col_6(
      shiny::h1(),
      shiny::verbatimTextOutput(ns("db_status")),
      shiny::uiOutput(ns('ui_host_textinput')),
      shiny::uiOutput(ns('ui_port_textinput')),
      shiny::uiOutput(ns('ui_user_textinput')),
      shiny::uiOutput(ns('ui_password_textinput')),
      shiny::uiOutput(ns('ui_dbname_textinput')),
      shiny::uiOutput(ns('ui_connect_button'))
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

mod_database_server <- function(id, r, txt){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Connect to database
    if (!exists("con")) {
      # Just connect
      con <- shiny::reactive(connect_db(host = input$host, port = input$port,
                                        user = input$user, dbname = input$dbname,
                                        password = input$password))
    } else {
      # Disconnect and reconnect
      DBI::dbDisconnect(con)
      con <- shiny::reactive(connect_db(host = input$host, port = input$port,
                                        user = input$user, dbname = input$dbname,
                                        password = input$password))
    }

    ### Trash code
    # # Write csv-file to database
    # shiny::observeEvent(r$ts_upload$upload, {
    #   if (!is.null(con) & length(r$ts_upload$upload) >= 1) {
    #     print(paste("Upload:", basename(r$ts_upload$upload)))
    #     for (path in r$ts_upload$upload) {
    #       DBI::dbWriteTable(conn = con,
    #                         name = tools::file_path_sans_ext(names(which(
    #                           r$ts_upload$upload == path))),
    #                         value = utils::read.csv(r$ts_upload$upload),
    #                         overwrite = TRUE)
    #       r$ts_upload$upload = r$ts_upload$upload[!r$ts_upload$upload == path]
    #     }
    #     output$db_tables <- renderText(DBI::dbListTables(con))
    #   }
    #   print(paste("Upload:", basename(r$ts_upload$upload)))
    # })

    ### Render UI Elements

    # Text output of connection status
    output$db_status <- shiny::renderText({summary(con())})

    # Database access parameters
    output$ui_host_textinput <- shiny::renderUI({
      shiny::textInput(ns("host"), label = txt[9], value = "localhost")
    })
    output$ui_port_textinput <- shiny::renderUI({
      shiny::textInput(ns("port"), label = txt[8], value = "5432")
    })
    output$ui_user_textinput <- shiny::renderUI({
      shiny::textInput(ns("user"), label = txt[6], value = "user")
    })
    output$ui_password_textinput <- shiny::renderUI({
      shiny::textInput(ns("password"), label = txt[7], value = "mysecretpassword")
    })
    output$ui_dbname_textinput <- shiny::renderUI({
      shiny::textInput(ns("dbname"), label = txt[2], value = "mydb")
    })

    # Cleanup routine
    cancel.onSessionEnded <- session$onSessionEnded(function() {
      DBI::dbDisconnect(con)
    })
  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
