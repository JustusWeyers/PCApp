#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_ui <- function(id){
  # Namespace
  ns <- shiny::NS(id)

  # UI

  ##
  ##
  ##

  ## --- Justus Weyers 14.10.2024

  # Fluid page
  shiny::tagList(
    shiny::fluidPage(

      # Data export

      ##
      ##
      ##

      ## Title of the section
      shiny::uiOutput(ns("data_header")),

      ##
      ##
      ##
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("checkboxarray"))
          )
        ),
        shiny::fluidRow(
          col_6(
            shiny::uiOutput(ns("download_data_button"))
          ),
          col_6(

          )
        )
      ),

      # Image export

      ## Title of the section
      shiny::uiOutput(ns("images_header")),

      ##
      ##
      ##
      shiny::uiOutput(ns("img_parameter_box"))

    )
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # .. Server logic via observers

    observeEvent(c(input$img_format, input$img_unit, input$img_width, input$img_height), {
      r$settings[["img_format"]] = input$img_format
      r$settings[["img_unit"]] = input$img_unit
      r$settings[["img_width"]] = input$img_width
      r$settings[["img_height"]] = input$img_height
    })
    
    # Download
    
    # .. UI elements rendering

    ## Data
    

    
    internal_tables = shiny::reactive(
      intersect(user.tables(r$db)$tablename, c("primary_table", "datagroup_table", "settings", "selection_table", "selected_timeseries"))
    )
    
    pca_tables = shiny::reactive(
      intersect(user.tables(r$db)$tablename, c("principal_components", "scaled_timeseries_table", "loadings", "timeseries_table", "reference_hydrographs"))
    )
    
    readin_tables = shiny::reactive(
      setdiff(user.tables(r$db)$tablename, c(internal_tables(), pca_tables()))
    )
    
    output$internal_checkboxes = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("internal_checkboxes"),
        label = NULL,
        choices = internal_tables()
      )
    )
    
    output$pca_checkboxes = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("pca_checkboxes"),
        label = NULL,
        choices = pca_tables()
      )
    )
    
    output$readin_checkboxes = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("readin_checkboxes"),
        label = NULL,
        choices = readin_tables()
      )
    )
    
    output$checkboxarray = shiny::renderUI({
      shiny::fluidRow(
        col_6(
          shinydashboard::box(
            title = "Readin", width = "100%",
            collapsed = TRUE, collapsible = TRUE,
            shiny::uiOutput(ns("readin_checkboxes"))
          ),
          shinydashboard::box(
            title = "PCA", width = "100%",
            collapsed = TRUE, collapsible = TRUE,
            shiny::uiOutput(ns("pca_checkboxes"))
          ),
          shinydashboard::box(
            title = "Internal", width = "100%",
            collapsed = TRUE, collapsible = TRUE,
            shiny::uiOutput(ns("internal_checkboxes"))
          )
        )
      )
    })
    
    
    output$data_header = shiny::renderUI({
      expr = shiny::titlePanel(r$txt[[101]])
    })
    
    available_tables = shiny::reactive({
      c(input$internal_checkboxes, input$readin_checkboxes, input$pca_checkboxes)
    })

    output$download_data = shiny::downloadHandler(
      filename = function() {
        paste("output", "zip", sep=".")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())

        print(available_tables())
        
        for (tblnm in c(available_tables())) {
          path <- paste0(tblnm, ".csv")
          fs <- c(fs, path)
          write.csv(get.table(r$db, tblnm), path, row.names = FALSE)
        }
        zip(zipfile=fname, files=fs)
      },
      contentType = "application/zip"
    )

    output$download_data_button = shiny::renderUI(
      downloadButton(ns("download_data"), label = r$txt[[102]])
    )

    ## Images

    output$images_header = shiny::renderUI({
      expr = shiny::titlePanel(r$txt[[103]])
    })

    output$image_format = shiny::renderUI({
      shiny::selectInput(
        inputId = ns("img_format"),
        label = r$txt[[106]],
        choices = c("png", "svg"),
        selected = "png",
      )
    })
    
    output$image_unit = shiny::renderUI({
      if (identical(input$img_format, "png")) {
        shiny::selectInput(
          inputId = ns("img_unit"),
          label = r$txt[[120]],
          choices = c("in", "cm", "mm", "px"),
          selected = "px",
        )
      }
    })

    output$image_width = shiny::renderUI({
      if (identical(input$img_format, "png")) {
        shiny::numericInput(
          inputId = ns("img_width"),
          label = r$txt[[104]],
          value = 1920,,
          min = 100,
          max = 3000,
          step = 1
        )
      }
    })

    output$image_height = shiny::renderUI({
      if (identical(input$img_format, "png")) {
        shiny::numericInput(
          inputId = ns("img_height"),
          label = r$txt[[105]],
          value = 1200,
          min = 100,
          max = 3000,
          step = 1
        )
      }
    })

    output$img_parameter_box = shiny::renderUI({
      shinydashboard::box(
        width = "100%", title = r$txt[[36]],
        col_3(
          shiny::uiOutput(ns("image_format")),
        ),
        col_3(
          shiny::uiOutput(ns("image_unit")),
        ),
        col_3(
          shiny::uiOutput(ns("image_width")),
        ),
        col_3(
          shiny::uiOutput(ns("image_height"))
        )
      )
    })

  })
}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
