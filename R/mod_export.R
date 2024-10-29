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
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_8(
            col_4(
              shiny::uiOutput(ns("download_images_button"))
            ),
            col_8(
              shiny::uiOutput(ns("img_parameter_box"))
            )
          )
        )
      )

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

    observeEvent(input$img_format, {
      print("Plots")
      print(r$plots)
    })

    # .. UI elements rendering

    ## Data

    output$data_header = shiny::renderUI({
      expr = shiny::titlePanel(r$txt[[101]])
    })

    output$download_data = shiny::downloadHandler(
      filename = function() {
        paste("output", "zip", sep=".")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        for (i in c(1,2,3,4,5)) {
          path <- paste0("sample_", i, ".csv")
          fs <- c(fs, path)
          write(i*2, path)
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
        collapsed = TRUE, collapsible = TRUE,
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
    
    # Download 

    output$download_images = shiny::downloadHandler(
      filename = function() {
        paste("output", "zip", sep=".")
      },
      content = function(fname) {
        tryCatch({
          fs <- c()
          tmpdir <- tempdir()
          setwd(tempdir())
          for (i in 1:length(r$plots)) {
            if (is.null(input$img_format)) {
              f = "png"
            } else {
              f = input$img_format
            }
            path <- paste0(names(r$plots)[[i]], ".", f)
            fs <- c(fs, path)
            w = NA
            h = NA
            u = c("in", "cm", "mm", "px")
            if (!is.null(input$img_width) & identical(input$img_format, "png")) {
              w = input$img_width 
              h = input$img_height
              u = input$img_unit
            }
            ggplot2::ggsave(path, plot = r$plots[[i]], width = w, height = h, units = u)
          }
          zip(zipfile=fname, files=fs)
        }, error = function(e) print(e))
      },
      contentType = "application/zip"
    )

    output$download_images_button = shiny::renderUI(
      shiny::downloadButton(ns("download_images"), label = r$txt[[102]])
    )

  })
}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
