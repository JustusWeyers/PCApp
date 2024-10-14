#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidPage(

      shiny::fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, width = 12,
          shiny::titlePanel("PCApp"),
          shiny::htmlOutput(ns("subtitle")),
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, width = "100%",
          col_6(
            shiny::htmlOutput(ns("pcapp_txt")),
          ),
          col_6(
            shiny::uiOutput(ns("info_plot")),
            shiny::uiOutput(ns("button"))
          )
        )
      ),

      shiny::fluidRow(
        shiny::uiOutput(ns("info_tabbox")),
      )
    )

  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    home_server = shiny::reactiveValues(
      data = internal$sampledata[,c(1, sample(2:31, 6))]
    )

    demoplot = function(timestamp, df) {
      colo =
      return({
        par(mfrow = c(6, 1), mar = c(0, 0, 0, 0))
        lapply(1:6, function(x) {
          plot(timestamp, df[,x], type = "l", bty = "n", ylab = "", yaxt = "n",
               col = "black",
               xlab = "", xaxt = "n")
        })
      })
    }

    demoplot_pc = function(timestamp, df) {
      colo =
        return({
          par(mfrow = c(6, 1), mar = c(0, 0, 0, 0))
          lapply(1:6, function(x) {
            plot(timestamp, df[,x], type = "l", bty = "n", ylab = "", yaxt = "n",
                 col = "black",
                 xlab = "", xaxt = "n", ylim = c(min(df[,1]), max(df[,1])))
          })
        })
    }

    observeEvent(input$button, {
      home_server$data = internal$sampledata[,c(1, sample(2:31, 6))]
    })

    output$sample_data_plot = shiny::renderPlot({
      demoplot(as.Date(home_server$data$timestamp), home_server$data[,2:7])
    })

    output$sample_pc_plot = shiny::renderPlot({
      pca = prcomp(home_server$data[,2:7])
      pc = pca$x
      cl = data.frame(pca$rotation %*% diag(sqrt(eigen(stats::cov(home_server$data[,2:7]))$values)))
      if (median(cl[,1]) < 0) {
        pc[,1] = pc[,1] * (-1)
      }
      return(demoplot_pc(as.Date(home_server$data$timestamp), pc))
    })

    output$pcapp_txt = shiny::renderUI({
      HTML(gsub("\n", "<br/>", gsub("\r", "", r$txt[["pcapp_info.txt"]])))
    })

    output$import_txt = shiny::renderUI({
      HTML(gsub("\n", "<br/>", gsub("\r", "", r$txt[["import_info.txt"]])))
    })

    output$select_txt = shiny::renderUI({
      HTML(gsub("\n", "<br/>", gsub("\r", "", r$txt[["select_info.txt"]])))
    })

    output$pca_txt = shiny::renderUI({
      HTML(gsub("\n", "<br/>", gsub("\r", "", r$txt[["pca_info.txt"]])))
    })

    output$export_txt = shiny::renderUI({
      HTML(gsub("\n", "<br/>", gsub("\r", "", r$txt[["export_info.txt"]])))
    })

    output$info_tabbox = shiny::renderUI({
      shinydashboard::tabBox(
        id = "tabset1", width = 12,
        shiny::tabPanel("Import", shiny::htmlOutput(ns("import_txt"))),
        shiny::tabPanel("Selection", shiny::htmlOutput(ns("select_txt"))),
        shiny::tabPanel("PCA", shiny::htmlOutput(ns("pca_txt"))),
        shiny::tabPanel("Export", shiny::htmlOutput(ns("export_txt")))
      )
    })

    output$info_plot = shiny::renderUI({
      shinydashboard::box(
        solidHeader = TRUE, width = 12,
        shiny::fluidRow(
          col_6(
            shiny::plotOutput(ns("sample_data_plot"), height = 300)
          ),
          col_6(
            shiny::plotOutput(ns("sample_pc_plot"), height = 300)
          )
        )
      )
    })

    output$button = shiny::renderUI({
      shiny::actionButton(inputId = ns("button"), label = r$txt[[99]])
    })


    output$subtitle = shiny::renderUI(
      HTML(r$txt[["An application for principal component analysis with time series"]])
    )

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
