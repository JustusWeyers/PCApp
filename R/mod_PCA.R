#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PCA_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    "PCA",
    shiny::tagList(
      shiny::fluidPage(
        # Header 1
        shiny::uiOutput(ns("ui_header1")),
        shinydashboard::box(
          width = "100%", solidHeader = TRUE,
          shiny::fluidRow(
            col_12(
              plotOutput(ns("pcs"))
            )
          )
        ),
        # Header 2
        shiny::uiOutput(ns("ui_header2")),
        shinydashboard::box(
          width = "100%", solidHeader = TRUE,
          shiny::fluidRow(
            col_12(
              plotOutput(ns("plotPercentofVariance"))
            )
          )
        )
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####

    # PCA server's reactive values
    pcaserver = reactiveValues()

    # Functions

    pcsplot = function(df) {
      df <- reshape2::melt(df, id.vars="timestamp")
      plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=timestamp, y=value, colour = variable)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()

      return(plot)
    }

    kommunalitaeten = function(eigenvalues){
      # Calculate percent of total variance
      pTV = eigenvalues / sum(eigenvalues) * 100

      # Make plotting df
      df = data.frame(PC = paste0(1:length(pTV), "."), ev = eigenvalues,
                      pTV = pTV, csum = cumsum(pTV))
      df$PC <- factor(df$PC, levels = df$PC)

      # Crop
      # df = df[1:(length(which(eigenvalues>=1))+1),]

      p = ggplot2::ggplot(df, ggplot2::aes(x = PC, y = csum)) +
        ggplot2::geom_bar(stat="identity", col = "black", fill = "white") +
        ggplot2::theme_minimal()

      return(p)
    }

    # Reactive functions

    prep_table = reactive({
      t = get.table(r$db, "selected_timeseries")
      t$timestamp = as.Date(t$timestamp)
      return(t)
    })

    z = reactive(
      apply(X = prep_table()[,2:ncol(prep_table())], FUN=scale, MARGIN = 2)
    )

    eigenvalues = reactive({
      eigen(cov(z()))$values
    })

    pca = reactive(
      prcomp(z(), center=TRUE, scale.=TRUE, retx=TRUE)
    )

    pcs = reactive({
      pcs_ = data.frame(pca()$x)
      pcs_$timestamp = prep_table()$timestamp
      return(pcs_)
    })



    # Server



    # UI

    output$plotPercentofVariance = renderPlot({
      kommunalitaeten(eigenvalues())
    })

    output$pcs = renderPlot({
      pcsplot(pcs())
    })

    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel("Principal components")
    })
    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel("Communalities")
    })

    ####

  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")
