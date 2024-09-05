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

  shiny::tabsetPanel(
    type = "tabs",

    shiny::tabPanel(
      shiny::textOutput(ns("ui_tab_title_pca")),
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
          ),
          # Header 3
          shiny::uiOutput(ns("ui_header3")),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                tableOutput(ns("loadings"))
              )
            )
          )
        )
      )
    ),

    shiny::tabPanel(
      shiny::textOutput(ns("ui_tab_title_loadings")),
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

    get_timeseries = function(db) {
      if ("selected_timeseries" %in% user.tables(db)$tablename) {
        t = get.table(db, "selected_timeseries")
        t$timestamp = as.Date(t$timestamp)
        return(t)
      } else {
        return(data.frame())
      }
    }

    # PCA server's reactive values
    pcaserver = reactiveValues(
      ts = get_timeseries(shiny::isolate(r$db))
    )

    # Functions

    pcsplot = reactive({
      # df <- reshape2::melt(df, id.vars="timestamp")
      df = tidyr::pivot_longer(data = pcs(), cols = colnames(pca()$x))
      plot <- ggplot2::ggplot(data=df, ggplot2::aes(x = timestamp, y = value, colour = name)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()

      return(plot)
    })

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

    z = reactive(
      apply(X = pcaserver$ts[,2:ncol(pcaserver$ts)], FUN=scale, MARGIN = 2)
    )

    eigenvalues = reactive({
      eigen(stats::cov(z()))$values
    })

    pca = reactive(
      stats::prcomp(z(), center=TRUE, scale.=TRUE, retx=TRUE)
    )

    pcs = reactive({
      pcs_ = data.frame(pca()$x)
      pcs_$timestamp = pcaserver$ts$timestamp
      return(pcs_)
    })

    loadings = reactive({
      CL = data.frame(pca()$rotation %*% diag(sqrt(eigenvalues())))
      colnames(CL) = paste0("PC", seq(1, length(eigenvalues())))
      rownames(CL) = rownames(pca()$rotation)
      return(CL)
    })

    geo_loadings = reactive({
      print("### r #########")

      loc = fetch_metadata(r$metadata, rownames(loadings()), c("name", "latitude", "longitude"))
      data = merge(loc, loadings(), by.x= "hash", by.y = "row.names")
      # names(data)[1] <- "hash"
      # loc = st_as_sf()

      print(data)

      return(data)
    })

    output$loadings = renderTable(geo_loadings()) # loadings(), rownames = TRUE)



    # Server

    observeEvent(r$cache_selection_trigger, {
      print("pca server observed cache")
      pcaserver$ts = get_timeseries(r$db)
    })

    # UI

    output$plotPercentofVariance = renderPlot({
      kommunalitaeten(eigenvalues())
    })

    output$pcs = renderPlot({
      pcsplot()
    })

    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel("Principal components")
    })

    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel("Communalities")
    })

    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel("Loadings")
    })

    output$ui_tab_title_pca <- shiny::renderText(r$txt[55])

    output$ui_tab_title_loadings <- shiny::renderText(r$txt[63])

    ####

  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")
