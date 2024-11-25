#' PCA-PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_PCA_ui <- function(id){
  ns <- NS(id)
  
  ## 1. Visualisation of principal compolents (PCs)
  
  ## The first tabPanel shows the received PCs from the calculations as
  ## timeseries itself. Therefore it is easy to gain a first impression wether
  ## the PCA was successful or not. This is in particular assessable from
  ## the communalities plot which takes also place on the first tabPanel.
  
  shiny::tabPanel(
    shiny::uiOutput(ns("ui_PCA_title")),
    
    shiny::fluidPage(
      
      shiny::uiOutput(ns("ui_principal_components")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::plotOutput(ns("pcs"))
          )
        )
      ),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("pc_plot_select"))
          )
        )
      ),
      # Header 2
      shiny::uiOutput(ns("ui_communalities")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          plotOutput(ns("plotPercentofVariance"))
        ),
        shiny::fluidRow(
          shiny::uiOutput(ns("comm_plot_radioButton"))
        )
      ),
      # Header
      shiny::uiOutput(ns("ui_parameters")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("ui_flip_PCA"))
          )
        )
      )
    )
  )
}
    
#' PCA-PCA Server Functions
#'
#' @noRd 
mod_PCA_PCA_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    pcsplot = reactive({
      if (length(input$pc_plot_pcs)>=1) {
        df = r$pca$pcs[,c("timestamp", input$pc_plot_pcs)]
        df = tidyr::pivot_longer(data = df, cols = input$pc_plot_pcs)
        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x = timestamp, y = value, colour = name)) +
          ggplot2::geom_line() +
          ggplot2::theme_minimal()

        return(plot)
      }
    })
    
    ## comm_plot() function
    communalities = shiny::reactive({
      # Calculate percent of total variance
      pTV = r$pca$eigenvalues / sum(r$pca$eigenvalues) * 100

      # Make plotting df
      df = data.frame(PC = paste0(1:length(pTV), "."), ev = r$pca$eigenvalues,
                      pTV = pTV, csum = cumsum(pTV))
      df$PC <- factor(df$PC, levels = df$PC)
      
      return(df)
      
    })
    
    comm_plot = shiny::reactive({
      if (identical(input$comm_plot_type, r$txt[[121]])) {
        comm_plot_bar(communalities(), label = r$txt[[64]])
      } else if (identical(input$comm_plot_type, r$txt[[122]])) {
        comm_plot_stacked(communalities(), label = r$txt[[64]])
      }
    })
    
    pc_names = shiny::reactive(
      paste0("PC", 1:(ncol(r$ts)-1))
    )

    pc_names_eigen = shiny::reactive(
      pc_names()[shiny::isolate(r$pca$eigenvalues)>1]
    )
    
    # _. Observers
    
    shiny::observeEvent(is.null(input$flip_pca), {
      if (is.null(input$flip_pca)) {
        s = rep(FALSE, length(pc_names()))
      } else {
        s = pc_names() %in% input$flip_pca 
      }
      r$settings[["flip_pca"]] = s
      r$flip = s
    })
    
    # observeEvent(comm_plot(), {
    #   r$plots[["communalities"]] = format_plot(comm_plot())
    # })
    #
    
    # observeEvent(pcsplot(), {
    #   r$plots[["principal_components"]] = format_plot(pcsplot())
    # })
    
    # _. UI elements
    
    output$ui_PCA_title <- shiny::renderText(r$txt[[55]])
    
    output$ui_principal_components <- shiny::renderUI({
      shiny::titlePanel(r$txt[[65]])
    })

    output$ui_communalities <- shiny::renderUI({
      shiny::titlePanel(r$txt[[64]])
    })

    output$ui_parameters <- shiny::renderUI({
      shiny::titlePanel(r$txt[[85]])
    })
    
    output$plotPercentofVariance = shiny::renderPlot({
      comm_plot()
    })

    output$pcs = renderPlot({
      pcsplot()
    })

    output$pc_plot_select = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("pc_plot_pcs"),
        label = r$txt[[89]],
        choices = pc_names(),
        selected = pc_names_eigen(),
        inline = TRUE
      )
    )
    
    output$ui_flip_PCA = shiny::renderUI({
      shiny::checkboxGroupInput(
        inputId = ns("flip_pca"),
        label = r$txt[[84]],
        choices = pc_names(),
        selected = shiny::isolate(pc_names()[r$settings[["flip_pca"]]]),
        inline = TRUE
      )
      
    })
    
    output$comm_plot_radioButton = shiny::renderUI(
      shinyThings::buttonGroup(
        inputId = ns("comm_plot_type"),
        choices = c(r$txt[[121]], r$txt[[122]]),
        selected = r$txt[[121]]
      )
    )
    
  })
}
    
## To be copied in the UI
# mod_PCA_PCA_ui("PCA-PCA_1")
    
## To be copied in the server
# mod_PCA_PCA_server("PCA-PCA_1")
