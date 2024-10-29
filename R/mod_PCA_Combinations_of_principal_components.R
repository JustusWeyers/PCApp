#' PCA_Combinations_of_principal_components UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_Combinations_of_principal_components_ui <- function(id){
  ns <- NS(id)
  
  ## 4. Combinations of principal components
  
  ## The first principal component usually covers a substantial portion of
  ## the overall variance of the examined timeseries. This behaviuor can be
  ## interpreted as "typical medium timeline". Therefore it can be interesting
  ## to combine the this PC with a PC of higher rank to analyse wether there
  ## is a noteworthy change in temporal patterns in the combined timeseries.
  ## In this section the user can decide, which timeseries he wants to add to
  ## the first principal component and view the changes in a plot.
  
  
  shiny::tabPanel(
    # Title of the tabPanel
    shiny::textOutput(ns("ui_tab_title_combinations")),
    
    # Fluid page
    shiny::fluidPage(
      # Header 6
      shiny::uiOutput(ns("ui_header6")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::plotOutput(ns("ui_combiplot")),
            shiny::uiOutput(ns("daterange_slider"))
          )
        )
      ),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_6(
            shiny::uiOutput(ns("ui_combi_pc"))
          ),
          col_6(
            shiny::uiOutput(ns("ui_weight_slider"))
          )
        )
      )
    )
  )
}
    
#' PCA-Combinations_of_principal_components Server Functions
#'
#' @noRd 
mod_PCA_Combinations_of_principal_components_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    combiplot = shiny::reactive({
      df = r$pca$pcs[,c("timestamp", "PC1")]
      df$PC1 = scale(df$PC1)
      df$combi = (1-input$weight) * df$PC1 + input$weight * scale(r$pca$pcs[,input$combi_pc])
      colnames(df) = c("timestamp", "PC1", "combi")

      i = lubridate::interval(input$date_combi_slider[1], input$date_combi_slider[2])
      df = subset(x = df, subset = lubridate::`%within%`(df$timestamp, i))

      p = ggplot2::ggplot(df, ggplot2::aes(x = timestamp)) +
        ggplot2::geom_line(ggplot2::aes(y = PC1), col = "grey") +
        ggplot2::geom_line(ggplot2::aes(y = combi), col = "#F8766D") +
        ggplot2::theme_minimal()
      return(p)
    })
    
    pc_names = shiny::reactive(
      paste0("PC", 1:(ncol(r$ts)-1))
    )
    
    first_date = shiny::reactive(min(r$ts$timestamp))

    last_date = shiny::reactive(max(r$ts$timestamp))
    
    # 3. Observers
    
    observeEvent(combiplot(), {
      r$plots[["combination_plot"]] = format_plot(combiplot())
    })
    
    # 4. UI elements
    
    output$ui_combiplot = shiny::renderPlot(
      combiplot()
    )
    
    output$ui_combi_pc = shiny::renderUI(
      shiny::selectInput(
        inputId = ns("combi_pc"),
        label = r$txt[[87]],
        choices = setdiff(pc_names(), "PC1")
      )
    )

    output$daterange_slider = shiny::renderUI(
      shiny::sliderInput(
        inputId = ns("date_combi_slider"),
        "Slider",
        min = first_date(),
        max = last_date(),
        value = c(first_date(), last_date()),
        width = "100%"
      )
    )

    output$ui_weight_slider = shiny::renderUI(
      shiny::sliderInput(
        inputId = ns("weight"),
        label = paste(r$txt[[90]], input$combi_pc),
        min = 0,
        max = 1,
        value = 1/sqrt(2),
        step = 1/(10*sqrt(2)),
        round	= 2
      )
    )
    
    output$ui_header6 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[88]])
    })

    output$ui_tab_title_combinations <- shiny::renderText(r$txt[86])
    
 
  })
}
    
## To be copied in the UI
# mod_PCA_Combinations_of_principal_components_ui("PCA_Combinations_of_principal_components_1")
    
## To be copied in the server
# mod_PCA_Combinations_of_principal_components_server("PCA_Combinations_of_principal_components_1")
