#' PCA-PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList tabPanel fluidPage fluidRow uiOutput plotOutput
#' @importFrom shinydashboard box

mod_PCA_PCA_ui <- function(id){
  ns <- NS(id)
  
  ## 1. Visualization of principal components (PCs)
  
  ## The first tabPanel shows the received PCs from the calculations as  
  ## timeseries themself. This makes it easy to get a first impression of 
  ## whether the PCA was successful or not. This can also be seen in particular 
  ## from the communities plot.
  
  shiny::tabPanel(
    
    # Title of the tabPanel
    shiny::uiOutput(ns("ui_PCA_title")),
    
    # Fluid page setup
    shiny::fluidPage(
      
      # Title "Principal components"
      shiny::uiOutput(ns("ui_principal_components")),
      
      # FluidRow with plot of PCs
      shiny::fluidRow(
        col_12(
          shiny::uiOutput(ns("ui_pcs_plot"))
        )
      ),
      
      # Box containing check-boxes for PC selection
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("ui_plot_checkbox"))
          )
        )
      ),
      
      # Header "Communalities"
      shiny::uiOutput(ns("ui_communalities")),
      
      # FluidRow plot
      shiny::fluidRow(
        col_12(
          shiny::uiOutput(ns("ui_comm_plot"))
        )
      ),
      
      # FluidRow radioButton 
      shiny::fluidRow(
        col_12(
          shiny::uiOutput(ns("ui_comm_radioButton"))
        )
      ),
      
      # Header "PCA Parameters"
      shiny::uiOutput(ns("ui_parameters")),
      
      # Box with check-boxes to "flip" PCs
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("ui_flip_radioButton"))
          )
        )
      )
      
    )
  )
  
}
    
#' PCA-PCA Server Functions
#'
#' @noRd 
#'
#' @importFrom shiny reactive isolate observeEvent renderText renderUI 
#' @importFrom shiny titlePanel checkboxGroupInput
#' @importFrom shinyThings buttonGroup
#' @importFrom tidyr pivot_longer

mod_PCA_PCA_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # 1. Reactives
    
    ## Communalities
    communalities = shiny::reactive({
      # Calculate percent of total variance
      Pov = r$pca$eigenvalues / sum(r$pca$eigenvalues) * 100
      # Make plotting df
      df = data.frame(
        PC = paste0(1:length(Pov), "."), 
        ev = r$pca$eigenvalues,
        Pov = Pov, 
        csum = cumsum(Pov)
      )
      df$PC <- factor(df$PC, levels = df$PC)
      return(df)
    })
    
    # PCs plot reactive
    pcsplot = shiny::reactive({
      if (length(input$pc_plot_pcs) >= 1) {
        df = r$pca$pcs[,c("timestamp", input$pc_plot_pcs)]
        df = tidyr::pivot_longer(data = df, cols = input$pc_plot_pcs)
        return(plot_pcs(df))
      }
    })
    
    # Communalities plot reactive
    comm_plot = shiny::reactive({
      print(input$comm_plot_type)
      if (identical(input$comm_plot_type, "bar")) {
        comm_plot_bar(communalities(), label = r$txt[[64]])
      } else if (identical(input$comm_plot_type, "stacked")) {
        print("Stackkkkkked")
        comm_plot_stacked(communalities(), label = r$txt[[64]])
      }
    })
    
    # Names of PCs
    pc_names = shiny::reactive({
      paste0("PC", 1:(ncol(r$ts)-1))
    })

    # Names of PCs with eigenvalues > 1
    pc_names_eigen = shiny::reactive(
      pc_names()[shiny::isolate(r$pca$eigenvalues)>1]
    )
    
    # 2. Server functions
    
    # Observe which PCs shall be flipped
    shiny::observeEvent(is.null(input$flip_pca), {
      if (is.null(input$flip_pca)) {
        s = rep(FALSE, length(pc_names()))
      } else {
        s = pc_names() %in% input$flip_pca 
      }
      r$settings[["flip_pca"]] = s
      r$flip = s
    })
    
    
    # 3. UI elements
    
    # Title of the tabPanel
    output$ui_PCA_title <- shiny::renderText({
      r$txt[[55]]
    })
    
    # Title "Principal components"
    output$ui_principal_components <- shiny::renderUI({
      shiny::titlePanel(r$txt[[65]])
    })
    
    # Plot PCs
    output$ui_pcs_plot = shiny::renderUI({
      # Cretae PlotPanel-Object
      ppo = methods::new(
        "PlotPanel", 
        name = "principalcomponents", 
        caption = "Placeholder",
        fillins = c("none"),
        plot = list(pcsplot())
      )
      plotServer(ppo, r)
      ppo@name <- ns(ppo@name)
      return(plotUI(ppo)())
    })
    
    # Checkbox input to select PCs for plotting
    output$ui_plot_checkbox = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("pc_plot_pcs"),
        label = r$txt[[89]],
        choices = pc_names(),
        selected = pc_names_eigen(),
        inline = TRUE
      )
    )
    
    # Title "Communalities"
    output$ui_communalities <- shiny::renderUI({
      shiny::titlePanel(r$txt[[64]])
    })
    
    # Plot communalities
    output$ui_comm_plot = shiny::renderUI({
      # Cretae PlotPanel-Object
      ppo = methods::new(
        "PlotPanel", 
        name = "communalities", 
        caption = "Placeholder",
        fillins = c("none"),
        plot = list(comm_plot())
      )
      plotServer(ppo, r)
      ppo@name <- ns(ppo@name)
      return(plotUI(ppo)())
    })

    # Radio buttons to select communalities plot type
    output$ui_comm_radioButton = shiny::renderUI(
      shinyThings::buttonGroup(
        inputId = ns("comm_plot_type"),
        choices = c("bar", "stacked"),
        choice_labels = c(r$txt[[121]], r$txt[[122]]),
        selected = "bar"
      )
    )
    
    # Title "PCA parameters"
    output$ui_parameters <- shiny::renderUI({
      shiny::titlePanel(r$txt[[85]])
    })
    
    # Checkbox input to flip some PCs
    output$ui_flip_radioButton = shiny::renderUI({
      shiny::checkboxGroupInput(
        inputId = ns("flip_pca"),
        label = r$txt[[84]],
        choices = pc_names(),
        selected = shiny::isolate(pc_names()[r$settings[["flip_pca"]]]),
        inline = TRUE
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_PCA_PCA_ui("PCA-PCA_1")
    
## To be copied in the server
# mod_PCA_PCA_server("PCA-PCA_1")
