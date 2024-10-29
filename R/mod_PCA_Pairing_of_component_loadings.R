#' PCA-Pairing_of_component_loadings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_Pairing_of_component_loadings_ui <- function(id){
  ns <- NS(id)
  
  ## 3. Pairing of component loadings
  
  ##
  shiny::tabPanel(
    # Title of the tab panel
    shiny::uiOutput(ns("ui_tab_title_damping")),
    # Fluid page
    shiny::fluidPage(
      
      # Header 4
      shiny::uiOutput(ns("ui_header4")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_6(
            shiny::plotOutput(
              ns("ui_circle_plot"),
              click = ns("circle_click"),
              width = "100%",
              height = "100%"),
          ), col_6(
            shiny::plotOutput(ns("ui_damping_plot"))
          )
        )
      ),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("ui_damping_pc"))
          )
        )
      )
    )
  )
}
    
#' PCA-Pairing_of_component_loadings Server Functions
#'
#' @noRd 
mod_PCA_Pairing_of_component_loadings_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 1. Reactive values
    
    pca_pairing_server = shiny::reactiveValues(
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table")
    )
    
    # 2. Reactives
    
    pc_names = shiny::reactive(
      paste0("PC", 1:(ncol(r$ts)-1))
    )
    
    damping = shiny::reactive({
      if (is.null(input$damping_pc)) return(NULL)
      plotdf = data.frame(
        CL1 = r$pca$loadings[,"PC1"],
        CL2 = r$pca$loadings[,input$damping_pc],
        id = rownames(r$pca$loadings)
      )
      plotdf$damping_coef = round(atan(plotdf$CL2/plotdf$CL1) * 180/pi, 2)
      plotdf$group = group(
        pt = pca_pairing_server$primary_table,
        dgt = pca_pairing_server$datagroup_table,
        hashs = plotdf$id
      )

      plotdf$color = color(
        pt = get.table(r$db, "primary_table"),
        dgt = get.table(r$db, "datagroup_table"),
        hashs = plotdf$id
      )
      return(plotdf)
    })
    
    pc_names_eigen = shiny::reactive(
      pc_names()[shiny::isolate(r$pca$eigenvalues)>1]
    )
    
    circle_plot_df = shiny::reactive({
      if (is.null(input$damping_pc)) return(NULL)
      plotdf = r$pca$loadings[,c("PC1", input$damping_pc)]
      colnames(plotdf) = c("PCA", "PCB")
      plotdf$id = rownames(r$pca$loadings)
      plotdf$group = group(
        pt = pca_pairing_server$primary_table,
        dgt = pca_pairing_server$datagroup_table,
        hashs = plotdf$id
      )
      plotdf$color = color(
        pt = get.table(r$db, "primary_table"),
        dgt = get.table(r$db, "datagroup_table"),
        hashs = plotdf$id
      )

      if (!is.null(pca_pairing_server$selected_id)) {
        plotdf[plotdf$id == pca_pairing_server$selected_id, "color"] = "red"
      }

      return(plotdf)
    })

    circle_plot = shiny::reactive({
      p = ggplot2::ggplot() +
        ggplot2::geom_point(
          data = circle_plot_df(),
          colour = circle_plot_df()$color,
          cex = 4,
          ggplot2::aes(
            x = PCA,
            y = PCB,
            pch = group)
        ) +
        ggplot2::geom_path(
          data = circle_fun(c(0,0), 2, npoints = 100),
          ggplot2::aes(x, y)
        ) +
        ggplot2::coord_fixed(
          ratio = 1,
          xlim = c(-1, 1),
          ylim = c(-1, 1)
        ) +
        ggplot2::xlab(paste(r$txt[[92]], "PC1")) +
        ggplot2::ylab(paste(r$txt[[92]], input$damping_pc)) +
        ggplot2::theme_minimal()
      return(p)
    })
    
    boxplot_loading = shiny::reactive({
      if (is.null(damping())) return(NULL)
      p = ggplot2::ggplot() +
        ggplot2::geom_boxplot(
          data = damping(),
          ggplot2::aes(x = group, y = damping_coef)
        ) +
        ggplot2::theme_minimal()
      if (!is.null(pca_pairing_server$selected_id)) {
        p = p + ggplot2::geom_point(
          data = damping()[damping()$id == pca_pairing_server$selected_id,],
          ggplot2::aes(x = group, y = damping_coef)
        )
      }
      return(p)
    })
    
    # 3. Observers
    
    observeEvent(input$circle_click, {
      pca_pairing_server$selected_id = shiny::nearPoints(
        circle_plot_df(),
        input$circle_click,
        addDist = TRUE
      )[,3]
    })
    
    # observeEvent(circle_plot(), {
    #   r$plots[["circle_plot"]] = format_plot(circle_plot())
    # })
    
    # # observeEvent(boxplot_loading(), {
    # #   r$plots[["boxplot_loading"]] = format_plot(boxplot_loading())
    # # })
    
    # 4. UI elements
    
    output$ui_damping_plot = shiny::renderPlot(
      boxplot_loading(), width = 500, height = 500
    )

    output$ui_damping_pc = shiny::renderUI(
      shiny::selectInput(
        inputId = ns("damping_pc"),
        label = r$txt[[87]],
        choices = setdiff(pc_names(), "PC1")
      )
    )
    
    output$ui_circle_plot = shiny::renderPlot({
      if (is.null(circle_plot_df())) return(NULL)
      circle_plot()
    }, height = 500, width = 500)
    
    output$ui_header4 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[91]])
    })
    
    output$ui_tab_title_damping <- shiny::renderText(r$txt[91])
 
  })
}
    
## To be copied in the UI
# mod_PCA_Pairing_of_component_loadings_ui("PCA-Pairing_of_component_loadings_1")
    
## To be copied in the server
# mod_PCA_Pairing_of_component_loadings_server("PCA-Pairing_of_component_loadings_1")
