#' PCA-Correlations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_Correlations_ui <- function(id){
  ns <- NS(id)
  ## 6. Correlations between loadings and metadata (rasterdata)
  shiny::tagList(
    # Title of the tabPanel
    shiny::textOutput(ns("ui_tab_title_correlations")),
    # Fluid page
    shiny::tagList(
      shiny::fluidPage(
        
        shiny::uiOutput(ns("ui_header8")),
        
        fluidRow(
          col_12(
            shinydashboard::box(
              width = "100%", solidHeader = TRUE,
              shiny::fluidRow(
                col_12(
                  shiny::plotOutput(ns("ui_corr_plot"))
                )
              )
            )
          )
        ),
        
        fluidRow(
          col_12(
            shinydashboard::box(
              width = "100%", solidHeader = TRUE,
              shiny::fluidRow(
                col_12(
                  shiny::uiOutput(ns("ui_choose_corr"))
                  
                )
              )
            )
          )
        )
      )
    )
  )
}
    
#' PCA-Correlations Server Functions
#'
#' @noRd 
mod_PCA_Correlations_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    corplotdf = shiny::reactive({
      print("# --- Cor ---------")

      l = lapply(selected_metadata(), function(df)
        if(input$corr_data %in% colnames(df)) {
          return(df[,c("id", input$corr_data)])
        } else {
          return(NULL)
        }
      )


      df = dplyr::distinct(dplyr::bind_rows(l))

      idhash = na.omit(pca_server$primary_table[,c("name", "id")])

      df$id = purrr::map_vec(df$id, function(id) {
        print(id)
        idhash$name[idhash$id == id]
      })

      print("SUMMARIZE")

      print(head(df))

      colnames(df) = c("id", "data")

      if (is(df$data, "numeric")) {
        df = df |>
          dplyr::group_by(id) |>
          dplyr::summarise(data = mean(data))
      }

      if (is(df$data, "character")) {
        df = df |>
          dplyr::group_by(id) |>
          dplyr::summarise(data = paste(data, collapse = ", "))
      }

      print(head(df))

      df$group = group(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = df$id
      )

      df$color = color(
        pt = get.table(r$db, "primary_table"),
        dgt = get.table(r$db, "datagroup_table"),
        hashs = df$id
      )

      df = merge(df, data.frame(loadings()), by.x = "id", by.y = "row.names")

      df = df[,c("id", "data", "group", "color", input$corr_pc)]

      colnames(df) = c("id", "data", "group", "color", "pc")

      return(df)
    })
    
    # _. UI elements
    
    # output$ui_choose_corr <- shiny::renderUI({
    #   shiny::fluidRow(
    #     col_6(
    #       shiny::selectInput(inputId = ns("corr_pc"), label = r$txt[[96]], choices = pc_names())
    #     ),
    #     col_6(
    #       shiny::selectInput(inputId = ns("corr_data"), label = r$txt[[96]], choices = unique(unlist(sapply(selected_metadata(), colnames))))
    #     )
    #   )
    # })
    
    # observeEvent(corr_plot(), {
    #   r$plots[["scatterplot_metadata"]] = format_plot(corr_plot())
    # })
    
    # output$ui_corr_plot = shiny::renderPlot(
    #   corr_plot()
    # )
    
    # corr_plot = shiny::reactive({
    #   if (is(corplotdf()$data, "numeric")) {
    #     p = ggplot2::ggplot(corplotdf(), ggplot2::aes(x = pc, y = data)) +
    #       ggplot2::geom_point() +
    #       ggplot2::theme_minimal()
    #     
    #     return(p)
    #   }
    #   if (is(corplotdf()$data, "character")) {
    #     p = ggplot2::ggplot(corplotdf(), ggplot2::aes(x = data, y = pc)) +
    #       ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
    #       ggplot2::geom_boxplot() +
    #       ggplot2::theme_minimal()
    #     return(p)
    #   }
    # })
 
    # output$ui_header8 <- shiny::renderUI({
    #   shiny::titlePanel(r$txt[[94]])
    # })
    
    # output$ui_tab_title_correlations <- shiny::renderText(r$txt[94])
    
  })
}
    
## To be copied in the UI
# mod_PCA_Correlations_ui("PCA-Correlations_1")
    
## To be copied in the server
# mod_PCA_Correlations_server("PCA-Correlations_1")
