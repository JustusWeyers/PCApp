#' PCA-Linear_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_Linear_regression_ui <- function(id){
  ns <- NS(id)
  
  ## 5. Regressions
  
  shiny::tabPanel(
    # Title of the tabPanel
    shiny::textOutput(
      outputId = ns("ui_tab_title_regression")
    ),
    # Fluid page
    shiny::fluidPage(
      shiny::uiOutput(
        outputId = ns("ui_header7")
      ),
      fluidRow(
        col_12(
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::plotOutput(
                  outputId = ns("ref_ts_plot")
                )
              )
            )
          )
        )
      ),
      
      fluidRow(
        col_6(
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::uiOutput(ns("barplot"))
              )
            )
          )
        ),
        col_6(
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::verbatimTextOutput(
                  outputId = ns("ref_summary")
                )
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
                shiny::tableOutput(ns("ui_regression_pcs"))
              )
            )
          )
        )
      )
    )
  )
}
    
#' PCA-Linear_regression Server Functions
#' 
#' @importFrom broom glance
#'
#' @noRd 
mod_PCA_Linear_regression_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 1. Reactive values
    
    ref_server = shiny::reactiveValues(
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table")
    )
    
    # 2. Reactives
    
    pc_names = shiny::reactive(
      paste0("PC", 1:(ncol(r$ts)-1))
    )
    
    ## metadata_names() reactive function
    
    ## Return database names of metadata
    metadata_names = shiny::reactive({
      # Fetch coy of primary_table
      pt = ref_server$primary_table
      # Return name column filtered for metadata
      return(pt[pt$dtype == "Metadata", "name"])
    })
    
    ## metadata() reactive function
    
    ## List of metadata tables
    metadata = shiny::reactive(
      lapply(metadata_names(), function(nm) {
        if (paste0(nm, "_clean") %in% user.tables(r$db)$tablename) {
          get.table(r$db, paste0(nm, "_clean"))
        }
      })
    )
    
    ref_ts_plot = shiny::reactive({
      if (is.null(ref_server$selected_id)) return(NULL)
      print(paste("ids:", ids(ref_server$primary_table, ref_server$selected_id)))
      plotids = ids(ref_server$primary_table, ref_server$selected_id)
      sub = paste0(
        nms(plotids, metadata()),
        " (",
        plotids,
        ")"
        )
      plotdf = refhydr()[refhydr()$hashs == ref_server$selected_id,] |>
        tidyr::unnest(augment)
      plotdf$timestamp = r$ts$timestamp
      p = ggplot2::ggplot(plotdf) +
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = ts), color = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = .fitted)) +
        ggplot2::xlab(r$txt[[115]]) +
        ggplot2::ylab(r$txt[[119]]) +
        ggplot2::labs(
          title = r$txt[[116]],
          subtitle = sub
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 12)
        ) +
        ggplot2::theme_minimal()
      return(p)
    })
    
    hashs = shiny::reactive(
      colnames(r$ts)[colnames(r$ts) != "timestamp"]
    )
    
    refhydr = shiny::reactive({
      linmodel = function(df) {
        lm(ts~., data = df)
      }

      pcs = r$pca$pcs[,input$regression_pcs]

      df = tibble::tibble(hashs = hashs())

      df$data = sapply(df$hashs, function(hash) {
        df = data.frame(cbind(r$ts[,hash], pcs))
        colnames(df) = c("ts", input$regression_pcs)
        return(df)
      }, simplify = FALSE)

      df = dplyr::mutate(df, model = data |> purrr::map(linmodel))

      df = df |> dplyr::mutate(
        glance = purrr::map(model, broom::glance),
        rsq = glance |> purrr::map_dbl("r.squared"),
        tidy = purrr::map(model, broom::tidy),
        augment = purrr::map(model, broom::augment),
        sm = purrr::map(model, summary)
      )

      df = df |> dplyr::arrange(dplyr::desc(rsq))

      return(df)
    })
    
    # 3. Observers
    
    observeEvent(input$bar_click, {
      i = round(input$bar_click$y)
      ref_server$selected_id = refhydr()$hashs[order(refhydr()$rsq)][i]
    })

    observeEvent(ref_ts_plot(), {
      r$plots[["ref_ts_plot"]] = format_plot(ref_ts_plot())
    })
    
    # 4. UI elements
    
    output$ref_summary = shiny::renderPrint({
      if (!is.null(ref_server$selected_id)) {
        sm_ = refhydr() |>
          dplyr::filter(hashs == ref_server$selected_id) |>
          dplyr::select(sm)
        return(sm_$sm)
      }
    })

    output$ref_ts_plot = shiny::renderPlot(ref_ts_plot())
     
    output$ref_bar_plot = shiny::renderPlot({
      plotdf = refhydr()
      ggplot2::ggplot(plotdf, ggplot2::aes(x = stats::reorder(hashs, rsq), y = rsq)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::ylim(c(0, 1)) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal()
    })
    
    output$ui_regression_pcs = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("regression_pcs"),
        label = "Choose PC's for linear regression",
        choices = pc_names(),
        selected = pc_names()[r$pca$eigenvalues>1],
        inline = TRUE,
      )
    )
    
    output$barplot = shiny::renderUI(
      shiny::plotOutput(
        outputId = ns("ref_bar_plot"),
        click = ns("bar_click"),
        height = 10*nrow(refhydr())
      )
    )
    
    output$ui_header7 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[95]])
    })
    
    output$ui_tab_title_regression <- shiny::renderText(r$txt[93])
    
 
  })
}
    
## To be copied in the UI
# mod_PCA_Linear_regression_ui("PCA-Linear_regression_1")
    
## To be copied in the server
# mod_PCA_Linear_regression_server("PCA-Linear_regression_1")
