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
  
  ## The fifth tabPanel is intended to show the possibilities of calculating 
  ## so-called reference hydrographs by means of multiple linear regression 
  ## using a small number of principal components. For this purpose, the 
  ## reference hydrographs obtained are compared with the original time series 
  ## in a plot. It is possible to view the regression parameters and to add 
  ## further principal components as regressors.
  
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

      shiny::fluidRow(
        col_12(
          shiny::uiOutput(
            ns("ref_ts_plot")
          )
        )
      ),
      
      fluidRow(
        col_12(
          shiny::uiOutput(ns("tabbox"))
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
      if (is.null(ref_server$selected_id)) {
        ref_server$selected_id = refhydr()$hashs[sample.int(nrow(refhydr()), 1)]
      }
      
      plotdf = refhydr()[refhydr()$hashs == ref_server$selected_id,] |>
        tidyr::unnest(augment)
      
      
      plotdf$timestamp = r$ts$timestamp
      
      p = ggplot2::ggplot(plotdf) +
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = ts), color = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = .fitted)) +
        ggplot2::ylab(r$txt[[119]]) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 12),
          axis.title.x = ggplot2::element_blank()
        )
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
    
    shiny::observeEvent(r$click, {
      
      if (r$click$name == "rsquaredbarplot") {
        i = round(r$click$y)
        ref_server$selected_id = refhydr()$hashs[order(refhydr()$rsq)][i]
      }

    })
    
    glance = shiny::reactive({
      df = refhydr()
      df$ids = ids(ref_server$primary_table, df$hashs)
      df$names = nms(df$ids, metadata())
      identity = df[,c("hashs", "ids", "names")]
      return(data.frame(cbind(identity, dplyr::bind_rows(df$glance))))
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

    output$ref_ts_plot = shiny::renderUI({
      hash = as.character(ref_server$selected_id)
      rsq = as.character(round(refhydr()$rsq[refhydr()$hashs == ref_server$selected_id], 2))
      po = methods::new(
        "PlotPanel", 
        name = "referencehydrograph", 
        caption = r$txt[["refhydr.txt"]],
        fillins = c(names_ids(ref_server$primary_table, hash), rsq),
        plot = list(ref_ts_plot()),
        height = 200
      )
      plotServer(po, r)
      po@name <- ns(po@name)
      return(plotUI(po)())
    })
    
    output$ref_bar_plot = shiny::renderUI({
      # Create plot data.frame
      plotdf = refhydr()
      plotdf$names = names_ids(ref_server$primary_table, plotdf$hashs)
      plotdf$color = color(ref_server$primary_table, ref_server$datagroup_table, plotdf$hashs)
  
      # Build ggplot
      ggp = ggplot2::ggplot(plotdf, ggplot2::aes(y = stats::reorder(names, rsq), x = rsq)) +
        ggplot2::geom_bar(stat = "identity", fill = plotdf$color) +
        ggplot2::xlim(c(0, 1)) +
        ggplot2::xlab(expression(~r^"2")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none") +
        ggplot2::scale_y_discrete(labels = scales::wrap_format(20))
      
      # Instantiate PlotPanel-Object
      ppo = methods::new(
        "PlotPanel", 
        name = "rsquaredbarplot", 
        caption = "Placeholder",
        fillins = "none",
        plot = list(ggp),
        clickable = TRUE
      )
      plotServer(ppo, r)
      ppo@name <- ns(ppo@name)
      return(plotUI(ppo)())
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

    output$ui_header7 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[95]])
    })
    
    output$ui_tab_title_regression <- shiny::renderText(r$txt[93])
    
    output$glance = DT::renderDataTable(
      
      server = FALSE,

      extensions = 'Buttons',
      
      options = list(
        scrollX = TRUE,
        selection = 'single',
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('csv')
      ),
      
      class = "display",
      
      glance()
    )
    
    output$tabbox = shiny::renderUI(
      shinydashboard::tabBox(
        width = "100%",
        shiny::tabPanel(
          r$txt[[123]], 
          fluidRow(
            col_12(
              col_6(
                shiny::uiOutput(
                  ns("ref_bar_plot")
                )
              ),
              col_6(
                shiny::verbatimTextOutput(
                  outputId = ns("ref_summary")
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          r$txt[[53]],
          shiny::fluidRow(
            col_12(
              DT::DTOutput(ns("glance"))
            )
          )
        )
      )
    )

  })
}
    
## To be copied in the UI
# mod_PCA_Linear_regression_ui("PCA-Linear_regression_1")
    
## To be copied in the server
# mod_PCA_Linear_regression_server("PCA-Linear_regression_1")
