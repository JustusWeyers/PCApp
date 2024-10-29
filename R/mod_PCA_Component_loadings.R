#' PCA-Component_loadings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_Component_loadings_ui <- function(id){
  ns <- NS(id)
  
  ## 2. Component loadings
  
  ## The second tabPanel deals with the spatial aspect of PCs. Since each
  ## timeseries can be assigned a place and loadings on different PCs, maps
  ## are a good tool to retrieve possibly interesting spatial patterns.
  ## However it is therefore necessary to supply coordinates of each station
  ## in the metadata import section together with a usable id column.
  
  shiny::tabPanel(

    shiny::textOutput(ns("ui_tab_title_loadings")),
    
    shiny::fluidPage(
      # Header 5
      shiny::uiOutput(ns("ui_header5")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_2(
            shiny::uiOutput(ns("select_PC"))
          ),
          col_10(
            tabBox(
              id = "tabset1", width = "100%",
              tabPanel(
                "Map",
                shiny::plotOutput(
                  ns("ui_geo_loadings_plot"),
                  width = "100%",
                  height = "100%"
                )
              ),
              tabPanel(
                "Interactive Map",
                "Yet to come"
              ),
              tabPanel(
                "Table",
                DT::dataTableOutput(
                  ns("ui_loadings")
                )
              )
            )
          )
        )
      )
    )
  )
}
    
#' PCA-Component_loadings Server Functions
#'
#' @noRd 
mod_PCA_Component_loadings_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 1. Reactive values
    
    loadings_server = shiny::reactiveValues(
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table")
    )
    
    # 2. Reactives
    
    pc_names = shiny::reactive(
      paste0("PC", 1:(ncol(r$ts)-1))
    )
    
    roi = shiny::reactive({
      pt = loadings_server$primary_table
      dgt = loadings_server$datagroup_table
      dgkey = dgt[dgt$name ==  "Untersuchungsgebiet", "key"]
      if (any(pt$dgroup == dgkey)) {
        feature = sf::st_read(r$db@con, pt[which(pt$dgroup == dgkey)[1], "name"])
        return(sf::st_transform(feature, sf::st_crs(r$settings[["crs"]])))
      } else {
        return(NULL)
      }
    })
    
    hashs = shiny::reactive(
      colnames(r$ts)[colnames(r$ts) != "timestamp"]
    )
    
    ## metadata_names() reactive function

    ## Return database names of metadata
    metadata_names = shiny::reactive({
      # Fetch coy of primary_table
      pt = loadings_server$primary_table
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
    
    ## selected_metadata() reactive function

    ## Aims retrieve relevant metadata in a format, that can be stored in the
    ## database in order to be easily available for analysis.
    selected_metadata = shiny::reactive({
      if (length(metadata())<1) return(NULL)
      l = lapply(metadata(), function(df) {
        if ("id" %in% colnames(df)) {
          df[df$id %in% ids(loadings_server$primary_table, hashs()),]
        }
      })
      l[sapply(l, is.null)] = NULL
      return(l)
    })
    
    ## geo_loadings() reactive function

    ## Merge loadings with coordinates from metadata.
    geo_loadings = shiny::reactive({
      if (is.null(selected_metadata())) return(NULL)
      # Fetch locations via util function
      loc = fetch_metadata(
        metadata = selected_metadata(), #? selected_metadata
        ids = ids(loadings_server$primary_table, rownames(r$pca$loadings)),
        field = c("name", "latitude", "longitude")
      )

      loads = r$pca$loadings
      loads$id = ids(loadings_server$primary_table, rownames(loads))
      loads$group = group(
        pt = loadings_server$primary_table,
        dgt = loadings_server$datagroup_table,
        hashs = rownames(loads))

      # Mergelocations and loadings
      data = merge(
        x = loc,
        y = loads,
        by = "id"
      )
      data$id = format(data$id, scientific= FALSE)

      # Return NA free data
      return(data)
    })
    
    # _. Observers
    
    observeEvent(r$cache_selection_trigger, {
      loadings_server$primary_table = get.table(shiny::isolate(r$db), "primary_table")
      loadings_server$datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table")
    })
    
    # observeEvent(geoloadings_plot(), {
    #   r$plots[["loading_map"]] = format_plot(geoloadings_plot())
    # })
    
    output$ui_geo_loadings_plot = shiny::renderPlot(
      geoloadings_plot(), height = 800, width = 800
    )
    
    geoloadings_plot = shiny::reactive({
      if (is.null(geo_loadings())) return(NULL)
      plotdf = geo_loadings()[,c("name", "group", "latitude", "longitude", input$select_PC)]
      colnames(plotdf) = c("name", "group", "latitude", "longitude", "PC")
      plotdf = sf::st_as_sf(
        plotdf,
        coords = c("longitude", "latitude"),
        crs = sf::st_crs(r$settings[["crs"]])
      )
      p = ggplot2::ggplot()
      if (!is.null(roi())) {
        p = p + ggplot2::geom_sf(data = roi(), fill = "white")
      }
      p = p +
        ggplot2::geom_sf(data = plotdf, ggplot2::aes(col = PC, shape = group), cex = 4) +
        ggplot2::scale_colour_gradient2(low = "blue", mid = "grey90", high = "red", limits=c(-1,1)) +
        ggplot2::theme_minimal()

      # if(length(input$ui_loadings_rows_selected) > 0){
      #   plotdf_selected = plotdf[input$ui_loadings_rows_selected,]
      #   p = p + ggplot2::geom_sf(
      #     data = plotdf_selected,
      #     pch = 21,
      #     col = "green",
      #     cex = 7,
      #     stroke = 3,
      #     fill = "transparent")
      # }
      
      return(p)
    })
    
    output$ui_loadings = DT::renderDataTable(
      geo_loadings(), selection = 'single'
    )
    
    output$select_PC = shiny::renderUI({
      shiny::selectInput(
        inputId = ns("select_PC"),
        label = "Select a PC",
        choices = pc_names())
    })
    
    output$ui_header5 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[63]])
    })
    
    output$ui_tab_title_loadings <- shiny::renderText(r$txt[[63]])
    
 
  })
}
    
## To be copied in the UI
# mod_PCA_Component_loadings_ui("PCA-Component_loadings_1")
    
## To be copied in the server
# mod_PCA_Component_loadings_server("PCA-Component_loadings_1")
