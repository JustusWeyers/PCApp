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
  # Namespace
  ns <- NS(id)

  # UI

  ## This module ui returns a {shiny} tabsetPanel with multiple tab panels for
  ## different aspects of principal component analysis (PCA). The tabPanels are
  ## dedicated to treat the following aspects:
  ##
  ## 1. Visualisation of principal compolents (PCs)
  ## 2. Component loadings
  ## 3. Pairing of component loadings
  ## 4. Combinations of principal components
  ## 5. Regressions with PCs
  ## 6. Correlations between loadings and metadata (rasterdata)
  ##
  ## Corresponding to these four aspects the UI returns four {shiny} tabPanels.

  ## --- Justus Weyers 05.09.2024
  
  # shiny::uiOutput(ns("ui"))
  
  shiny::tabsetPanel(
    type = "tabs",
    mod_PCA_PCA_ui("PCA-PCA"),
    mod_PCA_Component_loadings_ui("PCA-Component_loadings"),
    mod_PCA_Pairing_of_component_loadings_ui("PCA-Pairing_of_component_loadings"),
    mod_PCA_Combinations_of_principal_components_ui("PCA-Combinations_of_principal_components"),
    mod_PCA_Linear_regression_ui("PCA-Linear_regression"),
    # mod_PCA_Correlations_ui("PCA-Correlations")
  )

}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Server

    ## The module server performs necessary calculations an renders the ui
    ## elements. There are basically four sections to differentiate:

    ## # 1. Reactive values initialization
    ## # 2. Reactive functions definitions
    ## # 3. Server logic via observers
    ## # 4. UI elements rendering

    ## The following code is ordered in this sequence. For standard functions
    ## it is inevitable to define them at first place. However this server also
    ## makes use of some utility functions. These are defined in the /R
    ## directory. The server makes also use of the slots and functions of the
    ## child classes of class Database defined in /R/fct_classDatabase.R file

    ## --- Justus Weyers 05.09.2024

    # 1. Reactive values initialization

    # PCA server's reactive values
    pca_server = shiny::reactiveValues(
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table"),
      ts = get_timeseries(shiny::isolate(r$db)),
      selected_id = NULL
    )
    
    # 2. Reactive functions definitions
    
    z = shiny::reactive(
      apply(X = r$ts[,2:ncol(r$ts)], FUN=scale, MARGIN = 2)
    )

    eigenvalues = shiny::reactive({
      eigen(stats::cov(z()))$values
    })

    pca = shiny::reactive(
      stats::prcomp(z(), center=TRUE, scale.=TRUE, retx=TRUE)
    )

    pcs = shiny::reactive({
      pcs_ = data.frame(pca()$x)
      if (!is.null(r$flip)) {
        pcs_[,r$flip] = pcs_[,r$flip] * (-1)
      }
      pcs_$timestamp = r$ts$timestamp
      return(pcs_)
    })

    loadings = shiny::reactive({
      CL = data.frame(pca()$rotation %*% diag(sqrt(eigenvalues())))
      if (!is.null(r$flip)) {
        CL[,r$flip] = CL[,r$flip] * (-1)
      }
      colnames(CL) = paste0("PC", seq(1, length(eigenvalues())))
      rownames(CL) = rownames(pca()$rotation)
      return(CL)
    })
    
    # loadings_limited = reactive({
    #   ll = subset(loadings(), select = eigenvalues()>1)
    #   rownames(ll) = rownames(loadings())
    #   return(ll)
    # })
    
    # 4. Server logic via observers
    
    shiny::observeEvent(c(r$db, r$cache_selection_trigger), {
      pca_server$primary_table = get.table(r$db, "primary_table")
      pca_server$datagroup_table = get.table(r$db, "datagroup_table")
      r$ts = get_timeseries(r$db)
      pca_server$selected_id = NULL
    })
    
    shiny::observeEvent(c(r$ts, r$flip), {
      if (nrow(r$ts)>0) {
        r$pca$z = z()
        r$pca$eigenvalues = eigenvalues()
        r$pca$pca = pca()
        r$pca$pcs = pcs()
        r$pca$loadings = loadings()
      }
    })
    
    shiny::observeEvent(pcs(), {
      if (is(pcs(), "data.frame")) {
        pcs = dplyr::select(pcs(), "timestamp", dplyr::everything())
        write.dbtable(r$db, "principal_components", pcs)
      }
    })

    shiny::observeEvent(loadings(), {
      if (is(loadings(), "data.frame")) {
        write.dbtable(r$db, "loadings", loadings())
      }
    })

    shiny::observeEvent(z(), {
      if (is(z(), "data.frame")) {
        write.dbtable(r$db, "z", z())
      }
    })
    

  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")
