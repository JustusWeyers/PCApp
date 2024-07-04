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
  shiny::tabPanel(
    "PCA",
    shiny::tagList(
      shiny::fluidPage(
        shiny::plotOutput(ns("plotPercentofVariance"))
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, r, txt){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####

    # PCA server's reactive values
    pcaserver = reactiveValues()

    # Functions

    kommunalitaeten = function(eigenvalues){

      print(eigenvalues)

      # Calculate percent of total variance
      pTV = eigenvalues / sum(eigenvalues) * 100

      # Make plotting df
      df = data.frame(PC = paste0(1:length(pTV), "."), ev = eigenvalues,
                      pTV = pTV, csum = cumsum(pTV))
      df$PC <- factor(df$PC, levels = df$PC)

      # Crop
      df = df[1:(length(which(eigenvalues>=1))+1),]

      p = ggplot2::ggplot(df, ggplot2::aes(x = PC, y = csum)) +
        ggplot2::geom_bar(stat="identity", col = "black", fill = "white") +
        # ggplot2::geom_hline(yintercept = 80, linetype = "dashed", col = "grey40") +
        # ggplot2::geom_text(x = 2*100/length(pTV), y = 80, label = "80%", vjust = -1, adj = 0, col = "grey40") +
        # ggplot2::geom_text(aes(label=format(csum, digits = 1)), vjust=1.6)+
        # ggplot2::geom_text(aes(label=format(round(ev, 2), digits = 2)), vjust=-1, color="grey40")+
        # ggplot2::ylim(0, 101) +
        # ggplot2::xlab("\nHauptkomponente") +
        # ggplot2::ylab("\nAnteil an der Gesamtvarianz [%]") +
        # ggplot2::theme(axis.title.y = ggplot2::element_text(margin = margin(t = 0, r = 35, b = 0, l = 0)),
        #       axis.title.x = ggplot2::element_text(margin = margin(t = 35, r = 0, b = 0, l = 0))) +
        ggplot2::theme_minimal()

      return(p)
    }

    # Reactive functions

    prep_table = reactive(
      get.table(r$db, "prep_table")
    )

    z = reactive(
      apply(X = prep_table()[,2:ncol(prep_table())], FUN=scale, MARGIN = 2)
    )

    eigenvalues = reactive({
      eigen(cov(z()))$values
    })

    pca = reactive(
      prcomp(z(), center=TRUE, scale.=TRUE, retx=TRUE)
    )


    # Server



    # UI

    output$plotPercentofVariance = renderPlot({
      kommunalitaeten(eigenvalues())
    })

    ####

  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")
