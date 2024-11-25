#' classTimeseriesTable
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Class definition
setClass("PlotPanel",
         slots = c(
           name = "character",
           caption = "character",
           plot = "list",
           placeholder = "list",
           fillins = "character"
         ),
         prototype = list(
           name = NA_character_,
           caption = "caption",
           plot = list(),
           placeholder = list(
             ggplot2::ggplot(
               data.frame(x = 0, y = 0, text = "No plot available")
              ) + 
              ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text)) +
              ggplot2::theme_void()
           ),
           fillins = NA_character_
         )
)

# Methods

## Generate a box UI

setGeneric("plotUI", function(obj) standardGeneric("plotUI"))

setMethod("plotUI",
          methods::signature(obj = "PlotPanel"),
          function (obj) {
            ui = function(id = obj@name) {
              ns <- NS(id)
              shiny::tagList(
                
                ####
                
                # The displayed box
                shinydashboard::box(
                  width = "100%", solidHeader = TRUE,
                  col_12(
                    shiny::plotOutput(ns("plot"), click="plot_click"),
                  ),
                  col_10(
                    shiny::uiOutput(ns("caption"))
                  ),
                  col_2(
                    fluidRow(column(12, div(
                      shiny::uiOutput(ns("download_images_button")), style = "float: right")))
                  )
                )
                
                ####
                
              )
            }
            return(ui)
          })

## Generate box Servers

setGeneric("plotServer", function(obj, r) standardGeneric("plotServer"))

setMethod("plotServer",
          methods::signature(obj = "PlotPanel"),
          definition = function(obj, r) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns
              
              
              ####
              
              replace = function(caption, fillins) {
                for (fillin in fillins) {
                  caption = sub("#s", fillin, caption)
                }
                return(caption)
              }
              
              output$caption = shiny::renderUI(
                if (length(obj@plot) > 0) {
                  print(replace(obj@caption, obj@fillins))
                  return(shiny::withMathJax(
                    replace(obj@caption, obj@fillins)
                  ))
                }
              )
              
              output$download_images_button = shiny::renderUI(
                if (length(obj@plot) > 0) {
                  shiny::downloadButton(ns("download_images"), label = r$txt[[102]], class = "btn-xs")
                }
              )
              
              output$plot = shiny::renderPlot(
                if (length(obj@plot) > 0) {
                  obj@plot[[1]]
                } else {
                  obj@placeholder[[1]]
                }
              )
              
              output$download_images = shiny::downloadHandler(
                filename = paste0(obj@name, ".", r$settings[["img_format"]]),
                content = function(file) {
                  ggplot2::ggsave(
                    file, 
                    plot = format_plot(obj@plot[[1]]), 
                    device = r$settings[["img_format"]],
                    width = r$settings[["img_width"]], 
                    height = r$settings[["img_height"]], 
                    units = r$settings[["img_unit"]]
                  )
                }
              )
              
              ####
              
            })
            return(server)
          })
