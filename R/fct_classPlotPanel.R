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
           fillins = "character",
           clickable = "logical",
           height = "numeric"
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
           fillins = NA_character_,
           clickable = FALSE,
           height = 400
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
                    shiny::uiOutput(
                      outputId = ns("ui_plot")
                    )
                  ),
                  col_10(
                    span(
                      shiny::uiOutput(
                        outputId = ns("caption")
                      ), 
                      style="color:gray; font-size:8.0pt"
                    )
                  ),
                  col_2(
                    fluidRow(
                      col_12(
                        div(
                          shiny::uiOutput(ns("download_images_button")), 
                          style = "float: right"
                        )
                      )
                    )
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
              
              # 1. Functions
              
              # Helper to replace placeholders in caption with fillins
              replace = function(caption, fillins) {
                for (fillin in fillins) {
                  caption = sub("#s", fillin, caption)
                }
                return(caption)
              }
              
              # 2. Server functions
              
              # Observer for clicks
              shiny::observeEvent(input$plot_click, {
                r$click = append(list(name = obj@name), input$plot_click)
              })
              
              # Download handler
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
              
              # 3. UI Elements
              
              # Caption
              output$caption = shiny::renderUI(
                if (length(obj@plot) > 0) {
                  return(shiny::withMathJax(
                    replace(obj@caption, obj@fillins)
                  ))
                }
              )
              
              # Actual plot
              output$plot = shiny::renderPlot(
                if (length(obj@plot) > 0) {
                  obj@plot[[1]]
                } else {
                  obj@placeholder[[1]]
                }
              )
              
              output$ui_plot = shiny::renderUI(
                if (obj@clickable) {
                  shiny::plotOutput(
                    outputId = ns("plot"), 
                    click=ns("plot_click"),
                    width = "100%",
                    height = obj@height
                  )
                } else {
                  shiny::plotOutput(
                    outputId = ns("plot"),
                    width = "100%",
                    height = obj@height
                  )
                }
              )
              
              # Actionbutton
              output$download_images_button = shiny::renderUI(
                if (length(obj@plot) > 0) {
                  shiny::downloadButton(
                    outputId = ns("download_images"), 
                    label = r$txt[[102]], 
                    class = "btn-xs"
                  )
                }
              )
              
              ####
              
            })
            return(server)
          })
