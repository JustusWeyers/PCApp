#' general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom rnaturalearth ne_countries

mod_general_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    shiny::textOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage(
        # Body
        shiny::fluidRow(
          col_12(
            shiny::uiOutput(ns("ui_header1")),
            shinydashboard::box(
              width = "100%", solidHeader = TRUE,
              shiny::fluidRow(
                col_4(
                  shiny::uiOutput(ns("ui_crs_textinput")),
                  shiny::uiOutput(ns("ui_crs_details_box"))
                ),
                col_8(
                  shiny::plotOutput(ns("crs_plot"))
                )
              )
            ),

            shiny::uiOutput(ns("ui_header2")),
            shinydashboard::box(
              width = "100%", solidHeader = TRUE,
              shiny::fluidRow(
                col_2(
                  shiny::uiOutput(ns("ui_lang_input"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' general Server Functions
#'
#' @noRd
mod_general_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #################
    ### Constants ###
    #################

    RANDOMADDRESS = random_address()

    #######################
    ### Reactive values ###
    #######################

    language = reactive({
      input[[paste0(RANDOMADDRESS, "_lang")]]
    })

    crs_input = reactive({
      input[[paste0(RANDOMADDRESS, "_crs")]]
    })

    crs = reactive({
      tryCatch({
        if (is.null(crs_input())) {
          return(NULL)
        } else if (!is.na(as.numeric(crs_input()))) {
          return(sf::st_crs(as.numeric(crs_input())))
        } else if (nchar(crs_input())>=1) {
          return(sf::st_crs(as.numeric(crs_input())))
        } else {
          return(NULL)
        }
      }, warning = function(e) {
        return(NULL)
      })
    })

    bbox = reactive({
      if (!is.null(crs())) {
        crs_string = unname(unlist(stringi::stri_split_lines(crs())))
        bbox_string = trimws(crs_string[grepl("BBOX", crs_string, fixed = TRUE)])
        coords = as.numeric(unname(unlist(stringr::str_extract_all(bbox_string, "[-+]?\\d+\\.*\\d*"))))
        print(coords)
        coords_df = data.frame(
          lat = c(coords[1], coords[1], coords[3], coords[3]),
          lon = c(coords[2], coords[4], coords[4], coords[2])
        )
        return(coords_df)
      }
    })

    bbox_shp = reactive({
      if (!is.null(bbox())) {
        bbox = sf::st_as_sf(bbox(), coords = c("lon", "lat"), crs = 4326)
        bbox = sf::st_as_sfc(sf::st_bbox(bbox))
        return(bbox)
      }
    })

    ####################
    ### Server logic ###
    ####################

    observeEvent(language(), {
      r$lang = language()
    })



    ##########
    ### UI ###
    ##########

    output$ui_crs_textinput <- shiny::renderUI(
      shiny::textInput(ns(paste0(RANDOMADDRESS, "_crs")), r$txt[[73]], value = "25833", placeholder = r$txt[[77]])
    )

    output$ui_lang_input <- shiny::renderUI(
      shiny::selectInput(
        inputId = ns(paste0(RANDOMADDRESS, "_lang")),
        label = NULL,
        choices = sort(names(PCApp:::internal$apptext)),
        selected = r$lang)
    )

    output$ui_crs_text = shiny::renderText(
      if (!is.null(crs())) {
        paste0(unlist(stringi::stri_split_lines(crs())), "\n")
      }
    )

    output$crs_plot <- shiny::renderPlot({
      world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      p = ggplot2::ggplot(world) +
        ggplot2::geom_sf() +
        ggplot2::theme_bw()
      if (!is.null(bbox_shp())) {
        p = p + ggplot2::geom_sf(data = sf::st_transform(bbox_shp(), sf::st_crs(world)),
                                 color = "red", fill = "transparent", linewidth = 1)
      }
      return(p)
    })

    output$ui_tab_title = shiny::renderText(r$txt[[66]])

    output$ui_crs_details_box = shiny::renderUI(
      shinydashboard::box(
        width = "100%", collapsible = TRUE, collapsed = TRUE, title = r$txt[[78]],
        shiny::verbatimTextOutput(ns("ui_crs_text"), placeholder = TRUE)
      )
    )

    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[67]])
    })
    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[68]])
    })

  })
}

## To be copied in the UI
# mod_general_ui("general_1")

## To be copied in the server
# mod_general_server("general_1")
