#' selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom zoo na.approx

mod_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      # Header 1
      shiny::uiOutput(ns("ui_header1")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_2(
            shiny::uiOutput(ns("groupcheckboxes")),
          ),
          col_10(
            shiny::uiOutput(ns('tabs'))
          )
        )
      ),
      # Header 2
      shiny::uiOutput(ns("ui_header2")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_2(
            shiny::uiOutput(ns("ui_daterange_input"))
          ),
          col_10(
            shiny::plotOutput(ns("lint"), height = 200)
          )
        )
      ),
      shiny::uiOutput(ns("ui_header3")),
      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::uiOutput(ns("cachebutton"))
      )
    )
  )
}

#' selection Server Functions
#'
#' @noRd
mod_selection_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####

    get_sparam = function(d) {
      if ("selection_table" %in% user.tables(d)) {
        get.table(d, "selection_table")
      } else return(list())
    }

    # Reactive values
    selection_server = reactiveValues(
      sparam = get_sparam(shiny::isolate(r$db))
    )

    primary_table = reactive({
      get.table(r$db, "primary_table")
    })

    datagroup_table = reactive({
      get.table(r$db, "datagroup_table")
    })

    groups = reactive({
      datagroup_table()[datagroup_table()$dtype == "Timeseries",]
    })

    selected_groups = reactive({
      selection = groups()[groups()$name %in% input$group_checkboxes,]
      if (length(selection$name) > 0) {
        selection_server$sparam[["selected_groups"]] <- selection$name
      }
      return(selection)
    })

    tables = reactive({
      if (length(selected_groups())>0) {
        l = lapply(groups()$key, function(k){
          primary_table()[primary_table()$dgroup == k,"name"]
        })
        return(setNames(l, selected_groups()$name))
      }
    })

    # Combination of selected groups in one data.frame
    combimatrix <- reactive({
      get.table(r$db, "timeseries_table")
    })

    data_list = reactive({
      l = lapply(tables(), function(nms) {
        combimatrix()[,c("timestamp", nms)]
      })
      return(setNames(l, names(tables())))
    })

    output$groupcheckboxes <- shiny::renderUI({
      shiny::checkboxGroupInput(ns("group_checkboxes"), label = NULL, choices = groups()$name, selected = groups()$name)
    })

    # Filled missing dates
    combimatrix_filled = reactive({
      if (!is.null(first_obs()) & !is.null(last_obs())) {
        dates = data.frame(timestamp = seq(as.Date(first_obs()), as.Date(last_obs()), by = "days"))
        d = purrr::reduce(.x = list(combimatrix(), dates), .f = dplyr::full_join, by = "timestamp")
        d = d[order(d$timestamp),]
        return(d)
      }
    })

    linint = reactive({
      if (!is.null(combimatrix_filled())) {
        cn = colnames(combimatrix_filled())[colnames(combimatrix_filled()) != "timestamp"]
        lint = data.frame(apply(X = combimatrix_filled()[,cn], FUN = zoo::na.approx, na.rm = FALSE, MARGIN = 2))
        lint$timestamp <- combimatrix_filled()$timestamp
        return(lint)
      }
    })

    croptable = reactive({
      if (!is.null(linint())) {
        i = lubridate::interval(input$daterange[1], input$daterange[2])
        crop = linint()[lubridate::`%within%`(linint()$timestamp, i),]
        return(crop)
      }
    })

    nafree = reactive({
      if (!is.null(croptable())) {
        t = croptable()[ , colSums(is.na(croptable())) == 0]
        t = dplyr::select(t, "timestamp", dplyr::everything())
      }
    })

    first_obs = reactive({
      if (length(combimatrix()$timestamp)>0) {
        head(combimatrix()$timestamp, n = 1)
      }
    })

    last_obs = reactive({
      if (length(combimatrix()$timestamp)>0) {
        tail(combimatrix()$timestamp, n = 1)
      }
    })

    colors = reactive({
      if (length(selected_groups()$key)>0) {
        l = lapply(selected_groups()$gparam, jsonlite::fromJSON)
        return(data.frame(group = selected_groups()$name, color = sapply(l, function(s) s[["color"]])))
      }
    })

    plot_proportions = reactive({
      if (length(selected_groups())>0 & !is.null(colors())) {
        plotdf = lapply(tables(), function(nms) length(nms[nms %in% colnames(nafree())]))
        plotdf = data.frame(group = names(tables()), occurences = unname(unlist(plotdf)))
        plotdf = merge(plotdf, colors(), by = "group")

        p = ggplot2::ggplot(data=plotdf, ggplot2::aes(x=group, y=occurences, fill = color)) +
          ggplot2::geom_bar(stat="identity") +
          ggplot2::scale_fill_manual(values = rev(plotdf$color)) +
          ggplot2::ggtitle("Number of available timeseries") +
          ggplot2::ylab("Number") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face="bold", size = 15),
            axis.title.y=ggplot2::element_blank(),
            plot.title.position = "plot",
            legend.position="none"
          )
        return(p)
      }
    })

    output$tabs <- renderUI({
      tabs <- lapply(input$group_checkboxes, function(g) {
        tabPanel(g, fluidRow(col_12(
          DT::renderDataTable(data_list()[[g]]),
          style = "overflow-x: scroll;")
          )
        )
      })
      tabs[["width"]] = 12
      do.call(tabBox, tabs)
    })

    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel("Select groups")
    })

    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel("Choose Daterange")
    })

    output$ui_daterange_input <- shiny::renderUI({
      print(selection_server$sparam)
      if (all(c("start", "end") %in% names(selection_server$sparam))) {
        start = selection_server$sparam[["start"]]
        end = selection_server$sparam[["end"]]
      } else {
        start = first_obs()
        end = last_obs()
      }
      print(c(start, end))
      shiny::dateRangeInput(inputId = ns("daterange"), start = start,
                            label = NULL,
                            end = end, min = first_obs(),
                            max = last_obs(), language = "en")
    })

    output$lint <- shiny::renderPlot(
      plot_proportions()
    )

    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel("Cache selection")
    })

    output$cachebutton <- shiny::renderUI({
      shiny::actionButton(ns("cache_selection_button"), label = "Cache selected data")
    })

    cache_button = reactive(
      input[["cache_selection_button"]]
    )
    shiny::observeEvent(cache_button(), {
      print("Pressi")
    })

    shiny::observeEvent(input$daterange, {
      selection_server$sparam[["start"]] <- input$daterange[1]
      selection_server$sparam[["end"]] <- input$daterange[2]
    })

    shiny::observeEvent(cache_button(), {
      if (length(selection_server$sparam)>0) {
        write.dbtable(r$db, "selection_table", data.frame(sparam = toString(jsonlite::toJSON(selection_server$sparam))))
      }
      write.dbtable(r$db, "selected_timeseries", nafree())
    })

    ####

  })
}

## To be copied in the UI
# mod_selection_ui("selection_1")

## To be copied in the server
# mod_selection_server("selection_1")

### Playground
# tables = reactive({
#   if (length(selected_groups())>0) {
#     l = lapply(groups()$key, function(k){
#       primary_table()[primary_table()$dgroup == k,"name"]
#     })
#     return(setNames(l, selected_groups()$name))
#   }
# })
#
# # List of groups and related tables
# data_matrix = reactive({
#   if (length(tables()) > 0) {
#     # Iterate over table names
#     l = lapply(tables(), function(tns) {
#       # For every tablename in table names fetch table from database
#       dfs = lapply(tns, function(n) {
#         setNames(get.table(r$db, paste0(n, "_clean")), c("timestamp", n))
#       })
#       # Remove empty groups
#       # dfs = dfs[length(dfs)>=1]
#       # Merge dataframes groupwise
#       df = purrr::reduce(.x = dfs, .f = dplyr::full_join, by = "timestamp")
#       df$timestamp = as.Date(df$timestamp)
#       return(df)
#     })
#     return(setNames(l, selected_groups()$name))
#   }
# })
