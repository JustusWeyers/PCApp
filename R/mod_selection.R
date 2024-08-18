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
            shiny::plotOutput(ns("proportions_plot"), height = 200)
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

    #################
    ### Functions ###
    #################

    # Get selection parameter
    get_sparam = function(d) {
      if ("selection_table" %in% user.tables(d)$tablename) {
        jsonlite::fromJSON(get.table(d, "selection_table")$sparam)
      } else return(NULL)
    }

    ##########################
    ### Reactive functions ###
    ##########################

    cache_button = reactive(
      input[["cache_selection_button"]]
    )

    first_obs = reactive(
      if (length(selection_server$combimatrix$timestamp)>0) {
        utils::head(selection_server$combimatrix$timestamp, n = 1)
      }
    )

    last_obs = reactive(
      if (length(selection_server$combimatrix$timestamp)>0) {
        utils::tail(selection_server$combimatrix$timestamp, n = 1)
      }
    )

    #######################
    ### "Data Pipeline" ###
    #######################

    # 1. Names of available groups
    # Returns a data.frame with key, name, dtype and gparam columns
    groups = reactive({
      selection_server$datagroup_table[selection_server$datagroup_table$dtype == "Timeseries",]
    })

    # 2. Subset the groups() data.frame by checkbox user input
    selected_groups = reactive({
      groups()[groups()$name %in% input$group_checkboxes,]
    })

    # 3. Gather the names of timeseries for each group. Result is beeing
    # returned as named list
    groups_and_names = reactive({
      keys = selected_groups()$key
      names = selected_groups()$name
      l = lapply(keys, function(k) selection_server$primary_table[selection_server$primary_table$dgroup == k,"name"])
      return(stats::setNames(l, names))
    })

    # 4. Returns a list which contains timeseries data.frames for chosen groups
    data_list = reactive(
      stats::setNames(lapply(unname(groups_and_names()), function(nms) {
        selection_server$combimatrix[,c("timestamp", nms)]
      }), names(groups_and_names()))
    )

    # Filled missing dates
    combimatrix_filled = reactive({
      if (!is.null(first_obs()) & !is.null(last_obs())) {
        dates = data.frame(timestamp = seq(as.Date(first_obs()), as.Date(last_obs()), by = "days"))
        d = purrr::reduce(.x = list(selection_server$combimatrix, dates), .f = dplyr::full_join, by = "timestamp")
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


    colors = reactive({
      if (length(selected_groups()$key)>0) {
        l = lapply(selected_groups()$gparam, jsonlite::fromJSON)
        return(data.frame(group = selected_groups()$name, color = sapply(l, function(s) s[["color"]])))
      }
    })


    ####################
    ### Server logic ###
    ####################

    # Reactive values
    selection_server = reactiveValues(
      sparam = get_sparam(shiny::isolate(r$db)),
      combimatrix = get.table(shiny::isolate(r$db), "timeseries_table"),
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table")
    )

    observeEvent(r$import_trigger, {
      selection_server$primary_table <- get.table(r$db, "primary_table")
      selection_server$datagroup_table <- get.table(r$db, "datagroup_table")
      selection_server$combimatrix <- get.table(r$db, "timeseries_table")
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

    shiny::observeEvent(cache_button(), {
      print("Pressi")
    })


    ###################
    ### UI Elements ###
    ###################

    output$groupcheckboxes <- shiny::renderUI({
      shiny::checkboxGroupInput(ns("group_checkboxes"), label = NULL, choices = groups()$name, selected = groups()$name)
    })

    output$tabs <- renderUI({
      tabs <- lapply(input$group_checkboxes, function(g) {
        tabPanel(
          g,
          fluidRow(
            col_12(
              DT::renderDataTable(data_list()[[g]], options = list(scrollX = TRUE))
            )
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
      print("Render daterange")
      if (all(c("start", "end") %in% names(selection_server$sparam))) {
        start = selection_server$sparam[["start"]]
        end = selection_server$sparam[["end"]]
      } else {
        start = first_obs()
        end = last_obs()
      }
      shiny::dateRangeInput(
        inputId = ns("daterange"), start = start, label = NULL,
        end = end, min = first_obs(), max = last_obs(),
        language = "en"
      )
    })

    output$proportion_plot <- shiny::renderPlot(
      if (length(selected_groups())>0 & !is.null(colors())) {
        plotdf = lapply(groups_and_names(), function(nms) length(nms[nms %in% colnames(nafree())]))
        plotdf = data.frame(group = names(groups_and_names()), occurences = unname(unlist(plotdf)))
        plotdf = merge(plotdf, colors(), by = "group")

        p = ggplot2::ggplot(data=plotdf, ggplot2::aes(x=group, y = occurences, fill = color)) +
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
    )

    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel("Cache selection")
    })

    output$cachebutton <- shiny::renderUI({
      shiny::actionButton(
        inputId = ns("cache_selection_button"),
        label = "Cache selected data")
    })


    ####

  })
}

## To be copied in the UI
# mod_selection_ui("selection_1")

## To be copied in the server
# mod_selection_server("selection_1")
