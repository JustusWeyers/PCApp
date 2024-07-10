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
    shiny::uiOutput(ns("rawtable_ui")),
    shiny::uiOutput(ns("daterange_ui")),
    shiny::uiOutput(ns("select_groups_ui"))
  )
}

#' selection Server Functions
#'
#' @noRd
mod_selection_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####

    # Select server reactive values

    selectserver = shiny::reactiveValues()

    # Functions
    cl_data = function(d, name, gparam) {
      t = get.table(d, name)
      cn = c(gparam[["timestamp"]], gparam[["value"]])
      if (all(cn %in% colnames(t))) {
        t = t[,cn]
      }

      # Remove missing values
      t = t[!identical(as.character(t$value), gparam[["missing_val"]]),]

      colnames(t) <- c("timestamp", name)

      tryCatch(
        {
          t$timestamp = as.Date(
            format(t$timestamp, scientific = FALSE),
            format = gparam[["dateformat"]]
          )
        }, error = function(cond) {
          NULL
        }
      )

      return(t)
    }
    # Reactive functions

    datagroup_table = reactive({
      get.table(r$db, "datagroup_table")
    })

    groups = reactive({
      datagroup_table()[datagroup_table()$dtype == "Timeseries",]
    })


    group_tables = reactive({
      l = lapply(groups()$name, function(g) get.table(r$db, g))
      return(setNames(l, groups()$name))
    })

    tablenames = reactive({
      l = lapply(group_tables(), function(gt) gt$name)
      return(setNames(l, names(group_tables())))
    })

    group_params = reactive({
      l = lapply(groups()$gparam, function(p) as.list(jsonlite::fromJSON(p)))
      return(return(setNames(l, groups()$name)))
    })

    rawtables = reactive({
      # Iterate over table names
      l = lapply(names(tablenames()), function(g) {
        tns = unlist(tablenames()[g])
        # For every tablename in table names fetch table from database
        dfs = lapply(tns, function(n) cl_data(r$db, name = n, gparam = group_params()[[g]]))
        # Remove empty groups
        dfs = dfs[length(dfs)>=1]
        # Merge dataframes groupwise
        df = purrr::reduce(.x = dfs, .f = dplyr::full_join, by = "timestamp")
        return(df)
      })
      return(setNames(l, groups()$name))
    })

    observeEvent(input$select_groups, {
      selectserver$combi_table = purrr::reduce(.x = rawtables()[input$select_groups], .f = dplyr::full_join, by = "timestamp")
      selectserver$combi_table = selectserver$combi_table[order(selectserver$combi_table$timestamp),]
    })

    start = reactive(as.Date(head(selectserver$combi_table$timestamp, n = 1)))
    end = reactive(as.Date(tail(selectserver$combi_table$timestamp, n = 1)))

    observeEvent(input$daterange, {
      d = selectserver$combi_table
      # Data columns
      dcol = 2:ncol(d)
      # Add missing days
      d = dplyr::full_join(d, data.frame(timestamp = seq(from = min(d$timestamp), to = max(d$timestamp), by = "days")), by = "timestamp")
      d = d[order(d$timestamp),]
      # Linear interpolation
      d[,dcol] = sapply(d[,dcol], zoo::na.approx, na.rm = FALSE)
      # # Cropping
      i = lubridate::interval(input$daterange[1], input$daterange[2])
      d = d[lubridate::`%within%`(d$timestamp, i),]
      # # Remove NA columns
      d = dplyr::select_if(d, .predicate = ~ !any(is.na(.)))
      # Write to tatabase
      # d$timestamp = as.Date(d$timestamp)
      write.dbtable(r$db, "prep_table", d)
    })

    # UI

    output$daterange_ui = shiny::renderUI({
      dateRangeInput(
        ns("daterange"),
        r$txt[57],
        start = start(),
        end = end(),
        min = start(),
        max = end()
      )
    })


    output$select_groups_ui = renderUI({
      shiny::checkboxGroupInput(
        inputId = ns("select_groups"),
        label = r$txt[58],
        choices = groups()$name,
        selected = groups()$name
      )
    })

    ####

  })
}

## To be copied in the UI
# mod_selection_ui("selection_1")

## To be copied in the server
# mod_selection_server("selection_1")
