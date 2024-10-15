#' selection UI Function
#'
#' @description A shiny module customized to select some data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage uiOutput fluidRow plotOutput
#' @importFrom shinydashboard box
#' @importFrom zoo na.approx
#' @importFrom DT DTOutput
#' @import Rcpp

mod_selection_ui <- function(id){
  # Namespace
  ns <- shiny::NS(id)

  # UI

  ## This module ui returns a {shiny} fluid page with four sections,
  ## where the user can maneuver through the proces of selecting
  ## the appropriate data for principal component analysis.

  ## --- Justus Weyers 05.09.2024

  # Fluid page
  shiny::tagList(
    shiny::fluidPage(

      # Group selection

      ## First box serves to choose Groups. Therefore an array of checkboxes
      ## gets created based on the imported group, where the user can choose
      ## which groups of timeseries data he wants to further analyse.

      ## Title of the section
      shiny::uiOutput(ns("ui_header1")),

      ## The displayd box itself has no title since the title becomes seperately
      ## rendered.The Box has full width. First element in col_2 is the checkbox
      # array. In the col_10 the tab-box with the groupdata is rendered.

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

      # Period of investigation

      ## In the second section the user gets enabled to choose a period of
      ## investigation. This is of great importance for he analysis because
      ## based on these decisions the imported timeseries can fit this period
      ## or not. So this criterium has a big influence on the number of
      ## available timeseries.

      ## Title of the section
      shiny::uiOutput(ns("ui_header2")),

      ## In order to keep the layout consistent the rendered box has no title as
      ## well and is of full width. It contains two columns where in the left
      ## col_2 a daterange-input becomes rendered while in the right col_10 a
      ## plot is generated to show the available timeseries per Group.

      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_2(
            shiny::uiOutput(ns("ui_daterange_input"))
          ),
          col_10(
            shiny::plotOutput(ns("proportion_plot"), height = 200)
          )
        )
      ),


      # Maximum data gap length

      ## Another criterium which might affect the number of well suited
      ## timeseries for principal component analysis is the presence of gaps
      ## in the timeseries since too long data gaps should not become linearly
      ## interpolated. The third section of this page addresses this topic.

      ## Title of the section
      shiny::uiOutput(ns("ui_header3")),

      ## As of the other boxes, this box has no title itself as well and is of
      ## full width. It contains a input for the level of autocorrelation and
      ## an input to choose class width in the left col_2. In the col_10 on the
      ## right hand side a table gets rendered containing information on maximum
      ## gap length within the previously defined period of investigation and
      ## calculated levels of autocorrelationl per class

      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_2(
            shiny::uiOutput(ns("ui_alpha_slider")),
            shiny::uiOutput(ns("ui_dtclass_input")),
            shiny::uiOutput(ns("ui_acor_button"))
          ),
          col_10(
            DT::DTOutput(ns("gapdf"))
          )
        )
      ),

      # Save selection

      ## The last section serves to store the selected data in the database in
      ## order to keep them ready for the analysis itself. It is planned to
      ## enable the user to store multiple selection profiles.

      # Title of the section
      shiny::uiOutput(ns("ui_header4")),

      ## The full width and titleless box contains stand of now simply a button,
      ## to write results of selection to database. It is planned to choose form
      ## different selection profiles right here (see above).

      shinydashboard::box(
        width = "100%", solidHeader = TRUE,
        shiny::fluidRow(
          col_2(
            shiny::uiOutput(ns("cachebutton"))
          )
        )
      )

    )
  )
}

#' selection Server Functions
#'
#' @param id internal {shiny} parameter
#' @param r applications global variable
#'
#' @noRd
#'
#' @importFrom shiny moduleServer sliderInput numericInput reactive withProgress
#' @importFrom shiny incProgress  observeEvent dateRangeInput  reactiveValues
#' @importFrom shiny renderUI tabPanel titlePanel checkboxGroupInput fluidRow
#' @importFrom shiny isolate renderPlot actionButton
#' @importFrom shinydashboard tabBox
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual ggtitle ylab
#' @importFrom ggplot2 coord_flip theme_minimal theme element_text element_blank
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils head tail
#' @importFrom data.table data.table
#' @importFrom stats setNames
#' @importFrom DT renderDataTable
#' @importFrom dplyr select everything select_if
#' @importFrom lubridate `%within%` interval

mod_selection_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    # Namespace
    ns <- session$ns

    # Server

    ## The module server performs necessary calculations an renders the ui
    ## elements. There are basically five sections to differentiate:

    ## # 1. Function definitions
    ## # 2. Reactive functions definitions
    ## # 2.1. Unrelated reactive functions
    ## # 2.2. "Data pipeline" reactive functions
    ## # 3. Reactive values initialization
    ## # 4. Server logic via observers
    ## # 5. UI elements rendering

    ## The following code is ordered in this sequence. For standard functions
    ## it is inevitable to define them at first place. However this server also
    ## makes use of some utility functions as well as one c++ function. These
    ## are defined in the /R directory respectively in the /src directory. The
    ## server makes also use of the slots and functions of the child classes
    ## of class Database defined in /R/fct_classDatabase.R file.

    ## --- Justus Weyers 05.09.2024

    # 1. Function definitions

    ## get_sparam() function

    ## The get_sparam function checks if there is a table called
    ## 'selection_table' in the connected database. Eventually it reads its
    ## content in the sparam-column and returns it via jsonlite as list.
    ## Parameter: d Database object
    ## Value: List of parameters for the selection

    get_sparam = function(d) {
      if ("selection_table" %in% user.tables(d)$tablename) {
        jsonlite::fromJSON(get.table(d, "selection_table")$sparam)
      } else return(NULL)
    }

    # 2.1. Unrelated reactive functions

    ## first_obs() reactive function

    ## Returns the first date in the 'timeseries_table' which is stored in
    ## servers reactiveValues. Depends: selection_server
    first_obs = shiny::reactive(
      if (length(selection_server$timeseries_table$timestamp)>0) {
        # Get first element of selection_server$timeseries_table$timestamp
        as.Date(utils::head(selection_server$timeseries_table$timestamp, n = 1))
      }
    )

    ## last_obs() reactive function

    ## Returns the last date in the 'timeseries_table' which is stored in
    ## servers reactiveValues. Depends on selection_server reaciveValues.
    last_obs = shiny::reactive(
      if (length(selection_server$timeseries_table$timestamp)>0) {
        # Get last element of selection_server$timeseries_table$timestamp
        as.Date(utils::tail(selection_server$timeseries_table$timestamp, n = 1))
      }
    )

    ## colors() reactive function

    ## Returns a data.frame containing the names of the selected groups in the
    ## group column and the associated color in the color column. Depends on
    ## the names and group parameter (gparam) in selected_groups().
    colors = shiny::reactive(
      if (length(selected_groups()$key) > 0) {
        # Get gparam in list
        l = lapply(selected_groups()$gparam, jsonlite::fromJSON)
        # Names and colors in data.frame
        df = data.frame(
          group = selected_groups()$name,
          color = sapply(l, function(s) s[["color"]])
        )
        # Return data.frame
        return(df)
      }
    )

    ## period_of_investigation() reactive function

    ## Returns a {lubridate} interval of range input$daterange
    period_of_investigation = shiny::reactive({
      lubridate::interval(input$daterange[1], input$daterange[2])
    })

    ## gaps() reactive function

    ## The gaps reactive function performs the task to check which is the
    ## maximum data gap length within the period of investigation of each
    ## timeseries and calculates the autocorrelation classwise for
    ## non-periodical data. Depends on the accepted groups and names as well
    ## as on user inputs for accepted autocorrelationlevel and class with in
    ## days. Also depends on selection_server$timeseries_table$timestamp.
    gaps = shiny::reactive({

      # Preconditions
      is_null_names = is.null(groups_and_names())
      is_null_input1 = is.null(input$alpha_slider)
      is_null_input2 = is.null(input$dtclass_input)
      if (any(c(is_null_names, is_null_input1, is_null_input2))) return(NULL)

      # Set up a result data.frame. It contains a column with names of the
      # accepted timeseries and a yet empty column of maximum gap
      dat = data.frame(# data.table::data.table(
        id = colnames(nafree())[colnames(nafree()) != "timestamp"],
        maxgap = NA,
        maxcor = NA
      )

      # User defined level of autocorrelation
      alpha = input$alpha_slider

      # User defined class width
      dt = input$dtclass_input

      # Check if userinputs are ok
      if (is.na(dt) | dt <= 1 | (dt %% 2) != 0) {
        # Prematurely return result data.table
        return(dat)
      }

      # Retrieve dates from timeseries_table and turn them into numeric
      dates = as.numeric(as.Date(selection_server$timeseries_table$timestamp))
      # Define classes based on the dates
      classes = c(1, seq(dt/2, to = floor(0.25 * diff(range(dates))), by = dt))

      # Paste some classnames for the result of autocorrelation calculation
      class_names = paste0(classes[-length(classes)], "...", (classes[-1]-1))
      # Set up a empty data.table (column names starting with numbers)
      corr = data.frame( #data.table::data.table(
        matrix(ncol = length(class_names), nrow = nrow(dat))
      )
      # Give the data.table the 'class_names' as column names
      colnames(corr) = paste0("dt", class_names)

      # Iterate over names in dat names-column and calculate the level of
      # autocorrelation for each class. Framed by a shiny progress bar.
      shiny::withProgress(message = 'Calculating Autocorrelation', value = 0, {
        for (i in 1:nrow(dat)) {
          # Fetch station time series from db
          data = get.table(r$db, paste0(dat[i,"id"], "_clean"))
          # Turn timestamp column into Date
          data$timestamp = as.Date(data$timestamp)
          # Calculate Autocorrelation via R/utils_autocorrelation.R
          acordf = acor(data = data, classes = classes, alpha = alpha)
          # print("Combine")
          # print(acordf)
          # print(corr)
          # print(1:nrow(acordf))
          # Fill correlation data into corr data.table
          corr[i,1:nrow(acordf)] = acordf$Autocorrelation

          dat$maxcor[i] = classes[nrow(acordf)]-1

          # Data.frame containing date and difference to next date columns
          gapdf = data.frame(
            ts = data$timestamp,
            diff = c(diff(as.numeric(data$timestamp)), 0)
          )

          # Crop gapsdf to contain only gaps within the period of investigation
          gapdf = subset(
            x = gapdf,
            subset = lubridate::`%within%`(gapdf$ts, period_of_investigation())
          )

          # Write maximum difference or zero to 'dat' data.frame at position i
          if (nrow(gapdf) >= 1) {
            dat$maxgap[i] = max(gapdf$diff)
          } else {
            dat$maxgap[i] = 0
          }

          # Increase loading bar
          shiny::incProgress(1/nrow(dat), detail = paste("Station", dat$id[i]))

        }
      })

      # Drop NA columns
      corr = corr[,colSums(is.na(corr)) < nrow(corr)]
      # Combine dat and corr data.tables
      dat = data.table::data.table(cbind(dat, corr))

      # Return result data.table
      return(dat)
    })

    ## data_list() reactive function

    ## Create a list which contains a subset of timeseries_table based on
    ## column names for each group. The list is going to be displayed in a tab-
    ## box in the first section.
    data_list = shiny::reactive({
      l = lapply(unlist(groups_and_names()), function(nms) {
        selection_server$timeseries_table[,c("timestamp", nms)]
      })
      l = stats::setNames(l, names(groups_and_names()))
      return(l)
    })

    ## data_tabs() reactive function

    ## Build some tabs for the data in data_list() reactive function. Actually
    ## rendered become the panels in the UI section (section five).
    data_tabs = shiny::reactive({
      # Build some tab panels for the data in data_list()
      panels = lapply(input$group_checkboxes, function(g) {
        shiny::tabPanel(g,
          shiny::fluidRow(
            col_12(
              DT::renderDataTable(
                expr = data_list()[[g]],
                options = list(scrollX = TRUE)
              )
            )
          )
        )
      })
      panels[["width"]] = 12
      return(panels)
    })

    # 2.2. "Data pipeline" reactive functions

    ## The following eight reactive functions display some kind of action chain
    ## to deliver a clean and usable dataframe for analysis in the end. There
    ## fore the function performe the following tasks

    ## 1. groups()                        Subset datagroup_table
    ## 2. selected_groups()               Subset groups() by userinput
    ## 3. groups_and_names()              Divide timeseries into group list
    ## 4. timeseries_table_all_dates()    Fill timeseries_table missing dates in
    ## 5. linint()                        Linear interpolation
    ## 6. croptable()                     Crop to period of investigation
    ## 7. nafree()                        Remove too short timeseries
    ## 8. gap_conform()                   Remove timeseries with too long gaps

    ## 1. groups() reactive function

    ## Create subset of the datagroup_table. The table becomes filtered to
    ## contain only timeseries data. Returns a data.frame with key, name, dtype
    ## and gparam columns
    groups = shiny::reactive(
      subset(selection_server$datagroup_table, dtype == "Timeseries")
    )

    ## 2. selected_groups() reactive function

    ## The user chooses which groups he wants for analysis via the
    ## 'input$group_checkboxes' input. The data.frame returned by groups()
    ## becomes subsetted to contain only those rows with accepted groups
    selected_groups = shiny::reactive(
      subset(groups(), name %in% input$group_checkboxes)
    )

    ## 3. groups_and_names() reactive function

    ## Divide names of timeseries data into corresponding groups and return
    ## a named list. List names represent the groups. Depends on
    ## selected_groups()
    groups_and_names = shiny::reactive({
      # Fetch a copy of primary_table
      pt = selection_server$primary_table
      # Fetch group keys of selected groups
      keys = selected_groups()$key
      # Build list. Per groupkey return timeseries
      l = lapply(keys, function(k) pt[pt$dgroup == k, "name"])
      tt_cn = colnames(selection_server$timeseries_table)
      l = lapply(l, function(nms) nms[nms %in% tt_cn])
      # Fetch names of selected groups
      names = selected_groups()$name
      # Returnnamed list
      return(stats::setNames(l, names))
    })

    ## 4. timeseries_table_all_dates() reactive functions

    ## The combimarix might miss some days, where no value is available. In
    ## order to perform the linear interpolation correctly the missing dates
    ## become inserted via a merge
    timeseries_table_all_dates = shiny::reactive({
      # Preconditions
      dates_ok = (!is.null(first_obs()) & !is.null(last_obs()))
      names_ok = length(groups_and_names()) >= 1
      if (!all(dates_ok & names_ok)) return(NULL)

      # Build data.frame containing all dates as single column
      dates = seq(as.Date(first_obs()), as.Date(last_obs()), by = "days")
      dates = data.frame(timestamp = dates)
      # Subset timeseries_table to selected timeseries
      cn = unname(unlist(groups_and_names()))
      combi = selection_server$timeseries_table[,c("timestamp", cn)]
      # Merge selection_server$timeseries_table and dates
      d = merge(dates, combi, all = TRUE, by = "timestamp")
      # Sort by timestamp
      d = d[order(d$timestamp),]
      # Return data.frame
      return(d)
    })

    ## 5. linint() reactive function

    ## Linear interpolation of timeseries_table_all_dates() data.frame. The
    ## function makes use of the na.approx() function from package {zoo}.
    linint = reactive({
      # Precondition
      if (is.null(timeseries_table_all_dates())) return(NULL)
      # Get colnames of timeseries_table_all_dates()
      cn = colnames(timeseries_table_all_dates())
      # Remove "timestamp" from colnames
      cn = cn[cn != "timestamp"]
      # Perform linear interpolation
      lint = data.frame(apply(
        X = timeseries_table_all_dates()[,cn],
        FUN = zoo::na.approx, na.rm = FALSE,
        MARGIN = 2
      ))
      # Add timestamp column back again
      lint$timestamp = timeseries_table_all_dates()$timestamp
      # Move timestamp column to first place
      lint = dplyr::select(lint, "timestamp", dplyr::everything())
      # Return interpolated data.frame
      return(lint)
    })

    ## 6. croptable() reactive function

    ## The interpolated table gets cut to the period of investigation. This way
    ## Data from outside this period are taken into account for interpolation
    ## without beeing inside the prepared data afterwards itself
    croptable = shiny::reactive({
      # Precondition
      if (is.null(linint())) return(NULL)
      # Fetch {lubridate} interval from user inputs
      poi = period_of_investigation()
      # Select rows based on {lubridate} interval
      crop = linint()[lubridate::`%within%`(linint()$timestamp, poi),]
      # Return cropped data.frame
      return(crop)
    })

    ## 7. nafree() reactive function

    ## Some timeseries have been interpolated and are within the period of
    ## investigation, but are only available for a shorter part than the period
    ## itself is. This is indicated by NA values at head or tail of these
    ## timeseries. This function removes all columns with remaining NA's.
    nafree = shiny::reactive({
      # Preconditions
      if (is.null(croptable())) return(NULL)
      # Remove NA's
      return(dplyr::select_if(croptable(), ~!any(is.na(.))))
    })

    ## 8. conform() reactive function

    ##
    ##
    ##
    conform = reactive({
      if (ncol(selection_server$gaps)>0) {
        ok_ids = gaps()$id[gaps()$maxgap < gaps()$maxcor]
        conform = nafree()[,c("timestamp", intersect(colnames(nafree()), ok_ids))]
        return(conform)
      }
    })

    # 3. Reactive values initialization

    ## Reactive values. Reactive values holds 1. selection parameter (sparam)
    ## which is a list of parameters like preselected dates loaded via
    ## get_sparam. The 'timeseries_table' variable holds the merged timeseries
    ## of all imported files. 'primary_table' and 'datagroup_table' are
    ## copies of the pendants with the same name in the database.
    selection_server = shiny::reactiveValues(
      sparam = get_sparam(shiny::isolate(r$db)),
      timeseries_table = get.table(shiny::isolate(r$db), "timeseries_table"),
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table"),
      gaps = data.frame()
    )

    # 4. Server logic via observers

    ## When something in the imports module changes, the primary_table, the
    ## datagroup_table and the timeseries_table become updated.
    shiny::observeEvent(
      eventExpr = r$import_trigger,
      handlerExpr = {
        selection_server$primary_table = get.table(r$db, "primary_table")
        selection_server$datagroup_table = get.table(r$db, "datagroup_table")
        selection_server$timeseries_table = get.table(r$db, "timeseries_table")
      }
    )

    ## Observe date range user input and change the corresponding sparam
    ## entries. The sparams are written to the database, when cache button
    ## is pressed.
    shiny::observeEvent(
      eventExpr = input$daterange,
      handlerExpr = {
        selection_server$sparam[["start"]] = input$daterange[1]
        selection_server$sparam[["end"]] = input$daterange[2]
      }
    )

    # ## Observe if new metadata are available from reactive values and
    # ## eventually add these to global reactive values.
    # shiny::observeEvent(
    #   eventExpr = selected_metadata(),
    #   handlerExpr = {
    #     r$metadata = selected_metadata()
    #   }
    # )

    shiny::observeEvent(
      eventExpr = input$calc_autocor,
      handlerExpr = {
        selection_server$gaps = gaps()
      }
    )

    ## Observe the cache button. If pressed write sparam to database, as well as
    ## the table with the prepared data from conform() reactive function. Change
    ## cache_selection_trigger in order to update the analysos module.
    shiny::observeEvent(
      eventExpr = input$cache_selection_button,
      handlerExpr = {
        # Handle sparam
        if (length(selection_server$sparam)>0) {
          # Convert sparam to data.frame format
          sparam_string = toString(jsonlite::toJSON(selection_server$sparam))
          df = data.frame(sparam = sparam_string)
          # Write the data.frame to batabase
          write.dbtable(r$db, "selection_table", df)
        }

        if (is.null(conform())) {
          selection_server$gaps = gaps()
        }

        # Save selected data
        write.dbtable(r$db, "selected_timeseries", conform())
        # Update trigger
        r$cache_selection_trigger = !(r$cache_selection_trigger)
      }
    )

    # 5. UI elements rendering

    # Header for the choose groups section
    output$ui_header1 = shiny::renderUI({
      expr = shiny::titlePanel(r$txt[[69]])
    })

    ## Checkbox array for group selection
    output$groupcheckboxes = shiny::renderUI(
      expr = shiny::checkboxGroupInput(
        inputId = ns("group_checkboxes"),
        label = NULL,
        choices = groups()$name,
        selected = groups()$name
      )
    )

    ## Tab box with available data by selected groups
    output$tabs = renderUI({
      expr = do.call(shinydashboard::tabBox, data_tabs())
    })

    ## Header for the choose period of investigation section
    output$ui_header2 = shiny::renderUI(
      expr = shiny::titlePanel(r$txt[[70]])
    )

    ## Render the date range input. If sparam contains information on the
    ## selected start and end value take these as default values. Otherwise
    ## simply take first and last observation date as default.
    output$ui_daterange_input = shiny::renderUI(
      expr = {
        # Case distinction wether sparam contains information on start/end
        if (all(c("start", "end") %in% names(selection_server$sparam))) {
          start = selection_server$sparam[["start"]]
          end = selection_server$sparam[["end"]]
        } else {
          start = first_obs()
          end = last_obs()
        }
        # Build a date range input ui
        dateinput = shiny::dateRangeInput(
          inputId = ns("daterange"), start = start, label = NULL,
          end = end, min = first_obs(), max = last_obs(),
          language = "en"
        )
        # Return the date range input ui
        return(dateinput)
      }
    )

    ## A ggplot showing the amount of available timeseries over the period
    ## of investigation. Depends on the nafree(), the color() and the
    ## groups_and_names() reactive functions.
    output$proportion_plot = shiny::renderPlot(
      expr = {
        # Preconditions
        if (!(length(selected_groups()) > 0) | is.null(colors())) return(NULL)
        # Count number of timeseries per group and store these in a list
        l = lapply(
          X = groups_and_names(),
          FUN = function(nms) length(nms[nms %in% colnames(nafree())])
        )
        # Build a dataframe from 'l' with group-name and occurence column
        plotdf = data.frame(
          group = names(groups_and_names()),
          occurences = unname(unlist(l))
        )
        # Merge color information into the data.frame by group name
        plotdf = merge(plotdf, colors(), by = "group")

        # Create ggplot p
        p = ggplot2::ggplot(
          # Use The 'plotdf' data.frame
          data=plotdf,
          # Define the aesthetics
          ggplot2::aes(x=group, y = occurences, fill = color)) +
          # Create bar plot
          ggplot2::geom_bar(stat="identity") +
          # Fill bars with group colors
          ggplot2::scale_fill_manual(values = rev(plotdf$color)) +
          # Add A Title
          ggplot2::ggtitle(r$txt[[82]]) +
          # Add a
          ggplot2::ylab(r$txt[[83]]) +
          # Make bar chart horizontal
          ggplot2::coord_flip() +
          # Set theme minimal
          ggplot2::theme_minimal() +
          # Some more settings regarding the theme
          ggplot2::theme(
            # Make title font bold
            plot.title = ggplot2::element_text(face="bold", size = 15),
            # Remove y axis title
            axis.title.y = ggplot2::element_blank(),
            # Place title on left margin
            plot.title.position = "plot",
            # Remove legend
            legend.position = "none"
          )
        # Return the plot
        return(p)
      }
    )

    ## Render title for the maximum gap length section.
    output$ui_header3 = shiny::renderUI(
      expr = shiny::titlePanel(r$txt[[79]])
    )

    ## Render a range slider for accepted
    ## autocorrelation level.
    output$ui_alpha_slider = renderUI(

      shiny::tagList(
        tags$style(type='text/css', "
          #reverseSlider .irs-bar {
              border-top: 1px solid #ddd;
              border-bottom: 1px solid #ddd;
              background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
          }
          #reverseSlider .irs-bar-edge {
              border: 1px solid #ddd;
              background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
              border-right: 0;
          }
          #reverseSlider .irs-line {
              background: #428bca;
              border: 1px solid #428bca;
          }
        "),
        div(id = "reverseSlider",
          shiny::sliderInput(
            inputId = ns("alpha_slider"),
            label = r$txt[[80]],
            min = 0.1,
            max = 1.0,
            value = 0.5,
            step = 0.01
          )
        )
      )
    )

    ## Render a numeric input for class with
    ## for the autocorrelation calculation.
    output$ui_dtclass_input = renderUI(
      expr = shiny::numericInput(
        inputId = ns("dtclass_input"),
        label =  r$txt[[81]],
        value = 30,
        min = 1,
        max = 2*365
      )
    )

    output$ui_acor_button = renderUI(
      expr = shiny::actionButton(
        inputId = ns("calc_autocor"),
        label =  r$txt[[100]]
      )
    )

    ## Render the table in the third section representing the data.frame with
    ## information about maximum gap length and levels of autocorrelation.
    output$gapdf = DT::renderDataTable(
      expr = selection_server$gaps, options = list(scrollX = TRUE)
    )

    ## Render title for the cache section.
    output$ui_header4 = shiny::renderUI(
      expr = shiny::titlePanel(r$txt[[71]])
    )

    ## Cache action button.
    output$cachebutton = shiny::renderUI(
      expr = shiny::actionButton(
        inputId = ns("cache_selection_button"),
        label = r$txt[[72]])
    )

  })
}

## To be copied in the UI
# mod_selection_ui("selection_1")

## To be copied in the server
# mod_selection_server("selection_1")
