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
  ## 2. Damping coefficient
  ## 3. Loadings on maps
  ## 4. Combinations of principal components
  ## 5. Regressions with PCs
  ## 6. Correlations between loadings and metadata (rasterdata)
  ##
  ## Corresponding to these four aspects the UI returns four {shiny} tabPanels.

  ## --- Justus Weyers 05.09.2024

  # tabsetPanel
  shiny::tabsetPanel(
    type = "tabs",

    ## 1. Visualisation of principal compolents (PCs)

    ## The first tabPanel shows the received PCs from the calculations as
    ## timeseries itself. Therefore it is easy to gain a first impression wether
    ## the PCA was successful or not. This is in particular assessable from
    ## the communalities plot which takes also place on the first tabPanel.

    shiny::tabPanel(
      # Title of the tab panel
      shiny::textOutput(ns("ui_tab_title_pca")),
      # Fluid page
      shiny::tagList(
        shiny::fluidPage(
          # Header 1
          shiny::uiOutput(ns("ui_header1")),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::plotOutput(ns("pcs"))
              )
            )
          ),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::uiOutput(ns("pc_plot_select"))
              )
            )
          ),
          # Header 2
          shiny::uiOutput(ns("ui_header2")),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                plotOutput(ns("plotPercentofVariance"))
              )
            )
          ),
          # Header
          shiny::uiOutput(ns("ui_header3")),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::uiOutput(ns("ui_flip_PCA"))
              )
            )
          )
        )
      )
    ),

    ## 2. Damping coefficient

    ##
    shiny::tabPanel(
      # Title of the tab panel
      shiny::textOutput(ns("ui_tab_title_damping")),
      # Fluid page
      shiny::tagList(
        shiny::fluidPage(

          # Header 4
          shiny::uiOutput(ns("ui_header4")),
          shiny::textOutput(ns("selected_id")),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_6(
                shiny::plotOutput(
                  ns("ui_circle_plot"),
                  click = ns("circle_click"),
                  width = "100%",
                  height = "100%"),
              ), col_6(
                shiny::plotOutput(ns("ui_damping_plot"))
              )
            )
          ),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::uiOutput(ns("ui_damping_pc"))
              )
            )
          )

        )
      )
    ),


    ## 3. Loadings on maps

    ## The second tabPanel deals with the spatial aspect of PCs. Since each
    ## timeseries can be assigned a place and loadings on different PCs, maps
    ## are a good tool to retrieve possibly interesting spatial patterns.
    ## However it is therefore necessary to supply coordinates of each station
    ## in the metadata import section together with a usable id column.

    shiny::tabPanel(
      # Title of the tabPanel
      shiny::textOutput(ns("ui_tab_title_loadings")),
      # Fluid page
      shiny::tagList(
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
    ),

    ## 4. Combinations of principal components

    ## The first principal component usually covers a substantial portion of
    ## the overall variance of the examined timeseries. This behaviuor can be
    ## interpreted as "typical medium timeline". Therefore it can be interesting
    ## to combine the this PC with a PC of higher rank to analyse wether there
    ## is a noteworthy change in temporal patterns in the combined timeseries.
    ## In this section the user can decide, which timeseries he wants to add to
    ## the first principal component and view the changes in a plot.

    shiny::tabPanel(
      # Title of the tabPanel
      shiny::textOutput(ns("ui_tab_title_combinations")),
      # Fluid page
      shiny::tagList(
        shiny::fluidPage(

          # Header 6
          shiny::uiOutput(ns("ui_header6")),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_12(
                shiny::plotOutput(ns("ui_combiplot")),
                shiny::uiOutput(ns("daterange_slider"))
              )
            )
          ),
          shinydashboard::box(
            width = "100%", solidHeader = TRUE,
            shiny::fluidRow(
              col_6(
                shiny::uiOutput(ns("ui_combi_pc"))
              ),
              col_6(
                shiny::uiOutput(ns("ui_weight_slider"))
              )
            )
          )
        )
      )
    ),

    ## 5. Regressions

    shiny::tabPanel(
      # Title of the tabPanel
      shiny::textOutput(ns("ui_tab_title_regression")),
      # Fluid page
      shiny::tagList(
        shiny::fluidPage(

          shiny::uiOutput(ns("ui_header7")),

          fluidRow(
            col_12(
              shinydashboard::box(
                width = "100%", solidHeader = TRUE,
                shiny::fluidRow(
                  col_12(
                    # shiny::plotOutput(ns("referenzHydrograph_plot"))
                    shiny::plotOutput(ns("ref_ts_plot"))
                  )
                )
              )
            )
          ),

          fluidRow(
            col_6(
              shinydashboard::box(
                width = "100%", solidHeader = TRUE,
                shiny::fluidRow(
                  col_12(
                    shiny::plotOutput(
                      ns("ref_bar_plot"),
                      click = ns("bar_click")
                    )
                  )
                )
              )
            ),
            col_6(
              shinydashboard::box(
                width = "100%", solidHeader = TRUE,
                shiny::fluidRow(
                  col_12(
                    shiny::verbatimTextOutput(ns("ref_summary"))
                  )
                )
              )
            )
          ),

          fluidRow(
            col_12(
              shinydashboard::box(
                width = "100%", solidHeader = TRUE,
                shiny::fluidRow(
                  col_12(
                    # shiny::plotOutput(ns("referenzHydrograph_plot"))
                    shiny::tableOutput(ns("ui_regression_pcs"))
                  )
                )
              )
            )
          )

        )
      )
    ),


    ## 6. Correlations between loadings and metadata (rasterdata)
    shiny::tabPanel(
      # Title of the tabPanel
      shiny::textOutput(ns("ui_tab_title_correlations")),
      # Fluid page
      shiny::tagList(
        shiny::fluidPage(

          shiny::uiOutput(ns("ui_header8")),

          fluidRow(
            col_12(
              shinydashboard::box(
                width = "100%", solidHeader = TRUE,
                shiny::fluidRow(
                  col_12(
                    shiny::plotOutput(ns("ui_corr_plot"))
                  )
                )
              )
            )
          ),

          fluidRow(
            col_12(
              shinydashboard::box(
                width = "100%", solidHeader = TRUE,
                shiny::fluidRow(
                  col_12(
                    shiny::uiOutput(ns("ui_choose_corr"))

                  )
                )
              )
            )
          )
        )
      )
    )

  )
}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    circle_plot_df = shiny::reactive({
      if (is.null(input$damping_pc)) return(NULL)
      plotdf = loadings()[,c("PC1", input$damping_pc)]
      colnames(plotdf) = c("PCA", "PCB")
      plotdf$id = rownames(loadings())
      plotdf$group = group(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = plotdf$id
      )
      plotdf$color = color(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = plotdf$id
      )

      if (!is.null(pca_server$selected_id)) {
        plotdf[plotdf$id == pca_server$selected_id, "color"] = "red"
      }

      return(plotdf)
    })

    # Server

    ## The module server performs necessary calculations an renders the ui
    ## elements. There are basically five sections to differentiate:

    ## # 1. Function definitions
    ## # 2. Reactive functions definitions
    ## # 3. Reactive values initialization
    ## # 4. Server logic via observers
    ## # 5. UI elements rendering

    ## The following code is ordered in this sequence. For standard functions
    ## it is inevitable to define them at first place. However this server also
    ## makes use of some utility functions. These are defined in the /R
    ## directory. The server makes also use of the slots and functions of the
    ## child classes of class Database defined in /R/fct_classDatabase.R file

    ## --- Justus Weyers 05.09.2024

    # 1. Function definitions

    ## ids() function

    ## Returns the ids of the timeseries.
    ids = function(pt, hashs) {
      pt[pt$name %in% hashs,"id"]
    }

    ## circle_fun() function

    ## Credits @joran:
    ## https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    circle_fun <- function(center = c(0,0),diameter = 1, npoints = 100){
      r = diameter / 2
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }

    ## group() function

    ## Returns the ids of the timeseries.
    group = function(pt, dgt, hashs) {
      gr = pt[pt$name %in% hashs,"dgroup"]
      gr = sapply(gr, function (gkey) dgt[dgt$key == gkey,"name"])
      return(gr)
    }

    ## color() function

    color = function(pt, dgt, hashs) {
      gr = pt[pt$name %in% hashs, "dgroup"]
      gr = sapply(gr, function (gkey) jsonlite::fromJSON(dgt[dgt$key == gkey,"gparam"])[["color"]])
    }

    ## get_timeseries() function

    get_timeseries = function(db) {
      if ("selected_timeseries" %in% user.tables(db)$tablename) {
        t = get.table(db, "selected_timeseries")
        t$timestamp = as.Date(t$timestamp)
        return(t)
      } else {
        return(data.frame())
      }
    }

    ## kommunalitaeten() function

    kommunalitaeten = function(eigenvalues){
      # Calculate percent of total variance
      pTV = eigenvalues / sum(eigenvalues) * 100

      # Make plotting df
      df = data.frame(PC = paste0(1:length(pTV), "."), ev = eigenvalues,
                      pTV = pTV, csum = cumsum(pTV))
      df$PC <- factor(df$PC, levels = df$PC)

      # Crop
      # df = df[1:(length(which(eigenvalues>=1))+1),]

      p = ggplot2::ggplot(df, ggplot2::aes(x = PC, y = csum)) +
        ggplot2::geom_bar(stat="identity", col = "black", fill = "white") +
        ggplot2::geom_text(
          ggplot2::aes(label=format(csum, digits = 1)),
          vjust = 1.6
        )+
        ggplot2::geom_text(
          ggplot2::aes(label = format(round(ev, 2), digits = 2)),
          vjust = -1,
          color = "grey40"
        )+

        ggplot2::theme_minimal()

      return(p)
    }



    # 2. Reactive functions definitions

    hashs = reactive(
      colnames(pca_server$ts)[colnames(pca_server$ts) != "timestamp"]
    )

    z = reactive(
      apply(X = pca_server$ts[,2:ncol(pca_server$ts)], FUN=scale, MARGIN = 2)
    )

    eigenvalues = reactive({
      eigen(stats::cov(z()))$values
    })

    pca = reactive(
      stats::prcomp(z(), center=TRUE, scale.=TRUE, retx=TRUE)
    )

    pcs = reactive({
      pcs_ = data.frame(pca()$x)
      if (r$settings[["flip_pca"]]) {
        pcs_ = pcs_ * (-1)
      }
      pcs_$timestamp = pca_server$ts$timestamp
      return(pcs_)
    })

    loadings = reactive({
      CL = data.frame(pca()$rotation %*% diag(sqrt(eigenvalues())))
      if (r$settings[["flip_pca"]]) {
        CL = CL * (-1)
      }
      colnames(CL) = paste0("PC", seq(1, length(eigenvalues())))
      rownames(CL) = rownames(pca()$rotation)
      return(CL)
    })

    loadings_limited = reactive({
      ll = subset(loadings(), select = eigenvalues()>1)
      rownames(ll) = rownames(loadings())
      return(ll)
    })

    pc_names = shiny::reactive(
      colnames(loadings())
    )

    pc_names_eigen = shiny::reactive(
      pc_names()[eigenvalues()>1]
    )

    first_date = shiny::reactive(min(pca_server$ts$timestamp))

    last_date = shiny::reactive(max(pca_server$ts$timestamp))

    ## geo_loadings() reactive function

    ## Merge loadings with coordinates from metadata.
    geo_loadings = shiny::reactive({
      # Fetch locations via util function
      loc = fetch_metadata(
        metadata = selected_metadata(), #? selected_metadata
        ids = ids(pca_server$primary_table, rownames(loadings())),
        field = c("name", "latitude", "longitude")
      )

      loads = loadings()
      loads$id = ids(pca_server$primary_table, rownames(loads))
      loads$group = group(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
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

    pcsplot = reactive({
      if (length(input$pc_plot_pcs)>=1) {
        df = pcs()[,c("timestamp", input$pc_plot_pcs)]
        df = tidyr::pivot_longer(data = df, cols = input$pc_plot_pcs)
        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x = timestamp, y = value, colour = name)) +
          ggplot2::geom_line() +
          ggplot2::theme_minimal()

        return(plot)
      }
    })

    ## metadata_names() reactive function

    ## Return database names of metadata
    metadata_names = shiny::reactive({
      # Fetch coy of primary_table
      pt = pca_server$primary_table
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
    selected_metadata = reactive({
      l = lapply(metadata(), function(df) {
        if ("id" %in% colnames(df)) {
          d = df[df$id %in% ids(pca_server$primary_table, hashs()),]
          return(d)
        }
      })
      l[sapply(l, is.null)] = NULL
      return(l)
    })

    roi = shiny::reactive({
      pt = pca_server$primary_table
      dgt = pca_server$datagroup_table
      dgkey = dgt[dgt$name ==  "Untersuchungsgebiet", "key"]

      if (any(pt$dgroup == dgkey)) {
        feature = sf::st_read(r$db@con, pt[which(pt$dgroup == dgkey)[1], "name"])
        return(sf::st_transform(feature, sf::st_crs(r$settings[["crs"]])))
      } else {
        return(NULL)
      }

    })

    combiplot = shiny::reactive({
      df = pcs()[,c("timestamp", "PC1")]
      df$combi = (1-input$weight) * df$PC1 + input$weight * pcs()[,input$combi_pc]
      colnames(df) = c("timestamp", "PC1", "combi")

      i = lubridate::interval(input$date_combi_slider[1], input$date_combi_slider[2])
      df = subset(x = df, subset = lubridate::`%within%`(df$timestamp, i))

      p = ggplot2::ggplot(df, ggplot2::aes(x = timestamp)) +
        ggplot2::geom_line(ggplot2::aes(y = PC1), col = "grey") +
        ggplot2::geom_line(ggplot2::aes(y = combi), col = "#F8766D") +
        ggplot2::theme_minimal()
      return(p)
    })

    damping = shiny::reactive({
      if (is.null(input$damping_pc)) return(NULL)
      plotdf = data.frame(
        CL1 = loadings()[,"PC1"],
        CL2 = loadings()[,input$damping_pc],
        id = rownames(loadings())
      )
      plotdf$damping_coef = round(atan(plotdf$CL2/plotdf$CL1) * 180/pi, 2)
      plotdf$group = group(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = plotdf$id
      )
      plotdf$color = color(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = plotdf$id
      )
      return(plotdf)
    })

    refhydr = shiny::reactive({
      linmodel = function(df) {
        lm(ts~., data = df)
      }

      pcs = pcs()[,input$regression_pcs]

      df = tibble::tibble(hashs = hashs())

      df$data = sapply(df$hashs, function(hash) {
        df = data.frame(cbind(pca_server$ts[,hash], pcs))
        colnames(df) = c("ts", input$regression_pcs)
        return(df)
      }, simplify = FALSE)

      df = dplyr::mutate(df, model = data |> purrr::map(linmodel))

      df = df |> dplyr::mutate(
        glance = purrr::map(model, broom::glance),
        rsq = glance |> purrr::map_dbl("r.squared"),
        tidy = purrr::map(model, broom::tidy),
        augment = purrr::map(model, broom::augment),
        sm = purrr::map(model, summary)
      )

      df = df |> dplyr::arrange(dplyr::desc(rsq))

      return(df)
    })

    output$ref_ts_plot = shiny::renderPlot({
      if (is.null(pca_server$selected_id)) return(NULL)
      plotdf = refhydr()[refhydr()$hashs == pca_server$selected_id,] |>
        tidyr::unnest(augment)
      plotdf$timestamp = pca_server$ts$timestamp
      p = ggplot2::ggplot(plotdf) +
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = ts), color = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = .fitted)) +
        ggplot2::theme_minimal()
      return(p)
    })

    output$ref_bar_plot = shiny::renderPlot({
      plotdf = refhydr()
      ggplot2::ggplot(plotdf, ggplot2::aes(x = stats::reorder(hashs, rsq), y = rsq)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::ylim(c(0, 1)) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal()
    })

    output$ref_summary = shiny::renderPrint({
      if (!is.null(pca_server$selected_id)) {
        sm_ = refhydr() |>
          dplyr::filter(hashs == pca_server$selected_id) |>
          dplyr::select(sm)
        return(sm_$sm)
      }
    })

    # 3. Reactive values initialization

    # PCA server's reactive values
    pca_server = shiny::reactiveValues(
      primary_table = get.table(shiny::isolate(r$db), "primary_table"),
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table"),
      ts = get_timeseries(shiny::isolate(r$db)),
      selected_id = NULL
    )

    # 4. Server logic via observers

    observeEvent(r$cache_selection_trigger, {
      print("pca server observed cache")
      pca_server$primary_table = get.table(shiny::isolate(r$db), "primary_table")
      datagroup_table = get.table(shiny::isolate(r$db), "datagroup_table")
      pca_server$ts = get_timeseries(r$db)
    })

    observeEvent(input$flip_pca, {
      r$settings[["flip_pca"]] = input$flip_pca
    })

    observeEvent(input$circle_click, {
      pca_server$selected_id = shiny::nearPoints(
        circle_plot_df(),
        input$circle_click,
        addDist = TRUE
      )[,3]
    })

    observeEvent(input$bar_click, {
      i = round(input$bar_click$y)
      pca_server$selected_id = refhydr()$hashs[order(refhydr()$rsq)][i]
    })


    # 5. UI elements rendering

    output$daterange_slider = shiny::renderUI(
      shiny::sliderInput(
        inputId = ns("date_combi_slider"),
        "Slider",
        min = first_date(),
        max = last_date(),
        value = c(first_date(), last_date()),
        width = "100%"
      )
    )

    ## ggf. irgendwann loeschen (auch ui)
    output$selected_id = shiny::renderText(
      toString(pca_server$selected_id)
    )

    output$ui_loadings = DT::renderDataTable(
      geo_loadings(), selection = 'single'
    )

    output$ui_circle_plot = shiny::renderPlot({
      if (is.null(circle_plot_df())) return(NULL)
      ggplot2::ggplot() +
        ggplot2::geom_point(
          data = circle_plot_df(),
          colour = circle_plot_df()$color,
          cex = 4,
          ggplot2::aes(
            x = PCA,
            y = PCB,
            pch = group)
        ) +
        ggplot2::geom_path(
          data = circle_fun(c(0,0), 2, npoints = 100),
          ggplot2::aes(x, y)
        ) +
        ggplot2::coord_fixed(
          ratio = 1,
          xlim = c(-1, 1),
          ylim = c(-1, 1)
        ) +
        ggplot2::xlab(paste(r$txt[[92]], "PC1")) +
        ggplot2::ylab(paste(r$txt[[92]], input$damping_pc)) +
        ggplot2::theme_minimal()
    }, height = 500, width = 500)

    output$ui_weight_slider = shiny::renderUI(
      shiny::sliderInput(
        inputId = ns("weight"),
        label = paste(r$txt[[90]], input$combi_pc),
        min = 0,
        max = 1,
        value = 1/sqrt(2),
        step = 1/(10*sqrt(2)),
        round	= 2
      )
    )

    output$ui_damping_pc = shiny::renderUI(
      shiny::selectInput(
        inputId = ns("damping_pc"),
        label = r$txt[[87]],
        choices = setdiff(pc_names(), "PC1")
      )
    )

    output$ui_combi_pc = shiny::renderUI(
      shiny::selectInput(
        inputId = ns("combi_pc"),
        label = r$txt[[87]],
        choices = setdiff(pc_names(), "PC1")
      )
    )

    output$ui_combiplot = shiny::renderPlot(
      combiplot()
    )

    output$pc_plot_select = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("pc_plot_pcs"),
        label = r$txt[[89]],
        choices = pc_names(),
        selected = pc_names_eigen(),
        inline = TRUE
      )
    )

    output$ui_geo_loadings_plot = shiny::renderPlot({
      if (!(nrow(geo_loadings())>=1)) return(NULL)
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
        ggplot2::scale_colour_gradient2(low = "blue", mid = "grey90", high = "red", limits=c(-1,1)) # +
        # ggplot2::scale_shape_manual()


      p = p + ggplot2::theme_minimal()

      if(length(input$ui_loadings_rows_selected) > 0){
        plotdf_selected = plotdf[input$ui_loadings_rows_selected,]
        p = p + ggplot2::geom_sf(
          data = plotdf_selected,
          pch = 21,
          col = "green",
          cex = 7,
          stroke = 3,
          fill = "transparent")
      }



      return(p)
    }, height = 800, width = 800)


    output$ui_damping_plot = shiny::renderPlot({
      if (is.null(damping())) return(NULL)
      p = ggplot2::ggplot() +
        ggplot2::geom_boxplot(
          data = damping(),
          ggplot2::aes(x = group, y = damping_coef)
        ) +
        ggplot2::theme_minimal()
      if (!is.null(pca_server$selected_id)) {
        p = p + ggplot2::geom_point(
          data = damping()[damping()$id == pca_server$selected_id,],
          ggplot2::aes(x = group, y = damping_coef)
        )
      }
      return(p)
    }, width = 500, height = 500)

    output$select_PC = shiny::renderUI({
      shiny::selectInput(
        inputId = ns("select_PC"),
        label = "Select a PC",
        choices = pc_names())
    })

    output$ui_flip_PCA = shiny::renderUI({
      checkboxInput(
        inputId = ns("flip_pca"),
        label = r$txt[[84]],
        value = shiny::isolate(r$settings[["flip_pca"]]))
    })


    output$plotPercentofVariance = renderPlot({
      kommunalitaeten(eigenvalues())
    })

    output$pcs = renderPlot({
      pcsplot()
    })

    output$ui_header1 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[65]])
    })

    output$ui_header2 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[64]])
    })

    output$ui_header3 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[85]])
    })

    output$ui_header4 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[91]])
    })

    output$ui_header5 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[63]])
    })

    output$ui_header6 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[88]])
    })

    output$ui_header7 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[95]])
    })

    output$ui_header8 <- shiny::renderUI({
      shiny::titlePanel(r$txt[[94]])
    })

    output$ui_regression_pcs = shiny::renderUI(
      shiny::checkboxGroupInput(
        inputId = ns("regression_pcs"),
        label = "Choose PC's for linear regression",
        choices = pc_names(),
        selected = pc_names()[eigenvalues()>1],
        inline = TRUE,
      )
    )

    corplotdf = shiny::reactive({
      print("# --- Cor ---------")

      l = lapply(selected_metadata(), function(df)
        if(input$corr_data %in% colnames(df)) {
          return(df[,c("id", input$corr_data)])
        } else {
          return(NULL)
        }
      )


      df = dplyr::distinct(dplyr::bind_rows(l))

      idhash = na.omit(pca_server$primary_table[,c("name", "id")])

      df$id = purrr::map_vec(df$id, function(id) {
        print(id)
        idhash$name[idhash$id == id]
      })

      print("SUMMARIZE")

      print(head(df))

      colnames(df) = c("id", "data")

      if (is(df$data, "numeric")) {
        df = df |>
          dplyr::group_by(id) |>
          dplyr::summarise(data = mean(data))
      }

      if (is(df$data, "character")) {
        df = df |>
          dplyr::group_by(id) |>
          dplyr::summarise(data = paste(data, collapse = ", "))
      }

      print(head(df))

      df$group = group(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = df$id
      )

      df$color = color(
        pt = pca_server$primary_table,
        dgt = pca_server$datagroup_table,
        hashs = df$id
      )

      df = merge(df, data.frame(loadings()), by.x = "id", by.y = "row.names")

      df = df[,c("id", "data", "group", "color", input$corr_pc)]

      colnames(df) = c("id", "data", "group", "color", "pc")

      return(df)
    })

    output$ui_corr_plot = shiny::renderPlot({

      if (is(corplotdf()$data, "numeric")) {
        p = ggplot2::ggplot(corplotdf(), ggplot2::aes(x = pc, y = data)) +
          ggplot2::geom_point() +
          ggplot2::theme_minimal()

        return(p)
      }
      if (is(corplotdf()$data, "character")) {
        p = ggplot2::ggplot(corplotdf(), ggplot2::aes(x = data, y = pc)) +
          ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
          ggplot2::geom_boxplot() +
          ggplot2::theme_minimal()
        return(p)
      }
    })

    output$ui_choose_corr <- shiny::renderUI({
      shiny::fluidRow(
        col_6(
          shiny::selectInput(inputId = ns("corr_pc"), label = r$txt[[96]], choices = pc_names())
        ),
        col_6(
          shiny::selectInput(inputId = ns("corr_data"), label = r$txt[[96]], choices = unique(unlist(sapply(selected_metadata(), colnames))))
        )
      )
    })

    output$ui_tab_title_pca <- shiny::renderText(r$txt[55])

    output$ui_tab_title_loadings <- shiny::renderText(r$txt[63])

    output$ui_tab_title_combinations <- shiny::renderText(r$txt[86])

    output$ui_tab_title_damping <- shiny::renderText(r$txt[91])

    output$ui_tab_title_regression <- shiny::renderText(r$txt[93])

    output$ui_tab_title_correlations <- shiny::renderText(r$txt[94])

  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")
