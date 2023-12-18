# Define server logic

server <- function(input, output, session) {
  options(shiny.maxRequestSize=2000*1024^2)

  # UNIFI API panel
  # ================

  rv = reactiveValues()

  observeEvent(input$connecttounifi, {
      spsComps::shinyCatch({
        rv$con = patRunifi::create_connection_params(apihosturl = input$serverurl, identityurl = input$authorizationurl, username = input$unifiuser, password = input$unifipwd)
    }, prefix = "", blocking_level = "error")
  })

  observeEvent(rv$con, {
    if(!is.null(rv$con)) {
      output$connectionInfobox <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox("Connected to the UNIFI API",
      tags$span(p("Navigate in the folders and data in the", style = "display: inline"), actionLink("switch_tab", "Navigate tab", style = "color: white; text-decoration: underline; display: inline")),
        icon = icon("thumbs-up", lib = "glyphicon"), color = "green", fill = T
      )
    })
    }
  })

  observeEvent(input$switch_tab, {
      shinydashboard::updateTabItems(session, "inTab", selected = "navigate_folders")
    })


# Navigation panel
# ================

  observeEvent(input$loadfolders, {
        # js$loadFolders('foldersDiv')

            rv$folders = parquetMS::folders_search(rv$con)
            nodes = makeNodes(rv$folders$path)
            output$jstreefolders <- jsTreeR::renderJstree(suppressMessages(jsTreeR::jstree(nodes, theme = "proton")))
      })
      observe({
        rv$sel = input[["jstreefolders_selected"]]
    })

  output$folders <- renderUI({
      tags$span(HTML('<div id="foldersDiv"></div>'))
  })

  analysisList <- shiny::eventReactive(input$jstreefolders_selected, {
    req(input$jstreefolders_selected)
    print(input$jstreefolders_selected)
    if(length(input$jstreefolders_selected) != 0) {
    df = rv$folders
    selfolder = input$jstreefolders_selected[[1]]$text
    folderid = df[df$name %in% selfolder,]
    folderid = folderid[,c("id")]
    rv$analysislist = parquetMS::analysis_search(folderid, rv$con)
    analysisList <- data.table::as.data.table(rv$analysislist)
    return(analysisList)
    }
  })

  analysisDt <- reactive({
    req(analysisList())
    analysisDt1 <- analysisList()
    analysisDtCols <- c("id", "name", "description")
    analysisDt <- analysisDt1[,..analysisDtCols]
    return(analysisDt)
  })

  output$analysis_datatable <- DT::renderDT(
    analysisDt(),
    selection = "single"
    )

  # the selected analysis from Unifi
  selAnalysis <- reactive({
    req(input$analysis_datatable_rows_selected)
    s = input$analysis_datatable_rows_selected
    analysisList <- analysisList()
    if(length(s)) {
      sel <- analysisList[s,]
    }
    return(sel)
  })

  observeEvent(selAnalysis(), {

    sel <- selAnalysis()
      analysisId <- unlist(sel[,c("id")])
      rv$samples_list <- parquetMS::get_samples_list(analysisId, rv$con)

          }, ignoreNULL = TRUE)

  output$samples_datatable <- DT::renderDT(
    rv$samples_list,
    style = "bootstrap",
    options = list(scrollX = TRUE),
    selection = "single"
    )

  # the selected sample from Unifi
  selSample <- reactive({
    # req(input$samples_datatable_rows_selected)
    s = input$samples_datatable_rows_selected
    samples_list <- rv$samples_list
    if(length(s)) {
      sel <- samples_list[s,]
      return(sel)
    }
  })

  ## This will not work to choose a folder to save files on client side
  ## if app is running on a  server
  shinyFiles::shinyDirChoose(
    input,
    'selected_dir',
    roots = c(home = '~')
  )

  selected_dir <- reactive(input$selected_dir)

  output$selected_dir <- renderText({
    shinyFiles::parseDirPath(c(home = '~'), selected_dir())
  })
  # convert one sample
  observeEvent(input$convert_one, {
    # validate(need(length(input$samples_datatable_rows_selected) > 0, message = "No sample selected - please first select a sample in the table above"))
    sel <- selSample()
    sampleId <- unlist(sel[,c("id")])
    if(input$fileFormat == 1) format = "parquet" else format = "hdf5"
    progressr::withProgressShiny(message = "Conversion in progress", {
    spsComps::shinyCatch({
      # sampleId = "0134efbf-c75a-411b-842a-4f35e2b76347"
          collected_data = parquetMS::collect_one_sample_data(sampleId, rv$con, num_spectras = 5)
          parquetMS::save_one_sample_data(collected_data, path = shinyFiles::parseDirPath(c(home = '~'), selected_dir()), format = format)
    },
      prefix = "", blocking_level = "none"
    )
  })

  # output$conversion_end_message = renderText(glue::glue("Sample converted and saved!"))
  })

  # convert all samples from a selected Analysis
  observeEvent(input$convert_all, {
    # validate(need(length(input$samples_datatable_rows_selected) > 0, message = "No sample selected - please first select a sample in the table above"))
    shinyjs::disable("convert_all")
    shinyjs::disable("convert_one")

        sel <- selAnalysis()
        analysisId <- unlist(sel[,c("id")])
        # analysisId = "e236bf99-31cd-44ae-a4e7-74915697df65"
        if(input$fileFormat == 1) format = "parquet" else format = "hdf5"
        progressr::withProgressShiny(message = "Conversion in progress", {
          spsComps::shinyCatch({
              parquetMS::convert_all_samples_data(analysisId, rv$con, format = format, path = shinyFiles::parseDirPath(c(home = '~'), selected_dir()), num_spectra = 2)
          },
            prefix = "", blocking_level = "error"
          )
  })
  shinyjs::enable("convert_all")
  shinyjs::enable("convert_one")
  output$conversion_end_message = renderText(glue::glue("Sample converted and saved!"))
  })

} #end of server function
