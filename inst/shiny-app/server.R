# Define server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=2000*1024^2)

###############################
token = "ce42ee6bc3ba4e9c4c5a63f70f400447"

rv = reactiveValues()

observeEvent(input$connexion, {
  con = patRunifi::create_connection_params(apihosturl = "http://localhost:50034/unifi/v1", identityurl = "http://localhost:50333/identity/connect/token" )
  if (!is.null(con)) {
    output$connexion_status <- renderText("Connexion réussie")
  } else {
    output$connexion_status <- renderText("Échec de la connexion")
  }
  
})

observeEvent(input$processing_2, {


  folders = patRunifi::folders_research(con)

      folders = data.frame(subset(folders, select = -c(path, folderType, parentId)))


      rv$folders = folders

      output$tablefolder = renderDT(
        rv$folders,
        style = "bootstrap",
        filter = "top",
        options = list(scrollX = TRUE),
        selection = "single"
                )

        observeEvent(input$tablefolder_rows_selected, {
          row_chosen= input$tablefolder_rows_selected
          table_row_chosen = folders[row_chosen,]
          id_chosen = table_row_chosen$id
          analysis_id = patRunifi::items_research(con, id_chosen)
          rv$analysis_id = analysis_id

          output$tableanalysisId = renderDT(
            rv$analysis_id,
            style = "bootstrap",
            filter = "top",
            options = list(scrollX = TRUE),
            selection = "single"
                    )
        }) #end of observeEvent rows_selected


          observeEvent(input$tableanalysisId_rows_selected, {
          row_chosen= input$tableanalysisId_rows_selected
          analysis_id = rv$analysis_id
          table_row_chosen = analysis_id[row_chosen,]
          id_chosen = table_row_chosen$id
          rv$analysis_id_chosen = id_chosen
          analysis_name = table_row_chosen$name

          rv$analysis_name = analysis_name
          sample = patRunifi::sample_research(con, id_chosen)
          rv$sample = sample

          }) #end of observeEvent rows_selected


          output$tableRow_3 = renderDT(
            rv$sample[,1:2],
            style = "bootstrap",
            filter = "top",
            options = list(scrollX = TRUE),
            selection = "single"
                    )

          observeEvent(input$tableRow_3_rows_selected, {
          row_chosen = input$tableRow_3_rows_selected

          table_row_chosen = rv$sample[row_chosen,]
          id_chosen = table_row_chosen$id
          name_chosen = table_row_chosen$name

          rv$sample_id = id_chosen
          rv$name_chosen = name_chosen

          sampleid = rv$sample_id
          sample = rv$samples

          output$sampleid_text <- renderText({
            sample_id <- rv$sample_id
            sample_name <- rv$sample_name

            if (!is.null(sample_id)) {
              text <- paste("Selected Analysis:", rv$analysis_name, "\nSelected Sample Name:", name_chosen)
              HTML(text)
            } else {
              "No sample selected"
            }
          })

          observeEvent(input$collect_one, {
            progressMessage <- paste("Collecting data for sample:", samplename, "This may take a while...")
            
            withProgress(message = progressMessage, value = 0, {
              incProgress(1, detail = "Please wait", session = session)
              patRunifi::collect_one_sample_data(connection_params = con, sampleid = sampleid, samplename = samplename, analysis_name = analysis_name)
                            setProgress(1)
            })
          })
          
          
          
          observeEvent(input$collect_all, {
            progressMessage <- paste("Collecting all sample of :", analysis_name, "This may take a while...")
            
            withProgress(message = progressMessage, value = 0, {
              incProgress(1, detail = "Please wait", session = session)
              patRunifi::collect_all_samples_data(connection_params = con,analysis_id = rv$analysis_id_chosen )
              setProgress(1)
            })
          })
          
          


        })





})# End of the observeEvent

} #end of server function
