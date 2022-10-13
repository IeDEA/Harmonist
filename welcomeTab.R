observeEvent(input$goToHelp,{
  updateTabItems(session,"tabs", "help")
})

output$welcomeUI <- renderUI({
  if (projectDef$hub_y == "1"){
    submitIntro <- paste0(" This option is available only if you entered the toolkit by selecting an active data request in the ",
    networkName,
    " Hub. You may explain remaining data quality errors or warnings and click to share your dataset with the investigator who created the data request.")
  } else {
    submitIntro <- "This option is only available to research networks with a Harmonist project management Hub."
  }
  
  if (networkName == "IeDEA"){
    welcomeInfo <- tagList(
      tags$p("This toolkit is designed to facilitate data quality checking, reporting, and sharing of HIV observational datasets that conform to the ", 
             a("IeDEA Data Exchange Standard (IeDEA DES)", 
               href="http://iedeades.org", target="_blank"), 
             " for approved research concepts in the IeDEA network. The toolkit may be accessed in two ways:"),
      tags$ul(
        tags$li(tags$b("Visiting ",a("IeDEA Harmonist Data Toolkit",
                                     href="http://iedeadata.org/iedea-harmonist", target="_blank"),
                       " directly."),
                " Available options: Upload IeDEA tables, conduct and review data quality checks, download reports, ",
                "and view plots of variable in shared tables. ", 
                "When the Toolkit session ends (either by closing the browser or selecting Exit from the menu) the uploaded tables are deleted."),
        tags$li(tags$b("Logging in to the ",
                       a(" IeDEA Hub", href="http://iedeahub.org", target="_blank"),
                       " and choosing an active data request (concept)."),
                " An additional option is available in this case: If the data quality checks are successful, ",
                "the dataset can be submitted to secure cloud storage for  retrieval by the author of the selected concept.", 
                " All files will be deleted after retrieval or 30 days. ")
      )
    )
  } else {
    welcomeInfo <- tags$p(
      "This toolkit is designed to facilitate data quality checking, reporting, and sharing of observational datasets that conform to the ",
      networkName,
      " common data model"
    )
  }
  # note below that path for href for sample dataset is relative to www
  
  return(
    fluidPage(
      tags$h3(class = "row_text title",tags$strong(span("Introduction ", class = "text-grey")),
              " Toolkit Overview", backToHubMessage()),
      fluidRow(
        class = "rowNoActiveDataRequest",
        box(
          solidHeader = TRUE,
          width = 12,
          welcomeInfo
        )
      ),
      fluidRow(
        class = "rowNoActiveDataRequest",
        box(
          solidHeader = TRUE,
          title = "Data Toolkit Steps",
          width = 12,
          tagList(
            tags$p("Download this ",
                   tags$a("Sample dataset ", href = "projectFiles/sample_dataset.zip", target = "_blank"), 
                   " to test the Toolkit and learn about its functions. ",
                   "View the ", 
                   tags$a("Harmonist Data Toolkit Video ", 
                          href = "https://youtu.be/pL_RRhvzX-w", target="_blank"), 
                   " for a demonstration. Please visit the",
                   actionLink("goToHelp", "Help page"), 
                   "for additional resources."),
            tags$ul(
              tags$li(
                tags$b("Step 1: Upload files"), " Browse and select your files containing the",
                networkName,
                " tables you would like to check for data quality"
              ),
              tags$li(
                tags$b("Step 2: Check data")," Use the interactive summary of possible inconsistencies found in the dataset and download spreadsheets to review details offline"
              ),
              tags$li(
                tags$b("Step 3: Create reports")," Choose the desired report format and report contents and generate reports summarizing the dataset you uploaded"
              ),
              tags$li(
                tags$b("Step 4: Submit data"), submitIntro
              ),
              tags$li(
                tags$b("Visualize data"), " Choose tables and variables to plot to aid in better understanding your dataset"
              )
            )
          )
          
        )
      ),
      actionButton("fromWelcomeToStep1", "Continue to Step 1: Upload files", class="container-left btn-success")
    )
  )

})
