output$reportPage <- renderUI({
  if (resetFileInput$reset) {
    return(tags$h3(notReadyMessage))
  }
  if (is.null(errorTable())){
    return(tags$h3(notReadyMessage))
  }
  if (is.null(errorTable()[[1]])){
    return(tags$h3(notReadyMessage))
  }
  req(errorTable()[[1]])
  if (hubInfo$fromHub){
    step4UI <- actionButton("step4", "Continue to Step 4: Submit data")
  } else step4UI <- NULL
  
  return(
    fluidPage(
      tags$h3(class = "pleft title",tags$strong(span("STEP 3 ", class = "text-blue")),
              " Create summary", backToHubMessage()),
      tags$h5(class = "pleft subtitle", step3Subtitle),
      
      fluidRow(
        column(width = 4,
               
               "Report format options",
               selectInput("reportType","Choose type of report to download",choices = c("pdf","html"), selected = "pdf"),
               uiOutput("programsToInclude"),
               textInput("datasetDesc","Enter a brief description of this dataset to include on the cover of your report")
        ),
        column(width = 5,
               "Report content",
               checkboxInput("includeDataSummary", "Summary statistics of tables", value = TRUE),
               checkboxInput("includeErrorSummary", "Summary of data quality checks", value = TRUE),
               checkboxInput("includeHistograms", "Histograms of dates", value = TRUE),
               fluidRow(uiOutput("histOptions"))
               
        )
      ),
      fluidRow(column(4,offset = 2,uiOutput("downloadReport"))),
      tags$br(),
      tags$br(),
      step4UI
    )
  )
})

