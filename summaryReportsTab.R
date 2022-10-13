# if user decides to upload new dataset, unlink previously loaded dataset and change reset to TRUE
observeEvent(input$yesRestart3,{
  lastActivity(Sys.time())
  #unlink(infile()$files$datapath)
  cleanupFiles(infile())
  startDQ(NULL)
  resetFileInput$reset <- TRUE
  resetFileInput$newData <- FALSE
  errorCount(0)
  errorExcess(FALSE)
  useSampleData(FALSE)
  submitSuccess(NULL)
  tablesAndVariables <- NULL
  groupByChoice(defGroupVar)
  currentGroupSelection(NULL)
  finalGroupChoice(NULL)
  groupByInfo <- NULL
  resetHistoRange()
  updateTabItems(session, "tabs", "upload")
  removeModal()
})

observeEvent(input$uploadNew3,{
  showModal(modalDialog(
    title = div(tags$b("Restart Confirmation"), style = "color: #605ea6;"),
    fluidRow(
      box(
        width = 12,
        tags$p("Are you sure? The current dataset and data quality check results will be deleted from memory if you continue. ")
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("yesRestart3", "Continue")
    )
  ))
})

observeEvent(input$returnStep2,{
  updateTabItems(session, "tabs", "reviewerror")
})

output$reportPage <- renderUI({
  if (resetFileInput$reset) {
    return(tags$h3(class = "row_text title", notReadyMessage))
  }
  if (is.null(errorTable())){
    return(tags$h3(class = "row_text title", notReadyMessage))
  }
  if (is.null(errorTable()[[1]])){
    return(tags$h3(class = "row_text title",notReadyMessage))
  }
  req(errorTable()[[1]])

  restartBox <- box(
    width = 5,
    title = span("Restart session"),
    tagList(
      tags$p("Start over and upload a", tags$b("revised or different dataset.")),
      actionButton("uploadNew3","Upload new dataset")
    )
  )
  if (hubInfo$fromHub){
    step4UI <- fluidRow(class = "rowUploadComplete",
                        box(
                          width = 5,
                            title = span("Continue to Submit Data", style = "color: white"),
                            solidHeader = TRUE,
                            status = "success",
                            tagList(
                              tags$p("When you are ready to submit this dataset for",
                                    tags$b(userDetails()$uploadconcept_mr, ","),
                                     "continue to the next step."),
                              tags$p(tags$em('Note: A default "all groups" report will be stored when you submit your dataset. This report is accessible via the', networkName, 'Hub.')),

                              actionButton("step4", "Continue to Step 4", class="btn-success")
                            )
                          ),
                        restartBox
    )
  } else {
    step4UI <- tagList(fluidRow(#class = "rowUploadComplete",
                        box(
                          width = 10,
                          title = span("Note"),
                          tagList(
                            tags$p(tags$b("Step 4: Submit Data"), "is available only when responding to an active data request through the",
                                   a(" IeDEA Hub.", href="http://iedeahub.org", target="_blank"))
                          ))),
    fluidRow(
                        box(
                          width = 5,
                          title = "Revisit Data Quality Results",
                          tags$p("View data quality check results interactively or download details to explore offline."),
                          actionButton("returnStep2", "Return to Step 2")
                        ),
                        restartBox
    )
    )
  }
  
  dqRequestedVarText <- ifelse(hubInfo$fromHub,
                               paste("Include variables requested by", userDetails()$uploadconcept_mr),
                               paste0("Include only ", indexTableName, " (required variables)"))
  defaultReportContent <- ifelse(
    hubInfo$fromHub,
    "By default, only requested variables (i.e., variables specified for a data call) are summarized.",
    paste0("By default, the report summarizes the required variables in ", indexTableName, ".")
  )
  
  if (networkName == "IeDEA"){
    dqMetricsUI <-       fluidRow(
      box(
        width = 10,
        title = "Quality Metrics Report",
        fluidRow(column(width = 8,
                        tags$p("This report presents simple visualizations of three data quality metrics: DES compliance, logical consistency of data, and data completeness. Metrics are presented for each group and DES table in the submitted dataset.", defaultReportContent),
                        radioButtons("dqVars",
                                     "",
                                     choiceValues = c("dqRequested", "dqAll"),
                                     choiceNames = c(
                                       dqRequestedVarText,
                                       "Include all IeDEA DES variables in this dataset"
                                     ))),
                        # textInput("reportDesc",
                        #           "(Optional) Short title for report heading")),
                 column(width = 2,
                        img(src="reportIcon2.png", class="report_icon")
                 )
        ),
        fluidRow(
          column(
            width = 2,
            offset = 3,
            downloadButton("DQMetrics", "Generate Quality Metrics Report")
          )
        )
      )
    )
  } else {
    dqMetricsUI <- fluidRow(tags$p(""))
  }
  
  return(
    tagList(
      tags$h3(class = "row_text title",tags$strong(span("STEP 3 ", class = "text-blue")),
              " Create reports", backToHubMessage()),
      tags$h5(class = "row_text subtitle", 
              "Generate and download customized reports summarizing uploaded dataset content and quality."),
      fluidRow(
        box(
          width = 10,
          title = "Data Summary Report",
          
          fluidRow(
            column(width = 8,
                   tags$p("This customizable data report summarizes uploaded",
                          networkName,
                          projectDef$datamodel_abbrev,
                          "tables and patient counts per table. Optional report content includes summary statistics, date histograms, and data quality check summaries. Reports can be generated for individual groups (e.g., sites, programs) or all groups combined."),
                   fluidRow(
                     column(width = 6,
                            # tags$b("Customize report"),
                            selectInput("reportType","File format for report",
                                        choices = c("PDF" = "pdf", "HTML" = "html")),
                            uiOutput("programsToInclude") #,
                            #uiOutput("chooseMultipleGroups"),
                            # textInput("datasetDesc",
                            #           "(Optional) Short title for report heading")
                     ),
                     column(width = 6,
                            offset = 0,
                            tags$b("Select report content"),
                            checkboxInput("includeDataSummary", "Summary statistics of tables", value = TRUE),
                            checkboxInput("includeHistograms", "Histograms of dates", value = TRUE),
                            checkboxInput("includeErrorSummary", "Summary of data quality checks", value = TRUE),
                            #fluidRow(uiOutput("histOptions"))                   
                            uiOutput("histOptions")
                            
                            
                     ))),
            column(width = 2,
                   img(src="reportIcon.png", class="report_icon")
            )),
          fluidRow(column(1, offset = 3, uiOutput("downloadReport")))
        )),
      # if network is IeDEA, dqMetrics report option should be available
      dqMetricsUI,
# uncomment when prepared to add in africa dashboard option
     # fluidRow(
      #  box(
      #    width = 10,
      #    title = "Africa Dashboard Aggregation",
      #    fluidRow(
      #      selectInput("regionchoice", "Select African Region:", 
      #                  choices = c("CA", "EA", "SA-SA", "SA", "WA"), 
       #                 selected = NULL, width = "20%")
      #      ),
      #    fluidRow(
      #      column(
      #        width = 2,
      #        offset = 3,
      #        downloadButton("aggregations", "Generate dashboard aggregations")
      #      )
      #    )
      #  )
     # ),
      step4UI
    )
  )
})

