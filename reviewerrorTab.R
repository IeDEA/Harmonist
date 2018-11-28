# helper function 
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

# summaryToShow: generate list of summary tables of errors by table, with badges and functioning buttons added
summaryToShow <- reactive({
  req(errorTable())
  errors <- errorTable()$errorsByTable
  errorSummary <- errorTable()$errorSummary
  summaryToShow <- list()
  
  for (tableName in names(formattedTables())){
    summaryOut <- NULL
    if (tableName %in% names(errors)){
      summaryOut <- errorSummary %>% filter(table == tableName) %>% 
        arrange(category) %>% 
        arrange(severity) %>% 
        mutate("Error" = paste(category,": ", variable))
      colorLevel <- sapply(summaryOut$severity, function(x){
        if (x=="Error") {return("red")}
        else return("orange")
      })
      summaryOut[["Severity"]] <-
        paste0('<span class="badge" style="background-color: ', colorLevel,'">',summaryOut$severity,'</span>')
      
      summaryOut[["Option"]] <- 
        shinyInput(actionButton, nrow(summaryOut), 'button_', 
                   label = "View Detail", 
                   onclick = 'Shiny.onInputChange(\"select_button\",  this.id); Shiny.onInputChange(\"lastClick\", Math.random())' )
      
    } else {summaryOut <- 0}
    summaryToShow[[tableName]] <- summaryOut
  }
  return(summaryToShow)
})


# display error summary interactively in separate tables, by table-----------
output$errorSummarySep <- renderUI({
  if (resetFileInput$reset) {
    return(tags$h3(notReadyMessage))
  }
  if (is.null(uploadList())){
    return(tags$h3(notReadyMessage))
  }
  if (is.null(errorTable())){
    return(tags$h3(notReadyMessage))
  }
  if (nrow(errorTable()$errorDetail) == 0){
    return(tags$h3("No errors or warnings detected in dataset"))
  }
  
  req(summaryToShow())
  
  errors <- errorTable()$errorsByTable
  errorSummary <- errorTable()$errorSummary
  
   if (isolate(errorExcess())){
     moreErrorsMessage <- tags$p(tags$strong("Note:"), " The number of errors detected (", 
                                 format(isolate(errorCount()), scientific = FALSE, big.mark = ","),
                                  ") exceeds the current capacity of the Toolkit. Only the first ",
                                 format(errorLimit, scientific = FALSE, big.mark=","), "errors are included in this report.")
   } else moreErrorsMessage <- NULL

  boxArgs <- lapply(uploadList()$AllDESTables, function(tableName) {
    if (tableName %in% names(errors)){
      summary <- summaryToShow()[[tableName]][,c("Error","Severity", "number", "Option")]
      numberOfErrors <- nrow(errors[[tableName]])
      print(numberOfErrors)
  
      output[[paste0("summary_",tableName)]] <- 
        DT::renderDataTable(summary, 
                            selection = "single", rownames = FALSE,
                            colnames = c("Error" = "Error","Severity" = "Severity", "Count" = "number"," " = "Option"),
                            escape = FALSE,
                             options = list(
                               columnDefs = list(
                                 list(orderable = FALSE, className = "dt-center", targets = 3), # disable ordering for View Detail column
                                 list(className = "dt-center", targets = 1)
                               )
                             )
                           )
        
      tabPanel(title = tagList(span(tableName, style="font-weight: bold; font-size: smaller"), 
                               span(numberOfErrors, class="badge", style="background-color: red")), 
               DT::dataTableOutput(paste0("summary_",tableName))
      )
    }
    else{
      tabPanel(title = tagList(span(tableName, style="font-weight: bold; font-size: smaller; color: green"), shiny::icon("check-square-o")), 
               renderText(paste0("No errors in ",tableName)))
    }
  })
  boxArgs <- addInvalidCodesTab(boxArgs, errorTable()$badCodeSummary)
  
  boxArgs$width <- 10
  boxArgs$id <- "tabset1"
  div(class = "harmonist-error-table",
      tags$h3(class = "row_text title",tags$strong(span("STEP 2 ", class = "text-orange")),
              " Review data checks", backToHubMessage()),
      tags$h5(class = "row_text subtitle",
              step2Subtitle),
      moreErrorsMessage,
      div(class = "row_text subtitle row",
          div(class = "col-sm-4", div("Error summary by table", style = "font-size: x-large")), 
          div(class = "col-sm-3 col-sm-offset-3", style = "text-align: right",
              actionButton("showDownloadDetailModal", icon = icon("download"), "Download details"))),
      fluidRow(
        do.call(tabBox, boxArgs)
      ),
      fluidRow(
        actionButton("step3","Proceed to Step 3: Create summary", class="btn-success")
      )
  )
})

observeEvent(input$showDownloadDetailModal,{
  lastActivity(Sys.time())
  # For now, maxErrorsForHTML is set to zero; change to enable html error reports JUDY
  if (nrow(errorTable()$errorDetail) < maxErrorsForHTML){
    errorHTML_UI <- box(
      title = "Download Error Detail in HTML format",
      uiOutput("programsToIncludeErrorHTML"),
      fluidRow(column(3,offset = 1,uiOutput("downloadErrorHTML")))
    )
  } else errorHTML_UI <- NULL
  
  showModal(modalDialog(
    title = div(tags$b("Download detailed results of data quality checks"), style = "color: #605ea6;"),
      fluidRow(
        box(
          width = 12,
          title = "Options",
          checkboxInput("separateTables", 
                        "Create separate spreadsheets for each IeDEA table uploaded"),
          uiOutput("programsToIncludeErrorCSV"),
          tags$p("Download the ",
                 tags$a("IeDEA Harmonist Error Spreadsheet Guide ", 
                        href = "errorSpreadsheetGuide.pdf", target="_blank"), " for an explanation of the content of the file.")
        ),
        errorHTML_UI
      ),
   # fluidRow(column(3,offset = 1,uiOutput("downloadErrorDetail")))
   footer = uiOutput("downloadErrorDetail")
  )
  )
})

output$downloadErrorDetail <- renderUI({
  if (is.null(errorTable()))return(NULL)
  if (nrow(errorTable()$errorDetail) == 0) return(NULL)
  if (is.null(input$programsInErrorCSV)){
    return(NULL)
  }
  reportChoice <- input$programsInErrorCSV
  if (reportChoice == "all"){
    return(
      tagList(
        modalButton("Cancel"),
        downloadButton("reportErrors", "Download error detail in CSV format")
      )
    )
  }
  else if (reportChoice == "allZip"){
    return(downloadButton("reportZipAllErrors", "Download ZIP file of individual error reports"))
  }
  else {
    program <- input$programsInErrorCSV
    return(downloadButton("reportOneProgramErrors", 
                          paste0("Download Error Detail: PROGRAM ", program)))
  }
})




# currentTable() updates when observeEvent executes
observeEvent(input$tabset1,currentTable())

# currentTable: extract table name from HTML ------------------------------
currentTable <- eventReactive(input$tabset1,{
  lastActivity(Sys.time())
  tablePlusHTML <- str_after_first(input$tabset1,">")
  tableName <- unname(str_before_first(tablePlusHTML,"<"))
  print(paste0("table = ",tableName))
  trackDetailsForREDCap$tabClicks <- rbind(trackDetailsForREDCap$tabClicks,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "errortab",
                                             errortab_name = tableName,
                                             stringsAsFactors = FALSE))
  return(tableName)
})

# count how many times "view details" buttons clicked to add to REDCap progress
errorDetailViewCount <- reactiveVal(0)

# Generate error detail modal when "View Detail" clicked
# use input$lastClick as trigger for viewing detail because it is assigned a different
# (random) value with each click. This allow the same row to be selected twice in a row
observeEvent(input$lastClick, {
  lastActivity(Sys.time())
  tableName <- currentTable()
  # Invalid Code Summary does not have option to click for detail
  if (tableName == "InvalidCodes"){
    return(NULL)
  }
  
  # input$selectButton returns "button_nrow" where nrow is the row number of the 
  # selected View Details button
  selectedRow <- as.numeric(str_after_first(input$select_button,"button_"))
  summaryTable <- summaryToShow()[[tableName]]
  
  print(selectedRow)
  
  rowData <- summaryTable[selectedRow,]
  errorDetail <- errorTable()$errorDetail
  allDetails <- errorDetail %>% 
    filter(table == tableName) %>% 
    filter(error_field == rowData$variable) %>% 
    filter(category == rowData$category)
  # count how many times "view details" buttons clicked to add to REDCap progress
  errorDetailViewCount(errorDetailViewCount()+1)
  detailsForModal <- makeDetailsPretty(allDetails, rowData$variable)
  linkToDES <- paste0(redcap_des_url,"&tid=", tableDef[[tableName]][["redcapIndex"]],
                      "&vid=", tableDef[[tableName]][["variables"]][[rowData$variable]][["redcapIndex"]],
                      "&page=variableInfo")
  showModal(modalDialog(
    size = "l",
    title = div(tags$b(paste0("Error detail for variable ",rowData$variable, " in ",tableName)), style = "color: #605ea6;"),
    tags$p("See the ",
           tags$a("IeDEA DES ", href = linkToDES, target="_blank"), " for details about ", rowData$variable),
    
    wellPanel(tags$h4(detailsForModal$errorDesc)),
    tags$h4(paste0(rowData$number, " instance(s) of ", rowData$category  #"(",rowData$percent,"%)
    )),
    wellPanel(
      DT::renderDataTable({
        detailsForModal$toShow
      },
      rownames = FALSE,
      selection = "none"
      ) 
    ),
    footer = actionButton("close_detail_modal", label = "Close", class = "color_btn")
    
  ))
}, ignoreInit = TRUE)

observeEvent(input$close_detail_modal, {
  lastActivity(Sys.time())
  removeModal()
})