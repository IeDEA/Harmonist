# checkAdditionalErrorVariables -----------------------------------------
checkAdditionalErrorVariables <- function(logicalVector, df){
  
  for (i in 2:4){
    errorVariableName <- paste0("error_field", i)
    errorValueName <- paste0("error", i)
    
    if (exists(errorVariableName, df)){
      if (!logicalVector[[errorVariableName]]){
        logicalVector[[errorValueName]] <- FALSE
      }
    }
  }
  return(logicalVector) 
}

# addInvalidCodesTab -----------------------------------------------------------------
addInvalidCodesTab <- function(boxArgs, badCodeSummary){
  nextIndex <-  length(boxArgs) + 1
  numOfErrors <- sum(badCodeSummary$Count)
  if (numOfErrors > 0){
    codeLinks <- character(nrow(badCodeSummary))
    for (i in 1:nrow(badCodeSummary)){
      tableID <- tableDef[[badCodeSummary$Table[[i]]]][["redcapIndex"]]
      variableID <- tableDef[[badCodeSummary$Table[[i]]]][["variables"]][[badCodeSummary$Variable[[i]]]][["redcapIndex"]]
      linkToDES <- paste0(redcap_des_url,
                          "&tid=", tableID,
                          "&vid=", variableID,
                          "&page=variableInfo")
      codeLinks[[i]] <- paste0('<a href="', linkToDES, '"',
                               ' target = "_blank" class="btn btn-link">Valid Codes for ',
                               badCodeSummary$Variable[[i]],'</a>')
      # sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
      
      
    }
    badCodeSummary[["Valid Codes"]] <- codeLinks
    
    badCodes <- badCodeSummary %>% select(-Table, -Percent) %>% arrange(desc(Count))
    names(badCodes) <- c("Variable", "Invalid Code", "Count", "Valid Codes")
    newBoxArg <- tabPanel(title = tagList(span("Invalid Codes", style="font-weight: bold; font-size: smaller"), 
                                          span(numOfErrors, class="badge", style="background-color: red")), 
                          renderDataTable({badCodes}, 
                                          rownames = FALSE, escape = FALSE,
                                          options = list(
                                            columnDefs = list(
                                              list(orderable = FALSE, className = "dt-center", targets = 3), # disable ordering for View codes link column
                                              list(className = "dt-center", targets = 1)
                                              
                                            )
                                          )))
    
  } else {
    newBoxArg <- tabPanel(title = tagList(span("InvalidCodes", style="font-weight: bold; font-size: smaller; color: #00a65a"), shiny::icon("check")), 
                          renderText("No invalid codes in this dataset"))
  }
  boxArgs[[nextIndex]] <- newBoxArg
  return(boxArgs)
}

findVariableName <- function(df, idFieldName){
  variableName <- Mode(df[[idFieldName]])
  if (is_empty(variableName)) return(NULL)
  return(variableName)
}

# makeDetailsPretty -------------------------------------------------------------------------
# makeDetailsPretty isn't pretty... but it removes empty columns from error detail data frame
# (columns that don't apply to the error selected by the user) *but* is careful to keep
# empty columns when the error IS that the variable is blank
makeDetailsPretty <- function(df, error_field, linkToDataModel){
  #remove empty columns; not every error type has data in each column
  emptyCols <- sapply(df, function(x) all(x=="", na.rm = TRUE))
  #make sure errorValue isn't removed -- the blank entry is important
  emptyCols[["error"]] <- FALSE
  # if the errorVariable2 field is not empty, make sure the errorValue2 column isn't removed
  # blank entries are important if that's the error variable, repeat for errorValues2 and 3
  # I know this isn't elegant!
  emptyCols <- checkAdditionalErrorVariables(emptyCols, df)
  df <- df[!emptyCols]
 
  toShow <- NULL
  # track number of identifying columns to aid re-ordering columns for display
  numberOfIdCols <- 1
  
  if (exists("id1", df)){
    identifier <- df$id1_field[[1]]
    toShow[[identifier]] <- df$id1
  }
  if (exists("id2", df)){
    identifier <- findVariableName(df, "id2_field")
    if (is.null(identifier)) identifier <- "Unknown"
    toShow[[identifier]] <- df$id2
    numberOfIdCols <- 2
  }
  if (exists("id3", df)){
    identifier <- findVariableName(df, "id3_field")
    if (is.null(identifier)) identifier <- "Unknown"
    toShow[[identifier]] <- df$id3
    numberOfIdCols <- 3
  }
  lineReturn <- tags$br()
  if (networkName == "IeDEA"){
    if (df$category[[1]] == "Invalid Code"){
      if (str_detect(df$description[[1]], "Valid codes")){ # then valid codes are listed in description
        explanation <- "See more details about"
      } else {
        explanation <- "See valid codes for"
        lineReturn <- ""
      }
    } else explanation <- "See specifications for"
  }
  
  
  # before adding error_field column to data frame to show in modal,
  # make sure that it isn't one of the ID columns already in the "to show"
  # This is most important if it is a pervasive error
  if (!error_field %in% names(toShow)){
    toShow[[error_field]] <- df$error
  }
  
  if (networkName == "IeDEA") {
    linkToVariableInDataModel <- tags$a(
      paste0(error_field, " in the IeDEA DES."), 
      href = linkToDataModel, target="_blank")
  } else {
    explanation <- ""
    linkToVariableInDataModel <- ""
  }
  
  # if all errors have identical descriptions put it at top of table
  if (length(unique(df$description)) ==1){
    errorDesc <- tags$em(df$description[[1]], 
                        lineReturn,
                        explanation, 
                        linkToVariableInDataModel
                        )
  } else {
    # otherwise, put link to DES at top of table but detail in table
    errorDesc <- tags$em(explanation, 
                         linkToVariableInDataModel,
                        "Detailed error descriptions included below:")
    toShow[["Details"]] <- df$description
  }
  # use error variable names as column names for all variables found in df
  if (exists("error2", df)){
    variableName <- df$error_field2[df$error_field2 != ""][[1]] # in case of pervasive errors error_field2 and error_field3 might be blank but other rows must have value because these were not found to be emptyCols
    toShow[[variableName]] <- df$error2
  }
  if (exists("error3", df)){
    variableName <- df$error_field3[df$error_field3 != ""][[1]]
    toShow[[variableName]] <- df$error3
  }
  if (exists("error4", df)){
    variableName <- df$error_field4[df$error_field4 != ""][[1]]
    toShow[[variableName]] <- df$error4
  }
  
  toShow <- data.frame(as.list(toShow), stringsAsFactors = FALSE)
  # now put columns in logical order, so that for example RECART_D is followed by RECART_D_A
  if (ncol(toShow) > 1){
    allCols <- names(toShow)
    idCols <- allCols[1:numberOfIdCols]
    otherCols <- allCols[numberOfIdCols+1:length(allCols)]
    toShow <- toShow[, c(idCols, sort(otherCols))]
  }
  
  if (any(df$quantity > 1, na.rm = TRUE)){
    toShow$Quantity <- df$quantity
  }
  
  return(list(toShow = toShow,
              errorDesc = errorDesc))
  
}


# summaryToShow: generate list of summary tables of errors by table, with badges and functioning buttons added
summaryToShow <- reactive({
  req(errorTable())
  errors <- errorTable()$errorsByTable
  errorSummary <- errorTable()$errorSummary
  summaryToShow <- list()
  for (tableName in uploadList()$AllDESTables){
    summaryOut <- NULL
    if (tableName %in% names(errors)){
      summaryOut <- errorSummary %>% filter(table == tableName) %>% 
        arrange(category) %>% 
        arrange(severity)# %>% 
        #mutate("Error" = paste0(category,": ", variable))
      # create an error name to list in table that only includes the variable name once and doesn't
      # list a variable name for duplicate record errors
      summaryOut[["Error"]] <- ifelse(
        str_detect(summaryOut$category, summaryOut$variable) | 
          str_detect(summaryOut$category, "Duplicate"),
        summaryOut$category, 
        paste0(summaryOut$category,": ", summaryOut$variable))
      colorLevel <- sapply(summaryOut$severity, function(x){
        if (x=="Error") {
          return("red")
        }
        else if (x == "Critical") {
          return(criticalColor)
        } else return("orange")
      })
      # it looks more attractive to have same number of characters in badges but 
      severityForBadge <- summaryOut$severity
      severityForBadge[severityForBadge == "Warning"] <- "Warn"
      severityOrder <- sapply(severityForBadge, switch, Warn = 0, Error = 1, Critical = 2, 9)
      
      summaryOut[["Severity"]] <-
        paste0('<span class="badge badge-fix order-', severityOrder, '" style="background-color: ', colorLevel,'">',severityForBadge,'</span>')
      
      summaryOut[["Option"]] <- 
        shinyInput(actionButton, nrow(summaryOut), 'button_', 
                   label = "View Detail", 
                   onclick = 'Shiny.onInputChange(\"select_button\",  this.id); Shiny.onInputChange(\"lastClick\", Math.random())' )
      
    } else {summaryOut <- 0}
    summaryToShow[[tableName]] <- summaryOut
  }
  return(summaryToShow)
})


output$afterStep2Options <- renderUI({

  msgIntro <- "Your dataset contains"
  totalErrors <- sum(errorTable()$errorOnlySummary$Count)
  
  # default situation is if there are no critical errors, so initially set box prperties for that case:
  continueHeader <- tags$p(tags$b(span(icon("check-square"),"Error checks completed")))
  criticalErrorMsg <- NULL
  continueStatus <- "success"
  continueTextColor <- "color:white"
  continueButtonColor <- "btn-success"
  continueSolid <- TRUE
  restartStatus <- NULL
  restartTextColor <- NULL
  restartButtonColor <- NULL
  criticalRestartInfo <- NULL
  restartSolid <- FALSE

  if (totalErrors == 0){
    dqresults <- tags$p(msgIntro,
                        span("0 total errors.", class = "text-green", style = "font-weight: bold")
    )
  } else{
    # we know there are some errors. Are any critical? 
    if (nrow(errorTable()$criticalErrors) == 0){
      totalCriticalErrors <- 0
    } else {
      totalCriticalErrors <- sum(errorTable()$criticalErrors$Count)
    }
    if (nrow(errorTable()$badCodeSummary) == 0){
      totalBadCodes <- 0
    } else {
      totalBadCodes <- sum(errorTable()$badCodeSummary$Count)
    }
    
    # when counting error categories, only include Errors not Warnings, and count number of
    # unique error codes (so that all date logic are in same category, not separate
    # categories for "BIRTH_D before.." etc)
    totalErrorCategories <- uniqueN(errorTable()$errorDetail[errorTable()$errorDetail$severity %in% c("Critical", "Error"), "errorCode"])
    
    if (totalCriticalErrors > 0){
      # if critical errors exist, reset box properties
      continueHeader <- tags$p(tags$b(span(icon("exclamation-triangle"),"Critical Error Warning")))
      criticalErrorMsg <- span(totalCriticalErrors, 
                                span("Critical", class="badge badge-critical"), 
                                 makeItPluralOrNot("error", totalCriticalErrors),
                               style = paste0("color:", criticalColor,"; ","font-weight: bold"))

      continueStatus <- NULL
      continueTextColor <- NULL
      continueButtonColor <- NULL
      continueSolid <- FALSE
      restartStatus <- "success"
      restartTextColor <- "color:white"
      restartButtonColor <- "btn-success"
      criticalRestartInfo <- tags$p(
        "We recommend that you review and correct the critical errors found in the dataset.",
        "To review the errors offline, download the",
        actionLink("step2DownloadDetailModal", "error detail CSV") # icon = icon("download"),
      )
      restartSolid <- TRUE
    } 
    
    if (totalBadCodes == 0){
      if (!is.null(criticalErrorMsg)){
        including <- "including"
      } else {
        including <- NULL
      }
      # no bad codes
      dqresults <- tags$p(msgIntro, 
                          span(totalErrors,
                               span(makeItPluralOrNot("Error", totalErrors), class = "badge badge-error"),
                               "in",
                               totalErrorCategories, "error", 
                               makeItPluralOrNot("category", totalErrorCategories),
                               class = "text-red", style = "font-weight: bold"),
                          including,
                          criticalErrorMsg
                          )
    } else {
      #There are bad codes
      if (!is.null(criticalErrorMsg)){
        connector <- "and"
      } else connector <- NULL
      dqresults <- tags$p(msgIntro, 
                          span(totalErrors, 
                               span(makeItPluralOrNot("Error", totalErrors), class = "badge badge-error"),
                               "in", 
                               totalErrorCategories, "error", 
                               makeItPluralOrNot("category", totalErrorCategories), 
                               class = "text-red", style = "font-weight: bold"),
                          "including",
                          criticalErrorMsg,
                          connector,
                          span(totalBadCodes, "invalid ", makeItPluralOrNot("code", totalBadCodes),
                               class = "text-red", style = "font-weight: bold")
      )
    }
  }
  
  fluidRow(class = "rowUploadComplete",
           box(
             width = 5,
             title = span("Continue to Summary", style = continueTextColor),
             solidHeader = continueSolid,
             status = continueStatus, 
             tagList(
               continueHeader,
               tags$p(dqresults),
               tags$p("If you have already reviewed the content of the dataset, proceed to the next step to",
                      tags$b("generate a summary of the data.")),
               actionButton("step3","Continue to Step 3", class = continueButtonColor)
             )
           ),
           box(
             width = 5,
             title = span("Restart session",  style = restartTextColor),
             solidHeader = restartSolid,
             status = restartStatus,
             tagList(
               criticalRestartInfo,
               tags$p("Start over and upload a", tags$b("revised or different dataset.")),
               actionButton("uploadNew2","Upload new dataset", class = restartButtonColor)
             )
           )
  )
})

restartAfterDQ <- function(){
  showModal(modalDialog(
    title = div(tags$b("Restart Confirmation"), style = "color: #605ea6;"),
    fluidRow(
      box(
        width = 12,
        tags$p("Are you sure? The current dataset and data quality check results will be deleted from memory if you continue. ")
      )
    ),
    # fluidRow(column(3,offset = 1,uiOutput("downloadErrorDetail")))
    footer = tagList(
      modalButton("Cancel"),
      actionButton("yesRestart2", "Continue")
    )
  ))
}

observeEvent(input$uploadNew2,{
  restartAfterDQ()
})

# if user decides to upload new dataset, unlink previously loaded dataset and change reset to TRUE
observeEvent(input$yesRestart2,{
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
  updateTabItems(session, "tabs", "upload")
  removeModal()
})


# display error summary interactively in separate tables, by table-----------
output$errorSummarySep <- renderUI({
  if (resetFileInput$reset) {
    return(tags$h3(class = "row_text title", notReadyMessage))
  }
  if (is.null(uploadList())){
    return(tags$h3(class = "row_text title", notReadyMessage))
  }
  if (is.null(formattedTables())){
    return(tags$h3(class = "row_text title", notReadyMessage))
  }
  if (is.null(errorTable())){
    return(tags$h3(class = "row_text title", notReadyMessage))
  }
  # if (nrow(errorTable()$errorDetail) == 0){
  #   return(tags$h3(class = "row_text title", "No errors or warnings detected in dataset"))
  # }
  
  showChoices <- uiOutput("afterStep2Options")

  if (nrow(errorTable()$errorDetail) == 0){
    tagList(
      fluidRow(tags$h3(class = "row_text title",tags$strong(span("STEP 2 ", class = "text-orange")),
                " Check data", backToHubMessage())),
    showChoices
    )
  } else {
  req(summaryToShow())
  
  errors <- errorTable()$errorsByTable
  errorSummary <- errorTable()$errorSummary
  criticalErrors <- errorTable()$criticalErrors
  # for now, don't report total number of errors because errorCount actually counts the number of 
  # error ROWS (quantity reported in one row can be > 1). Solution is to add another reactive variable
  # to count errors as well, not just rows
   if (isolate(errorExcess())){
     moreErrorsMessage <- tags$p(tags$strong("Note:"), " The number of error records detected",
                                 "exceeds the current capacity of the Toolkit. Only the first ",
                                 format(sum(errorSummary$number), scientific = FALSE, big.mark=","), 
                                 "errors and warnings are included in this report.")
   } else moreErrorsMessage <- NULL
  
  if (is.null(criticalErrors) || (nrow(criticalErrors) == 0) ){
    criticalErrorsBox <- NULL
  } else {
    criticalErrorsWarning <- paste0("Your dataset contains ", 
           sum(criticalErrors$Count), " critical ", 
           makeItPluralOrNot("error", sum(criticalErrors$Count)),
           " in ", nrow(criticalErrors), " ", makeItPluralOrNot("category", nrow(criticalErrors)),
           ". We highly recommend that you correct critical errors and upload the revised dataset in Step 1. Note that critical errors require correction or explanation for dataset submission."
    )
    criticalErrorsBox <-
      tags$div(class="alert alert-critical", role="alert",
               criticalErrorsWarning
      )
  }

  boxArgs <- lapply(uploadList()$AllDESTables, function(tableName) {
    if (tableName %in% names(errors)){
      summary <- summaryToShow()[[tableName]][,c("Error","Severity", "number", "Option")]
      summary <- summary %>% 
        arrange(desc(Severity), desc(number))

      numberOfErrors <- sum(summary$number)
      print(numberOfErrors)
      allWarnings <- all(str_detect(summary$Severity, "Warn"))
      badgeColor <- ifelse(allWarnings,"background-color: orange", "background-color: red")
      
      anyCritical <- any(str_detect(summary$Severity, "Critical"))
      if (anyCritical){
        criticalCount <- summary %>% filter(str_detect(Severity, "Critical")) %>% summarise(total = sum(number))
        criticalCount <- criticalCount$total
        criticalBadge <- span(criticalCount, class="badge badge-critical")
        # now remove critical errors from total error count in error badge:
        numberOfErrors <- numberOfErrors - criticalCount
        if (numberOfErrors > 0){ 
          errorBadge <- span(numberOfErrors, class="badge", style=badgeColor)
        } else errorBadge <- NULL #in rare case that all the errors were critical errors
      } else {
        errorBadge <- span(numberOfErrors, class="badge", style=badgeColor)
        criticalBadge <- NULL
      }

      output[[paste0("summary_",tableName)]] <- 
        DT::renderDataTable(summary, 
                            selection = "single", rownames = FALSE,
                            colnames = c("Error description" = "Error", "Severity" = "Severity", "Count" = "number", " " = "Option"),
                            escape = FALSE,
                             options = list(
                               columnDefs = list(
                                 list(orderable = FALSE, className = "dt-center", targets = 3), # disable ordering for View Detail column
                                 list(className = "dt-center", targets = 1)
                               )
                             )
                           )
         
      tabPanel(title = tagList(span(tableName, style="font-weight: bold; font-size: smaller"), 
                               errorBadge,
                               criticalBadge), 
               DT::dataTableOutput(paste0("summary_",tableName))
      )
    }
    else if (tableName %in% tablesAndVariables$blankTables) {
      tabPanel(title = tagList(span(tableName, style="font-weight: bold; font-size: smaller"), shiny::icon("exclamation-triangle")), 
               renderText(paste0("No records found in ",tableName)))
    }
    else {
      tabPanel(title = tagList(span(tableName, style="font-weight: bold; font-size: smaller; color: green"), shiny::icon("check")), 
               renderText(paste0("No errors in ",tableName)))
    }
  })
  boxArgs <- addInvalidCodesTab(boxArgs, errorTable()$badCodeSummary)
  
  boxArgs$width <- 10
  boxArgs$id <- "tabset1"
  div(class = "harmonist-error-table",
      tags$h3(class = "row_text title",tags$strong(span("STEP 2 ", class = "text-orange")),
              " Check data", backToHubMessage()),
      tags$h5(class = "row_text subtitle",
              "View interactive summary of errors and download detailed results of data quality checks to review offline."),
      tags$div(
        class = "row_text criticalWarning",
        criticalErrorsBox
      ),
      moreErrorsMessage,
      div(class = "row_text subtitle row",
          div(class = "col-sm-4", div("Error Summary by Table", style = "font-size: x-large")), 
          div(class = "col-sm-3 col-sm-offset-3", style = "text-align: right",
              actionButton("showDownloadDetailModal", icon = icon("download"), "Download error detail CSV"))),
      fluidRow(
        do.call(tabBox, boxArgs)
      ),
      uiOutput("afterStep2Options")
  )
  }
})

downloadDetailModal <- function(){
  showModal(tags$div(id="dataQualityChecks", modalDialog(
    title = div(icon("download"),("Download detailed results of data quality checks")),
    fluidRow(
      box(
        width = 12,
        title = "Options",
        checkboxInput("separateTables", 
                      "Create separate spreadsheets for each table in dataset"),
        uiOutput("programsToIncludeErrorCSV"),
        tags$p("View the ",
               tags$a("Harmonist Toolkit Error Spreadsheet Guide ", 
                      href = "errorSpreadsheetGuide.pdf", target="_blank"), " for an explanation of the content of the file(s).")
      )
    ),
    # fluidRow(column(3,offset = 1,uiOutput("downloadErrorDetail")))
    footer = uiOutput("downloadErrorDetail")
  ))
  )
}


observeEvent(input$showDownloadDetailModal,{
  lastActivity(Sys.time())
  downloadDetailModal()

})

observeEvent(input$step2DownloadDetailModal,{
  lastActivity(Sys.time())
  downloadDetailModal()
  
})

output$downloadErrorDetail <- renderUI({
  if (is.null(errorTable()))return(NULL)
  if (nrow(errorTable()$errorDetail) == 0) return(NULL)
  if (is.null(input$programsInErrorCSV)) return(NULL)
  
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
    return(
      tagList(
        modalButton("Cancel"),
        downloadButton("reportZipAllErrors", "Download ZIP file of individual error reports"))
    )
  }
  else {
    groupVar <- finalGroupChoice()
    group <- input$programsInErrorCSV
    return(
      tagList(
        modalButton("Cancel"),
        downloadButton("reportOneProgramErrors", 
                          paste0("Download Error Detail: ", groupVar, ": ", group)))
    )
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
  errorDetailViewCount(isolate(errorDetailViewCount())+1)
  tableID <- tableDef[[tableName]][["redcapIndex"]]
  variableID <- tableDef[[tableName]][["variables"]][[rowData$variable]][["redcapIndex"]]
  # since the first variable record in REDCap is actually "" instead of "1":
  if (variableID == 1) variableID <- ""
  linkToDES <- paste0(redcap_des_url,
                      "&tid=", tableID,
                      "&vid=", variableID,
                      "&page=variableInfo"
  )
  detailsForModal <- makeDetailsPretty(allDetails, rowData$variable, linkToDES)
  if (!is.data.frame(detailsForModal$toShow)) detailTable <- NULL
  else {
    detailTable <- wellPanel(
    DT::renderDataTable({
      detailsForModal$toShow
    },
    rownames = FALSE,
    selection = "none"
    )
    )}
  severity <- tolower(allDetails$severity[[1]])
  if (severity == "critical") {
    severityShow <- "critical error"
  } else {
    severityShow <- severity
  }
  errorWord <- ifelse(rowData$number > 1, paste0(severityShow, "s"), severityShow)
  badgeClass <- switch(severity,
                       "error" = "badge badge-error",
                       "warning" = "badge badge-warn",
                       "critical" = "badge badge-critical")
  #badgeColor <- ifelse(severity == "error", "background-color: red", "background-color: orange")
  mentionVariable <- ifelse(str_detect(rowData$category, rowData$variable), 
                            "",
                            paste("for", rowData$variable))
  showModal(tags$div(id="infoModal", modalDialog(
    size = "l",
    title = div(icon("info-circle"),"Error Detail"),
    tags$h4(rowData$category, mentionVariable, "in", tableName,
            span(paste(rowData$number, errorWord), class = badgeClass)), # style=badgeColor)),
    
    detailsForModal$errorDesc,
    tags$br(),
    tags$br(),
    detailTable,
    footer = actionButton("close_detail_modal", label = "Close", class = "color_btn")
    
  )))
}, ignoreInit = TRUE)

observeEvent(input$close_detail_modal, {
  lastActivity(Sys.time())
  removeModal()
})
