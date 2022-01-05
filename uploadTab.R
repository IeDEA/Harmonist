
collapseStatus <- reactiveValues(
  noActive = FALSE,
  active = FALSE,
  uploadSummary = FALSE
)

requestedVariables <- reactive({
  if (!hubInfo$fromHub) return(NULL)
  concept <- userDetails()$uploadconcept_mr
  message <- "Your dataset"
  if (is.null(uploadList()$MissingTables) &&
      (is_empty(tablesAndVariables$missingConceptColumns))){
    return(tags$p(message, "contains", span("all variables requested by", concept, 
                                             class = "text-green", style = "font-weight: bold")))
  }
  
  numVarsMissing <- tablesAndVariables$missingVariableCount
  numTables <- length(tablesAndVariables$missingConceptColumns)
  connector <- ifelse(numTables == 1, "in", "across")
  return(
    
    tagList(
      tags$p(message, "is", 
             tags$a(span("missing", numVarsMissing,
                         makeItPluralOrNot("variable", numVarsMissing),
                         class = "text-red", style = "font-weight: bold"),
                    href = "#missingBox"),
             "requested by", concept,
             connector, numTables, 
             paste0(makeItPluralOrNot("table", numTables), ".")
      )
             
              #actionLink("showMissing", span("missing", numVarsMissing,
                                  # makeItPluralOrNot("variable", numVarsMissing), "requested by", concept,
                                  # connector, numTables, makeItPluralOrNot("table", numTables), ".",
                                  # class = "text-red", style = "font-weight: bold")))
    #  tags$ul(tablesAndVariables$missingConceptColumns) # for now don't repeat info in yellow box
      )
  )
})


unrecognizedVariables <- function(){
  numExtraFiles <- length(uploadList()$ExtraFiles)
  extraFilesFlag <- numExtraFiles > 0
  numExtraVariables <- tablesAndVariables$details$non_des_count
  extraVariablesFlag <- numExtraVariables  > 0
  numDeprecated <-  tablesAndVariables$details$deprecated_count
  deprecatedFlag <- numDeprecated > 0
  numberOfMessages <- sum(extraFilesFlag, extraVariablesFlag, deprecatedFlag)
  if (numberOfMessages == 0) return(NULL)
  
  message <- "In addition, your dataset contains"
  period1 <- ""
  period2 <- ""
  if (numberOfMessages == 3){
    connector1 <- ","
    connector2 <- ", and"
  } else if (numberOfMessages == 2){
    if (extraFilesFlag){
      connector1 <- " and"
      connector2 <- ""
      if (extraVariablesFlag) period2 <- "."
    } else {
      connector1 <- ""
      connector2 <- " and"
    }
    
  } else {
    connector1 <- ""
    connector2 <- ""
    if (extraFilesFlag) period1 <- "."
    else if (extraVariablesFlag) period2 <- "."
  }

  
  extraFileMsg <- if_else(extraFilesFlag,
                          paste0(numExtraFiles, " unrecognized ", makeItPluralOrNot("file", numExtraFiles), period1),
                          "")
  extraVariablesMsg <- if_else(extraVariablesFlag,
                               paste0(numExtraVariables, " unrecognized ", makeItPluralOrNot("variable", numExtraVariables), period2),
                               "")
  deprecatedVariablesMsg <- if_else(deprecatedFlag,
                               paste0(numDeprecated, " deprecated ", makeItPluralOrNot("variable", numDeprecated), "."),
                               "")
  return(tags$p(message, tags$b(paste0(extraFileMsg, connector1)), tags$b(paste0(extraVariablesMsg, connector2)),
                tags$b(paste0(deprecatedVariablesMsg)),
                "See summary box above for details."))
}


output$groupByMenuUI <- renderUI({
  groupOptions <- groupByInfo()
  if (is.null(groupOptions$otherGroupOptions)){
    groups <- defGroupVar
    message <- tagList(
      tags$p("For reporting purposes your dataset will be grouped by the variable",
             defGroupVar),

      makeBulletedList(groupOptions$programs),
      tags$em("If you prefer that your data are grouped differently for reports,", 
              "please include that variable in",
              indexTableName,
              "and upload your revised dataset.")
    )
    return(message)
  }

  if (is.null(formattedTables())){
    groupVar <- currentGroupSelection()
    if (is.null(groupVar)) groupVar <- defGroupVar
    
    if (groupVar == defGroupVar){
      groupNames <- sort(groupOptions$programs)
    } else {
      groupNames <- sort(groupOptions$otherGroupOptions[[groupVar]]$levels)
    }
    
    message <- tagList(
      tags$p("Your data will be grouped by", tags$b(groupVar),"for reporting into the following",
             paste0(makeItPluralOrNot("group", length(groupNames)), ":")),
      makeBulletedList(groupNames),
      tags$br(),
      tags$em("To group by a different variable, choose from the list below of alternate grouping variables found in your",
              indexTableName,
              "file or upload a revised dataset that includes the desired grouping variable in",
              indexTableName
      ),
      tags$br(),
      selectInput("groupBySelect", 
                  "Grouping variables:", 
                  c(defGroupVar, names(groupOptions$otherGroupOptions)),
                  selected = groupVar)
    )
    return(message)
  }
  
  else {
    groupVar<- finalGroupChoice()
    if (groupVar == defGroupVar) groupNames <- groupOptions$programs
    else groupNames <- groupOptions$otherGroupOptions[[groupVar]]$levels
    tagList(
      tags$p("Your data will be grouped for reporting purposes by", tags$b(groupVar)),
      makeBulletedList(groupNames),
      tags$br(),
      tags$em("Data checks are complete and grouped by ", groupVar, 
              ". Upload your dataset again to choose a different grouping variable.", 
              "Grouping variables must be included in",
              indexTableName)
    )
  }
  
})


showGroupModal <- function(saveAndGo = FALSE){
  if (!is.null(finalGroupChoice()) || is.null(groupByInfo()$otherGroupOptions)){ 
    buttons <- modalButton("Close")
  } else {
    # other group options exist
    if (saveAndGo){
      # the user was attempting to continue to data quality checks with one patient group. Now
      # once they save, start DQ checks. Otherwise, just save
      saveButton <- actionButton("saveGroupAndGo", "Save", class = "btn-success")
      cancelButton <- actionButton("cancelAndGo", "Cancel")
    } else {
      saveButton <- actionButton("saveGroup", "Save", class = "btn-success")
      cancelButton <- actionButton("cancelGroup", "Cancel")
    }
    buttons <- tagList(
      cancelButton,
      saveButton
    )
  } 
  
  

  showModal(tags$div(id="dataQualityChecks",
                     modalDialog(
                       easyClose = FALSE, 
                       title = "Dataset Grouping Options", 
                       size = 'l',
 
                       uiOutput("groupByMenuUI"),
                       footer = buttons,
                       fade = FALSE
                     )
  ))
}


observeEvent(input$openGroupModal,{
  showGroupModal()
})

observeEvent(input$cancelGroup,{
  removeModal()
})

observeEvent(input$cancelAndGo,{
  removeModal()
  startStep2()
})

observeEvent(input$saveGroup,{
  groupByChoice(input$groupBySelect)
  removeModal()
})

observeEvent(input$saveGroupAndGo,{
  groupByChoice(input$groupBySelect)
  removeModal()
  startStep2()
})


observeEvent(input$groupBySelect,{
  currentGroupSelection(input$groupBySelect)
})

output$uploadIntro <- renderUI({
  tagList(
    tags$h3(class = "row_text title",tags$strong(span("STEP 1 ", class = "text-red")),
            " Upload files", backToHubMessage()),
    
    tags$h5(class = "row_text subtitle", 
            paste0(
              "Choose the files containing your ",
              networkName,
              " tables to check for data quality. After files are uploaded, review the table summarizing uploaded files and variables."
              )
    )
  )
})

observeEvent(uploadList(),{
  if (is.null(uploadList())) return(NULL)
  collapseStatus$noActive <- TRUE
  collapseStatus$active <- TRUE
})

observeEvent(input$noActiveID,{
  collapseStatus$noActive <- !collapseStatus$noActive
})

observeEvent(input$activeID,{
  collapseStatus$active <- !collapseStatus$active
})

observeEvent(input$uploadSummaryID,{
  collapseStatus$uploadSummary <- !collapseStatus$uploadSummary
})

getPreferredDataFormats <- function(concept){
  formatPrefs <- names(concept)[str_detect(names(concept), "dataformat_prefer")]
  requestedFormats <- c()
  
  for (formatPref in formatPrefs){
    formatChosen <- concept[[formatPref]]
    if (is.null(formatChosen)) next
    if (formatChosen == "1") {
      index <- as.numeric(str_after_last(formatPref, "_"))
      formatName <- fileFormats[[as.character(index)]] #fileFormats in definitions.R
      requestedFormats <- c(requestedFormats, formatName)
    }
  }
  if (is.null(requestedFormats)) requestedFormats <- "None specified"
  return(requestedFormats)
}

getContactDisplay <- function(contact, datacontactFlag = FALSE){
  # datacontactFlag indicates adding "Data Contact" in italics after the email link
  if (is_blank(contact)) return(NULL)
  regionCode <- hubInfo$userDetails$regionCode
  name <- paste(contact$firstname, contact$lastname, paste0("(", regionCode,")"))
  # in case Institution field is blank in REDCap:
  if (safeTrimWS(contact$institution) == ""){
    institution <- ""
  } else {
    institution <- paste0(", ", contact$institution)
  }
  
  if (datacontactFlag){
    displayText <- 
      tags$li(a(name, href=paste0("mailto:", contact$email), target="_blank"),
              em("(Data contact)"), institution)
  } else {
    displayText <- 
      tags$li(a(name, href=paste0("mailto:", contact$email), target="_blank"),
              institution)
  }
  return(displayText)
}

# Show user details about the current data request
dataRequestInfo <- reactive({
  if (projectDef$hub_y != "1") return(NULL)
  if (!hubInfo$fromHub){
    # at this point this is specific to the IeDEA Hub
    box(
      collapsible = TRUE,
      collapsed = collapseStatus$noActive,
      width = 10,
      title = actionLink("noActiveID", "No Active Data Request"),
      tagList(
        tags$p(em("There is no active data request currently linked to this data upload. Not all features of the Toolkit are available.")),
        tags$ul(tags$li(span(strong("Available: "), style="color:#00a65a"),"data upload, data quality checking, report generation"),tags$li(span(strong("Not available: "), style="color:#dd4b39"),"data upload to cloud")),
        tags$p("You may select an active data request on the IeDEA Hub", a(" Submit Data", href="http://iedeahub.org", target="_blank")," page.")
      )
    )
  }
  else {
    req(userDetails())
    req(concept())
  
    concept_url <- paste0(plugin_url, "?token=",
                      userDetails()$uploadhub_token, "&option=ttl",
                      "&record=", userDetails()$uploadconcept_id,
                      "&pid=58325")
    request_url <- paste0(plugin_url, "?token=",
                          userDetails()$uploadhub_token, "&option=sop",
                          "&record=", userDetails()$datacall_id,
                          "&pid=58325")
    listOfTableLabels <- tagList(
      lapply(names(concept()$tablefields), function(x){
        tableLabelClass <- paste0("label des-",tableDef[[x]]$table_category)
        span(x, class = tableLabelClass)
      })
    )
    
    requestedFormats <- concept()$requestedFormats
    
    box(
      width = 10,
      collapsible = TRUE,
      collapsed = collapseStatus$active,
      title = actionLink("activeID", tags$div(strong(userDetails()$uploadconcept_mr), "Active Data Request")),
      
      tagList(
        tags$p(div(strong("Title "),class="col-md-2"), div(userDetails()$uploadconcept_title,class="col-md-10")),
        tags$br(),
        tags$p(div(strong("Hub Pages"), class="col-md-2"),
               div(tags$a(span(paste(userDetails()$uploadconcept_mr,"on Hub"),icon("external-link")), 
                          href = concept_url, target="_blank"),
                   ",",
                   HTML('&nbsp;'),HTML('&nbsp;'),
                   tags$a(span("Data Specification",icon("external-link")), 
                          href = request_url, target="_blank"),
                   HTML('&nbsp;'),HTML('&nbsp;'),
                   pdfDownloadButton("sopFinal"),
                   class = "col-md-10"
               )
        ),

        tags$p(
          div(strong("Requested Tables "),class="col-md-2"),
          tags$div(listOfTableLabels, class="col-md-10"), class = "row toolkit"),
        tags$p(
          div(strong("Requested Data Format "),class="col-md-2"),
          tags$div(paste(requestedFormats, collapse = ", "), class="col-md-10"), class = "row toolkit"
        ),
        tags$p(div(strong("Contacts"), class = "col-md-2"),
               div(
                 tags$ul(
                   getContactDisplay(concept()$contact1),
                   getContactDisplay(concept()$contact2),
                   getContactDisplay(concept()$datacontact, datacontactFlag = TRUE)
                 ),
                 class = "col-md-10"
               ),
               class = "row toolkit"
        ),
        tags$p(div(strong("Data Downloaders"), class = "col-md-2"),
                div(
                  tags$ul(
                    lapply(concept()$downloaders, function(x){
                      getContactDisplay(x)})
                    ), class = "col-md-10"),
                class = "row toolkit")
      )
    )
  }
})

output$dataRequestInfo <- renderUI({
  if (is.null(hubInfo$fromHub)) return(NULL)
  fluidRow(
    dataRequestInfo()
    
  )
})

output$sopFinal <- downloadHandler(

  filename = "sopfinal.pdf",
  content = function(file) {
    record_id_Harmonist3 <- hubInfo$userDetails$datacall_id
    sopfinal <- downloadREDCapFile(file, tokenForHarmonist3,
                                   record_id_Harmonist3, "sop_finalpdf")
    if (inherits(sopfinal, "postFailure")) {
      # File failed to download, not sure there's anything to do about it
    }
  }
)

output$uploadMissingSummary <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (!hubInfo$fromHub) return(NULL)
  if (is.null(uploadList())) return(NULL)
  if (is.null(uploadedTables())) return(NULL)
  if (is.null(uploadList()$MissingTables) &&
      (is_empty(tablesAndVariables$missingConceptColumns))) return(NULL)
  if (!is.null(uploadList()$MissingTables)){
    listOfTables <- tagList(
      lapply(uploadList()$MissingTables, function(x){
        tableLabelClass <- paste0("label des-",tableDef[[x]]$table_category)
        span(x, class = tableLabelClass)
      })
    )
    missingTablesMessage <- tags$p(
      div(strong("The following",
                 makeItPluralOrNot("table", length(uploadList()$MissingTables)),
                 "requested by", userDetails()$uploadconcept_mr, "were not found:"),
          listOfTables)
    )
  } else missingTablesMessage <- NULL
  
  if (!is_empty(tablesAndVariables$missingConceptColumns)){
    missingColsMessage <- 
      tagList(
        tags$p(strong("The following",
                      makeItPluralOrNot("variable", tablesAndVariables$missingVariableCount),
                      "requested by", userDetails()$uploadconcept_mr, "were not found:")),
        tags$ul(tablesAndVariables$missingConceptColumnsFormatted)
      )
  } else missingColsMessage <- NULL
  fluidRow(  
    tags$a(id = "missingBox"),
    box(
      width = 10,
      solidHeader = TRUE,
      status = "warning",
      title = "Missing Variables",
     # missingTablesMessage, JUDY remove this if final decision
      missingColsMessage
    )
  )
})

output$uploadFileFormatError <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (!hubInfo$fromHub) return(NULL)
  if (is.null(uploadList())) return(NULL)
  if (is.null(uploadedTables())) return(NULL)
  if (is_empty(uploadList()$nonmatchingFileFormats)) return(NULL)

  nonmatchingFiles <- uploadList()$nonmatchingFileFormats
  reqFormats <- concept()$requestedFormats
  requestedmsg <- paste(sapply(reqFormats,function(i) as.character(tags$strong(i))), 
                        paste0(".",concept()$requestedExtensions), sep = " (")
  requestedmsg <- glue::glue_collapse(
    paste0(requestedmsg, ")"), 
    sep = ", ",
    last = " and ")
  
  nonmatchingMessage <- 
    tagList(
      tags$p(tags$strong("The following",
                 makeItPluralOrNot("file", length(nonmatchingFiles)),
                 makeItPluralOrNot("does", length(nonmatchingFiles)),
                 "not match the file format",
                 "requested by", 
                 paste0(userDetails()$uploadconcept_mr,":")
             )),
          makeBulletedList(nonmatchingFiles),
      tags$p(userDetails()$uploadconcept_mr,
             "requests", 
             HTML(requestedmsg), 
             "files.",
             tags$em("Note that CSV files are always an acceptable option."))
    )

  fluidRow(
    tags$a(id = "fileFormatBox"),
    box(
      width = 10,
      solidHeader = TRUE,
      status = "warning",
      title = "File Format Error",
      nonmatchingMessage
    )
  )
})



# uploadSummary UI --------------------------------------------------------
output$uploadSummary <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(uploadList())) return(NULL)
  if (is.null(uploadedTables())) return(NULL)
  uploadInfo <- renderTable({tablesAndVariables$details$variableSummaryToDisplay}, 
                            sanitize.text.function = function(x) x) #this allows escape html in renderTable
  
  # if the uploaded dataset included deprecated variables, add explanatory line below table

    if (tablesAndVariables$details$deprecated_count > 0){
      if (projectDef$datamodel_url_y == "1"){
        deprecatedMessage <- div(
          tags$em(
            tags$span("Note: Deprecated variables are shown in red. See", 
                      tags$a(tags$u("iedeades.org"), href="http://iedeades.org", 
                             target="_blank", style= "color: #dd4b39"),
                      "for details.", style= "color: #dd4b39")
          )
        )
      } else {
        deprecatedMessage <- div(
          tags$em(
            tags$span("Note: Deprecated variables are shown in red.", style= "color: #dd4b39")
          )
        )
      }
  } else deprecatedMessage <- NULL
  
  if (!is_empty(uploadList()$emptyFiles)){
    emptyFileMessage <- tagList(
      tags$p(" The following uploaded",
             makeItPluralOrNot("file", length(uploadList()$emptyFiles)),
             "were empty:"),
      makeBulletedList(uploadList()$emptyFiles)
    )
  } else emptyFileMessage <- NULL
  
  if (!is.null(tablesAndVariables$blankTables)){
    blankTableMessage <- tagList(
      tags$p(" The following file(s) had ",
      networkName,
      " table names and headers but 0 valid records:"),
      makeBulletedList(tablesAndVariables$blankTables)
    )
  } else blankTableMessage <- NULL
  
  if (length(uploadList()$ExtraFiles) == 0){
    extraFileMessage <- NULL
  } else {
    extraFileMessage <- tagList(
      tags$p(" ",
             makeItPluralOrNot("File", length(uploadList()$ExtraFiles)),
             "in other format or not conforming to the ",
             networkName,
             "data model (excluded from data quality checks):",
             paste(uploadList()$ExtraFiles, collapse = ", "))
      
      # renderTable({
      #   uploadList()$ExtraFiles
      # }, rownames = FALSE, colnames = FALSE)
    )
  }
  
  if (any(uploadList()$tablesWithNoPatientID %in% pregnancyTables)){
    filesNotCheckedMessage <- 
      tags$p("The following tables that use a key identifier other than PATIENT are excluded from some data quality checks at this time but will be included in the next version of Harmonist:",
             paste0(combine_words(uploadList()$tablesWithNoPatientID[uploadList()$tablesWithNoPatientID %in% pregnancyTables]), ".")
    )
  } else {
    filesNotCheckedMessage <- NULL
  }
  
  tagList(
    fluidRow(
      box(
        id = "uploadSummary",
        width = 10,
        title = actionLink("uploadSummaryID", 
                           span("Summary of Uploaded ",
                           networkName,
                           " Tables", style = "color: white")),
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = collapseStatus$uploadSummary,
        uploadInfo,
        deprecatedMessage,
        blankTableMessage,
        extraFileMessage,
        emptyFileMessage,
        filesNotCheckedMessage
      )
    )
  )
  
})

# UI to prompt user to select files to check and submit
output$selectFiles <- renderUI({
  if (is.null(hubInfo$fromHub)) return(NULL)
  
  if (projectDef$datamodel_url_y == "1"){
    datamodelText <- tags$a(
      networkName, 
      projectDef$datamodel_name, 
      href=projectDef$datamodel_url, target="_blank"
      )
  } else {
    datamodelText <- paste0(
      networkName, " ",
      projectDef$datamodel_name
    )
  }
  
  if (networkName == "IeDEA"){
    sampleDataDesc <- tags$p(
      "The sample dataset contains 48 intentionally error-filled records representing the following IeDEA DES tables:",
      tagList(
        lapply(c("tblBAS", "tblLTFU", "tblVIS", "tblART", "tblLAB", "tblLAB_BP"), function(x){
          tableLabelClass <- paste0("label des-",tableDef[[x]]$table_category)
          span(x, class = tableLabelClass)
        })
      )
    )
  } else {
    sampleDataDesc <- paste0("The sample dataset contains intentionally error-filled records ", 
                             "for use in demonstrating the Harmonist Data Toolkit")
  }
  uploadFileUI <- 
    tagList(
      fluidRow(
        box(width = 5, 
            status= "success",
            solidHeader = TRUE,
            title = "Select Data Files", 
            tagList(
              tags$p("Upload data in the ",
                     datamodelText,
                     " format. ",
                     indexTableName,
                     " is required."),
              tags$p("Allowed file formats include ", 
                     strong("CSV, SAS, Stata, SPSS, RDS, or a ZIP containing multiple files"), " of this type."),
              tags$p(tags$em("Select a single ZIP file or multiple files with Ctrl+Click"), align = "center"),
              fileInput("loaded",
                        "Data files", 
                        multiple = TRUE)
            )
        ),
        box(width = 5,
            title = "Use Sample Dataset",
            tags$p("Launch the Toolkit with a sample dataset (fake data) for practice, testing, and demonstrations."),
            sampleDataDesc,
            tags$br(),
            actionButton("runWithSample", "Launch with Sample Data")
        )
      )
    )
  
  if  (resetFileInput$reset){
    return(uploadFileUI)
  }
  else if (!is.null(uploadedTables())){
    # revisit here Judy  - maybe call groupByInfo() here
    if (is.null(groupByInfo()$otherGroupOptions)) buttonLabel <- "Details"
    else if (!is.null(finalGroupChoice())) buttonLabel <- "Details"
    else buttonLabel <- "Change selection"

    if (!hubInfo$fromHub){
      requiredVariableMessage <- 
        tags$p("Your dataset contains", 
               span("all required",
               projectDef$datamodel_abbrev,
               " variables", class = "text-green", style = "font-weight: bold"))
    } else requiredVariableMessage <- NULL # required variables are also requested variables
    requestedVariablesMessage <- requestedVariables()
    unrecognizedFileAndVariableMessage <- unrecognizedVariables()
    reportGroupingMessage <- tags$p("Your dataset will be subdivided for reports by ", 
                                    tags$b(groupByChoice()),
                                    xsButton("openGroupModal", label = buttonLabel)
                                    )
    return(
      tagList(
      fluidRow(class = "rowUploadComplete",
               box(
                 width = 5, 
                 title = span("Upload Complete", style = "color: white"),
                 solidHeader = TRUE,
                 status = "success",
                 tagList(
                   tags$p(tags$b(span(icon("check-square"),"Dataset successfully uploaded"))),
                   requiredVariableMessage,
                   requestedVariablesMessage,
                   unrecognizedFileAndVariableMessage,
                   reportGroupingMessage,
                   actionButton("step2","Continue to Step 2", class="btn-success")
                 )
               ),
               box(
                 width = 5,
              
                 title = span("Restart session"),
                 tagList(
                   tags$p("Start over and upload a", tags$b("revised or different dataset.")),
                   actionButton("uploadNew","Upload new dataset")
                 )
               )))
    )
  } else if (is.null(uploadList())){
    return(
      uploadFileUI
    )
  }
})

observe({
  if (tooBusy()) {
    shinyjs::hide("selectFiles")
  }
  else shinyjs::show("selectFiles")
})

observeEvent(input$testContinue,{
  hubInfo$fromHub <- FALSE
  resetFileInput$newData <- TRUE
  resetFileInput$reset <- FALSE
  updateTabItems(session,"tabs", "upload")
  updateQueryString("?tab=upload", mode = "push")
  useSampleData(TRUE)
})

observeEvent(input$testStop,{
  useSampleData(FALSE)
  removeModal()
})

observeEvent(input$runWithSample,{
  if (!hubInfo$fromHub){
    useSampleData(TRUE)
    resetFileInput$newData <- TRUE
    resetFileInput$reset <- FALSE
  } else {
    informNoConcept(userDetails()$uploadconcept_mr)
  }
})
