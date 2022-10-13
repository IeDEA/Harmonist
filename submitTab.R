
readyToSubmit <- reactiveVal(FALSE)

# If the user selects the Step 2 button in the error summary window, return to Step 2
observeEvent(input$critErrGoBack,{
  updateTabItems(session,"tabs", "reviewerror")
})

observeEvent(input$backToStep2,{
  updateTabItems(session,"tabs", "reviewerror")
})

# If the user has file format errors and chooses to return to step 1:
observeEvent(input$formatErrGoBack,{
  updateTabItems(session,"tabs", "upload")
})


output$submitSummary <- renderUI({
  
  # if submission already successful, hide submit UI
  if (!is.null(submitSuccess())) return(NULL)
  
  # if this research network does not have a hub, no Submit option is available
  if (projectDef$hub_y == 0){
    return(
      tagList(
        tags$h3(class = "row_text title",tags$strong(span("STEP 4 ", class = "text-green")),
                " Submit data"),
        tags$h5(class = "row_text subtitle","Submit dataset for selected concept."),
        fluidRow(
          box(
            width = 10,
            title = span("Submit Data"),
            tagList(
              span(
                tags$h5("This option is only available in research networks that have implemented a Harmonist Hub.")
              )
            )
          )
        )
      )
    )
  }
  
  # if the user does not have a token OR is using the sample dataset, no option to submit should exist
  if ((!hubInfo$fromHub) || useSampleData()){
    if (networkName == "IeDEA"){
      submitInfo <- tags$p("You may select an active data request on the ", 
                           a(" IeDEA Hub", href="http://iedeahub.org", target="_blank")," page.")

    } else {
      submitInfo <- NULL
    }
    return(
      tagList(
        tags$h3(class = "row_text title",tags$strong(span("STEP 4 ", class = "text-green")),
                " Submit data", backToHubMessage()),
        tags$h5(class = "row_text subtitle","Submit dataset for selected concept."),
        fluidRow(
          box(
            width = 10,
            title = span("Submit Data"),
            tagList(
              span(
                tags$h5("This option is only available when submitting datasets for an active concept. "),
                submitInfo
              )
            )
          )
        )
      )
    )
  }
  
  # if the user is responding to data request through the hub...
  if (hubInfo$fromHub){
    if (resetFileInput$reset) return(tags$h3(class = "row_text title", notReadyMessage))
    if (is.null(startDQ())) return(tags$h3(class = "row_text title", notReadyMessage))
    if (is.null(errorTable()[[1]])) return(tags$h3(class = "row_text title", notReadyMessage))
    
    
    concept <- userDetails()$uploadconcept_mr
    downloaders <- paste(sapply(concept()$downloaders, 
                                function(x){paste(x$firstname, x$lastname)}), collapse = ", ")
    
    # compile info about missing tables and columns
    if (is.null(uploadList()$MissingTables) &&
        (is_empty(tablesAndVariables$missingConceptColumns))){
      missingMsg <- tags$li("Includes", span("all variables",
                                             class = "text-green", style = "font-weight: bold"),
                            "requested by", paste0(concept, "."))
    } else {
      numVarsMissing <- tablesAndVariables$missingVariableCount
      numTables <- length(tablesAndVariables$missingConceptColumns)
      connector <- ifelse(numTables == 1, "in", "across")
      missingMsg <- tags$li(span("Missing", numVarsMissing,
                                 makeItPluralOrNot("variable", numVarsMissing),
                                 class = "text-red", style = "font-weight: bold"),
                            "requested by", concept,
                            connector, numTables, paste0(makeItPluralOrNot("table", numTables),"."))
    }
    
    # now compile info about errors
    errors <- errorTable()
    totalErrors <- sum(errors$errorOnlySummary$Count)
    totalWarnings <- sum(errors$warnOnlySummary$Count)
    justCritical <- sum(errors$criticalErrors$Count)
    justErrors <- totalErrors - justCritical
    
    fileFormatErr <- uploadList()$nonmatchingFileFormats
    if (!is_empty(fileFormatErr)){
      justFileFormatErr <- length(fileFormatErr)
    } else {
      justFileFormatErr <- 0
    }
    
    readyMsg <- tags$p(tags$b(span(icon("check-square"),"Ready to transfer data")))
    summaryBoxStatus <- "success" # will change to warning if critical errors exist
    criticalBullet <- NULL
    badFileBullet <- NULL
    warningBullet <- NULL
    criticalFlag <- FALSE
    
    if ((totalErrors + totalWarnings + justFileFormatErr) == 0){
      errorBullet <- tags$li(span("0 data quality issues", 
                                  class = "text-green", style = "font-weight: bold"),
                             "detected.")
      readyToSubmit(TRUE)
      
    } else {
      if (justFileFormatErr > 0){
        badFileMsg <- paste("uploaded", 
                            makeItPluralOrNot("file", length(fileFormatErr)),
                            makeItPluralOrNot("does", length(fileFormatErr)),
                            "not match requested format.",
                            collapse = " ")
      } else {
        badFileMsg <- NULL
      }
      if (justCritical > 0 && justErrors > 0) additional <- "additional"
      else additional <- NULL
      
      if (justCritical > 0) warnAboutExplain <- tags$em("Critical errors require explanation.")
      else warnAboutExplain <- NULL
      
      criticalBullet <- tags$li(span("Critical", class="badge badge-critical"),
                                tags$b(justCritical), "critical",
                                makeItPluralOrNot("error", justCritical), "detected.",
                                warnAboutExplain)
      
      # only include file format info if error
      if (justFileFormatErr > 0){
        badFileBullet <- tags$li(span("Critical", class="badge badge-critical"),
                                 tags$b(justFileFormatErr), badFileMsg,
                                 warnAboutExplain)
      } else badFileBullet <- NULL
      
      
      errorBullet <- tags$li(span("Error", class="badge badge-error"),
                             tags$b(justErrors), additional, 
                             makeItPluralOrNot("error", justErrors), "detected.")
      
      warningBullet <- tags$li(span("Warn", class="badge badge-warn"),
                               tags$b(totalWarnings), 
                               tags$em("possible"),
                               "data quality",
                               makeItPluralOrNot("issue", totalWarnings), "detected.")
      
      if (justCritical + justFileFormatErr == 0) readyToSubmit(TRUE)
      
      # if any major errors exist, remove "Ready to transfer..." message
      if (justCritical > 0){
        criticalFlag <- TRUE
        summaryBoxStatus <- "warning"
        readyMsg <- tags$p(
          tags$strong(span(icon("exclamation-triangle"),"Critical errors found in dataset.")),
          tags$em("We highly recommend that you correct the critical errors offline and upload the revised dataset.",
                  "To review these errors, return to",
                  actionButton("critErrGoBack", "Step 2", style='padding:4px; font-size:100%'),
                  ".",
                  "If you choose to proceed, any remaining critical errors require explanation below."
          )
        )
      }
      # if there were requested files in a format other than a requested format, notify
      if (!is_empty(uploadList()$nonmatchingFileFormats)){
        nonmatchingFlag <- TRUE
        # if critical flag is TRUE, summary box status and ready msg have already been changed. 
        # If critical flag is FALSE, this needs to be done now. Set status to warning and remove ready msg
        if (!criticalFlag){
          summaryBoxStatus <- "warning"
          readyMsg <- ""
        }
        nonmatchingFiles <- uploadList()$nonmatchingFileFormats
        reqFormats <- concept()$requestedFormats
        requestedmsg <- paste(reqFormats, 
                              paste0(".",concept()$requestedExtensions), sep = " (")
        requestedmsg <- glue::glue_collapse(
          paste0(requestedmsg, ")"), 
          sep = ", ",
          last = " and ")
        
        nonmatchingMessage <- 
          
          tags$em(makeItPluralOrNot("File", length(nonmatchingFiles)),
                  glue::glue_collapse(nonmatchingFiles, sep = ", ", last = " and "),
                  makeItPluralOrNot("does", length(nonmatchingFiles)),
                  "not match the",
                  " requested file",  
                  paste0(makeItPluralOrNot("format", length(reqFormats)), ":"), 
                  paste0(requestedmsg, "."),
                  "Note that CSV file format is always an acceptable option.",
                  " To review requested file formats and upload revised files, return to ",
                  actionButton("formatErrGoBack", "Step 1", style='padding:4px; font-size:100%'),
                  "."
          )
        

        fileMsg <- tags$p(
          tags$strong(span(icon("exclamation-triangle"),"Critical file format errors.")),
          nonmatchingMessage
                  
          )
        readyMsg <- tagList(
          readyMsg,
          fileMsg
        )
      }
    }
    
    return(
      tagList(
        tags$h3(class = "row_text title",tags$strong(span("STEP 4 ", class = "text-green")),
                " Submit data", backToHubMessage()),
        tags$h5(class = "row_text subtitle","Submit dataset for selected concept. "),
        fluidRow(
          box(
            width = 10,
            solidHeader = TRUE,
            status = summaryBoxStatus,
            title = span("Transfer Data for IeDEA Concept", style = "color: white"),
            tagList(
              readyMsg,
              tags$p(tags$b("Dataset Summary:")),
              tags$ul(
                tags$li(nrow(formattedTables()[[indexTableName]]), "unique patient records included."),
                tags$li(length(tablesAndVariables$tablesToCheck), "IeDEA DES",
                        makeItPluralOrNot("table", length(tablesAndVariables$tablesToCheck)),
                        "included."),
                missingMsg
              ),
              tags$p(tags$b("Error Summary:")),
              tags$ul(
                criticalBullet,
                badFileBullet,
                errorBullet,
                warningBullet
              ),
              
              tags$p(tags$b("After transfer:")),
              tags$ul(
                tags$li("Uploaded data will be", tags$b("stored for 30 days.")),
                tags$li("Data will be automatically deleted after 30 days. You can manually delete your uploaded datasets via the IeDEA Hub."),
                tags$li("Approved data downloaders will be able to retrieve your data through the Hub.(Downloaders:",
                        paste0(downloaders, ")"))
              )
            )
          )
        )
      )
    )
  }
})

######################## code related to explaining critical errors

criticalSummary <- reactive({
  if (is.null(errorTable()[[1]])) return(NULL)
  if ( (is.null(errorTable()$criticalErrors) ||
        nrow(errorTable()$criticalErrors) == 0) && 
       is_empty(uploadList()$nonmatchingFileFormats) ){
    return(NULL)
  } 
  critical <- errorTable()$criticalErrors
  critical <- critical %>% 
    mutate(tableAndCount = paste0(table, " (", Count, ")")) %>% 
    group_by(category, error_field) %>% 
    summarise(tables = combine_words(tableAndCount)) %>% ungroup() %>% 
    mutate(toShow = paste(category, error_field, "in", tables)) %>% select(toShow)

  if (!is_empty(uploadList()$nonmatchingFileFormats)){
    badFiles <- uploadList()$nonmatchingFileFormats
    badFileMessage <- paste0(makeItPluralOrNot("File", length(badFiles)),
                      " not in compliance with the requested format: ",
                      paste(badFiles, collapse = ", "))
    critical <- critical %>% add_row(toShow = badFileMessage)
  }
  
  

  return(critical)
})



observe({
  if (is.null(errorTable()[[1]])) return(NULL)
  if (is.null(errorTable()$criticalErrors) && 
      is_empty(uploadList()$nonmatchingFileFormats)){
    readyToSubmit(TRUE)
    return(NULL)
  }
  if (nrow(errorTable()$criticalErrors) == 0 && 
      is_empty(uploadList()$nonmatchingFileFormats)){
    readyToSubmit(TRUE)
    return(NULL)
  }
  if (is.null(input$explain1)){
    readyToSubmit(FALSE)
    return(NULL)
  }
  boxes <- criticalSummary()
  boxContents <- list()
  for (boxNum in 1:nrow(boxes)){
    boxContents[[boxNum]] <- input[[paste0("explain", boxNum)]]
  }
  emptyBoxes <- lapply(boxContents, is_blank_or_NA_elements)
  if (any(unlist(emptyBoxes))){
    readyToSubmit(FALSE)
    shinyjs::disable("submit")
    shinyjs::show("mandatoryMessage")
  } else {
    readyToSubmit(TRUE)
    shinyjs::enable("submit")
    shinyjs::hide("mandatoryMessage")   
  }
})

explanations <- reactiveValues(
  boxContents = NULL
)

output$explainCritical <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(startDQ())) return(NULL)
  if (!hubInfo$fromHub) return(NULL)
  if (is.null(errorTable()[[1]])) return(NULL)
  if (is.null(errorTable()$criticalErrors) && is_empty(uploadList()$nonmatchingFileFormats)) return(NULL)
  if (sum(errorTable()$criticalErrors$Count) == 0 && is_empty(uploadList()$nonmatchingFileFormats)) return(NULL)
  if (!is.null(submitSuccess())) return(NULL)
  
  critical <- criticalSummary()
  typesOfCritical <- nrow(critical)

  textBoxList <- list()
  for (error in 1:typesOfCritical){
    print("rendering text boxes")
    textBoxList[[error]] <- textAreaInput(
      paste0("explain", error), 
      label = paste0(error, ". ", critical$toShow[[error]]),
      height = "50px"
    ) %>%
      shiny::tagAppendAttributes(style = 'width: 95%;')
  }
  
  # if there is only one type of critical error and it is actually file format error, 
  # different message:
  if (nrow(critical) == 1 &&
      str_detect(critical$toShow[[1]], "requested format")){
     msg <- "Using the space below, please justify submitting files that do not match the requested format."
  } else {
    msg <- "Using the space below, please justify the inclusion of records found containing critical errors."
  }
  
  return(
    fluidRow(
      tags$p(msg),
      textBoxList
    )
  )
})


output$submitOptions <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(startDQ())) return(NULL)
  if (!hubInfo$fromHub) return(NULL)
  if (is.null(errorTable()[[1]])) return(NULL)
  if (!is.null(submitSuccess())) return(NULL)
  
  # if any files didn't match requested format, explanation required
  # and check for errors to see if explanation is required
  badFormatFlag <- FALSE
  criticalFlag <- FALSE
  if (!is_empty(uploadList()$nonmatchingFileFormats)){
    badFormatFlag <- TRUE
  } 
  if (is.null(errorTable()$criticalErrors)){ #this means there were no errors of any kind
    criticalFlag <- FALSE
  } else if (nrow(errorTable()$criticalErrors) == 0) { #there were errors but none critical
    criticalFlag <- FALSE
  } else if (sum(errorTable()$criticalErrors$Count) > 0) { #critical errors exist
    criticalFlag <- TRUE
  }
  
  explainFlag <- badFormatFlag || criticalFlag
  
  
  if (explainFlag){
    if (criticalFlag){
      criticalMsg <- "critical errors"
    } else {
      criticalMsg <- ""
    }
    
    if (badFormatFlag){
      badFormatMsg <- "file formats"
    } else {
      badFormatMsg <- ""
    }
    
    if (badFormatFlag && criticalFlag){
      connector <- "and"
    } else {
      connector <- ""
    }
    
    warningMsg <- paste("Please correct",
                         criticalMsg, connector, badFormatMsg,
                         "before submitting your dataset. Remaining critical issues must be explained below.",
                        collapse = " ")
  } else {
    warningMsg <- NULL
  }
  
  concept <- userDetails()$uploadconcept_mr
  #create input elements and later make them enabled or disabled, depending on the number of critical errors
  uploadNotesBox <- 
    textAreaInput("uploadNotes",
                  "Message to accompany your file upload (visible to Data Downloaders on the Hub):",
                  width = "100%")
  
  finalizeButtons <- 
    radioButtons("finalize", 
                 label = tagList(
                   "(Optional) Does this complete the", concept, 
                   "data submission from your region?",
                   tags$br(),
                   tags$em(span("This will set your region's data submission status on the Hub. You can change it manually on the Hub (Submit Data page).", style = "font-weight: normal"))),
                 choices = c("Yes, this completes the data submission from my region" = "2", 
                             # 2 is the code for Complete Data in Harmonist 3
                             "No, this is a partial data submission" = "1", 
                             # 1 is the code for Partial Data in Harmonist 3
                             "Do not set data submission status at this time" = "0"
                 ),
                 selected = "0"
    )
  
  # if critical errors exist-----------------------------------------------------------
  if (explainFlag){
    submitStatus <- NULL
    titleText <- "Submit Data with Critical Errors"
    submitButton <- shinyjs::disabled(actionButton("submit", "Submit Data"))
    uploadNotesInput <- uploadNotesBox
    finalizeButtonsInput <- finalizeButtons
    reminderText <- "Complete explanations above"
    submitSolid <- FALSE

    restartBox <- box(
      width = 5,
      solidHeader = TRUE,
      status = "success",
      title = "Review and Correct Errors",
      "Please review your critical issues and upload a revised dataset.",
      tags$br(),
      tags$br(),
      tags$p(actionLink("step4DownloadDetailModal", icon = icon("download"), "Download error detail CSV")),
      tags$br(),
      actionButton("backToStep2", "Return to Step 2", class = "btn-success")
    )
  } else {
    # no critical errors ---------------------------
    restartBox <- NULL
    submitStatus <- "success"
    titleText <- "Submit Data"
    submitButton <- actionButton("submit", "Submit Data", class = "btn-success")
    uploadNotesInput <- uploadNotesBox
    finalizeButtonsInput <- finalizeButtons
    submitStatus <- "success"
    reminderText <- ""
    submitSolid <- TRUE
    
  }
  
  submitBox <-  box(
    width = 5,
    title = titleText,
    solidHeader = submitSolid,
    status = submitStatus,
    fluidRow(tags$p(tags$em(warningMsg))),
    uiOutput("explainCritical"),
    uploadNotesInput,
    finalizeButtonsInput,
    tags$p("Click below to submit your data to secure cloud storage."),
    submitButton,
    tags$em(id ="mandatoryMessage", 
               span(reminderText, style = "color:red"))
  )
  return(
    fluidRow(
      class = "rowUploadComplete",
      submitBox,
      restartBox
    )
  )
})

zipInputDataset <- function(timest, uploadNotes = "", criticalExplanations = ""){
  zipfile <- infile()$zipfile
  if (is.null(zipfile)) {
    files <- infile()$files
    oldwd <- getwd()
    setwd(dirname(files$datapath[[1]]))
    
    # rename files back to their original names
    file.rename(files$datapath, files$name)
    
    zipfile <- tempfile(fileext = ".zip")
    zip::zip(zipfile, files$name)
    
    # rename files again to avoid confusing shiny, might be unnecessary
    file.rename(files$name, files$datapath)
    
    setwd(oldwd)
  }
  
  dir <- tempfile()
  dir.create(dir)
  
  zipDataFileName <- paste0(paste(userDetails()$uploadconcept_mr, userDetails()$regionCode, 
                                  userDetails()$uploaduser_lastname, 
                                  timest, sep = "_"), "_dataset.zip")
  old_wd <- getwd()
  setwd(dir)
  file.copy(zipfile, zipDataFileName)
  
  # create text for file with upload notes/error explanation
  if (uploadNotes == ""){
    uploadNotes <- "The user did not add any upload notes."
  } 
  
  cat("Dataset Upload Information\n",
      "IeDEA Concept:", userDetails()$uploadconcept_mr, 
      paste0(userDetails()$datacall_id, ". ", userDetails()$uploadconcept_title), "\n",
      "Dataset uploaded by", userDetails()$uploaduser_firstname, userDetails()$uploaduser_lastname,
      "from IeDEA Region", userDetails()$regionCode,"\n",
      "Uploaded on", format(as.Date(timest,"%Y%m%d%H%M"), "%Y-%m-%d"),
      "at", format(as.Date(timest,"%Y%m%d%H%M"), "%H:%M"), "\n",
      "Upload notes:\n",
      uploadNotes, "\n",
      criticalExplanations,
      file="upload_notes.txt")

  fileName <- tempfile(fileext = ".zip")
  zip::zip(fileName, c(zipDataFileName, "upload_notes.txt"))
  setwd(old_wd)
  unlink(dir, recursive = TRUE)
  
  return(fileName)
}

storeDatasetAWS <- function(zipfile, timest){
  conceptID <- userDetails()$uploadconcept_mr
  
  awsPath <- paste0(paste(conceptID, userDetails()$regionCode, userDetails()$uploaduser_id, 
                          timest, sep = "/"),"/")
  
  objectName <- paste0(paste(conceptID, userDetails()$regionCode, userDetails()$uploaduser_lastname, 
                             timest, sep = "_"), ".zip")
  awsObject <- paste0(awsPath, objectName)  
  submitResult <- aws.s3::put_object(file = zipfile, object = awsObject, bucket = AWS_bucket_name)

  if (!submitResult) submitSuccess(FALSE)
  if (submitResult){
    submitSuccess(TRUE)
  }
  return(list(awsPath = awsPath,
              zipFileName = objectName))
}

storeUploadDetailsAWS <- function(uploadDetails, reportFile, timest) {

  conceptID <- userDetails()$uploadconcept_mr

  awsPath <- "pending/"

  # store upload details as json
  objectName <- paste0(paste(conceptID, userDetails()$regionCode, userDetails()$uploaduser_lastname,
                             timest, "details", sep = "_"), ".json")
  awsObject <- paste0(awsPath, objectName)

  data <- jsonlite::toJSON(as.data.frame(uploadDetails))
  submitResult <- aws.s3::put_object(file = charToRaw(data), object = awsObject, bucket = AWS_bucket_name)
  if (!submitResult) return(submitResult)

  # store report pdf
  awsObject <- paste0(awsPath, basename(reportFile))
  submitResult <- aws.s3::put_object(file = reportFile, object = awsObject, bucket = AWS_bucket_name)

  return(submitResult)
}

createSubmissionReport <- function(type = c("PDF", "html")) {
  dir <- tempfile("dir")
  dir.create(dir)

  # create report name
  reportName <- paste0("Report",
                       paste(userDetails()$uploadconcept_mr, userDetails()$regionCode,
                             userDetails()$uploaduser_lastname,
                             format(Sys.time(),"%Y%m%d%H%M"), sep = "_"),
                       ".", tolower(type))
  filename <- file.path(dir, reportName)

  createReport(file = filename, reportType = type,
               includeHistograms = TRUE,
               includeDataSummary = TRUE,
               includeErrorSummary = TRUE) 
             #  datasetDesc = NULL)

  return(list(dir = dir, filename = filename))
}

observeEvent(input$submit,{
  lastActivity(Sys.time())
  reportModal(titleText = "Storing dataset in secure cloud storage",
              message = "Please wait until dataset transfer is complete")
  # save criticalError explanations in explanations$boxContents
  boxes <- criticalSummary()
  if (is.null(boxes)){
    # no critical errors
    criticalExplanations <- ""
  } else {
    # critical errors are present, put together explanations
    boxContents <- list()
    for (boxNum in 1:nrow(boxes)){
      boxContents[[boxNum]] <- paste0(boxNum, ". ", boxes$toShow[[boxNum]], ". Explanation: ",
                                      input[[paste0("explain", boxNum)]]
                                      )
    }
    explanations$boxContents <- boxContents
    criticalExplanations <- paste0(
      "\nCritical errors found in this dataset are listed below along with the user-provided justification for including these records.\n",
      paste(boxContents, collapse = "\n")
    )
  }

  # zip dataset, generate report, submit dataset
  uploadTimestamp <- format(Sys.time(),"%Y%m%d%H%M")
  zipfile <- zipInputDataset(timest = uploadTimestamp,
                             uploadNotes = input$uploadNotes,
                             criticalExplanations = criticalExplanations)
  on.exit(unlink(zipfile), add = TRUE)



  # attempt to upload dataset to AWS
  uploadInfo <- storeDatasetAWS(zipfile, uploadTimestamp)
  if (!submitSuccess()) {
    # failed to submit dataset to AWS
    removeModal()
    errorMessageModal("Failed to submit dataset! Please contact Judy Lewis.")
    return()
  }

  # successfully uploaded dataset to AWS
  if (input$finalize != "0") postParticipationStatus(input$finalize)

  # setup upload details
  partialFailure <- FALSE
  uploadDetails <- list(
    "data_assoc_concept" = userDetails()$uploadconcept_id, #from Harmonist 18
    "data_assoc_request" = userDetails()$datacall_id, #from Harmonist 18
    "data_upload_person" = userDetails()$uploaduser_id, #from Harmonist 18
    "data_upload_region" = userDetails()$uploadregion_id, #from Harmonist 18
    "responsecomplete_ts" = as.character(Sys.time()),
    "upload_notes" = input$uploadNotes,
    "data_upload_n" = length(formattedTables()[[indexTableName]][[patientVar]]),
    "data_upload_bucket" = AWS_bucket_name,
    "data_upload_folder" = uploadInfo$awsPath,
    "data_upload_zip" = uploadInfo$zipFileName,
    "critical_explanation" = criticalExplanations
  )

  reportModal(titleText = "Creating dataset report for access in IeDEA Hub",
              message = "Please wait until report is complete")
  # create report
  reportType <- "PDF"
  reportInfo <- createSubmissionReport(reportType)
  on.exit(unlink(reportInfo$dir, recursive = TRUE), add = TRUE)

  reportModal(titleText = "Saving dataset report in IeDEA Hub",
              message = "Please wait until report submission is complete")
  # try to get REDCap record id
  submitRecordNum <- getREDCapRecordID(tokenForHarmonist9)
  if (!inherits(submitRecordNum, "postFailure")) {
    # successfully got REDCap record id
    uploadDetails[["record_id"]] <- submitRecordNum

    # try to submit upload details to REDCap
    data <- jsonlite::toJSON(as.data.frame(uploadDetails))
    result <- redcapPOST(url = redcap_url, encode = "form",
                         body = list(token = tokenForHarmonist9, content = "record",
                                     format = "json", data = data),
                         .description = "Attempting to create record")

    if (!inherits(result, "postFailure")) {
      # try to upload report file
      result <- uploadREDCapFile(reportInfo$filename, tokenForHarmonist9,
                                 submitRecordNum,
                                 paste0("data_upload_", tolower(reportType)))
    }

    if (inherits(result, "postFailure")) {
      # failed to submit upload info and/or report to REDCap
      partialFailure <- TRUE
    }
  } else {
    # failed to get REDCap record id
    partialFailure <- TRUE
  }

  # If there was a partial failure, that means the dataset was uploaded to
  # AWS, but no REDCap record was created.
  if (!partialFailure) {
    postProgress(list(action_step = "submitdata",
                      upload_filenames = paste(input$loaded$name, collapse = ",")))
    removeModal()
  } else {
    # attempting to store upload details in AWS
    result <- storeUploadDetailsAWS(uploadDetails, reportInfo$filename, uploadTimestamp)
    if (!result) {
      # failed to store upload details in AWS, which means the dataset has
      # been orphaned
      cat("Failed to store upload details in AWS\n", file = stderr())
      removeModal()
      errorMessageModal("There was an error while submitting the dataset. Please contact Judy Lewis.")
    } else {
      # maybe not even tell the user about this
      removeModal()
      errorMessageModal("There was a partial error when submitting the dataset. Please contact Judy Lewis.")
    }
  }
})

output$submitOutcome <- renderUI({
  if (is.null(submitSuccess())) return(NULL)
  if (!submitSuccess()) {
    submitResults <- 
      box(class = "alert alert-danger",
          width = 10, 
          title = span("Error encountered submitting dataset", style = "color: white"),
          tagList(
            tags$h3("Please contact judy.lewis@vumc.org")#,
          )
      )
  }
  if (submitSuccess()){
    submitResults <- 
      fluidRow(class = "submit-success-msg",
               tags$div(class = "alert alert-success",
                        tagList(
                          tags$p(span(icon("check-square"),"Your dataset has been stored securely for retrieval by the requesting investigator and will be deleted after 30 days"))#,
                        )
               ))
  }
  tagList(
    fluidRow(
      tags$h3(class = "row_text title",tags$strong(span(icon("check"), "Upload Success", class = "text-green")),
              backToHubMessage())
    ),
    submitResults
  )
})

output$afterSubmit <- renderUI({
  if (is.null(submitSuccess())) return(NULL)
  fluidRow(
    box(
      tagList(
        tags$p("Further Toolkit options (available in the sidebar or below):",
               tags$li(actionLink("visualize", "Visualize dataset")),
               tags$li(actionLink("restart", "Upload a revised dataset")),
               tags$li(actionLink("feedback", "Provide feedback to the IeDEA Harmonist development team")),
               tags$li(actionLink("exitaftersubmit", "Exit the data toolkit")))
      )
    )
  )
})

observeEvent(input$visualize,{
  updateTabItems(session, "tabs", "interactivePlots")
})

observeEvent(input$restart,{
  updateTabItems(session, "tabs", "upload")
})

observeEvent(input$feedback,{
  updateTabItems(session, "tabs", "feedback")
})

observeEvent(input$step4DownloadDetailModal,{
  lastActivity(Sys.time())
  downloadDetailModal()
  
})

