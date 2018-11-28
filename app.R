# app.R
# 
# The goals of this Shiny App are as follows:
#   1. Read in files (zip, csv, SAS, SPSS, or Stata) containing tables adhering to the IeDEA DES 
#   2. Check for presence of required variables and tables
#   3. Conduct data quality checks
#   4. Provide feedback on possible data quality errors, interactively and in downloadable form 
#   5. Create reports summarizing dataset characteristics and data quality errors
#   6. Allow interactive visualization of dataset
#   7. Submit checked files to secure file storage (authenticated users)
# 
#   Judy Lewis, PhD, Vanderbilt Institute for Clinical and Translational Research
#   Jeremy Stephens, Vanderbilt University Department of Biostatistics
#   Version 1: August 10, 2017
#   

library(shiny)
library(shinydashboard)
library(rjson)
library(tools)
library(lubridate)
library(shinyjs)
library(rmarkdown)
library(knitr)
library(haven)
library(readr)
library(DT)
library(tidyverse)
library(ggplot2)
library(plotly)
library(aws.s3)
library(cowplot)
library(dashboardthemes)
library(httr)
library(jsonlite)
library(kableExtra)
library(Hmisc)
library(scales)
library(filesstrings)
library(purrr)
library(htmltools)
library(data.table)


source("server_name.R", local = TRUE)
source("redcapTokens.R", local = TRUE)
source("definitions.R", local = TRUE)
source("awsKey.R", local = TRUE)
source("helpers.R", local = TRUE)
source("initializeErrorFrames.R", local = TRUE)
source("text.R")

###############################
# All column names in uploaded spreadsheets will be converted to all caps since variable names in 
# the IeDEA data specification are all caps.
###############################

shinyUI <- dashboardPage(

  title = "IeDEA Harmonist Data Toolkit",
                         
  # custom header to include badge to indicate presence or absence of an active data request
  tags$header(
    span(
      img(src="iedea_logo-100x40.png"),
      span("Harmonist Data Toolkit",class="harmonist-title"),
      class = "harmonist-logo"
    ),
    uiOutput("requestStatus", class = "request-status"),
    class = "main-header"
  ),

  # Sidebar panel
  dashboardSidebar(width = 260,
                   collapsed = FALSE,
                   sidebarMenuOutput("menu"),
                   uiOutput("exitUI"),
                   uiOutput("warnOnQuit")
  ),
  
  #main Panel
  dashboardBody(
    shinyjs::useShinyjs(),
    
    tabItems(
      tabItem(tabName = "welcome",
              uiOutput("welcomeUI")
      ),
      tabItem(tabName = "upload",
              fluidPage(
                uiOutput("uploadIntro"),
                uiOutput("dataRequestInfo"),
                uiOutput("uploadMissingSummary"),
                uiOutput("uploadSummary"),
                uiOutput("selectFiles")
              )
      ),
      tabItem(tabName = "summaryReports",
              fluidPage(
                uiOutput("reportPage")
              )
      ),      
      tabItem(tabName = "submit",
              fluidPage(
                uiOutput("submitToAWS"),
                uiOutput("submitOutcome"),
                uiOutput("afterSubmit")
              )
      ),
      tabItem(tabName = "interactivePlots",
              uiOutput("plotUI")
              ),
      tabItem(tabName = "help",
              uiOutput("helpTabUI")
              ),
      tabItem(tabName = "feedback",
              uiOutput("feedbackTabUI")
              ),
      tabItem(tabName = "reviewerror",
              fluidPage(
                uiOutput("errorSummarySep")
                )
      )
    ),
    tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.css"),
     tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
  )
)

shinyServer <- function(input, output, session){

  # JUDY revisit this: is it necessary? pull from REDCap
  toReport <- rjson::fromJSON(file = "datasetSummary.json") 
  
  sessionID <- reactive(session$token)
  
  # indicator of REDCap API status
  redcapUp <- reactiveVal(TRUE)

  
  source("welcomeTab.R", local = TRUE)
  source("uploadTab.R", local = TRUE)
  source("reviewerrorTab.R", local = TRUE)
  source("summaryReportsTab.R", local = TRUE)
  source("submitTab.R", local = TRUE)
  source("helpTab.R", local = TRUE)
  source("feedbackTab.R", local = TRUE)
  source("exitTab.R", local = TRUE)
  
  source("REDCapFunctions.R", local = TRUE)
  source("modalFunctions.R", local = TRUE)
  source("dataQuality.R", local = TRUE)
  source("dateChecking.R", local = TRUE)
  source("dataChecks.R", local = TRUE)
  source("dateComparisons.R", local = TRUE)
  source("dataQuality.R", local = TRUE)
  source("reportGenerationOptions.R", local = TRUE)
  
  # code for report generation ---------------------------------------
  source("downloadReports.R", local = TRUE)
  source("downloadErrorDetails.R", local = TRUE)
  
  # code for interactive plot exploration-----------------------------
  source("interactivePlotting.R", local = TRUE)
  
  # store region codes and names in a data frame-- either from REDCap or if REDCap not available,
  # the codes and names are provided (must be updated manually)
  regionData <- getAllRegionInfo() # redcapUp will be reset to FALSE here if redcap error
  
  # reactive values
  lastActivity <- reactiveVal(isolate(Sys.time()))
  
  # reload guard is javascript function that prevents application from closing. Must be set to FALSE 
  # to allow application to close session
  reloadGuardOn <- reactiveVal(TRUE)
  
  # useSampleData is set to TRUE if the user chooses to run the Toolkit with the sample dataset provided
  # In this case, the user should be prevented from uploading the dataset to the Hub in response to a data request
  useSampleData <- reactiveVal(FALSE)
  
  # errorCount is a reactive value that counts the number of errors detected so far. If it exceeds the value
  # specified in definitions.R then errorExcess is set to TRUE and errors are no longer accumulated
  # This is a temporary fix to address memory problems
  errorCount <- reactiveVal(0)
  errorExcess <- reactiveVal(FALSE)
  
  # startDQ is a reactive variable that indicates that the user is ready for data quality checks to begin
  startDQ <- reactiveVal(NULL)
  
  # hubInfo is a reactive variable that indicates whether or not the user arrived at the application 
  # with a valid token (fromHub will be TRUE) and stores the details about the user if they have arrived 
  # with a valid token
  hubInfo <- reactiveValues(
    fromHub = NULL,
    userDetails = NULL
  )
  
  # resetFileInput - reactive resetFileInput$reset value to indicate if user is starting over with new files
  # initially, reset = FALSE. If user chooses uploading a new dataset, reset = TRUE 
  # When reset == TRUE, all variables and UI associated with uploaded dataset reset to NULL
  resetFileInput <- reactiveValues(
    reset = FALSE,
    newData = FALSE
  )
  
  # tablesAndVariables - reactive value to store lists of uploaded tables and variables
  tablesAndVariables <- reactiveValues()
  
  # submitSuccess reactive Val to indicate if effort to submit dataset to AWS was successful or not
  submitSuccess <- reactiveVal(NULL)
  

  #logic to terminate application execution if idle
  observe({
    # Re-execute this reactive expression after 10 minutes (600,000 milliseconds).
    # This expression could also be re-executed when the lastActivity value changes, so it needs to be checked. 
    if (is.null(infile())) return()
    invalidateLater(intervalToCheckUserActivity, session)
    idle <- difftime(Sys.time(), isolate(lastActivity()), units = "mins") # number of minutes since last activity
    if (idle >= idleMinToWarn && idle < idleMinToExit ) {
      reloadGuardOn(FALSE)
      
      # x minutes of inactivity
      # Show modal dialog to warn user that session is timing out in 10 minutes
      idleWarningModal(idleTime = round(idle, digits = 0))
    }
    else if (idle >= idleMinToExit) {
      
      idleExitModal(idleTime = round(idle, digits = 0))
      exitActions()
      }
  })
  
  
  getUserInfo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    encryptedToken <- query[["tokendt"]]
    if (is.null(encryptedToken)){
      return(NULL)
    }
    if (!redcapUp()){
      errorMessageModal(messageHeader = "REDCap Error",
                        message = "Inability to access REDCap at this time", 
                        secondaryMessage = "Session will proceed without access to data submission")
      updateTabItems(session,"tabs", "welcome")
      return(NULL)
    }
    
    invalidToken <- FALSE
    decryptedToken <- getCrypt(encryptedToken,"d")

    if (is.null(decryptedToken)) invalidToken <- TRUE 
    else {
      userInfo <- getOneRecord(tokenForHarmonist18, decryptedToken, projectName = "Harmonist 18")
      if (is_empty(userInfo) || exists("error", userInfo)) invalidToken <-  TRUE
      else userInfo$decryptedToken <- decryptedToken
    }

    if (invalidToken){
      # ask steph here about modal
      errorMessageModal(messageHeader = "REDCap Error",
                        message = "Invalid REDCap token or inability to access REDCap.", 
                        secondaryMessage = "Session will proceed without access to data submission")
      updateTabItems(session,"tabs", "welcome")
      return(NULL)
    }
    return(userInfo)
  })
  

  
  
  observeEvent(session$token,{
    postProgress(list(action_step = "start"))
      userInfo <- getUserInfo()
      if (is.null(userInfo)){
        hubInfo$fromHub <- FALSE
        hubInfo$userDetails <- defaultUserInfo # in definitions.R

      } else {
        hubInfo$fromHub <- TRUE
        updateTabItems(session,"tabs", "upload")
        regionID <- as.numeric(userInfo[["uploadregion_id"]])
        userInfo[["regionName"]] <- regionData$region_name[[regionID]] 
        userInfo[["regionCode"]] <- regionData$region_code[[regionID]] 
        hubInfo$userDetails <- as.list(userInfo)
        
      }
  })
  
  # backToHubMessage - reactive variable, adds option at top of every page to return to the IeDEA Hub
  # *if* the user came from the Hub. Otherwise, no option to return to Hub
  backToHubMessage <- reactive({
    if (is.null(hubInfo$fromHub)) return(NULL)
    if (hubInfo$fromHub){
      
      hub_url <- paste0(plugin_url, "?token=",
                       userDetails()$uploadhub_token, "&option=sop",
                       "&record=", userDetails()$datacall_id,
                       "&pid=58325")
      tags$a(span(paste(userDetails()$uploadconcept_mr,"on Hub"),icon("external-link")), 
             href = hub_url, target="_blank", class = "backIedea")
    } else {
      return(NULL)
    }
  })
  
  # if user decides to upload new dataset, unlink previously loaded dataset and change reset to TRUE
  observeEvent(input$uploadNew,{
    lastActivity(Sys.time())
    unlink(infile()$files$datapath)
    startDQ(NULL)
    resetFileInput$reset <- TRUE
    resetFileInput$newData <- FALSE
    useSampleData(FALSE)
    submitSuccess(NULL)
  })
  
  observeEvent(input$loaded,{
    resetFileInput$newData <- TRUE
    resetFileInput$reset <- FALSE
  })
  

  
 # If user clicks button on Welcome tab labeled "Continue to Step 1" change active tab to Step 1: Upload files
  observeEvent(input$fromWelcomeToStep1,{
    updateTabItems(session,"tabs", "upload")
  })
  
  # If user clicks button on Upload tab labeled "Continue to Step 2" change active tab to Step 2: Review data checks
  observeEvent(input$step2,{
    startDQ(TRUE)
    formattedTables()
    errorTable()
    updateTabItems(session,"tabs", "reviewerror")
  })
 
  # If user clicks button on reviewerrors tab labeled "Continue to Step 3" change active tab to Step 3: create summary
  observeEvent(input$step3,{
    updateTabItems(session,"tabs", "summaryReports")
  })
  
  # If user clicks button on Create summary tab labeled "Continue to Step 4" change active tab to Step 4: Submit data
  observeEvent(input$step4,{
    updateTabItems(session,"tabs", "submit")
  })
  
  # reactive variable to track if URL query string was already updated in response to browser navigation arrow click
  justUpdated <- reactiveVal(FALSE)
  
  observeEvent(input$tabs,{
    if (justUpdated()) {
      justUpdated(FALSE)
      return(NULL)
    }
    tokenString <- NULL
    token <- getQueryString()[["tokendt"]]
    if (!is_empty(token)){
      tokenString <- paste0("tokendt=", token, "&")
    }
 
    lastActivity(Sys.time())
    req(sessionID())
    tab <- input$tabs
    if ((tab == "reviewerror") && (!is.null(uploadedTables())) && is.null(startDQ())){
      startDQ(TRUE)
      formattedTables()
      errorTable()
    }
    updateQueryString(paste0("?",tokenString,"tab=",input$tabs), mode = "push")
    if (tab == "help"){
      postProgress(list(action_step = "help"))
    } else if ((tab == "reviewerror") && !is.null(errorTable()[[1]])){
      postProgress(list(action_step = "reviewerror"))
    }
  })
  
  observeEvent(getQueryString()[["tab"]],{
    req(input$tabs)
    
    newTabRequest <- getQueryString()[["tab"]]
    justUpdated(FALSE)
    if (newTabRequest != input$tabs){
      updateTabItems(session, "tabs", newTabRequest)
      justUpdated(TRUE)
    }
  })
  

  # userDetails ------------------------------------------------------------
  # user information either passed through URL if from iedeahub or selected by user otherwise
  userDetails <- reactive({
    if (is.null(hubInfo$userDetails)) return(NULL)
    return(hubInfo$userDetails)
  })
  
  #read in example concept description json. In the future: choose specific concept as determined by token in URL
  concept <- reactive({
    if (!hubInfo$fromHub) return(rjson::fromJSON(file = "concept0.json"))
    record_id_Harmonist3 <- hubInfo$userDetails$datacall_id
    conceptInfo <- as.list(getOneRecord(tokenForHarmonist3, record_id_Harmonist3, projectName = "Harmonist 3"))
    conceptInfo$tablefields <- rjson::fromJSON(conceptInfo$shiny_json)
    conceptInfo$contact1 <- getOneRecord(tokenForHarmonist5, conceptInfo$sop_creator, projectName = "Harmonist 5")
    conceptInfo$contact2 <- getOneRecord(tokenForHarmonist5, conceptInfo$sop_creator2, projectName = "Harmonist 5")
    conceptInfo$datacontact <- getOneRecord(tokenForHarmonist5, conceptInfo$sop_datacontact, projectName = "Harmonist 5")
    return(conceptInfo)
  }) 
  
  # request status badge in header, persists, indicates if user is responding to active data request
  output$requestStatus <- renderUI({
    if (is.null(hubInfo$userDetails)) return(NULL)
    
    if (hubInfo$fromHub){
      region <- userDetails()$regionCode
      name <- paste(userDetails()$uploaduser_firstname, userDetails()$uploaduser_lastname)
      return(
        tagList(span(region, class="label label-primary"), span(name))
      )
    } else {
      return(
        tagList(span("No active data request", class="badge badge-request"))
      )
    }
  })
  
  # infile -----------------------------------------------------------------
  # infile is a reactive variable that returns link to files selected and uploaded to toolkit
  # (unzipped if a zip file was selected)
  infile <- reactive({
    if (resetFileInput$reset) return(NULL) 
    if (!resetFileInput$newData) return(NULL)
    req(session)
    req(userDetails())
    lastActivity(isolate(Sys.time()))
    if (useSampleData()){
      dir <- tempfile(pattern = 'dir')
      zipPath <- file.path("www", "sampleTables.zip")
      filenames <- unzip(zipPath, exdir = dir)
      result <- as.data.frame(t(sapply(filenames, function(filename) {
        list(name = basename(filename), size = file.size(filename), type = "", datapath = filename)
      })))
      return(list(files = result))
    }
    
    if (is.null(input$loaded)) return(NULL)
    
    postProgress(list(action_step = "uploaddata", 
                      toolkituser_id = userDetails()$uploaduser_id,
                      datarequest_id = userDetails()$datacall_id,
                      upload_filenames = paste(input$loaded$name,collapse = ","), 
                      upload_filesize = paste(input$loaded$size,collapse = ",")))
    
    zipTypes <- c("application/zip", "application/x-zip-compressed")
    
    # check to see if the user uploaded a zip file
    if (any(input$loaded$type %in% zipTypes)) {
      # if a zip file was shared, make sure that only one file was uploaded
      if (nrow(input$loaded) > 1) {
        errorMessageModal(messageHeader = "Multiple ZIP Files Detected",
                          message = "You must either upload a single ZIP file or multiple data files.")
        return(NULL)
      }
      
      # save zip file path in case the user wants to upload it later
      zipfile <- input$loaded$datapath

      # extract the zip file and return an error if the unzip function is not successful
      filenames <- tryCatch(unzip(zipfile, exdir = dirname(input$loaded$datapath)), 
                            warning = function(w) return(NULL),
                            error = function(e) return(NULL)
      )
      if (is.null(filenames)){
        errorMessageModal(
          messageHeader = "Unable To Open ZIP File",
          message = paste0("Error: ", input$loaded$name, " does not appear to be saved in a valid ZIP file format. Please confirm that binary compression or other non-standard ZIP format was not used to save the dataset."))
        return(NULL)
      }
      result <- as.data.frame(t(sapply(filenames, function(filename) {
        list(name = basename(filename), size = file.size(filename), type = "", datapath = filename)
      })))
      #unlink(input$loaded$datapath) # check to see if this works
      list(files = result, zipfile = zipfile)
    } else {
      # user uploaded one or more non-zip files
      list(files = input$loaded)
    }
  })
  
  # readOneTable ------------------------------------------------------------
  # readOneTable reads in one table at a time and converts the column names to all caps 
  # to match the IeDEA DES and forces table 
  
  readOneTable <- function(tableName){
    print("Begin ReadOneTable")
    inputLink <- infile()$files
    index <- match(tolower(tableName), tolower(file_path_sans_ext(inputLink$name)))
    fileExt <- tolower(file_ext(inputLink$name[index]))
    updateModal(paste0("Reading file: ", inputLink$name[index]))
    # read csv files
    if (fileExt == "csv"){
      myfile <- tryCatch(read_csv(inputLink[[index, 'datapath']],
                                  col_types = cols(.default = "c"),
                                  na = character(),
                                  trim_ws = TRUE),
                         # warning = function(w) {},# JUDY revisit this. Warning occurs if strange characters in fields
                         error = function(e) {
                           fileReadError(extension = fileExt, 
                                         fileName = inputLink$name[index],
                                         errorMessage = e$message)
                           resetFileInput$reset <- TRUE
                           return(NULL)
                         })
    }
    # read SAS sas7bdat files
    else if (fileExt == "sas7bdat"){
      myfile <- tryCatch(read_sas(inputLink[[index, 'datapath']]),
                         error = function(e){
                           fileReadError(extension = fileExt, 
                                         fileName = inputLink$name[index],
                                         errorMessage = e$message)
                           resetFileInput$reset <- TRUE
                           return(NULL)
                         })
    }
    # read Stata DTA files
    else if (fileExt == "dta"){
      myfile <- tryCatch(read_dta(inputLink[[index, 'datapath']]),
                    error = function(e){
                      fileReadError(extension = fileExt, 
                                    fileName = inputLink$name[index],
                                    errorMessage = e$message)
                      resetFileInput$reset <- TRUE
                      return(NULL)
                    })
    }
    # read SPSS SAV files
    else if (fileExt == "sav"){
      myfile <- tryCatch(read_sav(inputLink[[index, 'datapath']]),
                    error = function(e){
                      fileReadError(extension = fileExt, 
                                    fileName = inputLink$name[index],
                                    errorMessage = e$message)
                      resetFileInput$reset <- TRUE
                      return(NULL)
                    })
    }
    else {
      errorMessageModal(messageHeader = "Invalid File Type",
                        message = "Valid file extensions: .csv, .sas7bdat, .dat, .sav")
      resetFileInput$reset <- TRUE
      return(NULL)
    }

    if (!is.null(myfile)){
      myfile <- as.data.frame(myfile)
      # determine if any of the column names include non-alphabetic characters
      # because toupper will crash 
      badColumns <- which(grepl("\\W", names(myfile)))
      names(myfile)[badColumns] <- paste0(iconv(names(myfile)[badColumns], from = "", to = "ASCII",""),
                                          "_INVALID_CHARACTERS_IN_VARIABLE_NAME")
      names(myfile) <- toupper(names(myfile))
    }
    print("End ReadOneTable")
    return(myfile)
  }
  
  
  # uploadList() ------------------------------------------------------------
  # This function reacts and executes when files have been selected for uploading
  # The uploaded file names are compared with requested filenames and a list of matching tables,
  # extra tables, and missing tables is returned. Regardless of the case (lower, upper or mixed)
  # of the file names of the tables, the lists of table names will be reassigned to follow the 
  # mixed case DES standard.
  
  uploadList <- reactive ({
    if (resetFileInput$reset) return(NULL)
    loaded <- infile()
    if (is.null(loaded) || is.null(loaded$files))
      return(NULL)
    
    allfiles <- loaded$files
    # confirm that all files have extensions reflecting valid file formats for Harmonist (stored in definitions.R)
    allfiles$extension <- tolower(file_ext(allfiles$name))
    nonDESTypes <- !allfiles$extension %in% validFileTypes
    if (all(nonDESTypes)){
      listOfFiles <- lapply(allfiles$name, function(x){return(tags$li(x))})
      
      messageHeader <- "No Valid IeDEA Files Detected" 
      message <- tagList(
        tags$p("Valid file types include ", paste(validFileTypesToDisplay, collapse = ", "), ". "),
        tags$p("Uploaded file(s) detected: "), 
        listOfFiles)
      
      errorMessageModal(messageHeader = messageHeader, 
                        message = message)
      resetFileInput$reset <- TRUE
      return(NULL)
    }

    allfiles$tableName <- tolower(tolower(file_path_sans_ext(allfiles$name)))
    invalidFiles <- allfiles[which( (!allfiles$tableName %in% tolower(names(tableDef))) |
                                      nonDESTypes),]

    # confirm that all of the uploaded files are of an acceptable file type --in addition to valid file types. This is to prohibit
    # uploading of executable files, etc
    prohibitedFiles <- !(tolower(invalidFiles$extension) %in% c(allowedExtraFileTypes, validFileTypes))
    if (any(prohibitedFiles)){
      badFiles <-  invalidFiles[which(prohibitedFiles)]
      
      messagePart1 <- case_when(length(badFiles) == 1 ~
                                  "The following non-IeDEA file is a prohibited file type: ",
                                length(badFiles) > 1 ~
                                  "The following non-IeDEA files are prohibited file types: ")
     
      errorMessageModal(messageHeader = "Prohibited File Type Detected",
                        message = paste0(messagePart1, 
                               paste(badFiles$name, collapse =  ",")))
      resetFileInput$reset <- TRUE
      return(NULL)
    }
   
   
    validFiles <- allfiles[which(!(allfiles$name %in% invalidFiles$name)),]
    
    # check to make sure that tblbas is included in valid uploaded files
    if (!("tblbas" %in% validFiles$tableName)){
      listOfFiles <- lapply(allfiles$name, function(x){return(tags$li(x))})

      # first see if tblbas was actually included but wrong file type
      if ("tblbas" %in% invalidFiles$tableName){
        errorMessageModal(messageHeader = "tblBAS: Invalid File Format",
                          message = tagList(
                            tags$p(paste0("The core data table (tblBAS) is required for all IeDEA DES data uploads and must be saved in one of the accepted formats (", validFileTypesToDisplay, ").")),
                            tags$p("You uploaded the following files: "), 
                            listOfFiles  ))
        resetFileInput$reset <- TRUE
        return(NULL)
      }
      # otherwise, no tblBAS found at all
      else {
        errorMessageModal(messageHeader = "tblBAS Not Detected",
                          message = tagList(
                            tags$p("The core data table (tblBAS) is required for all IeDEA DES data uploads."),
                            tags$p("The following files were uploaded: ", 
                                    listOfFiles)))
        resetFileInput$reset <- TRUE
        return(NULL)
      }
    }
    
    uploaded <- file_path_sans_ext(validFiles$name)
    needed <- names(concept()$tablefields) # JUDY check on this later: shouldn't concept be reactive variable, not global?
    tablesUploadedMatching <- tolower(needed) %in% tolower(uploaded)
    matchingTables <- needed[tablesUploadedMatching]
    # make sure tblBAS listed first in matchingTables #JUDY use table_order from REDCap here
    matchingTables <- intersect(names(tableDef), matchingTables)
    missingTables <- needed[!tablesUploadedMatching]
    if (is_empty(missingTables)) missingTables <- NULL

    tablesUploadedNeeded <- tolower(uploaded) %in% tolower(needed)  
    extraTables <- uploaded[!tablesUploadedNeeded]
    extraUploadedDESTables <- tolower(names(tableDef)) %in% tolower(extraTables)
    extraDESTables <- names(tableDef)[extraUploadedDESTables]
    extraNonDESTables <- uploaded[!(tolower(uploaded) %in% tolower(names(tableDef)))]
    extraFiles <- invalidFiles$name
    
    allDESTables <- c(matchingTables, extraDESTables)
    tablesWithPatientID <- intersect(allDESTables, allTablesWithPatientID)
    tablesWithNoPatientID <- allDESTables[!allDESTables %in% allTablesWithPatientID]
    # JUDY CHANGE WHEN FIGURE OUT PREGNANCY-RELATED TABLES: For now, ignore all pregnancy-related tables
    # Remove the following line when that logic is added to Harmonist
    nonPregTables <- setdiff(tablesWithNoPatientID, pregnancyTables)
    allDESTables <- c(tablesWithPatientID, nonPregTables)
    # edit tablesWithPatientID to NOT include tblBAS since that is the required PATIENT table and we often want to merge it with all other PATIENT tables
    tablesWithPatientID <- tablesWithPatientID[!(tablesWithPatientID=="tblBAS")] #tables other than tblBAS with PATIENT as ID
    uploadedFiles <- list(matchingTables, extraDESTables, extraNonDESTables, extraFiles,
                          missingTables, allDESTables, tablesWithPatientID, tablesWithNoPatientID)
    names(uploadedFiles) <- c("MatchingTables","ExtraDESTables", "ExtraTables", "ExtraFiles",
                              "MissingTables", "AllDESTables", "tablesWithPatientID", "tablesWithNoPatientID")
    return(uploadedFiles)
  })
  
  # checkTableForMissingColumns ---------------------------------------------
  checkTableForMissingColumns <- function(table, tableName){
    uploadedColumnNames <- names(table)
    requiredColumns <- findVariablesMatchingCondition(tableName, "variable_required", "1")
    missingRequiredColumns <- requiredColumns[which(!(requiredColumns %in% uploadedColumnNames))]
    if (is_empty(missingRequiredColumns)) missingRequiredColumns <- NULL
    
    if (tableName %in% uploadList()$MatchingTables){
      requestedColumns <- get(tableName, concept()$tablefields)
      additionalRequestedColumns <- requestedColumns[which(!requestedColumns %in% requiredColumns)]
      missingAdditionalRequestedColumns <- additionalRequestedColumns[which(!(additionalRequestedColumns %in% uploadedColumnNames))]
      if (is_empty(missingAdditionalRequestedColumns)) missingAdditionalRequestedColumns <- NULL
    } else missingAdditionalRequestedColumns <- NULL
    
    
    return(list(
      required = missingRequiredColumns,
      requested = missingAdditionalRequestedColumns
    ))
  }
  
  #************This only uploads tables matching IeDEA DES names,
  # and only uploads tables once all requested tables have been selected
  # The data for table names(uploadedTables())[i] are stored in uploadedTables()[[i]]
  # uploadedTables table names will match the IeDEA DES format of mixed case table
  # names, regardless of case of file names. The uploadedTables column names within each table 
  # will be all caps to match IeDEA DES format (regardless of case of column names in original files)
  

  
  uploadedTablesInitial <- reactive({
    if (resetFileInput$reset) return(NULL)
    loaded <- infile()
    if (is.null(loaded) || is.null(loaded$files) || is.null(uploadList())) return(NULL)
    updateModal("Reading in files")
    allfiles <- loaded$files
    uploaded <- NULL
    missingCols <- NULL
    missingColsRequested <- NULL
    for (tableName in uploadList()$AllDESTables){
      result <- readOneTable(tableName)
      # if error encountered in reading file
      if (is.null(result)){
        return(NULL)
      }
      uploaded[[tableName]] <- result
      missingInTable <- checkTableForMissingColumns(table = uploaded[[tableName]], tableName = tableName)
      if (!is.null(missingInTable$required)){
        missingCols[[tableName]] <- paste(missingInTable$required, collapse = ", ")
      }
      if (!is.null(missingInTable$requested)){
        missingColsRequested[[tableName]] <- paste(missingInTable$requested, collapse = ", ")
      }
    }
    removeModal()
    
    if (nrow(uploaded$tblBAS) == 0){
      errorMessageModal(messageHeader = "No Records in tblBAS",
                        message = "Core table tblBAS is empty. This table is required and must have valid records.")
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    blankPATIENTS <- (trimws(uploaded$tblBAS$PATIENT) == "")
    if (any(blankPATIENTS, na.rm=TRUE)){
      errorMessageModal(messageHeader = "Missing PATIENT ID in tblBAS",
                        message = paste0(sum(blankPATIENTS)," blank PATIENT ID(s) in tblBAS: Row # ",
                                         paste(which(blankPATIENTS),collapse = ", ")))
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    duplicatedPATIENTS <- duplicated(uploaded$tblBAS$PATIENT)
    if (any(duplicatedPATIENTS, na.rm=TRUE)){
      dupID <- unique(uploaded$tblBAS$PATIENT[which(duplicatedPATIENTS)])
      errorMessageModal(messageHeader = "Duplicate PATIENT ID in tblBAS",
                        paste0(sum(duplicatedPATIENTS)," duplicate PATIENT records in tblBAS involving the following PATIENT IDs: ",
                               paste(dupID, collapse = ", ")))
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    if (!is.null(missingCols)){
      missing <- rownames_to_column(as.data.frame(missingCols))
      setnames(missing, c(1,2), c("Table", "Missing Required Field"))
      messageTable <- missing
      textForREDCap <- paste(paste0(names(missingCols),": ", missingCols), collapse = "; ")
      errorMessageModal(messageHeader = "Missing Required Fields",
                        message = "The tables below are missing required fields",
                        messageTable = messageTable, textForREDCap = textForREDCap)
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    
    # check to see if any read_xxx functions failed. If so, alert user and restart
    badFiles <- sapply(uploaded, function(x) is.null(x))
    if (any(badFiles, na.rm=TRUE)){
      #retrieve full file name to alert user
      badIndices <- match(tolower(names(which(badFiles))), tolower(file_path_sans_ext(allfiles$name)))
      badFileNames <- allfiles$name[badIndices]
      message <- paste0("An error was encountered when attempting to read the following IeDEA file(s): ", 
                        paste(badFileNames, collapse = ", "))
      errorMessageModal(message)
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    noRecordFiles <- sapply(uploaded, function(x) nrow(x)==0)
    if (any(noRecordFiles)){
      message <- paste0(paste(names(which(noRecordFiles)), collapse = ", "),
                        " contain(s) 0 records")
      errorMessageModal(message)
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    if (!is.null(missingColsRequested)){
      missingColsRequested <- rownames_to_column(as.data.frame(missingColsRequested))
      setnames(missingColsRequested, c(1,2), c("Table", "Missing Requested Fields"))
    } 
    
    #summarize info about uploaded tables and variables
    tablesAndVariablesList <- listsOfTablesAndVariables(uploaded, missingColsRequested)
    # store info about uploaded tables and variables in reactive variable:
    tablesAndVariables$list <- tablesAndVariablesList
    
    postProgress(list(action_step = "read_files", 
                      des_variables = as.character(jsonlite::toJSON(tablesAndVariablesList$des_variables)),
                      non_des_variables = as.character(jsonlite::toJSON(tablesAndVariablesList$non_des_variables)),
                      des_tables = paste(names(uploaded), collapse = ","),
                      non_des_files = paste(uploadList()$ExtraFiles, collapse = ","),
                      num_patients = uniqueN(uploaded$tblBAS$PATIENT)))
    return(uploaded)
  })
  
  
  
  # uploadedTables: list of all data frames of uploaded tables
  uploadedTables <- reactive({
    if (resetFileInput$reset) return(NULL)
    if (is.null(uploadedTablesInitial())) return(NULL)
    else {
      lastActivity(Sys.time())
      return(uploadedTablesInitial())
    }
  })
  
  # matchingColumns is a reactive variable: a list of all IeDEA DES variables present in dataset
  # (useful in some data quality functions)
  matchingColumns <- reactive({
    if (is.null(uploadedTables())) return(NULL)
    if (resetFileInput$reset) return(NULL)
    matchingColumns <- sapply(uploadList()$AllDESTables, function(tableName){
      return(intersect(names(uploadedTables()[[tableName]]), names(tableDef[[tableName]]$variables)))
    })
    return(matchingColumns)
  })

  # code for formatting uploaded tables before plotting and reporting---------------------------------------
  # also makes checking valid codes, etc easier
  formattedTables <- reactive({
    if (is.null(startDQ())) return(NULL)
    req(startDQ())
    loaded <- infile()
    if (is.null(loaded) || is.null(loaded$files)) return(NULL)
    if (is.null(uploadedTables())){
      return(NULL)
    }
    updateModal("Preparing tables for data quality checks")
    source("formattingTablesCode.R", local = TRUE)
    formattedTables <- forceModeTables()
    return(formattedTables)
  })
  
  # code for performing data quality checks and generating error summary frames------------------------
  # errorTable returns list of error detail and summaries
  errorTable <- reactive({
    if (resetFileInput$reset) return(NULL)
    if (is.null(uploadList())) return(NULL)
    if (is.null(formattedTables())) return(NULL)
    updateModal("checking for errors", 1)
    # initialize data frame to accumulate error details
    
    errorFrame <- initializeErrorTable()
    results <- dataQualityChecks(errorFrame)

    if (is.null(results)){
      return(NULL)
    }
    errorFrame <- results$errorFrame
    missingFrame <- results$missingFrame
    
    lastActivity(Sys.time())
    appearanceSummary <- findPatients()
    updateModal("Summarizing errors")
    summaries <- summarizeAllErrors(errorFrame, missingFrame)
   
    removeModal()
    lastActivity(Sys.time())

    # store results of data quality checks in REDCap
    postProgress(list(action_step = "dqcomplete",
                      error_summary = as.character(jsonlite::toJSON(summaries$summaryFrames$summaryFrame))))
    return(list(
      "totalErrors" = nrow(errorFrame),
      "errorDetail" = as.data.frame(errorFrame),
      "errorSummary" = as.data.frame(summaries$summaryFrames$summaryFrame),
      "highLevelErrorSummary" = as.data.frame(summaries$summaryFrames$highLevelSummary),
      "errorOnlySummary" = as.data.frame(summaries$summaryFrames$errorOnlySummary),
      "warnOnlySummary" = as.data.frame(summaries$summaryFrames$warnOnlySummary),
      "missingSummary" = as.data.frame(summaries$missingSummaryFrame),
      "missingSummaryByProgram" = as.data.frame(summaries$missingSummaryFrameByProgram), 
      "badCodeSummary" = as.data.frame(summaries$summaryFrames$badCodeFrame),
      "unknownCodeSummary" = as.data.frame(summaries$unknownSummaryFrame),
      "unknownCodeSummaryByProgram" = as.data.frame(summaries$unknownSummaryFrameByProgram),
      "missingAndUnknownByProgram" = as.data.frame(summaries$missingAndUnknownByProgram),
      "missingAndUnknown" = as.data.frame(summaries$missingAndUnknown),
      "errorsByTable" = summaries$errorsByTable,
      "appearanceSummary" = appearanceSummary
      ))
  })
  
  output$menu <- renderMenu({
    if (is.null(hubInfo$fromHub)) return(NULL)
    headingText <- "ACTIONS"
    conceptBadge <- NULL
    if (hubInfo$fromHub){
      conceptMessage <- hubInfo$userDetails$uploadconcept_mr
      conceptBadge <- tagList(span(conceptMessage, class="badge badge-active"))
      
    } 
    sidebarMenu(
      id = "tabs",
      menuItem(tagList(span("Introduction to Toolkit")), tabName = "welcome",
               selected = TRUE),
      menuItem(tagList(span(headingText), conceptBadge), tabName = "menu-header"),
      menuItem(tagList(span("STEP 1: ", class = "text-red", style = "font-weight: bold"),
                       span("Upload tables")), 
               tabName = "upload"), 
      menuItem(tagList(span("STEP 2: ", class = "text-orange", style = "font-weight: bold"),
                       span("Review data checks")),
               tabName = "reviewerror"),
      menuItem(tagList(span("STEP 3: ", class = "text-blue", style = "font-weight: bold"),
                       span("Create summary")), 
               tabName = "summaryReports"),
      menuItem(tagList(span("STEP 4: ", class = "text-green", style = "font-weight: bold"),
                       span("Submit data")), 
               tabName = "submit"),
      menuItem(tagList(span("TOOLS")), tabName = "menu-header"),
      
      
      menuItem("Visualize data", 
               tabName = "interactivePlots", icon = icon("bar-chart")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem("Provide feedback", tabName = "feedback", icon = icon("envelope"))
    )
  })
  
  output$warnOnQuit <- renderUI({
    if (reloadGuardOn()){
      return(
      tagList(
        tags$script("window.onbeforeunload = function() { return true; }")
      ))
    }
    else {
      return(
      tagList(
        tags$script("window.onbeforeunload = null;")

      ))
    }
  })

}

shinyApp(ui=shinyUI, server = shinyServer)