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
library(httr)
library(jsonlite)
library(kableExtra)
library(Hmisc)
library(scales)
library(filesstrings)
library(purrr)
library(htmltools)
library(data.table)

# global variable to track size of uploaded files so that users can be warned when application busy
usage <- list()

source("server_name.R", local = TRUE)
source("redcapTokens.R", local = TRUE)
source("awsKey.R", local = TRUE)
source("helpers.R", local = TRUE)
source("definitions.R", local = TRUE)
source("initializeErrorFrames.R", local = TRUE)

#source("otherVisitStats.R", local = TRUE)

# html for appBusy announcement
busyAnnounce <- read_file("busyAnnounce.html")

###############################
# All column names in uploaded spreadsheets will be converted to all caps since variable names in 
# the IeDEA data specification are all caps.
###############################

# appBusy will be TRUE when the application not responsive due to calculations
appBusy <- FALSE

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
                   uiOutput("warnOnQuit"),
                   tags$div(
                     id = "versionInfo",
                     tags$p("Harmonist Data Toolkit Version 2.0"),
                     tags$p(tags$a("Contact us", href="mailto:harmonist@vumc.org", target="_blank"))
                   )
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
                uiOutput("tooBusyMessage"),
                uiOutput("uploadIntro"),
                uiOutput("dataRequestInfo"),
                uiOutput("uploadMissingSummary"),
                uiOutput("uploadSummary"),
                uiOutput("selectFiles")
              )
      ),
      tabItem(tabName = "reviewerror",
              fluidPage(
                uiOutput("errorSummarySep")
              )
      ),
      tabItem(tabName = "summaryReports",
              fluidPage(
                uiOutput("reportPage")
              )
      ),      
      tabItem(tabName = "submit",
              fluidPage(
                uiOutput("submitSummary"),
                uiOutput("submitOptions"),
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
              )
    ),
    tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.css"),
     tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
     
     # setup google analytics (only if file exists)
     if (file.exists("google-analytics.txt")) {
       includeHTML("google-analytics.txt")
     }
    )
  )
)

shinyServer <- function(input, output, session){

  # JUDY revisit this: is it necessary? pull from REDCap
  toReport <- rjson::fromJSON(file = "datasetSummary.json") 
  
  sessionID <- reactive(session$token)
  
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

  # reactive values
  sessionStartTime <- reactiveVal(isolate(Sys.time()))
  lastActivity <- reactiveVal(isolate(Sys.time()))
    
  # store region codes and names in a data frame-- either from REDCap or if REDCap not available,
  # the codes and names are provided (must be updated manually)
  regionData <- getAllRegionInfo()
  
  # reload guard is javascript function that prevents application from closing. Must be set to FALSE 
  # to allow application to close session
  reloadGuardOn <- reactiveVal(TRUE)
  sessionOver <- reactiveVal(FALSE)
  
  # useSampleData is set to TRUE if the user chooses to run the Toolkit with the sample dataset provided
  # In this case, the user should be prevented from uploading the dataset to the Hub in response to a data request
  useSampleData <- reactiveVal(FALSE)
  
  # errorCount is a reactive value that counts the number of errors detected so far. If it exceeds the value
  # specified in definitions.R then errorExcess is set to TRUE and errors are no longer accumulated
  # This is a temporary fix to address memory problems
  errorCount <- reactiveVal(0)
  errorRows <- reactiveVal(0)
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
  # recordDeleted reactive variable to indicate that record in Harmonist 18 security has been deleted
  # if the user has entered with an encrypted token from the Hub
  recordDeleted <- reactiveVal(FALSE)
  
  # testUser is a reactive variable that will prevent the creation of records in Harmonist 17
  # if TRUE
  # 
  testUser <- reactiveVal(FALSE)
  
  # resetFileInput - reactive resetFileInput$reset value to indicate if user is starting over with new files
  # initially, reset = FALSE. If user chooses uploading a new dataset, reset = TRUE 
  # When reset == TRUE, all variables and UI associated with uploaded dataset reset to NULL
  resetFileInput <- reactiveValues(
    reset = FALSE,
    newData = FALSE
  )
  
  # tablesAndVariables - reactive value to store lists of uploaded tables and variables
  tablesAndVariables <- reactiveValues(
    details = NULL,
    blankTables = NULL
  )

  
  # submitSuccess reactive Val to indicate if effort to submit dataset to AWS was successful or not
  submitSuccess <- reactiveVal(NULL)

 
  groupByChoice <- reactiveVal("PROGRAM")
  currentGroupSelection <- reactiveVal(NULL)  
  finalGroupChoice <- reactiveVal(NULL)

  
  #logic to terminate application execution if idle
  observe({
    # Re-execute this reactive expression after 10 minutes (600,000 milliseconds).
    # This expression could also be re-executed when the lastActivity value changes, so it needs to be checked. 
    if (is.null(infile())) return()
    cat("Session:", isolate(sessionID()),"is active", 
        as.character(now()), "\n", sep = " ", file = stderr())
    if (sessionOver()) {
      reloadGuardOn(FALSE)
      cat("Session:", isolate(sessionID()),"Application already over, another reload/close attempt", 
          as.character(now()), "\n", sep = " ", file = stderr())
      session$reload()
      session$close()
      return()
    }
    
    invalidateLater(intervalToCheckUserActivity, session)
    idle <- difftime(Sys.time(), isolate(lastActivity()), units = "mins") # number of minutes since last activity
    if (idle >= idleMinToWarn && idle < idleMinToExit ) {
      reloadGuardOn(FALSE)
      
      # x minutes of inactivity
      # Show modal dialog to warn user that session is timing out in 10 minutes
      idleWarningModal(idleTime = round(idle, digits = 0))
    }
    else if (idle >= idleMinToExit) {
      reloadGuardOn(FALSE)
      idleExitModal(idleTime = round(idle, digits = 0))
      cat("Session:", sessionID(),"Initiating exit actions because idle too long", 
          as.character(now()), "\n", sep = " ", file = stderr())
      exitActions()
      }
  })
  

  # getUserInfo ------------------------------------------------------
  # reactive variable to retrieve details about user if entered from the Hub, NULL otherwise
  getUserInfo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    encryptedToken <- query[["tokendt"]]
    if (is.null(encryptedToken)){
      return(NULL)
    }

    invalidToken <- FALSE
    decryptedToken <- getCrypt(encryptedToken,"d")
    if (is.null(decryptedToken)) invalidToken <- TRUE 
    else {
      userInfo <- getOneRecord(tokenForHarmonist18, decryptedToken,
                               projectName = "Harmonist 18")
      if (inherits(userInfo, "postFailure")) {
        errorMessageModal(messageHeader = "REDCap Error",
                          message = "Inability to access REDCap at this time",
                          secondaryMessage = "Session will continue without access to data submission")
        updateTabItems(session,"tabs", "welcome")
        return(NULL)
      }

      if (is_empty(userInfo)) invalidToken <-  TRUE
      else {
        expiration <- userInfo$tokenexpiration_ts
        # if no expiration date in record OR if expiration date has passed, invalid token
        if (is_blank(expiration) || (Sys.Date() - as.Date(expiration) > 1)) invalidToken <- TRUE
        else {
          userInfo$decryptedToken <- decryptedToken
          cat("Session:", isolate(sessionID())," valid token","\n", file = stderr())  
        }
      }
    }

    if (invalidToken){
      # ask steph here about modal
      errorMessageModal(messageHeader = "Invalid or expired Toolkit token",
                        message = "Please log in to the IeDEA Hub again and choose Upload Data.", 
                        secondaryMessage = "You may also continue this session without access to data submission option")
      updateTabItems(session,"tabs", "welcome")
      return(NULL)
    }
    return(userInfo)
  })
  

  
 #  At beginning of session this will be triggered and will in obtain user info and determine
 #  if the user entered from the hub or not
  observeEvent(session$token,{
    print(paste("session =", session$token, "usage = ", sum(unlist(usage))))
    postProgress(list(action_step = "start"))
    
    # if a user is testing the toolkit, no need to add progress to Harmonist 17
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["test"]]) && query[["test"]]=="T") testUser(TRUE)
    
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
  
  tooBusy <- reactive({
    if (is.null(infile())){
      invalidateLater(10000, session)
      totalUsage <- sum(unlist(usage))
      if (totalUsage > maxTotalUsage){ #maxTotalUsage defined in definitions.R
        return(TRUE)
      }
    }
    return(FALSE)
  })
  
  output$tooBusyMessage <- renderUI({
    print(sum(unlist(usage)))
    if (tooBusy()){
      cat("Session:", sessionID(),"User prevented from uploading files", 
          as.character(now()), "usage=", sum(unlist(usage)), "\n", sep = " ", file = stderr())
      return(
        box(#class = "alert alert-danger",
            width = 12, 
            solidHeader = TRUE,
            status = "danger",
            title = span("Harmonist Toolkit Busy"),# style = "color: white"),
            tagList(
              tags$p("Please return to the toolkit in 15 minutes. Other users have uploaded large datasets which will make your session slow. Please contact the harmonist team if you experience this error multiple times.")#,
            )
        )
      )
    } else return(NULL)
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
    # if DQ checks have not been performed, go ahead and restart:
    if (is.null(formattedTables())){
      lastActivity(Sys.time())
      cleanupFiles(infile())
      startDQ(NULL)
      resetFileInput$reset <- TRUE
      resetFileInput$newData <- FALSE
      errorCount(0)
      errorExcess(FALSE)
      useSampleData(FALSE)
      submitSuccess(NULL)
      tablesAndVariables <- NULL
      groupByChoice("PROGRAM")
      currentGroupSelection(NULL)
      finalGroupChoice(NULL)
      groupByInfo <- NULL
      resetHistoRange()
    } else {
      # data quality checks are already complete. Confirm that the user wants to start over
      restartAfterDQ()
    }
  })
  

  
  # if user decides to upload new dataset, unlink previously loaded dataset and change reset to TRUE
  observeEvent(input$yesRestart,{
    lastActivity(Sys.time())
    cleanupFiles(infile())
    startDQ(NULL)
    resetFileInput$reset <- TRUE
    resetFileInput$newData <- FALSE
    errorCount(0)
    errorRows(0)
    errorExcess(FALSE)
    useSampleData(FALSE)
    submitSuccess(NULL)
    tablesAndVariables <- NULL
    groupByChoice("PROGRAM")
    currentGroupSelection(NULL)
    finalGroupChoice(NULL)
    groupByInfo <- NULL
    resetHistoRange()
  })
  
  # when user selects files in fileInput UI, reset newData flag to TRUE and reset flag to FALSE
  observeEvent(input$loaded,{
    resetFileInput$newData <- TRUE
    resetFileInput$reset <- FALSE
    errorCount(0)
    errorRows(0)
    errorExcess(FALSE)
    # if no data has been uploaded before, and if the session start time
    # was more than 2 minutes ago, that means the session hasn't really started. 
    # The application has been idle so reset the session start time as now
    # so that the session_mins value in REDCap is accurate
    if (is.null(infile()) && difftime(Sys.time(), sessionStartTime(), units="mins") > 2){
      sessionStartTime(Sys.time())
    }
    # if it's the first time data uploaded and user is from Hub, delete token
    if (hubInfo$fromHub && !recordDeleted()){
      if (hubInfo$userDetails$decryptedToken != "tokenjudy"){
        result <- deleteOneRecord(tokenForHarmonist18, hubInfo$userDetails$decryptedToken)
        if (inherits(result, "postFailure")) {
          # record NOT deleted, so this means that someone could re-use the
          # token later to possibly upload another dataset (this is okay)
        } else {
          recordDeleted(TRUE)
        }
      }
    }
    cat("Session:", isolate(sessionID()),"User selected files to upload at", 
        as.character(now()), "\n", sep = " ", file = stderr())
  })


 # If user clicks button on Welcome tab labeled "Continue to Step 1" change active tab to Step 1: Upload files
  observeEvent(input$fromWelcomeToStep1,{
    updateTabItems(session,"tabs", "upload")
  })

  # If user clicks button on Upload tab labeled "Continue to Step 2" change active tab to Step 2: Check data
  # and initiate data quality checks with startDQ TRUE. 
  observeEvent(input$step2,{
    if (is.null(startDQ()) &&
        groupByChoice() == "PROGRAM" && 
        groupByInfo()$numPrograms == 1 &&
        !is.null(groupByInfo()$otherGroupOptions)){
      showModal(tags$div(id="dataQualityChecks",
                         modalDialog(
                           easyClose = FALSE, 
                           title = "Confirm Grouping Options", 
                           size = 'l',
                           "Your patients are currently grouped by PROGRAM and are all in one program.",
                           "To choose a different grouping option select the Change button below.",
                           footer = tagList(
                             actionButton("continueOneProg", "Continue"),
                             actionButton("changeSelection", "Change", class = "btn-success")
                           ),
                           fade = FALSE
                         )
      ))
    } else {
      startStep2()
    }

    # if some fatal error was found in data checks, errorTable() will be null
    if (is.null(errorTable())){
      updateTabItems(session, "tabs", "upload")
    } else updateTabItems(session,"tabs", "reviewerror")
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
  
  # if a new tab is selected (either by clicking on a tab or using the browser navigation arrows), 
  # check to see if query string was already updated. If not, update the URL with the 
  # selected tab
  observeEvent(input$tabs,{
    if (justUpdated()) {
      justUpdated(FALSE)
      return(NULL)
    }
    
    tab <- input$tabs
    
    # if user has chosen Step 2 tab and dataset has been uploaded but data quality checks haven't been 
    # initiated, start data quality checks
    if ((tab == "reviewerror") && (!is.null(uploadedTables())) && is.null(startDQ())){
      if (groupByChoice() == "PROGRAM" && 
          groupByInfo()$numPrograms == 1 &&
          !is.null(groupByInfo()$otherGroupOptions)){
        showModal(tags$div(id="dataQualityChecks",
                           modalDialog(
                             easyClose = FALSE, 
                             title = "Confirm Grouping Options", 
                             size = 'l',
                             "Your patients are currently grouped by PROGRAM and are all in one program.",
                           "To choose a different grouping option select the Change button below.",
                             footer = tagList(
                               actionButton("continueOneProg", "Continue"),
                               actionButton("changeSelection", "Change", class = "btn-success")
                             ),
                             fade = FALSE
                           )
        ))
      } else {
        startStep2()
      }
    }
    
    tokenString <- NULL
    token <- getQueryString()[["tokendt"]]
    if (!is_empty(token)){
      tokenString <- paste0("tokendt=", token, "&")
    }
    
    lastActivity(Sys.time())
    req(sessionID())
 
    
    # update URL to include tab= newly selected tab name
    updateQueryString(paste0("?",tokenString,"tab=",input$tabs), mode = "push")
    
    # track how often user clicks on Help or Review Errors tabs
    if (tab == "help"){
      postProgress(list(action_step = "help"))
    } else if ((tab == "reviewerror") && !is.null(uploadedTables())){
      postProgress(list(action_step = "reviewerror"))
    }
  })
  
  observeEvent(input$continueOneProg,{
    removeModal()
    startStep2()
  })
  
  startStep2 <- function(){
    tokenString <- NULL
    token <- getQueryString()[["tokendt"]]
    if (!is_empty(token)){
      tokenString <- paste0("tokendt=", token, "&")
    }
    startDQ(TRUE)
    finalGroupChoice(groupByChoice())
    formattedTables()
    errorTable()
    updateQueryString(paste0("?",tokenString,"tab=reviewerror"), mode = "push")
  }
  
  
  observeEvent(input$changeSelection,{
    tokenString <- NULL
    token <- getQueryString()[["tokendt"]]
    if (!is_empty(token)){
      tokenString <- paste0("tokendt=", token, "&")
    }
    updateQueryString(paste0("?",tokenString,"tab=reviewerror"), mode = "push")
    showGroupModal(saveAndGo = TRUE)
  })
  
  # when the URL changes, check to see if the new tab is different from the current tab
  # 
  observeEvent(getQueryString()[["tab"]],{
    req(input$tabs)
    newTabRequest <- getQueryString()[["tab"]]
    if (newTabRequest == "showMissing") {
      updateTabItems(session, "tabs", "upload")
      
    }
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
    cat("Session:", isolate(sessionID())," user is from the Hub","\n", file = stderr())  

    record_id_Harmonist3 <- hubInfo$userDetails$datacall_id
    conceptInfo <- getOneRecord(tokenForHarmonist3, record_id_Harmonist3, projectName = "Harmonist 3", formName = "data_specification")
    if (inherits(conceptInfo, "postFailure")) {
      errorMessageModal(messageHeader = "Failed to fetch request concept",
                        message = "Please try again later")
      updateTabItems(session, "tabs", "welcome")
      return(rjson::fromJSON(file = "concept0.json"))
    }
    conceptInfo <- as.list(conceptInfo)
    conceptInfo$tablefields <- rjson::fromJSON(conceptInfo$shiny_json[[1]])
    conceptInfo$contact1 <- getOneRecord(tokenForHarmonist5, conceptInfo$sop_creator[[1]], projectName = "Harmonist 5")
    conceptInfo$contact2 <- getOneRecord(tokenForHarmonist5, conceptInfo$sop_creator2[[1]], projectName = "Harmonist 5")
    conceptInfo$datacontact <- getOneRecord(tokenForHarmonist5, conceptInfo$sop_datacontact[[1]], projectName = "Harmonist 5")
    # data downloaders are stored as a comma-delimited list in REDCap sop_downloaders
    downloaders <- as.list(strsplit(conceptInfo$sop_downloaders[[1]], ",")[[1]])
    conceptInfo$downloaders <- lapply(downloaders, function(x){getOneRecord(tokenForHarmonist5, x, projectName = "Harmonist 5")})
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
    cat("Session:", sessionID(),"User files selected at",  
        as.character(now()), "size",
        paste(input$loaded$size,collapse = ","),"\n", sep = " ", file = stderr())
    postProgress(list(action_step = "uploaddata", 
                      toolkituser_id = userDetails()$uploaduser_id,
                      datarequest_id = userDetails()$datacall_id,
                      upload_filenames = paste(input$loaded$name,collapse = ","), 
                      upload_filesize = paste(as.character(input$loaded$size),
                                              collapse = ", ")))
    
    zipTypes <- c("application/zip", "application/x-zip-compressed")

        # check to see if the user uploaded a zip file
    if (any(input$loaded$type %in% zipTypes)) {
      # if a zip file was shared, make sure that only one file was uploaded
      if (nrow(input$loaded) > 1) {
        errorMessageModal(messageHeader = "File Selection Error",
                          message = tagList(
                            tags$p("You must either upload a single ZIP file or multiple data files."),
                            tags$p("Uploaded files detected:"),
                            makeBulletedList(input$loaded$name)
                          )
        )
        cleanupFiles(input$loaded)
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
        cleanupFiles(input$loaded)
        return(NULL)
      }
      result <- as.data.frame(t(sapply(filenames, function(filename) {
        list(name = basename(filename), size = file.size(filename), type = "", datapath = filename)
      })))
      #unlink(input$loaded$datapath) # check to see if this works
      # are there any nested zip files? are there multiple files with the same name? These are hard stops
      nestedZipFlag <- any(str_detect(result$name, ".zip"))

      tableNames <- tolower(file_path_sans_ext(result$name))
      tableNames <- tableNames[which(tableNames %in% tolower(names(tableDef)))]
      duplicateTableFlag <- any(duplicated(tableNames))
  
  
      if (nestedZipFlag || duplicateTableFlag){
        nestedZipMsg <- ifelse(nestedZipFlag, "nested ZIP files", "")
        duplicateTableMsg <- ifelse(duplicateTableFlag, " files with duplicate IeDEA table names", "")
        connector <- ifelse(nestedZipFlag && duplicateTableFlag, "and", "")
        errorMessageModal(
          messageHeader = "File Selection Error",
          message = tagList(
            tags$p("Your ZIP file included",
                           nestedZipMsg,
                           connector,
                           duplicateTableMsg),
            tags$p("Uploaded files detected:"),
            makeBulletedList(result$name)
          )
        )
        cleanupFiles(input$loaded)
        return(NULL)
      }
      
      
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
    index <- which(tolower(file_path_sans_ext(inputLink$name)) == tolower(tableName) &
                     tolower(file_ext(inputLink$name)) %in% validFileTypes)
    fileExt <- tolower(file_ext(inputLink$name[index]))
    updateModal(message = inputLink$name[index],
                title = "Reading file",
                subtitle = NULL)
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
    else { # we should never reach this point...
      errorMessageModal(messageHeader = "Invalid File Type",
                        message = tagList(
                          tags$p("Your uploaded files included: "),
                          tags$p("Valid file extensions for IeDEA tables:", 
                                 paste(validFileTypes, collapse = ", "), " and zip")
                        )
                        )
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
      listOfFiles <- makeBulletedList(allfiles$name)
      messageHeader <- "No Valid IeDEA Files Detected"
      
      message <- tagList(
        tags$p("Valid IeDEA Harmonist Data Toolkit file types:", paste(validFileTypesToDisplay, collapse = ", "), ". "),
        tags$p("Uploaded", makeItPluralOrNot("file", length(allfiles$name)), "detected: "), 
        listOfFiles)
      
      errorMessageModal(messageHeader = messageHeader, 
                        message = message)
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    

    allfiles$tableName <- tolower(tolower(file_path_sans_ext(allfiles$name)))
    invalidFiles <- allfiles[which( (!allfiles$tableName %in% tolower(names(tableDef))) |
                                      nonDESTypes),]
    emptyFiles <- allfiles[which(allfiles$size == 0),"name"]

    # confirm that all of the uploaded files are of an acceptable file type --in addition to valid file types. This is to prohibit
    # uploading of executable files, etc
    prohibitedFiles <- !(tolower(invalidFiles$extension) %in% c(allowedExtraFileTypes, validFileTypes))
    if (any(prohibitedFiles)){
      badFiles <-  invalidFiles[which(prohibitedFiles),]
      extraZipFilesIndices <- badFiles$extension == "zip"
      extraZipFiles <- badFiles[which(extraZipFilesIndices),]
      if (!all(extraZipFilesIndices)){
        badFiles <- badFiles[which(!extraZipFilesIndices),]
        messagePart1 <- case_when(length(badFiles$name) == 1 ~
                                    "The following non-IeDEA file is a prohibited file type: ",
                                  length(badFiles$name) > 1 ~
                                    "The following non-IeDEA files are prohibited file types: ")
        
        errorMessageModal(messageHeader = "Prohibited File Type Detected",
                          message = paste0(messagePart1, 
                                           paste(badFiles$name, collapse =  ", ")))
        resetFileInput$reset <- TRUE
        return(NULL)
      }
    }
   
    validFiles <- allfiles[which(!(allfiles$name %in% c(invalidFiles$name, emptyFiles))),]
    
    # check to make sure that tblbas is included in valid uploaded files
    if (!("tblbas" %in% validFiles$tableName)){
      listOfFiles <- makeBulletedList(allfiles$name)

      # first see if tblbas was actually included but wrong file type
      if ("tblbas" %in% invalidFiles$tableName){
        errorMessageModal(messageHeader = "tblBAS: Invalid File Format",
                          message = tagList(
                            tags$p(paste0("The core data table (tblBAS) is required for all IeDEA DES data uploads and must be saved in one of the accepted formats (", validFileTypesToDisplay, ").")),
                            tags$p("You uploaded the following files: "), 
                            listOfFiles  ))
        resetFileInput$reset <- TRUE
        return(NULL)
      } else if (any(startsWith(tolower(emptyFiles), "tblbas"))){ # check to make sure tblBAS has records and inform user if error
          errorMessageModal(messageHeader = "No Records in tblBAS",
                            message = "Core table tblBAS is empty. This table is required and must have valid records.")
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
    
    fileNames <- tolower(file_path_sans_ext(validFiles$name))
    tableNames <- fileNames[which(fileNames %in% tolower(names(tableDef)))]
    duplicateTableFlag <- any(duplicated(tableNames))
    if (duplicateTableFlag){
      dupTables <- paste0(unique(tableNames[which(duplicated(tableNames))]))
      dupFileNames <- sort(validFiles$name[fileNames %in% dupTables])
   
      errorMessageModal(
        messageHeader = "File Selection Error",
        message = tagList(
          tags$p("You uploaded IeDEA files with duplicate file names:",
          makeBulletedList(dupFileNames)
          )
        )
      )
      return(NULL)
    }
    uploaded <- file_path_sans_ext(validFiles$name)
    requested <- names(concept()$tablefields) 
    tablesUploadedMatching <- tolower(requested) %in% tolower(uploaded)
    # make vector of uploaded tables that match request, names in DES mixed case format from request
    matchingTables <- requested[tablesUploadedMatching]
    # make sure tblBAS listed first in matchingTables 
    matchingTables <- intersect(names(tableDef), matchingTables)
    missingTables <- requested[!tablesUploadedMatching]
    if (is_empty(missingTables)) missingTables <- NULL
    
    tablesUploadedRequested <- tolower(uploaded) %in% tolower(requested)  
    extraTables <- uploaded[!tablesUploadedRequested]
    extraUploadedDESTables <- tolower(names(tableDef)) %in% tolower(extraTables)
    extraDESTables <- names(tableDef)[extraUploadedDESTables]
    extraNonDESTables <- uploaded[!(tolower(uploaded) %in% tolower(names(tableDef)))]
    extraFiles <- invalidFiles$name
    # allDESTables lists all des tables in correct case and in correct order
    allDESTables <- intersect(names(tableDef), c(matchingTables, extraDESTables))

    tablesWithPatientID <- intersect(allDESTables, allTablesWithPatientID)
    tablesWithNoPatientID <- allDESTables[!allDESTables %in% allTablesWithPatientID]
    # JUDY CHANGE WHEN FIGURE OUT PREGNANCY-RELATED TABLES: For now, ignore all pregnancy-related tables
    # Remove the following line when that logic is added to Harmonist
    nonPregTables <- setdiff(tablesWithNoPatientID, pregnancyTables)
 
    # below, remove tblBAS from tablesWithPatientID since that is the required PATIENT table 
    # and we often want to merge it with all other PATIENT tables
    tablesWithPatientID <- tablesWithPatientID[!(tablesWithPatientID=="tblBAS")] #tables other than tblBAS with PATIENT as ID
    
    uploadedFiles <- list(matchingTables, extraDESTables, extraNonDESTables, extraFiles,
                          missingTables, allDESTables, tablesWithPatientID, tablesWithNoPatientID, emptyFiles)
    names(uploadedFiles) <- c("MatchingTables","ExtraDESTables", "ExtraTables", "ExtraFiles",
                              "MissingTables", "AllDESTables", "tablesWithPatientID", 
                              "tablesWithNoPatientID", "emptyFiles")
    return(uploadedFiles)
  })
  
  # checkTableForMissingColumns ---------------------------------------------
  checkTableForMissingColumns <- function(table, tableName){
    uploadedColumnNames <- names(table)
    requiredColumns <- findVariablesMatchingCondition(tableName, tableDef, "variable_required", "1")
    # temporarily: allow GENDER instead of SEX   JUDY
    requiredColumns <- requiredColumns[requiredColumns != "SEX"]
    missingRequiredColumns <- requiredColumns[which(!(requiredColumns %in% uploadedColumnNames))]

    # Check remaining ID columns in table to see if column present but emtpy
    nonMissingID <- setdiff(tableIDField[[tableName]], missingRequiredColumns)
    for (columnName in nonMissingID){
      if (all(is_blank_or_NA_elements(table[[columnName]]))){
        missingRequiredColumns <- c(missingRequiredColumns, columnName)
      }
    }

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
  #  updateModal(title = "Reading in files")
    allfiles <- loaded$files
    uploaded <- NULL
    blankTables <- NULL
    missingCols <- NULL
    missingColsRequested <- list()
    missingColsRequestedFormatted <- NULL
    missingVariableCount <- 0
    for (tableName in uploadList()$AllDESTables){
      result <- readOneTable(tableName)
      # if error encountered in reading file
      if (is.null(result)){
        return(NULL)
      }
   
      # keep track of table with headers but no records that aren't blank or NA
      if (is_table_blank(result)) blankTables <- c(blankTables, tableName)

      uploaded[[tableName]] <- result
      missingInTable <- checkTableForMissingColumns(table = uploaded[[tableName]], tableName = tableName)
      if (!is.null(missingInTable$required)){
        missingCols[[tableName]] <- paste(missingInTable$required, collapse = ", ")
      }
      if (!is.null(missingInTable$requested)){
        tableLabelClass <- paste0("label des-",tableDef[[tableName]]$table_category)
        missingVariableCount <- missingVariableCount + length(missingInTable$requested)
        missingColsRequestedFormatted[[tableName]] <- tags$li(span(tableName, class = tableLabelClass),
                                                     paste(missingInTable$requested, collapse = ", "))
        missingColsRequested[[tableName]] <- missingInTable$requested
      }
     
    }
    for (tableName in uploadList()$MissingTables){
      tableLabelClass <- paste0("label des-",tableDef[[tableName]]$table_category)
      requestedColumns <- get(tableName, concept()$tablefields)
      numberMissing <- length(requestedColumns)
      missingVariableCount <- missingVariableCount + numberMissing
      missingColsRequested[[tableName]] <- requestedColumns
      missingColsRequestedFormatted[[tableName]] <- tags$li(span(tableName, class = tableLabelClass),
                                                    paste0("Table missing (", numberMissing, " variables)"))
    }
    # put missingColsRequested in order of DES
    allTableNames <- names(tableOrder)
    theseTablesInOrder <- allTableNames[allTableNames %in% names(missingColsRequested)]
    if (!is_empty(missingColsRequested)) missingColsRequested <- missingColsRequested[theseTablesInOrder]
    removeModal()
    
    # check to make sure tblBAS has records and inform user if error
    if ("tblBAS" %in% blankTables){
      errorMessageModal(messageHeader = "No Records in tblBAS",
                        message = "Core table tblBAS is empty. This table is required and must have valid records.")
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    # check for missing PATIENT IDs and inform user if error
    blankPATIENTS <- is_blank_or_NA_elements(uploaded$tblBAS$PATIENT)
    if (any(blankPATIENTS, na.rm=TRUE)){
      errorMessageModal(messageHeader = "Missing PATIENT ID in tblBAS",
                        message = paste0(sum(blankPATIENTS)," blank PATIENT ",
                                         makeItPluralOrNot("ID", sum(blankPATIENTS)), 
                                         " in tblBAS: Row # ",
                                         paste(which(blankPATIENTS),collapse = ", ")))
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    # Check for duplicate PATIENT ids but no need to exclude blank patient IDs since those would already 
    # be caught by missing ID check above
    duplicatedPATIENTS <- duplicated(uploaded$tblBAS$PATIENT, na.rm = TRUE)
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
      missing$Table <- tableBadge(missing$Table)
      textForREDCap <- paste(paste0(names(missingCols),": ", missingCols), collapse = "; ")
      errorMessageModal(messageHeader = "Missing or Empty Required Columns",
                        message = "Required columns in the following tables are missing or completely blank:",
                        messageTable = missing, textForREDCap = textForREDCap)
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    # check to see if any read_xxx functions failed. If so, alert user and restart
    badFiles <- sapply(uploaded, function(x) is.null(x))
    if (any(badFiles, na.rm=TRUE)){
      #retrieve full file name to alert user
      badIndices <- match(tolower(names(which(badFiles))), tolower(file_path_sans_ext(allfiles$name)))
      badFileNames <- allfiles$name[badIndices]
      message <- paste0("An error was encountered when attempting to read the following IeDEA ",
                        makeItPluralOrNot("file", length(badFileNames)),
                        ": ",
                        paste(badFileNames, collapse = ", "))
      errorMessageModal(message)
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    # summarize info about uploaded tables and variables and store in reactive variable 
    # to be used in reports and upload summary tab
    tablesAndVariables$blankTables <- blankTables
    tablesAndVariables$tablesToCheck <- uploadList()$AllDESTables[!uploadList()$AllDESTables %in% blankTables]
    tablesAndVariables$tablesToCheckWithPatientID <- intersect(uploadList()$tablesWithPatientID, tablesAndVariables$tablesToCheck)
    desCols <- lapply(tablesAndVariables$tablesToCheck, function(tableName){
       return(intersect(names(tableDef[[tableName]]$variables), names(uploaded[[tableName]])))
     })
    names(desCols) <- tablesAndVariables$tablesToCheck
    tablesAndVariables$matchingColumns <- desCols
    tablesAndVariables$missingConceptColumnsFormatted <- missingColsRequestedFormatted
    tablesAndVariables$missingConceptColumns <- missingColsRequested
    tablesAndVariables$missingVariableCount <- missingVariableCount
    results <- createListsOfTablesAndVariables(uploaded, tableDef)
    tablesAndVariables$details <- results
    postProgress(list(action_step = "read_files", 
                      des_variables = as.character(jsonlite::toJSON(results$des_variables)),
                      non_des_variables = as.character(jsonlite::toJSON(results$non_des_variables)),
                      des_tables = paste(names(uploaded), collapse = ","),
                      non_des_files = paste(uploadList()$ExtraFiles, collapse = ","),
                      num_patients = uniqueN(uploaded$tblBAS$PATIENT),
                      programs = paste(as.character(unique(sanitizeNames(uploaded$tblBAS$PROGRAM))), collapse = ", ")))
    return(uploaded)
  })
  
  
  groupByInfo <- reactive({
    req(uploadedTablesInitial())
    if (resetFileInput$reset) return(NULL)
    if (is.null(uploadedTablesInitial())) return(NULL)
    req(tablesAndVariables)

    tblBAS <- uploadedTablesInitial()$tblBAS
    programs <- sort(unique(na.omit(tblBAS$PROGRAM)))
    programs <- programs[!is_blank_or_NA_elements(programs)]
    numPrograms <- length(programs)

    otherGroupOptions <- NULL
    results <- list(
      programs = programs,
      numPrograms = numPrograms,
      otherGroupOptions = otherGroupOptions
    )
    extraVars <- tablesAndVariables$details$non_des_variables$tblBAS
    if ("CENTER" %in% names(tblBAS)) {
      extraVars <- c(extraVars, "CENTER")
    }
    if (is_empty(extraVars)) return(results)
    # only allow character columns to be grouping columns (not dates, etc)
    extraVars <- extraVars[sapply(tblBAS[,extraVars], is.character)]
    if (is_empty(extraVars)) return(results)
    # get rid of column names with non-ASCII characters:
    extraVars <- extraVars[!grepl(pattern = "[^A-Za-z0-9_-]+", extraVars)]
    if (is_empty(extraVars)) return(results)
    # don't use dates or date approx codes as groups
    extraVars <- extraVars[!endsWith(extraVars, "_D") & !endsWith(extraVars, "_A") & !is_blank_or_NA_elements(extraVars)]
    if (is_empty(extraVars)) return(results)
    classes <- sapply(extraVars, function(x){class(tblBAS[[x]])})
    extraVars <- extraVars[classes %in% c("numeric", "character")]
    if (is_empty(extraVars)) return(results)
      missingness <- sapply(extraVars, function(x){sum(is_blank_or_NA_elements(tblBAS[[x]]))})/nrow(tblBAS)
      extraVars <- extraVars[which(missingness == 0)]
      # find other possible grouping variables, should be character and should be 
      # mostly non-missing
      if (is_empty(extraVars)) return(results)
      
      for (possibleGroupVar in extraVars){
        groupLevels <- unique(na.omit(tblBAS[[possibleGroupVar]]))
        groupLevels <- groupLevels[!is_blank_or_NA_elements(groupLevels)]
        numLevels <- length(groupLevels)
        if ((numLevels > 1) && (numLevels < maxNumberOfReportGroups)){  #in definitions.R
          otherGroupOptions[[possibleGroupVar]] <- list(levels = sort(groupLevels),
                                                        numLevels = numLevels)
        }
    }
    results$otherGroupOptions <- otherGroupOptions
    return(results)
  })
  
  # tableRowsByGroup returns a list of data frames, one for each table, 
  # with the 
  tableRowsByGroup <- reactive({
    if (resetFileInput$reset) return(NULL)
    if (is.null(finalGroupChoice())) return(NULL)
    if (is.null(formattedTables())) return(NULL)
    
    groupVar <- finalGroupChoice()
  # note that if groupVar is CENTER it will be factors...
    groupLevels <- sort(unique(formattedTables()$tblBAS[[groupVar]]))
    # create dataframe that includes all group levels in first column
    groups <- tibble(groupLevels)
    names(groups) <- groupVar
    # now for every table make a dataframe that indicates how many rows are 
    # in each group in that table
    # 
    tableRowsByGroup <- lapply(formattedTables(),
                               function(df){
                                 thisTableGroups <- groups
                                 if (is.factor(df[[groupVar]])){
                                   if ("Unknown" %in% levels(df[[groupVar]])){
                                     thisTableGroups[nrow(groups)+1, groupVar] <- NA
                                     thisTableGroups[[groupVar]] <- fct_explicit_na(thisTableGroups[[groupVar]], na_level = "Unknown")
                                   }
                                 } else {
                                   if ("Unknown" %in% df[[groupVar]]){
                                     thisTableGroups[nrow(groups)+1, groupVar] <- "Unknown"
                                   }
                                 }
                                 df %>% group_by(!! rlang::sym(groupVar)) %>% 
                                   summarise(numRows = n()) %>% ungroup() %>% 
                                   right_join(thisTableGroups) %>% replace_na(list(numRows=0))
                               })
    return(tableRowsByGroup)
      
  })
  
  
  # uploadedTables: list of all data frames of uploaded tables
  uploadedTables <- reactive({
    if (resetFileInput$reset) return(NULL)
    if (is.null(uploadedTablesInitial())) return(NULL)
    else {
      usage[[session$token]] <<- sum(unlist(infile()$files$size))
      groupByInfo()
      lastActivity(Sys.time())
      return(uploadedTablesInitial())
    }
  })
  
  
  # # matchingColumns is a reactive variable: a list of all IeDEA DES variables present in dataset
  # # that contain data
  # # (useful in some data quality functions)
  # matchingColumns <- reactive({
  #   if (is.null(uploadedTables())) return(NULL)
  #   if (resetFileInput$reset) return(NULL)
  #   matchingColumns <- sapply(uploadList()$AllDESTables, function(tableName){
  #     if (tableName %in% tablesAndVariables$blankTables) return(NULL)
  #     return(intersect(names(uploadedTables()[[tableName]]), names(tableDef[[tableName]]$variables)))
  #   })
  #   return(matchingColumns)
  # })

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
  #  updateModal("Preparing tables for data quality checks")
    source("formattingTablesCode.R", local = TRUE)
    # since time-consuming table formatting is about to begin,
    # set appBusy TRUE
    appBusy <<- TRUE
    formattedTables <- forceModeTables(finalGroupChoice(), uploadedTables())
    appBusy <<- FALSE
    return(formattedTables)
  })
  
  # code for performing data quality checks and generating error summary frames------------------------
  # errorTable returns list of error detail and summaries
  errorTable <- reactive({
    if (resetFileInput$reset) return(NULL)
    if (is.null(uploadList())) return(NULL)
    if (is.null(formattedTables())) return(NULL)

    # initialize data frame to accumulate error details
    errorFrame <- list()
    resources <- list(
      finalGroupChoice = finalGroupChoice(),
      formattedTables = formattedTables(),
      uploadedTables = uploadedTables(),
      uploadList = uploadList(),
      tablesAndVariables = tablesAndVariables,
      tableRowsByGroup = tableRowsByGroup()
    )
    # since time-consuming data quality checks are about to begin,
    # set appBusy TRUE
    appBusy <<- TRUE
    errorFrame <- dataQualityChecks(errorFrame, resources)
    appBusy <<- FALSE
    if (is.null(errorFrame)){
      return(NULL)
    }
    
    lastActivity(Sys.time())
    # for now, only use tables with PATIENT ID in appearance summary
    updateModal("Checking for the number of valid patients in each table")
    groupBy <- resources$finalGroupChoice
    appearanceSummary <- findPatients(resources$formattedTables,
                                      resources$tablesAndVariables$tablesToCheckWithPatientID,
                                      resources$tableRowsByGroup$tblBAS[[groupBy]],
                                      groupBy)

    cat("Session:", isolate(sessionID())," about to summarize all errors","\n", file = stderr())  
    updateModal("Checking for the number of valid patients in each table")
    
    summaries <- summarizeAllErrors(errorFrame, resources$finalGroupChoice, resources)
    cat("Session:", isolate(sessionID())," about to summarize critical errors","\n", file = stderr())  
    
    criticalErrors <- summarizeCriticalErrors(errorFrame, resources$finalGroupChoice)
    cat("Session:", isolate(sessionID())," finished summarizing critical errors","\n", file = stderr())  
    
    
    removeModal()
    lastActivity(Sys.time())

    postProgress(list(action_step = "dqcomplete",
                      toolkituser_id = userDetails()$uploaduser_id,
                      datarequest_id = userDetails()$datacall_id,
                      upload_filenames = paste(input$loaded$name,collapse = ","),
                      num_patients = uniqueN(resources$formattedTables$tblBAS$PATIENT),
                      error_summary = as.character(jsonlite::toJSON(summaries$summaryFrames$summaryFrameWithCodes))))
    return(list(
      "totalErrors" = nrow(errorFrame),
      "errorDetail" = as.data.frame(errorFrame),
      "errorSummary" = as.data.frame(summaries$summaryFrames$summaryFrame),
      "highLevelErrorSummary" = as.data.frame(summaries$summaryFrames$highLevelSummary),
      "errorOnlySummary" = as.data.frame(summaries$summaryFrames$errorOnlySummary),
      "warnOnlySummary" = as.data.frame(summaries$summaryFrames$warnOnlySummary),
      "missingSummary" = as.data.frame(summaries$missingSummaryFrame),
      "missingSummaryByGroup" = as.data.frame(summaries$missingSummaryFrameByGroup), 
      "badCodeSummary" = as.data.frame(summaries$summaryFrames$badCodeFrame),
      "unknownCodeSummary" = as.data.frame(summaries$unknownCodeSummary),
      "unknownCodeSummaryByGroup" = as.data.frame(summaries$unknownCodeSummaryByGroup),
      "missingAndUnknownByGroup" = as.data.frame(summaries$missingAndUnknownByGroup),
      "missingAndUnknown" = as.data.frame(summaries$missingAndUnknown),
      "errorsByTable" = summaries$errorsByTable,
      "appearanceSummary" = appearanceSummary,
      "criticalErrors" = criticalErrors
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
                       span("Upload files")), 
               tabName = "upload"), 
      menuItem(tagList(span("STEP 2: ", class = "text-orange", style = "font-weight: bold"),
                       span("Check data")),
               tabName = "reviewerror"),
      menuItem(tagList(span("STEP 3: ", class = "text-blue", style = "font-weight: bold"),
                       span("Create reports")), 
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

app <- shinyApp(ui=shinyUI, server = shinyServer)
# originalHandler <- app$httpHandler
# app$httpHandler <- function(req) {
#   cat("handling request from custom handler...\n")
#   if (appBusy) {
#     cat("app is busy!\n")
#     resp <- list(status = 200, content_type = "text/html; charset=UTF-8",
#                  content = busyAnnounce,
#                  headers = list())
#     class(resp) <- "httpResponse"
#     return(resp)
#   } else {
#     cat("app is not busy.\n")
#     originalHandler(req)
#   }
# }
app


