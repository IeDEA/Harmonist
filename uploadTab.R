
collapseStatus <- reactiveValues(
  noActive = FALSE,
  active = FALSE,
  uploadSummary = FALSE
  
)

output$uploadIntro <- renderUI({
  tagList(
    tags$h3(class = "row_text title",tags$strong(span("STEP 1 ", class = "text-red")),
            " Upload Files", backToHubMessage()),
    
    tags$h5(class = "row_text subtitle", step1Subtitle) #in text.R
  )
})

observeEvent(uploadList(),{
  if (is.null(uploadList())) return(NULL)
  collapseStatus$noActive <- TRUE
  collapseStatus$active <- TRUE
  if (length(uploadList()$AllDESTables) > maxTablesWithoutCollapsing){ #in definitions.R
    collapseStatus$uploadSummary <- TRUE
  }
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

# Show user details about the current data request
dataRequestInfo <- reactive({
      
  if (!hubInfo$fromHub){
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
    # )
  }
  else {
    req(userDetails())
    req(concept())
    contact1 <- concept()$contact1
    contact1RegionNum <- as.numeric(contact1$person_region)
    contact1RegionCode <- regionData$region_code[[contact1RegionNum]]
    contact1Name <- paste(contact1$firstname, contact1$lastname, paste0("(", contact1RegionCode,")"))
    contact1Email <- contact1$email
    contact2 <- concept()$contact2
    contact2RegionNum <- as.numeric(contact2$person_region)
    contact2RegionCode <- regionData$region_code[[contact2RegionNum]]
    contact2Name <- paste(contact2$firstname, contact2$lastname, paste0("(", contact2RegionCode,")") )
    contact2Email <- contact2$email
    datacontact <- concept()$datacontact
    dataRegionNum <- as.numeric(datacontact$person_region)
    dataRegionCode <- regionData$region_code[[dataRegionNum]]
    dataName <- paste(datacontact$firstname, datacontact$lastname, paste0("(", dataRegionCode,")"))
    dataEmail <- datacontact$email
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
        tags$br(),
        # I need to make table names colored badges
        tags$p(
          div(strong("Requested Tables: "),class="col-md-2"),
          tags$div(listOfTableLabels, class="col-md-10"), class = "row toolkit"),
        tags$br(),
        tags$p(div(strong("Contacts"), class = "col-md-2"),
               div(
                 tags$ul(
                   tags$li(a(contact1Name, href=paste0("mailto:", contact1Email), target="_blank"), ",", contact1$institution),
                   tags$li(a(contact2Name, href=paste0("mailto:", contact2Email), target="_blank"), ",", contact1$institution)
                 ),
                 class = "col-md-10"
               ),
               class = "row toolkit"
        ),
        tags$br(),
        
        tags$p(div(strong("Data Downloaders"), class = "col-md-2"),
               div(
                 tags$ul(
                   tags$li(
                     a(dataName, href = paste0("mailto:", dataEmail), target="_blank"),
                     ",", datacontact$institution)), class = "col-md-10"),
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
    
    sopfinal <- downloadREDCapFile(file, tokenForHarmonist3, record_id_Harmonist3, "sop_finalpdf")
  }
)

output$uploadMissingSummary <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(uploadList())) return(NULL)
  if (is.null(uploadedTables())) return(NULL)
  if (is.null(uploadList()$MissingTables) &&
      (is.null(tablesAndVariables$list$missingConceptColumnsDF))) return(NULL)
  if (!is.null(uploadList()$MissingTables)){
    listOfTables <- lapply(uploadList()$MissingTables, function(x){return(tags$li(x))})
    missingTablesMessage <- tagList(
      tags$p("The following tables requested by", userDetails()$uploadconcept_mr, "were not found:"),
      tags$ul(listOfTables)
    )
  } else missingTablesMessage <- NULL

  if (!is.null(tablesAndVariables$list$missingConceptColumnsDF)){
    missingColsMessage <- 
      tagList(
        tags$p("The following fields requested by", userDetails()$uploadconcept_mr, "were not found:"),
        renderTable(tablesAndVariables$list$missingConceptColumnsDF)
      )
  } else missingColsMessage <- NULL
  fluidRow(  
  box(
    width = 10,
    solidHeader = TRUE,
    status = "warning",
    title = "Missing",
    missingTablesMessage,
    missingColsMessage
  
  )
  )
})


# uploadSummary UI --------------------------------------------------------
output$uploadSummary <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(uploadList())) return(NULL)
  if (is.null(uploadedTables())) return(NULL)

  tablesAndVariables$list$variableSummaryToDisplay
  uploadInfo <- renderTable(tablesAndVariables$list$variableSummaryToDisplay)
  if (length(uploadList()$ExtraFiles) == 0){
    extraFileMessage <- NULL
  } else {
    extraFileMessage <- tagList(
      tags$p(" File(s) in other formats or not conforming to IeDEA DES (Excluded from data quality checks):"),
      renderTable({
        uploadList()$ExtraFiles
      }, rownames = FALSE, colnames = FALSE))
    
  }
  
  if (any(uploadList()$tablesWithNoPatientID %in% pregnancyTables)){
    filesNotCheckedMessage <- tagList(
      tags$p("Tables that use a key identifier other than PATIENT are excluded from data quality checks at this time but will be included in the next version of Harmonist"),
      renderTable({
        uploadList()$tablesWithNoPatientID[uploadList()$tablesWithNoPatientID %in% pregnancyTables]
      }, rownames = FALSE, colnames = FALSE)
    )
  } else {
    filesNotCheckedMessage <- NULL
  }
  
  tagList(
    fluidRow(
      box(
        id = "uploadSummary",
        width = 10,
        title = actionLink("uploadSummaryID", span("Summary of uploaded IeDEA tables", style = "color: white")),
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = collapseStatus$uploadSummary,
        uploadInfo,
        extraFileMessage,
        filesNotCheckedMessage
      )
    )
  )
  
})

# UI to prompt user to select files to check and submit
output$selectFiles <- renderUI({
  if (is.null(hubInfo$fromHub)) return(NULL)

  uploadFileUI <- 
    tagList(
      
      
    fluidRow(
        box(width = 5, 
            status= "success",
            solidHeader = TRUE,
            title = "Select Data Files", 
            tagList(
              tags$p("Upload data in the ",
                           a("IeDEA Data Exchange Standard (IeDEA DES)", 
                     href="http://iedeades.org", target="_blank"),
                     " format. tblBAS is required."),
              tags$p("Allowed file formats include ", 
                     strong("CSV, SAS, Stata, SPSS, or a ZIP containing multiple files"), " of this type."),
              tags$p(tags$em("Select a single ZIP file or multiple files with Ctrl+Click"), align = "center"),
              fileInput("loaded",
                        "Data files", 
                        multiple = TRUE)
            )
        ),
        box(width = 5,
            title = "Use Sample Dataset",
            tags$p("Launch the Toolkit with a sample dataset (fake data) for practice, testing, and demonstrations."),
            actionButton("runWithSample", "Launch with Sample Data"))
      )
    )
  
  if  (resetFileInput$reset){
    return(uploadFileUI)
  }
  else if (!is.null(uploadedTables())){
    return(
      tagList(
      fluidRow(class = "rowUploadComplete",
               box(
                 width = 5, 
                 title = span("Upload complete", style = "color: white"),
                 solidHeader = TRUE,
                 status = "success",
                 tagList(
                   tags$h3(span(icon("check-square-o"),"Dataset successfully uploaded")),
                   actionButton("step2","Continue to Step 2", class="btn-success")
                 )
               ),
               box(
                 width = 5,
              
                 title = span("Restart session"),
                 tagList(
                   tags$h3(span("I would like to upload a different dataset")),
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


# for development purposes: display user info read in from URL
output$parameters <- renderText({
  userDetails <- userDetails()
  if (is.null(userDetails$user)) return(NULL)
  return(paste(paste0("userID = ",userDetails$user), 
               paste0("region = ", userDetails$regionID), 
               paste0("uploadconcept_mr = ",userDetails$uploadconcept_mr),
               paste0("token = ", userDetails$token), sep = ",  "))
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