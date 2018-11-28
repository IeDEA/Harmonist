output$submitToAWS <- renderUI({
  if (!is.null(submitSuccess())) return(NULL)
  if ((!hubInfo$fromHub) || useSampleData()){
    return(
      fluidPage(
        tags$h3(class = "pleft title",tags$strong(span("STEP 4 ", class = "text-green")),
                " Submit data", backToHubMessage()),
       # tags$h5(class = "pleft subtitle","Option not available"),
        fluidRow(
          box(class = "title",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            title = span("Submit Data", style = "color: white"),
            tagList(
              span(
                tags$h5("This option is only available when submitting datasets for an active concept. "),
                tags$p("You may select an active data request on the ", a(" IeDEA Hub", href="http://iedeahub.org", target="_blank")," page.")
              ))))))
  }
  if (hubInfo$fromHub){
    if (resetFileInput$reset) return(tags$h3(notReadyMessage))
    if (is.null(startDQ())) return(tags$h3(notReadyMessage))
    if (is.null(errorTable()[[1]])){
      return(tags$h3(notReadyMessage))
    }
    else {
      totalErrors <- sum(errorTable()$errorOnlySummary$Count)
      typesOfErrors <- nrow(errorTable()$errorOnlySummary)
      if (totalErrors == 0) errorExplanation <- NULL
      # this is the information about your dataset that will be shared with IeDEA regions....
      else errorExplanation <- 
        tagList(
          tags$p(totalErrors, 
                  " potential data quality issues were detected in the dataset. See Step 2 for details."
                    ),
          textAreaInput("errorExplanation",
                                                "Enter an explanation of the errors that remain in your dataset to accompany your dataset.",
                                         width = "100%")
        )
     
      return(
        fluidPage(
          tagList(
            tags$h3(class = "row_text title",tags$strong(span("STEP 4 ", class = "text-green")),
                    " Submit data", backToHubMessage()),
            tags$h5(class = "row_text subtitle","Submit dataset for selected concept. "),
            fluidRow(
              box(
                width = 6,
                solidHeader = TRUE,
                status = "primary",
                title = span("Submit Data", style = "color: white"),
                tagList(
                  errorExplanation,
                  
                  tags$p(paste0("Click below to submit your data to secure cloud storage to be retrieved by ",
                               # paste(concept()$contact1$firstname, concept()$contact1$lastname), " and ",
                                paste(concept()$datacontact$firstname, concept()$datacontact$lastname))),
                  actionButton("submit", "Submit Data (Not currently enabled)")
                )
              )
            )
          )
            
        )
        
      )
    }
  }
})

zipInputDataset <- function(){
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
  return(zipfile)
}

storeDatasetAWS <- function(zipfile){
  regionID <- userDetails()$uploadregion_id
  conceptID <- userDetails()$uploadconcept_mr
  
  awsPath <- paste0(paste(conceptID, userDetails()$regionCode, userDetails()$uploaduser_id, 
                   format(Sys.time(),"%Y%m%d%H%M"), sep = "/"),"/")
  
  objectName <- paste0(paste(conceptID, userDetails()$regionCode, userDetails()$uploaduser_lastname, 
                             format(Sys.time(),"%Y%m%d%H%M"), sep = "_"), ".zip")
  awsObject <- paste0(awsPath, objectName)  
  # temporarily remove function to store file in AWS S3 Bucket
 # submitResult <- aws.s3::put_object(file = zipfile, object = awsObject, bucket = AWS_bucket_name)
  submitResult <- TRUE
  if (!submitResult) submitSuccess(FALSE)
  if (submitResult){
    submitSuccess(TRUE)

  }
  return(list(awsPath = awsPath,
              zipFileName = objectName))
}


dataUploadsREDCap <- function(uploadInfo){
  # create empty record]
  submitRecordNum <- getREDCapRecordID(tokenForHarmonist9) #, url = redcap_test_url)
  print("submitRecordNum successsful")
  uploadDetails <- list()
  uploadDetails[["record_id"]] <- submitRecordNum
  uploadDetails[["data_assoc_concept"]] <- userDetails()$uploadconcept_id #from Harmonist 18
  uploadDetails[["data_assoc_request"]] <- userDetails()$datacall_id #from Harmonist 18
  uploadDetails[["data_upload_person"]] <- userDetails()$uploaduser_id #from Harmonist 18
  uploadDetails[["data_upload_region"]] <- userDetails()$uploadregion_id #from Harmonist 18
  uploadDetails[["responsecomplete_ts"]] <- as.character(Sys.time())
  uploadDetails[["upload_notes"]] <- input$errorExplanation
  uploadDetails[["data_upload_n"]] <- length(formattedTables()$tblBAS$PATIENT)
  uploadDetails[["data_upload_bucket"]] <- AWS_bucket_name
  uploadDetails[["data_upload_folder"]] <- uploadInfo$awsPath
  uploadDetails[["data_upload_zip"]] <- uploadInfo$zipFileName
  
  data <- jsonlite::toJSON(data.frame(uploadDetails))
  result <- POST(redcap_url, encode = "form",
                 body = list(token = tokenForHarmonist9, content = "record",
                             format = "json", data = data))
  checkPostError(result)
 
  
cat("posting pdf","\n")
  postReport(recordNum = submitRecordNum, "PDF")
  cat("posting html","\n")
  postReport(recordNum = submitRecordNum, "html")
  cat("posted html")
  
}


observeEvent(input$submit,{
  lastActivity(Sys.time())
  
  zipfile <- zipInputDataset()
  
  reportModal(titleText = "Storing dataset in secure cloud storage",
              message = "Please wait until dataset upload is complete")
  
  uploadInfo <- storeDatasetAWS(zipfile)

  if (submitSuccess()){
  #  dataUploadsREDCap(uploadInfo)
    trackDetailsForREDCap$submit <- rbind(trackDetailsForREDCap$submit,
                                          data.frame(
                                            action_ts = as.character(Sys.time()),
                                            action_step = "submitdata",
                                            stringsAsFactors = FALSE
                                          ))
  }
  removeModal()

})

output$submitOutcome <- renderUI({
  if (is.null(submitSuccess())) return(NULL)
  if (!submitSuccess()) {
    submitResults <- 
      box(class = "alert alert-danger",
          width = 4, 
          title = span("Error encountered submitting dataset", style = "color: white"),
          tagList(
            tags$h3("Please contact judy.lewis@vumc.org")#,
          )
      )
  }
  if (submitSuccess()){
    submitResults <- 

             fluidRow(class = "alert alert-success",

               tagList(
                 tags$p(span(icon("check-square-o"),"Your dataset has been stored securely for retrieval by the requesting investigator and will be deleted after 30 days"))#,
               )
             )
  }
  fluidRow(
    submitResults
  )
})

output$afterSubmit <- renderUI({
  if (is.null(submitSuccess())) return(NULL)
  fluidRow(
    box(
      tagList(
        tags$p("Further Toolkit options (available in the sidebar or below):"),
        tags$li(actionLink("visualize", "Visualize dataset")),
        tags$li(actionLink("restart", "Upload a revised dataset")),
        tags$li(actionLink("feedback", "Provide feedback to the IeDEA Harmonist development team")),
        tags$li(actionLink("exitaftersubmit", "Exit the data toolkit"))
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


