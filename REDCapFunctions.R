trackDetailsForREDCap <- reactiveValues()

trackDetailsForREDCap$plots <- data.frame(
  action_ts = character(),
  action_step = character(),
  plot_table = character(),
  plot_var1 = character(),
  plot_var2 = character(),
  stringsAsFactors = FALSE
)

trackDetailsForREDCap$reports <- data.frame(
  action_ts = character(),
  action_step = character(),
  report_type = character(),
  report_format = character(),
  stringsAsFactors = FALSE
)

trackDetailsForREDCap$tabClicks <- data.frame(
  action_ts = character(),
  action_step = character(),
  errortab_name = character(),
  stringsAsFactors = FALSE
)

trackDetailsForREDCap$submit <- data.frame(
  action_ts = character(),
  action_step = character()
)


checkPostError <- function(result, message = NULL){
  if (is.null(result)) return(TRUE)
  if (result$status_code == 200) return(FALSE)
  if (result$status_code !=200){
    cat("REDCap error Code = ", result$status_code, message, "\n", file = stderr())
    # errorMessageModal("Error accessing REDCap", 
    #                   secondaryMessage = paste("Code = ", result$status_code, message)
    #)
  
    return(TRUE)
  }
}

redcapPOST <- function(..., timeout_in = 5) {
  if (isolate(redcapUp())) {
    result <- try(POST(timeout(timeout_in), ...))
    if (inherits(result, "try-error")) {
      redcapUp(FALSE)
      return(NULL)
    }
    return(result)
  }
  return(NULL)
}


getREDCapRecordID <-  function(projectToken, url = redcap_url, projectName = NULL){
  if (!isolate(redcapUp())) return(NULL)
  result <- redcapPOST(url = url, encode = "form",
                 body = list(token = projectToken, 
                             content = "generateNextRecordName"))
  postError <- checkPostError(result, message = paste("Attempting to get next record number from", projectName))
  # if error in accessing redcap (either failed POST or redcap error) postError will be TRUE
  if (postError) return(NULL)
  nextRecordNum <- rawToChar(result$content)
  return(nextRecordNum)
}

postProgress <- function(progressList){ 
  if (server_name == "Windows") return(NULL)
  if (server_name == "AWSLocal") return(NULL)
  if (!isolate(redcapUp())) return(NULL)
  recordNum <- getREDCapRecordID(tokenForHarmonist17, projectName = "Harmonist 17")
  # if error accessing redcap, recordNum will be NULL; don't continue
  if (is.null(recordNum)) return(NULL)
  progressList$server_name <- server_name
  progressList$session_id <- isolate(sessionID()) #isolate in case postProgress called from onSessionEnded
  #JUDY what about vector of entries
  progressList$record_id <- recordNum
  progressList$action_ts <- Sys.time()
  if (!is.null(isolate(userDetails()))){
    progressList$userregion_id <- isolate(userDetails()$uploadregion_id)
    if (isolate(hubInfo$fromHub)){
      progressList$datarequest_id <- isolate(userDetails()$uploadconcept_mr)
    }
    
  }
  #remove null elements from list so that conversion to data frame will be successful
  nonNullElements <- names(which(sapply(progressList, function(x){!is.null(x)})))
  df <- as.data.frame(progressList[nonNullElements], stringsAsFactors = FALSE)
  
  data <- jsonlite::toJSON(df)
  result <- redcapPOST(url = redcap_url, encode = "form",
                 body = list(token = tokenForHarmonist17, content = "record",
                             format = "json", data = data))
  checkPostError(result, message = "Attempting to post progress to Harmonist 17")
  
}

postProgressMultiple <- function(df){
  if (nrow(df) == 0) return(NULL)
  if (!isolate(redcapUp())) return(NULL)
  if (server_name == "Windows") return(NULL)
  if (server_name == "AWSLocal") return(NULL)
  recordNum <- getREDCapRecordID(tokenForHarmonist17, projectName = "Harmonist 17")
  df$record_id <- seq(from = recordNum, by = 1, length.out = nrow(df))
  df$server_name <- server_name
  df$session_id <- isolate(sessionID()) #isolate in case postProgress called from onSessionEnded
  if (!is.null(isolate(userDetails()))){
    df$userregion_id <- isolate(userDetails()$uploadregion_id)
  }
  
  data <- jsonlite::toJSON(df)
  result <- POST(redcap_url, encode = "form",
                 body = list(token = tokenForHarmonist17, content = "record",
                             format = "json", data = data))
  checkPostError(result, message = "Attempting to write multiple records to Harmonist 17")
}

# getAllRecordsTest-------------------------------------------------------
# getAllRecordsTest <- function(projectToken){  ## modify when in production
#   result <- POST(redcap_test_url, encode = "form",
#                  body = list(token = projectToken, 
#                              content = "record",
#                              format = "json"))
#   
#   checkPostError(result)
#   df <-  jsonlite::fromJSON(rawToChar(result$content))
#   return(df)
# }

# getAllRecords
getAllRecords <- function(projectToken, projectName = NULL){  ## modify when in production
  result <- POST(redcap_url, encode = "form",
                 body = list(token = projectToken, 
                             content = "record",
                             format = "json"))
  
  checkPostError(result, message = paste("Attempting to get records from", projectName))
  df <-  jsonlite::fromJSON(rawToChar(result$content))
  return(df)
}

getOneRecord <- function(projectToken, record_id_value, projectName = NULL){
  if (!isolate(redcapUp())) return(NULL)
  result <- redcapPOST(url = redcap_url, encode = "form",
                 body = list(token = projectToken, 
                             content = "record",
                             format = "json",
                             records = record_id_value))
  postError <- checkPostError(result, message = paste("Attempting to get records from", projectName))
  if (postError) return(NULL)
  df <-  jsonlite::fromJSON(rawToChar(result$content))
  return(df)
}

# getOneRecordTest <- function(projectToken, record_id_value){
#   result <- POST(redcap_test_url, encode = "form",
#                  body = list(token = projectToken, 
#                              content = "record",
#                              format = "json",
#                              records = record_id_value))
#   checkPostError(result)
#   df <-  jsonlite::fromJSON(rawToChar(result$content))
#   return(df)
# }

deleteOneRecord <- function(projectToken, record_id_value){
 # browser()
  result <- POST(redcap_url, encode = "form",
                 body = list(token = projectToken, 
                             content = "record",
                             action = "delete",
                             records = record_id_value))
  checkPostError(result)
}



# postFeedback---------------------------------------------------------
postFeedback <- function(message, userEmail = ""){
  if (server_name == "Windows") return(NULL)
  
  progressList <- list(feedback_message = message,
                       user_email = userEmail)
  recordNum <- getREDCapRecordID(tokenForHarmonist17A, projectName = "Harmonist 17A")
  progressList$server_name <- server_name
  progressList$session_id <- isolate(sessionID()) #isolate in case postProgress called from onSessionEnded
  #JUDY what about vector of entries
  progressList$record_id <- recordNum
  progressList$feedback_ts <- Sys.time()
  if (!is.null(isolate(userDetails()))){
    progressList$userregion_id <- isolate(userDetails()$uploadregion_id)
  }
  
  df <- as.data.frame(progressList, stringsAsFactors = FALSE)
  
  data <- jsonlite::toJSON(df)
  result <- POST(redcap_url, encode = "form",
                 body = list(token = tokenForHarmonist17A, content = "record",
                             format = "json", data = data))
  checkPostError(result)
}

postReport <- function(recordNum, type = c("PDF", "html")){
  if (server_name == "Windows") return(NULL)
  
  dir <- tempfile("dir")
  dir.create(dir)
  filename <- file.path(dir, paste0("harmonist-report.", tolower(type)))
  
  createReport(file = filename, reportType = type,
               includeHistograms = TRUE,
               includeDataSummary = TRUE,
               includeErrorSummary = TRUE,
               datasetDesc = NULL)
  
  
  result <- POST(redcap_url, encode = "multipart",
                 body = list(token = tokenForHarmonist9, content = "file",
                             action = "import", record = recordNum,
                             field = paste0("data_upload_", tolower(type)), file = upload_file(filename)))
  
  unlink(dir, recursive = TRUE)
  checkPostError(result)
}



#

downloadREDCapFile <- function(filename, projectToken, record_id, name_of_field) {
  result <- POST(redcap_url, write_disk(filename, overwrite = TRUE),
                 encode = "form",
                 body = list(token = projectToken,
                             content = "file",
                             action = "export",
                             record = record_id,
                             field = name_of_field))
  
  checkPostError(result)
  return(filename)
}

getAllRegionInfo <- function(){
  result <- NULL
  if (isolate(redcapUp())){
    result <- redcapPOST(url = redcap_url, encode = "form",
                         body = list(token = tokenForHarmonist4,
                                     content = "record",
                                     format = "json"))
  }
  if (!is.null(result)){
    regionData <- jsonlite::fromJSON(rawToChar(result$content))
    return(regionData)
  } else {
    return(list(
      record_id = as.character(1:9),
      region_name = c("Harmonist Test", "West Africa", "Central Africa", "NA-ACCORD",
                      "CCASAnet", "Southern Africa", "East Africa", "Asia Pacific", "NIH"),
      region_code = c("TT", "WA", "CA", "NA", "CN", "SA", "EA", "AP", "NH")
    ))
  }
}

