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

postFailureModal <- function(result, message = "Error accessing REDCap") {
  errorMessageModal(message = message,
                    secondaryMessage = paste0(result$errorMessage, "; ", result$description))
}

postProgress <- function(progressList) {
  if (server_name == "Windows") return(NULL)
  if (server_name == "AWSLocal") return(NULL)
  if (isolate(testUser())) return(NULL)
  recordNum <- getREDCapRecordID(tokenForHarmonist17, projectName = "Harmonist 17")
  # if error accessing redcap, don't continue
  if (inherits(recordNum, "postFailure")) return(NULL)
  progressList$server_name <- server_name
  progressList$session_id <- isolate(sessionID()) #isolate in case postProgress called from onSessionEnded
  #JUDY what about vector of entries
  progressList$record_id <- recordNum
  progressList$action_ts <- Sys.time()
  if (!is.null(isolate(userDetails()))){
    progressList$userregion_id <- isolate(userDetails()$uploadregion_id)
    if (isolate(hubInfo$fromHub)){
      progressList$datarequest_id <- isolate(userDetails()$uploadconcept_mr)
      progressList$toolkituser_id <- isolate(userDetails()$uploaduser_id)
    }
  }
  #remove null elements from list so that conversion to data frame will be successful
  nonNullElements <- names(which(sapply(progressList, function(x){!is.null(x)})))
  df <- as.data.frame(progressList[nonNullElements], stringsAsFactors = FALSE)

  data <- jsonlite::toJSON(df)
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = tokenForHarmonist17, content = "record",
                                   format = "json", data = data),
                       .description = "Attempting to post progress to Harmonist 17")
  if (inherits(result, "postFailure")) {
    # not critical; ignore
  }
}

postProgressMultiple <- function(df){
  if (nrow(df) == 0) return(NULL)
  if (server_name == "Windows") return(NULL)
  if (server_name == "AWSLocal") return(NULL)
  if (isolate(testUser())) return(NULL)
  recordNum <- getREDCapRecordID(tokenForHarmonist17, projectName = "Harmonist 17")
  if (inherits(recordNum, "postFailure")) return(NULL)
  df$record_id <- seq(from = recordNum, by = 1, length.out = nrow(df))
  df$server_name <- server_name
  df$session_id <- isolate(sessionID())

  if (!is.null(isolate(userDetails()))){
    df$userregion_id <- isolate(userDetails()$uploadregion_id)
    if (isolate(hubInfo$fromHub)){
      df$datarequest_id <- isolate(userDetails()$uploadconcept_mr)
      df$toolkituser_id <- isolate(userDetails()$uploaduser_id)
    }
  }

  data <- jsonlite::toJSON(df)
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = tokenForHarmonist17, content = "record",
                                   format = "json", data = data),
                       .description = "Attempting to write multiple records to Harmonist 17")
  if (inherits(result, "postFailure")) {
    # not critical; ignore
  }
}

postParticipationStatus <- function(status){
  if (is_empty(status)) return(NULL)
  if (status == "0") return(NULL)
 # if (server_name == "Windows") return(NULL)
 # if (server_name == "AWSLocal") return(NULL)
  if (isolate(testUser())) return(NULL)

  record_id_Harmonist3 <- hubInfo$userDetails$datacall_id
  regionNum <- isolate(userDetails()$uploadregion_id)
  #record details
  recordDetails <- list(
    record_id = isolate(hubInfo$userDetails$datacall_id),
    redcap_repeat_instrument = "region_participation_status",
    redcap_repeat_instance = regionNum,

    data_region = regionNum,
    data_response_status = status,
    region_update_ts = as.character(Sys.time())
  )

  if (status == "2"){ # 2 is the code for Complete Data in Harmonist 3
    recordDetails$region_complete_ts = as.character(Sys.time())
  }

  df <- as.data.frame(recordDetails, stringsAsFactors = FALSE)
  data <- jsonlite::toJSON(df)

  result <- redcapPOST(
    url = redcap_url,
    encode = "form",
    body = list(
      token = tokenForHarmonist3,
      content = "record",
      format = "json",
      data = data
    ),
    .description = "Attempting to write finalize status to Harmonist 3"
  )
  if (inherits(result, "postFailure")) {
    # If this fails, the user can still change the status in the Hub.
    # Therefore, this error can be ignored. Failures are always printed out to
    # stderr, so there will be a record of the failure.
  }
}

# postFeedback---------------------------------------------------------
postFeedback <- function(message, userEmail = ""){
  if (server_name == "Windows") return(NULL)

  progressList <- list(feedback_message = message,
                       user_email = userEmail)
  result <- getREDCapRecordID(tokenForHarmonist17A, projectName = "Harmonist 17A")
  if (inherits(result, "postFailure")) {
    postFailureModal(result)
    return()
  }

  recordNum <- result
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
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = tokenForHarmonist17A,
                                   content = "record",
                                   format = "json",
                                   data = data),
                       .description = "Recording feedback")
  if (inherits(result, "postFailure")) {
    postFailureModal(result)
  }
}

getAllRegionInfo <- function(){
  # get region names and codes. If not successful, no problem; read in list of region names (keep updated)
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = tokenForHarmonist4,
                                   content = "record",
                                   format = "json"),
                       .description = "Attempting to get regioninfo")

  if (inherits(result, "postFailure")) {
    # POST failed, so use hard-coded values
    return(list(
      record_id = as.character(1:10),
      region_name = c("Harmonist Test", "West Africa", "Central Africa", "NA-ACCORD",
                      "CCASAnet", "Southern Africa", "East Africa", "Asia Pacific", "NIH", "External Users"),
      region_code = c("TT", "WA", "CA", "NA", "CN", "SA", "EA", "AP", "NH", "EX")
    ))
  } else {
    # POST succeeded
    regionData <- jsonlite::fromJSON(httr::content(result, as = "text"))
    return(regionData)
  }
}

getOneRegionInfo <- function(record_id_value){
  # get region names and codes. If not successful, no problem; read in list of region names (keep updated)
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = tokenForHarmonist4, 
                                   content = "record",
                                   format = "json",
                                   records = record_id_value
                                   ),
                       # body = list(token = tokenForHarmonist4,
                       #             content = "record",
                       #             format = "json"),
                       .description = "Attempting to get regioninfo")
  
  if (inherits(result, "postFailure")) {
    cat("REDCap POST failure accessing regions", "\n", file = stderr())
    # POST failed, so use hard-coded values
    region_names <-  c("Harmonist Test", "West Africa", "Central Africa", "NA-ACCORD",
                    "CCASAnet", "Southern Africa", "East Africa", "Asia Pacific", "NIH", "External Users")
    region_codes <- c("TT", "WA", "CA", "NA", "CN", "SA", "EA", "AP", "NH", "EX")
    return(list(
      region_name = region_names[[record_id_value]],
      region_code = region_codes[[record_id_value]]
    ))
  } else {
    # POST succeeded
    regionData <- jsonlite::fromJSON(httr::content(result, as = "text"))
    return(regionData)
  }
}
