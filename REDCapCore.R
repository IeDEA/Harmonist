redcapPOST <- function(..., .timeoutValue = 10, .description = NULL) {
  if (is.null(.timeoutValue)) {
    result <- try(POST(...))
  } else {
    result <- try(POST(timeout(.timeoutValue), ...))
  }

  errorOccurred <- FALSE
  errorMessage <- NULL
  if (inherits(result, 'try-error')) {
    # usually means a timeout occurred
    errorOccurred <- TRUE
    if (grepl("timeout was reached", result, ignore.case = TRUE)) {
      errorMessage <- "Timeout reached"
    } else {
      errorMessage <- as.character(result)
    }
  } else if (result$status_code != 200) {
    # remote host (REDCap) indicated an error via status code
    errorOccurred <- TRUE
    errorMessage <- paste0("Status code (", result$status_code, ") indicated that a failure occurred")
    capture.output(print(httr::content(result)), file = stderr())
  }

  if (errorOccurred) {
    cat("REDCap POST failure (", errorMessage, ")", .description, "\n", file = stderr())
    result <- c("errorMessage" = errorMessage, "description" = .description)
    class(result) <- "postFailure"
  }

  return(result)
}

getREDCapRecordID <- function(projectToken, url = redcap_url, projectName = NULL, ...) {
  result <- redcapPOST(url = url, encode = "form",
                       body = list(token = projectToken,
                                   content = "generateNextRecordName"),
                       .description = paste("Attempting to get next record number from", projectName), ...)

  if (inherits(result, "postFailure")) {
    return(result)
  }

  nextRecordNum <- httr::content(result, as = "text")
  return(nextRecordNum)
}

getAllRecords <- function(projectToken, projectName, ...) {
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = projectToken,
                                   content = "record",
                                   format = "json", ...),
                       .description = paste("Attempting to get all records from", projectName))
  if (inherits(result, "postFailure")) {
    return(result)
  } else {
    return(rjson::fromJSON(httr::content(result, as = "text")))
  }
}

getOneRecord <- function(projectToken, record_id_value, projectName = NULL, formName = NULL){
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = projectToken, 
                                   content = "record",
                                   format = "json",
                                   records = record_id_value,
                                   forms = formName),
                       .description = paste("Attempting to get records from", projectName))
  if (inherits(result, "postFailure")) {
    return(result)
  } else {
    df <- jsonlite::fromJSON(httr::content(result, as = "text"))
    if (is_empty(df)){
      return("")
    } else if (nrow(df) == 1){
      return(df)
    } else {
      return("")
    }
  }
}

deleteOneRecord <- function(projectToken, record_id_value){
  result <- redcapPOST(url = redcap_url, encode = "form",
                       body = list(token = projectToken,
                                   content = "record",
                                   action = "delete",
                                   'records[0]' = record_id_value,
                       .description = paste("Attempting to delete record", record_id_value)))
  return(result)
}

downloadREDCapFile <- function(filename, projectToken, record_id, name_of_field) {
  result <- redcapPOST(url = redcap_url, write_disk(filename, overwrite = TRUE),
                       encode = "form",
                       body = list(token = projectToken,
                                   content = "file",
                                   action = "export",
                                   record = record_id,
                                   field = name_of_field),
                       .description = "Downloading file from REDCap")

  if (inherits(result, "postFailure")) {
    return(result)
  } else {
    return(filename)
  }
}

uploadREDCapFile <- function(filename, projectToken, record_id, name_of_field) {
  result <- redcapPOST(url = redcap_url, encode = "multipart",
                       body = list(token = projectToken, content = "file",
                                   action = "import", record = record_id,
                                   field = name_of_field,
                                   file = upload_file(filename)),
                       .description = "Attempting to upload file")

  return(result)
}

# Get a JSON file from REDCap from a Harmonist 11 database
getREDCapJSON <- function(type = c('0A', '0B', '0C'), projectToken, version = NULL) {
  type <- match.arg(type)

  filterLogic <- paste0('[type] = "', tolower(type), '"')
  if (!is.null(version)) {
    if (is.na(as.integer(version))) {
      stop('version must be a number')
    }
    filterLogic <- paste0(filterLogic, ' and [version] = "', version, '"')
  }

  # get record ids (i.e. '1 0a', '2 0a', '1 0b', '2 0b')
  result <- getAllRecords(projectToken, 'Harmonist11',
                          fields = 'record_id,type,version,jsoncopy_file',
                          filterLogic = filterLogic)
  if (inherits(result, 'postFailure')) {
    return(result)
  }

  if (length(result) == 0) {
    warning("couldn't find any JSON records for type = ", type, " and version = ", version)
    return(NULL)
  }

  # sort by version and pick the largest one
  idx <- order(as.integer(sapply(result, function(x) x$version)), decreasing = TRUE)
  record <- result[[idx[1]]]

  if (!nzchar(record$jsoncopy_file)) {
    warning("JSON file doesn't exist for record ", record$record_id)
    return(NULL)
  }

  newVersion <- FALSE
  #filename <- tempfile(paste0('Harmonist', type, '_'), fileext = '.json')
  filename <- file.path('projectFiles', paste0('Harmonist', type, '_', record$version, '.json'))
  if (!file.exists(filename)) {
    newVersion <- TRUE
    result <- downloadREDCapFile(filename, projectToken, record$record_id, 'jsoncopy_file')
    if (inherits(result, 'postFailure')) {
      return(result)
    }
  }
  result <- list(filename = filename, version = record$version, newVersion = newVersion)
  return(result)
}

# look for latest version of JSON file in the 'projectFiles' directory, or return the
# repository version if all else fails
getBackupJSONFile <- function(type = c('0A', '0B', '0C')) {
  type <- match.arg(type)

  result <- c()
  files <- list.files('projectFiles', paste0("Harmonist", type, "_\\d+.json"))
  if (length(files) > 0) {
    result <- file.path('projectFiles', files[order(as.integer(sub("^.+?(\\d+).+?$", "\\1", files)), decreasing=TRUE)[1]])
  }

  if (length(result) == 0) {
    result <- paste0('Harmonist', type, '.json')
  }
  return(result)
}

# Split table and variable names (i.e. tblBAS:PATIENT) and optionally verify
# expected table name
splitVarName <- function(inputVarName, expectedTableName = NULL) {
  parts <- strsplit(inputVarName, ":")[[1]]
  if (length(parts) != 2) {
    stop("invalid input variable name: ", inputVarName)
  }
  tableName <- parts[1]
  varName <- parts[2]
  if (!is.null(expectedTableName) && tableName != expectedTableName) {
    arg1 <- substitute(inputVarName)
    arg2 <- substitute(expectedTableName)
    stop("expected table name for ", arg1, " (", inputVarName, ") to be ",
         arg2, " (", expectedTableName, ") but got ", tableName)
  }
  list(tableName = tableName, varName = varName)
}
