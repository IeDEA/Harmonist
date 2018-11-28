numberOfCharactersInDate <- 10 #yyyy-mm-dd
dateErrorType <- list(
  "format" = "Improper date format",
  "value" = "Invalid date value",
  "future" = "Future date",
  "earlyDate" = paste0("Date before ", year(minimumExpectedDate)),
  "earlyBIRTH_D" = paste0("BIRTH_D before ", year(minimumExpectedBirthDate)),
  "_D_A" = "Date provided but _A coded as Unknown"
)
dateErrorDesc <- list(
  "format" = "Improper date format: must be YYYY-MM-DD",
  "value" = "Invalid date value: month must be 1-12, day must be 1-# of days in the month",
  "future" = "This date is in the future",
  "earlyDate" = paste0("Date before ", year(minimumExpectedDate)),
  "earlyBIRTH_D" = paste0("BIRTH_D before ", year(minimumExpectedBirthDate)),
  "_D_A" = paste0("If a date approximation is coded as Unknown, the date entry should be blank or ",dateIndicatingUnknown)
)
dateErrorCode <- list(
  "format" = "Improper date format",
  "value" = "Invalid date value",
  "future" = "Future date",
  "earlyDate" = "Date Logic",
  "earlyBIRTH_D" = "Date Logic",
  "_D_A" = "Date _A Logic"
)




addToErrorFrameDateFormatErrors <- function(errorFrame, table, field, tableName, errorNumber, severity, errorVariable2 = ""){
  errorFrame <- addToErrorFrame(errorFrame, table, field, tableName, errorType = dateErrorType[[errorNumber]],
                              errorCode = dateErrorCode[[errorNumber]], 
                              severity, message = dateErrorDesc[[errorNumber]], errorVariable2)
  return(errorFrame)
}
  
  
#   
#   numErrors <- nrow(table)
#   newRow <- NULL
#   newRow$table <- rep(tableName, numErrors)
#   
#   idList <- tableIDField[[tableName]]
# 
#   for (index in seq_along(idList)){
#     idVariable <- idList[[index]]
#     idField <- idFieldNames[[index]] #JUDY replace with paste0 but then look for idfield and idvalue change to 1
#     idValue <- as.character(idValueNames[[index]])
#     newRow[[idField]] <- idVariable
#     newRow[[idValue]] <- as.character(get(idVariable, table))
#   }
#   
#   newRow$errorVariable <- field
#   newRow$errorValue <- as.character(get(field, table))
#   newRow$errorType <- dateErrorType[[errorNumber]]
#   newRow$errorDesc <- dateErrorDesc[[errorNumber]]
#   newRow$severity <- severity
#   newRow$errorCode <- dateErrorCode[[errorNumber]]
#   newRow$errorVariable2 <- errorVariable2
#   browser()
#   if (exists("PROGRAM", table)) newErrors$PROGRAM <- table$PROGRAM
#   else if (idList[[1]] == "PATIENT"){
#     newErrors$PROGRAM <- uploadedTables()$tblBAS[match(newErrors$idField,
#                                                        uploadedTables()$tblBAS$PATIENT ),"PROGRAM"]
#   } else newErrors$PROGRAM <- "Unknown"
#  
#   index <- paste0(tableName, field, errorNumber)
#   errorFrame[[index]] <- data.frame(newRow, stringsAsFactors = FALSE)
#   
#   return(errorFrame)
# }



findUnknownDatesCodedImproperly <- function(errorFrame, formattedTable, dateFieldName, tableName){
  if (paste0(dateFieldName,"_A") %in% names(formattedTable)){
    dateField_A <- paste0(dateFieldName, "_A")
    print(dateField_A)
    datesImproperlyCoded <- formattedTable %>%  filter(.[dateField_A] == "Unknown") %>% 
      filter(!is.na(.[dateFieldName])) %>% 
      filter(.[dateFieldName] != dateIndicatingUnknown)
    if (nrow(datesImproperlyCoded) > 0){
      errorFrame <- addToErrorFrameDateFormatErrors(errorFrame,datesImproperlyCoded, 
                                                    dateFieldName, tableName, "_D_A", "Warn", dateField_A)
    }
  }
  return(errorFrame)
}

findBadDateFormats <- function(errorFrame, table, formattedTable, dateFieldName, tableName){
  # any NA values in a date field in formattedTable is either (1) missing or (2) bad date format
  potentialBadDates <- is.na(formattedTable[[dateFieldName]])
  if (!any(potentialBadDates)) return(errorFrame)
  
  else {
    dates <- table[[dateFieldName]]
    if (is.Date(dates)){
      missingDates <- is.na(dates)
    } else missingDates <- is.na(dates) | safeTrimWS(dates) == ""
    badDates <- potentialBadDates & !missingDates
    if (any(badDates)){
      errorFrame <- addToErrorFrameDateFormatErrors(errorFrame,table[which(badDates),], 
                                                    dateFieldName, tableName, "format", "Error")
    }
  }
  return(errorFrame)
}

findFutureDates <- function(errorFrame, table, formattedTable, dateFieldName, tableName){
  if (dateFieldName %in% datesInFuture) return(errorFrame)
  else {
    futureDates <- formattedTable[[dateFieldName]] > today()
    if (any(futureDates, na.rm = TRUE)){
      errorFrame <- addToErrorFrameDateFormatErrors(errorFrame,table[which(futureDates),], 
                                                    dateFieldName, tableName, "future","Error")
    }
  }
  return(errorFrame)
}

findDatesOutOfRange <- function(errorFrame, table, formattedTable, dateFieldName, tableName){
      # now check for dates that seem earlier than expected range, first BIRTH_D then other dates
      # 
  if (dateFieldName == "BIRTH_D"){
    minDate <- minimumExpectedBirthDate
    errorType <- "earlyBIRTH_D"
    earlyDates <- (formattedTable[[dateFieldName]] < minDate) & (formattedTable[[dateFieldName]] != dateIndicatingUnknown)
    if (any(earlyDates, na.rm = TRUE)){
      errorFrame <- addToErrorFrameDateFormatErrors(errorFrame, table[which(earlyDates),],
                                                    dateFieldName, tableName, errorType,"Warn")
    }
  }
  else if (!(dateFieldName %in% datesThatCanBeBefore1980)){
    minDate <- minimumExpectedDate
    errorType  <- "earlyDate"
    earlyDates <- (formattedTable[[dateFieldName]] < minDate) & (formattedTable[[dateFieldName]] != dateIndicatingUnknown)
    if (any(earlyDates, na.rm = TRUE)){
      errorFrame <- addToErrorFrameDateFormatErrors(errorFrame, table[which(earlyDates),],
                                                    dateFieldName, tableName, errorType,"Warn")
    }
  }
  return(errorFrame)
}
  


detectInvalidDates <- function(errorFrame, table, formattedTable, dateFieldName, tableName, severity){
  #check first for dates with _A = Unknown but date != 1911-11-11 (dateIndicatingUnknown)
  errorFrame <- findUnknownDatesCodedImproperly(errorFrame, formattedTable, dateFieldName, tableName)
  errorFrame <- findBadDateFormats(errorFrame, table, formattedTable, dateFieldName, tableName)
  errorFrame <- findFutureDates(errorFrame, table, formattedTable, dateFieldName, tableName)
  errorFrame <- findDatesOutOfRange(errorFrame, table, formattedTable, dateFieldName, tableName)
  return(errorFrame)
} 


checkDatesInTables <- function(errorFrame){
  for (tableName in uploadList()$AllDESTables){
    print(tableName)
    dateFields <- intersect(names(formattedTables()[[tableName]]),
                            findVariablesMatchingCondition(tableName, "data_format", "YYYY-MM-DD"))
    
    if (is_empty(dateFields)) next
    
    table <- get(tableName, uploadedTables())
    formattedTable <- get(tableName, formattedTables())

 
    for (dateFieldName in dateFields){
      errorFrame <- detectInvalidDates(errorFrame, table, formattedTable,
                                         dateFieldName, tableName, severity = "Error")
      }
  }
  return(errorFrame)
}