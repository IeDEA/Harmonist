numberOfCharactersInDate <- 10 #yyyy-mm-dd
dateErrorType <- list(
  "format" = "Invalid date format",
  "future" = "Future date",
  "earlyDate" = paste0("Date before ", year(minimumExpectedDate)),
  "earlyBIRTH_D" = paste0("BIRTH_D before ", year(minimumExpectedBirthDate)),
  "_D_A" = "Conflict between date and date approximation"
)
dateErrorDesc <- list(
  "format" = "Date format must be YYYY-MM-DD.",
  "future" = "This date is in the future.",
  "earlyDate" = paste0("Dates (other than BIRTH_D) are expected to be after ", year(minimumExpectedDate), "."),
  "earlyBIRTH_D" = paste0("Patient BIRTH_D is expected to be after ", year(minimumExpectedBirthDate), "."),
  "_D_A" = paste0("A date was provided but the date approximation was coded as Unknown, which is conflicting information. If a date approximation is coded as Unknown, the date entry should be blank or ", dateIndicatingUnknown, ".")
)
dateErrorCode <- list(
  "format" = "1.5a", #"Invalid date format",
  "future" = "2", #"Future date",
  "earlyDate" = "2.2a", #"Date Logic",
  "earlyBIRTH_D" = "2.2b", #"Date Logic",
  "_D_A" = "2.3a" #"Date _A Logic"
)




addToErrorFrameDateFormatErrors <- function(resources, groupVar, errorFrame, table, field, tableName, errorNumber, severity, ...
                                           ){
  errorFrame <- addToErrorFrame(resources$formattedTables$tblBAS, groupVar, errorFrame, table, field, tableName, errorType = dateErrorType[[errorNumber]],
                              errorCode = dateErrorCode[[errorNumber]], 
                              severity, message = dateErrorDesc[[errorNumber]], ...)
  return(errorFrame)
}


findUnknownDatesCodedImproperly <- function(resources, groupVar, errorFrame, formattedTable, dateFieldName, tableName){
  if (paste0(dateFieldName,"_A") %in% names(formattedTable)){
    dateField_A <- paste0(dateFieldName, "_A")
    print(dateField_A)
    datesImproperlyCoded <- formattedTable %>%  filter(!! rlang::sym(dateField_A) == "Unknown") %>% 
      filter(!is.na(!! rlang::sym(dateFieldName))) %>% 
      filter(!! rlang::sym(dateFieldName) != dateIndicatingUnknown)
    if (nrow(datesImproperlyCoded) > 0){
      errorFrame <- addToErrorFrameDateFormatErrors(resources, groupVar, errorFrame, datesImproperlyCoded, 
                                                    dateFieldName, tableName, "_D_A", "Warning",  
                                                    error_field2 = dateField_A, 
                                                    error2 = uploadedTables()[[tableName]][datesImproperlyCoded$recordIndex, dateField_A])
    }
  }
  return(errorFrame)
}

findBadDateFormats <- function(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName){
  # first, add warning for date column formatted as numeric instead of date
  if (class(table[[dateFieldName]])[[1]] == "numeric"){
    errorFrame <- addGeneralError(groupVar, errorFrame, field = dateFieldName, tableName = tableName, 
                                  errorType = "Invalid date column format",
                                  errorCode = "1.5b", 
                                  severity = "Warning",
                                  message = paste(dateFieldName, "is a date field but is formatted in this table as numeric. Data quality checks assume this to be the number of days since 1960-01-01 but please save this field as a date"))
    return(errorFrame)
  }
  # if date column not formatted as numeric, continue:
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
      errorFrame <- addToErrorFrameDateFormatErrors(resources, groupVar, errorFrame,table[which(badDates),], 
                                                    dateFieldName, tableName, "format", "Error")
    }
  }
  return(errorFrame)
}

findFutureDates <- function(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName){
  if (dateFieldName %in% datesInFuture) return(errorFrame)
  else {
    futureDates <- formattedTable[[dateFieldName]] > today()
    if (any(futureDates, na.rm = TRUE)){
      errorFrame <- addToErrorFrameDateFormatErrors(resources, groupVar, errorFrame,table[which(futureDates),], 
                                                    dateFieldName, tableName, "future","Error")
    }
  }
  return(errorFrame)
}

findDatesOutOfRange <- function(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName){
      # now check for dates that seem earlier than expected range, first BIRTH_D then other dates
      # 
  if (dateFieldName == "BIRTH_D"){
    minDate <- minimumExpectedBirthDate
    errorType <- "earlyBIRTH_D"
    earlyDates <- (formattedTable[[dateFieldName]] < minDate) & (formattedTable[[dateFieldName]] != dateIndicatingUnknown)
    if (any(earlyDates, na.rm = TRUE)){
      errorFrame <- addToErrorFrameDateFormatErrors(resources, groupVar, errorFrame, table[which(earlyDates),],
                                                    dateFieldName, tableName, errorType,"Warning")
    }
  }
  else if (!(dateFieldName %in% datesThatCanBeBefore1980)){
    minDate <- minimumExpectedDate
    errorType  <- "earlyDate"
    earlyDates <- (formattedTable[[dateFieldName]] < minDate) & (formattedTable[[dateFieldName]] != dateIndicatingUnknown)
    if (any(earlyDates, na.rm = TRUE)){
      errorFrame <- addToErrorFrameDateFormatErrors(resources,
                                                    groupVar, errorFrame, table[which(earlyDates),],
                                                    dateFieldName, tableName, errorType,"Warning")
    }
  }
  return(errorFrame)
}
  


detectInvalidDates <- function(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName, severity){
  #check first for dates with _A = Unknown but date != 1911-11-11 (dateIndicatingUnknown)
  errorFrame <- findUnknownDatesCodedImproperly(resources, groupVar, errorFrame, formattedTable, dateFieldName, tableName)
  errorFrame <- findBadDateFormats(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName)
  errorFrame <- findFutureDates(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName)
  errorFrame <- findDatesOutOfRange(resources, groupVar, errorFrame, table, formattedTable, dateFieldName, tableName)
  return(errorFrame)
} 


checkDatesInTables <- function(errorFrame, resources){
  for (tableName in resources$tablesAndVariables$tablesToCheck){
    print(tableName)
    dateFields <- intersect(names(resources$formattedTables[[tableName]]),
                            findVariablesMatchingCondition(tableName, tableDef, "data_format", "YYYY-MM-DD"))
    
    if (is_empty(dateFields)) next
    
    table <- get(tableName, resources$uploadedTables)
    formattedTable <- get(tableName, resources$formattedTables)

 
    for (dateFieldName in dateFields){
      errorFrame <- detectInvalidDates(resources, resources$finalGroupChoice, errorFrame, table, formattedTable,
                                         dateFieldName, tableName, severity = "Error")
      }
  }
  return(errorFrame)
}