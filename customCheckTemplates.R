# later: pull from REDCap
textForUnknown <- "Unknown"

findDateRecords <- function(df, dateVar, dateVarSym, dateIsBlank = c(TRUE, FALSE)){
  # find records meeting date condition
  if (dateIsBlank){
    # find records with a blank date field
    dateRecords <- df %>% 
      filter(is.na(!!dateVarSym))
    message <- paste("no valid date is entered for", dateVar, collapse = " ")
  } else {
    # find records with non-missing date field
    dateRecords <- df %>% 
      filter(!is.na(!!dateVarSym))
    message <- paste("a date is entered for", dateVar, collapse = " ")
  }
  return(list(
    badRecords = dateRecords,
    message = message))
}

findCodeRecords <- function(df, codeVar, codeVarSym, codeCondition, codeValLabel, codeVal){
  
  if (codeCondition == "equal"){
    # records with date condition that have specific code value for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym == codeValLabel)
    message <- paste("and", codeVar, "equals", codeVal,
                      paste0("(", codeValLabel, ")"),
                      collapse = " ")
  } else if (codeCondition == "notequal"){
    # records with date condition that have any value other than specific code value for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym != codeVal)
    message <- paste("and", codeVar, "is anything other than", 
                      codeVal, 
                      paste0("(", codeValLabel, ")"),
                      collapse = " ")
  } else if (codeCondition == "missing"){
    # records with date condition that are missing an entry for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym == missingCode) #"Missing")
    message <- paste(codeVar, "is blank (missing)", collapse = " ")
  } else if (codeCondition == "not_missing"){
    # records with date condition that are missing an entry for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym != missingCode) #"Missing")
    message <- paste(codeVar, "is not blank (missing)", collapse = " ")
  } else if (codeCondition == "missing_or_unknown"){
    # records with date condition that are missing an entry for codeVar or are
    # coded "Unknown" for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym %in% c(missingCode, textForUnknown)) #"Missing"
    message <- paste(codeVar, "is blank (missing) or coded as",
                      textForUnknown,
                      collapse = " ")
  } else if (codeCondition == "not_missing_or_unknown"){
    # records with date condition that are missing an entry for codeVar or are
    # coded "Unknown" for codeVar
    badRecords <- df %>% 
      filter(! (!!codeVarSym %in% c(missingCode, textForUnknown)))
    message <- paste(codeVar, "has an entry (other than the code for",
                      paste0(textForUnknown, ")"),
                      collapse = " ")
  }
  return(
    list(
      badRecords = badRecords,
      message = message
    
  ))
}



checkDateAgreeWithCode <- function(errorFrame, 
                                   resources,
                                   tableName, 
                                   dateVar,
                                   dateIsBlank = c(TRUE, FALSE),
                                   codeVar,
                                   codeCondition = c("equal", 
                                                     "notequal", 
                                                     "missing",
                                                     "not_missing",
                                                     "missing_or_unknown",
                                                     "not_missing_or_unknown"),
                                   codeValLabel = NULL,
                                   codeVal = NULL,
                                   attributeTo = NULL,
                                   severity = c("Error", "Warning", "Critical")
){
  # the conditions in the arguments define an ERROR record
  table <- resources$formattedTables[[tableName]]
  # check first to make sure both columns are present in the table
  if (!dateVar %in% names(table) || !codeVar %in% names(table)) return(errorFrame)
  # now we know both columns exist
  # create variables for non standard evaluation
  dateVarSym <- rlang::sym(dateVar)
  codeVarSym <- rlang::sym(codeVar)
  
  # find records meeting date condition
  check1 <- findDateRecords(
    df = table, 
    dateVar = dateVar, 
    dateVarSym = dateVarSym, 
    dateIsBlank = dateIsBlank
  )
  
  if (nrow(check1$badRecords) == 0) return(errorFrame)
  
  # now, if there are bad records, find records which meet codeCondition
  
  check2 <- findCodeRecords(
    df = check1$badRecords, 
    codeVar = codeVar, 
    codeVarSym = codeVarSym, 
    codeCondition = codeCondition, 
    codeValLabel = codeValLabel, 
    codeVal = codeVal
  )

  
  # no bad records, then exit function
  if (nrow(check2$badRecords)==0) return(errorFrame)
  
  # if there are are bad records, add them to the errorFrame
  message <- paste("If",
                   check1$message, 
                   "and",
                   check2$message, 
                   "then this record is in error",
                   collate = " ")
  
  error_field <- dateVar
  error_field2 <- codeVar
  
  if (attributeTo == codeVar){
    error_field <- codeVar
    error_field2 <- dateVar
  }
  
  errorFrame <- addToErrorFrame(
    resources$formattedTables$tblBAS,
    resources$finalGroupChoice, errorFrame, 
    resources$uploadedTables[[tableName]][badRecords$recordIndex,], 
    error_field, 
    tableName,
    "Date in conflict with code", 
    errorCode = "2.3",
    severity, message, 
    error_field2 = error_field2, 
    error2 = as.character(resources$uploadedTables[[tableName]][badRecords$recordIndex,error_field2]))
  
  return(errorFrame)
}

checkDateAgreeWithDate <- function(errorFrame, 
                                   resources,
                                   tableName, 
                                   date1Var,
                                   date1IsBlank = c(TRUE, FALSE),
                                   date2Var,
                                   date2IsBlank = c(TRUE, FALSE),
                                   attributeTo = NULL,
                                   severity = c("Error", "Warning", "Critical")
){
  # the conditions in the arguments define an ERROR record
  table <- resources$formattedTables[[tableName]]
  # check first to make sure both columns are present in the table
  if (!date1Var %in% names(table) || !date2Var %in% names(table)) return(errorFrame)
  # now we know both columns exist
  # create variables for non standard evaluation
  date1VarSym1 <- rlang::sym(date1Var)
  date2VarSym2 <- rlang::sym(date2Var)

  # find records meeting date1 condition
  check1 <- findDateRecords(
    df = table, 
    date1Var = date1Var, 
    date1VarSym = date1VarSym, 
    date1IsBlank = date1IsBlank
  )
  
  if (nrow(check1$badRecords) == 0) return(errorFrame)
  
  
  if (date1IsBlank){
    # find records with a blank date1 field
    date1Records <- table %>% 
      filter(is.na(!!date1VarSym))
    message1 <- paste("If no valid date is entered for", date1Var, collapse = " ")
  } else {
    # find records with non-missing date1 field
    date1Records <- table %>% 
      filter(!is.na(!!date1VarSym))
    message1 <- paste("If a date is entered for", date1Var, collapse = " ")
  }
  
  # now, among those records, which meet date2 condition
  if (date2IsBlank){
    # find records with a blank date2 field
    badRecords <- date1Records %>% 
      filter(is.na(!!date2VarSym))
    message2 <- paste("and", 
                      "if no valid date is entered for", date2Var,
                      collapse = " ")
  } else {
    # find records with non-missing date2 field
    badRecords <- date1Records %>% 
      filter(!is.na(!!date2VarSym))
    message2 <- paste("and", 
                      "if a valid date is entered for", date2Var,
                      collapse = " ")
  } 
  
  # no bad records, then exit function
  if (nrow(badRecords)==0) return(errorFrame)
  
  # if there are are bad records, add them to the errorFrame
  message <- paste(message1, message2, 
                   "then this record is in error",
                   collate = " ")
  
  error_field <- date1Var
  error_field2 <- date2Var
  
  if (attributeTo == date2Var){
    error_field <- date2Var
    error_field2 <- date1Var
  }
  
  errorFrame <- addToErrorFrame(
    resources$formattedTables$tblBAS,
    resources$finalGroupChoice, errorFrame, 
    resources$uploadedTables[[tableName]][badRecords$recordIndex,], 
    error_field, 
    tableName,
    "Date in conflict with code", 
    errorCode = "2.3",
    severity, message, 
    error_field2 = error_field2, 
    error2 = as.character(resources$uploadedTables[[tableName]][badRecords$recordIndex,error_field2]))
  
  return(errorFrame)
}



checkAgreement <- function(errorFrame, resources){

  errorFrame <- checkDateAgreeWithCode(
    errorFrame,
    resources,
    "tblBAS",
    dateVar = "AIDS_D",
    dateIsBlank = FALSE,
    codeVar = "AIDS_Y",
    codeCondition = "notequal",
    codeValLabel = "Yes",
    codeVal = 1,
    attributeTo = "AIDS_D",
    severity = "Error")
  
  errorFrame <- checkDateAgreeWithCode(
    errorFrame,
    resources,
    "tblART",
    dateVar = "ART_SD",
    dateIsBlank = TRUE,
    codeVar = "ARTSTART_RS",
    codeCondition = "not_missing_or_unknown",
    attributeTo = "ARTSTART_RS",
    severity = "Error")

  return(errorFrame)
}

