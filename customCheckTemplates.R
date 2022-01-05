# later: pull from REDCap
textForUnknown <- "Unknown"

##########################################################################################
# findDateRecords
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

##########################################################################################
# findCodeRecords
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
      filter(!!codeVarSym == "Missing")
    message <- paste(codeVar, "is blank (missing)", collapse = " ")
  } else if (codeCondition == "not_missing"){
    # records with date condition that are missing an entry for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym != "Missing")
    message <- paste(codeVar, "is not blank (missing)", collapse = " ")
  } else if (codeCondition == "missing_or_unknown"){
    # records with date condition that are missing an entry for codeVar or are
    # coded "Unknown" for codeVar
    badRecords <- df %>% 
      filter(!!codeVarSym %in% c("Missing", textForUnknown))
    message <- paste(codeVar, "is blank (missing) or coded as",
                      textForUnknown,
                      collapse = " ")
  } else if (codeCondition == "not_missing_or_unknown"){
    # records with date condition that are missing an entry for codeVar or are
    # coded "Unknown" for codeVar
    badRecords <- df %>% 
      filter(! (!!codeVarSym %in% c("Missing", textForUnknown)))
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
  
  # check to confirm table exists in dataset
  if (!tableName %in% names(resources$formattedTables)) return(errorFrame)
  
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
  
  # check to confirm table exists in dataset
  if (!tableName %in% names(resources$formattedTables)) return(errorFrame)
  
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

##############################################################################################
# checkCodeAgreeWithCode 
# FIX ME add error type and error code as arguments
checkCodeAgreeWithCode <- function(errorFrame, 
                                   resources,
                                   tableName1, 
                                   codeVar1,
                                   codeCondition1 = c("equal", 
                                                     "notequal", 
                                                     "missing",
                                                     "not_missing",
                                                     "missing_or_unknown",
                                                     "not_missing_or_unknown"),
                                   codeValLabel1 = NULL,
                                   codeVal1 = NULL,
                                   tableName2,
                                   codeVar2,
                                   codeCondition2 = c("equal", 
                                                     "notequal", 
                                                     "missing",
                                                     "not_missing",
                                                     "missing_or_unknown",
                                                     "not_missing_or_unknown"),
                                   codeValLabel2 = NULL,
                                   codeVal2 = NULL,
                                   attributeTo = NULL,
                                   severity = c("Error", "Warning", "Critical")
){
  # the conditions in the arguments define an ERROR record

  # check to confirm table(s) exists in dataset
  tableNames <- unique(c(tableName1, tableName2))
  if (!tableNames %in% names(resources$formattedTables)) return(errorFrame)
  
  # check first to make sure the coded var 1 exists in table 1
  if (!codeVar1 %in% names(resources$formattedTables[[tableName1]])) return(errorFrame)
  # now make sure coded var 2 exists in table 2
  if (!codeVar2 %in% names(resources$formattedTables[[tableName2]])) return(errorFrame)
  
  table1 <- resources$formattedTables[[tableName1]]
  
  # create variables for non standard evaluation
  codeVarSym1 <- rlang::sym(codeVar1)
  codeVarSym2 <- rlang::sym(codeVar2)
  
  # find records meeting code 1 condition
  check1 <- findCodeRecords(
    df = table1, 
    codeVar = codeVar1, 
    codeVarSym = codeVarSym1, 
    codeCondition = codeCondition1, 
    codeValLabel = codeValLabel1, 
    codeVal = codeVal1
  )
  
  if (nrow(check1$badRecords) == 0) return(errorFrame)
  
  # Now join with table 2 and see if records meet 2nd condition
  
  if (tableName1 == tableName2){
    sameTable <- TRUE
    combinedTable <- check1$badRecords
  } else {
    sameTable <- FALSE
    # this works if both tables have patientVar as columns - FIX ME
    combinedTable <- inner_join(check1$badRecords, 
                               resources$formattedTables[[tableName2]],
                               by = patientVar)
  }
  
  # if no patients from the first check exist in the combined Table, exit
  if (nrow(combinedTable) == 0) return(errorFrame)
  
  # find records meeting code 2 condition
  check2 <- findCodeRecords(
    df = combinedTable, 
    codeVar = codeVar2, 
    codeVarSym = codeVarSym2, 
    codeCondition = codeCondition2, 
    codeValLabel = codeValLabel2, 
    codeVal = codeVal2
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
  
  error_field <- codeVar1
  error_field2 <- codeVar2
  
  
  errorFrame <- addToErrorFrame(
    resources$formattedTables$tblBAS,
    resources$finalGroupChoice, 
    errorFrame, 
    resources$uploadedTables[[tableName]][badRecords$recordIndex,], 
    codeVar1, 
    tableName1,
    errorType = "Code conflict", 
    errorCode = "2.3",
    severity, message, 
    error_field2 = codeVar2, 
    error2 = as.character(resources$uploadedTables[[tableName]][badRecords$recordIndex,error_field2]),
    crosstable = tableName2)
  
  return(errorFrame)
}




checkAgreement <- function(errorFrame, resources){
  browser()
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

