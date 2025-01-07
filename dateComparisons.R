add_A_to_errorFrame <- function(indexTable, groupVar, errorFrame, first_A, first_A_flag, 
                                second_A, second_A_flag, firstDate, secondDate, 
                                tableName_first, tableName_second,
                                tableOfErrorRecords_first, 
                                tableOfErrorRecords_second, severity = "Error"){
  # tableOfErrorRecords is uploadedTables not formattedTables at this time
  
  message <- paste0(secondDate, " should not be before ", firstDate)
  errorType <- paste0(secondDate, " before ", firstDate)
  if (first_A_flag){
    toPrint <- tableOfErrorRecords_first[[first_A]]
    toPrint[which(safeTrimWS(toPrint) == "")] <- "blank (treated as D)"
    invalidDateCodeIndices <- which(!toPrint %in% c("blank (treated as D)", names(date_A_codes)))
    toPrint[invalidDateCodeIndices] <- paste0("an invalid code (", toPrint[invalidDateCodeIndices],", treated as D)")
    first_A_message <- paste0(" when ", first_A, " is ", toPrint)
  } else first_A_message <- paste0(" when no ", first_A, " column is provided")
  
  if (second_A_flag){
    toPrint <- tableOfErrorRecords_second[[second_A]]
    toPrint[which(safeTrimWS(toPrint) == "")] <- "blank (treated as D)"
    invalidDateCodeIndices <- which(!toPrint %in% c("blank (treated as D)", names(date_A_codes)))
    toPrint[invalidDateCodeIndices] <- paste0("an invalid code (", toPrint[invalidDateCodeIndices],", treated as D)")
    second_A_message <- paste0(" and ", second_A, " is ", toPrint)
  } else second_A_message <- paste0(" and no ", second_A, " column is provided")
  
  
  # we know that either the first date or the second date, or both, have _A fields
  
  if (first_A_flag && second_A_flag){
    variable3 <- first_A
    value3 <- tableOfErrorRecords_first[[first_A]]
    variable4 <- second_A
    value4 <- tableOfErrorRecords_second[[second_A]]
  } else {
    variable4 <- ""
    value4 <- ""
  }
  if (!first_A_flag){
    variable3 <- second_A
    value3 <- tableOfErrorRecords_second[[second_A]]
  } else if (!second_A_flag){
    variable3 <- first_A
    value3 <- tableOfErrorRecords_first[[first_A]]
  }
 
  message <- paste0(message, first_A_message, second_A_message, ".")
  errorFrame <- addToErrorFrame(indexTable, groupVar,
                                errorFrame = errorFrame,
                                table = tableOfErrorRecords_second,
                                field = secondDate,
                                tableName = tableName_second,
                                errorType = errorType,
                                severity = severity,
                                message = message,
                                error_field2 = firstDate,
                                error2 = as.character(tableOfErrorRecords_first[[firstDate]]),
                                error_field3 = variable3,
                                error3 = as.character(value3),
                                error_field4 = variable4,
                                error4 = as.character(value4),
                                crosstable = tableName_first,
                                errorCode = "2.1")
  return(errorFrame)
}

# compare every date field with dates that should be before most dates, exceptions specified
globalDateChecksBefore <- function(errorFrame, resources){
  gc()
  # dateApproxFlag (set in definitions.R, is TRUE if this network has a date approximation 
  # variable that can accompany date variables
  if (dateApproxFlag){
    dateApproxExt <- projectDef$date_approx
  } 
  
  # find *before* global date fields present in dataset, store in checkTheseGlobalDates
  globalChecks <- globalDateBeforeChecks
  if (separateDateVars){
    checkTheseGlobalDates <- intersect(names(globalChecks), 
                                     paste0(unlist(resources$tablesAndVariables$matchingColumns, use.names = FALSE), "_CALCDATE"))
  } else {
    checkTheseGlobalDates <- intersect(names(globalChecks), 
                                     unlist(resources$tablesAndVariables$matchingColumns, use.names = FALSE))
  }
  
  for (globalDateName in checkTheseGlobalDates){
    print(globalDateName)
    # read in details about this global date field, such as table name, exceptions
    dateCheck <- get(globalDateName, globalChecks)
    dateVarsInMain <- intersect(names(resources$formattedTables[[dateCheck$table]]),
                                findVariablesMatchingCondition(dateCheck$table, tableDef, "data_format", "YYYY-MM-DD"))
    if (dateApproxFlag){
      possibleDateApproxVars <- paste0(dateVarsInMain, dateApproxExt)
      dateApproxVars <- intersect(names(resources$formattedTables[[dateCheck$table]]),
                            possibleDateApproxVars)
      allDateRelatedVars <- c(dateVarsInMain, dateApproxVars)
    } else {
      allDateRelatedVars <- dateVarsInMain
    }
    
    globalDate <- resources$formattedTables[[dateCheck$table]] %>% 
      filter(!(!!rlang::sym(globalDateName) %in% dateIndicatingUnknown)) %>%  
      filter(!is.na(!!rlang::sym(globalDateName))) %>% 
      select(!!patientVarSym, 
             any_of(allDateRelatedVars),
             recordIndex1=recordIndex)
    
    # iterate through all uploadedTables that have patientVar as primary ID
    for (tableName in c(indexTableName, tablesAndVariables$tablesToCheckWithPatientID)){
      # find all of the date fields in current table, except global date field and any exceptions
      variablesInTable <- names(resources$formattedTables[[tableName]])
      # find all the dates in the current table but leave out the globalDate and any exceptions
      dateFields <- intersect(findVariablesMatchingCondition(tableName, tableDef, "data_format", "YYYY-MM-DD"), 
                              variablesInTable)
      if (is_empty(dateFields)) next
      dateFields <- dateFields[!(dateFields %in% c(globalDateName, dateCheck$exceptions))]
      if (is_empty(dateFields)) next
      
      if (dateApproxFlag){
        # find _A columns for only those dates included in dateFields
        possibleDate_A_fields <- paste0(dateFields, projectDef$date_approx)
        date_A_fields <-  intersect(variablesInTable, 
                                    possibleDate_A_fields)
      } else {
        date_A_fields <- NULL
      }

      # if the current table is different from dateCheck$table, then merge the global date table 
      # with the current table, columns recordIndex, patientVar, and all dateFields, merge by patientVar 
      # Otherwise, if current table IS the same as dateCheck$table, then include all globalDate and other date fields
      if (dateCheck$table != tableName){
        dateTable <- left_join(globalDate, 
                               resources$formattedTables[[tableName]][,c("recordIndex",
                                                                         patientVar,
                                                                         dateFields, 
                                                                         date_A_fields)], 
                               by=patientVar) %>% rename(recordIndex2=recordIndex)
      } else dateTable <- globalDate %>% mutate(recordIndex2=recordIndex1)
      firstDate <- globalDateName
      table1Name <- dateCheck$table
      table2Name <- tableName
      for (secondDate in dateFields){
        badDates <- NULL
        badDates <- dateTable[which( (dateTable[[secondDate]] < dateTable[[firstDate]]) &
                                       (!dateTable[[secondDate]] %in% dateIndicatingUnknown) ),]
        
        if (nrow(badDates) == 0) next
        #--------------------------potential date logic errors were found---------------------
        #errorType <-  paste0("Date before ", firstDate) 
        errorType <- paste0(secondDate, " before ", firstDate) 
        message <- paste0(secondDate, " should not be before ", firstDate, ".")
        #------If network doesn't have date approximation variables OR -----------------------
        #------if no _A fields, these are all errors------------------------------------------
        if (
          !dateApproxFlag ||
          (
            !(exists(paste0(firstDate, projectDef$date_approx), badDates)) && 
            !(exists(paste0(secondDate, projectDef$date_approx), badDates))
          )
        ){

          errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                        resources$finalGroupChoice,
                                        errorFrame, 
                                        table = resources$uploadedTables[[table2Name]][badDates$recordIndex2,],
                                        field = secondDate, tableName = table2Name,
                                        errorType = errorType, severity = "Error", message = message,
                                        error_field2 = firstDate, 
                                        error2 = as.character(resources$uploadedTables[[table1Name]][badDates$recordIndex1,firstDate]),
                                        errorCode = "2.1")

        } else {
          errorFrame <- generalDateOrder(resources,
                                         resources$finalGroupChoice,
                                         errorFrame = errorFrame,
                                         firstDate = firstDate, secondDate = secondDate, 
                                         firstTableName=table1Name, secondTableName=table2Name,
                                         jointTable = badDates)
        }
      }
    }
  }
  return(errorFrame)
}

# compare every date field with dates that should be after most dates, exceptions specified
globalDateChecksAfter <- function(errorFrame, resources){
  # dateApproxFlag (set in definitions.R, is TRUE if this network has a date approximation 
  # variable that can accompany date variables
  if (dateApproxFlag){
    dateApproxExt <- projectDef$date_approx
  } 
  # find *after* global date fields present in dataset, store in checkTheseGlobalDates
  globalChecks <- globalDateAfterChecks
  if (separateDateVars){
    checkTheseGlobalDates <- intersect(names(globalChecks), 
                                       paste0(unlist(resources$tablesAndVariables$matchingColumns, use.names = FALSE), "_CALCDATE"))
  } else {
    
    checkTheseGlobalDates <- intersect(names(globalChecks), 
                                       unlist(resources$tablesAndVariables$matchingColumns, use.names = FALSE))
  }

  for (globalDateName in checkTheseGlobalDates){
    print(globalDateName)
    # read in details about this global date field, such as table name, exceptions
    dateCheck <- get(globalDateName, globalChecks)
    dateVarsInMain <- intersect(names(resources$formattedTables[[dateCheck$table]]),
                                findVariablesMatchingCondition(dateCheck$table, tableDef, "data_format", "YYYY-MM-DD"))
    if (dateApproxFlag){
      possibleDateApproxVars <- paste0(dateVarsInMain, dateApproxExt)
      dateApproxVars <- intersect(names(resources$formattedTables[[dateCheck$table]]),
                                  possibleDateApproxVars)
      allDateRelatedVars <- c(dateVarsInMain, dateApproxVars)
    } else {
      allDateRelatedVars <- dateVarsInMain
    }
    
    globalDate <- resources$formattedTables[[dateCheck$table]] %>% 
      filter(!(!!rlang::sym(globalDateName) %in% dateIndicatingUnknown)) %>%  
      filter(!is.na(!!rlang::sym(globalDateName))) %>% 
      select(!!patientVarSym,
             any_of(allDateRelatedVars),
             recordIndex2=recordIndex)
    # REVISIT THIS 
    # iterate through all uploadedTables that have patientVar as ID
    for (tableName in c(indexTableName, resources$tablesAndVariables$tablesToCheckWithPatientID)){
      # find all of the date fields in current table, except global date field and any exceptions
      variablesInTable <- names(resources$formattedTables[[tableName]])
      # find all the dates in the current table but leave out the globalDate and any exceptions
      dateFields <- intersect(findVariablesMatchingCondition(tableName, tableDef, "data_format", "YYYY-MM-DD"), 
                              variablesInTable)
      if (is_empty(dateFields)) next
      dateFields <- dateFields[!(dateFields %in% c(globalDateName, dateCheck$exceptions))]
      if (is_empty(dateFields)) next
      # find _A columns for only those dates included in dateFields
      if (dateApproxFlag){
        # find _A columns for only those dates included in dateFields
        possibleDate_A_fields <- paste0(dateFields, projectDef$date_approx)
        date_A_fields <-  intersect(variablesInTable, 
                                    possibleDate_A_fields)
      } else {
        date_A_fields <- NULL
      }
      
      # if the current table is different from dateCheck$table, then merge the global date table 
      # with the current table, columns recordIndex, patientVar, and all dateFields, merge by patientVar 
      # Otherwise, if current table IS the same as dateCheck$table, then include all globalDate and other date fields
      if (dateCheck$table != tableName){
        dateTable <- left_join(globalDate, 
                               resources$formattedTables[[tableName]][, c("recordIndex",
                                                                          patientVar,
                                                                          dateFields,
                                                                          date_A_fields)], 
                               by=patientVar) %>% rename(recordIndex1=recordIndex)
      } else dateTable <- globalDate %>% mutate(recordIndex1=recordIndex2)
      
      secondDate <- globalDateName
      table2Name <- dateCheck$table
      table1Name <- tableName
      for (firstDate in dateFields){
        badDates <- NULL
        badDates <- dateTable[which( (dateTable[[secondDate]] < dateTable[[firstDate]]) &
                                       (!dateTable[[secondDate]] %in% dateIndicatingUnknown) ),]

        if (nrow(badDates) == 0) next
        #--------------------------potential date logic errors were found---------------------
        errorType <- paste0(secondDate, " before ", firstDate) #edited
        message <- paste0(secondDate, " should not be before ", firstDate, ".")
        #------If network doesn't have date approximation variables OR -----------------------
        #------if no _A fields, these are all errors------------------------------------------
        if (
          !dateApproxFlag ||
          (
            !(exists(paste0(firstDate, projectDef$date_approx), badDates)) && 
            !(exists(paste0(secondDate, projectDef$date_approx), badDates))
          )
        ){
          errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                        resources$finalGroupChoice,
                                        errorFrame, 
                                        table = resources$uploadedTables[[table2Name]][badDates$recordIndex2,],
                                        field = secondDate, tableName = table2Name,
                                        errorType = errorType, severity = "Error", message = message,
                                        error_field2 = firstDate, 
                                        error2 = as.character(resources$uploadedTables[[table1Name]][badDates$recordIndex1,firstDate]),
                                        errorCode = "2.1")
        } else {
          
          errorFrame <- generalDateOrder(resources,
                                         resources$finalGroupChoice,
                                         errorFrame = errorFrame,
                                         firstDate = firstDate, secondDate = secondDate, 
                                         firstTableName=table1Name, secondTableName=table2Name, 
                                         jointTable = badDates)
        }
      }
    }
  }
  return(errorFrame)
}



# generalDateOrder -------------------------------------------------------------------------     
generalDateOrder <- function(resources, groupVar, errorFrame, firstDate, secondDate, firstTableName, 
                             secondTableName, jointTable){
  #-------Now add fake _A if one or the other doesn't have _A
  # judy change variable not included to NA
  first_A_flag <- TRUE
  first_A <- paste0(firstDate, projectDef$date_approx)
  second_A_flag <- TRUE
  second_A <- paste0(secondDate, projectDef$date_approx)
  if (!exists(paste0(firstDate, projectDef$date_approx), jointTable)){
    first_A_flag <- FALSE
    jointTable[[first_A]] <- "Variable not included in dataset"  ##or NA?
  } else if (!exists(paste0(secondDate, projectDef$date_approx), jointTable)){
    second_A_flag <- FALSE
    jointTable[[second_A]] <- "Variable not included in dataset" ##or NA?
  }
  # datePairChecks in definitions.R (from dateApproximationLogic.json) outlines 
  # date _A logic
  for (dateCheck in datePairChecks){
    datesToCheck <- jointTable[which(
      (jointTable[[first_A]] %in% dateCheck$first_A) &
        (jointTable[[second_A]] %in% dateCheck$second_A)),]
    
    if (nrow(datesToCheck) == 0) next
    # if year check not required, no further checking needed; these are errors
    if (!dateCheck$yearCheck){
      thisCheckBadDates <- datesToCheck
      errorFrame <- add_A_to_errorFrame(resources$formattedTables[[indexTableName]], 
                                        groupVar, errorFrame,
                                        first_A, first_A_flag, 
                                        second_A, second_A_flag, 
                                        firstDate, secondDate, 
                                        tableName_first = firstTableName,
                                        tableName_second = secondTableName,
                                        tableOfErrorRecords_first = 
                                          resources$uploadedTables[[firstTableName]][thisCheckBadDates$recordIndex1,],
                                        tableOfErrorRecords_second = 
                                          resources$uploadedTables[[secondTableName]][thisCheckBadDates$recordIndex2,])
    } else if (dateCheck$yearCheck){
      # JUDY why doesn't this work
      # thisCheckBadDates <- datesToCheck %>% 
      #   filter(year(.[firstDate]) > year(.[secondDate]))
      thisCheckBadDates <- datesToCheck[which(
        year(datesToCheck[[firstDate]]) > year(datesToCheck[[secondDate]])
      ),]
      if (nrow(thisCheckBadDates) == 0) next
      # judy ADD MESSAGE ABOUT YEAR
      errorFrame <- add_A_to_errorFrame(resources$formattedTables[[indexTableName]],
                                        groupVar, errorFrame,
                                        first_A, first_A_flag, 
                                        second_A, second_A_flag, 
                                        firstDate, secondDate, 
                                        tableName_first = firstTableName,
                                        tableName_second = secondTableName,
                                        tableOfErrorRecords_first = 
                                          resources$uploadedTables[[firstTableName]][thisCheckBadDates$recordIndex1,],
                                        tableOfErrorRecords_second = 
                                          resources$uploadedTables[[secondTableName]][thisCheckBadDates$recordIndex2,])
      
      
      
    }
    if (dateCheck$monthCheck == TRUE){
      # after eliminating records with year1 > year2, remaining records have year1==year2
      # if monthCheck is true, error if month(date1) > month(date2)
      equalYears <- anti_join(datesToCheck, thisCheckBadDates, names(datesToCheck))
      thisCheckBadDates <- equalYears[which(
        month(equalYears[[firstDate]]) > month(equalYears[[secondDate]])
      ),] 
      #Judy  filter(month(.[firstDate]) > month(.[secondDate]))
      if (nrow(thisCheckBadDates) == 0) next
      # judy add message about month
      errorFrame <- add_A_to_errorFrame(resources$formattedTables[[indexTableName]],
                                        groupVar, errorFrame,
                                        first_A, first_A_flag, 
                                        second_A, second_A_flag, 
                                        firstDate, secondDate, 
                                        tableName_first = firstTableName,
                                        tableName_second = secondTableName,
                                        tableOfErrorRecords_first = 
                                          resources$uploadedTables[[firstTableName]][thisCheckBadDates$recordIndex1,],
                                        tableOfErrorRecords_second = 
                                          resources$uploadedTables[[secondTableName]][thisCheckBadDates$recordIndex2,])
    }
    
  }
  return(errorFrame)
}




## withinTableDateOrder ----------------------------------------------------------------------
withinTableDateOrder <- function(errorFrame, resources){
  dateOrderPairs <- list() # pairs found based on naming conventions, sdExt and edExt
  dateOrdersInData <- list() # pairs found in dataset that match date order json 
  dataset <- resources$formattedTables
  currentTables <- names(dataset)
  # if no sdExt or edExt were specified in Harmonist0C, no need to check for 
  # date order pairs based on naming convention. Only check for those 
  # explicitly listed in withinTableDateOrder.json
  startEndFlag <- TRUE
  
  if (is.null(sdExt) || is.null(edExt)
      || (safeTrimWS(sdExt) == "") || (safeTrimWS(edExt) == "")){
    startEndFlag <- FALSE
  } 
  
  if (startEndFlag){
    # create a list of date order pairs in each table
    for (tableName in currentTables){
      tableData <- dataset[[tableName]]
      varsInTable <- names(tableData)
      # currently allows for only one date order pair per table
      endDateNames <- varsInTable[which(endsWith(varsInTable, edExt))]
      if (is_empty(endDateNames)) next
      baseDateNames <- str_sub(endDateNames, end = -(1 + nchar(edExt)))
      # in case multiple date order pairs in a table:
      for (baseDateName in baseDateNames){
        startDateName <- paste0(baseDateName, sdExt)
        if (!startDateName %in% varsInTable) next
        # we have found a date order pair. Add to list.
        endDateName <- paste0(baseDateName, edExt)
        dateOrderPairs[[paste(tableName, baseDateName, sep = "_")]] <- 
          tibble(tableName = tableName,
                 firstDate = startDateName,
                 secondDate = endDateName)
      }
    }
  }

  # now we have the date order pairs based on naming convention 
  # listed in dateOrderPairs
  
  if (!is_empty(dateOrderPairs)){
    dateOrderPairs <- rbindlist(dateOrderPairs, use.names = TRUE)
  }
  
  # if dateOrders (from withinTablesDateOrder json) listed other 
  # within table date order pairs, include here (if exist in dataset)
  if (!is_empty(dateOrders)){
    dateOrdersInData <- rbindlist(dateOrders, use.names = TRUE) %>% 
      filter(tableName %in% resources$tablesAndVariables$tablesToCheck)
    if (nrow(dateOrdersInData) == 0){
      dateOrdersInData <- list()
    }
  } 

  if (is_empty(dateOrderPairs) && is_empty(dateOrdersInData)){
    return(errorFrame)
  } else if (!is_empty(dateOrdersInData)){
    dateOrderPairs <- rbindlist(list(dateOrderPairs, dateOrdersInData), use.names = TRUE)
  }
  
  # make sure no duplicate date order checks
  dateOrderPairs <- unique(dateOrderPairs) 
  if (is_empty(dateOrderPairs)) return(errorFrame)
  
  for (dateCheckNum in 1:nrow(dateOrderPairs)){
    thisCheck <- dateOrderPairs[dateCheckNum,]
    tableName <- thisCheck$tableName
    firstDate <- thisCheck$firstDate
    secondDate <- thisCheck$secondDate
    table <- dataset[[tableName]]
    dateFields <- c(firstDate, secondDate)
    
    # if both dates aren't in table, go to next date order pair
    if (!all(dateFields %in% names(table))) next
    
    if (dateApproxFlag){
      # find _A columns for only those dates included in dateFields
      possibleDate_A_fields <- paste0(dateFields, projectDef$date_approx)
      date_A_fields <-  intersect(names(table), 
                                  possibleDate_A_fields)
    } else {
      date_A_fields <- NULL
    }
    
    tableToCheck <- table %>% select(tableIDField[[tableName]],
                                     all_of(dateFields),
                                     all_of(date_A_fields), 
                                     recordIndex)
    date1 <- sym(firstDate)
    date2 <- sym(secondDate)
    badDates <- tableToCheck %>% filter(!is.na(!!date1)) %>% filter(!is.na(!!date2)) %>% 
      filter(! (!!date1 %in% dateIndicatingUnknown)) %>% filter(! (!!date2 %in% dateIndicatingUnknown)) %>% 
      filter(!!date2 < !!date1)
    
    
    if (nrow(badDates) == 0) next
    #--------------------------potential date logic errors were found---------------------
    errorType <- paste0(secondDate, " before ", firstDate)
    message <- paste0(secondDate, " should not be before ", firstDate, " in table ", tableName, ".")
    #------Simple case first: if no _A fields, these are all errors-----------------------
    if (
      !dateApproxFlag ||
      (
        !(exists(paste0(firstDate, projectDef$date_approx), badDates)) && 
        !(exists(paste0(secondDate, projectDef$date_approx), badDates))
      )
    ){
      errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                    resources$finalGroupChoice,
                                    errorFrame, 
                                    table = resources$uploadedTables[[tableName]][badDates$recordIndex,],
                                    field = secondDate, tableName = tableName,
                                    errorType = errorType, severity = "Error", message = message,
                                    error_field2 = firstDate, 
                                    error2 = as.character(resources$uploadedTables[[tableName]][badDates$recordIndex,firstDate]),
                                    errorCode = "2.1")
      
    } else {
      table <- badDates %>% mutate(recordIndex1 = recordIndex) %>% mutate(recordIndex2 = recordIndex)
      errorFrame <- generalDateOrder(resources, 
                                     resources$finalGroupChoice, errorFrame = errorFrame,
                                     firstDate = firstDate, secondDate = secondDate, 
                                     firstTableName=tableName, secondTableName=tableName, jointTable = table)
    }
    
  }
  return(errorFrame)
}
      




