findIt <- function(df, varName, indexTable){
  if (exists(varName, df)) {
    result <- df[[varName]]

    # if it's already a factor, unknowns have been handled
    if (is.factor(result)) return(result)

    # if any patients weren't in the indexTable, replace with Unknown
    if (any(is.na(result))){
      result <- replace_na(result, "Unknown")
    }

    # If any groupVars were missing in the indexTable, replace with Unknown
    if (any(safeTrimWS(result) == "")){
      blankGroup <- safeTrimWS(result) == ""
      result[which(blankGroup)] <- NA
      result <- replace_na(result, "Unknown")
    }
    return(result)
  }
  # if the grouping variable is not in the dataset, see if the patient variable is
  # and find the grouping variable by linking to indexTable
  if (exists(patientVar, df)){
    result <- indexTable[match(df[[patientVar]], indexTable[[patientVar]]), varName]

    # if it's already a factor, unknowns have been handled
    if (is.factor(result)) return(result)

    # if any patients weren't in the indexTable, replace with Unknown
    if (any(is.na(result))){
      result <- replace_na(result, "Unknown")
    }

    # If any groupVars were missing in the indexTable, replace with Unknown
    if (any(safeTrimWS(result) == "")){
      blankGroup <- safeTrimWS(result) == ""
      result[which(blankGroup)] <- NA
      result <- replace_na(result, "Unknown")
    }
  }
  else {
    result <- "Unknown"
  }
  return(result)
}

# adds pervasive errors to the error frame. Requires a summaryOfErrors data frame
# summaryOfErrors has three columns: error, grouping variable column, and the quantity of errors
# of this type in the corresponding group
addPervasiveToErrorFrame <- function(groupVar, errorFrame, summaryOfErrors, 
                                     tableName, category, fieldName, 
                                     message, severity = "Error", errorCode){
  # update errorCount, errorRowCount, and errorExcess
  lastErrorCount <- isolate(errorCount())
  newErrorCount <- lastErrorCount + sum(summaryOfErrors[["quantity"]])
  errorCount(newErrorCount)
  newErrorRowCount <- isolate(errorRows()) + nrow(summaryOfErrors)
  errorRows(newErrorRowCount)
  
  if (isolate(errorRows()) > errorLimit) {
    errorExcess(TRUE)
  }
  id1_fieldName <- tableIDField[[tableName]][[1]]
  summaryOfErrors <- summaryOfErrors %>% 
    mutate(table = tableName[[1]], # in case a vector of messages got passed along 
           category = category[[1]],
           error_field = fieldName[[1]], 
           description = message[[1]],  
           severity = severity[[1]],
           errorCode = errorCode[[1]],
           id1_field = id1_fieldName[[1]],
           id1 = paste(quantity, "records linked to patients in ", !!rlang::sym(groupVar))
    )
  
  index <- paste0(tableName[[1]], fieldName[[1]], "Pervasive", category[[1]])
  if (exists(index, errorFrame)) {
    errorFrame[[index]] <- rbind(errorFrame[[index]], summaryOfErrors)
  } else errorFrame[[index]] <- summaryOfErrors   
  
  return(errorFrame)
}

addGeneralError <- function(groupVar, 
                            errorFrame, field, tableName, errorType, errorCode, 
                            severity, message, quantity = 1){
  newErrors <- structure(
    list(
      "All", # defGroupVar (or should this be NA)
      "All", # GROUP
      tableName, # table
      field, # error_field
      errorType, # error
      errorType, # category
      severity, # severity
      errorCode, # errorCode
      message[[1]], # description
      quantity # quantity
    ),
    names = c(defGroupVar, "GROUP", "table", "error_field", "error",
              "category", "severity", "errorCode", "description", "quantity")
  )
  
  if (groupVar == defGroupVar){
    newErrors$GROUP <- NULL
  } else names(newErrors)[[2]] <- groupVar
  
  index <- paste0(tableName, field, errorType, "general")
  if (exists(index, errorFrame)) {
    errorFrame[[index]] <- rbind(errorFrame[[index]], as_tibble(newErrors))
  } else errorFrame[[index]] <- as_tibble(newErrors )
  
  return(errorFrame)
}


addToErrorFrame <- function(indexTable, groupVar, errorFrame, table, field, tableName, errorType, 
                            errorCode, severity, message, ...){ 
  idList <- tableIDField[[tableName]]
  idColumns <- c()
  for (i in 1:length(idList)){
    idColumns <- c(idColumns, idFieldNames[[i]], idValueNames[[i]]) # idFieldNames, idValueNames in definitions.R
  }
  
  groupColumns <- unique(c(defGroupVar, groupVar))
  
  columnNames <- c(groupColumns, "table", idColumns, minimumErrorDetail, names(list(...))) #minimumErrorDetail in definitions.R
  numErrorRows <- nrow(table)
  print(field)
  if (numErrorRows > tooManyOfSameErrorType){
    print("inside too many errors")
    groupColumnsInTable <- intersect(groupColumns, names(table))
    if (is_empty(groupColumnsInTable)){
      summaryOfErrors <- table %>% select(!!rlang::sym(idList[[1]]))
    } else {
      summaryOfErrors <- table[, c(idList[[1]], groupColumnsInTable)]
    }
    print("making sure program included")
    if (!groupVar %in% names(summaryOfErrors)){
      summaryOfErrors[[groupVar]] <- findIt(summaryOfErrors, groupVar, indexTable = indexTable)
    }
    if (!defGroupVar %in% names(summaryOfErrors)){
      summaryOfErrors[[defGroupVar]] <- findIt(summaryOfErrors, defGroupVar, indexTable = indexTable)
    }
    
    summaryOfErrors <- summaryOfErrors %>%  
      group_by(!! rlang::sym(groupVar)) %>% 
      summarise(quantity = n()) %>% ungroup()
    summaryOfErrors[["error"]] <- paste0("Example: ", table[1, field])
    if (length(message) > 1){
      message <- message[[1]]
    }
    print("adding pervasive error")
    errorFrame <- addPervasiveToErrorFrame(
      groupVar = groupVar,
      errorFrame = errorFrame,
      summaryOfErrors = summaryOfErrors,
      tableName = tableName,
      category = errorType,
      fieldName = field,
      message = message[[1]],
      severity = severity,
      errorCode = errorCode
    )
    return(errorFrame)
  }
  # otherwise, add one error row per error in errorFrame
  lastErrorCount <- isolate(errorCount())
  newErrorCount <- lastErrorCount + numErrorRows
  errorCount(newErrorCount)
  
  lastErrorRowCount <- isolate(errorRows())
  
  if (isolate(errorRows()) > errorLimit) {
    errorExcess(TRUE)
  }
  
  # Otherwise, add one error row per error record:
  newErrorRowCount <- lastErrorRowCount + numErrorRows
  errorRows(newErrorRowCount)
  
  newErrors <- setNames(data.frame(matrix(nrow = numErrorRows, ncol = length(columnNames))),
                        columnNames)
  newErrors[,] <- ""
  
  newErrors$table <- rep(tableName, numErrorRows)
  for (index in seq_along(idList)){
    idVariable <- idList[[index]]
    idField <- idFieldNames[[index]] #idFieldNames in definitons.R
    idValue <- as.character(idValueNames[[index]])
    newErrors[[idField]] <- idVariable
    newErrors[[idValue]] <- as.character(get(idVariable, table))
  }
  
  # if defGroupVar and/or groupVar already exist in error table, add to frame
  # Otherwise, will be added later to full dataframe
  newErrors[[defGroupVar]] <- findIt(table, defGroupVar, indexTable = indexTable)
  if (groupVar != defGroupVar){
    newErrors[[groupVar]] <- findIt(table, groupVar, indexTable = indexTable)
  }
  
  newErrors$error_field <- field
  newErrors$error <- as.character(get(field, table))
  newErrors$category <- errorType
  newErrors[, names(list(...))] <- list(...)
  newErrors$severity <- severity
  newErrors$description <- message
  newErrors$errorCode <- errorCode
  # if the error already includes a count of error records, use that quantity
  if ("quantity" %in% names(table)){
    newErrors$quantity <- table$quantity
  } else {
    newErrors$quantity <- 1
  }
  
  index <- paste0(tableName, field, errorType)
  if (exists(index, errorFrame)) {
    index <- paste0(index, sample(1:100, 1)) # make this entry unique
    # errorFrame[[index]] <- rbind(errorFrame[[index]], newErrors)
  } 
  errorFrame[[index]] <- newErrors

  print("done adding error")
  return(errorFrame)
}


# blankTables adds information about tables with no records to the errorFrame------
blankTables <- function(groupVar, errorFrame, blankTableList){
  if (is.null(blankTableList)) return(errorFrame)
  for (tableName in blankTableList){
    errorFrame <- addGeneralError(groupVar, 
                                  errorFrame, field = "all fields", tableName = tableName, 
                                  errorType = "No records in table", 
                                  errorCode = "3",
                                  severity = "Warning", 
                                  message = paste0(tableName, " has no data."))
  }
  return(errorFrame)
}


# checkCodedVariables: compare all coded fields with valid codes for that variable. Invalid codes = Error----
checkCodedVariables <- function(errorFrame, resources){
  severity <- "Error"
  groupBy <- resources$finalGroupChoice
  for (tableName in resources$tablesAndVariables$tablesToCheck){
    formattedTable <- get(tableName, resources$formattedTables)
    table <- get(tableName, resources$uploadedTables)
    variablesInTable <- names(resources$formattedTables[[tableName]])
    
    codedFieldNames <- intersect(variablesInTable,
                                 findVariablesMatchingCondition(tableName, tableDef, "has_codes","Y"))
    if (length(codedFieldNames)==0) next
    
    for (codedField in codedFieldNames){
      codeIndex <- as.numeric(tableDef[[tableName]]$variables[[codedField]]$code_list_ref)
      # if codes are not in sequential order, need character for codelist reference
      codeList <- codes[[as.character(codeIndex)]]
      validCodes <- names(codeList)

      # all invalid codes should be indicated as invalid code in formattedTable
      badCodeIndices <- formattedTable[[codedField]] == "Invalid Code"
      if (any(badCodeIndices, na.rm = TRUE)){
        indices <- formattedTable$recordIndex[which(badCodeIndices)]
        badCodesRecords <- table[indices, unique(c(tableIDField[[tableName]], codedField))]
        badCodesRecords[[groupBy]] <- formattedTable[which(badCodeIndices), groupBy]
        
        if (length(validCodes) > maxCodesToShow){
          message <- paste0("This code is not found in the DES for ", codedField, ".")
        }
        else message <- paste0("This code is not found in the DES for ", codedField, ".",
                               " Valid codes for ", codedField, " are ",
                               combine_words(names(codeList)),
                               ".")
        
        # if number of bad codes is not excessive, add to errorFrame as usual
        if (nrow(badCodesRecords) < limitOnInvalidCodesToRecord){
          errorFrame <- addToErrorFrame(
            resources$formattedTables[[indexTableName]],
            resources$finalGroupChoice, 
            errorFrame, badCodesRecords, codedField, tableName, 
            errorType = "Invalid Code", 
            errorCode = "1.6", severity, message)
        } else {
          # otherwise, too many bad codes to write individual records to errorFrame for each one
          
          # are some of the individual unique codes not excessive? Write them as usual
          nonExcessiveBadCodes <- badCodesRecords %>% 
            group_by_at(vars(!! rlang::sym(codedField))) %>% 
            mutate(count = n()) %>% ungroup() %>% 
            filter(count <= limitOnInvalidCodesToRecord) %>% select(-count)
          
          if (nrow(nonExcessiveBadCodes) > 0){
            errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                          groupVar = groupBy,
                                          errorFrame = errorFrame,
                                          table = nonExcessiveBadCodes,
                                          field = codedField,
                                          tableName = tableName,
                                          errorType = "Invalid Code",
                                          errorCode = "1.6", severity, message)
          }
          
          # now, check to see if there are pervasive invalid codes;
          # if so, no need to document each individually but by group instead:
          # 
          pervasiveBadCodes <- badCodesRecords %>% group_by_at(vars(!! rlang::sym(codedField))) %>% 
            mutate(count = n()) %>% ungroup() %>% 
            filter(count >limitOnInvalidCodesToRecord) %>% 
            group_by_at(vars(!! rlang::sym(codedField), (!! rlang::sym(groupBy)))) %>% 
            summarise(quantity = n()) %>% ungroup() %>% 
            rename(error = !! rlang::sym(codedField))
          
          if (nrow(pervasiveBadCodes) > 0){
            errorFrame <- addPervasiveToErrorFrame(
              groupVar = groupBy,
              errorFrame = errorFrame, 
              summaryOfErrors = pervasiveBadCodes,
              tableName = tableName, 
              category = "Invalid Code",
              fieldName = codedField,
              message = message,
              severity = "Error", errorCode = "1.6")
          }
        }
      }
    }
  }
  return(errorFrame)
}



checkValidRange <- function(errorFrame, groupVar, tableName, formattedTable, numericField, severity, resources){
  limits <- numericLimits[[numericField]]
  #Check for values greater than upper limit that aren't == value for unknown
  if (!is.null(limits$unknown)){
    formattedTable <- formattedTable %>% filter((.)[numericField] != limits$unknown)
  }
  tooHighRecords <- formattedTable %>% filter((.)[numericField] >limits$upper)
  if ((numericField == heightVar) && (nrow(tooHighRecords) > 0.9*nrow(formattedTable))){
    message <- paste0(heightVar, " should be reported in meters. Your ", heightVar, " values are greater than ", limits$upper, " and appear to be in cm or inches.")
    errorFrame <- addGeneralError(groupVar,
                                  errorFrame = errorFrame, 
                                  field = numericField, tableName = tableName, 
                                  errorType = "Value Above Expected Range",
                                  errorCode = "2.2c", severity = "Error",
                                  message = message, 
                                  quantity = nrow(tooHighRecords))
    
    return(errorFrame)
    
  }
  if (nrow(tooHighRecords) > 0){
    message <- paste0("The maximum value expected for ", numericField, " is ",limits$upper, limits$units, ".")
    if(exists("unknown",limits)){
      message <- paste0(message, " Note: The code for Unknown is ",limits$unknown, ".")
    }
    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  groupVar, errorFrame, tooHighRecords, numericField, tableName, 
                                  "Value Above Expected Range",errorCode = "2.2c", severity, message)
  }
  tooLowRecords <- formattedTable %>% filter((.)[numericField] < limits$lower)
  if (nrow(tooLowRecords) > 0){
    message <- paste0("The minimum value expected for ", numericField, " is ",limits$lower, limits$units, ".")
    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  groupVar, errorFrame, tooLowRecords, numericField, tableName, 
                                  "Value Below Expected Range",errorCode = "2.2c", severity, message)
  }
  return(errorFrame)
}

checkNumericValues <- function(errorFrame, resources){
  for(tableName in resources$tablesAndVariables$tablesToCheck){
    table <- get(tableName, resources$uploadedTables)
    formattedTable <- get(tableName, resources$formattedTables)
    variablesInTable <- names(formattedTable)
    numericFieldNames <- intersect(variablesInTable, findVariablesMatchingCondition(tableName, tableDef, "data_format","Numeric"))
    codedFieldNames <- intersect(variablesInTable, findVariablesMatchingCondition(tableName, tableDef, "has_codes","Y"))
    numericFieldNames <- numericFieldNames[!numericFieldNames %in% codedFieldNames ]
    
    if (is_empty(numericFieldNames)) next
    
    for (numericField in numericFieldNames){
      #non-numeric entries should be "NA" in formattedTable
      potentialBadRecords <- is.na(formattedTable[[numericField]])
      if (any(potentialBadRecords)){
        table[which(potentialBadRecords), numericField] <- trimws(table[which(potentialBadRecords), numericField])
        #find non-numeric fields that aren't blank fields in original table
        badRecords <- table[which(potentialBadRecords),] %>% 
          filter(!(.)[numericField]=="") %>% filter(!is.na((.)[numericField]))
        if (nrow(badRecords)>0){
          message <- paste0("Numeric value required for ", numericField)
          severity <- "Error"
          errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                        resources$finalGroupChoice, errorFrame, 
                                        badRecords, numericField, tableName, 
                                        "Invalid format",
                                        errorCode = "1.5c",
                                        severity, message)
        }
      } 
      
      #check to see if numeric values are within a valid range
      if (numericField %in% names(numericLimits)){
        errorFrame <- checkValidRange(errorFrame, resources$finalGroupChoice, tableName, formattedTable[!potentialBadRecords,], numericField, "Warning", resources)
      }
      # if excessive numeric range errors like heightVar all in cm then break *both* loops **JUDY could this happen now?
      if (is.null(errorFrame)) break
    }
    if (is.null(errorFrame)) break
  }
  return(errorFrame)
}



# findMissingRequiredValues adds an error to errorFrame for missing required variables only
findMissingRequiredValues <- function(errorFrame, resources){
  groupBy <- resources$finalGroupChoice
  for (tableName in resources$tablesAndVariables$tablesToCheck){
    print(paste0("checking missing in ",tableName))
    variablesInTable <- names(resources$formattedTables[[tableName]])
    uploadedTable <- get(tableName, resources$uploadedTables)
    # we know that columns exist for all required variables because can't proceed beyond upload if
    # required column missing
    requiredVariables <- findVariablesMatchingCondition(tableName, tableDef, "variable_required", "1")
    
    for (fieldName in requiredVariables){
      if (networkName == "IeDEA"){
         if (fieldName %in% requiredAtBaseline)  next # in specific definitions, only required at baseline SRN
      }

      print(paste0("checking missing ", fieldName, Sys.time()))
      
      # if this is a coded variable, missing entries are labeled "Missing", otherwise, check for blank or NA
      if (tableDef[[tableName]][["variables"]][[fieldName]]$has_codes == "Y"){
        badRecords <- resources$formattedTables[[tableName]][[fieldName]] == "Missing"
      } else badRecords <- is_blank_or_NA_elements(uploadedTable[[fieldName]])
      
      # if this is a primary key for this table, it is listed first in the required variables
      # and is error 1.1a. Missing defGroupVar in indexTableName is 1.1c.
      # Other missing required key variables are error 1.4a
      if (fieldName == requiredVariables[[1]]){
        errorCode <- "1.1a"
        severity <- "Critical"
      } else if (fieldName == defGroupVar && tableName == indexTableName){
        errorCode <- "1.1c"
        severity <- "Critical"
      } else {
        errorCode <- "1.3"
        severity <- "Error"
      }
      
      print(paste0("finished finding bad ", Sys.time()))
      # if the whole column is blank/NA, add as pervasive error
      if (all(badRecords)){
        summaryForErrorFrame <- resources$formattedTables[[tableName]] %>% 
          select(!! rlang::sym(groupBy)) %>% 
          group_by_all() %>% 
          summarise(quantity = n()) %>% ungroup() %>% 
          mutate(error = "Missing")
        errorFrame <- addPervasiveToErrorFrame(
          groupVar = groupBy,
          errorFrame = errorFrame, 
          summaryOfErrors = summaryForErrorFrame,
          tableName = tableName, 
          category = "Missing Required Variable",
          fieldName = fieldName,
          message = paste0(fieldName," is a required variable in this table. This column should not be blank."),
          severity = severity, 
          errorCode = errorCode)
      }
      else if (any(badRecords)){
        badRecords <- which(badRecords)
        print("found missing")
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      resources$finalGroupChoice, errorFrame, 
                                      resources$formattedTables[[tableName]][badRecords,], 
                                      fieldName, tableName,
                                      "Missing Required Variable", 
                                      errorCode = errorCode, 
                                      severity = severity, 
                                      paste0(fieldName,
                                             " is a required variable in this table and should not be blank."))
      }
    }
  }
  return(errorFrame)
}

# addDeprecatedWarning ---------------------------------------------------------
# add warning details for deprecated variables in dataset
# 
addDeprecatedWarning <- function(errorFrame, resources){
  deprecated <- resources$tablesAndVariables$details$deprecated_list
  if (is_empty(deprecated)){
    return(errorFrame)
  }
  groupVar <- resources$finalGroupChoice
  for (tableName in names(deprecated)){
    deprecatedVars <- deprecated[[tableName]]
    for (variableName in deprecatedVars){
      varInfo <- tableDef[[tableName]][["variables"]][[variableName]]
      depDate <- varInfo$variable_deprecated_d
      message <- paste(
        variableName, "in", 
        tableName, "was deprecated on",
        paste0(depDate,".")
      )
      if (!is.null(varInfo$variable_replacedby)){
        #attempt to extract new table and variable
        replacement <- strsplit(varInfo$variable_replacedby, ":")[[1]]
        # if somehow there's no table:variable in string, ignore
        if (length(replacement) != 2) next
        
        newTableIndex <- replacement[[1]]
        newTableName <- findTableByIndex(newTableIndex)
        if (is.null(newTableName)) next
        newVariableIndex <- replacement[[2]]
        newVariableName <- findVariablesMatchingCondition(tableName, tableDef, "redcapIndex", newVariableIndex)
        if (is_empty(newVariableName)) next
        message <- paste(message, "Please use", newVariableName, "found in", newTableName, "instead.")
      }
      if (!is.null(varInfo$variable_deprecatedinfo)){
        message <- paste(message, varInfo$variable_deprecatedinfo)
      }
      if ( ((Sys.Date() - as.Date(depDate))/365.25) > 1 ){
        errorType <- "Deprecated variable"
        errorCode <- "1.2b"
        severity <- "Error"
        message <- paste0(message, "This variable was deprecated over a year ago.")
      } else {
        errorType <- "Recently deprecated variable"
        errorCode <- "1.2a"
        severity <- "Warning"
      }
      errorFrame <- addGeneralError(groupVar, 
                                    errorFrame, 
                                    field = variableName, 
                                    tableName = tableName, 
                                    errorType = errorType, 
                                    errorCode = errorCode,
                                    severity = severity, 
                                    message = message,
                                    quantity = 1
      )
    }
  }
  return(errorFrame)
}

#Summarize the number of missing entries in the missingByGroup frame (which already excludes Unknown)
summarizeMissing <- function(frame){
  if (is_empty(frame)) return(frame)
  if (nrow(frame) == 0) return(frame)
  summary <- frame %>% 
    group_by(table, variable, category) %>% summarise(number = sum(number)) %>% 
    mutate(percent = round(100*number/nrow(formattedTables()[[table]]),1)) %>% 
    mutate(category = "Missing")
  return(summary)
}

#summarize the number of missing entries by Program and variableName (exclude unknown/invalid group)
summarizeMissingByGroup <- function(groupVar, errorFrame, resources, summaryFrames){
  rowsByGroup <- rbindlist(resources$tableRowsByGroup, use.names = TRUE, idcol = "table")
  # tableRows <- unlist(lapply(resources$formattedTables, nrow))
  missingSummary <- list()
  # errorFrame already documents missing *required* variables; summarize them here if they exist:
  if (!is_empty(errorFrame) && ("Missing Required Variable" %in% errorFrame$category)){
    missingSummary[["required"]] <- errorFrame %>% filter(category == "Missing Required Variable") %>% 
      filter(!(!! rlang::sym(groupVar) %in% c("Unknown", "Invalid")) ) %>% 
      group_by_at(vars((!! rlang::sym(groupVar)), table, error_field)) %>% 
      summarise(number = sum(quantity)) %>% ungroup() %>%
      left_join(rowsByGroup, by = c("table", groupVar)) %>% 
      mutate(percent = round(100*number/numRows, 1)) %>% 
      mutate(category = "Missing") %>% rename(variable = error_field) %>% 
      select(-numRows)
  }
  
  # now create summary table of interesting variables + variables with flagged missing_action in REDCap
  for (tableName in resources$tablesAndVariables$tablesToCheck){
    groupRows <- resources$tableRowsByGroup[[tableName]] %>% 
      filter(numRows > 0)
    if (is_empty(groupRows)) next
    print(paste0("checking missing in ",tableName))
    variablesInTable <- names(resources$formattedTables[[tableName]])
    uploadedTable <- get(tableName, resources$uploadedTables)
    requiredVariables <- intersect(variablesInTable,
                                   findVariablesMatchingCondition(tableName, tableDef, "variable_required", "1"))
    variablesToTrack <- intersect(variablesInTable,
                                  findVariablesMatchingCondition(tableName, tableDef, "variable_missingaction", "1"))
    
    varsToCheck <- c(variablesToTrack, intersect(variablesInTable, interesting))
    # if it's required, missing values trigger errors
    varsToCheck <- varsToCheck[!varsToCheck %in% requiredVariables]
    for (fieldName in varsToCheck){
      print(paste0("checking missing ", fieldName, Sys.time()))
      
      # if this is a coded variable, missing entries are labeled "Missing", otherwise, check for blank or NA
      if (tableDef[[tableName]][["variables"]][[fieldName]]$has_codes == "Y"){
        badRecords <- resources$formattedTables[[tableName]][[fieldName]] == "Missing"
      } else {
        badRecords <- is_blank_or_NA_elements(uploadedTable[[fieldName]])
      }
      
      if (any(badRecords)){
        missingSummary[[paste0(tableName,fieldName)]] <- 
          resources$formattedTables[[tableName]][which(badRecords), ] %>% 
          select(!! rlang::sym(groupVar)) %>% 
          filter(!(!! rlang::sym(groupVar) %in% c("Unknown", "Invalid")) ) %>% 
          group_by_all() %>% summarise(number = n()) %>% ungroup() %>% 
          mutate(table = tableName) %>% 
          mutate(variable = fieldName) %>% 
          left_join(groupRows, by = groupVar) %>% 
          mutate(percent = round(100*number/numRows, 1)) %>% 
          select(-numRows) %>% 
          mutate(category = "Missing") %>% 
          select(!! rlang::sym(groupVar), table, variable, everything())
      }
    }
  }
  missingSummary <- rbindlist(missingSummary)
  return(missingSummary)
}


summarizeUnknownCodesByGroup <- function(groupVar, resources){  ## do this with formattedTables after program added to every table
  unknownCodesByGroup <- list()
  allTableNames <- resources$tablesAndVariables$tablesToCheck
  for (tableName in allTableNames){
    if (!exists(groupVar, resources$formattedTables[[tableName]])) next
    numRowsByGroup <- resources$tableRowsByGroup[[tableName]]
    table <- resources$formattedTables[[tableName]] %>% 
      filter( !(!!rlang::sym(groupVar)) %in% c("Unknown", "Invalid") )
    if (nrow(table) == 0) next
    variablesInTable <- names(table)
    codedFieldNames <- intersect(variablesInTable,
                                 findVariablesMatchingCondition(tableName, 
                                                                tableDef, 
                                                                "has_codes","Y"))
    variablesWorthCheckingForUnknown <- codedFieldNames[(
      (!codedFieldNames %in% codesThatCanBeBlank) &
        !endsWith(codedFieldNames,"_RS") &
        !endsWith(codedFieldNames,"_A"))]
    for (codedField in variablesWorthCheckingForUnknown) {
      if (any(tolower(table[[codedField]])=="unknown", na.rm = TRUE)){
        unknownCodesByGroup[[codedField]] <- table %>% 
          filter(!!rlang::sym(codedField) == "Unknown") %>% 
          group_by(!! rlang::sym(groupVar)) %>% summarise(number = n()) %>% 
          ungroup() %>% 
          left_join(numRowsByGroup, by = groupVar) %>% 
          mutate(table = tableName,
                 variable = codedField,
                 category = "Unknown", 
                 percent = round(100*number/numRows, 1)) %>% 
          select(-numRows) %>% 
          select(!! rlang::sym(groupVar), table, variable, everything())
      }
    }
  }
  unknownCodesByGroup <- rbindlist(unknownCodesByGroup, use.names = TRUE, fill = TRUE)
  return(unknownCodesByGroup)
}


summarizeUnknownCodes <- function(unknownByGroup){  
  if (nrow(unknownByGroup)==0){
    return(list())
  }
  unknownSummary <- unknownByGroup %>% 
    group_by_at(vars("table","variable","category")) %>% 
    summarise(num=sum(number)) %>% 
    mutate(perc = round(100*num/nrow(formattedTables()[[table]]),1)) %>% 
    rename(number = num, percent= perc)
  return(unknownSummary)
}

PatientIDChecks <- function(errorFrame, resources){
  validPatients <- resources$formattedTables[[indexTableName]][[patientVar]]
  for (tableName in resources$tablesAndVariables$tablesToCheckWithPatientID){
    currentTable <- resources$formattedTables[[tableName]]
    badRecords <- !(currentTable[,patientVar] %in% validPatients)
    if (any(badRecords[!is.na(badRecords)])){
      # missing patientVar id is detected by a different check so ignore blank or NA
      badRecordTable <- currentTable[which(badRecords),] %>% 
        filter(!is_blank_or_NA_elements(!!patientVarSym))
      # now check to see if any remaining records with invalid patient id
      if (nrow(badRecordTable) == 0) next
      errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                    resources$finalGroupChoice, errorFrame, 
                                    badRecordTable, patientVar, tableName,
                                    paste0("Invalid ", patientVar, " ID"), 
                                    errorCode = "1.1b", 
                                    "Critical", 
                                    paste0("No record was found for this ", 
                                           patientVar, " in ", indexTableName, 
                                           ". Every ", patientVar, 
                                           " should have an entry in ", 
                                           indexTableName, "."))
    }
  }
  return(errorFrame)
}

duplicateRecordChecks <- function(errorFrame, resources){
  groupVar <- resources$finalGroupChoice
  for (tableName in resources$tablesAndVariables$tablesToCheck[!(resources$tablesAndVariables$tablesToCheck %in% duplicateRecordExceptions)]){
    print(paste0("checking for duplicates in ", tableName))
    idFields <- tableIDField[[tableName]]
    # if there is only one identifier in this table, doesn't work to use [, idFields] and keep
    # name of column. If more than one, works fine
    if (length(idFields) == 1){
      table <- tibble(resources$uploadedTables[[tableName]][idFields])
    } else {
      table <- tibble(resources$uploadedTables[[tableName]][, idFields])
    }
    
    if (uniqueN(table) == nrow(table)){
      # this means there are no duplicate combinations of key identifiers
      next
    }

    print(paste0("before dupRows", Sys.time()))
    # quantity = the number of rows that are DUPLICATES of a previous row - 
    # subtract the original row from the count
    duplicates <- table %>% group_by_all() %>% summarise(quantity = n() - 1) %>%
      ungroup() %>% filter(quantity > 0)
    print(paste0("after dupRows", Sys.time()))
    
    if (nrow(duplicates) == 0) { 
      print("IT IS POSSIBLE TO GET HERE")
      next
    }
    print("Duplicates found")
    print(nrow(duplicates))
    # otherwise, we know there are duplicates
    # check first for tables with patientVar as unique identifier; a duplicate record in that case 
    # is an error that requires explanation
    if ((length(idFields) == 1) && (idFields[[1]] == patientVar)){
      errorCode <- "1.1d"
      message <- paste0(patientVar, " is the only identifier in this table; every ", patientVar, " value should be unique")
      severity <- "Critical"
    } else {
      errorCode <- "1.8"
      message <- paste(
        "This record has key identifier", makeItPluralOrNot("value", length(idFields)), 
        paste0("(", paste(idFields, collapse = ", "), ")"),
        "that duplicate other record(s) in this table.")
      severity <- "Error"
    }
    
    print("about to add duplicates to error frame")

    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  resources$finalGroupChoice, errorFrame, 
                                  duplicates, 
                                  idFields[1], tableName, 
                                  "Duplicate Record", 
                                  errorCode = errorCode, 
                                  severity = severity, 
                                  message)
    print("finished adding duplicates ")
  }
  return(errorFrame)
}



checkForDecreasingHeight <- function(errorFrame, resources) {
  
  if (!heightVar %in% names(numericLimits)){
    return(errorFrame)
  }
  
  if (!heightTableName %in% names(resources$formattedTables)){
    return(errorFrame)
  }
  
  if (!heightVar %in% resources$tablesAndVariables$matchingColumns[[heightTableName]]){
    return(errorFrame)
  }
  
  if (!heightDateVar %in% resources$tablesAndVariables$matchingColumns[[heightTableName]]){
    return(errorFrame)
  }
  
  heightTable <- resources$formattedTables[[heightTableName]]
  
  heightTable <- heightTable %>% filter(!is.na(!!heightVarSym)) %>% 
    filter(!!heightVarSym != numericLimits[[heightVar]]$unknown) %>% 
    filter(!!heightVarSym !="") %>% filter(!!heightVarSym < numericLimits[[heightVar]]$upper) #only include valid heights
  
  temp <- arrange(heightTable, !!rlang::sym(heightDateVarSym)) %>%  
    arrange(!!patientVarSym) %>% 
    group_by(!!patientVarSym) %>% mutate(checkIt = c(0,diff(!!heightVarSym))) 
  
  if (nrow(temp) == 0){
    return(errorFrame)
  }
  
  potentialBadRecords <- (temp$checkIt < -1*maxHeightDecreaseInM)
  
  if (any(potentialBadRecords)) {
    badRecords <- which(potentialBadRecords)
    message <- paste("Height should not decrease significantly over time. The height on this date was less than on the previous date. On ",
                     temp[[heightDateVar]][badRecords-1]," this patient's height was ", temp[[heightVar]][badRecords-1], "m.")
    
    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  resources$finalGroupChoice, errorFrame, temp[badRecords,], heightVar, 
                                  heightTableName, "Conflicting Height Data",
                                  errorCode = "2.3f", "Warning", message)
  }
  return(errorFrame)
}






findTableRows <- function(tableNameVector){
  tableNames <- unique(tableNameVector)
  tableRows <- lapply(formattedTables()[tableNames],function(x){return(nrow(x))})
  numberOfRows <- tableRows[tableNameVector]
  numberOfRows <- unlist(numberOfRows, use.names = FALSE)
  return(numberOfRows)
}

invalidCodeStatement <- function(numUniqueCodes, codeValue){
  # if number of unique codes is excessive, replace with message about unique codes
  ifelse(numUniqueCodes > limitOnInvalidCodesToShow, 
         paste0(numUniqueCodes," unique invalid codes"),
         codeValue)
}

# Summarize the errors, in anticipation of report
summarizeErrors <- function(errorFrame, tableData){
  # if there are no errors:
  if (is_empty(errorFrame)){
    badCodeSummary <- NULL
    summaryFrame <- list()
    errorOnly <- list()
    warnOnly <- list()
    highLevelSummary <- list()
    summaryFrameWithCodes <- list()
  } else {
    # we know there are some errors or warnings
    # are there any invalid codes?
    if (!"Invalid Code" %in% errorFrame$category) badCodeSummary <- NULL
    else {
      # if bad codes exist in errorFrame, summarize them:
      badCodeSummary <- errorFrame %>% filter(category == "Invalid Code") %>% 
        group_by(table, error_field, error) %>% summarise(Count = sum(quantity)) %>%  
        group_by(table, error_field) %>%   mutate(uniquecodes = n()) %>% ungroup() %>%
        mutate("InvalidCode" = invalidCodeStatement(uniquecodes, error)) %>% 
        group_by(table, error_field, InvalidCode) %>% summarise(Count = sum(Count)) %>% 
        ungroup() %>% mutate(tableRows = findTableRows(table)) %>% 
        mutate(Percent = round(100*Count/tableRows, 1)) %>% 
        select(-tableRows) %>% 
        rename(Table = table, Variable = error_field) %>% 
        select(Table, Variable, InvalidCode, Count, Percent)
    }
    summaryFrameWithCodes <- errorFrame %>% 
      group_by(errorCode, category, table, error_field, severity) %>% 
      summarise(number = sum(quantity)) %>%
      mutate(numRows = nrow(tableData[[table]])) %>% 
      # for date logic errors, one bad date triggers multiple errors, 
      # so cap at number of records in table:
      mutate(number = pmin(number, numRows)) %>% 
      mutate(percent = round(100*number/numRows, 1)) %>% 
      select(-numRows) %>% 
      rename(variable = error_field) %>% 
      # put in table order and variable order
      mutate(tableOrder = as.numeric(tableDef[[table]][["table_order"]])) %>% 
      mutate(varOrder = as.numeric(tableDef[[table]][["variables"]][[variable]][["variable_order"]])) %>%
      arrange(tableOrder, varOrder) %>% 
      select(-varOrder, -tableOrder) %>% ungroup()
    
    summaryFrame <- summaryFrameWithCodes %>% select(-errorCode)
    
    errorOnly <- summaryFrame %>% filter(severity %in% c("Critical", "Error")) %>% select(-severity) %>% 
      rename(Error = "category", Table = "table", Variable = "variable", Count = "number", Percent = "percent")
    warnOnly <- summaryFrame %>% filter(severity == "Warning") %>% select(-severity) %>% 
      rename(Error = "category", Table = "table", Variable = "variable", Count = "number", Percent = "percent")
    highLevelSummary <- errorFrame %>% group_by(errorCode) %>% summarise(Count = n())
  }
  
  return(list("summaryFrame" = summaryFrame, 
              "badCodeFrame" = badCodeSummary,
              "errorOnlySummary" = errorOnly,
              "warnOnlySummary" = warnOnly,
              "highLevelSummary" = highLevelSummary,
              "summaryFrameWithCodes" = summaryFrameWithCodes))
}




invalidProgram <- function(errorFrame, resources){
  ### JUDY add check for PROGRAM in tblCENTER
  if ( (defGroupVar %in% names(resources$uploadedTables[[indexTableName]]))
       & (defGroupTableName %in% resources$tablesAndVariables$tablesToCheck) ){
    # if you get to this point there is at least one complete entry for defGroupVar in defGroupTable but to be on the safe side...
    validPrograms <- na.omit(unique(resources$formattedTables[[defGroupTableName]][[defGroupVar]]))
    if (length(validPrograms) == 0){
      return(errorFrame)
    }
    badPrograms <- resources$uploadedTables[[indexTableName]] %>% 
      select(!!patientVarSym, defGroupVarSym) %>% 
      filter(!(!!defGroupVarSym %in% validPrograms)) 
    if (nrow(badPrograms) > 0){
      message <- paste0("There is no entry for ", defGroupVar, " = ", badPrograms[[defGroupVar]],
                        " in ", defGroupTableName, ".")
      errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                    resources$finalGroupChoice, errorFrame, badPrograms, 
                                    defGroupVar, indexTableName, paste0("Invalid ", defGroupVar), 
                                    errorCode = "1.7", "Error", message)
    }
  }
  return(errorFrame)
}


divideErrorsByTable <- function(errorFrame){
  tablesWithErrors <- sort(unique(errorFrame$table))
  errorFrameByTable <- 
    lapply(tablesWithErrors, function(tableName){
      thisTableErrors <- errorFrame %>% filter(table==tableName) %>% select_if(function(col){any(col != "", na.rm = TRUE)})
      return(thisTableErrors)
    })
  names(errorFrameByTable) <- tablesWithErrors
  return(errorFrameByTable)
}

# function to create patient appearance summary data frame
initializeAppearanceSummary <- function(groupVar, programNames,tableNames){
  rows <- length(programNames)* length(tableNames)
  x <- data.frame(
    "GROUP" = character(rows),
    "table" = character(rows),
    "number" = numeric(rows),
    "percent" = numeric(rows),
    stringsAsFactors = FALSE)
  names(x)[[1]] <- groupVar
  return(x)
}

# Create a table of the number of VALID patients included in each table
findPatients <- function(formattedTables, tablesToCheckWithPatientID, groupNames, groupVar){
  
  if (length(tablesToCheckWithPatientID) == 0) return(NULL)
  
  cat("Session:", isolate(sessionID())," checking for patient appearance","\n", file = stderr())
  cat("Session:", isolate(sessionID())," checking group names","\n", file = stderr())  
  # exclude Missing because these patients are not assigned to a group
  groupNames <- groupNames[!groupNames %in% c("Missing")]
  
  if (length(groupNames) == 0) return(NULL)
  
  # now we know there is at least one table to examine and at least one 
  # patient group
  
  appearanceSummary <- initializeAppearanceSummary(groupVar, groupNames, tablesToCheckWithPatientID)
  row <- 0
  for (groupName in groupNames){
    patientsInGroup <- formattedTables[[indexTableName]][[patientVar]][formattedTables[[indexTableName]][[groupVar]] == groupName]
    numberOfPatientsInGroup <- length(patientsInGroup)
    for (tableName in tablesToCheckWithPatientID){
      row <- row + 1
      appearanceSummary[row, groupVar] <- groupName
      appearanceSummary[row, "table"] <- tableName
      patientIDsInTable <- sum((patientsInGroup %in% formattedTables[[tableName]][[patientVar]]), na.rm = TRUE)
      percent <-  round(100*patientIDsInTable/numberOfPatientsInGroup,1) 
      appearanceSummary[row, "number"] <- patientIDsInTable
      appearanceSummary[row, "percent"] <- percent
    }
  }
  # just in case there were no groups, etc
  if (nrow(appearanceSummary) == 0){
    appearanceSummary <- NULL
  }
  
  cat("Session:", isolate(sessionID())," appearance summary complete","\n", file = stderr())  
  
  return(appearanceSummary)
}


sumThem <- function(x,y){
  out <- rowSums(data.frame(x,y), na.rm=TRUE)
  return(out)
}


combineMissingAndUnknownByGroup <- function(missing, unknown, groupVar){
  if (is_empty(missing) || is_empty(unknown)) return(NULL)
  summary <- full_join(missing, unknown, by=c(groupVar,"table","variable")) %>% 
    mutate(category = "missingOrUnknown") %>% 
    mutate(number = sumThem(number.x,number.y)) %>% 
    mutate(percent = sumThem(percent.x,percent.y)) %>% 
    select(-ends_with(".x")) %>% select(-ends_with(".y")) %>% 
    filter(variable %in% c(interesting,allRequiredVariables)) %>% 
    filter(!!rlang::sym(groupVar) != "Missing")
  
  return(summary)
}

combineMissingAndUnknown <- function(missing, unknown){ 
  if (is_empty(missing) || is_empty(unknown)) return(NULL)
  combined <- merge(missing, unknown,by=c("table","variable"), suffixes = c(".x",".y"), all = TRUE)
  combined$category.x[is.na(combined$category.x)] <- ""
  combined$category.y[is.na(combined$category.y)] <- ""
  summary <- combined[,c("table","variable")]
  summary$category <- trimws(paste(combined$category.x,combined$category.y))
  summary$category[nchar(summary$category) > 8] <- "Missing or Unknown"
  summary$number <- as.integer(rowSums(combined[,c("number.x", "number.y")], na.rm = TRUE))
  summary$percent <- rowSums(combined[,c("percent.x", "percent.y")], na.rm = TRUE)
  summary <- summary %>% filter(variable %in% c(interesting,allRequiredVariables))
  return(summary)
}

summarizeCriticalErrors <- function(errorFrame, groupBy){
  if (nrow(errorFrame) == 0) return(NULL)
  criticalErrors <- errorFrame %>% filter(startsWith(errorCode, "1.1")) %>% 
    group_by(errorCode, category, table, error_field) %>% summarise(Count = sum(quantity))
  return(criticalErrors)
}
