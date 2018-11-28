

addToMissingErrorFrame <- function(missingFrame, table, field, tableName, errorCode, severity){
  
  newRow <- list()
  numErrors <- nrow(table)
  
  newRow$table <- rep(tableName, numErrors)
  
  idList <- tableIDField[[tableName]]
  for (index in seq_along(idList)){
    idVariable <- idList[[index]]
    idField <- idFieldNames[[index]] #JUDY replace with paste0 but then look for idfield and idvalue change to 1
    idValue <- as.character(idValueNames[[index]])
    newRow[[idField]] <- idVariable
    newRow[[idValue]] <- as.character(get(idVariable, table))
  }
  
  newRow$missingVariable <- field
  newRow$severity <- severity
  newRow$errorCode <- errorCode
  index <- paste0(tableName, field, errorCode)
  missingFrame[[index]] <- data.frame(newRow, stringsAsFactors = FALSE)
  
  return(missingFrame)
}

addToErrorFrame <- function(errorFrame, table, field, tableName, errorType, errorCode, 
                            severity, message, ...){ 
  numErrors <- nrow(table)
  lastErrorCount <- isolate(errorCount())
  newErrorCount <- lastErrorCount + numErrors
  errorCount(newErrorCount)
  if (isolate(errorExcess())) return(errorFrame)
  if (isolate(errorCount()) > errorLimit) {
     errorExcess(TRUE)
     return(errorFrame)
  }
  idList <- tableIDField[[tableName]]
  idColumns <- c()
  for (i in 1:length(idList)){
    idColumns <- c(idColumns, idFieldNames[[i]], idValueNames[[i]])
  }
  
  columnNames <- c("PROGRAM", "table", idColumns, minimumErrorDetail, names(list(...))) #minimumErrorDetail in definitions.R
  newErrors <- setNames(data.frame(matrix(nrow = numErrors, ncol = length(columnNames))),
                        columnNames)
  newErrors[,] <- ""

  newErrors$table <- rep(tableName, numErrors)
  for (index in seq_along(idList)){
    idVariable <- idList[[index]]
    idField <- idFieldNames[[index]] #idFieldNames in definitons.R
    idValue <- as.character(idValueNames[[index]])
    newErrors[[idField]] <- idVariable
    newErrors[[idValue]] <- as.character(get(idVariable, table))
  }
  if (exists("PROGRAM", table)) newErrors$PROGRAM <- table$PROGRAM
  else if (idList[[1]] == "PATIENT"){
    newErrors$PROGRAM <- uploadedTables()$tblBAS[match(newErrors$id1,
                                                       uploadedTables()$tblBAS$PATIENT ),"PROGRAM"]
  } else newErrors$PROGRAM <- "Unknown"
  newErrors$error_field <- field
  newErrors$error <- as.character(get(field, table))
  newErrors$category <- errorType
  newErrors[, names(list(...))] <- list(...)
  
  newErrors$severity <- severity
  newErrors$description <- message
  
  newErrors$errorCode <- errorCode

  

  index <- paste0(tableName, field, errorType)
  errorFrame[[index]] <- newErrors   
  
  # programsInThisError <- unique(newErrors$PROGRAM)
  # summaryByTable[[tableName]][[index]] <- data.frame("Field" = field, "Category" = errorType, "Count" = numErrors)
  # for (programName in programsInThisError){
  #   summaryIndex <- 
  # }
  
  return(errorFrame)
}

addToSummaryFrame <- function(summaryFrame, severity, tableName, variableName, errorType, numberOfErrors, percentErrorsTable)
{
 newerrors <- data.frame(
   "severity" = severity,
    "table" = tableName, 
    "variable" = variableName,
    "category" = errorType, 
    "number"= numberOfErrors,
    "percent"= percentErrorsTable, stringsAsFactors = FALSE)    
 index <- paste0(tableName, variableName, errorType)
 summaryFrame[[index]] <- data.frame(newerrors, stringsAsFactors = FALSE)
  return(summaryFrame)
}

addToSummaryFrameByProgram <- function(summaryFrame, programName, tableName, variableName, errorType, numberOfErrors, percentErrorsTable)
{
  newerrors <- data.frame(
    "PROGRAM" = as.character(programName),
    "table" = as.character(tableName), 
    "variable" = variableName,
    "category" = errorType, 
    "number"= numberOfErrors,
    "percent"= percentErrorsTable, stringsAsFactors = FALSE)    
  
  index <- paste0(programName, tableName, variableName, errorType)
  summaryFrame[[index]] <- data.frame(newerrors, stringsAsFactors = FALSE)
  
  return(summaryFrame)
}

addToErrorCodeSummaryFrame <- function(summaryFrame, tableName, variableName, errorValue, numberOfErrors, percentErrorsTable)
{
  newerrors <- data.frame(
    "table" = as.character(tableName), 
    "variable" = variableName,
    "errorValue" = errorValue, 
    "number"= numberOfErrors,
    "percent"= percentErrorsTable, stringsAsFactors = FALSE)    
  
  index <- paste0(tableName, variableName, errorValue)
  summaryFrame[[index]] <- data.frame(newerrors, stringsAsFactors = FALSE)
  
  return(summaryFrame)
}



# checkCodedVariables: compare all coded fields with valid codes for that variable. Invalid codes = Error
checkCodedVariables <- function(errorFrame){
  severity <- "Error"
  for(tableName in uploadList()$AllDESTables){
    formattedTable <- get(tableName, formattedTables())
    table <- get(tableName, uploadedTables())
    variablesInTable <- names(formattedTables()[[tableName]])
    
    codedFieldNames <- intersect(variablesInTable,
                                 findVariablesMatchingCondition(tableName, "has_codes","Y"))
    if (length(codedFieldNames)==0) next    
    for (codedField in codedFieldNames){
      codeIndex <- as.numeric(tableDef[[tableName]]$variables[[codedField]]$code_list_ref)
      codeList <- codes[[codeIndex]]
      validCodes <- c("", names(codeList), NA)
      # any NA records in formattedTable[[codedField]] are either invalid codes, blank, or NA
      badCodesRecords <- table[formattedTable[which(is.na(formattedTable[[codedField]])), "recordIndex"],]
      badCodesRecords <- badCodesRecords[which(!(as.character(get(codedField, badCodesRecords)) %in% 
                                                   validCodes)),]
      if (tableDef[[tableName]]$variables[[codedField]]$data_format == "Numeric"){
        # if the code is numeric, allow either integer or decimal for integer codes. For example,
        # ART_RS code list includes 1 and 1.1, 1.2, but not 1.0, but data analysis packages sometimes force everything
        # in the column to have a decimal so we will accept either 1 or 1.0 as valid
        badCodesRecords <- badCodesRecords[which(!(as.numeric(get(codedField, badCodesRecords)) %in% 
                                                     as.numeric(names(codeList)))),]
      }
      
      if (nrow(badCodesRecords)>0){
        if (length(validCodes) > maxCodesToShow){
          message <- paste0("See iedeades.org for a list of valid codes for ", codedField)
        }
        else message <- paste0("Valid codes for ", codedField, " are ",paste(names(codeList), collapse =", "))
       # if (nrow(badCodesRecords) < limitOnInvalidCodesToRecord){
          errorFrame <- addToErrorFrame(errorFrame, badCodesRecords, codedField, tableName, "Invalid code", 
                                        errorCode = "Invalid Code", severity, message)
        # } else {
        #   # if there are excessive instances of bad codes for a variable, check to see if there 
        #   # are commonly used invalid codes; no need to document each individually but by PROGRAM
        #   uniqueBadCodes <- unique(badCodesRecords[[codedField]])
        #   for (badCode in uniqueBadCodes){
        #     theseRows <- which(badCodesRecords[[codedField]] == badCode)
        #     numberOfThisBadCode <- length(theseRows)
        #     if (numberOfThisBadCode > limitOnInvalidCodesToRecord){
        #       if (exists(PROGRAM, ))
        #       browser()
        #       errorFrame <- addToErrorFrame(errorFrame, table = ,
        #                                     tableName = tableName,
        #                                     errorType = "Invalid Code", errorCode = "Invalid Code",
        #                                     severity = severity, message = "message",
        #                                     quantity = NULL
        #                                     )
        #     } else {
        #       # if number of instances of this invalid code are not excessive, 
        #       # add detail to errorFrame in usual way
        #       errorFrame <- addToErrorFrame(errorFrame, badCodesRecords, codedField, tableName, "Invalid code", 
        #                                     errorCode = "Invalid Code", severity, message)
        #     }
        #  }
       # }
      }
    }
  }
  return(errorFrame)
}

checkValidRangeLabIDValueWithUnits <- function(errorFrame, numericField, tableName, tableSubset, severity){
  dependency <- labIDLimits[[numericField]]$dependency
  limitInfo <- labIDLimits[[numericField]][[dependency[1]]]
  rowsWithValidLabs <- tableSubset[[dependency[1]]] %in% names(limitInfo)
  table2Check <- tableSubset[rowsWithValidLabs,]
  
  for (i in 1:nrow(table2Check)){
    labName <- table2Check[i,dependency[1]]
    unitName <- table2Check[i,dependency[2]]
    validUnits <- names(limitInfo[[labName]][[dependency[2]]])
    if (! (unitName %in% validUnits)){
      message <- paste0(unitName, " is not a valid unit type for ",
                        dependency[1]," = ", labName, ". The only valid unit code(s) for this test: ",
                        paste(validUnits, collapse = ", ")
      )
      errorFrame <- addToErrorFrame(errorFrame, table2Check[i,], dependency[2], tableName, "Invalid Lab Units",
                                    errorCode = "Invalid Lab Units", "Error",
                                    message)
    }
    else if (!(table2Check[i,numericField] == labIDLimits[[numericField]]$undetectable)){
      upperLimit <- limitInfo[[labName]][[dependency[2]]][[unitName]]$upper
      lowerLimit <- limitInfo[[labName]][[dependency[2]]][[unitName]]$lower
      if (as.numeric(table2Check[i,numericField]) > as.numeric(upperLimit)){
        message <- paste0("The maximum value allowed for ", numericField, " is ",upperLimit,
                          " when the lab test is ", labName,
                          " and the unit code is ", unitName)
        errorFrame <- addToErrorFrame(errorFrame, table2Check[i,], numericField, tableName, "Value Above Expected Range", 
                                      errorCode = "Numeric Range Error", "Error",
                                      message)
      }
      else if (as.numeric(table2Check[i,numericField]) < as.numeric(lowerLimit)){
        message <- paste0("The minumum value allowed for ", numericField, " is ",lowerLimit,
                          " when the lab test is ", labName,
                          " and the unit code is ", unitName)
        errorFrame <- addToErrorFrame(errorFrame, table2Check[i,], numericField, tableName, "Value Below Expected Range",
                                      errorCode = "Numeric Range Error","Error",
                                      message)
      }
    }
  }
  return(errorFrame)
}

  
checkBPValues <- function(errorFrame, table){
  tableName <- "tblLAB_BP"
  #find records missing or invalid units
  recordsMissingUnits <- is.na(table$BP_U)
  message <- paste0("Valid units must be provided for blood pressure values")
  #in erorrFrame use unformatted original data from uploadedTables()
  errorFrame <- addToErrorFrame(errorFrame, uploadedTables()$tblLAB_BP[recordsMissingUnits,],"BP_U", tableName, 
                                errorCode = "Missing Units", "Missing Units", "Warn", message)
  #check BP values on records containing units
  recordsToCheck <- table[!recordsMissingUnits,]
  for (units in names(BPLabLimits)){
    unitLimits <- get(units, BPLabLimits)
    upper <- unitLimits$upper
    lower <- unitLimits$lower
    unitCode <- unitLimits$unitCode
    theseUnits <- recordsToCheck %>% filter(BP_U==units)
    for (labTest in c("BP_SYS","BP_DIA")){
      tooHighRecords <- theseUnits[[labTest]] > upper
      if (any(tooHighRecords, na.rm=TRUE)){
        message <- paste0("The maximum value allowed for ", labTest, " is ",upper,
                          " when the units are ", units, 
                          " as indicated by BP_U = ", unitCode)
        errorFrame <- addToErrorFrame(errorFrame, theseUnits[tooHighRecords,], labTest, tableName, 
                                      "Value Above Expected Range", errorCode = "Numeric Range Error", "Warn", message)
        
      }
      recordsToCheck <- theseUnits[!tooHighRecords]
      tooLowRecords <- recordsToCheck[[labTest]] < lower
      if (any(tooLowRecords)){
        message <- paste0("The minimum value allowed for ", labTest, " is ",lower,
                          " when the units are ", units, 
                          " as indicated by BP_U = ", unitCode)
        errorFrame <- addToErrorFrame(errorFrame, recordsToCheck[tooLowRecords,], labTest, tableName, 
                                      "Value Below Expected Range", errorCode = "Numeric Range Error", "Warn", message)
      }
    }
  }
  return(errorFrame)
}

checkValidRangeWithUnits <- function(errorFrame, numericField, tableName, tableSubset, severity){
  limitInfo <- labLimitsUnits[[numericField]]
  dependency <- limitInfo$dependency
  testName <- numericField
  validUnits <- names(limitInfo[[dependency]])
  rowsWithValidUnits <- tableSubset[[dependency]] %in% validUnits
  table2Check <- tableSubset[rowsWithValidUnits,]
  for (unit in validUnits){
    upper <- limitInfo[[dependency]][[unit]]$upper
    lower <- limitInfo[[dependency]][[unit]]$lower
    unitName <- limitInfo[[dependency]][[unit]]$units
    tooLowRecords <- ( (table2Check[[dependency]]==unit)
                                 & (as.numeric(table2Check[[testName]])<as.numeric(lower)) )
    tooHighRecords <- ( (table2Check[[dependency]]==unit)
                       & (as.numeric(table2Check[[testName]])>as.numeric(upper)) )
    if (any(tooLowRecords[!is.na(tooLowRecords)])){
      message <- paste0("The minimum value allowed for ", numericField, " is ",lower,
                        " when the units are ", unitName, 
                       " as indicated by ",dependency," = ", unit)
      errorFrame <- addToErrorFrame(errorFrame, table2Check[tooLowRecords,], numericField, tableName, 
                                    "Value Below Expected Range", errorCode = "Numeric Range Error", severity, message)
    }
    if (any(tooHighRecords[!is.na(tooHighRecords)])){
      message <- paste0("The maximum value allowed for ", numericField, " is ",upper,
                        " when the units are ", unitName, 
                        " as indicated by ",dependency," = ", unit)
      errorFrame <- addToErrorFrame(errorFrame, table2Check[tooHighRecords,], numericField, tableName, 
                                    "Value Above Expected Range", errorCode = "Numeric Range Error", severity, message)
    }
  }
  
  return(errorFrame)
}

checkValidRange <- function(errorFrame, tableName, formattedTable, numericField, severity){
  limits <- numericLimits[[numericField]]
  #Check for values greater than upper limit that aren't == value for unknown
  if (!is.null(limits$unknown)){
    formattedTable <- formattedTable %>% filter((.)[numericField] != limits$unknown)
  }
  tooHighRecords <- formattedTable %>% filter((.)[numericField] >limits$upper)
  if ((numericField == "HEIGH") && (nrow(tooHighRecords) > 0.9*nrow(formattedTable))){
    errorMessageModal("HEIGH should be reported in meters. Your HEIGH values are too large and appear to be in cm or inches.")
    resetFileInput$reset <- TRUE
    return(NULL)
  }
  if (nrow(tooHighRecords) > 0){
    message <- paste0("The maximum value expected for ", numericField, " is ",limits$upper, limits$units)
    if(exists("unknown",limits)){
      message <- paste0(message, ". Note: The code for Unknown is ",limits$unknown)
    }
    errorFrame <- addToErrorFrame(errorFrame, tooHighRecords, numericField, tableName, 
                                  "Value Above Expected Range",errorCode = "Numeric Range Error", severity, message)
  }
  tooLowRecords <- formattedTable %>% filter((.)[numericField] < limits$lower)
  if (nrow(tooLowRecords) > 0){
    message <- paste0("The minimum value expected for ", numericField, " is ",limits$lower, limits$units)
    errorFrame <- addToErrorFrame(errorFrame, tooLowRecords, numericField, tableName, 
                                  "Value Below Expected Range",errorCode = "Numeric Range Error", severity, message)
  }
  return(errorFrame)
}

checkNumericValues <- function(errorFrame){
  for(tableName in uploadList()$AllDESTables){
    table <- get(tableName, uploadedTables())
    formattedTable <- get(tableName, formattedTables())
    variablesInTable <- names(formattedTable)
    numericFieldNames <- intersect(variablesInTable, findVariablesMatchingCondition(tableName, "data_format","Numeric"))
    codedFieldNames <- intersect(variablesInTable, findVariablesMatchingCondition(tableName, "has_codes","Y"))
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
          message <- paste0("Numeric Value Expected for ",numericField)
          severity <- "Error"
          errorFrame <- addToErrorFrame(errorFrame, badRecords, numericField, tableName, "Numeric Format Error",
                                        errorCode = "Numeric Format Error",severity, message)
        }
      } 

      #check to see if numeric values are within a valid range
      if (numericField %in% names(numericLimits)){
        errorFrame <- checkValidRange(errorFrame, tableName, formattedTable[!potentialBadRecords,], numericField, "Warn")
      }
      # if excessive numeric range errors like HEIGH all in cm then break *both* loops
      if (is.null(errorFrame)) break
    }
    if (is.null(errorFrame)) break
  }
  return(errorFrame)
}
 
# Check for records with lab values but missing lab units.
checkLabValues <- function(errorFrame){
  if ("tblLAB_BP" %in% uploadList()$AllDESTables){
    errorFrame <- checkBPValues(errorFrame, formattedTables()$tblLAB_BP)
  }

  if ("tblLAB_CD4" %in% uploadList()$AllDESTables){
    # if CD4_U is %, out of range values will be flagged later
    # if CD4_U is cells/mm<sup>3</sup> then CD4_V shouldn't exceed upper limit defined in definitions.R
    recordsToCheck <- formattedTables()$tblLAB_CD4 %>% filter(CD4_U == "cells/mm<sup>3</sup>") 
    #negative values are ok: they indicate (less than) detection limit
    tooHighRecords <- recordsToCheck %>% filter(CD4_V > CD4$upperLimit)
    if (nrow(tooHighRecords)>0){
      severity <- "Warn"
      message <- paste0("CD4_V can't exceed ", CD4$upperLimit, " when the unit = cells/mm3")
      errorFrame <- addToErrorFrame(errorFrame, uploadedTables()$tblLAB_CD4[tooHighRecords$recordIndex,], "CD4_V", "tblLAB_CD4", 
                                    "Value Above Expected Range", 
                                    errorCode = "Numeric Range Error", severity, message)
    }
    #errorFrame <- checkForSpikes(errorFrame, "tblLAB_CD4", "CD4_V", "CD4_U", "CD4_D")
  }
  
  labTablesToCheck <- intersect(labTablesRequiringUnits, uploadList()$AllDESTables)
  for (tableName in labTablesToCheck){
    thisTable <- get(tableName, formattedTables())
    labValue <- names(thisTable)[endsWith(names(thisTable),"_V")]
    # if no labValue field in table, check next table
    if (length(labValue)==0) next
    labName <- strsplit(labValue,"_V")[[1]]
    if (exists(paste0(labName,"_U"), thisTable)){
      labUnits <- paste0(labName,"_U")
      recordsWithLabValues <- thisTable %>% filter(!is.na(.[labValue]))
      recordsMissingUnits <- recordsWithLabValues %>% filter(is.na(.[labUnits]))
      if (nrow(recordsMissingUnits)>0){
        potentialBadRecords <- uploadedTables()[[tableName]][recordsMissingUnits$recordIndex,]
        badRecords <- potentialBadRecords %>% filter(is.na(.[labUnits])| (.[labUnits]==""))
        if (nrow(badRecords) == 0) next
        severity <- "Warn"
        message <- paste0("Lab Units must be provided for Lab Values")
        errorFrame <- addToErrorFrame(errorFrame, badRecords, labUnits, tableName, 
                                      "Missing Units",
                                      errorCode = "Missing Units", severity, message, 
                                      error_field2 = labValue, 
                                      errorValue2 = as.character(badRecords[,labValue]))
      }
      recordsAboveRangePercent <- recordsWithLabValues %>% 
        filter(.[labUnits]=="%") %>% 
        filter(.[labValue] > 100)
      if (nrow(recordsAboveRangePercent)>0){
        severity <- "Error"
        message <- paste0(labValue," can't exceed 100 when the units = %")
        errorFrame <- addToErrorFrame(errorFrame, recordsAboveRangePercent, labValue, tableName, 
                                      "Value Above Expected Range", 
                                      errorCode = "Numeric Range Error", severity, message)
      }
      #taking lower check out for now -- HICDep does this check but possibly not useful for IeDEA
       recordsBelowRangePercent <- recordsWithLabValues %>% filter(labUnits=="%") %>% filter(labValue <= 1)
      if (nrow(recordsBelowRangePercent)>0){
         severity <- "Warn"
         message <- paste0("Possible error When ",labValue," is < 1 when the unit = %. Should this be x100?")
         errorFrame <- addToErrorFrame(errorFrame, recordsAboveRangePercent, labValue, tableName, 
                                       "Percent < 1.0", errorCode = "Numeric Range Error", severity, message)
       }
    }
  }
  return(errorFrame)
}


findMissingValues <- function(errorFrame){  ###variables to track: required + missingaction in REDCap
  missingFrame <- initializeMissingErrors()
  for (tableName in uploadList()$AllDESTables){
    print(paste0("checking missing in ",tableName))
    variablesInTable <- names(formattedTables()[[tableName]])
    uploadedTable <- get(tableName, uploadedTables())
    
    requiredVariables <- intersect(variablesInTable,
                                   findVariablesMatchingCondition(tableName, "variable_required", "1"))
    variablesToTrack <- intersect(variablesInTable,
                                  findVariablesMatchingCondition(tableName, "variable_missingaction", "1"))
      
    # compile a list of variables in this table that should not be blank
    variablesToCheck <- unique(c(requiredVariables, variablesToTrack))
    for (fieldName in variablesToCheck){
      severity <- "Warn" 
      varClass <- class(uploadedTable[[fieldName]])[[1]] # sometimes dates have format  "POSIXct" "POSIXt"
      print(paste0("checking missing ", fieldName, Sys.time()))
      if (varClass == "character"){
        badRecords <- ((safeTrimWS(uploadedTable[[fieldName]]) == "") | is.na(uploadedTable[[fieldName]]))
      } else {
        badRecords <- is.na(uploadedTable[[fieldName]])
      }
        print(paste0("finished finding bad ", Sys.time()))

      if (any(badRecords)){
        print("found missing")
        if (fieldName %in% requiredVariables){
          severity <- "Error"
          errorFrame <- addToErrorFrame(errorFrame, uploadedTable[which(badRecords),], fieldName, tableName,
                                        "Missing Required Variable", errorCode = "Missing Required Variable", 
                                        "Error", 
                                        paste0(fieldName," is a required variable and should not be blank"))
        }
        missingFrame <- addToMissingErrorFrame(missingFrame, uploadedTable[which(badRecords),], fieldName, tableName,
                                              "Missing", severity)
      }
    }
  }
  missingFrame <- rbindlist(missingFrame, use.names = TRUE, fill = TRUE)
  if (!is_empty(missingFrame)){
    missingFrame[is.na(missingFrame)] <- ""
  }
  if (!is_empty(missingFrame)){
    missingFrame <- left_join(missingFrame, formattedTables()$tblBAS[,c("PATIENT","PROGRAM")],
                              by = c("id1" = "PATIENT"))
  }

  return(list(missingFrame = missingFrame, errorFrame = errorFrame))
}

#Summarize the number of missing entries by variableName
summarizeMissing <- function(frame){
  if (is_empty(frame)) return(frame)
  tableRows <- sapply(formattedTables(),function(x){return(nrow(x))})
  summary <- frame %>% group_by_at(vars(table,missingVariable)) %>% 
    dplyr::summarise(number =n()) %>% left_join(rownames_to_column(as.data.frame(tableRows, stringsAsFactors = FALSE), var="table")) %>% 
    mutate(percent = round(100*number/tableRows,1)) %>% select(-tableRows) %>% mutate(category="Missing") %>% 
    rename(variable =missingVariable)
  return(summary)
}

#summarize the number of missing entries by Program and variableName
summarizeMissingByProgram <- function(frame){
  if (is_empty(frame)) return(frame)
  programNames <- unique(frame$PROGRAM)
  tableNames <- unique(frame$table)
  rows <- length(programNames)*length(tableNames)
  tableRows <- data.frame(
    "PROGRAM" = character(rows), 
    "table" = character(rows),
    "numRows" = numeric(rows),
    stringsAsFactors = FALSE)
  row <- 0
  for (tableName in tableNames){
    for (programName in programNames){
      row <- row+1
      tableRows$PROGRAM[[row]] <- programName
      tableRows$table[[row]] <- tableName
      tableRows$numRows[[row]] <- nrow(formattedTables()[[tableName]] %>% filter(PROGRAM==programName))
    }
  }
  summary <- frame %>% 
    #filter(!is.na(PROGRAM)) %>% 
    group_by_at(vars(PROGRAM,table,missingVariable)) %>% 
    dplyr::summarise(number=n()) %>%
    ungroup() %>% left_join(tableRows, by=c("PROGRAM","table")) %>% 
    mutate(percent = round(100*number/numRows,1)) %>% select(-numRows) %>% mutate(category="Missing") %>% 
    rename(variable =missingVariable)

  return(summary)
}


summarizeUnknownCodesByProgram <- function(){  ## do this with formattedTables after program added to every table
  summaryOfUnknownCodesByProgram <- initializeSummaryFrameByProgram()
  allTableNames <- uploadList()$AllDESTables
  for (programName in unique(formattedTables()$tblBAS$PROGRAM)){
    for (tableName in allTableNames){
      table <- get(tableName, formattedTables()) %>% filter(PROGRAM == programName) 
      variablesInTable <- names(table)
      codedFieldNames <- intersect(variablesInTable,
                                   findVariablesMatchingCondition(tableName, "has_codes","Y"))
      variablesWorthCheckingForUnknown <- codedFieldNames[(
        (!codedFieldNames %in% codesThatCanBeBlank) &
          !endsWith(codedFieldNames,"_RS") &
          !endsWith(codedFieldNames,"_A"))]
      for (codedField in variablesWorthCheckingForUnknown) {
          numberCodedUnknown <- sum(tolower(table[[codedField]])=="unknown", na.rm = TRUE)
          if (numberCodedUnknown > 0){
            summaryOfUnknownCodesByProgram <- addToSummaryFrameByProgram(summaryOfUnknownCodesByProgram, programName, tableName, 
                                                                         codedField, "Unknown", numberCodedUnknown, 
                                                                         round(100*numberCodedUnknown/nrow(table),1))
          }
      }
    }
  }
  summaryOfUnknownCodesByProgram <- rbindlist(summaryOfUnknownCodesByProgram, use.names = TRUE, fill = TRUE)
  return(summaryOfUnknownCodesByProgram)
}


summarizeUnknownCodes <- function(unknownByProgram){  
  if (nrow(unknownByProgram)==0){
    return(initializeSummaryFrame())
  }
  unknownSummary <- unknownByProgram %>% 
    group_by_at(vars("table","variable","category")) %>% 
    summarise(num=sum(number)) %>% 
    mutate(perc = round(100*num/nrow(formattedTables()[[table]]),1)) %>% 
    rename(number = num, percent= perc)
  return(unknownSummary)
}

PatientIDChecks <- function(errorFrame){
  validPATIENTs <- formattedTables()$tblBAS$PATIENT
  for (tableName in uploadList()$tablesWithPatientID){
    if (tableName != "tblVIS"){
      currentTable <- get(tableName, uploadedTables()) 
      badRecords <- !(currentTable[,"PATIENT"] %in% validPATIENTs)
      if (any(badRecords[!is.na(badRecords)])){
        errorFrame <- addToErrorFrame(errorFrame, currentTable[badRecords,], "PATIENT", tableName,
                                      "Invalid PATIENT ID", errorCode = "Invalid PATIENT ID", "Error", "Every PATIENT should have an entry in tblBAS")
      }
    }
  }
  return(errorFrame)
}

duplicateRecordChecks <- function(errorFrame){
  for (tableName in uploadList()$AllDESTables[!(uploadList()$AllDESTables %in% duplicateRecordExceptions)]){
    tabledf <- get(tableName, uploadedTables())
    table <- as_tibble(tabledf)
    idFields <- tableIDField[[tableName]]
    duplicates <- table %>%
      count(.dots = idFields) %>%
      filter(n > 1)
    if (length(duplicates$n) > 0){
      message <- paste0(duplicates$n, " rows of ", tableName, 
                        " have identical values for ", paste(idFields, collapse = ", "))
      errorFrame <- addToErrorFrame(errorFrame, select(duplicates, idFields), 
                                    idFields[1], tableName, 
                                    "Duplicate Record",errorCode = "Duplicate Record", "Error", message)
    }
  }
  return(errorFrame)
}




checkForDecreasingHeight <- function(errorFrame) {
  if (!("tblVIS" %in% uploadList()$AllDESTables))
    return(errorFrame)
  
  tblVIS <- formattedTables()$tblVIS
  if (!("HEIGH" %in% names(tblVIS)))
    return(errorFrame)
  tblVIS <- tblVIS %>% filter(!is.na(HEIGH)) %>% 
    filter(HEIGH != numericLimits$HEIGH$unknown) %>% 
    filter(HEIGH !="") %>% filter(HEIGH < numericLimits$HEIGH$upper) #only include valid heights; heights above range will be flagged by numeric check
  
  temp <- arrange(tblVIS, VIS_D) %>%  arrange(PATIENT) %>% 
    group_by(PATIENT) %>% mutate(checkIt = c(0,diff(HEIGH))) 
  
  potentialBadRecords <- (temp$checkIt < -1*maxHeightDecreaseInM)
  
  if (any(potentialBadRecords)) {
    badRecords <- which(potentialBadRecords)
    message <- paste("Height should not decrease over time. The height on this date was less than on the previous date. On ",
                     temp$VIS_D[badRecords-1]," this Patient's Height was ", temp$HEIGH[badRecords-1], "m")

    errorFrame <- addToErrorFrame(errorFrame, temp[badRecords,], "HEIGH", "tblVIS", "Large decrease in HEIGH",
                                  errorCode = "Numeric Logic", "Warn", message)
  }
  return(errorFrame)
}

checkForConflictingStages <- function(errorFrame, stageVariable){
  # if all xxx_STAGE values are NA, exit function
  if (all(is.na(formattedTables()$tblVIS[,stageVariable]))) {return(errorFrame)}
  
  tblVIS <- formattedTables()$tblVIS %>% 
    filter(!is.na(.[stageVariable])) %>%
    filter(.[stageVariable] != "Unknown")
  
  # only include records with exact dates or _A blank (NA). The danger here is that NA could also mean invalid code
  if ("VIS_D_A" %in% names(tblVIS)){
    tblVIS <- tblVIS %>% filter(VIS_D_A %in% c(NA,"Exact to the date"))
  }
  
  tblVIS <-  tblVIS %>% 
    select(PATIENT, VIS_D, !!stageVariable)
  
  # detect patients with multiple records on same day
  duplicateDateRecords <- duplicated(tblVIS[,c("PATIENT","VIS_D")])
  
  # if there aren't multiple visit records on a single date, exit function
  if (!any(duplicateDateRecords))return(errorFrame)
  
  # find records with duplicate dates
  # the distinct function isolates records with same PATIENT and VIS_D but different _STAGE
  badRecords <- tblVIS %>% 
    filter(PATIENT %in% tblVIS[duplicateDateRecords,"PATIENT"]) %>% 
    filter(VIS_D %in% tblVIS[duplicateDateRecords, "VIS_D"]) %>% 
    distinct() %>% 
    group_by(PATIENT, VIS_D) %>% 
    mutate(n=n()) %>% 
    filter(n > 1) %>% 
    group_by(PATIENT, VIS_D, n) %>% 
    summarise_at(vars(!!stageVariable),
                 function(x) {
                   stages <- paste(as.numeric(x), collapse = ",") # create list of all stages on this date
                 }) %>% 
    mutate(message = paste0("This patient had ", n, " conflicting ", !!stageVariable, " reports on the same date" ))
  
    
    if (nrow(badRecords) >0){
      errorFrame <- addToErrorFrame(errorFrame, badRecords, stageVariable, "tblVIS", 
                                    paste0("Conflicting ", stageVariable, " on single date"),
                                    errorCode = "Logic", "Warn", badRecords$message)
    }
  return(errorFrame)
}


summarizeErrors <- function(errorFrame, tableData){
  summaryFrame <- initializeSummaryFrame()
  badCodeSummary <- initializeCodeErrorSummaryFrame()
  # this will put the tables in the order specified in AllDESTables
  tablesWithErrors <- intersect(uploadList()$AllDESTables, unique(errorFrame$table))
  for (tableName in tablesWithErrors){
    tableRows <- nrow(tableData[[tableName]])
    allErrorVariables <- unique(errorFrame$error_field[errorFrame$table == tableName])
    for (errorVariable in allErrorVariables){
      allErrorTypes <- unique(errorFrame$category[(errorFrame$table == tableName)&(errorFrame$error_field == errorVariable)])
      for (errorType in allErrorTypes){
        errorRows <- which(errorFrame$table == tableName 
                                       & errorFrame$error_field == errorVariable
                                       & errorFrame$category == errorType)        
        numberOfErrors <- length(errorRows)
        if (numberOfErrors > tableRows){ # for date logic errors, one bad date triggers multiple errors
          numberOfErrors <- length(unique(errorFrame$id1[errorRows]))
        }
        if (errorType == "Duplicate Record"){
          # tally the duplicate records, not the original record. The first character of "description" is the number of rows
          # with identical identifying fields, so extract that number (nth_number) and subtract 1, then sum all
          numberOfErrors <- sum(nth_number(errorFrame$description[errorRows],1) - 1)
        }
        severity <- errorFrame$severity[errorRows][[1]] #error severity should be the same for identical errorType and variable
        summaryFrame <- addToSummaryFrame(summaryFrame, severity, tableName, errorVariable, errorType, 
                                          numberOfErrors, round(100*numberOfErrors/tableRows,1))
        if(errorType == "Invalid code"){
          allBadCodes <- errorFrame[errorRows,
                                    c("table","error_field","error","category")]
          uniqueErrorCodes <- unique(allBadCodes$error)
          numberOfUniqueInvalidCodes <- length(uniqueErrorCodes)
          # if there are an excessive number of unique invalid codes, not appropriate for summary table
          # limitOnInvalidCodesToShow defined in definitions.R
          if (numberOfUniqueInvalidCodes > limitOnInvalidCodesToShow){
            numberOfBadCodes <- nrow(allBadCodes)
            message <- paste0(numberOfUniqueInvalidCodes," unique invalid codes")
            badCodeSummary <- addToErrorCodeSummaryFrame(badCodeSummary, tableName, errorVariable,
                                                         message, 
                                                         numberOfBadCodes,
                                                        round(100*numberOfBadCodes/tableRows,1))
          } else {
            for (badCode in uniqueErrorCodes){
              numberOfThisBadCode <- sum(allBadCodes$error == badCode)
              badCodeSummary <- addToErrorCodeSummaryFrame(badCodeSummary, tableName, errorVariable, badCode, 
                                                           numberOfThisBadCode,
                                                           round(100*numberOfThisBadCode/tableRows,1))
            }
          }
        }
      }
    }
  }
  
  badCodeSummary <- rbindlist(badCodeSummary, use.names = TRUE, fill = TRUE)
  
  summaryFrame <- rbindlist(summaryFrame, use.names = TRUE, fill = TRUE)
  
  if (exists("severity", summaryFrame)){
    summaryFrame <- summaryFrame %>% select(category, everything())
    errorOnly <- summaryFrame %>% filter(severity == "Error") %>% select(-severity) %>% 
      rename(Error = "category", Table = "table", Variable = "variable", Count = "number", Percent = "percent")
    warnOnly <- summaryFrame %>% filter(severity == "Warn") %>% select(-severity) %>% 
      rename(Error = "category", Table = "table", Variable = "variable", Count = "number", Percent = "percent")
    highLevelSummary <- errorFrame %>% group_by(errorCode) %>% summarise(Count = n())
  }
  else { #this means there were no errors or warnings, summaryFrame is an empty frame
    errorOnly <- summaryFrame
    warnOnly <- summaryFrame
    highLevelSummary <- summaryFrame
  }
  if (is_empty(badCodeSummary) || nrow(badCodeSummary) == 0) badCodeSummary <- NULL
  else badCodeSummary <- badCodeSummary %>% 
      rename(Table = "table", Variable = "variable", InvalidCode = "errorValue", Count = "number", Percent = "percent")
  

  return(list("summaryFrame"=summaryFrame, 
              "badCodeFrame" = badCodeSummary,
              "errorOnlySummary" = errorOnly,
              "warnOnlySummary" = warnOnly,
              "highLevelSummary" = highLevelSummary))
}


check_DAgreementWith_Y <- function(errorFrame){
  for (tableName in uploadList()$AllDESTables){
    table <- formattedTables()[[tableName]]
    var_Y <- unlist(strsplit(names(table)[endsWith(names(table),"_Y")],"_Y"), use.names = FALSE)
    var_D <- unlist(strsplit(names(table)[endsWith(names(table),"_D")],"_D"), use.names = FALSE)
    var_Y_D <- var_Y[var_Y %in% var_D]
    for (variable in var_Y_D){
      yVarName <- paste0(variable,"_Y")
      dateVarName <- paste0(variable, "_D")
      #error scenario 1: If a the _D field has a date but the _Y is anything other than 1 (Yes)
      badRecords <- table %>% filter(!is.na(.[dateVarName])) %>% 
                      filter(!(.[yVarName]=="Yes")) %>% pull(recordIndex)
      if(length(badRecords)>0){
        message <- paste0("If a date is recorded for ", dateVarName," then ",
                         yVarName, " must have a value of 1 (Yes)")
        errorFrame <- addToErrorFrame(errorFrame, uploadedTables()[[tableName]][badRecords,], yVarName, 
                                     tableName, 
                                     paste0("_D/_Y Logic"),# (", yVarName, " not YES when date provided for ", dateVarName, ")"), 
                                     errorCode = "Logic",
                                     "Error", message, 
                                     error_field2 = dateVarName, 
                                     errorValue2 = as.character(uploadedTables()[[tableName]][badRecords,dateVarName]))
      }
      # JUDY: revisit -- check with Bev
      #error scenario 2: If a the _Y field is Yes but the _D field is blank
      # for now I am removing that check-- it seems common to have a value for AIDS_Y with no AIDS_D
      # if (!(variable %in% c("FATHERDEATH", "MOTHERDEATH"))){
      #   badRecords <- table %>% filter(is.na(.[dateVarName])) %>% 
      #     filter(.[yVarName]=="Yes") %>% pull(recordIndex)
      #   if(length(badRecords)>0){
      #     recordsWithBlankDate <- uploadedTables()[[tableName]][badRecords,]
      #     # if uploaded file is in char format and had dates with invalid format like mm/dd/yyyy then those 
      #     # will be NA in formattedTables but they're not blank. They would've been flagged as invalid dates already. 
      #     # Eliminate them from this error:
      #     if (class(uploadedTables()[[tableName]][,dateVarName])=="character"){
      #        recordsWithBlankDate <- recordsWithBlankDate %>% filter(.[dateVarName] == "")
      #     }
      #     
      #     message <- paste0("If 1 (Yes) is recorded for ", yVarName," then a valid date should be provided for ",
      #                       dateVarName)
      #     errorFrame <- addToErrorFrame(errorFrame, recordsWithBlankDate, dateVarName, 
      #                                   tableName, "_D/_Y Logic", 
      #                                   errorCode = "Logic", "Warn", message, yVarName, 
      #                                   recordsWithBlankDate[,yVarName])
      #   }
      # }
    }
  }
  return(errorFrame)
}

checkSupplementalFields <- function(errorFrame){
  for (tableName in uploadList()$AllDESTables){
    table <- formattedTables()[[tableName]]
    variableName <- unlist(strsplit(names(table)[endsWith(names(table),"2")],"2"), use.names = FALSE)
    if (is.null(variableName)) next
    
    numberedFields <- sort(names(table)[startsWith(names(table),variableName)])
    numberOfNumFields <- length(numberedFields)
  #  if () last character not == numberOfNumFields, problem. JUDY check
    checkIt <- table %>% select(tableIDField[[tableName]],sort(starts_with(variableName)), recordIndex)
    for (index in sort(seq_along(numberedFields[1:numberOfNumFields-1]), decreasing = TRUE)){
      badRecords <- checkIt %>% filter(!is.na(.[numberedFields[[index+1]]])) %>%
        filter(.[numberedFields[[index+1]]] != "Unknown") %>% 
        filter(is.na(.[numberedFields[[index]]]) | .[numberedFields[[index]]] == "Unknown") 
      # now remove records with bad codes in numericFields[[index]] -- those will be NA in formatted table
      # but have already been flagged as bad codes, no need to duplicate error JUDY for now error duplicated
     # JUDY return to this: blankRecords <- uploadedTables()[[tableName]][badRecords$recordIndex,] %>% 
      #  filter(.[numberedFields[[index]]] == "")
     
        if (nrow(badRecords) > 0){
        message <- paste0("If a value is provided for ",
                          numberedFields[[index + 1]],
                          " then ",
                          numberedFields[[index]],
                          " should not be blank, coded as unknown, or an invalid code")
        errorFrame <- addToErrorFrame(errorFrame = errorFrame,
                                      table = uploadedTables()[[tableName]][badRecords$recordIndex,],
                                      field = numberedFields[[index]],
                                      tableName = tableName,
                                      errorType = "Empty supplemental field",
                                      errorCode = "Logic",
                                      severity = "Warn",
                                      message = message,
                                      error_field2 = numberedFields[[index + 1]],
                                      errorValue2 = as.character(uploadedTables()[[tableName]][badRecords$recordIndex,numberedFields[[index + 1]]]))
      }
    }
  }
  return(errorFrame)
}


##trying out janitor package...
check_DAgreementWith_Y2 <- function(errorFrame){
  for (tableName in uploadList()$AllDESTables){
    table <- formattedTables()[[tableName]]
    var_Y <- unlist(strsplit(names(table)[endsWith(names(table),"_Y")],"_Y"), use.names = FALSE)
    var_D <- unlist(strsplit(names(table)[endsWith(names(table),"_D")],"_D"), use.names = FALSE)
    var_Y_D <- var_Y[var_Y %in% var_D]
    if (length(var_Y_D) > 0){
      rule <- paste0("if(!is.na(",var_Y_D,"_D)) (",var_Y_D,"_Y == \"Yes\")")
      
      df <- data.frame(
        rule = c(rule)
      )
      
      v <- validator(.data=df)
      
      names(v) <- var_Y_D
      
      cf <- confront(table,v, na.value=FALSE)
      
      out <- values(cf)
      
      out.df <- as.data.frame(out)
      
      
      for (variable in var_Y_D){
        
        badRecords <-  which(!out.df[,variable])
        if(any(badRecords)){
          message <- paste0("If a date is recorded for ", paste0(variable,"_D")," then ",
                            paste0(variable,"_Y"), " must have a value of 1 (indicating YES)")
          errorFrame <- addToErrorFrame(errorFrame, table[badRecords,], paste0(variable,"_Y"), 
                                        tableName, "_D/_Y Logic", errorCode = "Logic", "Error", message, 
                                        error_field2 = paste0(variable,"_D"), 
                                        errorValue2 = as.character(table[badRecords,paste0(variable,"_D")]))
        }
      }
    }
  }
  return(errorFrame)
}

check_RSagreementWithDate <- function(errorFrame, dateExt){
  for (tableName in uploadList()$AllDESTables){
    table <- uploadedTables()[[tableName]]
    var_RS <- unlist(strsplit(names(table)[endsWith(names(table), "_RS")],"_RS"), use.names = FALSE)
    var_dateExt <- unlist(strsplit(names(table)[endsWith(names(table), dateExt)], dateExt), use.names = FALSE)
    var_RS_dateExt <- var_RS[var_RS %in% var_dateExt]
    for (variable in var_RS_dateExt){
      datesToCheck <- which(!(trimws(table[[paste0(variable, "_RS")]]) == "") &
                              !is.na(table[[paste0(variable, "_RS")]]) )
      badRecords <- which((trimws(table[datesToCheck,paste0(variable, dateExt)])=="") |
                       is.na(table[datesToCheck, paste0(variable, dateExt)]))
      if (length(badRecords) > 0){
        message <- paste0("If a reason is recorded for ", paste0(variable, "_RS"), " then ",
                          paste0(variable, dateExt), " should not be blank")
        errorFrame <- addToErrorFrame(errorFrame, table[datesToCheck,][badRecords,], paste0(variable,"_RS"), 
                                      tableName, "Reason/Date Logic",
                                      errorCode = "Logic", "Error", message, 
                                      error_field2 = paste0(variable, dateExt), 
                                      errorValue2 = as.character(table[datesToCheck,][badRecords,paste0(variable, dateExt)]))
      }
    }
  }
  return(errorFrame)
}

checkPatientVisits <- function(errorFrame){
  tblVIS <- formattedTables()$tblVIS
  badRecords <- tblVIS %>% group_by(PATIENT) %>%
    dplyr::summarize(numVisits = n()) %>% ungroup() %>% 
    filter(numVisits < 2)
  
  warnPatients <- formattedTables()$tblBAS[which(formattedTables()$tblBAS$PATIENT %in% badRecords$PATIENT),]
  errorFrame <- addToErrorFrame(errorFrame, warnPatients, "PATIENT", 
                                "tblBAS", "PATIENT with < 2 visit dates",
                                errorCode = "Logic", "Warn", "This patient has < 2 visit dates recorded in tblVIS")
  return(errorFrame)
}


invalidProgram <- function(errorFrame){
  ### JUDY add check for PROGRAM in tblCENTER
  if ( ("PROGRAM" %in% names(uploadedTables()[["tblBAS"]]))
       & ("tblPROGRAM" %in% uploadList()$AllDESTables) ){
    validPrograms <- uploadedTables()$tblPROGRAM$PROGRAM
    badPrograms <- uploadedTables()[["tblBAS"]] %>% select(PATIENT, PROGRAM) %>% 
       filter(!(PROGRAM %in% validPrograms)) %>% filter(PROGRAM != "") %>% filter(!is.na(PROGRAM))
    if (nrow(badPrograms) > 0){
      message <- paste0("There is no entry for PROGRAM = ", badPrograms$PROGRAM, " in tblPROGRAM")
      errorFrame <- addToErrorFrame(errorFrame, badPrograms, "PROGRAM", "tblBAS", "Invalid PROGRAM", 
                                    errorCode = "Invalid PROGRAM", "Error", message)
    }
  }
  return(errorFrame)
}


divideErrorsByTable <- function(errorFrame){
  tablesWithErrors <- sort(unique(errorFrame$table))
  errorFrameByTable <- 
    lapply(tablesWithErrors, function(tableName){
      thisTableErrors <- errorFrame %>% filter(table==tableName) %>% select_if(function(col){any(col != "", na.rm = TRUE)})
      # columnsToInclude <- unlist(lapply(names(errorFrame), function(x){
      #   if (any(thisTableErrors[[x]] != "", na.rm = TRUE)){
      #     return(x)
      #   }}))
      return(thisTableErrors)
    #  return(thisTableErrors[,columnsToInclude])
    })
  names(errorFrameByTable) <- tablesWithErrors
  return(errorFrameByTable)
}



addProgramToErrorFrame <- function(errorFrame){
  # errorFrame <- left_join(errorFrame, formattedTables()$tblBAS[,c("PATIENT","PROGRAM")], by = c("idValue" = "PATIENT"))
  # rowsMissingProgram <- which(is.na(errorFrame$PROGRAM))
  # errorFrame$PROGRAM[rowsMissingProgram] <- "Unknown"
  return(errorFrame)
}

# Create a table of the number of VALID patients included in each table
findPatients <- function(){
  tablesToCheck <- intersect(uploadList()$AllDESTables, patientShouldAppearInThese)
  programs <- unique(formattedTables()$tblBAS$PROGRAM)
  
  if (length(tablesToCheck)>0){
    row <- 0
    appearanceSummary <- initializeAppearanceSummary(programs, tablesToCheck)
    for (programName in unique(formattedTables()$tblBAS$PROGRAM)){
      patientsInProgram <- formattedTables()$tblBAS$PATIENT[get("PROGRAM", formattedTables()$tblBAS) == programName]
      numberOfPatientsInProgram <- length(patientsInProgram)
      for (tableName in tablesToCheck){
        row <- row+1
        appearanceSummary$PROGRAM[row] <- programName
        appearanceSummary$table[row] <- tableName
        patientIDsInTable <- sum((patientsInProgram %in% formattedTables()[[tableName]][["PATIENT"]]), na.rm = TRUE)
        percent <-  round(100*patientIDsInTable/numberOfPatientsInProgram,1) 
        appearanceSummary$number[row] <- patientIDsInTable
        appearanceSummary$percent[row] <- percent
      }
    }
  }
  else appearanceSummary <- NULL
  return(appearanceSummary)
}

sumThem <- function(x,y){
  out <- rowSums(data.frame(x,y), na.rm=TRUE)
  return(out)
}


combineMissingAndUnknownByProgram <- function(missing, unknown){
  if (is_empty(missing) || is_empty(unknown)) return(NULL)
  summary <- full_join(missing, unknown, by=c("PROGRAM","table","variable")) %>% 
    mutate(category = "missingOrUnknown") %>% 
    mutate(number = sumThem(number.x,number.y)) %>% 
    mutate(percent = sumThem(percent.x,percent.y)) %>% 
    select(-ends_with(".x")) %>% select(-ends_with(".y")) %>% 
    filter(variable %in% c(interesting,allRequiredVariables))

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




