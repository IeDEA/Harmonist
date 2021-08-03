checkBPValues <- function(errorFrame, table, groupVar, resources){
  tableName <- "tblLAB_BP"
  bpMeas <- intersect(c("BP_SYS","BP_DIA"), names(table))
  #if the table doesn't actually include any blood pressure results, return
  if (is_empty(bpMeas)) return(errorFrame)
  #if it does contain results, units column should be present. If no units column, 
  # if (!exists("BP_U", table)){
  #   #TYPE OF ERROR/WARNING HERE Judy
  #   errorFrame <- addGeneralError(groupVar = groupVar, errorFrame = errorFrame,
  #                                 field = "BP_U", tableName = tableName,
  #                                 errorType = "Missing units column in tblLAB_BP",
  #                                 errorCode = "3???",
  #                                 severity = "Warning",
  #                                 message = "Units should be provided for blood pressure measurements")
  #   return(errorFrame)
  # }
  
  # if we're continuing, we know that we have at least one blood pressure value column and we do have a 
  # units column
  # ### If BP_U is *required* in DES, we know that BP_U exists, required to proceed to dq checks
  ###### but just in case BP_U later is NOT required, make sure BP_U column exists
  #find records missing or invalid units
  if (!exists("BP_U", table)) return(errorFrame)
  
  recordsMissingUnits <- table$BP_U == "Missing"
  
  if (any(recordsMissingUnits, na.rm = TRUE)){
    message <- paste0("Valid units must be provided for blood pressure values.")
    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  groupVar, errorFrame, 
                                  table[which(recordsMissingUnits),],"BP_U", 
                                  tableName, 
                                  errorCode = "1.4", 
                                  errorType = "Missing Units", 
                                  severity = "Warning", message)
  }
  
  #check BP values on records containing units
  recordsWithUnits <- table[!recordsMissingUnits,]
  for (units in names(BPLabLimits)){
    unitLimits <- get(units, BPLabLimits)
    upper <- unitLimits$upper
    lower <- unitLimits$lower
    unitCode <- unitLimits$unitCode
    theseUnits <- recordsWithUnits %>% filter(BP_U==units)
    for (labTest in bpMeas){
      tooHighRecords <- theseUnits[[labTest]] > upper
      if (any(tooHighRecords, na.rm=TRUE)){
        message <- paste0("The maximum value allowed for ", labTest, " is ",upper,
                          " when the units are ", units, 
                          " as indicated by BP_U = ", unitCode, ".")
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      groupVar, errorFrame, theseUnits[tooHighRecords,], labTest, tableName, 
                                      errorType = "Value Above Expected Range", 
                                      errorCode = "2.2c", 
                                      "Warning", message,
                                      error_field2 = "BP_U", error2 = unitCode)
        
      }
      recordsToCheck <- theseUnits[!tooHighRecords,]
      tooLowRecords <- recordsToCheck[[labTest]] < lower
      if (any(tooLowRecords, na.rm = TRUE)){
        message <- paste0("The minimum value allowed for ", labTest, " is ",lower,
                          " when the units are ", units, 
                          " as indicated by BP_U = ", unitCode, ".")
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      groupVar, errorFrame, recordsToCheck[tooLowRecords,], labTest, tableName, 
                                      "Value Below Expected Range", errorCode = "2.2c", 
                                      "Warning", message, 
                                      error_field2 = "BP_U", error2 = unitCode)
      }
    }
  }
  return(errorFrame)
}

checkCD4Values <- function(errorFrame, table, groupVar, resources){
  tableName <- "tblLAB_CD4"
  if (!exists("CD4_V", table)) return(errorFrame)
  #### now that CD4_U is *required* we don't need to check for this
  # now we know that cd4 values are present, in which case, units column should be present:
  # if (!exists("CD4_U", table)){
  #   errorFrame <- addGeneralError(groupVar = groupVar, errorFrame = errorFrame,
  #                                 field = "CD4_U", tableName = tableName,
  #                                 errorType = "Missing Units",
  #                                 errorCode = "3",
  #                                 severity = "Warning",
  #                                 message = "Units should be provided for CD4 values")
  #   return(errorFrame)
  # }
  
  # if CD4_U is %, out of range values will be flagged later
  # if CD4_U is cells/mm<sup>3</sup> then CD4_V shouldn't exceed upper limit defined in definitions.R
  recordsToCheck <- table %>% filter(CD4_U == "cells/mm<sup>3</sup>") 
  #negative values are ok: they indicate (less than) detection limit
  tooHighRecords <- recordsToCheck %>% filter(CD4_V > CD4$upperLimit)
  if (nrow(tooHighRecords)>0){
    severity <- "Warning"
    message <- paste0("CD4_V shouldn't exceed ", CD4$upperLimit, " when the unit = cells/mm3.")
    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  groupVar, errorFrame, 
                                  tooHighRecords, 
                                  "CD4_V", "tblLAB_CD4", 
                                  "Value Above Expected Range", 
                                  errorCode = "2.2c", severity, message)
  }
  return(errorFrame)
}

# Check for records with lab values but missing lab units.
checkLabValues <- function(errorFrame, resources){
  if ("tblLAB_BP" %in% resources$tablesAndVariables$tablesToCheck){
    errorFrame <- checkBPValues(errorFrame, resources$formattedTables$tblLAB_BP, resources$finalGroupChoice, resources)
  }
  
  if ("tblLAB_CD4" %in% resources$tablesAndVariables$tablesToCheck){
    errorFrame <- checkCD4Values(errorFrame, resources$formattedTables$tblLAB_CD4, resources$finalGroupChoice, resources)
  }
  
  labTablesToCheck <- intersect(labTablesRequiringUnits, resources$tablesAndVariables$tablesToCheck)
  
  for (tableName in labTablesToCheck){
    thisTable <- get(tableName, resources$formattedTables)
    labValue <- names(thisTable)[endsWith(names(thisTable),"_V")]
    # if no labValue field in table, check next table
    if (length(labValue)==0) next
    labName <- strsplit(labValue,"_V")[[1]]
    if (exists(paste0(labName,"_U"), thisTable)){
      labUnits <- paste0(labName,"_U")
      # since using formatted version of table, and since units are coded values, any missing 
      # units are labeled "Missing". First find the records with non-missing lab values.
      # Can probably replace with is.na since lab values are always numeric... JUDY
      recordsMissingUnits <- thisTable %>% 
        filter(!is_blank_or_NA_elements(!! rlang::sym(labValue) )) %>% 
        filter(!! rlang::sym(labUnits) == "Missing")
      
      if (nrow(recordsMissingUnits)>0){
        severity <- "Warning"
        message <- "Lab Units must be provided for lab values."
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      resources$finalGroupChoice,
                                      errorFrame, 
                                      recordsMissingUnits, 
                                      labUnits, tableName, 
                                      "Missing Units",
                                      errorCode = "1.4", severity, message, 
                                      error_field2 = labValue, 
                                      error2 = as.character(recordsMissingUnits[,labValue]))
      }
      recordsAboveRangePercent <- thisTable %>%
        filter(!! rlang::sym(labUnits) == "%") %>% 
        filter(!! rlang::sym(labValue) > 100)
      if (nrow(recordsAboveRangePercent)>0){
        severity <- "Error"
        message <- paste0(labValue," shouldn't exceed 100 when the units = %.")
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      resources$finalGroupChoice, errorFrame, 
                                      resources$uploadedTables[[tableName]][recordsAboveRangePercent$recordIndex, ],
                                      labValue, tableName, "Value Above Expected Range", 
                                      errorCode = "2.2c", severity, message)
      }
    }
  }
  return(errorFrame)
}






findPatientsMissingFromtblART <- function(errorFrame, resources){
  if (!"tblART" %in% resources$tablesAndVariables$tablesToCheck) return(errorFrame)
  if (!exists("RECART_Y", resources$formattedTables[[indexTableName]])) return(errorFrame)
  patientsOnART <- resources$formattedTables[[indexTableName]] %>% 
    filter(RECART_Y == "Yes") %>% select(!!patientVarSym, recordIndex)
  
  patientsMissing <- !patientsOnART[[patientVar]] %in% resources$formattedTables$tblART[[patientVar]]
  
  if (!any(patientsMissing)) return(errorFrame)
  
  # add entries to errorFrame for patients that are listed as RECART_Y == Yes 
  # but with no entry in tblART
  
  badRecords <- resources$uploadedTables[[indexTableName]][patientsOnART$recordIndex[which(patientsMissing)],]
  message <- paste0("This patient is listed in ", indexTableName, " as recieving ART (RECART_Y=1) but has no entry in tblART")
  errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                resources$finalGroupChoice, errorFrame,
                                badRecords, "RECART_Y", indexTableName, 
                                "Conflict between RECART_Y and tblART",
                                errorCode = "2.3e",
                                "Warning", message)
  return(errorFrame)
}

check_DAgreementWith_Y <- function(errorFrame, resources){
  for (tableName in resources$tablesAndVariables$tablesToCheck){
    table <- resources$formattedTables[[tableName]]
    var_Y <- unlist(strsplit(names(table)[endsWith(names(table),"_Y")],"_Y"), use.names = FALSE)
    var_D <- unlist(strsplit(names(table)[endsWith(names(table),"_D")],"_D"), use.names = FALSE)
    var_Y_D <- var_Y[var_Y %in% var_D]
    for (variable in var_Y_D){
      yVarName <- paste0(variable,"_Y")
      dateVarName <- paste0(variable, "_D")
      #error scenario 1: If a the _D field has a date but the _Y is anything other than 1 (Yes)
      badRecords <- table %>% filter(!is.na(!!rlang::sym(dateVarName))) %>% 
        filter(!(!!rlang::sym(yVarName)=="Yes")) %>% pull(recordIndex)
      if(length(badRecords)>0){
        message <- paste0("If a date is recorded for ", dateVarName," then ",
                          yVarName, " must have a value of 1 (Yes).")
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      resources$finalGroupChoice, errorFrame, 
                                      resources$uploadedTables[[tableName]][badRecords,], 
                                      dateVarName, 
                                      tableName, 
                                      paste0("Y/N data in conflict with date"),# (", yVarName, " not YES when date provided for ", dateVarName, ")"), 
                                      errorCode = "2.3c",
                                      "Error", message, 
                                      error_field2 = yVarName, 
                                      error2 = as.character(resources$uploadedTables[[tableName]][badRecords,yVarName]))
      }
      # JUDY: revisit -- check with Bev
      # error scenario 2: If a the _Y field is Yes but the _D field is blank
      # for now I am removing that check-- it seems common to have a value for AIDS_Y with no AIDS_D, etc
      
    }
  }
  return(errorFrame)
}

check_RSagreementWithDates <- function(errorFrame, resources){
  for (tableName in resources$tablesAndVariables$tablesToCheck){
    formattedTable <- resources$formattedTables[[tableName]]
    table <- resources$uploadedTables[[tableName]]
    idFields <- tableIDField[[tableName]]
    varNames <- names(formattedTable)
    allvar_RS <- endsWith(varNames, "_RS")
    if (!any(allvar_RS, na.rm = TRUE)) next
    # now we know that at least one REASON variable exists
    # check first for START_RS/SD pair, then _RS/_SD or _D pair
    for (var_RSName in varNames[allvar_RS]){
      if (endsWith(var_RSName, "START_RS")){
        baseVarName <- str_sub(var_RSName, end = -(1 + nchar("START_RS")))
        dateName <- paste0(baseVarName, sdExt)
      } else if (endsWith(var_RSName, "_RS")){
        baseVarName <- str_sub(var_RSName, end = -(1 + nchar("_RS")))
        endDateNames <- paste0(baseVarName, c(edExt, "_D"))
        if (!any(endDateNames %in% varNames)) {
          dateName <- NULL
        } else {
          dateName <- endDateNames[which(endDateNames %in% varNames)][[1]] #shouldn't be more than 1! but edExt best
        }
      }
      if (is.null(dateName) || (!dateName %in% varNames)) next
      # now we know a date/reason pair exists: var_RSName, dateName
      # find records in formattedTable where RS is *not* missing or Unknown but the date *is* missing 
      
      badTable <- formattedTable %>% filter(! (!!rlang::sym(var_RSName) %in% c("Missing", "Unknown"))) %>% 
        filter(is.na(!!rlang::sym(dateName)))
      
      if (nrow(badTable) == 0) next
      # now we know bad records probably exist but we shouldn't flag them if a date was entered 
      # but was in the wrong format, etc.To confirm that we need to check those records in the original,
      # unformatted data table:
      originalBadData <- table[badTable$recordIndex,]
      
      notBlankDates <- !is_blank_or_NA_elements(originalBadData[[dateName]])
      if (all(notBlankDates)) next
      if (any(notBlankDates, na.rm = TRUE)){
        originalBadData <- originalBadData[which(!notBlankDates),]
      }
      
      if (nrow(badTable) > 0){
        message <- paste0("If a reason is recorded for ", var_RSName, " then ",
                          dateName, " should not be blank.")
        errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                      resources$finalGroupChoice, 
                                      errorFrame, 
                                      originalBadData, 
                                      var_RSName, 
                                      tableName, "Reason provided but date missing",
                                      errorCode = "2.3d", "Error", message, 
                                      error_field2 = dateName, 
                                      error2 = as.character(originalBadData$dateName))
      }
    }
  }
  return(errorFrame)
}

####################################################################
# warn user about deprecated codes #################################

checkForDeprecatedCodes <- function(errorFrame, resources){
  for(tableName in resources$tablesAndVariables$tablesToCheck){
    table <- get(tableName, resources$uploadedTables)
    formattedTable <- get(tableName, resources$formattedTables)
    variablesInTable <- names(formattedTable)
    codedFieldNames <- intersect(variablesInTable, findVariablesMatchingCondition(tableName, tableDef, "has_codes","Y"))
    if (is_empty(codedFieldNames)) next
    for (codedField in codedFieldNames){
      deprecatedRows <- which(str_detect(formattedTable[[codedField]], "deprecat"))
      if (is_empty(deprecatedRows)) next
      message <- formattedTable[deprecatedRows, codedField]
      errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                    resources$finalGroupChoice, errorFrame, 
                                    resources$uploadedTables[[tableName]][deprecatedRows,], 
                                    codedField, 
                                    tableName, 
                                    "Deprecated code", 
                                    errorCode = "1.2c",
                                    "Warning", message
      )
    }
  }
  return(errorFrame)
}


####################################################################
# compare dates with CLOSE_D and OPEN_D #################################
# 
# checkForDeprecatedCodes <- function(errorFrame, resources){
#   for(tableName in resources$tablesAndVariables$tablesToCheck){
#     table <- get(tableName, resources$uploadedTables)
#     formattedTable <- get(tableName, resources$formattedTables)
#     variablesInTable <- names(formattedTable)
#     codedFieldNames <- intersect(variablesInTable, findVariablesMatchingCondition(tableName, tableDef, "has_codes","Y"))
#     if (is_empty(codedFieldNames)) next
#     for (codedField in codedFieldNames){
#       deprecatedRows <- which(str_detect(formattedTable[[codedField]], "deprecat"))
#       if (is_empty(deprecatedRows)) next
#       message <- formattedTable[deprecatedRows, codedField]
#       errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
#                                     resources$finalGroupChoice, errorFrame, 
#                                     resources$uploadedTables[[tableName]][deprecatedRows,], 
#                                     codedField, 
#                                     tableName, 
#                                     "Deprecated code", 
#                                     errorCode = "1.2c",
#                                     "Warning", message
#       )
#     }
#   }
#   return(errorFrame)
# }
# 


#NOT CURRENTLY IMPLEMENTED
checkPatientVisits <- function(errorFrame, resources){
  tblVIS <- resources$formattedTables$tblVIS
  badRecords <- tblVIS %>% group_by(!!patientVarSym) %>%
    dplyr::summarize(numVisits = n()) %>% ungroup() %>% 
    filter(numVisits < 2)
  
  warnPatients <- resources$formattedTables[[indexTableName]][which(resources$formattedTables[[indexTableName]][[patientVar]] %in% badRecords[[patientVar]]),]
  errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                resources$finalGroupChoice, errorFrame, warnPatients, patientVar, 
                                indexTableName, paste0(patientVar, " with < 2 visit dates"),
                                errorCode = "3", "Warning", 
                                "This patient has < 2 visit dates recorded in tblVIS.")
  return(errorFrame)
}
