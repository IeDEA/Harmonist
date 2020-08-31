dataQualityChecks <- function(errorFrame, resources){
  # check numeric variables to confirm only numeric content
  temp <- updateModal("Checking numeric values",1,"Files read and formatted")
  errorFrame <- checkNumericValues(errorFrame, resources)

  # check date format, future dates,etc
  temp <- updateModal("Checking date logic and date format", temp$num, temp$last)
  errorFrame <- checkDatesInTables(errorFrame, resources)
  
  # check for missing values. Store missing required variables in errorFrame, all missing in missingFrame
  temp <- updateModal("Checking for missing values",temp$num, temp$last)
  errorFrame <- findMissingRequiredValues(errorFrame, resources)
  
  #cat("Memory usage: ", pryr::mem_used())
  # check coded variables
  temp <- updateModal("Checking coded variables",temp$num, temp$last)
  errorFrame <- checkCodedVariables(errorFrame, resources)
  
  # Check lab values for units and out-of-range values
  temp <- updateModal("Checking lab values", temp$num, temp$last)
  errorFrame <- checkLabValues(errorFrame, resources)
  
  # check all tables with PATIENT variable for PATIENTs that aren't in tblBAS
  temp <- updateModal("Checking tables for Patient IDs that don't exist in tblBAS", temp$num, temp$last)
  errorFrame <- PatientIDChecks(errorFrame, resources)
  
  # compare all dates to global dates
  temp <- updateModal("Comparing all dates to BIRTH_D, DEATH_D, DROP_D, and L_ALIVE_D", temp$num, temp$last)
  errorFrame <- globalDateChecksBefore(errorFrame, resources)
  errorFrame <- globalDateChecksAfter(errorFrame, resources)
  
  # check for duplicate records in all tables
  temp <- updateModal("Checking for duplicate records in tables", temp$num, temp$last)
  errorFrame <- duplicateRecordChecks(errorFrame, resources)
  
  # check for correct date order within tables
  temp <- updateModal("Checking for correct sequence for start dates and end dates", temp$num, temp$last)
  errorFrame <- withinTableDateOrder(errorFrame, resources)
  
  # check for patients with RECART_Y Yes but no entry in tblART
  temp <- updateModal("Checking for patients listed as receiving ART but not included in tblART", 
                      temp$num, temp$last)
  errorFrame <- findPatientsMissingFromtblART(errorFrame, resources)
  
  # check for possible typos in height: height that decreases rapidly
  if ("tblVIS" %in% tablesAndVariables$tablesToCheck){
    temp <- updateModal("Checking for possible typos in HEIGH: height values that decrease", temp$num, temp$last)
    errorFrame <- checkForDecreasingHeight(errorFrame, resources)
    
    # check for conflicting WHO stage on same date
    if ("WHO_STAGE" %in% names(formattedTables()$tblVIS)){
      temp <- updateModal("Checking for conflicting WHO_STAGE on the same date", temp$num, temp$last)
      errorFrame <- checkForConflictingStages(errorFrame, "WHO_STAGE", resources)
    }
    
    # check for conflicting CDC stage on same date
    if ("CDC_STAGE" %in% names(formattedTables()$tblVIS)){
      temp <- updateModal("Checking for conflicting CDC_STAGE on the same date", temp$num, temp$last)
      errorFrame <- checkForConflictingStages(errorFrame, "CDC_STAGE", resources)
    }
    
    # check for patients with less than 2 visit dates JUDY revisit
    #  temp <- updateModal("Checking for patients with less than 2 visit dates", temp$num, temp$last)
    #  errorFrame <- checkPatientVisits(errorFrame, resources)
  }
  
  # check for agreement between _D and _Y variables
  temp <- updateModal("Checking for agreement within _D and _Y pairs (AIDS_Y/AIDS_D, etc)", temp$num, temp$last)
  errorFrame <- check_DAgreementWith_Y(errorFrame, resources)
  
  # check for agreement between _ED and _RS
  temp <- updateModal("Checking for agreement in Date/Reason pairs (ART_ED/ART_RS, DROP_D/DROP_RS etc)", temp$num, temp$last)
  errorFrame <- check_RSagreementWithDates(errorFrame, resources)

  # document in the errorFrame any tables found to be blank
  temp <- updateModal("Checking for tables with no records", temp$num, temp$last)
  errorFrame <- blankTables(resources$finalGroupChoice, errorFrame, resources$tablesAndVariables$blankTables)
  # JUDY add check for _RS1, _RS2 etc logic and other _RS not included in _ED and _RS
  #temp <- updateModal("Checking for supplemental field logic errors", temp$num, temp$last)
  # errorFrame <- checkSupplementalFields(errorFrame)
  
  # check for PROGRAM entries not included in tblPROGRAM 
  if ("tblPROGRAM" %in% tablesAndVariables$tablesToCheck){
    temp <- updateModal("Checking for PROGRAM entries that don't exist in tblPROGRAM", 
                        temp$num, temp$last)
    errorFrame <- invalidProgram(errorFrame, resources)
    cat("Session:", isolate(sessionID())," errorFrame has ",
        nrow(errorFrame), " rows", "\n", file = stderr())  
  }
  temp <- updateModal("Checking for deprecated IeDEA DES variables in dataset", 
                      temp$num, temp$last)
  errorFrame <- addDeprecatedWarning(errorFrame, resources)
  
  # bind list of errors into one detailed data frame
  updateModal("Aggregating errors and warnings")
    errorFrame <- data.table::rbindlist(errorFrame, use.names = TRUE, fill = TRUE)
  cat("Session:", isolate(sessionID())," errorFrame binding complete\n", file = stderr())  
  
  if (!is_empty(errorFrame)){
    cat("Session:", isolate(sessionID())," there are errors","\n", file = stderr())  
    
    groupBy <- resources$finalGroupChoice
    cat("Session:", isolate(sessionID())," replacing blank program names with Unknown", "\n", file = stderr())  
    
    errorFrame$PROGRAM <- sanitizeNames(errorFrame$PROGRAM)
    errorFrame$PROGRAM[safeTrimWS(errorFrame$PROGRAM) == ""] <- NA
    errorFrame$PROGRAM[is.na(errorFrame$PROGRAM)] <- "Unknown"
    
    if (groupBy != "PROGRAM"){
      errorFrame[[groupBy]] <- sanitizeNames(errorFrame[[groupBy]])
      errorFrame[[groupBy]][safeTrimWS(errorFrame[[groupBy]]) == ""] <- NA
      errorFrame[[groupBy]][is.na(errorFrame[[groupBy]])] <- "Unknown"
    }
    cat("Session:", isolate(sessionID())," about to check for NA in errorFrame","\n", file = stderr())  
    
    errorFrame[is.na(errorFrame)] <- ""
  }
  
  return(errorFrame = errorFrame)
}

summarizeAllErrors <-  function(errorFrame, groupVar, resources){
  print(Sys.time())
  print("errorsByTable")
  errorsByTable <- divideErrorsByTable(errorFrame)
  print(Sys.time())
  print("summarize errors")
  summaryFrames <- summarizeErrors(errorFrame, resources$formattedTables)
  print(Sys.time())
  print("summarize codes by prog")
  unknownCodeSummaryByGroup <- summarizeUnknownCodesByGroup(groupVar, resources)
  print(Sys.time())
  print("unknown code Summary")
  
  unknownCodeSummary <- summarizeUnknownCodes(unknownCodeSummaryByGroup)
  print(Sys.time())
  print("missing by program Summary")
  missingSummaryFrameByGroup <- summarizeMissingByGroup(groupVar, errorFrame, resources, summaryFrames)
  print(Sys.time())
  print("missing Summary")
  missingSummaryFrame <- summarizeMissing(missingSummaryFrameByGroup)
  print(Sys.time())
  print("missing and unknown Summary by program")
  
  missingAndUnknownByGroup <- combineMissingAndUnknownByGroup(missingSummaryFrameByGroup,
                                                              unknownCodeSummaryByGroup,
                                                              groupVar)
  print(Sys.time())
  print("missing and unknown Summary")
  missingAndUnknown <- combineMissingAndUnknown(missingSummaryFrame, unknownCodeSummary)
  print(Sys.time())
  return(list(errorsByTable = errorsByTable,
              summaryFrames = summaryFrames,
              missingSummaryFrame = missingSummaryFrame,
              missingSummaryFrameByGroup = missingSummaryFrameByGroup,
              unknownCodeSummary = unknownCodeSummary,
              unknownCodeSummaryByGroup = unknownCodeSummaryByGroup,
              missingAndUnknown = missingAndUnknown,
              missingAndUnknownByGroup = missingAndUnknownByGroup
  ))
}
