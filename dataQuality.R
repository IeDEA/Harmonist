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

  # check coded variables
  temp <- updateModal("Checking coded variables",temp$num, temp$last)
  errorFrame <- checkCodedVariables(errorFrame, resources)
  
  # check all tables with PATIENT variable for PATIENTs that aren't in index table
  temp <- updateModal(paste0(
    "Checking tables for Patient IDs that don't exist in ",
    indexTableName), temp$num, temp$last)
  errorFrame <- PatientIDChecks(errorFrame, resources)
  
  # compare all dates to global dates as defined in globalDateBeforeChecks and 
  # globalDateAfterChecks json files
  temp <- updateModal("Comparing all dates to birthdate, death date, etc.", 
                      temp$num, temp$last)
  errorFrame <- globalDateChecksBefore(errorFrame, resources)
  errorFrame <- globalDateChecksAfter(errorFrame, resources)
  
  # check for duplicate records in all tables
  temp <- updateModal("Checking for duplicate records in tables", temp$num, temp$last)
  errorFrame <- duplicateRecordChecks(errorFrame, resources)
  
  # check for correct date order within tables: start date/end date pairs and others
  # defined in withinTableDateOrder json file
  temp <- updateModal("Checking for correct sequence for start dates and end dates", temp$num, temp$last)
  errorFrame <- withinTableDateOrder(errorFrame, resources)
  
  # check for possible typos in height: height that decreases rapidly
  if (!is.null(heightTableName)){
    if (heightTableName %in% tablesAndVariables$tablesToCheck){
      temp <- updateModal("Checking for possible typos in height: height values that decrease", temp$num, temp$last)
      errorFrame <- checkForDecreasingHeight(errorFrame, resources)
    }
  }
  

  # document in the errorFrame any tables found to be blank
  temp <- updateModal("Checking for tables with no records", temp$num, temp$last)
  errorFrame <- blankTables(resources$finalGroupChoice, errorFrame, resources$tablesAndVariables$blankTables)
  
  # add warning for deprecated variables
  temp <- updateModal("Checking for deprecated variables in dataset", 
                      temp$num, temp$last)
  errorFrame <- addDeprecatedWarning(errorFrame, resources)
  
  # check for PROGRAM entries not included in tblPROGRAM 
  if (!is.null(defGroupTableName)){
    if (defGroupTableName %in% tablesAndVariables$tablesToCheck){
      modalMessage <- paste0("Checking for ",
                             defGroupVar,
                             " entries that don't exist in ",
                             defGroupTableName)
      temp <- updateModal(modalMessage, temp$num, temp$last)
      errorFrame <- invalidProgram(errorFrame, resources)
      cat("Session:", isolate(sessionID())," errorFrame has ",
          nrow(errorFrame), " rows", "\n", file = stderr())  
    }
  }
  
  #############################################################################

  #### Domain-specific data check functions #####
  source("specificDataChecks.R", local = TRUE)
  # call domain specific data quality checks and add to errorFrame
  source("doSpecificChecks.R", local = TRUE)
    
  ################################################################################
  ##### ADD and CALL CUSTOM DATA QUALITY CHECKS HERE #############################
    
  source("customChecks.R", local = TRUE)

  ################################################################################
    

  ################################################################################
  # bind list of errors into one detailed data frame
  
  updateModal("Aggregating errors and warnings")
    errorFrame <- data.table::rbindlist(errorFrame, use.names = TRUE, fill = TRUE)
  cat("Session:", isolate(sessionID())," errorFrame binding complete\n", file = stderr())
  ###################################################################################
  
  if (!is_empty(errorFrame)){
    cat("Session:", isolate(sessionID())," there are errors","\n", file = stderr())  
    
    groupBy <- resources$finalGroupChoice
    cat("Session:", isolate(sessionID())," replacing blank program names with Unknown", "\n", file = stderr())  
    errorFrame[[defGroupVar]] <- sanitizeNames(errorFrame[[defGroupVar]])
    errorFrame[[defGroupVar]][safeTrimWS(errorFrame[[defGroupVar]]) == ""] <- NA
    errorFrame[[defGroupVar]][is.na(errorFrame[[defGroupVar]])] <- "Unknown"
    
    if (groupBy != defGroupVar){
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
