dataQualityChecks <- function(errorFrame){
  
 
  # check numeric variables to confirm only numeric content
  temp <- updateModal("Checking numeric values",1,"Files read and formatted")
  errorFrame <- checkNumericValues(errorFrame)
  if (is.null(errorFrame)) return(NULL)   #this is for height errors -- revisit, esp if convert to rbindlist
  
  # check date format, future dates,etc
  temp <- updateModal("Checking date logic and date format", temp$num, temp$last)
   errorFrame <- checkDatesInTables(errorFrame)
  
  # check for missing values. Store missing required variables in errorFrame, all missing in missingFrame
  temp <- updateModal("Checking for missing values",temp$num, temp$last)
  result <- findMissingValues(errorFrame)
  errorFrame <-  result$errorFrame
  missingFrame <- result$missingFrame
  
  # check coded variables
  temp <- updateModal("Checking coded variables",temp$num, temp$last)
  errorFrame <- checkCodedVariables(errorFrame)
  
  # Check lab values for units and out-of-range values
  temp <- updateModal("Checking lab values", temp$num, temp$last)
  errorFrame <- checkLabValues(errorFrame)
  
  # check all tables with PATIENT variable for PATIENTs that aren't in tblBAS
  temp <- updateModal("Checking tables for Patient IDs that don't exist in tblBAS", temp$num, temp$last)
  errorFrame <- PatientIDChecks(errorFrame)
  
  # compare all dates to global dates
  temp <- updateModal("Comparing all dates to BIRTH_D, DEATH_D, DROP_D, and L_ALIVE_D", temp$num, temp$last)
  errorFrame <- globalDateChecksBefore(errorFrame)
  errorFrame <- globalDateChecksAfter(errorFrame)
  
  # check for duplicate records in all tables
  temp <- updateModal("Checking for duplicate records in tables", temp$num, temp$last)
  errorFrame <- duplicateRecordChecks(errorFrame)
  
  # check for correct date order within tables
  temp <- updateModal("Checking for correct sequence for start dates and end dates", temp$num, temp$last)
  errorFrame <- withinTableDateOrder(errorFrame)
  
  # check for possible typos in height: height that decreases rapidly
  if ("tblVIS" %in% uploadList()$AllDESTables){
    temp <- updateModal("Checking for possible typos in HEIGH: height values that decrease", temp$num, temp$last)
    errorFrame <- checkForDecreasingHeight(errorFrame)
    
    # check for conflicting WHO stage on same date
    if ("WHO_STAGE" %in% names(formattedTables()$tblVIS)){
      temp <- updateModal("Checking for conflicting WHO_STAGE on the same date", temp$num, temp$last)
      errorFrame <- checkForConflictingStages(errorFrame, "WHO_STAGE")
    }
    
    # check for conflicting CDC stage on same date
    if ("CDC_STAGE" %in% names(formattedTables()$tblVIS)){
      temp <- updateModal("Checking for conflicting CDC_STAGE on the same date", temp$num, temp$last)
      errorFrame <- checkForConflictingStages(errorFrame, "CDC_STAGE")
    }
    
    # check for patients with less than 2 visit dates JUDY revisit
  #  temp <- updateModal("Checking for patients with less than 2 visit dates", temp$num, temp$last)
  #  errorFrame <- checkPatientVisits(errorFrame)
  }
  
  # check for agreement between _D and _Y variables
  temp <- updateModal("Checking for agreement within _D and _Y pairs (AIDS_Y/AIDS_D, etc)", temp$num, temp$last)
  errorFrame <- check_DAgreementWith_Y(errorFrame)
  
  # check for agreement between _ED and _RS
  temp <- updateModal("Checking for agreement in Date/Reason pairs (ART_ED/ART_RS, DROP_D/DROP_RS etc)", temp$num, temp$last)
  errorFrame <- check_RSagreementWithDate(errorFrame, "_ED")
  errorFrame <- check_RSagreementWithDate(errorFrame, "_D")
  
  
  # JUDY add check for _RS1, _RS2 etc logic and other _RS not included in _ED and _RS
  #temp <- updateModal("Checking for supplemental field logic errors", temp$num, temp$last)
 # errorFrame <- checkSupplementalFields(errorFrame)
  
  # check for PROGRAM entries not included in tblPROGRAM 
  if ("tblPROGRAM" %in% uploadList()$AllDESTables){
    temp <- updateModal("Checking for PROGRAM entries that don't exist in tblPROGRAM", temp$num, temp$last)
    errorFrame <- invalidProgram(errorFrame)
  }
  
  errorFrame <- data.table::rbindlist(errorFrame, use.names = TRUE, fill = TRUE)
  if (!is_empty(errorFrame)){
    errorFrame$PROGRAM[is.na(errorFrame$PROGRAM)] <- "Unknown"
    errorFrame[is.na(errorFrame)] <- ""
  }

  # if (!is_empty(errorFrame)){
  #   # replace NAs with blanks
  #   errorFrame[is.na(errorFrame)] <- ""
  #   
  #   print("Add program to error frame")
  #   
  #   errorFrame <- addProgramToErrorFrame(errorFrame)
  # }
  return(list(errorFrame = errorFrame,
              missingFrame = missingFrame))
}

summarizeAllErrors <-  function(errorFrame, missingFrame){
  print(Sys.time())
  print("errorsByTable")
  errorsByTable <- divideErrorsByTable(errorFrame)
  print(Sys.time())
  print("summarize errors")
  #This is where memory bombed
  summaryFrames <- summarizeErrors(errorFrame, uploadedTables())
  print(Sys.time())
  print("summarize codes by prog")
  unknownSummaryFrameByProgram <- summarizeUnknownCodesByProgram()
  print(Sys.time())
  print("unknown code Summary")
  
  unknownSummaryFrame <- summarizeUnknownCodes(unknownSummaryFrameByProgram)
  print(Sys.time())
  print("missing by program Summary")
  missingSummaryFrameByProgram <- summarizeMissingByProgram(missingFrame)
  print(Sys.time())
  print("missing Summary")
  missingSummaryFrame <- summarizeMissing(missingFrame)
  print(Sys.time())
  print("missing and unknown Summary by program")
  
  missingAndUnknownByProgram <- combineMissingAndUnknownByProgram(missingSummaryFrameByProgram, unknownSummaryFrameByProgram)
  print(Sys.time())
  print("missing and unknown Summary")
  missingAndUnknown <- combineMissingAndUnknown(missingSummaryFrame, unknownSummaryFrame)
  print(Sys.time())
  return(list(errorsByTable = errorsByTable,
              summaryFrames = summaryFrames,
              missingSummaryFrame = missingSummaryFrame,
              missingSummaryFrameByProgram = missingSummaryFrameByProgram,
              unknownSummaryFrame = unknownSummaryFrame,
              unknownSummaryFrameByProgram = unknownSummaryFrameByProgram,
              missingAndUnknown = missingAndUnknown,
              missingAndUnknownByProgram = missingAndUnknownByProgram
              ))
}
