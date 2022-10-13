##### IeDEA-Specific Data Quality Checks ####################################
if (networkName == "IeDEA"){
  # Check lab values for units and out-of-range values
  temp <- updateModal("Checking lab values", temp$num, temp$last)
  errorFrame <- checkLabValues(errorFrame, resources)
  
  # check for patients with RECART_Y Yes but no entry in tblART
  temp <- updateModal("Checking for patients listed as receiving ART but not included in tblART", 
                      temp$num, temp$last)
  errorFrame <- findPatientsMissingFromtblART(errorFrame, resources)
  
  # check for agreement between _D and _Y variables
  temp <- updateModal("Checking for agreement within _D and _Y pairs (AIDS_Y/AIDS_D, etc)", 
                      temp$num, temp$last)
  errorFrame <- check_DAgreementWith_Y(errorFrame, resources)
  
  
  # check for agreement between _ED and _RS
  temp <- updateModal("Checking for agreement in Date/Reason pairs (ART_ED/ART_RS, DROP_D/DROP_RS etc)", 
                      temp$num, temp$last)
  errorFrame <- check_RSagreementWithDates(errorFrame, resources)
  
  # add a warning if any codes have been marked as deprecated
  temp <- updateModal("Checking for deprecated codes", temp$num, temp$last)
  errorFrame <- checkForDeprecatedCodes(errorFrame, resources)
  
  if ("supSRN" %in% tablesAndVariables$tablesToCheck){
    temp <- updateModal("Performing SRN checks", temp$num, temp$last)
    errorFrame <- baselineCheckSRN(errorFrame, resources)
    errorFrame <- srnSexCheck(errorFrame, resources)
    errorFrame <- srnBirthDateCheck(errorFrame, resources) 
  }
  
  
  
  # check for patients with less than 2 visit dates JUDY revisit
  # if (visitTableName %in% tablesAndVariables$tablesToCheck){
  #   temp <- updateModal("Checking for patients with less than 2 visit dates", temp$num, temp$last)
  #   errorFrame <- checkPatientVisits(errorFrame, resources)
  # }
  
  # JUDY add check for _RS1, _RS2 etc logic and other _RS not included in _ED and _RS
  #temp <- updateModal("Checking for supplemental field logic errors", temp$num, temp$last)
  # errorFrame <- checkSupplementalFields(errorFrame)

  # check for agreement between _D and _Y variables
  temp <- updateModal("Checking for agreement within _D and _Y pairs (AIDS_Y/AIDS_D, etc)", temp$num, temp$last)
  errorFrame <- check_DAgreementWith_Y(errorFrame, resources)

}

#################################################################################
