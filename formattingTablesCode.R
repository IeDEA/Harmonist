calcAge <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

addAges <- function(formattedTableData){
  if (exists(ageDateVar, formattedTableData[[indexTableName]])){
    formattedTableData[[indexTableName]]$age <- calcAge(formattedTableData[[indexTableName]][[birthDateVar]], 
                                                          formattedTableData[[indexTableName]][[ageDateVar]])
  } else formattedTableData[[indexTableName]]$age <- NA

  return(formattedTableData)
}

addAgeGroupAndGroup <- function(formattedTableData, groupByVar){
  formattedTableData[[indexTableName]]$ageGroup <- NA
  ageGroupLevels <- names(ageGroups)
  for (ageGroup in ageGroupLevels){
    formattedTableData[[indexTableName]][which((formattedTableData[[indexTableName]]$age >= ageGroups[[ageGroup]]$lower) &
                                    (formattedTableData[[indexTableName]]$age < ageGroups[[ageGroup]]$upper + 1) ),
                              "ageGroup"] <- ageGroup
  }
  if (any(is.na(formattedTableData[[indexTableName]]$ageGroup))){
      formattedTableData[[indexTableName]][which(is.na(formattedTableData[[indexTableName]]$ageGroup)),"ageGroup"] <- "Unknown" 
      ageGroupLevels <- c(ageGroupLevels, "Unknown")
  }

  formattedTableData[[indexTableName]]$ageGroup <- factor(formattedTableData[[indexTableName]]$ageGroup, levels = ageGroupLevels)
  
  #groupLevels <- sort(unique(formattedTableData[[indexTableName]][[groupByVar]]))
  #formattedTableData[[indexTableName]][[groupByVar]] <- factor(formattedTableData[[indexTableName]][[groupByVar]],
  #                                               levels = groupLevels)

  # if any indexTableName records is missing defGroupVar or group, add it as "Missing"
  blankDefGroup <- is_blank_or_NA_elements(formattedTableData[[indexTableName]][[defGroupVar]])
  blankGroup <- is_blank_or_NA_elements(formattedTableData[[indexTableName]][[groupByVar]])
  if (any(blankDefGroup)){
    formattedTableData[[indexTableName]][which(blankDefGroup), defGroupVar] <- "Missing"
  }
  if (any(blankGroup)){
    formattedTableData[[indexTableName]][which(blankGroup), groupByVar] <- "Missing"
  }
  for (tableName in tablesAndVariables$tablesToCheckWithPatientID){
    currentTable <- formattedTableData[[tableName]]
    toJoinFromTable <- names(currentTable)
    # no need to add defGroupVar or groupByVar from indexTableName if already in table
    #if (groupByVar %in% names(formattedTableData[[tableName]])) browser() # what if a table has a different group value
    toJoinFromIndex <- c(patientVar, setdiff(c(defGroupVar, groupByVar), toJoinFromTable), "ageGroup")
    gc()
    formattedTableData[[tableName]] <- left_join(currentTable[,toJoinFromTable, drop = FALSE], 
                                                  formattedTableData[[indexTableName]][, toJoinFromIndex, drop = FALSE],
                                                  by = patientVar)
    # if any patients weren't found in indexTableName, the ageGroup = unknown. Add this factor level
    if (any(is.na(formattedTableData[[tableName]][["ageGroup"]]))){
      cat("replacing NA ages in", tableName, "with Unknown label", "\n", sep = " ", file = stderr())
      formattedTableData[[tableName]][["ageGroup"]] <- forcats::fct_explicit_na(formattedTableData[[tableName]][["ageGroup"]], "Unknown")
    }

    if (any(is.na(formattedTableData[[tableName]][[groupByVar]]))){
      if (is.factor(formattedTableData[[tableName]][[groupByVar]])){
        formattedTableData[[tableName]][[groupByVar]] <- fct_explicit_na(formattedTableData[[tableName]][[groupByVar]], na_level = "Unknown")
      } else {
        formattedTableData[[tableName]][[groupByVar]] <- replace_na(formattedTableData[[tableName]][[groupByVar]], "Unknown")
      }
    }
    rm(currentTable)
  }
  
  ##################################################################################
  # unique to IeDEA ################################################################
  if (networkName == "IeDEA" && "tblLAB_RES" %in% tablesAndVariables$tablesToCheck){
    mainREStable <- formattedTableData$tblLAB_RES[, unique(c("TEST_ID",patientVar,defGroupVar, groupByVar))] %>% 
      distinct(TEST_ID, .keep_all = TRUE) # this is to prevent rows being added during left_join but may eliminate some entries. unique() is to handle case when groupByVar == defGroupVar
    for (tableRESname in intersect(c("tblLAB_RES_LVL_2","tblLAB_RES_LVL_3"),names(formattedTableData))){
      formattedTableData[[tableRESname]] <- left_join(formattedTableData[[tableRESname]],
                                                      mainREStable, 
                                                      by = c("TEST_ID"))
    }
  }
  
  # determine which tables have not been linked to a group, such as pregnancy-related tables
  # For now, link those to group "Unknown"
  result <- lapply(formattedTableData, function(df) !exists(groupByVar, df))
  tablesWithNoGroup <- names(which(unlist(result)))
  for (tableName in tablesWithNoGroup){
    formattedTableData[[tableName]][,unique(c(defGroupVar, groupByVar))] <- "Not assigned"
  }
  return(formattedTableData)
}

# forceModeTables ---------------------------------------------
# This function forces the variable types to conform with the DES, converts invalid codes to NA, 
# and ultimately saves the formatted data as formattedTables()
forceModeTables <- function(groupByVar, uploadedTables){
  formattedTables <- list()
  for (tableName in tablesAndVariables$tablesToCheck){
    updateModal(message = paste0("Formatting ", tableName),
                title = "Preparing files for data quality checks",
                subtitle = NULL)
    print(tableName)
    cat("Formatting", tableName, "\n", sep = " ", file = stderr())

    variableDefs <- get(tableName, tableDef)
    variableNames <- intersect(names(tableDef[[tableName]]$variables),
                               names(uploadedTables[[tableName]]))

    # if the current table is indexTable

    if ((tableName == indexTableName)){
      print("1")
      formattedTables[[tableName]] <- uploadedTables[[tableName]][, unique(c(variableNames, groupByVar)), drop = FALSE]
      print("2")
      
    } else formattedTables[[tableName]] <- uploadedTables[[tableName]][, variableNames, drop = FALSE]

    if (exists(patientVar, formattedTables[[tableName]])){ # & 
      formattedTables[[tableName]][[patientVar]] <- as.character(formattedTables[[tableName]][[patientVar]])
    }
   
    for (variableName in variableNames){
      cat("Formatting", variableName, "\n", sep = " ", file = stderr())
      columnClass <- class(formattedTables[[tableName]][[variableName]])[[1]]
      
      # trim white space from character columns
      if (columnClass == "character"){
        formattedTables[[tableName]][[variableName]] <- safeTrimWS(formattedTables[[tableName]][[variableName]])
      }

      # coded variables first. Format as Factor --------------------------------------------
      if (variableDefs$variables[[variableName]]$has_codes == "Y"){
        codeFormat <- variableDefs$variables[[variableName]]$data_format
        codeIndex <- as.numeric(variableDefs$variables[[variableName]]$code_list_ref)
        codeList <- codes[[as.character(codeIndex)]]
        validCodes <- names(codeList)
        codeLabels <- unlist(codeList, use.names = FALSE)
        
        ##########################################################################
        # unique to IeDEA
        # if this variable is units, remove the html codes for superscripts...
        if (networkName == "IeDEA" && endsWith(variableName,"_U")) codeLabels <- removeHTML(codeLabels)
        
        # NUMERIC CODES FIRST ---------------------------------------------------------------
        # if the code list is numeric, convert column to numeric
        if (codeFormat == "Numeric"){
          # this will allow 93.0 and 93 to be the same code
          validCodes <- as.numeric(validCodes)
          # if the uploaded data is already in numeric format, indicate missing and then invalid codes will be NA
          if (is.numeric(formattedTables[[tableName]][[variableName]])){
             formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]],
                                                                    levels = c(validCodes, NA),
                                                                    labels = c(codeLabels, "Missing"),
                                                                    exclude = NULL)
             # now any remaining NA in coded variable vector are due to invalid codes. Make this explicit:
             if (any(is.na(formattedTables[[tableName]][[variableName]]))){
               cat("replacing NA codes in", variableName, "with Invalid Code label", "\n", sep = " ", file = stderr())
               
               formattedTables[[tableName]][[variableName]] <- 
                 forcats::fct_explicit_na(formattedTables[[tableName]][[variableName]], 
                                          "Invalid Code")
             }
          } else {
            # column must be in a different format. Before converting to numeric, check for non numeric entries
            # if any non numeric, non blank entries exist, replace with (fake) numeric code indicating *invalid format*
            if (!is.numeric(formattedTables[[tableName]][[variableName]])){
              
              blankRecords <- is_blank_or_NA_elements(formattedTables[[tableName]][[variableName]])
              nonNumeric <- which(!grepl("^-?\\d*\\.{0,1}\\d+$", 
                                         formattedTables[[tableName]][[variableName]]) &
                                    !blankRecords)
              formattedTables[[tableName]][nonNumeric,variableName] <- codeIndicatingInvalidCodeFormat
            }
            # now all entries should be numeric. Force to numeric type
            formattedTables[[tableName]][[variableName]] <- as.numeric(formattedTables[[tableName]][[variableName]])
            
            # now any missing entries are NA. 
            # The code below will label NA entries as "Missing" and invalid formats as "Invalid Code"
            formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]],
                                                                   levels = c(validCodes, 
                                                                              codeIndicatingInvalidCodeFormat,
                                                                              NA),
                                                                   labels = c(codeLabels, "Invalid Code", "Missing"),
                                                                   exclude = NULL)
            
            # now any remaining NA in coded variable vector are due to invalid codes. Make this explicit:
            if (any(is.na(formattedTables[[tableName]][[variableName]]))){
              badCodeIndices <- which(is.na(formattedTables[[tableName]][[variableName]]))
              formattedTables[[tableName]][[variableName]][badCodeIndices] <- "Invalid Code"
            }
          }
        }
        
        # NOW Character codes -----------------------------------------------------
        else {
          # trim white space
          if (columnClass == "character"){
            formattedTables[[tableName]][[variableName]] <- safeTrimWS(formattedTables[[tableName]][[variableName]])
          } else formattedTables[[tableName]][[variableName]] <- 
              safeTrimWS(as.character(formattedTables[[tableName]][[variableName]]))
          
          if (length(validCodes) == length(unique(codeLabels))){ #####if different, NA must be present? Jeremy
            # first set all blank entries to NA so that Missing level will apply both to blank and NA
            blankIndices <- formattedTables[[tableName]][[variableName]] == ""
            if (any(blankIndices, na.rm = TRUE)){
              formattedTables[[tableName]][[variableName]][which(blankIndices)] <- NA
            }
            # set factor levels
            formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]],
                                                                   levels = c(validCodes, NA), 
                                                                   labels = c(codeLabels, "Missing"),
                                                                   exclude = NULL)
            # now any new NA in coded variable vector are due to invalid codes. Make this explicit:
            if (any(is.na(formattedTables[[tableName]][[variableName]]))){
              cat("replacing NA codes in", variableName, "with Invalid Code label", "\n", sep = " ", file = stderr())
              
              formattedTables[[tableName]][[variableName]] <- 
                forcats::fct_explicit_na(formattedTables[[tableName]][[variableName]], "Invalid Code")
            }
            
          }
          else {
            print("unusual code situation")
            formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]],
                                                                   levels = validCodes, ordered = TRUE)
          }
        }
      }
        
      # Next, numeric variables that aren't coded. Format as Numeric
      else if (variableDefs$variables[[variableName]]$data_format == "Numeric"){
        if (columnClass != "numeric"){
          formattedTables[[tableName]][[variableName]] <- as.numeric(formattedTables[[tableName]][[variableName]])
        }
      }
      
      #Next date variables, format as date
      else if (variableDefs$variables[[variableName]]$data_format == "YYYY-MM-DD"){
        print(variableName)
        print(columnClass)
        if (columnClass == "POSIXct"){
          timeZone <- attributes(formattedTables[[tableName]][[variableName]])[["tzone"]]
          formattedTables[[tableName]][[variableName]] <- as.Date(formattedTables[[tableName]][[variableName]], 
                                                                  format = "%Y-%m-%d", tz = timeZone)
        } else if (columnClass %in% c("Date", "character")){
          formattedTables[[tableName]][[variableName]] <- as.Date(formattedTables[[tableName]][[variableName]], 
                                                                  "%Y-%m-%d")
        } else if (columnClass == "numeric") {
          formattedTables[[tableName]][[variableName]] <- as.Date(formattedTables[[tableName]][[variableName]], 
                                                                  "%Y-%m-%d", origin = "1960-01-01") #SAS origin date
        } else formattedTables[[tableName]][,variableName] <- as.Date(NA)
      }
      
      # next, if the variable should be categorical, set as factor JUDY revisit this. Any reason to do this?
      else if (variableName %in% isFactor){ #in definitions.R
        # in case there are any blank records in an "isFactor" field, don't create another factor level for ""
        print(variableName)
        blankRecords <- safeTrimWS(formattedTables[[tableName]][[variableName]])==""
        if (any(blankRecords, na.rm = TRUE)){
          formattedTables[[tableName]][which(blankRecords),variableName] <- NA
        }
        formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]])
      }
      #next, replace blank defGroupVar/Group entries with NA to facilitate reporting by program
      else if (variableName == groupByVar){
        if (columnClass != "character") {
          formattedTables[[tableName]][[variableName]] <- 
            as.character(formattedTables[[tableName]][[variableName]])
        }
        formattedTables[[tableName]][[variableName]] <- sanitizeNames(formattedTables[[tableName]][[variableName]])
        blankRecords <- safeTrimWS(formattedTables[[tableName]][[variableName]])==""
        if (any(blankRecords, na.rm = TRUE)){
          formattedTables[[tableName]][which(blankRecords),variableName] <- NA
        }
      }
      # any other variables, format as character
      else {
        formattedTables[[tableName]][[variableName]] <- as.character(formattedTables[[tableName]][[variableName]])
      }
    }
  }
  print("about to add ages")  
  formattedTables <- addAges(formattedTables)
  print("about to add age group and group")  
  formattedTables <- addAgeGroupAndGroup(formattedTables, groupByVar)

  # put columns in same order as DES
  for (tableName in tablesAndVariables$tablesToCheck){
    DESVariableNames <- intersect(names(tableDef[[tableName]][["variables"]]),
                                  names(formattedTables[[tableName]]))
    formattedTables[[tableName]] <- select(formattedTables[[tableName]],DESVariableNames, everything())
  }
  #Add index column to keep link between records in uploadedTables and formattedTables
  formattedTables <- lapply(formattedTables, function(x){
    if (nrow(x) == 0){
      x$recordIndex <- numeric(0)
      return(x)
    }
    else return(mutate(x,recordIndex=1:n()))})

  nrowsFormatted <- lapply(formattedTables, nrow)
  nrowsUploaded <- lapply(uploadedTables[tablesAndVariables$tablesToCheck], nrow)
  rowDiscrepancy <- mapply(function(x,y){x != y}, nrowsFormatted, nrowsUploaded)
  if (any(rowDiscrepancy)){
    errorMessageModal("STOP -- formattedTables not equivalent to uploadedTables")
    stop()
  }
  return(formattedTables)
}




