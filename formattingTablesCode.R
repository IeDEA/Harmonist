age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

addFirstCDC_STAGE <- function(formattedTableData){
  tblVIS <- formattedTableData$tblVIS
  visitData <- tblVIS %>% select(PATIENT, VIS_D, CDC_STAGE) %>% 
    filter(!is.na(CDC_STAGE)) %>% 
    group_by(PATIENT) %>% 
    dplyr::summarize(first_CDC_VIS_D = min(VIS_D, na.rm = TRUE)) %>% 
    left_join(tblVIS[,c("PATIENT","VIS_D", "CDC_STAGE")], by = c("PATIENT"="PATIENT","first_CDC_VIS_D" ="VIS_D")) %>% 
    group_by(PATIENT, first_CDC_VIS_D) %>% 
    summarise(first_CDC_STAGE = max(CDC_STAGE))
  
  formattedTableData$tblBAS <- left_join(formattedTableData$tblBAS, 
                                         visitData[,c("PATIENT","first_CDC_STAGE")], by = "PATIENT")
  
  return(formattedTableData)
}

addFirstWHO_STAGE <- function(formattedTableData){
  tblVIS <- formattedTableData$tblVIS
  visitData <- tblVIS %>% select(PATIENT, VIS_D, WHO_STAGE) %>% 
    filter(!is.na(WHO_STAGE)) %>% 
    group_by(PATIENT) %>% 
    dplyr::summarize(first_WHO_VIS_D = min(VIS_D, na.rm = TRUE)) %>% 
    left_join(tblVIS[,c("PATIENT","VIS_D", "WHO_STAGE")], by = c("PATIENT"="PATIENT","first_WHO_VIS_D" ="VIS_D")) %>% 
    group_by(PATIENT, first_WHO_VIS_D) %>% 
    summarise(first_WHO_STAGE = max(WHO_STAGE))
  
  formattedTableData$tblBAS <- left_join(formattedTableData$tblBAS, 
                                         visitData[,c("PATIENT","first_WHO_STAGE")], by = "PATIENT")
  
  return(formattedTableData)
}

addNumberOfVisits <- function(formattedTableData){
  tblVIS <- formattedTableData$tblVIS
  visitData <- tblVIS %>% group_by(PATIENT) %>%
    dplyr::summarize(numVisits = n()) 
  formattedTableData$tblBAS <- formattedTableData$tblBAS  %>%  left_join(visitData, by = "PATIENT")
  return(formattedTableData)
}

countNonNA <- function(vector){
  sum(!is.na(vector))
}

addVisitDetails <- function(formattedTableData){
  tblVIS <- formattedTableData$tblVIS
  visitFields <- c("HEIGH", "WEIGH", "CDC_STAGE","WHO_STAGE")
  resultNames <- c("numHeights","numWeights","numCDC_STAGE","numWHO_STAGE")
  resultNames <- resultNames[visitFields %in% names(tblVIS)]
  visitFields <- visitFields[visitFields %in% names(tblVIS)]
  
  visitData <- tblVIS %>% select(c("PATIENT", visitFields)) %>%  group_by(PATIENT) %>%
         dplyr::summarise_all(countNonNA) %>%  setNames(c("PATIENT",resultNames)) ####HERE HERE HERE HERE
  formattedTableData$tblBAS <- formattedTableData$tblBAS  %>%  left_join(visitData, by = "PATIENT")
  return(formattedTableData)
}

addNumberOfCD4 <- function(formattedTableData){
  tblCD4 <- formattedTableData$tblLAB_CD4
  visitData <- tblCD4 %>% filter(!is.na(CD4_V)) %>% group_by(PATIENT) %>%
    dplyr::summarize(numCD4 = n()) 
  formattedTableData$tblBAS <- formattedTableData$tblBAS  %>%  left_join(visitData, by = "PATIENT")
  return(formattedTableData)
}

addVisitStats <- function(formattedTables){
  if ("tblVIS" %in% names(formattedTables)){
    if (any(!is.na(formattedTables$tblVIS$WHO_STAGE))){
      print("adding who")
      formattedTables <- addFirstWHO_STAGE(formattedTables)
    }
    if (any(!is.na(formattedTables$tblVIS$CDC_STAGE))){
      print("adding cdc")
      
      formattedTables <- addFirstCDC_STAGE(formattedTables)
    }
    print("adding num visits")
    
    formattedTables <- addNumberOfVisits(formattedTables)
    print("addingVisitdetails")
    formattedTables <- addVisitDetails(formattedTables)
  }
  print("about to add cd4")
  if ("tblLAB_CD4" %in% names(formattedTables)){
    if ("CD4_v" %in% names(formattedTables$tblLAB_CD4)){
      print("adding numcd4")
      
      formattedTables <- addNumberOfCD4(formattedTables)
    }
  }
  return(formattedTables)
}


addAges <- function(formattedTableData){
  if (exists("ENROL_D", formattedTableData$tblBAS)){
    formattedTableData$tblBAS$ageAtEnrol <- age(formattedTableData$tblBAS$BIRTH_D, formattedTableData$tblBAS$ENROL_D)
  } else formattedTableData$tblBAS$ageAtEnrol <- NA

  return(formattedTableData)
}


addAgeGroupAndGroup <- function(formattedTableData, groupBy){
  formattedTableData$tblBAS$ageGroup <- NA
  ageGroupLevels <- names(ageGroups)
  for (ageGroup in ageGroupLevels){
    formattedTableData$tblBAS[which((formattedTableData$tblBAS$ageAtEnrol >= ageGroups[[ageGroup]]$lower) &
                                    (formattedTableData$tblBAS$ageAtEnrol < ageGroups[[ageGroup]]$upper + 1) ),
                              "ageGroup"] <- ageGroup
  }
  if (any(is.na(formattedTableData$tblBAS$ageGroup))){
      formattedTableData$tblBAS[which(is.na(formattedTableData$tblBAS$ageGroup)),"ageGroup"] <- "Unknown" 
      ageGroupLevels <- c(ageGroupLevels, "Unknown")
  }

  formattedTableData$tblBAS$ageGroup <- factor(formattedTableData$tblBAS$ageGroup, levels = ageGroupLevels)
  
  #groupLevels <- sort(unique(formattedTableData$tblBAS[[groupBy]]))
  #formattedTableData$tblBAS[[groupBy]] <- factor(formattedTableData$tblBAS[[groupBy]],
  #                                               levels = groupLevels)

  # if any tblBAS records is missing PROGRAM or group, add it as "Missing"
  blankPROGRAM <- is_blank_or_NA_elements(formattedTableData$tblBAS$PROGRAM)
  blankGroup <- is_blank_or_NA_elements(formattedTableData$tblBAS[[groupBy]])
  if (any(blankPROGRAM)){
    formattedTableData$tblBAS[which(blankPROGRAM), "PROGRAM"] <- "Missing"
  }
  if (any(blankGroup)){
    formattedTableData$tblBAS[which(blankGroup), groupBy] <- "Missing"
  }
  for (tableName in tablesAndVariables$tablesToCheckWithPatientID){
    currentTable <- formattedTableData[[tableName]]
    toJoinFromTable <- names(currentTable)
    # no need to add PROGRAM or groupBy from tblBAS if already in table
    #if (groupBy %in% names(formattedTableData[[tableName]])) browser() # what if a table has a different group value
    toJoinFromBAS <- c("PATIENT", setdiff(c("PROGRAM", groupBy), toJoinFromTable), "ageGroup")
    gc()
    formattedTableData[[tableName]] <- left_join(currentTable[,toJoinFromTable, drop = FALSE], 
                                                  formattedTableData$tblBAS[, toJoinFromBAS, drop = FALSE],
                                                  by = "PATIENT")
    # if any PATIENTS weren't found in tblBAS, the ageGroup = unknown. Add this factor level
    if (any(is.na(formattedTableData[[tableName]][["ageGroup"]]))){
      cat("replacing NA ages in", tableName, "with Unknown label", "\n", sep = " ", file = stderr())
      formattedTableData[[tableName]][["ageGroup"]] <- forcats::fct_explicit_na(formattedTableData[[tableName]][["ageGroup"]], "Unknown")
    }

    if (any(is.na(formattedTableData[[tableName]][[groupBy]]))){
      if (is.factor(formattedTableData[[tableName]][[groupBy]])){
        formattedTableData[[tableName]][[groupBy]] <- fct_explicit_na(formattedTableData[[tableName]][[groupBy]], na_level = "Unknown")
      } else {
        formattedTableData[[tableName]][[groupBy]] <- replace_na(formattedTableData[[tableName]][[groupBy]], "Unknown")
      }
    }
    rm(currentTable)
  }
  
  if ("tblLAB_RES" %in% tablesAndVariables$tablesToCheck){
    mainREStable <- formattedTableData$tblLAB_RES[, unique(c("TEST_ID","PATIENT","PROGRAM", groupBy))] %>% 
      distinct(TEST_ID, .keep_all = TRUE) # this is to prevent rows being added during left_join but may eliminate some entries. unique() is to handle case when groupBy == PROGRAM
    for (tableRESname in intersect(c("tblLAB_RES_LVL_2","tblLAB_RES_LVL_3"),names(formattedTableData))){
      formattedTableData[[tableRESname]] <- left_join(formattedTableData[[tableRESname]],
                                                      mainREStable, 
                                                      by = c("TEST_ID"))
    }
  }
  # determine which tables have not been linked to a group, such as pregnancy-related tables
  # For now, link those to group "Unknown"
  result <- lapply(formattedTableData, function(df) !exists(groupBy, df))
  tablesWithNoGroup <- names(which(unlist(result)))
  for (tableName in tablesWithNoGroup){
    formattedTableData[[tableName]][,unique(c("PROGRAM", groupBy))] <- "Not assigned"
  }
  return(formattedTableData)
}

# forceModeTables ---------------------------------------------
# This function forces the variable types to conform with the DES, converts invalid codes to NA, 
# and ultimately saves the formatted data as formattedTables()
forceModeTables <- function(groupBy, uploadedTables){
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
    # If CENTER supplied in tblBAS, keep it in formattedTables even though it's not included in tblBAS
    # EDIT this if CENTER is added as optional variable in DES

    if ((tableName == "tblBAS")){
      formattedTables[[tableName]] <- uploadedTables[[tableName]][, unique(c(variableNames, groupBy)), drop = FALSE]
    } else formattedTables[[tableName]] <- uploadedTables[[tableName]][, variableNames, drop = FALSE]

    if (exists("PATIENT", formattedTables[[tableName]])){ # & 
      formattedTables[[tableName]][["PATIENT"]] <- as.character(formattedTables[[tableName]][["PATIENT"]])
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
        codeList <- codes[[codeIndex]]
        validCodes <- names(codeList)
        codeLabels <- unlist(codeList, use.names = FALSE)
        
        # if this variable is units, remove the html codes for superscripts...
        if (endsWith(variableName,"_U")) codeLabels <- removeHTML(codeLabels)
        
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
      #next, replace blank PROGRAM/Group entries with NA to facilitate reporting by program
      else if (variableName == groupBy){
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
  formattedTables <- addAgeGroupAndGroup(formattedTables, groupBy)
  ## JUDY add this back in if interest in including visit data in report
  #formattedTables <- addVisitStats(formattedTables)
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




