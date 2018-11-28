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


addAgeGroupAndProgramAndCenter <- function(formattedTableData, centerFlag){
  formattedTableData$tblBAS$ageGroup <- NA
  for (ageGroup in names(ageGroups)){
    formattedTableData$tblBAS[which((formattedTableData$tblBAS$ageAtEnrol >= ageGroups[[ageGroup]]$lower) &
                                      (formattedTableData$tblBAS$ageAtEnrol < ageGroups[[ageGroup]]$upper + 1) ), 
                              "ageGroup"] <- ageGroup
  }
  if (any(is.na(formattedTableData$tblBAS$ageGroup))){
      ageGroups$Unknown <- NULL
      formattedTableData$tblBAS[which(is.na(formattedTableData$tblBAS$ageGroup)),"ageGroup"] <- "Unknown" 
  }

  formattedTableData$tblBAS$ageGroup <- factor(formattedTableData$tblBAS$ageGroup, levels = names(ageGroups))
  

  for (tableName in uploadList()$tablesWithPatientID){
    currentTable <- formattedTableData[[tableName]]
    toJoinFromTable <- names(currentTable)
    toJoinFromBAS <- c("PATIENT", "ageGroup")
    if ("PROGRAM" %in% names(currentTable)){
      if ("ORIG_PROGRAM" %in% names(formattedTableData$tblBAS)){
        toJoinFromTable <- toJoinFromTable[toJoinFromTable != "PROGRAM"]
        toJoinFromBAS <- c(toJoinFromBAS, "PROGRAM")
      }
    } else toJoinFromBAS <- c(toJoinFromBAS, "PROGRAM")
    if (centerFlag && (!("CENTER" %in% names(currentTable)))){
      toJoinFromBAS <- c(toJoinFromBAS, "CENTER")
    }
    gc()
    formattedTableData[[tableName]] <- left_join(currentTable[,toJoinFromTable], 
                                                  formattedTableData$tblBAS[, toJoinFromBAS],
                                                  by = "PATIENT")
    rm(currentTable)
  }
  
  if ("tblLAB_RES" %in% uploadList()$AllDESTables){
    mainREStable <- formattedTableData$tblLAB_RES[,c("TEST_ID","PATIENT","PROGRAM")] %>% 
      distinct(TEST_ID, .keep_all = TRUE) # this is to prevent rows being added during left_join but may eliminate some entries.
    for (tableRESname in intersect(c("tblLAB_RES_LVL_2","tblLAB_RES_LVL_3"),names(formattedTableData))){
      formattedTableData[[tableRESname]] <- left_join(formattedTableData[[tableRESname]],
                                                      mainREStable, 
                                                      by = c("TEST_ID"))
    }
  }
  return(formattedTableData)
}

# forceModeTables ---------------------------------------------
# This function forces the variable types to conform with the DES, converts invalid codes to NA, 
# and ultimately saves the formatted data as formattedTables()
forceModeTables <- function(){
  formattedTables <- uploadedTables()
  # if CENTER is included as a column in tblBAS 
  # and CENTER has more than one unique value,
  # combine it with PROGRAM and replace PROGRAM with that variable
  # JUDY: revisit later to make more clear
  if ("CENTER" %in% names(formattedTables$tblBAS)){
    centerNames <- unique(formattedTables$tblBAS$CENTER)
    # if there's only one center, it's not adding information
    if (length(centerNames[!is.na(centerNames)]) > 1) centerFlag <- TRUE
    else centerFlag <- FALSE
    # Except if CENTER is identical to PROGRAM then no need to include CENTER
    if (all(formattedTables$tblBAS$CENTER == formattedTables$tblBAS$PROGRAM)) centerFlag <- FALSE
  } else centerFlag <- FALSE
  
  ##JUDY TEMPORARY FOR DEMOS WITH DEMO DATASETS ##Eventually remove
  if (all(formattedTables$tblBAS$PROGRAM %in% c(NA, LETTERS))) centerFlag <- FALSE
  
  if (centerFlag){
    programNames <- unique(na.omit(formattedTables$tblBAS$PROGRAM))
   
    numPrograms <- length(programNames)
    if (numPrograms == 1){
      formattedTables$tblBAS <- formattedTables$tblBAS %>% 
        rename(ORIG_PROGRAM = PROGRAM) %>% 
        mutate(PROGRAM = CENTER)
    } else {
      formattedTables$tblBAS <- formattedTables$tblBAS %>% 
        rename(ORIG_PROGRAM = PROGRAM) %>% 
        mutate(PROGRAM = paste(ORIG_PROGRAM, CENTER,sep = "_"))
    }
  } 

  
  for (tableName in uploadList()$AllDESTables){
    updateModal(paste0("Formatting ", tableName))
    print(tableName)
    variableDefs <- get(tableName, tableDef)
    variableNames <- intersect(names(tableDef[[tableName]]$variables),
                               names(formattedTables[[tableName]]))
    # If CENTER supplied in tblBAS, keep it in formattedTables even though it's not included in tblBAS
    # EDIT this if CENTER is added as optional variable in DES
    if ((tableName == "tblBAS") && centerFlag){
      formattedTables[[tableName]] <- select(formattedTables[[tableName]], c(variableNames, "CENTER"))
    } else formattedTables[[tableName]] <- select(formattedTables[[tableName]], variableNames)
    
    if (exists("PATIENT", formattedTables[[tableName]])){ # & 
       # (mode(formattedTables[[tableName]]$PATIENT) != "character")){ JUDY why doesn't this work with ATOMIC
      formattedTables[[tableName]][["PATIENT"]] <- as.character(formattedTables[[tableName]][["PATIENT"]])
    }
   
    for (variableName in variableNames){
     columnClass <- class(formattedTables[[tableName]][[variableName]])[[1]]
      
      #coded variables first. Format as Factor
      if (variableDefs$variables[[variableName]]$has_codes == "Y"){
        codeIndex <- as.numeric(variableDefs$variables[[variableName]]$code_list_ref)
        codeList <- codes[[codeIndex]]
        validCodes <- names(codeList)
        codeLabels <- unlist(codeList, use.names = FALSE)
        
        # if this variable is units, remove the html codes for superscripts...
        if (endsWith(variableName,"_U")){codeLabels <- removeHTML(codeLabels)}
        formattedTables[[tableName]][[variableName]] <- as.character(formattedTables[[tableName]][[variableName]])
        if (length(validCodes) == length(unique(codeLabels))){ #####unsure of the rationale for this....JUDY
        
          
          formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]],
                                                               levels = validCodes, 
                                                               labels = codeLabels, ordered = TRUE,
                                                               exclude = NA)
        }
        else {
          formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]],
                                                               levels = validCodes, ordered = TRUE)
        }
      }
      #Next, numeric variables that aren't coded. Format as Numeric
      else if (variableDefs$variables[[variableName]]$data_format == "Numeric"){
        if (columnClass != "numeric"){
          formattedTables[[tableName]][[variableName]] <- as.numeric(formattedTables[[tableName]][[variableName]])
        }
      }
      #Next date variables, format as date
      else if (variableDefs$variables[[variableName]]$data_format == "YYYY-MM-DD"){
        print(variableName)
        print(columnClass)
        if (columnClass %in% c("POSIXct", "POSIXxt", "character", "Date")){
          formattedTables[[tableName]][[variableName]] <- as.Date(formattedTables[[tableName]][[variableName]], 
                                                                  "%Y-%m-%d", tz = "America/Chicago")
        }
        else if (columnClass == "numeric") {
          formattedTables[[tableName]][[variableName]] <- as.Date(formattedTables[[tableName]][[variableName]], 
                                                                  "%Y-%m-%d", tz = "America/Chicago", origin = "1960-01-01") #JUDY origin date?
        }
        else formattedTables[[tableName]][,variableName] <- as.Date(NA)
      }  
      # next, if the variable is categorical, set as factor JUDY revisit this. Any reason to do this?
      else if (variableName %in% isFactor){ #in definitions.R
        # in case there are any blank records in an "isFactor" field, don't create another factor level for ""
        print(variableName)
        blankRecords <- safeTrimWS(formattedTables[[tableName]][[variableName]])==""
        if (any(blankRecords)){
          formattedTables[[tableName]][which(blankRecords),variableName] <- NA
        }
        formattedTables[[tableName]][[variableName]] <- factor(formattedTables[[tableName]][[variableName]])
      }
     #next, replace blank PROGRAM entries with NA to facilitate reporting by program
     else if (variableName == "PROGRAM"){
       if (columnClass != "character") {
         formattedTables[[tableName]][[variableName]] <- 
           as.character(formattedTables[[tableName]][[variableName]])
       }
       blankRecords <- safeTrimWS(formattedTables[[tableName]][[variableName]])==""
       if (any(blankRecords)){
         formattedTables[[tableName]][which(blankRecords),variableName] <- NA
       }
     }
     # any other variables, format as character
      else {
        formattedTables[[tableName]][[variableName]] <- as.character(formattedTables[[tableName]][[variableName]])
      }
    }
  }
  formattedTables <- addAges(formattedTables)
  formattedTables <- addAgeGroupAndProgramAndCenter(formattedTables, centerFlag)
  
  ## JUDY add this back in if interest in including visit data in report
  #formattedTables <- addVisitStats(formattedTables)
  
  # put columns in same order as DES
  for (tableName in uploadList()$AllDESTables){
    DESVariableNames <- intersect(unlist(get(tableName, tableDef), use.names = FALSE),
                                  names(formattedTables[[tableName]]))
    formattedTables[[tableName]] <- select(formattedTables[[tableName]],DESVariableNames, everything())
  }
  #Add index column to keep link between records in uploadedTables and formattedTables
  formattedTables <- lapply(formattedTables, function(x){return(mutate(x,recordIndex=1:n()))})

  nrowsFormatted <- lapply(formattedTables, nrow)
  nrowsUploaded <- lapply(uploadedTables(), nrow)
  rowDiscrepancy <- mapply(function(x,y){x != y}, nrowsFormatted, nrowsUploaded)
  if (any(rowDiscrepancy)){
    errorMessageModal("STOP -- formattedTables not equivalent to uploadedTables")
    stop()
  }
  return(formattedTables)
}




