makeAUIbox <- function(UIName, title, width){
  return(
    box(
      title = tagList(span(title, style="color: white")),
      solidHeader = TRUE, 
      status = "primary",
      width =width,
      uiOutput(UIName)))
}


# customDownloadButton ----------------------------------------------------
pdfDownloadButton <- function(outputId){
  label = "PDF"
  tags$a(id = outputId, class = "btn btn-xs shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file-pdf-o"), label)
}


# removeHTML removes html formatting from a character string--------------
removeHTML <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

safeTrimWS <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}

# findVariablesMatchingCondition given a table name and redcap field name and a value 
# to find which variables satisfy that condition, for example, find all the variables
# in tblBAS that are required: findVariablesMatchingCondition("tblBAS","variable_required","1")
findVariablesMatchingCondition <- function(tableName, redcapFieldName, condition){
  matching <- sapply(tableDef[[tableName]]$variables, function(x){
    value <- x[[redcapFieldName]]
    !is.null(value) && value == condition
  })
  names(matching)[matching]
}

# some global variables to define
allTablesWithPatientID <- unlist(lapply(names(tableIDField), 
                                        function(x){if (get(x, tableIDField)[[1]]=="PATIENT") return(x)}),
                                 use.names = FALSE)
allRequiredVariables <- unique(unlist(lapply(names(tableDef),
                                             function(x){
                                               requiredColumns <- findVariablesMatchingCondition(x, "variable_required", "1")
                                             })))


# checkAdditionalErrorVariables -----------------------------------------
checkAdditionalErrorVariables <- function(logicalVector, df){
  
  for (i in 2:4){
    errorVariableName <- paste0("errorVariable", i)
    errorValueName <- paste0("errorValue", i)
    
    if (exists(errorVariableName, df)){
      if (!logicalVector[[errorVariableName]]){
        logicalVector[[errorValueName]] <- FALSE
      }
    }
  }
 return(logicalVector) 
}



# makeDetailsPretty -------------------------------------------------------------------------
# makeDetailsPretty isn't pretty... but it removes empty columns from error detail data frame
# (columns that don't apply to the error selected by the user) *but* is careful to keep
# empty columns when the error IS that the variable is blank
makeDetailsPretty <- function(df, error_field){
  #remove empty columns; not every error type has data in each column
  emptyCols <- sapply(df, function(x) all(x=="", na.rm = TRUE))
  #make sure errorValue isn't removed -- the blank entry is important
  emptyCols[["error"]] <- FALSE
  # if the errorVariable2 field is not empty, make sure the errorValue2 column isn't removed
  # blank entries are important if that's the error variable, repeat for errorValues2 and 3
  # I know this isn't elegant!
  
  emptyCols <- checkAdditionalErrorVariables(emptyCols, df)

  
  df = df[!emptyCols]
  numberOfErrors <- nrow(df)
  toShow <- NULL
  
  if (exists("id1", df)){
    identifier <- df$id1_field[[1]]
    toShow[[identifier]] <- df$id1
  }
  if (exists("id2", df)){
    identifier <- df$id2_field[[1]]
    toShow[[identifier]] <- df$id2
  }
  if (exists("id3", df)){
    identifier <- df$id3_field[[1]]
    toShow[[identifier]] <- df$id3_field
  }
  
  toShow[[error_field]] <- df$error
  
  if (length(unique(df$description)) ==1){
    errorDesc <- paste0("Error description: ",df$description[[1]])
  }
  else {
    errorDesc <- paste0("See detailed error descriptions in table below")
    toShow[["Details"]] <- df$description
  }
  # use error variable names as column names for all variables found in df
  if (exists("error2", df)){
    variableName <- df$error_field2[[1]]
    toShow[[variableName]] <- df$error2
  }
  if (exists("error3", df)){
    variableName <- df$error_field3[[1]]
    toShow[[variableName]] <- df$error3
  }
  if (exists("error4", df)){
    variableName <- df$error_field4[[1]]
    toShow[[variableName]] <- df$error4
  }
 
  toShow <- data.frame(as.list(toShow), stringsAsFactors = FALSE)
  return(list(toShow = toShow,
              errorDesc = errorDesc))
  
}

listsOfTablesAndVariables <- function(tableList, missingRequestedCols=NULL){
  tableNameList <- set_names(names(tableList), names(tableList))
  numberOfRecords <- mapply(nrow, tableList)
  variableDefs <- lapply(tableNameList, function(x){names(tableDef[[x]]$variables)})
  des_variables <- mapply(function(x, y) {
    des_variables <- intersect(y, names(x))
    des_variables <- paste(des_variables, collapse = ", ")
    return(des_variables)
  }, x =tableList, y =variableDefs )
  
  non_des_variables <- mapply(function(x, y) {
    variablesInTable <- names(x)
    non_des <- variablesInTable[which(!variablesInTable %in% y)]
    non_des <- paste(non_des, collapse = ", ")
    return(non_des)
  }, x =tableList, y =variableDefs )
  if (all(non_des_variables == "")){
    toDisplay <- data_frame("Table" = names(tableList), 
                            "Records" = numberOfRecords,
                            "IeDEA DES Variables"= des_variables)
  } else {
    toDisplay <- data_frame("Table" = names(tableList), 
                            "Records" = numberOfRecords,
                            "IeDEA DES Variables"= des_variables,
                            "Extra Variables" = non_des_variables)
  }
  
  
  return(list(
    des_variables = des_variables,
    non_des_variables = non_des_variables,
    variableSummaryToDisplay = toDisplay,
    missingConceptColumnsDF = missingRequestedCols
  ))
}

addInvalidCodesTab <- function(boxArgs, badCodeSummary){
  nextIndex <-  length(boxArgs) + 1
  numOfErrors <- sum(badCodeSummary$Count)
  if (numOfErrors > 0){
     codeLinks <- character(nrow(badCodeSummary))
     for (i in 1:nrow(badCodeSummary)){
       linkToDES <- paste0(redcap_des_url,"&tid=", tableDef[[badCodeSummary$Table[[i]]]][["redcapIndex"]],
                     "&vid=",
                     tableDef[[badCodeSummary$Table[[i]]]][["variables"]][[badCodeSummary$Variable[[i]]]][["redcapIndex"]],
                     "&page=variableInfo")
       codeLinks[[i]] <- paste0('<a href="', linkToDES, '"',
                                ' target = "_blank" class="btn btn-link">Valid Codes for ',
                                badCodeSummary$Variable[[i]],'</a>')
      # sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)


     }
    badCodeSummary[["Valid Codes"]] <- codeLinks
     
    badCodes <- badCodeSummary %>% select(-Table, -Percent) %>% arrange(desc(Count))
    names(badCodes) <- c("Variable", "Invalid Code", "Count", "Valid Codes")
    newBoxArg <- tabPanel(title = tagList(span("Invalid Codes", style="font-weight: bold; font-size: smaller"), 
                                          span(numOfErrors, class="badge", style="background-color: red")), 
                          renderDataTable({badCodes}, 
                                          rownames = FALSE, escape = FALSE,
                                          options = list(
                                            columnDefs = list(
                                              list(orderable = FALSE, className = "dt-center", targets = 3), # disable ordering for View codes link column
                                              list(className = "dt-center", targets = 1)
                                              
                                            )
                                          )))
    
  } else {
    newBoxArg <- tabPanel(title = tagList(span("InvalidCodes", style="font-weight: bold; font-size: smaller; color: green"), shiny::icon("check-square-o")), 
             renderText("No invalid codes in this dataset"))
  }
  boxArgs[[nextIndex]] <- newBoxArg
  return(boxArgs)
}

sanitizeNames <- function(names) {
  gsub(pattern = "[^A-Za-z0-9_-]+", replacement = "_", x = names)
}

findVariableNamesMatching <- function(tableName, characteristicName, characteristicValue){
  tableVariables <- tableDef[[tableName]]$variables
  matchingVars <- c()
  for (variableName in names(tableVariables)){
    if (tableVariables[[variableName]][[characteristicName]] == characteristicValue){
      matchingVars <- c(matchingVars, variableName)
    }
  }
  return(matchingVars)
}

# getCrypt ----------------------------------------
# 
library(openssl)

getCrypt <- function(string, action) {
 
  # these are stored in redcapTokens.R
  secret_key <- encryptionKey
  secret_iv <- encryptionIV
  
  output <- NULL
  key <- charToRaw(substr(sha256(secret_key), 1, 32))
  iv <- charToRaw(substr(sha256(secret_iv), 1, 16))
  
  
  
  # use AES CBC (256 bit)
  if (action == 'e') {
    output <- base64_encode(base64_encode(aes_cbc_encrypt(charToRaw(string), key, iv)))
  }
  else if (action == 'd') {
    # as of now, token has been encoded twice so decode twice
    for (decodeIteration in 1:2){
      string <- try(base64_decode(string))
      if (inherits(string, "try-error")) return(NULL)
      if (is_empty(string)) return(NULL)
    }
    rawOutput <- try(aes_cbc_decrypt(string, key, iv))
    if  (inherits(rawOutput, "try-error")) return(NULL)
    output <- rawToChar(rawOutput)
  }
  return(output)
}