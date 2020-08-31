# customDownloadButton ----------------------------------------------------
pdfDownloadButton <- function(outputId){
  label = "PDF"
  tags$a(id = outputId, class = "btn btn-xs shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file-pdf-o"), label)
}

# customActionButton-------------------------------------------------------
xsButton <- function(outputId, label){
  
  tags$button(id = outputId, 
              class = "btn btn-xs action-button shiny-bound-input",
              type = "button",
              label)
}


# removeHTML removes html formatting from a character string--------------
removeHTML <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


safeTrimWS <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}

# makeItPluralOrNot ------------------------------------
# makeItPluralOrNot("variable", 2) returns "variables" while makeItPluralOrNot("variable", 1) returns "variable"
makeItPluralOrNot <- function(word, quantity){
  if (quantity == 1) return(word)
  if (endsWith(word, "y")){
    return(paste0(str_sub(word, start = 1, end = nchar(word) - 1), "ies"))
  }
  return(paste0(word, "s"))
}


# is_blank tests for null, etc
is_blank <- function(x, false_triggers=FALSE){
  if(is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
  is.null(x) ||
    length(x) == 0 ||
    all(is.na(x)) ||
    all(safeTrimWS(x)=="") ||
    all(is.na(x) | safeTrimWS(x) == "") ||
    (false_triggers && all(!x))
}

is_blank_new <- function(rowData){
  return(all(is.na(rowData) | safeTrimWS(rowData) == ""))
}

is_blank_row_new <- function(df){
  blank_rows <- apply(X = df, MARGIN = 1, FUN = is_blank_new)
  return(blank_rows)
}

is_table_blank <- function(x, false_triggers=FALSE){
  is_column_blank <- sapply(head(x), is_blank)
  if (all(is_column_blank)){
    is_column_blank <- sapply(x, is_blank)
    if (all(is_column_blank)) return(TRUE)
    else return(FALSE)
  }
  else return(FALSE)
}

# is_blank_elements takes a vector and returns a logical vector indicating which elements were blank (" ")
is_blank_elements <- function(x){
  if (is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
  if (is.null(x) | length(x) == 0) return(TRUE)
  vectorClass <- class(x)[[1]]
  if (vectorClass %in% c("factor", "ordered")) vectorClass <- class(levels(x))
  result <- rep(FALSE, length(x))
  if (vectorClass == "character"){
    indicesToCheck <- which(!is.na(x))
    result[indicesToCheck] <- safeTrimWS(x[indicesToCheck]) == ""
    return(result)
  } 
  if (vectorClass %in% c("Date", "integer", "numeric") || startsWith(vectorClass, "POSIX")) 
    return(result)
  return(result)
}

# is_blank_elements takes a vector and returns a logical vector indicating which elements were blank or NA
is_blank_or_NA_elements <- function(x){
  if (is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
  if (is.null(x) | length(x) == 0) return(TRUE)
  vectorClass <- class(x)[[1]]
  if (vectorClass %in% c("factor", "ordered")) vectorClass <- class(levels(x))
  if (vectorClass == "character") return(is.na(x) | safeTrimWS(x) == "")
  if (vectorClass %in% c("Date", "integer", "numeric") || startsWith(vectorClass, "POSIX")) 
    return(is.na(x))
  return(is.na(x) | safeTrimWS(x) == "")
}

replace_blank_with_NA <- function(x){
  if (is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
  if (is.null(x) | length(x) == 0) return(TRUE)
  if (is.vector(x)){
    blanks <- is_blank_elements(x)
    if (any(blanks, na.rm = TRUE)) x[which(blanks)] <- NA
    return(x)
  }
  if (is.data.frame(x)){
    columnNames <- names(x)
    for (columnName in columnNames){
      blanks <- is_blank_elements(x[[columnName]])
      if (any(blanks, na.rm = TRUE)) x[which(blanks), columnName] <- NA
    }
    return(x)
  }
  return(x)
}

# is_blank_row <- function(df){
#   numRows <- nrow(df)
#   if (is.null(numRows)) return(is_blank_elements(df)) # vector, not a data frame, so pass to is_blank_elements
#   if (numRows == 0) return(TRUE)
#   result <- logical(numRows)
#   for (row in 1:numRows){
#     result[[row]] <- is_blank(df[row,])
#   }
#   return(result)
# }

# find the non-blank, non-NA statistical mode of a vector
Mode <- function(x) {
  x <- x[!is_blank_elements(x)]
  if (is_empty(x)) return(NULL)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


makeBulletedList <- function(inputList){
  if (is_blank(inputList)) return(NULL)
  if (!is.list(inputList)) inputList <- as.list(inputList)
  outputList <- lapply(inputList, function(x){return(tags$li(x))})
  tags$ul(outputList)
}


# helper function 
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}


# findVariablesMatchingCondition given a table name and redcap field name and a value 
# to find which variables satisfy that condition, for example, find all the variables
# in tblBAS that are required: findVariablesMatchingCondition("tblBAS","variable_required","1")
findVariablesMatchingCondition <- function(tableName, tableDef, redcapFieldName, condition){
  matching <- sapply(tableDef[[tableName]]$variables, function(x){
    value <- x[[redcapFieldName]]
    !is.null(value) && value == condition
  })
  names(matching)[matching]
}

# findTableByIndex: given the REDCap index of a table, return the table name
findTableByIndex <- function(index){
  # make sure it's a single redcapIndex
  if (length(index) != 1) return(NULL)
  allIndices <- sapply(tableDef, function(x){x$redcapIndex})
  # make sure it's a valid redcapIndex
  if (!index %in% allIndices) return(NULL)
  # now we know there's a match. There should only be only match but 
  # just in case, specify first
  tableName <- names(which(allIndices == index))[[1]]
  return(tableName)
}

#findVariable

# change table name to badge with color corresponding to DES
# 
tableBadge <-  function(tableNames){
  tableLabelClass <- sapply(tableNames, 
                            function(x){paste0("label des-",
                                               tableDef[[x]]$table_category
                            )})
  tableBadge <- paste0('<span class="',
                      tableLabelClass,'">',tableNames,'</span>')
  
  return(tableBadge)
}


# createListsOfTablesAndVariables ------------------------------------------------------------
createListsOfTablesAndVariables <- function(tableList, tableDef){
  tableNameList <- set_names(names(tableList), names(tableList))
  numberOfRecords <- sapply(tableList, nrow)
  # create variable to count deprecated variables
  deprecated_count = 0
  
  #variableDefs <- lapply(tableNameList, function(x){names(tableDef[[x]]$variables)})
  variableNameListByTable <- lapply(tableNameList, function(x){names(tableDef[[x]]$variables)})
  variableDefs <- lapply(tableNameList, function(x){tableDef[[x]]$variables})
  #create a list of des variables by table, highlight deprecated variables with red text
  #
  des_variables_list <- list()
  deprecated_list <- list()
  for (tableName in tableNameList){
    des_variables <- intersect(names(variableDefs[[tableName]]), names(tableList[[tableName]]))
    
    # Get list of deprecated variables in table using variable_status in tableDef:
    # 0, DRAFT (variable under consideration)
    # 1, Active
    # 2, DEPRECATED (variable retired from use)
    deprecatedVariables <- sapply(variableDefs[[tableName]], function(var){
      value <- var[["variable_status"]]
      !is.null(value) && value == "2"
    })
    deprecatedVariables <- intersect(des_variables, names(which(deprecatedVariables)))
    if (!is_empty(deprecatedVariables)){
      # keep track of total number of deprecated variables
      deprecated_count <- deprecated_count + length(deprecatedVariables)
      deprecated_list[[tableName]] <- deprecatedVariables
      active_des_variables <- des_variables[!des_variables %in% deprecatedVariables]
      des_variables <- paste0(paste(active_des_variables, collapse = ", "), ", ",
                              paste0('<span style="color:#dd4b39">', deprecatedVariables,'</span>', 
                                     collapse = ", ")
      )
    } else des_variables <- paste(des_variables, collapse = ", ")
    des_variables_list[[tableName]] <- des_variables
  }

  #create a list of variables in each table that are not found in the des for that table
  non_des_variables <- mapply(function(x, y) {
    variablesInTable <- names(x)
    non_des <- variablesInTable[which(!variablesInTable %in% y)]
   # non_des <- paste(non_des, collapse = ", ")
    return(non_des)
  }, x =tableList, y =variableNameListByTable, SIMPLIFY = FALSE)
  
  #create summary table. Only include NonDES column if those variables exist
  if (is_empty(unlist(non_des_variables))){
    non_des_count <- 0
    toDisplay <- tibble("Table" = tableBadge(names(tableList)), 
                            "Records" = numberOfRecords,
                            "IeDEA DES Variables" = unlist(des_variables_list))
  } else {
    all_non_des <- unique(unlist(non_des_variables))
    non_des_count <- length(all_non_des[!all_non_des %in% approvedExtraVariables])
    non_des_to_display <- lapply(non_des_variables, function(x)paste0(x, collapse = ", "))
    toDisplay <- tibble("Table" = tableBadge(names(tableList)), 
                            "Records" = numberOfRecords,
                            "IeDEA DES Variables" = unlist(des_variables_list),
                            "Extra Variables" = unlist(non_des_to_display))
  }
  # return details for uploadTab upload summary
  return(
    list(
      non_des_count = non_des_count,
      des_variables = removeHTML(des_variables_list),
      non_des_variables = non_des_variables,
      variableSummaryToDisplay = toDisplay,
      deprecated_count = deprecated_count,
      deprecated_list = deprecated_list
    )
  ) 
}


# sanitizeNames --------------------------------------------------------
sanitizeNames <- function(names) {
  # revised to allow < and >
  gsub(pattern = "[^\\<\\>A-Za-z0-9_-]+", replacement = "_", x = names)
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

# wrap_sentence inserts line breaks after complete word to make multiple strings <= specified length
# From stackOverflow RickyB Jan 1, 2015
wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}


# If a string has dots between words, convert them to spaces and convert string
# to title case for display
# usage: df$title_var <- sapply(df$var, titleReady)
titleReady <- function(x) paste(str_to_title(unlist(strsplit(x,"[.]"))),collapse=" ")

