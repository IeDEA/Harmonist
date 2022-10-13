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
  } else if (word == "does"){
    return("do")
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

# this is if you only need the variable name, not the table
findVarFromIndex <- function(indexstring){
  tablevar <- strsplit(indexstring, ":")[[1]]
  # if somehow there's no table:variable in string, ignore
  if (length(tablevar) != 2) return(NULL)
  
  varTableIndex <- tablevar[[1]]
  varTableName <- findTableByIndex(varTableIndex)
  if (is.null(varTableName)) return(NULL)
  varVariableIndex <- tablevar[[2]]
  varVariableName <- findVariablesMatchingCondition(varTableName, tableDef, "redcapIndex", varVariableIndex)
  if (is_empty(varVariableName)) return(NULL)
  return(varVariableName)
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

# find outliers in data visualization tab
grubbs.outliers <- function(data, threshold=0.1, cutoff=10)
{
  orig <- as.numeric(data)
  names(data) <- 1:length(data)
  data <- sort(data)
  good <- as.numeric(names(data))
  fail <- NULL
  test <- NULL
  cont <- TRUE
  
  # Strip lowest outliers
  while(cont && length(good) > cutoff)
  {
    test <- grubbs.test(orig[good])
    if(test$p.value >= threshold) {cont <- FALSE; next}
    if(grepl("lowest", test$alternative))
    {
      fail <- if(is.null(fail)) good[1] else c(fail, good[1])
      good <- good[2:length(good)]
    } else {
      fail <- if(is.null(fail)) good[length(good)] else c(fail, good[length(good)])
      good <- good[1:(length(good)-1),]
    }
  }     
  fail
}

IQR.outliers <- function(datavector, outlier_level)
{
  if (outlier_level == 'Less') { outlier_level = 2.6 } else { outlier_level = 1.5 }
  dataiqr <- IQR(datavector, na.rm = TRUE, type = 2)
  datamedian <- median(datavector, na.rm = TRUE)
  lowerlimit <- datamedian - outlier_level * dataiqr
  upperlimit <- datamedian + outlier_level * dataiqr
  data_no_outlier <- subset(datavector, datavector > lowerlimit & datavector < upperlimit)
  return (data_no_outlier)
}

IQR.outliers.second <- function(dataframe, outlier_level)
{
  if (outlier_level == 'Less') { outlier_level = 2.6 } else { outlier_level = 1.5 }
  double <- unlist(dataframe[1])
  dataiqr <- IQR(double, na.rm = TRUE, type = 2)
  datamedian <- median(double, na.rm = TRUE)
  lowerlimit <- datamedian - outlier_level * dataiqr
  upperlimit <- datamedian + outlier_level * dataiqr
  data_no_outlier <- subset(dataframe, dataframe[1] > lowerlimit & dataframe[1] < upperlimit)
  return (data_no_outlier)
}
