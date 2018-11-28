
initializeErrorTable <- function(){
  return(list())
  }

initializeMissingErrors <- function() {
  return(list())
}

initializeUnknownCodes <- function() {
 return(list())
}

initializeSummaryFrame <- function(){
  return(list())
}

initializeSummaryFrameByProgram <- function(){
  return(list())
}

initializeCodeErrorSummaryFrame <- function(){
  return(list())
}


initializeAppearanceSummary <- function(programNames,tableNames){
  rows <- length(programNames)* length(tableNames)
  x <- data.frame(
    "PROGRAM" = character(rows),
    "table" = character(rows),
    "number" = numeric(rows),
    "percent" = numeric(rows),
    stringsAsFactors = FALSE)
  return(x)
}


