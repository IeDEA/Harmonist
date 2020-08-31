
initializeAppearanceSummary <- function(groupVar, programNames,tableNames){
  rows <- length(programNames)* length(tableNames)
  x <- data.frame(
    "GROUP" = character(rows),
    "table" = character(rows),
    "number" = numeric(rows),
    "percent" = numeric(rows),
    stringsAsFactors = FALSE)
  names(x)[[1]] <- groupVar
  return(x)
}


