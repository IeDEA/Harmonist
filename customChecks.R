# add your custom data quality check functions here

checkName1 <- function(resources){
  # code here
  newErrors <- tibble()
  return(newErrors)
}

checkName2 <- function(resources){
  # code here
  newErrors <- tibble()
  return(newErrors)
}


# call new custom data quality checks here OR in dataQuality.R but must follow this format:

newErrors <- checkName1(resources)
index <- "checkName1" # some unique identifier
errorFrame[[index]] <- newErrors

browser()
newErrors <- checkName2(resources)
index <- "checkName2" # or can be errortype, filename, etc pasted together
errorFrame[[index]] <- newErrors

# as long as each data frame of error details is added to the errorFrame list, they will
# all be combined in rbindlist call in dataQuality.R to the overall data frame of error details