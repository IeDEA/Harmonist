# These variables will be unique to the research network

# redcap url. Change if not using vanderbilt redcap
redcap_url <- "https://redcap.vanderbilt.edu/api/" # edit for other servers


AWS_bucket_name <- "" # only for networks with hub

datesInFuture <- "NEXT_VISIT_D"
dateIndicatingUnknown <- c("1911-11-11") 
datesThatCanBeBefore1980 <- c("MOTHERDEATH_D", "FATHERDEATH_D", "DIS_D")
minimumExpectedDate <- as.Date("1980-01-01")
minimumExpectedBirthDate <- as.Date("1920-01-01")
maxHeightDecreaseInM <- 0.25 # if height decreases by this amount over time, error

# no reason to flag duplicate blood pressure or cd4 measurements as error;
# multiple measurements in one day is plausible
duplicateRecordExceptions <- c("tblLAB_BP", "tblLAB_CD4") 


# variables to allow in tables other than the prescribed table in iedeades, used in helpers.R
approvedExtraVariables <- c("PROGRAM", "CENTER", "REGION")

# the following explicit definitions should be moved to a json or REDCap
# for IeDEA: desiredPlots <- c("ENROL_D", "VIS_D", "ART_SD", "CD4_D","RNA_D","DIS_D" )
# for IeDEA: desiredTables <- c("tblBAS", "tblVIS", "tblART", "tblLAB_CD4", "tblLAB_RNA", "tblDIS")
desiredPlots <- c() # date variables to include in report histograms
desiredTables <- c() # corresponding table name for each of those variables

# IeDEA-specific, used to skip fields in check for codes that are missing or unknown -
# should this rely instead on for dq checkbox in 0A?
codesThatCanBeBlank <- c() 

trackNumberOfEntriesInVis <- c("HEIGHT","WEIGHT")

interesting <- c() # list variables to include in heat map
isFactor <- c("CENTER", "COUNTRY","PROVINCE", "DISTRICT", "CITY") 

# for report
visitStatsToReport <- c("Enrolled","Visits", "Deaths", "Transfers Out", "Viral Load","CD4")
# to add to report
otherStatsToReport <- c("Enrolled", "Patients with > 2 visits", "Patients deceased, transferred out, or with no visit in the past 6 months",
                        "Median length of follow up (years)", "Median number of viral loads per patient")

codesIndicatingTransfer <- c(codes$'32'$`4`, codes$'32'$`4.1`) # this is the code text, not numeric code value
numYearsInReport <- 8 # include 8 most recent years
minYearForVisitStats <- year(Sys.Date()) - numYearsInReport  

pregnancyTables <- c() #these are tables to exclude from patient-linked dq checks
# in IeDEA: ("tblPREG","tblNEWBORN","tblPREG_OUT","tblNEWBORN_ABNORM","tblDELIVERY_MUM","tblDELIVERY_CHILD")

# date approximation logic -- IeDEA-specific
datePairChecks <- jsonlite::read_json("dateApproximationLogic.json")
datePairChecks <- lapply(datePairChecks, function(x){
  x$first_A <- unlist(replace(x$first_A, x$first_A=="NA", NA))
  #what about NA
  
  x$second_A <- unlist(replace(x$second_A, x$second_A=="NA", NA))
  return(x)
})

# IeDEA-specific dq checks
BPLabLimits <- rjson::fromJSON(file = "BPLabLimits.json")
CD4 <- list(upperLimit= 7500, lowerLimit = 0) # when units == cells/mm3

date_A_codes <- codes$'2' # date approximation codes are Record 2 in Harmonist0B

# IeDEA-specific
redcap_des_url <- "https://redcap.vanderbilt.edu/plugins/iedea/des/index.php?pid=64149"
plugin_url <- "https://redcap.vanderbilt.edu/plugins/iedea/index.php"
faq_url <- "https://redcap.vanderbilt.edu/plugins/iedea/index.php?option=dfq"

# for IeDEA only - if user not from Hub
defaultUserInfo <- list(
  uploaduser_id = "",
  uploadregion_id = "1",
  regionName = "Harmonist Test",
  regionCode = "TT",
  datacall_id = "",
  uploadconcept_mr = "MR000",
  uploadconcept_title = "",
  request = "",
  token = "0"
)

## IeDEA-specific - make a list of tables with units...
findLabTablesRequiringUnits <- function(tableDef){
  tableList <- c()
  for (tableName in names(tableDef)){
    variableNames <- tableDef[[tableName]][["variables"]]
    if (startsWith(tableName, "tblLAB")){
      print(tableName)
      labValue <- names(variableNames)[endsWith(names(variableNames),"_V")]
      if (is_empty(labValue)) next
      labName <- strsplit(labValue[[1]],"_V")
      if (exists(paste0(labName,"_U"), variableNames)){
        tableList <- c(tableList, tableName)
      }
    }
  }
  return(tableList)
}

labTablesRequiringUnits <- findLabTablesRequiringUnits(tableDef) 

