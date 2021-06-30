# These variables will be unique to the research network

# redcap url. Change if not using vanderbilt redcap
redcap_url <- "https://redcap.vanderbilt.edu/api/" # edit for other servers


AWS_bucket_name <- "shiny-app-test" # change - should be in 0C?

datesInFuture <- "NEXT_VISIT_D"
dateIndicatingUnknown <- c("1911-11-11") 
datesThatCanBeBefore1980 <- c("MOTHERDEATH_D", "FATHERDEATH_D", "DIS_D")
minimumExpectedDate <- as.Date("1980-01-01")
minimumExpectedBirthDate <- as.Date("1920-01-01")
maxHeightDecreaseInM <- 0.75

# no reason to flag duplicate blood pressure or cd4 measurements as error;
# multiple measurements in one day is plausible
duplicateRecordExceptions <- c("tblLAB_BP", "tblLAB_CD4") 


# variables to allow in tables other than the prescribed table in iedeades, used in helpers.R
approvedExtraVariables <- c("PROGRAM", "CENTER", "REGION")

# the following explicit definitions should be moved to a json or REDCap
desiredPlots <- c("ENROL_DAT", "VISIT_DAT", "TREAT_STDAT" )
desiredTables <- c("tblINDEX", "tblVISIT", "tblTREAT")
# IeDEA-specific, used to skip fields in check for codes that are missing or unknown -
# should this rely instead on for dq checkbox in 0A?
codesThatCanBeBlank <- c("ART_RS", "ART_RS2", "ART_RS3", "ART_RS4", "PREG_Y", "DEATH_Y","DROP_Y", "MOTHERDEATH_Y","FATHERDEATH_Y","RNA_T")

trackNumberOfEntriesInVis <- c("WHO_STAGE","CDC_STAGE","HEIGH","WEIGH")

interesting <- c("SEX","NAIVE_Y","AIDS_Y","RECART_Y", "MODE", "MED_ID", "DEATH_Y", "CD4_V","LAB_V","RNA_V") # "WEIGH","HEIGH","WHO_STAGE","CDC_STAGE",
isFactor <- c("CENTER", "COUNTRY","PROVINCE", "DISTRICT", "CITY") #maybe take out

# for report
visitStatsToReport <- c("Enrolled","Visits", "Deaths", "Transfers Out", "Viral Load","CD4")
# to add to report
otherStatsToReport <- c("Enrolled", "Patients with > 2 visits", "Patients deceased, transferred out, or with no visit in the past 6 months",
                        "Median length of follow up (years)", "Median number of viral loads per patient")

codesIndicatingTransfer <- c(codes$'32'$`4`, codes$'32'$`4.1`) # this is the code text, not numeric code value
minYearForVisitStats <- year(Sys.Date()) - 8 # include 8 most recent years

pregnancyTables <- c("tblPREG","tblNEWBORN","tblPREG_OUT","tblNEWBORN_ABNORM","tblDELIVERY_MUM","tblDELIVERY_CHILD")

# date approximation logic -- IeDEA-specific
datePairChecks <- jsonlite::read_json("dateApproximationLogic.json")
datePairChecks <- lapply(datePairChecks, function(x){
  x$first_A <- unlist(replace(x$first_A, x$first_A=="NA", NA))
  #what about NA
  
  x$second_A <- unlist(replace(x$second_A, x$second_A=="NA", NA))
  return(x)
})

# IeDEA-specific dq checks
labIDLimits <- rjson::fromJSON(file = "labIDLimitsWithUnits.json")
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

