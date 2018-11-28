options(shiny.maxRequestSize = 3*1024^3) # Should this be adjusted?
errorLimit <- 200000

Sys.setenv(TZ = "America/Chicago")
intervalToCheckUserActivity <- 120000 # this is in milliseconds (currently 2 min) 120000
idleMinToWarn <- 20
idleMinToExit <- 30

AWS_bucket_name <- "shiny-app-test"

maxTablesWithoutCollapsing <- 6 #if more tables uploaded, collapse summary of upload box so that options below are visible

numericLimits <- rjson::fromJSON(file = "numericLimits.json")
labIDLimits <- rjson::fromJSON(file = "labIDLimitsWithUnits.json")
BPLabLimits <- rjson::fromJSON(file = "BPLabLimits.json")
CD4 <- list(upperLimit= 7500, lowerLimit = 0) 
maxErrorsForHTML <- 0 #JUDY change to include html option

globalDateBeforeChecks <- rjson::fromJSON(file = "globalDateBeforeChecks.json")
globalDateAfterChecks <- rjson::fromJSON(file = "globalDateAfterChecks.json")

tableDef <- rjson::fromJSON(file = "Harmonist0A.json")

# addREDCapIndices -----------------------------------------------------
# Function to add numbers to each table and variable definition that link to 
# the IeDEA DES website 
addREDCapIndices <- function(tableList){
  for (index1 in seq_along(tableList)){
    tableList[[index1]][["redcapIndex"]] <- index1
    for (index2 in seq_along(tableList[[index1]][["variables"]])){
      tableList[[index1]][["variables"]][[index2]][["redcapIndex"]] <- index2
    }
  }
  return(tableList)
}
tableDef <- addREDCapIndices(tableDef)
# rearrange tableNames according to table_order in REDCap
tableOrder <- sort(sapply(tableDef, function(x){return(as.numeric(x$table_order))}))
tableDef <- tableDef[names(tableOrder)]

codes <- rjson::fromJSON(file = "Harmonist0B.json")
date_A_codes <- codes$'2' # date approximation codes are Record 2 in Harmonist0A
  
tableIDField <- rjson::fromJSON(file = "HarmonistIDFields.json")


patientShouldAppearInThese <- c("tblVIS","tblLAB_CD4","tblLAB_RNA","tblDIS","tblART","tblLTFU", "tblMED")

labTablesRequiringUnits <- c("tblLAB","tblLAB_CD4","tblLAB_VIRO")

datesInFuture <- "NEXT_VISIT_D"

redcap_url <- "https://redcap.vanderbilt.edu/api/"
#redcap_test_url <- "https://redcaptest.vanderbilt.edu/api/"
redcap_des_url <- "https://redcap.vanderbilt.edu/plugins/iedea/des/index.php?pid=64149"
plugin_url <- "https://redcap.vanderbilt.edu/plugins/iedea/index.php"



datesThatCanBeBlank <- c("AIDS_D","ART_ED","DROP_D","DEATH_D", "TRANSFER_D", "MOTHERDEATH_D", "FATHERDEATH_D") #JUDY more?
codesThatCanBeBlank <- c("ART_RS", "ART_RS2", "ART_RS3", "ART_RS4", "PREG_Y", "DEATH_Y","DROP_Y", "MOTHERDEATH_Y","FATHERDEATH_Y","RNA_T")
dateIndicatingUnknown <- c("1911-11-11") #"1900-01-09", "1900-01-01","9999-09-09")
datesThatCanBeBefore1980 <- c("MOTHERDEATH_D", "FATHERDEATH_D", "DIS_D")
minimumExpectedDate <- as.Date("1980-01-01")
minimumExpectedBirthDate <- as.Date("1920-01-01")
maxHeightDecreasePercent <- 0.05
maxHeightDecreaseInM <- 0.75

maxCodesToShow <- 8 # upper limit on number of valid codes to show in message

limitOnInvalidCodesToShow <- 10 #upper limit on number of unique invalid codes to display in summary
limitOnInvalidCodesToRecord <- 100

CD4SpikeFactor <- 10


columnsToMergeWithErrorFrame <- c("PATIENT","PROGRAM","BIRTH_D")

# the following explicit definitions should be moved to a json:
desiredPlots <- c("ENROL_D", "VIS_D", "ART_SD", "CD4_D","RNA_D","DIS_D" )
desiredTables <- c("tblBAS", "tblVIS", "tblART", "tblLAB_CD4", "tblLAB_RNA", "tblDIS")
duplicateRecordExceptions <- c("tblLAB_BP", "tblLAB_CD4") # no reason to flag duplicate blood pressure measurements as error

trackNumberOfEntriesInVis <- c("WHO_STAGE","CDC_STAGE","HEIGH","WEIGH")

dontAddProgramTo <- c("tblLAB_RES_LVL_2","tblLAB_RES_LVL_3") # this should list all tables without PATIENT field

maxPercentDateErrors <- 20 #if more than 20% date format errors, stop execution and tell user to correct date format

interesting <- c("GENDER","NAIVE_Y","AIDS_Y","RECART_Y", "MODE", "MED_ID", "DEATH_Y", "CD4_V","LAB_V","RNA_V") # "WEIGH","HEIGH","WHO_STAGE","CDC_STAGE",
isFactor <- c("CENTER", "COUNTRY","PROVINCE", "DISTRICT", "CITY") #maybe take out

calcVarOptions <- c(
  "CDC class at ART start" = "cdcatartstart",
  "WHO stage at ART start" = "whoatartstart",
  "WHO stage at Enrolment" = "whoatenrol",
  "CD4 at ART Start" = "cd4atarvstart",
  "Date of CD4 at ARV Start" = "cd4dateatarvstart",
  "CD4 at Enrolment" = "cd4atenrol",
  "Date of CD4 at Enrolment" = "cd4dateatenrol",
  "Number of ART Medications" = "numARVs",
  "Ever pregnant (for females only)" = "everpreg",
  "Lost To Follow-Up" = "LTFU",
  "Gap In Care" = "gapincare")




validFileTypes <- c("csv", "sas7bdat", "dta", "sav")
validFileTypesToDisplay <- "CSV, SAS, Stata, SPSS, or ZIP"
allowedExtraFileTypes <- c("doc", "docx", "xls", "xlsx", "ppt", "pptx", "jpg",
                           "jpeg","png", "gif", "txt", "pdf", "rtf",
                           "xml")
ageGroups <- rjson::fromJSON(file = "ageGroups.json")

datePairChecks <- jsonlite::read_json("dateApproximationLogic.json")
datePairChecks <- lapply(datePairChecks, function(x){
  x$first_A <- unlist(replace(x$first_A, x$first_A=="NA", NA))
  #what about NA
  
  x$second_A <- unlist(replace(x$second_A, x$second_A=="NA", NA))
  return(x)
})

visitStatsToReport <- c("Enrolled","Visits", "Deaths", "Transfers Out", "Viral Load","CD4")
otherStatsToReport <- c("Patients with <2 visits", "Patients deceased or transferred out",
                        "Median length of follow up (years)", "Median number of RNA counts")
codesIndicatingTransfer <- c(codes$'32'$`4`, codes$'32'$`4.1`) # this is the code text, not numeric code value
minYearForVisitStats <- year(Sys.Date()) - 8
xAxisLabelAngle <- 45



idFieldNames <- c("id1_field","id2_field", "id3_field")
idValueNames <- c("id1","id2", "id3")


pregnancyTables <- c("tblPREG","tblNEWBORN","tblPREG_OUT","tblNEWBORN_ABNORM","tblDELIVERY_MUM","tblDELIVERY_CHILD")

minimumErrorDetail <- c("category", "error_field", "error", "description",	"severity")

newErrorDetailColumns <- c(PROGRAM = "PROGRAM", table = "table", table2 = "crosstable", idField = "id1_field",
                           idValue = "id1", idField2 = "id2_field", idValue2 = "id2", idField3 = "id3_field",
                           idValue3 = "id3",
                           errorType = "category", errorVariable = "error_field", errorValue = "error", 
                           errorVariable2 = "error_field2", errorValue2 = "error2", errorVariable3 = "error_field3", 
                           errorValue3 = "error3", errorVariable4 = "error_field4", errorValue4 = "error4", 
                           errorDesc = "description", severity = "severity")


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

notReadyMessage <- "Please complete Step 1 (upload files) and Step 2 (data quality checks)"


  

