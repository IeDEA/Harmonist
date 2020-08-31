options(shiny.maxRequestSize = 3*1024^3) # Should this be adjusted?
errorLimit <- 600000
allowedAdditionalErrors <- 50
tooManyOfSameErrorType <- 20000
maxTotalUsage <- 1500000000 #1.5GB
Sys.setenv(TZ = "America/Chicago")
intervalToCheckUserActivity <- 120000 # this is in milliseconds (currently 2 min) 120000
idleMinToWarn <- 20
idleMinToExit <- 30

criticalColor <- "#6f0000"

AWS_bucket_name <- "shiny-app-test" # change

numericLimits <- rjson::fromJSON(file = "numericLimits.json")
labIDLimits <- rjson::fromJSON(file = "labIDLimitsWithUnits.json")
BPLabLimits <- rjson::fromJSON(file = "BPLabLimits.json")
CD4 <- list(upperLimit= 7500, lowerLimit = 0) 
maxErrorsForHTML <- 0 #JUDY change to include html option

globalDateBeforeChecks <- rjson::fromJSON(file = "globalDateBeforeChecks.json")
globalDateAfterChecks <- rjson::fromJSON(file = "globalDateAfterChecks.json")
dateOrders <- rjson::fromJSON(file = "withinTableDateOrder.json")

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
codeIndicatingInvalidCodeFormat <- 10000 # Make sure no codeList has this code

# create list of identifying variables in each table
# key identifying fields should be flagged as required and key in REDCap
tableIDField <- lapply(tableDef, function(x){
  print(paste0("table ",x$table_order))
  ids <- sapply(x$variables, function(y){
    if (is.null(y$variable_key)) return(FALSE)
    if ( any(y$variable_required == "1") &&
         any(y$variable_key == "1") ) return(TRUE)
    else return(FALSE)
  })
  return(names(which(ids)))
})
# some global variables to define
allTablesWithPatientID <- unlist(lapply(names(tableIDField), 
                                        function(x){
                                          if (is_empty(tableIDField[[x]])) return(NULL)
                                          if (tableIDField[[x]][[1]] =="PATIENT") 
                                            return(x)
                                          else return(NULL)
                                        }),
                                 use.names = FALSE)

allRequiredVariables <- unique(unlist(lapply(names(tableDef),
                                             function(x){
                                               requiredColumns <- findVariablesMatchingCondition(x, tableDef,
                                                                                                 "variable_required",
                                                                                                 "1")
                                             })))


patientShouldAppearInThese <- c("tblVIS","tblLAB_CD4","tblLAB_RNA","tblDIS","tblART","tblLTFU", "tblMED")

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
labTablesRequiringUnits <- findLabTablesRequiringUnits(tableDef) #c("tblLAB","tblLAB_CD4","tblLAB_VIRO")

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
limitOnInvalidCodesToRecord <- 1000 # upper limit on number of instances of a specific invalid code to detail 

# the following explicit definitions should be moved to a json:
desiredPlots <- c("ENROL_D", "VIS_D", "ART_SD", "CD4_D","RNA_D","DIS_D" )
desiredTables <- c("tblBAS", "tblVIS", "tblART", "tblLAB_CD4", "tblLAB_RNA", "tblDIS")
duplicateRecordExceptions <- c("tblLAB_BP", "tblLAB_CD4") # no reason to flag duplicate blood pressure or cd4 measurements as error

trackNumberOfEntriesInVis <- c("WHO_STAGE","CDC_STAGE","HEIGH","WEIGH")

maxPercentDateErrors <- 20 #if more than 20% date format errors, stop execution and tell user to correct date format

interesting <- c("SEX","NAIVE_Y","AIDS_Y","RECART_Y", "MODE", "MED_ID", "DEATH_Y", "CD4_V","LAB_V","RNA_V") # "WEIGH","HEIGH","WHO_STAGE","CDC_STAGE",
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
otherStatsToReport <- c("Enrolled", "Patients with > 2 visits", "Patients deceased, transferred out, or with no visit in the past 6 months",
                        "Median length of follow up (years)", "Median number of viral loads per patient")
codesIndicatingTransfer <- c(codes$'32'$`4`, codes$'32'$`4.1`) # this is the code text, not numeric code value
minYearForVisitStats <- year(Sys.Date()) - 8
xAxisLabelAngle <- 45



idFieldNames <- c("id1_field","id2_field", "id3_field")
idValueNames <- c("id1","id2", "id3")


pregnancyTables <- c("tblPREG","tblNEWBORN","tblPREG_OUT","tblNEWBORN_ABNORM","tblDELIVERY_MUM","tblDELIVERY_CHILD")

minimumErrorDetail <- c("category", "error_field", "error", "description",	"severity")

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

# variables to allow in tables other than the prescribed table in iedeades, used in helpers.R
approvedExtraVariables <- c("PROGRAM", "CENTER", "REGION")

maxNumberOfReportGroups <- 25

# color palette for interactive plotting
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

RED_0 <- "#ee3e32" # red for 0%
RED_20 <- "#ee3e32" # red for 20%
ORANGE_50 <- "#f68838" # orange for 50%
YELLOW_80 <- "#fbb021" # yellow for 80%
LTGREEN_99 <- "#3C9934" # "#31956a", # lighter green for 99%
GREEN_100 <- "#1b8a5a" # green for 100%
BLUE_0 <- "#dbe3ed" # #d1dae3" #zero completeness light blue
BLUE_50 <- "#9ab1cc" #8da7c5" #   "#8ea3bb" # 50% completeness blue
BLUE_99 <-  "#7393b8" #"#4a6c92" # 99% completeness blue
BLUE_100 <- "#597fab" # #335a84" # 100% complete stronger blue #4A6C92 #4a6c92 #4a74a1
NAGRAY <- "#cccccc" # GRAY for NA values

HEATMAPCOLORS <- c(RED_0, RED_20, ORANGE_50, YELLOW_80, LTGREEN_99, GREEN_100, BLUE_0, BLUE_99, BLUE_100)

#file formats in REDCap are named by number but here they are:
fileFormats <- list("1" = "CSV", "2" = "SAS", "3" = "Stata", "4" = "SPSS", "5" = "R", "9" = "Other")

# Label to use for tables where records are not linked to a specific group/program
LABEL_FOR_NOT_LINKED <- "Not Linked*"
