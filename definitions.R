# settings 
options(shiny.maxRequestSize = 3*1024^3) # Should this be adjusted?
errorLimit <- 600000 # if this number of error records is reached, no new errors are added to error Frame
tooManyOfSameErrorType <- 10000
maxTotalUsage <- 1500000000 #1.5GB This is used to determine if tooBusy for new user
Sys.setenv(TZ = "America/Chicago")
intervalToCheckUserActivity <- 120000 # this is in milliseconds (currently 2 min) 120000
idleMinToWarn <- 20
idleMinToExit <- 30

maxCodesToShow <- 8 # upper limit on number of valid codes to show in message
limitOnInvalidCodesToShow <- 10 #upper limit on number of unique invalid codes to display in summary
limitOnInvalidCodesToRecord <- 1000 # upper limit on number of instances of a specific invalid code to detail 

maxNumberOfReportGroups <- 25 # used in determining alternate grouping variables
notReadyMessage <- "Please complete Step 1 (upload files) and Step 2 (data quality checks)"

## read in info from Harmonist0C
projectDef <- rjson::fromJSON(file = "Harmonist0C.json")
if (length(projectDef) != 1) {
  stop("projectDef should only have one record")
}

projectDef <- projectDef[[1]]

networkName <- projectDef$project_name
networkLogo <- projectDef$project_logo_100_40

# Set PATIENT variable name
patientVar <- projectDef$patient_id_var
patientVarSym <- rlang::sym(patientVar)

# Set PROGRAM variable name
defGroupVar <- projectDef$default_group_var
defGroupVarSym <- rlang::sym(defGroupVar)
defGroupTableName <- projectDef$group_tablename

# Set tblBAS table name
indexTableName <- projectDef$index_tablename
indexTableNameLower <- tolower(indexTableName)
indexTableSym <- rlang::sym(indexTableName)

# Set BIRTH_D variable name
birthDateVar <- projectDef$birthdate_var
birthDateVarSym <- rlang::sym(birthDateVar)

# Set ENROL_D variable name
ageDateVar <- projectDef$age_date_var
ageDateVarSym <- rlang::sym(ageDateVar)

# Set DEATH_D variable name
deathDateVar <- projectDef$death_date_var
deathDateVarSym <- rlang::sym(deathDateVar)

# Set ENROL_D variable name
enrolDateVar <- projectDef$enrol_date_var
enrolDateVarSym <- rlang::sym(enrolDateVar)

# Set HEIGH variable name
heightVar <- projectDef$height_var
heightVarSym <- rlang::sym(heightVar)

heightTableName <- projectDef$height_table
heightTableNameSym <- rlang::sym(heightTableName)

heightDateVar <- projectDef$height_date
heightDateVarSym <- rlang::sym(heightDateVar)

# Set date suffix
dateExt <- projectDef$date_ext

# Set start date suffix (i.e. _SD)
sdExt <- projectDef$sd_ext

# Set end date suffix (i.e. _ED)
edExt <- projectDef$ed_ext

# does this network have date approximations?
dateApproxFlag <- ifelse((projectDef$date_approx_y == 1) &&
                           (projectDef$date_approx != ""), TRUE, FALSE)

# Setup age groups
ageGroups <- sapply(1:6, function(i) {
  lower <- as.integer(projectDef[[paste0("age_", i, "_lower")]])
  upper <- as.integer(projectDef[[paste0("age_", i, "_upper")]])
  groupName <-
    if (upper == 120) {
      if (lower > 18) {
        paste0("Adults ", lower, "+")
      } else {
        paste0(lower, "+")
      }
    } else {
      paste0(lower, "-", upper)
    }
  
  group <- list()
  group[[groupName]] <- list(lower = lower, upper = upper)
  group
})


# columns in error detail report:
idFieldNames <- c("id1_field","id2_field", "id3_field")
idValueNames <- c("id1","id2", "id3")

# minimum info to add to errorFrame when error detected
minimumErrorDetail <- c("category", "error_field", "error", "description",	"severity")

# move to Harmonist0C ------------------------------------------------------------
codeIndicatingInvalidCodeFormat <- 10000 # Make sure no codeList has this code

# Label to use in dqmetrics for tables where records are not linked to a specific group/program
LABEL_FOR_NOT_LINKED <- "Not Linked*"

# data model definition -----------------------------------------------------------
tableDef <- rjson::fromJSON(file = "Harmonist0A.json")
codes <- rjson::fromJSON(file = "Harmonist0B.json")

# IeDEA-specific: link variables to DES website -------------------------
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


# create list of identifying variables in each table
# key identifying fields should be flagged as required and key in REDCap
tableIDField <- lapply(tableDef, function(x){
  ids <- sapply(x$variables, function(y){
    if (is.null(y$variable_key)) return(FALSE)
    if ( any(y$variable_required == "1") &&
         any(y$variable_key == "1") ) return(TRUE)
    else return(FALSE)
  })
  return(names(which(ids)))
})

# some global variables to define
allTablesWithPatientID <- unlist(
  lapply(
    names(tableIDField),
    function(x){
      if (is_empty(tableIDField[[x]])) return(NULL)
      if (tableIDField[[x]][[1]] == patientVar) return(x)
      else return(NULL)
    }),
  use.names = FALSE)


allRequiredVariables <- unique(
  unlist(
    lapply(
      names(tableDef),
      function(x){
        requiredColumns <- findVariablesMatchingCondition(
          x, tableDef, "variable_required", "1"
        )
      }
    )
  )
)


# report definitions ---------------------------------------------------------------
# for plots
xAxisLabelAngle <- 45

# color definitions for plots ------------------------------------------------------
criticalColor <- "#6f0000"
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

# Date order checks in IeDEA - store somewhere else?
# IeDEA-specific plausible limits ------------------------------------------
numericLimits <- rjson::fromJSON(file = "numericLimits.json")
globalDateBeforeChecks <- rjson::fromJSON(file = "globalDateBeforeChecks.json")
globalDateAfterChecks <- rjson::fromJSON(file = "globalDateAfterChecks.json")
dateOrders <- rjson::fromJSON(file = "withinTableDateOrder.json") # other than _ed/_sd

# data uploading -- file types allowed
validFileTypes <- c("csv", "sas7bdat", "dta", "sav")
validFileTypesToDisplay <- "CSV, SAS, Stata, SPSS, or ZIP"
allowedExtraFileTypes <- c("doc", "docx", "xls", "xlsx", "ppt", "pptx", "jpg",
                           "jpeg","png", "gif", "txt", "pdf", "rtf",
                           "xml")

# preferred file formats for IeDEA concepts in REDCap are named by number (dataformat_prefer):
fileFormats <- list("1" = "CSV", "2" = "SAS", "3" = "Stata", "4" = "SPSS", "5" = "R", "9" = "Other")

