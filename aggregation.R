aggVarNames <- c("Year", "Country", "Sex", "Age")

# aggVarNames <- c("Year")

clinicAggVarNames <- c("Year", "Country")

# ages <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50+")
ages <- as.character(1:6)
# for cd4 aggregations, eliminate those younger than 5 and rename the first group "5-9"
# agesCD4 <- ages
# agesCD4[[1]] <- "5-9"

cd4levels <-  c("<=200", "201-350", "351-500", ">500")
progressdf <- data.frame(Comment = character(100), Count = rep(NA, 100), stringsAsFactors = FALSE)

addToProgress <- function(text, quant){
  newRow <- which(!complete.cases(progressdf))[[1]]
  progressdf$Comment[[newRow]] <<- text
  progressdf$Count[[newRow]] <<- quant
}


source("clinicAggregation.R", local = TRUE)
source("artCascade.R", local = TRUE)
source("enrolCascade.R", local = TRUE)
source("cd4metrics.R", local = TRUE)

initialExclusions <- function(data){

  exclusions <-  list()
  
  # 1. Exclude all patients enrolled before 2004
  basic <- data$tblBAS %>% 
    filter(ENROL_D >= as.Date("2004-01-01"))
  addToProgress("Excluding enrol before 2004", (nrow(data$tblBAS) - nrow(basic)))
  
  # Determine best center close_d to use. Varies by region
  if (input$regionchoice == "CA"){
    basic <- basic %>% 
      mutate(CENTER = PROGRAM) 
    maxEnrolD <- basic %>% 
      select(CENTER, ENROL_D) %>% 
      filter(!is.na(ENROL_D)) %>% 
      group_by(CENTER) %>% 
      summarise(MAX_ENROL_D = max(ENROL_D)) %>% 
      ungroup()
    
    # we're using last ENROL_D as a surrogate for CLOSE_D for CENTERS 
    centerWithMaxEnrol <- data$tblCENTER %>% 
      left_join(maxEnrolD, by = "CENTER")
    
    if (input$regionchoice == "CA"){
      bomoiIndex <- which(centerWithMaxEnrol$CENTER == "CO-BOMOI")
      centerWithMaxEnrol$MAX_ENROL_D[bomoiIndex] <- centerWithMaxEnrol$CLOSE_D[bomoiIndex]
    }
    
    basic <- basic %>% 
      left_join((centerWithMaxEnrol %>% select(CENTER, COUNTRY, MAX_ENROL_D)), 
                by = "CENTER") %>%
      rename(BEST_CLOSE_D = MAX_ENROL_D)
  }
  
  else if (input$regionchoice == "EA"){
    basic <- basic %>% 
      # add a value in basic for CENTER since for now other regions have CENTER. Associate with CENTER_ENROL
      mutate(CENTER = CENTER_ENROL)
    
    maxEnrolD <- basic %>% 
      select(CENTER, ENROL_D) %>% 
      filter(!is.na(ENROL_D)) %>% 
      group_by(CENTER) %>% 
      summarise(MAX_ENROL_D = max(ENROL_D, na.rm = TRUE)) %>% 
      ungroup()
    
    # assuming DROP_CENTER exists
    # we're using last ENROL_D as a surrogate for CLOSE_D for CENTERS 
    centerWithMaxEnrol <- data$tblCENTER %>% 
      left_join(maxEnrolD, by = "CENTER") %>% 
      mutate(BEST_CLOSE_D = pmax(MAX_ENROL_D, CLOSE_D, DROP_CENTER, na.rm = TRUE))
    
    basic <- basic %>% 
      left_join(centerWithMaxEnrol, by = c("CENTER_ENROL" = "CENTER")) %>% 
      rename(ENROL_BEST_D = BEST_CLOSE_D) %>% 
      left_join((centerWithMaxEnrol %>% select(CENTER, BEST_CLOSE_D)), by = c("CENTER_LAST" = "CENTER")) %>% 
      rename(LAST_BEST_D = BEST_CLOSE_D) %>% 
      mutate(BEST_CLOSE_D = pmax(ENROL_BEST_D, LAST_BEST_D, na.rm = TRUE))
  }
  
  else if (input$regionchoice == "WA"){
    # PROGRAM and CENTER are the same thing in WA
    basic <- basic %>% 
      mutate(CENTER = PROGRAM) %>% 
      left_join(data$tblCENTER %>% select(CENTER, COUNTRY, CLOSE_D), by = "CENTER") %>% 
      mutate(BEST_CLOSE_D = CLOSE_D)
  }
  
  else if (input$regionchoice == "SA-SA"){
    # in South Africa country dataset, CENTER in tblCENTER is almost the same thing as PROGRAM but
    # the PROGRAM names are more standardized. *replace CENTER names with PROGRAM in tblCENTER for now
    basic <- basic %>% 
      mutate(CENTER = PROGRAM) %>% 
      left_join(data$tblCENTER %>% mutate(CENTER = PROGRAM) %>% select(CENTER, COUNTRY, CLOSE_D), by = "CENTER") %>% 
      mutate(BEST_CLOSE_D = CLOSE_D)
  }
  
  if (input$regionchoice == "SA"){
    # remove patients with missing CENTER before attempting to join with CENTER (4) Note: those patients do have PROGRAM
    
    addToProgress("Number excluded because CENTER is missing",
                  sum(is.na(basic$CENTER))
    )
    basic <- basic %>%
      filter(!is.na(CENTER))
    
    basic <- basic %>% 
      left_join(data$tblCENTER %>% select(CENTER, COUNTRY, CLOSE_D), by = "CENTER") %>% 
      mutate(BEST_CLOSE_D = CLOSE_D)
  }
  
  before <- nrow(basic)
  # keep only records with non-missing birthdate, enrol_d, birth_d before or same as enrol_d, known sex (not missing or Unknown) 
  basic <- basic %>% 
    filter(!is.na(BIRTH_D)) %>% 
    filter(!is.na(ENROL_D)) %>% 
    filter(BIRTH_D <= ENROL_D) %>% 
    filter(SEX %in% c("Male", "Female"))
  addToProgress("Now missing birth_d, enrol_d, missing or unknown sex, and birth_d > enrol_d",
                            before - nrow(basic))
  before <- nrow(basic)
  # keep only records with non-missing country
  basic <- basic %>%  
    filter(COUNTRY != "Missing") 
  addToProgress("Missing country",
                            (before - nrow(basic)))
  
  
  # exclude records with enrol_d after best_close_d
  exclusions$lateEnrolDates <- basic %>% 
    filter(ENROL_D > BEST_CLOSE_D) %>% 
    select(PATIENT)
  addToProgress( "Number with ENROL_D > BEST_CLOSE_D ",
                            nrow(exclusions$lateEnrolDates))
  
  # exclude patients with RECART_Y yes but no RECART_D
  exclusions$missingRECARTD <- basic %>% 
    filter(RECART_Y == "Yes") %>% 
    filter(is.na(RECART_D)) %>% 
    select(PATIENT)
  addToProgress("Number to exclude with RECART_Y 1 but missing RECART_D because no timing of ART initiation",
                            nrow(exclusions$missingRECARTD))
  
  exclusions$notArtNaive2 <- basic %>%
    filter(!is.na(RECART_D)) %>%
    filter(NAIVE_Y == "Yes") %>%
    filter(RECART_Y == "Yes") %>%
    filter(ENROL_D - RECART_D > 30) %>% select(PATIENT)
  addToProgress(
    "Number with Non-missing RECART, NAIVE_Y=1, RECART_Y=1, ENROL_D more than 30 days after RECART_D",
    nrow(exclusions$notArtNaive2))

  patientsToExclude <- unique(rbindlist(exclusions, use.names = TRUE, fill = TRUE)[["PATIENT"]])
  addToProgress( "Total excluded patients after initial exclusions", 
                            uniqueN(patientsToExclude))
  
  main <- basic %>% 
    filter(!PATIENT %in% patientsToExclude)
  addToProgress( "Total patients after initial exclusions", 
                            nrow(main))
  
  # per instructions, remove any Cameroon patients under 19
  main <- main %>% 
    filter(!(COUNTRY == "Cameroon" & ageAtEnrol < 19))
  addToProgress( "After excluding young Cameroon patients", 
                             nrow(main))
  return(main)
}

addAllLevel <- function(df, columnName){
  column = df[[columnName]]
  column <- factor(column, levels = c("ALL", levels(column)))
  return(column)
}



createAggregations <- function(data){
  aggRecordNum <- getREDCapRecordID(tokenForAfricaAgg)
  
  main <- initialExclusions(data)
  
  ### add details about transfer_d if applicable
  ltfu <- data$tblLTFU
  transfers <- which( (ltfu$DROP_Y == "Yes") & (ltfu$DROP_RS %in% codesIndicatingTransfer))
  ltfu$TRANSFER_D <- as.Date(NA)
  ltfu$TRANSFER_D[transfers] <- ltfu$DROP_D[transfers]
  
  # calculate potential follow up after enrollment and potential follow up after ART
  # also time elapsed from enrollment to ART initiation
  main <- main %>% 
    left_join(ltfu %>% select(PATIENT, TRANSFER_D), by = "PATIENT") %>% 
    mutate(
      PFU_TIME_ENROL = ifelse(
        !is.na(TRANSFER_D),
        TRANSFER_D - ENROL_D,
        BEST_CLOSE_D - ENROL_D
      ),
      PFU_TIME_ART = ifelse(
        !is.na(TRANSFER_D),
        TRANSFER_D - RECART_D,
        BEST_CLOSE_D - RECART_D
      ),
      timeEnrolART = RECART_D - ENROL_D
    )
  
  main <- main %>% 
    mutate(Year = lubridate::year(ENROL_D))
  
  afterInit <- main 
  
  ##### Perform CD4 exclusions and aggregations ##################
  cd4result <- aggregateCD4(afterInit, data$tblLAB_CD4)
  
  ###############################################################
  
  ##### Enrollment Care Cascade #################################
  enrresult <- aggregateEnrol(afterInit, data$tblLAB_RNA)
  
  ###############################################################
  
  ###### ART Initiation Care Cascade ############################
  artresult <- aggregateART(afterInit, data$tblLAB_RNA)
  
  ###############################################################
  return(artresult)
}

