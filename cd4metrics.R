calcPercentile <- function(perc, cd4){
  values <- cd4[!is.na(cd4)]
  values <- sort(values)
  numvals <- length(values)
  if (numvals == 0) return(NA)
  topn <- round(perc * numvals, digits = 0)
  if (topn == 0) topn <- 1
  if (topn > numvals) topn <- numvals
  result <- values[[topn]]
  return(result)
}


##################################################################################################
#### Create CD4 Metrics Aggregation #####
#########################################
aggregateAll_cd4 <- function(df){
  
  years <- as.character(min(df$Year):max(df$Year))
  countries <- sort(unique(df$Country))
  
  # make grouping variables factors
  
  df$Year <- factor(df$Year, levels = years, ordered = TRUE)
  df$Country <- factor(df$Country, levels = sort(countries), ordered = TRUE)
  df$Age <- factor(df$Age, levels = ages, ordered = TRUE)
  df$Sex <- factor(df$Sex, levels = c("Male", "Female"), ordered = TRUE)
  aggOut <- list()
  
  for (numVars in 0: length(aggVarNames)){
    aggGroups <- combn(aggVarNames, numVars, simplify = FALSE)
    for (aggGroup in aggGroups){
      aggOut[[paste(aggGroup, collapse = "_")]] <- df %>%
        group_by(.dots = aggGroup, .drop = FALSE) %>% 
        summarise(Eligible_CD4_Metrics = n(),
                  EnrCD4 = sum(EnrCD4),
                  NoEnrCD4 = sum(NoEnrCD4),
                  EnrCD4_Perc = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round((EnrCD4/Eligible_CD4_Metrics)*100, digits=1),
                    NA),
                  EnrCD4_PercSE = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round(100*1.96*
                            (sqrt(((EnrCD4/Eligible_CD4_Metrics)*(1-(EnrCD4/Eligible_CD4_Metrics)))/
                                    Eligible_CD4_Metrics)), digits=1),
                    NA),
                  EnrCD4_PercLCI = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round(EnrCD4_Perc - EnrCD4_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_PercUCI = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round(EnrCD4_Perc + EnrCD4_PercSE, digits = 1),
                    NA
                  ),
                  noEnrCD4_Perc = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round((NoEnrCD4/Eligible_CD4_Metrics)*100, digits=1),
                    NA),
                  noEnrCD4_PercSE = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round(100*1.96*
                            (sqrt(((NoEnrCD4/Eligible_CD4_Metrics)*(1-(NoEnrCD4/Eligible_CD4_Metrics)))/
                                    Eligible_CD4_Metrics)), digits=1),
                    NA),
                  noEnrCD4_PercLCI = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round(noEnrCD4_Perc - noEnrCD4_PercSE, digits = 1),
                    NA
                  ),
                  noEnrCD4_PercUCI = ifelse(
                    Eligible_CD4_Metrics > 0,
                    round(noEnrCD4_Perc + noEnrCD4_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_median = median(CD4Med, na.rm = TRUE),
                  EnrCD4_p25 = calcPercentile(0.25, CD4Med),
                  EnrCD4_p75 = calcPercentile(0.75, CD4Med),
                  EnrCD4_0_200 = sum(EnrCD4_0_200, na.rm = TRUE),
                  EnrCD4_201_350 = sum(EnrCD4_201_350, na.rm = TRUE),
                  EnrCD4_351_500 = sum(EnrCD4_351_500, na.rm = TRUE),
                  EnrCD4_501 = sum(EnrCD4_501, na.rm = TRUE),
                  EnrCD4_0_200_Perc = ifelse(
                    EnrCD4 > 0,
                    round((EnrCD4_0_200/EnrCD4)*100, digits=1),
                    NA),
                  EnrCD4_0_200_PercSE = ifelse(
                    EnrCD4 > 0,
                    round(100*1.96*(sqrt(((EnrCD4_0_200/EnrCD4)*
                                            (1-(EnrCD4_0_200/EnrCD4)))/EnrCD4)),
                          digits = 1),
                    NA
                  ),
                  EnrCD4_0_200_PercLCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_0_200_Perc - EnrCD4_0_200_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_0_200_PercUCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_0_200_Perc + EnrCD4_0_200_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_201_350_Perc = ifelse(
                    EnrCD4 > 0,
                    round((EnrCD4_201_350/EnrCD4)*100, digits=1),
                    NA),
                  EnrCD4_201_350_PercSE = ifelse(
                    EnrCD4 > 0,
                    round(100*1.96*(sqrt(((EnrCD4_201_350/EnrCD4)*
                                            (1-(EnrCD4_201_350/EnrCD4)))/EnrCD4)),
                          digits = 1),
                    NA
                  ),
                  EnrCD4_201_350_PercLCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_201_350_Perc - EnrCD4_201_350_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_201_350_PercUCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_201_350_Perc + EnrCD4_201_350_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_351_500_Perc = ifelse(
                    EnrCD4 > 0,
                    round((EnrCD4_351_500/EnrCD4)*100, digits=1),
                    NA),
                  EnrCD4_351_500_PercSE = ifelse(
                    EnrCD4 > 0,
                    round(100*1.96*(sqrt(((EnrCD4_351_500/EnrCD4)*
                                            (1-(EnrCD4_351_500/EnrCD4)))/EnrCD4)),
                          digits = 1),
                    NA
                  ),
                  EnrCD4_351_500_PercLCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_351_500_Perc - EnrCD4_351_500_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_351_500_PercUCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_351_500_Perc + EnrCD4_351_500_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_501_Perc = ifelse(
                    EnrCD4 > 0,
                    round((EnrCD4_501/EnrCD4)*100, digits=1),
                    NA),
                  EnrCD4_501_PercSE = ifelse(
                    EnrCD4 > 0,
                    round(100*1.96*(sqrt(((EnrCD4_501/EnrCD4)*
                                            (1-(EnrCD4_501/EnrCD4)))/EnrCD4)),
                          digits = 1),
                    NA
                  ),
                  EnrCD4_501_PercLCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_501_Perc - EnrCD4_501_PercSE, digits = 1),
                    NA
                  ),
                  EnrCD4_501_PercUCI = ifelse(
                    EnrCD4 > 0,
                    round(EnrCD4_501_Perc + EnrCD4_501_PercSE, digits = 1),
                    NA
                  )
        ) %>% 
        ungroup()
    }
  }
  
  agg <- rbindlist(aggOut, use.names = TRUE, fill = TRUE)

  for (aggVar in aggVarNames){
    agg[[aggVar]] <- addAllLevel(agg, aggVar)
  }
  
  agg <- agg %>% 
    replace_na(list(Year = "ALL", Country = "ALL", Age = "ALL", Sex = "ALL")) %>% 
    #add region column to aggregation
    mutate(Region = input$regionchoice) %>% 
    select(Region, all_of(aggVarNames), everything())
  

  return(agg)
}






#########################################################
# Find cd4 observations closest to art initiation
#########################################################
# begin with df after all initial exclusions

aggregateCD4 <- function(df, cd4){
  # first exclude patients unique to CD4 metrics
  orig <- df


  # CD4 exclusion: exclude patients with too little data
  
  tooLittleData <- df %>% 
    filter(
      PFU_TIME_ENROL < 91 & 
        (PFU_TIME_ART < 30 |
           is.na(RECART_D)) 
    ) %>% 
    select(PATIENT)
  addToProgress("Number of patients with less than 91 PFU after enrol AND noRECART_D OR PFU recart < 30", 
                            nrow(tooLittleData))
  
  # CD4 exclusion: exclude patients less than 5 years of age
  tooyoung <- df %>% 
    filter(ageAtEnrol < 5) %>% 
    select(PATIENT) 
  addToProgress( "Number of patients less than 5 years old at enrollment", 
                            nrow(tooyoung))
  
  df <- df %>% 
    filter(!PATIENT %in% tooyoung$PATIENT) %>% 
    filter(!PATIENT %in% tooLittleData$PATIENT)
  addToProgress( "Total patients after CD4 exclusions", 
                            nrow(df))

  # Add Age group label:
  df <- df %>% 
    mutate(Age = case_when(ageAtEnrol >= 50 ~ ages[[6]],
                           ageAtEnrol >= 40 & ageAtEnrol < 50 ~ ages[[5]],
                           ageAtEnrol >= 30 & ageAtEnrol < 40 ~ ages[[4]],
                           ageAtEnrol >= 20 & ageAtEnrol < 30 ~ ages[[3]],
                           ageAtEnrol >= 10 & ageAtEnrol < 20 ~ ages[[2]],
                           ageAtEnrol >= 5 & ageAtEnrol < 10 ~ ages[[1]])) %>% 
    rename(Sex = SEX, Country = COUNTRY)
 
  df <- df %>% 
    mutate(Eligible = 1) 
  
  # create cd4_clinic_N file ################
  aggregateClinic(df, "CD4Metrics")
  #############################################

  #################################
  # cd4 aggregations
  #################################
  
  addToProgress( "Patients in tblLAB_CD4", uniqueN(cd4$PATIENT))
  
  cd4 <- cd4 %>% filter(PATIENT %in% df$PATIENT)
  addToProgress( "CD4 patients after excluding patients", 
                            uniqueN(cd4$PATIENT))
  
  cd4 <- cd4 %>% 
    left_join((df %>% select(PATIENT, ENROL_D, RECART_D, BEST_CLOSE_D, Year, Age, Sex, Country)))
  
  cd4a <- cd4 %>% 
    filter(!is.na(CD4_V)) %>% 
    filter(!is.na(CD4_D)) %>% 
    filter(CD4_U != "Missing")
  addToProgress( "CD4 patients after removing missing cd4v, cd4u, and cd4 dates", 
                            uniqueN(cd4a$PATIENT))
  
  cd4b <- cd4a %>% 
    # how many patients have a cd4_v within 90 days of enrollment? 
    # keep all CD4s less than 30 days after ART initiation, So keep if CD4_D - RECART_D < 30
    # Keep only CD4 counts within 91 days of enrollment and no more than 30 days after ART start
    # keep only patients with units cells/mm3
    mutate(time_enr_cd4 = CD4_D - ENROL_D, #if > 0, CD4 is after enrollment
           time_art_cd4 = CD4_D - RECART_D, #if > 0, CD4 is after ART. If RECART_D is missing, time_art_cd4 will be NA )
           time_enr_cd4_abs = abs(time_enr_cd4)) %>%  # distance in days between CD4_D and ENROL_D 
    filter(time_enr_cd4_abs <= 91) %>% 
    filter(time_art_cd4 <= 30 | is.na(RECART_D)) %>% 
    filter(CD4_U == "cells/mm3")
  # this retains patients with RECART_D missing
  addToProgress( "CD4 patients after cd4 dates too far", 
                            uniqueN(cd4b$PATIENT))
  ###############################################################################
  
  #flag patients who have a CD4 measurement near enrollment:
  df <- df %>% 
    mutate(EnrCD4 = ifelse((PATIENT %in% unique(cd4b$PATIENT)), 1, 0),
           NoEnrCD4 = ifelse((PATIENT %in% unique(cd4b$PATIENT)), 0, 1))
  
  write.csv(progressdf, paste0(input$regionchoice, "_progress",
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), ".csv"),
            row.names = FALSE, na="")
  
  
  print(paste0("CD4 patients with measurements near enrollment = ", uniqueN(cd4b$PATIENT)))
  
  ################################################################################
  # Now that we've excluded records too far from ENROL_D,
  # let's find observation closest to enrollment
 
  cd4c <- cd4b  %>% 
    mutate(closestToEnrol = time_enr_cd4_abs,
           Year = year(CD4_D)) %>% 
    group_by(PATIENT) %>% 
    slice_min(closestToEnrol, n=1, with_ties = FALSE) %>% # choose observation closest to enrollment
    ungroup() %>% 
    mutate(CD4Med = CD4_V) %>%
    select(PATIENT, CD4Med, Year, Age, Country, Sex, closestToEnrol)
  
  
  
  df <- df %>% 
    left_join((cd4c %>% select(PATIENT, CD4Med)), by = "PATIENT")
  
  aggrWithCD4 <- df %>% 
    group_by(Year) %>%  #,  Country,Sex,Age) %>% 
    dplyr::summarise(Eligible_CD4_Metrics = n(),
                     EnrCD4 = sum(EnrCD4),
                     NoEnrCD4 = sum(NoEnrCD4),
                  #   NewEnCD4Counts = sum(CD4MedDenom),
                     EnrCD4_median = median(CD4Med, na.rm = TRUE)) 
  write.csv(aggrWithCD4, paste0(input$regionchoice, 
                                "_overall",
                                format(Sys.time(),'_%Y%m%d_%H%M%S'), ".csv"), row.names = FALSE, na="")
  
  #########################
  # Create separate CD4 file
  #########################

  
  df <- df %>% 
    mutate(
      EnrCD4_0_200 = ifelse(CD4Med <= 200, 1, 0),
      EnrCD4_201_350 = ifelse(CD4Med > 200 & CD4Med <= 350, 1, 0),
      EnrCD4_351_500 = ifelse(CD4Med > 350 & CD4Med <= 500, 1, 0),
      EnrCD4_501 = ifelse(CD4Med > 500, 1, 0)
    )
  
  CD4Data <- aggregateAll_cd4(df) #finalCD4(CD4Data)

  write.csv(CD4Data, file=paste0(input$regionchoice, "_CD4Metrics",
                                 format(Sys.time(),'_%Y%m%d_%H%M%S'), ".csv"), 
            row.names = FALSE, na="")
  return(CD4Data)
  
}
