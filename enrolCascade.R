


aggregateEnrol <- function(df, rna){
  orig <- df
  tooLittleData <- df %>% 
    filter(PFU_TIME_ENROL < 457) %>% 
    select(PATIENT)
  addToProgress("Number of patients with less than 457 PFU after enrol", 
                nrow(tooLittleData))
  df <- df %>% 
    filter(!PATIENT %in% tooLittleData$PATIENT)
  addToProgress( "Total patients in EnrolCareCascade after PFU exclusions", 
                 nrow(df))
  
  # Add Age group label:
  df <- df %>% 
    mutate(Age = case_when(ageAtEnrol >= 50 ~ ages[[6]],
                           ageAtEnrol >= 40 & ageAtEnrol < 50 ~ ages[[5]],
                           ageAtEnrol >= 30 & ageAtEnrol < 40 ~ ages[[4]],
                           ageAtEnrol >= 20 & ageAtEnrol < 30 ~ ages[[3]],
                           ageAtEnrol >= 10 & ageAtEnrol < 20 ~ ages[[2]],
                           ageAtEnrol >= 0 & ageAtEnrol < 10 ~ ages[[1]])) %>% 
    rename(Sex = SEX, Country = COUNTRY)
  
  # create enrol_clinic_N file ################
  aggregateClinic(df, "Enrol")
  #############################################

  df <- df %>% 
    mutate(time_to_ART = ifelse(RECART_Y == "Yes", RECART_D - ENROL_D, NA)) %>% 
    # this will be NA if RECART_D is NA or RECART_Y is not Yes
    mutate(ARTafter12m = ifelse( time_to_ART > 365, 1, 0),
           ART12m = ifelse( time_to_ART > -30 & time_to_ART < 366, 1, 0),
           ART30d = ifelse( time_to_ART > -30 & time_to_ART < 31, 1, 0),
           ART14d = ifelse( time_to_ART > -30 & time_to_ART < 15, 1, 0),
           ART7d = ifelse( time_to_ART > -30 & time_to_ART < 8, 1, 0)
    )
  
  #######################################
  # Add Viral Load info from tblLAB_RNA
  #######################################
  
  rna <- rna %>% 
    filter(PATIENT %in% df$PATIENT) %>% 
    filter(!is.na(RNA_D))
  
  if ("RNA_L" %in% names(rna)){
    rna <- rna %>% mutate(RNA_V = 
                            ifelse(is.na(RNA_V) & (RNA_L > 0), -1, RNA_V))
  }
  # otherwise, remove records with missing RNA_V
  rna <- rna %>% filter(!is.na(RNA_V))
  
  addToProgress( "Patients in RNA", uniqueN(data$tblLAB_RNA$PATIENT))
  addToProgress( 
    "Patients in RNA after elim pts not in NewEn and with blank date or value",
    uniqueN(rna$PATIENT))
  
  rna2 <- rna %>% 
    left_join(df %>% select(PATIENT, ENROL_D, RECART_Y, RECART_D, Year, Age, Country, Sex), 
              by = "PATIENT") %>% 
    mutate(time_enr_RNA = RNA_D - ENROL_D,  # this can't be NA because filtered out NA
           #     time_art_RNA = RNA_D - RECART_D,  # this will be NA if RECART_D is missing - not used at this time
           time_enr_RNA_abs = abs(time_enr_RNA)) %>%  #can't be NA
    mutate(VLM12m_N = ifelse(
      time_enr_RNA_abs < 457 & time_enr_RNA_abs > 273, # if observation between 274 days and 456 days
      # inclusive out from enrol_d count as vlm12mo
      1, 0)) %>% 
    mutate(VLS12m_N = ifelse(
      VLM12m_N == 1 & RNA_V < 1000, 1, 0)) %>% 
    mutate(VLM6m_N = ifelse(
      time_enr_RNA_abs <= 273 & time_enr_RNA_abs > 91, # if observation between 92 days and 273 days out from enrol_d
      # of enrol_d, count as vlm6mo
      1, 0)) %>% 
    mutate(VLS6m_N = ifelse(
      VLM6m_N == 1 & RNA_V < 1000, 1, 0))
  
  
  patientsVLM12 <- rna2 %>% filter(VLM12m_N == 1) %>% distinct(PATIENT)
  patientsVLS12 <- rna2 %>% filter(VLS12m_N == 1) %>% distinct(PATIENT)
  patientsVLM6 <- rna2 %>% filter(VLM6m_N == 1) %>% distinct(PATIENT)
  patientsVLS6 <- rna2 %>% filter(VLS6m_N == 1) %>% distinct(PATIENT)
  
  df <- df %>% 
    mutate(VLM12m_N = ifelse(PATIENT %in% patientsVLM12$PATIENT, 1, 0),
           VLS12m_N = ifelse(PATIENT %in% patientsVLS12$PATIENT, 1, 0),
           VLM6m_N = ifelse(PATIENT %in% patientsVLM6$PATIENT, 1, 0),
           VLS6m_N = ifelse(PATIENT %in% patientsVLS6$PATIENT, 1, 0)
    )
  


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
        summarise(EnrolCareCascade = n(),
                  ## Number patients initiating ART in each time window ##
                  ART7d = sum(ART7d, na.rm = TRUE),
                  ART14d = sum(ART14d, na.rm = TRUE),
                  ART30d = sum(ART30d, na.rm = TRUE),
                  ART12m = sum(ART12m, na.rm = TRUE),
                  ARTafter12m = sum(ARTafter12m, na.rm = TRUE),

                  ## now calculate percent of patients who initiated ART in each window
                  ## also standard error, and Upper and Lower confidence interval for perc
        
                  # 7 day window ###################################
                  ART7d_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(ART7d/EnrolCareCascade), digits = 1),
                    NA),
                  ART7d_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*
                            (sqrt(((ART7d/EnrolCareCascade)*
                                     (1-(ART7d/EnrolCareCascade)))/
                                    EnrolCareCascade)), digits=1),
                    NA),
                  ART7d_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART7d_Perc - ART7d_PercSE, digits = 1),
                    NA
                  ),
                  ART7d_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART7d_Perc + ART7d_PercSE, digits = 1),
                    NA
                  ),
         
                  # 14 day window  ######################################
                  ART14d_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(ART14d/EnrolCareCascade), digits = 1),
                    NA),
                  ART14d_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*
                            (sqrt(((ART14d/EnrolCareCascade)*
                                     (1-(ART14d/EnrolCareCascade)))/
                                    EnrolCareCascade)), digits=1),
                    NA),
                  ART14d_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART14d_Perc - ART14d_PercSE, digits = 1),
                    NA
                  ),
                  ART14d_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART14d_Perc + ART14d_PercSE, digits = 1),
                    NA
                  ),
                  
                  
                  # 30 day window ######################################
                  ART30d_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(ART30d/EnrolCareCascade), digits = 1),
                    NA),
                  ART30d_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*
                            (sqrt(((ART30d/EnrolCareCascade)*
                                     (1-(ART30d/EnrolCareCascade)))/
                                    EnrolCareCascade)), digits=1),
                    NA),
                  ART30d_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART30d_Perc - ART30d_PercSE, digits = 1),
                    NA
                  ),
                  ART30d_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART30d_Perc + ART30d_PercSE, digits = 1),
                    NA
                  ),
                  
                  # 12 month window ################################
                  ART12m_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(ART12m/EnrolCareCascade), digits = 1),
                    NA),
                  ART12m_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*
                            (sqrt(((ART12m/EnrolCareCascade)*
                                     (1-(ART12m/EnrolCareCascade)))/
                                    EnrolCareCascade)), digits=1),
                    NA),
                  ART12m_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART12m_Perc - ART12m_PercSE, digits = 1),
                    NA
                  ),
                  ART12m_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ART12m_Perc + ART12m_PercSE, digits = 1),
                    NA
                  ),
                  
                  # after 12 month window ###############################
                  ARTafter12m_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(ARTafter12m/EnrolCareCascade), digits = 1),
                    NA),
                  ARTafter12m_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*
                            (sqrt(((ARTafter12m/EnrolCareCascade)*
                                     (1-(ARTafter12m/EnrolCareCascade)))/
                                    EnrolCareCascade)), digits=1),
                    NA),
                  ARTafter12m_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ARTafter12m_Perc - ARTafter12m_PercSE, digits = 1),
                    NA
                  ),
                  ARTafter12m_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(ARTafter12m_Perc + ARTafter12m_PercSE, digits = 1),
                    NA
                  ),
                  # end of ART ##########################################

                  # VL measures and suppression #########################
                  ## VLM6m
                  VLM6m_N = sum(VLM6m_N, na.rm = TRUE),
                  noVLM6m_N = EnrolCareCascade - VLM6m_N,
                  VLM6m_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(VLM6m_N/EnrolCareCascade), digits = 1),
                    NA),
                  VLM6m_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM6m_N/EnrolCareCascade)*
                         (1-(noVLM6m_N/EnrolCareCascade)))/
                        EnrolCareCascade)), digits = 1),
                    NA),
                  VLM6m_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(VLM6m_Perc - VLM6m_PercSE, digits = 1),
                    NA),
                  VLM6m_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(VLM6m_Perc + VLM6m_PercSE, digits = 1),
                    NA),
                  ###############################################
                  # no VLM6m ####################################
                  noVLM6m_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(noVLM6m_N/EnrolCareCascade), digits = 1),
                    NA),
                  noVLM6m_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM6m_N/EnrolCareCascade)*
                         (1-(noVLM6m_N/EnrolCareCascade)))/
                        EnrolCareCascade)), digits = 1),
                    NA),
                  noVLM6m_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(noVLM6m_Perc - noVLM6m_PercSE, digits = 1),
                    NA),
                  noVLM6m_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(noVLM6m_Perc + noVLM6m_PercSE, digits = 1),
                    NA),
                  
                  ##################################################
                  ## VLS6m #########################################
                  VLS6m_N = sum(VLS6m_N, na.rm = TRUE),
                  VLS6m_Perc = ifelse(
                    VLM6m_N > 0,
                    round(100*(VLS6m_N/VLM6m_N), digits = 1),
                    NA),
                  VLS6m_PercSE = ifelse(
                    VLM6m_N > 0,
                    round(100*1.96*(sqrt(
                      ((VLS6m_N/VLM6m_N)*
                         (1-(VLS6m_N/VLM6m_N)))/
                        VLM6m_N)), digits = 1),
                    NA),
                  VLS6m_PercLCI = ifelse(
                    VLM6m_N > 0,
                    round(VLS6m_Perc - VLS6m_PercSE, digits = 1),
                    NA),
                  VLS6m_PercUCI = ifelse(
                    VLM6m_N > 0,
                    round(VLS6m_Perc + VLS6m_PercSE, digits = 1),
                    NA),
                  
                  ########### End of 6 M#############################
                  ###################################################
                  
                  ########### 12 M ##################################
                  ## 12M - VLM 12m ##################################
                  VLM12m_N = sum(VLM12m_N, na.rm = TRUE),
                  noVLM12m_N = EnrolCareCascade - VLM12m_N,
                  VLM12m_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(VLM12m_N/EnrolCareCascade), digits = 1),
                    NA),
                  VLM12m_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM12m_N/EnrolCareCascade)*
                         (1-(noVLM12m_N/EnrolCareCascade)))/
                        EnrolCareCascade)), digits = 1),
                    NA),
                  VLM12m_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(VLM12m_Perc - VLM12m_PercSE, digits = 1),
                    NA),
                  VLM12m_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(VLM12m_Perc + VLM12m_PercSE, digits = 1),
                    NA),
                  
                  #################################################
                  # no VLM12m ####################################
                  noVLM12m_Perc = ifelse(
                    EnrolCareCascade > 0,
                    round(100*(noVLM12m_N/EnrolCareCascade), digits = 1),
                    NA),
                  noVLM12m_PercSE = ifelse(
                    EnrolCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM12m_N/EnrolCareCascade)*
                         (1-(noVLM12m_N/EnrolCareCascade)))/
                        EnrolCareCascade)), digits = 1),
                    NA),
                  noVLM12m_PercLCI = ifelse(
                    EnrolCareCascade > 0,
                    round(noVLM12m_Perc - noVLM12m_PercSE, digits = 1),
                    NA),
                  noVLM12m_PercUCI = ifelse(
                    EnrolCareCascade > 0,
                    round(noVLM12m_Perc + noVLM12m_PercSE, digits = 1),
                    NA),
                  
                  ######################################################
                  ## VLS12m #########################################
                  VLS12m_N = sum(VLS12m_N, na.rm = TRUE),
                  VLS12m_Perc = ifelse(
                    VLM12m_N > 0,
                    round(100*(VLS12m_N/VLM12m_N), digits = 1),
                    NA),
                  VLS12m_PercSE = ifelse(
                    VLM12m_N > 0,
                    round(100*1.96*(sqrt(
                      ((VLS12m_N/VLM12m_N)*
                         (1-(VLS12m_N/VLM12m_N)))/
                        VLM12m_N)), digits = 1),
                    NA),
                  VLS12m_PercLCI = ifelse(
                    VLM12m_N > 0,
                    round(VLS12m_Perc - VLS12m_PercSE, digits = 1),
                    NA),
                  VLS12m_PercUCI = ifelse(
                    VLM12m_N > 0,
                    round(VLS12m_Perc + VLS12m_PercSE, digits = 1),
                    NA)
                  ###################################################

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
  
  write.csv(agg, paste0(input$regionchoice, "_EnrolCascadeaggregation", 
                           format(Sys.time(),'_%Y%m%d_%H%M%S'), ".csv"), row.names = FALSE, na="")
  
  return(agg)
}