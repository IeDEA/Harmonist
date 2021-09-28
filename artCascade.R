# ART CARE CASCADE - HIV CARE CASCADE AMONG PATIENTS INITIATING ART WITH AT LEAST 15mo POTENTIAL FU TIME #

aggregateART <- function(df, rna){
  
  orig <- df
  noART <- df %>% 
    filter(is.na(RECART_D)) %>% 
    select(PATIENT)
  addToProgress("Number of patients with missing RECART_D", 
                nrow(noART))
  
  tooLittleData <- df %>% 
    filter(PFU_TIME_ART < 457) %>% 
    select(PATIENT)
  
  addToProgress("Number of patients with less than 457 PFU after ART", 
                nrow(tooLittleData))
  
  df <- df %>% 
    filter(!PATIENT %in% noART$PATIENT) %>% 
    filter(!PATIENT %in% tooLittleData$PATIENT)
  
  addToProgress( "Total patients in ARTCareCascade after exclusions", 
                 nrow(df))

  # For this cascade, Year is Year of RECART start. In the other cascades it's year of enroll
  
  df <- df %>% 
    mutate(Year = lubridate::year(RECART_D))
  
  # Add Age group label:
  df <- df %>% 
    mutate(Age = case_when(ageAtEnrol >= 50 ~ ages[[6]],
                           ageAtEnrol >= 40 & ageAtEnrol < 50 ~ ages[[5]],
                           ageAtEnrol >= 30 & ageAtEnrol < 40 ~ ages[[4]],
                           ageAtEnrol >= 20 & ageAtEnrol < 30 ~ ages[[3]],
                           ageAtEnrol >= 10 & ageAtEnrol < 20 ~ ages[[2]],
                           ageAtEnrol >= 0 & ageAtEnrol < 10 ~ ages[[1]])) %>% 
    rename(Sex = SEX, Country = COUNTRY)

  # create ART_clinic_N file ################
  aggregateClinic(df, "ART")
  #############################################
  
  
  #######################################
  # Add Viral Load info from tblLAB_RNA
  #######################################
  
  rna <- data$tblLAB_RNA %>% 
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
    mutate(time_art_RNA = RNA_D - RECART_D) %>%   # this can't be NA because filtered out NA
         #  time_art_RNA_abs = abs(time_art_RNA)) %>%  #can't be NA
    mutate(VLM_ART12m_N = ifelse(
      time_art_RNA < 457 & time_art_RNA > 273, # if observation between 274 days and 456 days
      # inclusive out from recart_d count as VLM_ART12mo
      1, 0)) %>% 
    mutate(VLS_ART12m_N = ifelse(
      VLM_ART12m_N == 1 & RNA_V < 1000, 1, 0)) %>% 
    mutate(VLM_ART6m_N = ifelse(
      time_art_RNA <= 273 & time_art_RNA > 91, # if observation between 92 days and 273 days out from recart_d
      # of recart_D, count as VLM_ART6mo
      1, 0)) %>% 
    mutate(VLS_ART6m_N = ifelse(
      VLM_ART6m_N == 1 & RNA_V < 1000, 1, 0))
  
  
  patientsVLM_ART12 <- rna2 %>% filter(VLM_ART12m_N == 1) %>% distinct(PATIENT)
  patientsVLS_ART12 <- rna2 %>% filter(VLS_ART12m_N == 1) %>% distinct(PATIENT)
  patientsVLM_ART6 <- rna2 %>% filter(VLM_ART6m_N == 1) %>% distinct(PATIENT)
  patientsVLS_ART6 <- rna2 %>% filter(VLS_ART6m_N == 1) %>% distinct(PATIENT)
  
  df <- df %>% 
    mutate(VLM_ART12m_N = ifelse(PATIENT %in% patientsVLM_ART12$PATIENT, 1, 0),
           VLS_ART12m_N = ifelse(PATIENT %in% patientsVLS_ART12$PATIENT, 1, 0),
           VLM_ART6m_N = ifelse(PATIENT %in% patientsVLM_ART6$PATIENT, 1, 0),
           VLS_ART6m_N = ifelse(PATIENT %in% patientsVLS_ART6$PATIENT, 1, 0)
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
      print("Numvars = ", numVars)
      print(paste(aggGroup, collapse = "_"))
      aggOut[[paste(aggGroup, collapse = "_")]] <- df %>%
        group_by(.dots = aggGroup, .drop = FALSE) %>% 
        
        summarise(ARTCareCascade = n(),
                  
                  # VL measures and suppression #########################
                  ## VLM_ART6m
                  VLM_ART6m_N = sum(VLM_ART6m_N, na.rm = TRUE),
                  noVLM_ART6m_N = ARTCareCascade - VLM_ART6m_N,
                  VLM_ART6m_Perc = ifelse(
                    ARTCareCascade > 0,
                    round(100*(VLM_ART6m_N/ARTCareCascade), digits = 1),
                    NA),
                  VLM_ART6m_PercSE = ifelse(
                    ARTCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM_ART6m_N/ARTCareCascade)*
                         (1-(noVLM_ART6m_N/ARTCareCascade)))/
                        ARTCareCascade)), digits = 1),
                    NA),
                  VLM_ART6m_PercLCI = ifelse(
                    ARTCareCascade > 0,
                    round(VLM_ART6m_Perc - VLM_ART6m_PercSE, digits = 1),
                    NA),
                  VLM_ART6m_PercUCI = ifelse(
                    ARTCareCascade > 0,
                    round(VLM_ART6m_Perc + VLM_ART6m_PercSE, digits = 1),
                    NA),
                  ###############################################
                  # no VLM_ART6m ####################################
                  noVLM_ART6m_Perc = ifelse(
                    ARTCareCascade > 0,
                    round(100*(noVLM_ART6m_N/ARTCareCascade), digits = 1),
                    NA),
                  noVLM_ART6m_PercSE = ifelse(
                    ARTCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM_ART6m_N/ARTCareCascade)*
                         (1-(noVLM_ART6m_N/ARTCareCascade)))/
                        ARTCareCascade)), digits = 1),
                    NA),
                  noVLM_ART6m_PercLCI = ifelse(
                    ARTCareCascade > 0,
                    round(noVLM_ART6m_Perc - noVLM_ART6m_PercSE, digits = 1),
                    NA),
                  noVLM_ART6m_PercUCI = ifelse(
                    ARTCareCascade > 0,
                    round(noVLM_ART6m_Perc + noVLM_ART6m_PercSE, digits = 1),
                    NA),
                  
                  ##################################################
                  ## VLS_ART6m #########################################
                  VLS_ART6m_N = sum(VLS_ART6m_N, na.rm = TRUE),
                  VLS_ART6m_Perc = ifelse(
                    VLM_ART6m_N > 0,
                    round(100*(VLS_ART6m_N/VLM_ART6m_N), digits = 1),
                    NA),
                  VLS_ART6m_PercSE = ifelse(
                    VLM_ART6m_N > 0,
                    round(100*1.96*(sqrt(
                      ((VLS_ART6m_N/VLM_ART6m_N)*
                         (1-(VLS_ART6m_N/VLM_ART6m_N)))/
                        VLM_ART6m_N)), digits = 1),
                    NA),
                  VLS_ART6m_PercLCI = ifelse(
                    VLM_ART6m_N > 0,
                    round(VLS_ART6m_Perc - VLS_ART6m_PercSE, digits = 1),
                    NA),
                  VLS_ART6m_PercUCI = ifelse(
                    VLM_ART6m_N > 0,
                    round(VLS_ART6m_Perc + VLS_ART6m_PercSE, digits = 1),
                    NA),
                  
                  ########### End of 6 M#############################
                  ###################################################
                  
                  ########### 12 M ##################################
                  ## 12M - VLM 12m ##################################
                  VLM_ART12m_N = sum(VLM_ART12m_N, na.rm = TRUE),
                  noVLM_ART12m_N = ARTCareCascade - VLM_ART12m_N,
                  VLM_ART12m_Perc = ifelse(
                    ARTCareCascade > 0,
                    round(100*(VLM_ART12m_N/ARTCareCascade), digits = 1),
                    NA),
                  VLM_ART12m_PercSE = ifelse(
                    ARTCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM_ART12m_N/ARTCareCascade)*
                         (1-(noVLM_ART12m_N/ARTCareCascade)))/
                        ARTCareCascade)), digits = 1),
                    NA),
                  VLM_ART12m_PercLCI = ifelse(
                    ARTCareCascade > 0,
                    round(VLM_ART12m_Perc - VLM_ART12m_PercSE, digits = 1),
                    NA),
                  VLM_ART12m_PercUCI = ifelse(
                    ARTCareCascade > 0,
                    round(VLM_ART12m_Perc + VLM_ART12m_PercSE, digits = 1),
                    NA),
                  
                  #################################################
                  # no VLM_ART12m ####################################
                  noVLM_ART12m_Perc = ifelse(
                    ARTCareCascade > 0,
                    round(100*(noVLM_ART12m_N/ARTCareCascade), digits = 1),
                    NA),
                  noVLM_ART12m_PercSE = ifelse(
                    ARTCareCascade > 0,
                    round(100*1.96*(sqrt(
                      ((noVLM_ART12m_N/ARTCareCascade)*
                         (1-(noVLM_ART12m_N/ARTCareCascade)))/
                        ARTCareCascade)), digits = 1),
                    NA),
                  noVLM_ART12m_PercLCI = ifelse(
                    ARTCareCascade > 0,
                    round(noVLM_ART12m_Perc - noVLM_ART12m_PercSE, digits = 1),
                    NA),
                  noVLM_ART12m_PercUCI = ifelse(
                    ARTCareCascade > 0,
                    round(noVLM_ART12m_Perc + noVLM_ART12m_PercSE, digits = 1),
                    NA),
                  
                  ######################################################
                  ## VLS_ART12m #########################################
                  VLS_ART12m_N = sum(VLS_ART12m_N, na.rm = TRUE),
                  VLS_ART12m_Perc = ifelse(
                    VLM_ART12m_N > 0,
                    round(100*(VLS_ART12m_N/VLM_ART12m_N), digits = 1),
                    NA),
                  VLS_ART12m_PercSE = ifelse(
                    VLM_ART12m_N > 0,
                    round(100*1.96*(sqrt(
                      ((VLS_ART12m_N/VLM_ART12m_N)*
                         (1-(VLS_ART12m_N/VLM_ART12m_N)))/
                        VLM_ART12m_N)), digits = 1),
                    NA),
                  VLS_ART12m_PercLCI = ifelse(
                    VLM_ART12m_N > 0,
                    round(VLS_ART12m_Perc - VLS_ART12m_PercSE, digits = 1),
                    NA),
                  VLS_ART12m_PercUCI = ifelse(
                    VLM_ART12m_N > 0,
                    round(VLS_ART12m_Perc + VLS_ART12m_PercSE, digits = 1),
                    NA),
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
  
  write.csv(agg, paste0(input$regionchoice, "_ARTCascadeaggregation", 
                        format(Sys.time(),'_%Y%m%d_%H%M%S'), ".csv"), row.names = FALSE, na="")
  
  return(agg)
}