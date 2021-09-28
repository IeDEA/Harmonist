aggregateClinic <- function(df, cascadeName){
  ###############################
  # create separate Clinic_N file
  ###############################
  df <- df %>% 
    distinct(Year, Country, CENTER, BEST_CLOSE_D)
  
  # Clinic_N is grouped by Country and Year
  years <- as.character(min(df$Year):max(df$Year))
  countries <- sort(unique(df$Country))
  
  # make grouping variables factors
  
  df$Year <- factor(df$Year, levels = years, ordered = TRUE)
  df$Country <- factor(df$Country, levels = sort(countries), ordered = TRUE)
  
  aggOut <- list()
  
  for (numVars in 0: length(clinicAggVarNames)){
    aggGroups <- combn(clinicAggVarNames, numVars, simplify = FALSE)
    for (aggGroup in aggGroups){
      aggOut[[paste(aggGroup, collapse = "_")]] <- df %>%
        group_by(.dots = aggGroup, .drop = FALSE) %>% 
        distinct(CENTER, .keep_all = TRUE) %>% 
        summarise(Clinic_N = n()) %>% 
        ungroup()
    }
  }
  
  agg <- rbindlist(aggOut, use.names = TRUE, fill = TRUE)
  
  for (aggVar in clinicAggVarNames){
    agg[[aggVar]] <- addAllLevel(agg, aggVar)
  }
  
  agg <- agg %>% 
    replace_na(list(Year = "ALL", Country = "ALL")) %>% 
    #add region column to aggregation
    mutate(Region = input$regionchoice) %>% 
    select(Region, all_of(clinicAggVarNames), everything())
  
  write.csv(agg, paste0(input$regionchoice, 
                        "_",
                        cascadeName,
                               "_Clinic_N",
                               format(Sys.time(),'_%Y%m%d_%H%M%S'), ".csv"), 
            row.names = FALSE, na="")
  
}