generateFacetHeatMapDQ <- function(df, xVar, yVar, facetVar, titleText=NULL, makeBlue=NULL, caption=NULL){
  # this function generates heatmaps where 100 is good and 0 is bad. 
  # we take the "floor" of each percent so that anything less than 100.00000 appears
  # different from 100. We use 80 as the midpoint color 
  print("generating dq heatmap")

  temp <- df$percent
  df$percent <- floor(as.numeric(df$percent))
  
  # check for values between 0 and 1%; display these as 1 because only 0.000 should display as 0
  tinyValues <- (temp > 0) & (temp < 1.0)
  if (any(tinyValues, na.rm = TRUE)){
    df$percent[tinyValues] <- 1
  }

  BLUE_RANGE <-  200 # Add this to the category that you would like to make BLUE in the heat map, not stoplight
  df$percentColor <- df$percent
  if (!is.null(makeBlue)){
    if (makeBlue == "ALL"){
      df$percentColor <- df$percentColor + BLUE_RANGE
    } else {
      for (category in makeBlue){
        df$percentColor[which(df[[xVar]] == category)] <- df$percentColor[which(df[[xVar]] == category)] + BLUE_RANGE
      }
    }
  }
  
  # ensure that y-axis Variables are in correct order, top to bottom (requires reverse)
  if (is.factor(df[[yVar]])){
    df[[yVar]] <- fct_rev(factor(df[[yVar]]))
  } else {
    df[[yVar]] <- fct_rev(factor(df[[yVar]], unique(df[[yVar]])))
  }
  
  addColor <- scale_fill_gradientn(
    colors = c(RED_0, # red for 0%
               RED_20, # red for 20%
               ORANGE_50, # orange for 50%
               YELLOW_80, # yellow for 80%
               LTGREEN_99, # "#31956a", # lighter green for 99%
               GREEN_100, # green for 100%
               BLUE_0, #zero completeness light blue
               BLUE_50, # 50% completeness blue
               BLUE_99, # 99% completeness blue
               BLUE_100), #100% complete stronger blue
    values = rescale(c(0, 20, 50, 80, 99, 100, 
                       200, 250, 299, 300)),
    limits = c(0,300),
    na.value = NAGRAY # light gray gray 80 for NA
  )
  # if number of facets exceeds 4 rows by 4 cols (16), use base font size 6.8
  # otherwise, base font size of 9 should be fine

  baseFontSize  <- 8
  varFontSize <- 9
  
  if (!is.null(facetVar)){
    facetWrapCode <- facet_wrap(facetVar, drop = FALSE, scales = "free_x") # free_x labels each plot
    if ((uniqueN(df[[facetVar]]) > 16)){
      baseFontSize <- 5.5
      varFontSize <- 8
    }
    xAxisTextInfo <- element_text(size = baseFontSize)
    max_x_char <- max(nchar(as.character(unique(df[[xVar]]))))
    if (max_x_char > 10 && uniqueN(df[[xVar]]) > 4){ #put in definitions 
      # If x axis labels are long, rotate 90 degrees
      xAxisTextInfo <- element_text(angle = 90, hjust = 0.1, size = baseFontSize)
    }
  } else {
    facetWrapCode <- NULL
    # this means there is no facet wrap, so turn labels 90 degrees
    xAxisTextInfo <- element_text(angle = 90, hjust = 0.1, size = 8)
  }
  
  p <- ggplot(data = df, aes_string(x = xVar, y = yVar)) +
    geom_tile(aes(fill = percentColor), colour = "white", size = 0.01) +
    facetWrapCode +
    labs(x="",
         y="",
         caption = caption) +
    geom_text(aes(label = percent), size = 3) +
    scale_y_discrete(expand=c(0,0))+
    #define new breaks on x-axis
    scale_x_discrete(expand=c(0,0), position = "top") +
    geom_text(data = df %>% filter(is.na(percent)),
                             label = "N/A", size = 2) +
   # coord_equal() +
    addColor + 
    #  labs(title=titleText) +
    theme_grey(base_size=baseFontSize)+
    theme(
        axis.text.y = element_text(size = varFontSize),
        axis.text.x = xAxisTextInfo,
        plot.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.caption = element_text(face = "italic", size = 6.5, margin = margin(20,0,0,0)))#,
     # add this back in when upgrade packages   plot.caption.position = "plot")
  return(p)
}

createDetailedQualityHeatmap <- function(df, yVar, facetVar, numUnknownGroup=0, tableName){
  # in this case x is Metric and y is Variable
  # 

  metricLevels <- c("DES_percent", "Logic_percent", "Complete_percent")
  metricLabels <- c("Compliant", "Logical", "Complete")
  
 
  df$Metric <- factor(df$Metric, 
                      levels = metricLevels,
                      labels = metricLabels)

  generateFacetHeatMapDQ(
    df = df, 
    xVar = "Metric", 
    yVar = yVar, 
    facetVar = facetVar, 
    titleText = paste0("Table: ", tableName),
    makeBlue = "Complete",
    caption = expression(italic(bolditalic("Compliant:")~"% records compliant with DES"~~~~
                                bolditalic("Logical:")~"% of complete entries that are logically consistent"~~~~
                                bolditalic("Complete:")~"% records that are not blank")
                     )
  )
}

checkForDependency <- function(varName, groupVar, tableData){
  
  dateExtReasons <- list("_ED" = "_RS", "_D"= "_RS","_SD" = "START_RS") # or should this be in definitions
  rowsToUse <- NULL
  # Dependency 1: If this is a date approx, denominator is rows with a date entry
  if (endsWith(varName, "_A")){
    dateName <- str_sub(varName, end = -(1 + nchar("_A")))
    if (dateName %in% names(tableData)){
      # now we know there's a date/approx pair
      datesPresent <- !is.na(tableData[[dateName]])
      rowsToUse <- datesPresent
    }
  }
  
  # Dependency #2: If this is a REASON with a corresponding _Y
  # denom is rows with _Y == YES
  
  else if (endsWith(varName, "_RS")){
    # we know this is a REASON variable. Is there a corresponding _Y?
    # if not, is there a corresponding date? 
    
    if (endsWith(varName, "START_RS")){
      reasonExt <- "START_RS"
      dateExt <- "_SD"
    } else {
      reasonExt <- "_RS"
      dateExt <- c("_D", "_ED")
    }
    
    baseVarName <- str_sub(varName, end = -(1 + nchar(reasonExt)))
    
    # for REASON variables, first dependency is corresponding _Y if it exists
    # otherwise dependency is records with corresponding date variable not blank
    dateName <- paste0(baseVarName, dateExt)
    yVarName <- paste0(baseVarName, "_Y")
    
    if (yVarName %in% names(tableData)){
      # now we know there's a _RS/_Y pair, find rows with _Y == Yes
      yesPresent <- tableData[[yVarName]] == "Yes"
      rowsToUse <- yesPresent
    } else if (any(dateName %in% names(tableData))){
      # now we know there's a _RS/date pair, find rows with non blank date
      dateNames <- intersect(dateName, names(tableData))
      if (length(dateNames) > 1){
        dateName <- dateNames[endsWith(dateNames, "_ED")]
      } else {
        dateName <- dateNames
      }
      reasonName <- varName
      datesPresent <- !is.na(tableData[[dateName]])
      rowsToUse <- datesPresent
    }
  }
  
  
  # Dependency #2: If this is a date that has a corresponding _Y variable,
  # denominator is rows with _Y == Yes 
  else if (endsWith(varName, "_D")){
    baseVarName <- str_sub(varName, end = -(1 + nchar("_D")))
    varNameY <- paste0(baseVarName, "_Y")
    if (varNameY %in% names(tableData)){
      # now we know there's a _D/_Y pair, find rows with _Y == Yes
      yesPresent <- tableData[[varNameY]] == "Yes"
      rowsToUse <- yesPresent
    }
  }
  
  
  return(rowsToUse)
  
#  if (is.null(rowsToUse)){
#    return(NULL)
#  } else {
    # recordsSubset <- tableData[rowsToUse, c(groupVar, varName)] %>% 
    #   group_by((!! rlang::sym(groupVar))) %>% summarise(RecordsSubset = n())
    # completeSubset <- tableData[rowsToUse & nonBlank, c(groupVar, varName)] %>% 
    #   group_by((!! rlang::sym(groupVar))) %>% summarise(CompleteSubset = n())
    # complete <- tableData[which(nonBlank), c(groupVar, varName)] %>% 
    #   group_by((!! rlang::sym(groupVar))) %>% summarise(Complete = n()) 
    # # join these together. If NA is in the Complete column, that means that rows
    # # exist for that group but the current variable has no entries complete. Change 
    # # those NA's to 0
    # completeness <- recordsSubset %>% 
    #   full_join(completeSubset, by = groupVar) %>% 
    #   replace_na(list(CompleteSubset = 0)) %>% 
    #   full_join(complete, by = groupVar) #%>%
    # # replace_na(list(CompleteSubset = 0))
    # return(completeness)
#  }
}

checkForUseLast <- function(varName, lastVarName){
  # is this one of the RS2 RS3 RS4 variables? was the last variable RS or RS -1?
  #first does it include _RS?
  if (!str_detect(varName, "_RS")) return(FALSE)
  #next, if it ends with _RS (not a digit), it's the first one, not a supplemental reason
  if (endsWith(varName, "_RS")) return(FALSE)
  # Now we know it contains _RS but doesn't end in _RS
  # Next remove last character from current variable to get base name
  baseVarName <- str_sub(varName, end= -2) 
  # does the last variable name contain the base variable name?
  if (!str_detect(lastVarName, baseVarName)) return(FALSE)
  # Now we know that the current variable name is a sequel to the last variable
  # because the variables are in DES order
  # and we shouldn't go through the trouble of finding the rows to use in checking reasons
  # all over again
  return(TRUE)
  
}


summarizeDQMetrics <- function(errorFrame, tableRowsByGroup, formattedTables, groupingVar, varsInclude){
  if (is_empty(errorFrame)){
    tablesWithErrors <- NULL
  } else {
    errorFrame <- errorFrame %>% filter(severity %in% c("Critical", "Error"))
    tablesWithErrors <- unique(errorFrame$table)
  }
  
  requested <- concept()$tablefields
  
  heatmapData <- list()
  tableGroupMissing <- list()
  grouping <- list()
  # create lists to store details about coded variables  
  codedData <- list()
  codedHeatmap <- list()
  
  # Does user want to include only requested variables (or if no request, that's just required vars from tblBAS)?
  # Or all DES variables?
  if (varsInclude == "dqRequested"){
    toInclude <- requested
  } else {
    toInclude <- tablesAndVariables$matchingColumns
  }
  
  groupLevels <- tableRowsByGroup$tblBAS[[groupingVar]]
  if (is.factor(groupLevels)) {
    groupLevels <- as.character(groupLevels)
  }
  groupLevels <- sort(groupLevels[groupLevels != "Missing"]) #IS THIS NECESSARY?
  
  for (tableName in names(toInclude)){
    print(paste0("table = ", tableName))
    # THINK ABOUT HOW TO FLAG missing requested tables
    if (tableName %in% uploadList()$MissingTables){
      heatmapData[[tableName]] <- "TABLE MISSING"
      next
    }
    
    tableData <- formattedTables[[tableName]]
    
    print(paste0("table = ", tableName))
    # THINK ABOUT HOW TO FLAG missing tables
    if (tableName %in% uploadList()$MissingTables){
      heatmapData[[tableName]] <- "TABLE MISSING"
      next
    }
    
    
    groupTable <- tableRowsByGroup[[tableName]]
    
    # Next handle tables that don't have groupingVar as one of its columns 
    # if groupTable$numRows is zero that means no link between this table and tblBAS
    if (sum(groupTable$numRows) == 0){
      # if no patient ID in this table
      patientGroups <- FALSE
      groupVar <- "table"
      tableData$table <- tableName
      numRecordsByGroup <- tibble("table" = tableName, "Records" = nrow(tableData))
    } else {
      groupVar <- groupingVar
      ### Analyze this logic

      if ( ("Missing" %in% groupTable[[groupVar]]) && 
           (groupTable[which(groupTable[[groupVar]] == "Missing"), "numRows"] == 0) ){
        #only include Missing if necessary
        groupTable <- groupTable %>% filter(!! rlang::sym(groupVar) != "Missing" )
      }
      patientGroups <- TRUE
      if (any(is.na(tableData[[groupVar]]))){
        tableData[[groupVar]] <- replace_na(tableData[[groupVar]], "Unknown")
      } 

      
      groupTable <- tableRowsByGroup[[tableName]]
      #   groupTable[[groupVar]] <- as.factor(groupTable[[groupVar]]) not necess yet
      numRecordsByGroup <- groupTable %>% rename(Records = numRows)
    }
    
    grouping[[tableName]] <- groupVar
    
    varsRequested <- toInclude[[tableName]]
    
    # varsInTable (requested vars in table) will be in correct order, of requested variables
    varsInTable <- intersect(varsRequested, names(tableData))
    
    if (!tableName %in% tablesWithErrors){
      tableErrorFrame <- NULL
      varsWithErrors <- NULL
    } else {
      tableErrorFrame <- errorFrame %>% filter(table == tableName) %>% 
        select((!! rlang::sym(groupVar)), error_field, errorCode, quantity, starts_with("id"))
      # which requested variables have errors:
      varsWithErrors <- intersect(varsRequested, unique(tableErrorFrame$error_field))
    }
    
    lastVarName <-  NULL
    for (varName in varsRequested){
      print(paste0("gathering dq data: ", varName))
      # for now skip date approximation
    #  if (endsWith(varName, "D_A")) next
      # check first for requested variables that are present in table
      if (varName %in% names(tableData)){
        # document the number of non-blank entries in this column to use as denominator
        if (tableDef[[tableName]][["variables"]][[varName]][["has_codes"]] == "Y"){
          codedVar <- TRUE
          nonBlankRows <- tableData[[varName]] != "Missing"
          knownAndValidRows <- !(tableData[[varName]] %in% c("Missing", "Invalid code", "Unknown"))
        } else {
          codedVar <- FALSE
          knownAndValidRows <- NULL
          # if it's not a coded variable, use uploadedTables to check for missing
          # since formattedTables will be NA for text entries in date field or numeric field, 
          # for example
          nonBlankRows <- !is_blank_or_NA_elements(uploadedTables()[[tableName]][[varName]])
          # for _D/_Y pairs, use only # of _Y == 1 as the "nonBlank" for determining _D
          # completeness
        }
        
        # Evaluate Completeness -----------------------------------------------------
     #   completeness <- checkForDependency(varName, nonBlankRows, groupVar, tableData)
        
        # If this is a variable like xxxRS2 RS3 RS4, use the rowsToUse from the last variable
        useLast <- checkForUseLast(varName, lastVarName)
        # if this is TRUE, use the rowsToUse from the last variable
        
        if (!useLast){
          rowsToUse <- checkForDependency(varName, groupVar, tableData)
        } 
        
        if (is.null(rowsToUse)){
          dependency <- FALSE # this variable is not dependent on other variables
        } else {
          dependency <- TRUE # this variable is dependent on another variable; use subset for completeness
        }
        
        if (!dependency){
          complete <- tableData[nonBlankRows,] %>% group_by( !!rlang::sym(groupVar)) %>% summarise(Complete = n()) %>% ungroup()
          completeness <- numRecordsByGroup %>% left_join(complete, by = groupVar) %>% 
            replace_na(list(Complete = 0)) %>% 
            mutate(Complete = ifelse(Records == 0, NA, Complete)) %>% 
            mutate(Complete_percent = round(100*Complete/Records, 1))
        } else {
   
          recordsSubset <- tableData[rowsToUse, c(groupVar, varName)] %>% 
            group_by((!! rlang::sym(groupVar))) %>% summarise(RecordsSubset = n())
          completeSubset <- tableData[rowsToUse & nonBlankRows, c(groupVar, varName)] %>% 
            group_by((!! rlang::sym(groupVar))) %>% summarise(CompleteSubset = n())
          complete <- tableData[which(nonBlankRows), c(groupVar, varName)] %>% 
            group_by((!! rlang::sym(groupVar))) %>% summarise(Complete = n()) 
          # join these together. If NA is in the Complete column, that means that rows
          # exist for that group but the current variable has no entries complete. Change 
          # those NA's to 0
          completeness <- recordsSubset %>% 
            full_join(completeSubset, by = groupVar) %>% 
            replace_na(list(CompleteSubset = 0)) %>% 
            full_join(complete, by = groupVar) #%>%
          # replace_na(list(CompleteSubset = 0))

          completeness <- numRecordsByGroup %>% left_join(completeness, by = groupVar) %>% 
            replace_na(list(RecordsSubset = 0)) %>% 
            replace_na(list(Complete = 0)) %>% 
            mutate(Complete = ifelse(Records == 0, NA, Complete),
                   Complete_percent = round(100*CompleteSubset/RecordsSubset, 1)
            )
        }
        
        varDQ <- completeness
        # Completeness finished -----------------------------------------------------------
   
        if (codedVar && !endsWith(varName, "D_A") 
            && length(str_split(varName, "_RS")[[1]]) < 2){ # this excludes _RS2 _RS3 _RS4 for now
          if (is.null(rowsToUse)){
            checkCodeRows <- nonBlankRows
          } else {
            checkCodeRows <- rowsToUse
          }
          
          codedDQ <- tableData[checkCodeRows,c(groupVar, varName)] %>%
            filter(!! rlang::sym(varName) %in% c("Unknown", "Invalid Code")) %>%
            group_by((!! rlang::sym(groupVar))) %>%
            summarise(unknowninvalid = n())
          if (dependency){
            codedDQ <- left_join(varDQ, codedDQ, by = groupVar) %>%
              replace_na(list(unknowninvalid = 0)) %>%
              mutate(Known_percent = round(100*(CompleteSubset - unknowninvalid)/RecordsSubset, 1))
          } else {
            codedDQ <- left_join(varDQ, codedDQ, by = groupVar) %>%
              replace_na(list(unknowninvalid = 0)) %>%
              mutate(Known_percent = round(100*(Complete - unknowninvalid)/Records, 1))
          }
          
          
          codedData[[tableName]][[varName]] <- codedDQ[, c(groupVar,"Complete_percent", "Known_percent")] %>%
            rename(percent = Known_percent,
                   complete = Complete_percent)
        }
        
        # Evaluate Logic and DES compliance -----------------------------------------------------------
    
        # If the variable is not in the errorFrame, then no Logic or DES errors: (MAKE SURE FIRST IN TABLE)
        if (!varName %in% varsWithErrors){
          varDQ[["Variable"]] <-  varName
          varDQ[["table"]] <- tableName
          groupsEmpty <- varDQ$Complete == 0
          groupsNoRecords <- varDQ$Records == 0
          varDQ[["Logic_percent"]] <-  100
          varDQ[["DES_percent"]] <-  100
          
          if (any(groupsEmpty, na.rm = TRUE)){
            varDQ[which(groupsEmpty), "Logic_percent"] <- NA
            varDQ[which(groupsEmpty), "DES_percent"] <- 100
            varDQ[which(groupsEmpty), "Complete_percent"] <- 0
          }
          if (any(groupsNoRecords, na.rm = TRUE)){
            varDQ[which(groupsNoRecords), "Logic_percent"] <- NA
            varDQ[which(groupsNoRecords), "DES_percent"] <- NA
            varDQ[which(groupsNoRecords), "Complete_percent"] <- NA
          }
          if (dependency){
            groupsNoRecords <- varDQ$RecordsSubset == 0
            if (any(groupsNoRecords)){
              # DES compliance and Logic refer to all records but completeness is subset
              varDQ[which(groupsNoRecords), "Complete_percent"] <- NA
            }
          }
        } else {
          varErrors <- tableErrorFrame %>%
            filter(error_field == varName)
          
          # if multiple DES errors for a single record, only count once (distinct function removes dups)
          
          desError <- varErrors %>%
            filter(startsWith(errorCode, "1")) %>% select(-errorCode) %>%
            distinct() %>%
            group_by((!! rlang::sym(groupVar))) %>%
            summarise(desError = sum(quantity))
          
          logicError <- varErrors %>%
            filter(startsWith(errorCode, "2")) %>% select(-errorCode) %>%
            distinct() %>%
            group_by((!! rlang::sym(groupVar))) %>%
            summarise(logicError = sum(quantity))
  
          
          varDQ <- varDQ %>% left_join(logicError, by = groupVar) %>% 
            left_join(desError, by = groupVar) %>%
            replace_na(list(logicError = 0, desError = 0)) %>% 
      
            # if excessive errors have caused errors to be reported as a group ("20000 patients in A")
            # rather than by individual patient ID, we could be counting the same record multiple times here.
            
            mutate(logicError = ifelse(logicError > Complete, Complete, logicError)) %>% 
            mutate(desError = ifelse(desError > Records, Records, desError)) %>% 
            mutate(logicError = ifelse(Complete == 0, NA, logicError)) %>% 
            mutate(desError = ifelse(Records == 0, NA, desError)) %>% 
            mutate(Complete = ifelse(Records == 0, NA, Complete)) %>% 
            mutate(Logic = Complete - logicError,
                   Logic_percent = round(100*Logic/Complete, 1),
                   DES = Records - desError,
                   DES_percent = round(100*DES/Records, 1),
                   Variable = varName,
                   table = tableName)
          
          
        }
      } else { # otherwise, this is a requested variable MISSING from table
        varDQ <- numRecordsByGroup %>%
          mutate(Complete = 0,
                 Complete_percent = 0,
                 Logic_percent = NA,
                 DES_percent = NA,
                 Variable = varName,
                 table = tableName)
      }
      
      
      # make vertical table qualityMeasure column
      heatmapVars <- c("Logic_percent", "DES_percent",
                       "Complete_percent")

      forHeatmap <- gather(varDQ, heatmapVars,
                           key = "Metric", value = "percent")
      
      heatmapData[[tableName]][[varName]] <- forHeatmap
      
      lastVarName <- varName
    }
    
    
  }
  
  heatmapList <- list()
  for (tableName in names(toInclude)){
    if (tableName %in% uploadList()$MissingTables){
      # figure out what to do with blank
      heatmapList[[tableName]] <- "Table Missing"
      next
    }
    
    groupName <- grouping[[tableName]]
    tableDqData <- rbindlist(heatmapData[[tableName]], fill = TRUE)
    
    print(paste0("getting heatmaplist together", tableName))

    tableDqData[[groupName]] <- paste0(tableDqData[[groupName]],
                                       " (n=", tableDqData$Records, ")")
    heatmapList[[tableName]] <- createDetailedQualityHeatmap(
      df = tableDqData, 
      yVar = "Variable", 
      facetVar = groupName, 
      numUnknownGroup = tableGroupMissing[[tableName]], 
      tableName = tableName)
  }
  codedDqDataGroups <- list()
  notLinkedExists <- FALSE

  for (tableName in names(codedData)){
    groupName <- grouping[[tableName]]
    # look for tables with connection to valid groups 
    if (groupName == groupingVar){
      codedDqDataGroups[[tableName]] <- rbindlist(codedData[[tableName]], fill = TRUE, idcol = "Variable") %>% 
        mutate(Variable = paste(tableName, Variable, sep = ": ")) %>% 
        filter((!! rlang::sym(groupingVar)) %in% groupLevels)
      # for now, let's eliminate groups not found in tblBAS from the coded summary. Patients 
      # not included in tblBAS will be reflected in the other section of the report

    } else {
    # the records in this table are not mapped to any valid groups

      temp <- rbindlist(codedData[[tableName]], fill = TRUE, idcol = "Variable") %>% 
        mutate(Variable = paste(tableName, Variable, sep = ": "))
      temp[[groupingVar]] <- LABEL_FOR_NOT_LINKED
      codedDqDataGroups[[tableName]] <- temp
      notLinkedExists <- TRUE
    }
  }
  # were there any coded variables?
  codedHeatmap <- NULL
  if (!is_empty(codedDqDataGroups)){
    codedAll <- rbindlist(codedDqDataGroups, fill = TRUE, idcol = "Table")
    # for now, let's eliminate groups not found in tblBAS from the coded summary. Patients 
    # not included in tblBAS will be reflected in the other section of the report

    levels <- groupLevels
    
    if (notLinkedExists){
      levels <- c(levels, LABEL_FOR_NOT_LINKED)
      notLinkedCaption <- expression(paste(""^"*", italic("Not Linked:"), 
                                           " Records in some tables are not linked to patient IDs/groups"))
    } else notLinkedCaption <- ""
    
    codedAll <- codedAll %>% filter((!! rlang::sym(groupingVar)) %in% levels)
    # now need to set factor levels for Groups and include Unknown and Not linked at the end
    # if they exist
    
    codedAll[[groupingVar]] <- factor(codedAll[[groupingVar]], levels = levels)
    
    # to keep tables/variables in DES order, make factors first and force to correct order:
    # otherwise "complete" function will place in alphabetic order
    codedAll$Variable <- factor(codedAll$Variable, levels = unique(codedAll$Variable))
    codedAll <- tidyr::complete(codedAll, (!! rlang::sym(groupingVar)), Variable)

  
    codedHeatmap <-
      generateFacetHeatMapDQ(
        df = codedAll,
        xVar = groupingVar,
        yVar = "Variable",
        facetVar = NULL,
        titleText = "Coded Variables: Useful Information",
        caption = notLinkedCaption)
  } 
  #addMetricsToREDCap(heatmapData)
  return(list(heatmapList = heatmapList,
              codedHeatmap = codedHeatmap))
  
}


createdqHeatmaps <- reactive({
  if (is.null(errorTable()[[1]])) return(NULL)
  if (is.null(formattedTables())) return(NULL)
  allHeatmaps <- summarizeDQMetrics(errorTable()$errorDetail, 
                                    tableRowsByGroup(), 
                                    formattedTables(),
                                    finalGroupChoice(),
                                    input$dqVars)
 
  return(allHeatmaps)
})


