createStatsSummary <- function(tableData, programName, reportFormat){
  allVariablesInTables <- unlist(lapply(tableData,function(x){return(names(x))}), use.names = FALSE)
  variablesInReport <- intersect(names(toReport), allVariablesInTables)
  numberOfVariables <- length(variablesInReport)
  if (numberOfVariables == 0) summaryTableFactors <- NULL
  else {
    summaryTableFactors <-data.frame(Value = character(),
                                     Count = character(),
                                     Percent = character(),
                                     stringsAsFactors = FALSE)
    numberInGroup <- NULL
    
    for (variable in variablesInReport){
      
      report <- toReport[[variable]]
      headingText <- report$displayName
      variableData <- tableData[[report$table]][[variable]]
      if (sum(!is.na(variableData)) == 0){
        Value <- "NA"
        Count <- length(variableData)
        Percent <- 100.0
        
      }
      
      else if (report$type == "factor"){
        stats <- describe(tableData[[report$table]][[variable]])
        Value <- c(stats$values$value, "Missing")
        Count <- as.numeric(c(stats$values$frequency, stats$counts[["missing"]]))
        Percent <- round(100*Count/(as.numeric(stats$counts[["n"]])+ as.numeric(stats$counts[["missing"]])),1)
        
        
      }
      else if (report$type == "date"){
        
        variableData <- lubridate::year(variableData)
        br <- seq(min(variableData, na.rm = TRUE),max(variableData, na.rm = TRUE),by=1)
        count <- hist(variableData, br, plot=FALSE)
        
        Value <- c(count$breaks[2:length(br)], "Missing")
        Count <- as.numeric(c(count$counts, sum(is.na(variableData))))
        Percent <- round(100*Count/(length(variableData)),1)
        
      }
      statsCount <- data.frame(Value, Count, Percent)
      numberInGroup[[headingText]] <- nrow(statsCount)
      summaryTableFactors <- rbind(summaryTableFactors, statsCount)
    }
  }
  
  
  
  # tableTitle <- "Patient Characteristics "
  # if (!is.null(programName)){
  #   tableTitle <- paste0(tableTitle,"(",programName,")" )
  # }
  # 
  if (is.null(summaryTableFactors)) return(NULL)
  
  print(paste0("report format ",reportFormat))
  if (reportFormat == "latex"){
    statsSummaryTable <- kable(summaryTableFactors, longtable=T, booktabs = T, format = reportFormat, 
                               caption  = "Summary statistics from uploaded tables") %>% 
      kable_styling(latex_options = c("repeat_header")) %>% 
      group_rows(index = numberInGroup)
  } else {
    statsSummaryTable <- kable(summaryTableFactors, longtable=T, format = reportFormat) %>% 
      kable_styling(latex_options = c("repeat_header"), 
                    bootstrap_options = c("striped","bordered"), full_width = F, position = "left") %>% 
      
      group_rows(index = numberInGroup)
  }
  print("finished statssummarytable")
  return(statsSummaryTable)
}

earlyYears <-  function(x){
  x[which(x<minYearForVisitStats)] <- paste0("< ", minYearForVisitStats)
  return(x)
}

# JUDY revisit: what about excluding duplicate entries?
createVisitStats <- function(tableData, programName, reportFormat){
  visitStats <- NULL
  visitStats$enrol <- tableData$tblBAS  %>% select(PATIENT, ENROL_D) %>% 
    filter(ENROL_D != dateIndicatingUnknown) %>%
    filter(!is.na(ENROL_D)) %>% 
    filter(ENROL_D <= Sys.Date()) %>% 
    mutate(Year1 = year(ENROL_D)) %>% 
    mutate(Year = earlyYears(Year1)) %>% 
    group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[1]])
  if ("tblVIS" %in% uploadList()$AllDESTables){
    visitStats$visits <- tableData$tblVIS %>% select(PATIENT, VIS_D) %>% 
      filter(VIS_D != dateIndicatingUnknown) %>%
      filter(!is.na(VIS_D)) %>% 
      distinct(PATIENT, VIS_D, .keep_all = TRUE) %>% 
      filter(VIS_D <= Sys.Date()) %>% 
      mutate(Year1 = year(VIS_D)) %>% 
      mutate(Year = earlyYears(Year1)) %>% 
      group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[2]])
  }
  if ("tblLTFU" %in% uploadList()$AllDESTables){
    if ("DEATH_D" %in% names(tableData$tblLTFU)){
      visitStats$deaths <- tableData$tblLTFU %>% select(PATIENT, DEATH_D) %>% 
        filter(DEATH_D != dateIndicatingUnknown) %>% 
        filter(!is.na(DEATH_D)) %>% 
        distinct(PATIENT, .keep_all = TRUE) %>% 
        filter(DEATH_D <= Sys.Date()) %>% 
        mutate(Year1 = year(DEATH_D)) %>% 
        mutate(Year = earlyYears(Year1)) %>% 
        group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[3]])
    }
    if ("DROP_D" %in% names(tableData$tblLTFU)){
      if ("DROP_RS" %in% names(tableData$tblLTFU)){
        visitStats$transferred <-tableData$tblLTFU %>% select(PATIENT, DROP_D, DROP_RS) %>% 
          filter(DROP_RS %in% codesIndicatingTransfer) %>%  
          filter(DROP_D != dateIndicatingUnknown) %>%
          filter(!is.na(DROP_D)) %>% 
          filter(DROP_D <= Sys.Date()) %>% 
          mutate(Year1 = year(DROP_D)) %>% 
          mutate(Year = earlyYears(Year1)) %>% 
          group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[4]])
      }
    }
  }
  if ("tblLAB_RNA" %in% uploadList()$AllDESTables){
    visitStats$viralLoad <- tableData$tblLAB_RNA %>% select(PATIENT, RNA_D) %>% 
      filter(RNA_D != dateIndicatingUnknown) %>%
      filter(!is.na(RNA_D)) %>% 
      filter(RNA_D <= Sys.Date()) %>% 
      mutate(Year1 = year(RNA_D)) %>% 
      mutate(Year = earlyYears(Year1)) %>% 
      group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[5]])
  }
  if ("tblLAB_CD4" %in% uploadList()$AllDESTables){
    visitStats$CD4 <- tableData$tblLAB_CD4 %>% select(PATIENT, CD4_D) %>% 
      filter(CD4_D != dateIndicatingUnknown) %>%
      filter(!is.na(CD4_D)) %>% 
      filter(CD4_D <= Sys.Date()) %>% 
      mutate(Year1 = year(CD4_D)) %>% 
      mutate(Year = earlyYears(Year1)) %>% 
      group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[6]])
  }

  allData <- rbindlist(visitStats, use.names = TRUE, fill = TRUE)
  # make sure recent years all filled in, even if 0
  minYear <- minYearForVisitStats
  maxYear <- year(Sys.Date())
  years <- as.numeric(unique(allData$Year[!grepl("[[:punct:]]",allData$Year)]))
  missingYears <- setdiff(minYear:maxYear, years)
  numberMissingYears <- length(missingYears)
  newRows <- data_frame(Year = as.character(missingYears),
                        Count = rep(0, numberMissingYears),
                        Variable = rep("Enrolled", numberMissingYears))
  allData <- bind_rows(allData, newRows)
  visitStatsOut <- spread(allData, key = Year, Count, fill = 0)
  # arrange rows in desired order
  visitStatsOut$Variable <- factor(visitStatsOut$Variable, 
                                   levels = visitStatsToReport)
  visitStatsOut <- visitStatsOut %>% arrange(Variable) %>% 
    mutate(Total = rowSums(.[grepl("\\d", names(.))]))
  
  tableTitle <- "Summary (need title here) "
  if (!is.null(programName)){
    tableTitle <- paste0(tableTitle,"(",programName,")" )
  }
  print(paste0("report format ",reportFormat))
  if (reportFormat == "latex"){
    visitSummaryTable <- kable(visitStatsOut, longtable=T, booktabs = T, format = reportFormat, 
                               caption  = "Number of observations per year") %>% 
      kable_styling(latex_options = c("repeat_header")) 
  } else {
    visitSummaryTable <- kable(visitStatsOut, longtable=T, format = reportFormat,
                               caption  = "Number of observations per year") %>% 
      kable_styling(latex_options = c("repeat_header"), 
                    bootstrap_options = c("striped","bordered"), full_width = F, position = "left") 
  }
  return(visitSummaryTable)
}

# JUDY revisit: what about excluding duplicate entries?
createOtherStats <- function(tableData, programName, reportFormat){
  stats <- NULL
  if (exists("tblVIS", tableData)){
    if (exists("PROGRAM", tableData$tblVIS) && exists("VIS_D", tableData$tblVIS)){
      stats$fewVisits <- tableData$tblVIS  %>% select(PATIENT, PROGRAM, VIS_D) %>% 
        distinct() %>% 
        group_by(PATIENT, PROGRAM) %>% summarise( Visits = n()) %>% 
        ungroup() %>% filter(Visits < 2) %>% group_by(PROGRAM) %>% summarise(Number = n()) %>% 
        ungroup() %>% mutate(Variable = otherStatsToReport[[1]])
    }
  }
  
  if (exists("tblLTFU", tableData)){
    if (exists("PROGRAM", tableData$tblLTFU) && exists("DEATH_Y", tableData$tblLTFU) && 
        exists("DROP_RS", tableData$tblLTFU)){
      stats$ltfu <- tableData$tblLTFU  %>% select(PATIENT, PROGRAM, DEATH_Y, DROP_RS) %>%
        distinct(PATIENT, .keep_all = TRUE) %>% 
        filter(DEATH_Y=="Yes" | DROP_RS %in% codesIndicatingTransfer) %>% select(PROGRAM) %>% 
        group_by(PROGRAM) %>% 
        summarise(Number = n()) %>% mutate(Variable = otherStatsToReport[[2]])
    }
  }
  
  if (exists("tblLAB_RNA", tableData)){
    # add back in rna_v
    if (all(c("PROGRAM", "RNA_D") %in% names(tableData$tblLAB_RNA))){
      stats$rna <- tableData$tblLAB_RNA %>% select(PROGRAM, PATIENT, RNA_D) %>% #, RNA_V
        distinct(PATIENT, RNA_D, .keep_all = TRUE) %>% group_by(PROGRAM, PATIENT) %>% 
        summarise(counts = n()) %>% ungroup() %>% group_by(PROGRAM) %>% summarise(Number = median(counts)) %>% 
        mutate(Variable = otherStatsToReport[[4]])
    }
  }
  
 
  
  allData <- rbindlist(stats, use.names = TRUE, fill = TRUE)
  # make sure recent years all filled in, even if 0
 

  statsOut <- spread(allData, key = PROGRAM, Number, fill = 0)
  # arrange rows in desired order
  statsOut$Variable <- factor(statsOut$Variable, 
                                   levels = otherStatsToReport)
  statsOut <- statsOut %>% arrange(Variable) 
  
  print(paste0("report format ",reportFormat))
  if (reportFormat == "latex"){
    statSummaryTable <- kable(t(statsOut), longtable=T, booktabs = T, format = reportFormat, 
                               caption  = "Dataset Overview by PROGRAM") %>% 
      kable_styling(latex_options = c("repeat_header")) 
  } else {
    statSummaryTable <- kable(t(statsOut), longtable=T, format = reportFormat,
                               caption  = "Dataset Overview by PROGRAM") %>% 
      kable_styling(latex_options = c("repeat_header"), 
                    bootstrap_options = c("striped","bordered"), full_width = F, position = "left") 
  }
  return(statSummaryTable)
}


generateHistogramGrid <- function(tableData, programName, dateFields){
  tablesToInclude <- intersect(desiredTables, names(tableData))
  dateDataset <- NULL
  for (tableName in tablesToInclude){
    table <- get(tableName, tableData)
    variablesToInclude <- intersect(desiredPlots, names(table))
    if (length(variablesToInclude) == 0) next
    for (dateVariable in variablesToInclude){
      datesInTable <- table %>% 
        gather(!!dateVariable, key = "DateName", value = "Dates") %>% 
        select("DateName", "Dates") 
      dateDataset <- bind_rows(dateDataset, datesInTable)
    }
    
  }
  # ensure that histograms will be in desired order, as specified in dateFields
  dateDataset$DateName = factor(dateDataset$DateName, levels=dateFields)
  
  if (!is.null(dateDataset) && any(!is.na(dateDataset$Dates))){
    dateRange <- "fullRange"
    if (!is.null(input$fullOrRange)){
      dateRange <- input$fullOrRange
    }
    if (dateRange == "chooseRange"){
      minYear <- input$yearsToPlot[[1]]
      maxYear <- input$yearsToPlot[[2]]
    }
    else {
      minYear <- year(min(dateDataset$Dates[dateDataset$Dates > "1970-01-01"], na.rm = TRUE))
      maxYear <- as.numeric(format(Sys.Date(), "%Y"))
    }
    xmax <- as.Date(paste0(maxYear,"-12-31"))
    print(xmax)
    xmin <- as.Date(paste0(minYear, "-01-01"))
    
    p <- ggplot(dateDataset, aes(Dates, label = dateDataset$Dates)) + #text=paste0("Date:", as.Date(Dates))
      geom_histogram(boundary = xmin,
                     binwidth = 91.25,
                     colour = "#1F3552", #"blue",
                     alpha = .8) +
      facet_grid(DateName~.,scales =  "free_y", drop = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   limits = c(xmin, xmax),
                   expand = c(0,0)) +
      background_grid(major = 'x', minor = 'x') +
      theme_bw() + 
      theme(
        #  plot.title = element_text(size=8),
        axis.text.x=element_text(angle=90,vjust = 0.5),
        plot.title = element_text(size=9),
        #  axis.text=element_text(size=10),
        #  axis.title = element_text(size = 12),
        strip.text.y = element_text(size=7.5)
        #  strip.background = element_rect(fill="#FFFFFF")
      ) + 
      xlab("") +
      ylab("Number of observations") +
      labs(title = paste0("PROGRAM: ", sanitizeNames(programName)))
    gp <- p #ggplotly(p, tooltip = c("label","y"))
  }
  else gp <- NULL
  
  return(gp)
}


generateHistogramsAllPrograms <- function(){
  programs <- unique(formattedTables()$tblBAS$PROGRAM)
  
  datesToInclude <- intersect(desiredPlots, unlist(matchingColumns()))
  plotList <- NULL
  for (programName in programs){
    tableData <- lapply(formattedTables(), 
                        function(x,y){x %>% filter(PROGRAM == y)},
                        y=programName)
    plotList[[programName]] <- generateHistogramGrid(tableData, programName, datesToInclude)
  }
  return(plotList)
}

generateQualitySummaryPlots <- function(programSummary, programName){
  tableTitle <- "Percent Missing/Unknown Data"
  if (is.null(programSummary$missingAndUnknownSummary)) missingAndUnknownPlot <- NULL
  else {
    numMissingAndUnknown <-  nrow(programSummary$missingAndUnknownSummary)
    if (numMissingAndUnknown == 0) missingAndUnknownPlot <- NULL
    if (numMissingAndUnknown == 1){
      toShow <- programSummary$missingAndUnknownSummary
      toShow <- toShow %>% select(Table = table, Variable = variable, number, percent) %>% 
        mutate("Count(%)" = paste0(number, "(",percent,")")) %>% 
        select(-number, -percent)
    
      if (input$reportType == "html"){
        missingAndUnknownPlot <- kable(toShow, format = "html", caption = tableTitle) %>% 
         kable_styling("bordered", full_width = FALSE, position = "left") 
      }
      if (input$reportType == "pdf") {
        missingAndUnknownPlot <- kable(toShow, caption = tableTitle, format = "latex", longtable = T, booktabs = T) %>% 
          kable_styling(latex_options = c("repeat_header"))
      }
    }
    if (numMissingAndUnknown > 1){
      dataset <- programSummary$missingAndUnknownSummary 
      missingAndUnknownPlot <- ggplot(data = dataset, aes(x = reorder(variable, -percent), y = percent)) + 
        geom_bar(stat = "identity", color = "gray") +
        
        labs(title=tableTitle) +
        theme(legend.position="none") +
        xlab("variable") +
        ylab("% missing or Unknown") +
        coord_flip()
    }
  } 
  
  if (is.null(programSummary$appearanceSummary)) appearancePlot <- NULL
  else {
    numAppearances <- nrow(programSummary$appearanceSummary)
    if (numAppearances == 0) appearancePlot <- NULL
    if (numAppearances == 1) appearancePlot <- NULL
    if (numAppearances > 1){
      plotTitle <- "Patients from tblBAS"
      
      appearancePlot <- ggplot(data = programSummary$appearanceSummary, 
                               aes(x= reorder(table,-percent), y=percent, fill = "lightgrey")) + 
        geom_bar(stat = "identity") + 
        labs(title=plotTitle) +
        theme(legend.position="none") + 
        xlab("table") +
        ylab("% patients from tblBAS included") +
        coord_flip()
    }
  }
  
  return(list(missingAndUnknownPlot = missingAndUnknownPlot,
              appearancePlot = appearancePlot))
}

# generateErrorSummaryTables: errorSummary dataframe must have table and errorType columns
generateErrorSummaryTables <- function(errorSummary, caption = "", reportFormat, programName = NULL){
  if (is_empty(errorSummary) || nrow(errorSummary) == 0) return(NULL)
  tableNames <- unique(errorSummary$Table)
  displayOrder <- set_names(1:length(tableNames), tableNames)
  # to preserve desired table order, create numeric grouping column display
  errorSummary$display <- displayOrder[errorSummary$Table]
  errorSummaryShow <- errorSummary %>% arrange(display, desc(Count), Variable)
  groupingInfo <- errorSummaryShow %>% group_by(display, Table) %>% summarise(n = n())
  groupRows <- set_names(groupingInfo$n, groupingInfo$Table)
  errorSummaryShow <- errorSummaryShow %>% select(-Table, -display) 
  if (exists("Error", errorSummaryShow)) errorSummaryShow <- errorSummaryShow %>% rename(Description = "Error")
  if (reportFormat =="pdf"){ reportFormat <- "latex"}
  if (reportFormat == "latex"){
    errorSumTable <- kable(errorSummaryShow, longtable=T, booktabs = T, format = reportFormat, 
                           caption  = caption) %>% 
      kable_styling(latex_options = c("repeat_header")) %>% 
      group_rows(index = groupRows)
  } else { # for html caption is blank; caption in headings in rmd so that navigation enabled
    errorSumTable <- kable(errorSummaryShow, longtable=T, format = reportFormat) %>% 
      kable_styling(latex_options = c("repeat_header"), 
                    bootstrap_options = c("striped","bordered"), full_width = F, position = "left") %>% 
      
      group_rows(index = groupRows)
  }
  return(errorSumTable)
}




# generateDatasetSummary is passed tableData that has already been filtered for program == programName
generateDatasetSummary <- function(tableData, reportFormat, programName = NULL){
  tableTitle <- "Table Summary "
  if (!is.null(programName)){
   # tableTitle <- paste0(tableTitle,"(",programName,")")
  }
  totalPatients <- nrow(tableData$tblBAS)
  #compile list of valid PATIENT ids; only include those in tableSummary
  validPatients <- tableData$tblBAS$PATIENT
  
  # set up tableSummary data frame to include counts by age Group
  colNames <- c("Table","Records","Patients", names(ageGroups))
  tableSummary <- data.frame(matrix(vector(),ncol=length(colNames)))
  colnames(tableSummary) <-colNames
  
  row <- 1
  for (tableName in c("tblBAS", uploadList()$tablesWithPatientID)){
    results <- tableData[[tableName]] %>% mutate(Table = tableName) %>% mutate(Records = n()) %>% 
      filter(PATIENT %in% validPatients) %>% distinct(PATIENT,.keep_all = TRUE) %>% mutate(Patients = n()) %>% 
      filter(!is.na(ageGroup)) %>% 
      group_by(Table, Records, Patients, ageGroup) %>% summarise(number = n()) %>% spread(ageGroup, number)
    results <- as.data.frame(results)
    # if this is a report for one program there may not be any records in this table matching that program
    if (nrow(results) == 0){
      results <-  NULL
      results$Table <- tableName
      results$Records <- 0
    }
    
    tableSummary[row,names(results)] <- results
    row <- row + 1
  }
  tableSummary <- tableSummary %>% replace(is.na(.),0)
  
  for (tableName in setdiff(uploadList()$tablesWithNoPatientID, pregnancyTables)){ #JUDY fix this when we know how to handle preg tables
    results <- tableData[[tableName]] %>% mutate(Table = tableName) %>% group_by(Table) %>% summarise(Records = n())  
    results <- as.data.frame(results)
    # if this is a report for one program there may not be any records in this table matching that program
    if (nrow(results) == 0){
      results <-  NULL
      results$Table[[1]] <- tableName
      results$Records[[1]] <- 0
    }
    tableSummary[row,names(results)] <- results
    row <- row+1
  }
  # since age groups and patient counts don't apply to tables without Patient ID, replace NA with blank
  options(knitr.kable.NA = '')  
  
  if (reportFormat =="pdf"){ reportFormat <- "latex"}
  
  if (reportFormat == "html"){
    tableSummaryKable <- kable(tableSummary, format = "html", caption = tableTitle) %>% 
      kable_styling("bordered", full_width = FALSE, position = "left") %>% 
      add_header_above(c(" " = 3, "Age at enrollment" = length(ageGroups)))
  }
  else {
    tableSummaryKable <- kable(tableSummary, format = "latex", longtable = T, booktabs = T, caption = tableTitle) %>% 
      add_header_above(c(" " = 3, "Age at enrollment" = length(ageGroups)))
                            #   caption = tableTitle, longtable = T, booktabs = T) %>% 
    #  add_header_above(c(" " = 3, "Age at enrollment" = length(ageGroups))) #%>% 
     # kable_styling(latex_options = c("repeat_header"))
  }
  
  statsSummaryTable <- createStatsSummary(tableData, programName, reportFormat)
  visitStatsSummaryTable <- createVisitStats(tableData, programName, reportFormat)
 # otherStatsTable <- createOtherStats(tableData, programName, reportFormat)
  print("finished visitstats")
  
  return(list(
    totalPatients = totalPatients,
    tableSummaryKable = tableSummaryKable,
    statsSummaryTable = statsSummaryTable,
    visitStatsSummaryTable = visitStatsSummaryTable#,
    #otherStatsTable = otherStatsTable
  ))
}

summarizeForOneProgram <- function(programName, tableData){
  if (is_empty(errorTable()$errorDetail)) programErrorSummaries <- NULL
  else {
    errorsInThisProgram <- errorTable()$errorDetail %>% filter(PROGRAM == programName)
    if (nrow(errorsInThisProgram) == 0) programErrorSummaries <- NULL
    else programErrorSummaries <- summarizeErrors(errorsInThisProgram, tableData)
  }
  
  if (nrow(errorTable()$missingSummaryByProgram) > 0){
    programMissing <- errorTable()$missingSummaryByProgram %>% 
      filter(PROGRAM == programName) %>% select(-PROGRAM, -category)
  } else programMissing <- NULL
  
  if (nrow(errorTable()$unknownCodeSummaryByProgram) > 0){
    programUnknown <- errorTable()$unknownCodeSummaryByProgram %>% 
      filter(PROGRAM == programName) %>% select(-PROGRAM, -category)
  } else programUnknown <- NULL
  
  if (nrow(errorTable()$missingAndUnknownByProgram) > 0){
    programMissingAndUnknown <- errorTable()$missingAndUnknownByProgram %>% filter(PROGRAM == programName) %>% select(-PROGRAM)
  } else programMissingAndUnknown <- NULL
  if (!is.null(errorTable()$appearanceSummary)){
    programAppearance <- errorTable()$appearanceSummary %>% filter(PROGRAM == programName) %>% select(-PROGRAM)
  } else programAppearance <- NULL
  return(list("errorSummaries" = programErrorSummaries, 
              "missingSummary" = programMissing,
              "unknownSummary" = programUnknown,
              "missingAndUnknownSummary" = programMissingAndUnknown,
              "appearanceSummary" = programAppearance))
}



reportMessage <- function(currentTable = NULL, currentProgram = NULL){
  lastActivity(Sys.time())
  if (!is.null(currentTable)){
    tableMessage <- tags$h4(paste0("Currently processing table ", currentTable))
  } else tableMessage <- NULL
  if (!is.null(currentProgram)){
    programMessage <- tags$h4(paste0("Currently processing data from Program ", currentProgram))
  } else programMessage <- NULL
  reportModal(tableMessage = tableMessage, programMessage = programMessage)
}

# create a report -----------------------------------------------------
createReport <- function(file, reportType = c("PDF", "html"),
                         includeHistograms = input$includeHistograms,
                         includeDataSummary = input$includeDataSummary,
                         includeErrorSummary = input$includeErrorSummary,
                         datasetDesc = input$datasetDesc) {
  cat("createReport output file: ", file, "\n", sep = "", file = stderr())
  
  if (includeHistograms){
    plotList <- generateHistogramsAllPrograms()
  } else plotList <- NULL
  tableData <- formattedTables()
  datasetSummary <- generateDatasetSummary(tableData, reportFormat = tolower(reportType))
  errorSumTable <- generateErrorSummaryTables(errorTable()$errorOnlySummary, caption = "Summary of Errors", reportFormat = tolower(reportType))
  warnSumTable <- generateErrorSummaryTables(errorTable()$warnOnlySummary, caption = "Summary of Warnings", reportFormat = tolower(reportType))
  badCodeSumTable <- generateErrorSummaryTables(errorTable()$badCodeSummary, caption = "Summary of Invalid Codes", reportFormat = tolower(reportType))
  errorSummaryTables <- list("errors" = errorSumTable,
                             "warnings" = warnSumTable,
                             "badCodes" = badCodeSumTable)
  params <- list(
    allTables = formattedTables(),
    extraFiles = uploadList()$ExtraFiles,
    tableOfVariables = tablesAndVariables$list$variableSummaryToDisplay,
    datasetSummary = datasetSummary,
    errorSummary = errorTable()$errorSummary,
    highLevel = errorTable()$highLevelErrorSummary,
    badCodes = errorTable()$badCodeSummary,
    missingSummary = errorTable()$missingSummary,
    missingSummaryByProgram = errorTable()$missingSummaryByProgram,
    unknownCodeSummaryByProgram = errorTable()$unknownCodeSummaryByProgram,
    appearanceSummary = errorTable()$appearanceSummary,
    errorTable = errorTable(),
    datasetDesc = input$datasetDesc,
    plotList = plotList,
    includeDataSummary = includeDataSummary,
    includeHistograms = includeHistograms,
    includeErrorSummary = includeErrorSummary,
    region = userDetails()$regionName,
    userDetails = userDetails(),
    toReport = toReport,
    programsInReport = input$programsInReport,
    reportOutput = paste0(reportType,"_document"),
    errorSummaryTables = errorSummaryTables
  )
  tempReportName <-  paste0("report", reportType,".rmd")
  dir <- dirname(file)
  cat("createReport output directory: ", dir, "\n", sep = "", file = stderr())
  tempReport <- file.path(dir, tempReportName)
  tempImage <- file.path(dir, "iedeaLogoSmall.png")
  file.copy("iedeaLogoSmall.png", tempImage, overwrite = TRUE)
  file.copy(tempReportName, tempReport, overwrite = TRUE)
  knitr::knit_meta(class=NULL, clean = TRUE)
  
  rmarkdown::render(tempReport, output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

output$reportpdf <- downloadHandler(
  filename = "report.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
   
    reportMessage()
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = "allProgramsSummary",
                                             report_format = "pdf",
                                             stringsAsFactors = FALSE
                                           ))
    createReport(file, "PDF")
    removeModal()
  }
)

# download html report -----------------------------------------------------   
output$reporthtml <- downloadHandler(
  filename = "report.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    if (input$includeHistograms){
      plotList <- generateHistogramsAllPrograms()
    } else plotList <-  NULL
    reportMessage()
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = "allProgramsSummary",
                                             report_format = "html",
                                             stringsAsFactors = FALSE
                                           ))
    createReport(file, "html")
    removeModal()
  }
)

# download html report of error detail-----------------------------------------   
output$reportErrorsInHTML <- downloadHandler(
  filename = "reportErrors.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    reportMessage()
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = "errorDetail",
                                             report_format = "html",
                                             stringsAsFactors = FALSE
                                           ))
    programName <- input$programsInErrorHTML
    params <- list(
      errorSummary = errorTable()$errorSummary,
      badCodes = errorTable()$badCodeSummary,
      missingSummary = errorTable()$missingSummary,
      missingAndUnknown = errorTable()$missingAndUnknown,
      errorsByTable = errorTable()$errorsByTable,
      region = userDetails()$regionName,
      userDetails = userDetails()
    )
    tempReport <- file.path(tempdir(), "report_errors_html.Rmd")
    tempImage <- file.path(tempdir(), "iedeaLogoSmall.png")
    file.copy("iedeaLogoSmall.png", tempImage, overwrite = TRUE)
    file.copy("report_errors_html.Rmd", tempReport, overwrite = TRUE)
    knitr::knit_meta(class=NULL, clean = TRUE)
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
    removeModal()
  }
)


createReportOneProgram <- function(file, input, program){
  reportMessage(currentProgram = program)
  
  reportType <- input$reportType
  
  tableData <- lapply(formattedTables(), 
                      function(x,y){x %>% filter(PROGRAM == y)},
                      y=program)
  
  if (input$includeHistograms){
    datesToInclude <- intersect(desiredPlots, unlist(matchingColumns()))
    p <- generateHistogramGrid(tableData, program, datesToInclude)
  }
  if (input$includeErrorSummary){
    programSummary <- summarizeForOneProgram(program, tableData)
    errorSumTable <- generateErrorSummaryTables(programSummary$errorSummaries$errorOnlySummary, caption = "Summary of Errors", reportFormat = tolower(reportType))
    warnSumTable <- generateErrorSummaryTables(programSummary$errorSummaries$warnOnlySummary, caption = "Summary of Warnings", reportFormat = tolower(reportType))
    badCodeSumTable <- generateErrorSummaryTables(programSummary$errorSummaries$badCodeSummary, caption = "Summary of Invalid Codes", reportFormat = tolower(reportType))
    errorSummaryTables <- list("errors" = errorSumTable,
                               "warnings" = warnSumTable,
                               "badCodes" = badCodeSumTable)
    
    
    
    datasetQuality <- generateQualitySummaryPlots(programSummary, program)
  } else {
    programSummary <- NULL
    errorSummaryTables <- NULL
    datasetQuality <- NULL
  }
  
  datasetSummary <- generateDatasetSummary(tableData, reportFormat = tolower(reportType), programName = program)


  filename <- paste0("reportOneProgram",toupper(reportType),".rmd")
  params <- list(
    allTables = tableData,
    datasetSummary = datasetSummary,
    datasetQuality = datasetQuality,
    programSummary = programSummary,
    datasetDesc = input$datasetDesc,
    p = p,
    includeDataSummary = input$includeDataSummary,
    includeHistograms = input$includeHistograms,
    includeErrorSummary = input$includeErrorSummary,
    region = userDetails()$regionName,
    userDetails = userDetails(),
    toReport = toReport,
    programsInReport = sanitizeNames(program),
    errorSummaryTables = errorSummaryTables
  )
  tempReport <- file.path(tempdir(), filename)
  tempImage <- file.path(tempdir(), "iedeaReportHeader.png")
  file.copy("iedeaReportHeader.png", tempImage, overwrite = TRUE)
  tempImage2 <- file.path(tempdir(), "iedeaLogoSmall.png")
  file.copy("iedeaLogoSmall.png", tempImage2, overwrite = TRUE)
  file.copy(filename, tempReport, overwrite = TRUE)
  knitr::knit_meta(class=NULL, clean = TRUE)
  rmarkdown::render(tempReport, output_file = file,
                    # params = params2#,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}


# download pdf report of One Program-----------------------------------------------------   
output$reportOneProgramPDF <- downloadHandler(
  filename = function(){paste("reportProgram",sanitizeNames(input$programsInReport),userDetails()$user, Sys.Date(),".pdf",sep="_")}, #reportOneProgram.pdf",
  content = function(file) {
    reportMessage()
    createReportOneProgram(file, input, input$programsInReport)
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = paste0("oneProgram:",sanitizeNames(input$programsInReport)),
                                             report_format = "pdf",
                                             stringsAsFactors = FALSE
                                           ))
    removeModal()
  }
)

# download html report -----------------------------------------------------   
output$reportOneProgramHTML <- downloadHandler(
  filename = function(){paste("reportProgram",sanitizeNames(input$programsInReport),userDetails()$user, Sys.Date(),".html",sep="_")}, #reportOneProgram.pdf",
  content = function(file) {
    reportMessage()
    createReportOneProgram(file, input, input$programsInReport)
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = paste0("oneProgram:",sanitizeNames(input$programsInReport)),
                                             report_format = "html",
                                             stringsAsFactors = FALSE
                                           ))
    removeModal()
  }
)




output$reportZipAll <- downloadHandler(
  filename = function(){paste0("reportProgram",sanitizeNames(input$programsInReport),userDetails()$user, Sys.Date(),".zip")}, 
  content = function(file) {
    reportMessage()
    programNames <- unique(na.omit(formattedTables()$tblBAS$PROGRAM))
    for (programName in programNames){
      print(paste0("creating report program ", programName))
      createReportOneProgram(paste0(sanitizeNames(programName), '.', input$reportType), input, programName)
    }
    reportFilenames <- paste0(sanitizeNames(programNames), '.',input$reportType)
    oldwd <- getwd()
    setwd(tempdir())
    zip::zip(zipfile = file, reportFilenames)
    setwd(oldwd)
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                      data.frame(
                                        action_ts = as.character(Sys.time()),
                                        action_step = "downloadreport",
                                        report_type = paste0("individual reports, zipped"),
                                        report_format = input$reportType,
                                        stringsAsFactors = FALSE
                                      ))
    removeModal()
  }
)
