createStatsSummary <- function(tableData, reportFormat){
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
      # if the report$table doesn't include any valid patients, skip this variable
      if (length(variableData) == 0) next
      
      if (sum(!is.na(variableData)) == 0){
        Value <- "Missing"
        Count <- length(variableData)
        Percent <- 100.0
      }
      
      else if (report$type == "factor"){
        stats <- describe(tableData[[report$table]][[variable]])
        # If "Missing" is not one of the factor levels, compute missing 
        if (stats$counts[["missing"]] > 0){
          Value <- c(stats$values$value, "Missing")
          Count <- as.numeric(c(stats$values$frequency, stats$counts[["missing"]]))
          Percent <- round(100*Count/(as.numeric(stats$counts[["n"]])+ as.numeric(stats$counts[["missing"]])),1)
        } else {
          Value <- stats$values$value
          Count <- as.numeric(stats$values$frequency)
          Percent <- round(100*Count/as.numeric(stats$counts[["n"]]),1)
        }
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
      kableExtra::group_rows(index = numberInGroup) %>% 
      kable_styling(latex_options = c("repeat_header"))
  } else {
    statsSummaryTable <- kable(summaryTableFactors, longtable=T, format = reportFormat) %>% 
      kableExtra::group_rows(index = numberInGroup) %>% 
      kable_styling(latex_options = c("repeat_header"), 
                    bootstrap_options = c("striped","bordered"), full_width = F, position = "left")
  }
  print("finished statssummarytable")
  return(statsSummaryTable)
}

earlyYears <-  function(x){
  x[which(x<minYearForVisitStats)] <- paste0("< ", minYearForVisitStats)
  return(x)
}

# JUDY revisit: what about excluding duplicate entries?
createVisitStats <- function(tableData, reportFormat){
  visitStats <- NULL
  visitStats$enrol <- tableData$tblBAS  %>% select(PATIENT, ENROL_D) %>% 
    filter(ENROL_D != dateIndicatingUnknown) %>%
    filter(!is.na(ENROL_D)) %>% 
    filter(ENROL_D <= Sys.Date()) %>% 
    mutate(Year1 = year(ENROL_D)) %>% 
    mutate(Year = earlyYears(Year1)) %>% 
    group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[1]])
  if ("tblVIS" %in% tablesAndVariables$tablesToCheck){
    visitStats$visits <- tableData$tblVIS %>% select(PATIENT, VIS_D) %>% 
      filter(VIS_D != dateIndicatingUnknown) %>%
      filter(!is.na(VIS_D)) %>% 
      distinct(PATIENT, VIS_D, .keep_all = TRUE) %>% 
      filter(VIS_D <= Sys.Date()) %>% 
      mutate(Year1 = year(VIS_D)) %>% 
      mutate(Year = earlyYears(Year1)) %>% 
      group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[2]])
  }
  if ("tblLTFU" %in% tablesAndVariables$tablesToCheck){
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
  if ("tblLAB_RNA" %in% tablesAndVariables$tablesToCheck){
    visitStats$viralLoad <- tableData$tblLAB_RNA %>% select(PATIENT, RNA_D) %>% 
      filter(RNA_D != dateIndicatingUnknown) %>%
      filter(!is.na(RNA_D)) %>% 
      filter(RNA_D <= Sys.Date()) %>% 
      mutate(Year1 = year(RNA_D)) %>% 
      mutate(Year = earlyYears(Year1)) %>% 
      group_by(Year) %>% dplyr::summarize(Count = n()) %>% mutate(Variable = visitStatsToReport[[5]])
  }
  if ("tblLAB_CD4" %in% tablesAndVariables$tablesToCheck){
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
  newRows <- tibble(Year = as.character(missingYears),
                        Count = rep(0, numberMissingYears),
                        Variable = rep("Enrolled", numberMissingYears))
  allData <- bind_rows(allData, newRows)
  visitStatsOut <- spread(allData, key = Year, Count, fill = 0)
  # arrange rows in desired order
  visitStatsOut$Variable <- factor(visitStatsOut$Variable, 
                                   levels = visitStatsToReport,
                                   ordered = TRUE)
  # for newer version of packages:
  # visitStatsOut <- visitStatsOut %>% arrange(Variable) %>% rowwise() %>%  mutate(Total = sum(c_across(-Variable))) 
   
   visitStatsOut <- visitStatsOut %>% 
     mutate(Total = rowSums(visitStatsOut %>% select(-Variable))) %>% 
     arrange(Variable)

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



hiddenDatesMessage <- function(dates, lowerDate, upperDate){
  numBefore <- sum( (dates < lowerDate) & 
                      (dates != dateIndicatingUnknown) & 
                      !is.na(dates) )
  hiddenBefore <- numBefore > 0 
  
  numAfter <- sum( (dates > upperDate) & 
                     !is.na(dates) )
  hiddenAfter <- numAfter > 0 
  
  
  
  if (hiddenAfter || hiddenBefore){
    observeBefore <- ifelse(numBefore == 1, "observation", "observations")
    observeAfter <- ifelse(numAfter == 1, "observation", "observations")
    if (hiddenAfter && hiddenBefore) connector <- " and "
    else connector <- NULL
    additionalMessage <- " (Note: "
    if (hiddenBefore) beforeMessage <- paste(numBefore, observeBefore, "before year", year(lowerDate))
    else beforeMessage <- NULL
    if (hiddenAfter) afterMessage <- paste(numAfter, observeAfter, "after year", year(upperDate))
    else afterMessage <- NULL
    verb <- ifelse(sum(numBefore + numAfter) < 2, " is", " are")
    additionalMessage <- paste0(additionalMessage,
                                beforeMessage,
                                connector,
                                afterMessage,
                                verb,
                                " hidden)")
    
  } else additionalMessage <-  NULL
  return(additionalMessage)
}

createPlotTitle <- function(groupName, detailedMessage){
  groupVar <- finalGroupChoice()
  title <- paste0(groupVar,": ", sanitizeNames(groupName))
  if (is.null(detailedMessage)) return(list (
    title = bquote(bold(.(title))),
    subtitle = NULL))
  if (nchar(title) + nchar(detailedMessage) < 90){
    return(list(
      title = bquote(bold(.(title))~~~~~scriptstyle(italic(.(detailedMessage)))),
      subtitle = NULL))
  }
  #widthOfAdditionalMessage <- 95 - nchar(title) #used in generating histogram grid
 # wrappedTitle <- wrap_sentence(paste0(title, detailedMessage), widthOfPlotTitle)
 # temp <- wrap_sentence(detailedMessage, widthOfAdditionalMessage)
  return(list(
    title = bquote(bold(.(title))),
    subtitle = bquote(~~~~~italic(.(detailedMessage)))
  ))
}

generateHistogramGrid <- function(tableData, groupName, dateFields){
  print("generateHistogramGrid")
  tablesToInclude <- intersect(desiredTables, names(tableData))
  dateDataset <- NULL
  for (tableName in tablesToInclude){
    print(tableName)
    table <- get(tableName, tableData)
    variablesToInclude <- intersect(desiredPlots, names(table))
    if (length(variablesToInclude) == 0) next
    for (dateVariable in variablesToInclude){
      print(dateVariable)
      datesInTable <- table %>% 
        gather(!!dateVariable, key = "DateName", value = "Dates") %>% 
        select("DateName", "Dates") 
      dateDataset <- bind_rows(dateDataset, datesInTable)
    }
  }
  # ensure that histograms will be in desired order, as specified in dateFields
  dateDataset$DateName = factor(dateDataset$DateName, levels=dateFields)
  if (!is.null(dateDataset) && any(!is.na(dateDataset$Dates))){
    dateRange <- "defaultRange"
    if (!is.null(input$histoRange)){
      dateRange <- input$histoRange
    }
    if (dateRange == "chooseRange"){
      minYear <- input$yearsToPlot[[1]]
      maxYear <- input$yearsToPlot[[2]]
    }
    else {
      minYear <- 2000
      maxYear <- as.numeric(format(Sys.Date(), "%Y"))
    }
    xmax <- as.Date(paste0(maxYear,"-12-31"))
    xmin <- as.Date(paste0(minYear, "-01-01"))
    
    message <- hiddenDatesMessage(dateDataset$Dates, xmin, xmax)
    plotTitle <- createPlotTitle(groupName, message)
    
    p <- ggplot(dateDataset, aes(Dates, label = Dates)) + 
      geom_histogram(boundary = xmin,
                     binwidth = 91.25,
                     colour = "#1F3552", 
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
        axis.text.x=element_text(angle=90, vjust = 0.5),
        plot.title = element_text(size=9),
        plot.subtitle = element_text(size = 8),
        #  axis.text=element_text(size=10),
        #  axis.title = element_text(size = 12),
        strip.text.y = element_text(size=7.5)
        #  strip.background = element_rect(fill="#FFFFFF")
      ) + 
      xlab("") +
      ylab("Number of observations") +
    #  labs(title = plotTitle)
      labs(title = plotTitle$title,
           subtitle = plotTitle$subtitle)
    gp <- p 
  }
  else gp <- NULL
  
  return(gp)
}


generateHistogramsAllPrograms <- reactive({
  groupVar <- finalGroupChoice()
  groupVarSE <- rlang::sym(groupVar)
  groups <- unique(na.omit(formattedTables()$tblBAS[[groupVar]]))
  groups <- groups[!is_blank_or_NA_elements(groups)]
  
  datesToInclude <- intersect(desiredPlots, unlist(tablesAndVariables$matchingColumns))
  tablesToInclude <- intersect(desiredTables, names(formattedTables()))
  
  plotList <- NULL
  for (groupName in groups){
    tableData <- lapply(formattedTables()[tablesToInclude], 
                        function(x, y, z){
                          if (!exists(z, x)) return(NULL)
                          rowsInGroup <- which(x[[z]] == y)
                          return(x[rowsInGroup,])
                          },
                        y=groupName, z = groupVar)
    plotList[[groupName]] <- generateHistogramGrid(tableData, groupName, datesToInclude)
    print(paste0("plotlist", groupName))
    
  }
  return(plotList)
})

generateQualitySummaryPlots <- function(programSummary, programName){
  tableTitle <- "Percent Complete Data (Not Missing or Unknown)"
  if (is.null(programSummary$missingAndUnknownSummary)) missingAndUnknownPlot <- NULL
  else {
    numMissingAndUnknown <-  nrow(programSummary$missingAndUnknownSummary)
    if (numMissingAndUnknown == 0) missingAndUnknownPlot <- NULL
    if (numMissingAndUnknown == 1){
      toShow <- programSummary$missingAndUnknownSummary
      toShow <- toShow %>% select(Table = table, Variable = variable, number, percent) %>% 
        mutate("Count (%)" = paste0(number, "(",percent,")")) %>% 
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
      # change percent to reflect percent NOT missing
      dataset$percent <- 100-dataset$percent
      missingAndUnknownPlot <- ggplot(data = dataset, aes(x = reorder(variable, -percent), y = percent)) + 
        geom_bar(stat = "identity", fill = "#0072B2") + #"lightgrey") +
        
        labs(title=tableTitle) +
        theme(legend.position="none") +
        xlab("Variable Name") +
        ylab("% Complete (Not Missing or Unknown)") +
        coord_flip(ylim = c(0,100))
    }
  } 
  if (is.null(programSummary$appearanceSummary)) appearancePlot <- NULL
  else {
    numAppearances <- nrow(programSummary$appearanceSummary)
    if (numAppearances == 0) appearancePlot <- NULL
    if (numAppearances == 1) appearancePlot <- NULL
    if (numAppearances > 1){
      plotTitle <- "% Patients from tblBAS Included in Other Tables"
      
      appearancePlot <- ggplot(data = programSummary$appearanceSummary, 
                               aes(x= reorder(table,-percent), y=percent)) + 
        geom_bar(stat = "identity", fill = "#0072B2") + 
        labs(title=plotTitle) +
        theme(legend.position="none") + 
        xlab("Table Name") +
        ylab("% patients from tblBAS included") +
        coord_flip(ylim = c(0, 100))
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
  errorSummaryShow <- errorSummaryShow %>% ungroup() %>% select(-Table, -display)
  #browser() # start here with replacing with <1 and also right justifying
  if (exists("Error", errorSummaryShow)) errorSummaryShow <- errorSummaryShow %>% rename(Description = "Error")
  if (exists("InvalidCode", errorSummaryShow)){
    # if any characters that Kable can't handle are in any invalid codes, sanitize them
    errorSummaryShow$InvalidCode <- sanitizeNames(errorSummaryShow$InvalidCode)
  }
  if (reportFormat =="pdf"){ reportFormat <- "latex"}
  if (reportFormat == "latex"){
    errorSumTable <- kable(errorSummaryShow, longtable=T, booktabs = T, format = reportFormat, 
                           caption  = caption) %>% 
      kableExtra::group_rows(index = groupRows) %>% 
      kable_styling(latex_options = c("repeat_header"))
  } else { # for html caption is blank; caption in headings in rmd so that navigation enabled
    errorSumTable <- kable(errorSummaryShow, longtable=T, format = reportFormat) %>% 
      kableExtra::group_rows(index = groupRows) %>% 
      kable_styling(latex_options = c("repeat_header"), 
                    bootstrap_options = c("striped","bordered"), full_width = F, position = "left") 
  }
  return(errorSumTable)
}




# generateDatasetSummary is passed tableData. tableData$tblBAS has already been filtered for program == programName
generateDatasetSummary <- function(tableData, reportFormat){ 

  tableTitle <- "Table Summary "
  
  totalPatients <- nrow(tableData$tblBAS)
  #compile list of valid PATIENT ids; only include those in tableSummary
  validPatients <- tableData$tblBAS$PATIENT
  # set up tableSummary data frame to include counts by age Group
  ageGroupLabels <- levels(tableData$tblBAS$ageGroup)
  colNames <- c("Table","Records","Patients", ageGroupLabels)
  tableSummary <- data.frame(matrix(vector(),ncol=length(colNames)))
  colnames(tableSummary) <-colNames
  
  row <- 1
  for (tableName in c("tblBAS", tablesAndVariables$tablesToCheckWithPatientID)){
    numrecords <- nrow(tableData[[tableName]])
    tableWithValidPatients <- tableData[[tableName]] %>% filter(PATIENT %in% validPatients)
    # first check to make sure this table has data
    if (numrecords == 0){
      results <- data.frame(
        Table = tableName,
        Records = 0,
        Patients = 0,
        stringsAsFactors = FALSE
      )
    
    }
    # check to see if the patients in this table are actually included in tblBAS (otherwise don't include)
    else if (nrow(tableWithValidPatients) == 0){ #(!any(validPatients %in% tableData[[tableName]][["PATIENT"]])){
      results <- data.frame(
        Table = tableName,
        Records = numrecords,
        Patients = 0,
        stringsAsFactors = FALSE
      )
    } else { 
    cat("in dataset summary loop", tableName, "\n", sep = "", file = stderr())
    results <- tableWithValidPatients %>% select(PATIENT, ageGroup) %>% 
      mutate(Table = tableName) %>% mutate(Records = numrecords) %>% 
      distinct(PATIENT,.keep_all = TRUE) %>% mutate(Patients = n()) %>%
      filter(!is.na(ageGroup)) %>% 
      group_by(Table, Records, Patients, ageGroup) %>% summarise(number = n()) %>% spread(ageGroup, number)
    }
    results <- as.data.frame(results, stringsAsFactors = FALSE)
 
    # in case no records matching:
    if (nrow(results) == 0){
      results <- data.frame(
        Table = tableName,
        Records = 0,
        stringsAsFactors = FALSE
      )
    } 
    tableSummary[row,names(results)] <- results
    row <- row + 1
  }
  tableSummary <- tableSummary %>% replace(is.na(.),0)
  
  for (tableName in uploadList()$tablesWithNoPatientID){ #JUDY fix this when we know how to handle preg tables
    results <- tableData[[tableName]] %>% mutate(Table = tableName) %>% group_by(Table) %>% summarise(Records = n())  
    results <- as.data.frame(results)
    # if this is a report for one program there may not be any records in this table matching that program
    if (nrow(results) == 0){
      results <- data.frame(
        Table = tableName,
        Records = 0,
        stringsAsFactors = FALSE
      )
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
      add_header_above(c(" " = 3, "Age at Enrollment" = length(ageGroupLabels)))
  }
  else {
    tableSummaryKable <- kable(tableSummary, format = "latex", longtable = T, booktabs = T, caption = tableTitle) %>% 
      add_header_above(c(" " = 3, "Age at Enrollment" = length(ageGroupLabels)))
                            #   caption = tableTitle, longtable = T, booktabs = T) %>% 
    #  add_header_above(c(" " = 3, "Age at Enrollment" = length(ageGroupLabels))) #%>% 
     # kable_styling(latex_options = c("repeat_header"))
  }
# browser() 
  statsSummaryTable <- createStatsSummary(tableData, reportFormat)
  visitStatsSummaryTable <- createVisitStats(tableData, reportFormat)
#  otherStatsTable <- createOtherStats(tableData, reportFormat)
  print("finished visitstats")
  
  return(list(
    totalPatients = totalPatients,
    tableSummaryKable = tableSummaryKable,
    statsSummaryTable = statsSummaryTable,
    visitStatsSummaryTable = visitStatsSummaryTable#,
    #otherStatsTable = otherStatsTable
  ))
}

fullDatasetSummary <- reactive({
  req(formattedTables())
  req(errorTable()[[1]])
  if (is.null(input$reportType)){
    # this means it's an autogenerated report to be stored; user didn't create report
    reportType <- "pdf"
  } else {
    reportType <- input$reportType
  }
 
  results <- generateDatasetSummary(formattedTables(), reportType)
  return(results)
})

summarizeForOneGroup <- function(groupName, tableData){

  groupVar <- finalGroupChoice()
  groupVarSE <- (rlang::sym(groupVar))
  if (is_empty(errorTable()$errorDetail)) programErrorSummaries <- NULL
  else {
    errorsInThisProgram <- errorTable()$errorDetail %>% filter((!!groupVarSE) == groupName)
    if (nrow(errorsInThisProgram) == 0) programErrorSummaries <- NULL
    else programErrorSummaries <- summarizeErrors(errorsInThisProgram, tableData)
  }
  
  if (nrow(errorTable()$missingSummaryByGroup) > 0){
    programMissing <- errorTable()$missingSummaryByGroup %>% 
      filter((!!groupVarSE) == groupName) %>% select(-(!!groupVarSE), -category)
  } else programMissing <- NULL
  
  if (nrow(errorTable()$unknownCodeSummaryByGroup) > 0){
    programUnknown <- errorTable()$unknownCodeSummaryByGroup %>% 
      filter((!!groupVarSE) == groupName) %>% select(-(!!groupVarSE), -category)
  } else programUnknown <- NULL
  
  if (nrow(errorTable()$missingAndUnknownByGroup) > 0){
    programMissingAndUnknown <- errorTable()$missingAndUnknownByGroup %>% filter((!!groupVarSE) == groupName) %>% select(-(!!groupVarSE))
  } else programMissingAndUnknown <- NULL
  if (!is.null(errorTable()$appearanceSummary)){
    programAppearance <- errorTable()$appearanceSummary %>% filter((!!groupVarSE) == groupName) %>% select(-(!!groupVarSE))
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
    programMessage <- tags$h4(paste0("Currently processing data from ", currentProgram))
  } else programMessage <- NULL
  reportModal(tableMessage = tableMessage, programMessage = programMessage)
}

resetHistoRange <- function(){
  # call this function each time a new dataset 
  if (is.null(input$histoRange)) return(NULL)
  if (input$histoRange == "defaultRange") return(NULL)
  # after reports are generated,
  updateSelectInput(
    session,
    "histoRange",
    "Choose years to include in histograms",
    choices = c("Years: 2000 - present" = "defaultRange",
                "Choose a custom range of years" = "chooseRange")
  )
}

# create a report -----------------------------------------------------
createReport <- function(file, reportType = c("PDF", "html"),
                         includeHistograms = input$includeHistograms,
                         includeDataSummary = input$includeDataSummary,
                         includeErrorSummary = input$includeErrorSummary,
                         datasetDesc = input$datasetDesc) {
  cat("createReport output file: ", file, "\n", sep = "", file = stderr())
  print("in createreport")
  if (includeHistograms){
    plotList <- generateHistogramsAllPrograms()
  } else plotList <- NULL
  cat("histograms done", "\n", sep = "", file = stderr())
  datasetSummary <- fullDatasetSummary()
  cat("dataset summary done", "\n", sep = "", file = stderr())
  
  errorSumTable <- generateErrorSummaryTables(errorTable()$errorOnlySummary, caption = "Summary of Errors", reportFormat = tolower(reportType))
  print("errorsum done")
  
  warnSumTable <- generateErrorSummaryTables(errorTable()$warnOnlySummary, caption = "Summary of Warnings", reportFormat = tolower(reportType))
  badCodeSumTable <- generateErrorSummaryTables(errorTable()$badCodeSummary, caption = "Summary of Invalid Codes", reportFormat = tolower(reportType))
  errorSummaryTables <- list("errors" = errorSumTable,
                             "warnings" = warnSumTable,
                             "badCodes" = badCodeSumTable)
  tableOfVariables <- tablesAndVariables$details$variableSummaryToDisplay
  # JUDY edit the line below if you want colored badges in the reports along with table names
  tableOfVariables$Table <- removeHTML(tableOfVariables$Table)
  tableOfVariables$`IeDEA DES Variables` <- removeHTML(tableOfVariables$`IeDEA DES Variables`)

  params <- list(
    allTables = formattedTables(),
    extraFiles = uploadList()$ExtraFiles,
    tableOfVariables = tableOfVariables,
    datasetSummary = datasetSummary,
    errorSummary = errorTable()$errorSummary,
    highLevel = errorTable()$highLevelErrorSummary,
    badCodes = errorTable()$badCodeSummary,
    missingSummary = errorTable()$missingSummary,
    missingSummaryByGroup = errorTable()$missingSummaryByGroup,
    unknownCodeSummaryByGroup = errorTable()$unknownCodeSummaryByGroup,
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
    errorSummaryTables = errorSummaryTables,
    groupVar = finalGroupChoice()
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



createReportOneProgram <- function(file, input, groupName){
  reportMessage(currentProgram = groupName)
  groupVar <- finalGroupChoice()
  groupVarSE <- (rlang::sym(groupVar))
  reportType <- input$reportType

  tableData <- lapply(formattedTables(), 
                      function(x, y, z){
                        if (!exists(z, x)) return(x)
                        rowsInGroup <- which(x[[z]] == y)
                        return(x[rowsInGroup,])
                      },
                      y=groupName, z = groupVar)
  if (input$includeHistograms){
    datesToInclude <- intersect(desiredPlots, unlist(tablesAndVariables$matchingColumns))
    p <- generateHistogramGrid(tableData, groupName, datesToInclude)
  }
  if (input$includeErrorSummary){
    programSummary <- summarizeForOneGroup(groupName, tableData)
    errorSumTable <- generateErrorSummaryTables(programSummary$errorSummaries$errorOnlySummary, caption = "Summary of Errors", reportFormat = tolower(reportType))
    warnSumTable <- generateErrorSummaryTables(programSummary$errorSummaries$warnOnlySummary, caption = "Summary of Warnings", reportFormat = tolower(reportType))
    badCodeSumTable <- generateErrorSummaryTables(programSummary$errorSummaries$badCodeFrame, caption = "Summary of Invalid Codes", reportFormat = tolower(reportType))
    errorSummaryTables <- list("errors" = errorSumTable,
                               "warnings" = warnSumTable,
                               "badCodes" = badCodeSumTable)
    
    
    
    datasetQuality <- generateQualitySummaryPlots(programSummary, groupName)
  } else {
    programSummary <- NULL
    errorSummaryTables <- NULL
    datasetQuality <- NULL
  }
  
  datasetSummary <- generateDatasetSummary(tableData, reportFormat = tolower(reportType))

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
    programsInReport = sanitizeNames(groupName),
    errorSummaryTables = errorSummaryTables,
    groupVar = finalGroupChoice()
  )
  tempReport <- file.path(tempdir(), filename)

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
  filename = function(){paste("reportGroup",sanitizeNames(input$programsInReport),userDetails()$user, Sys.Date(),".pdf",sep="_")}, #reportOneProgram.pdf",
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
  filename = function(){paste("reportGroup",sanitizeNames(input$programsInReport),userDetails()$user, Sys.Date(),".html",sep="_")}, #reportOneProgram.pdf",
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
  filename = function(){paste0("reportGroup",sanitizeNames(input$programsInReport),userDetails()$user, Sys.Date(),".zip")}, 
  content = function(file) {
    reportMessage()
    groupVar <- finalGroupChoice()
    groupNames <- unique(na.omit(formattedTables()$tblBAS[[groupVar]]))
    groupNames <- groupNames[!is_blank_or_NA_elements(groupNames)]
    groupNames <- groupNames[groupNames != "Missing"]
    for (groupName in groupNames){
      print(paste0("creating report: ", groupName))
      createReportOneProgram(paste0(sanitizeNames(groupName), '.', input$reportType), input, groupName)
    }
    reportFilenames <- paste0(sanitizeNames(groupNames), '.',input$reportType)
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


# create a report -----------------------------------------------------
createMetricsReport <- function(file) {
  cat("createMetricsReport output file: ", file, "\n", sep = "", file = stderr())
  print("in createMetricsreport")
  source("dqMetrics.R", local = TRUE)
  requestedTables <- names(concept()$tablefields)
  dqHeatmaps <- createdqHeatmaps()
  if (length(names(dqHeatmaps$heatmapList)) == 1){
    tocFlag <- "no"
  } else {
    tocFlag <- "yes"
  }
  
  params <- list(
    requestedTables = requestedTables,
    region = userDetails()$regionName,
    userDetails = userDetails(),
    groupVar = finalGroupChoice(),
    dqHeatmaps = dqHeatmaps$heatmapList,
    codedDqHeatmaps = dqHeatmaps$codedHeatmap,
    tocFlag = tocFlag,
    reportDesc = input$reportDesc
  )
  tempReportName <-  paste0("reportdqmetrics.rmd")
  dir <- dirname(file)
  cat("createDQReport output directory: ", dir, "\n", sep = "", file = stderr())
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



output$DQMetrics <- downloadHandler(
  filename = "dqMetrics.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    reportMessage()
    
    createMetricsReport(file)
    removeModal()
  }
)

# createAggregationFile <- function(data){
#   source("aggregation.R", local = TRUE)
#   results <- createAggregations(data)
# }
# 
# output$aggregations <- downloadHandler(
#   filename = function() {
#     paste(input$regionchoice, 
#           "_mainAggregation_", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#   
#   reportMessage()
#   data <- formattedTables()
#   aggrCSV <- createAggregationFile(data)
# 
#   write.csv(aggrCSV, file, row.names = FALSE)
#   removeModal()
# }
#)


