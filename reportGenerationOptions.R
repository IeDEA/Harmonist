finalGroupLevels <- reactive({
  if (is.null(uploadedTables())) return(NULL)
  groupVar <- finalGroupChoice()
  
  groupNames <- as.character(sort(tableRowsByGroup()[[indexTableName]][[groupVar]]))
  groupNames <- groupNames[groupNames != "Missing"]
  names(groupNames) <- groupNames
  return(groupNames)
})

# determine if user prefers one program in report or all---------------------------------------------------------  
output$programsToInclude <- renderUI({
  if (is.null(uploadedTables())) return(NULL)
  groupVar <- finalGroupChoice()
  groupNames <- finalGroupLevels()
  # if no valid groups, don't offer individual group reports
  if (length(groupNames) == 0){
    availChoices <- "all"
    names(availChoices) <- "All"
  } else if (length(groupNames) == 1){
    # if only one group, don't offer to zip individual groups
    availChoices <- groupNames
  } else {
    availChoices <- list(
      "all",
      "allZip",
      groupNames#,
     # "multigroup"
    )
    names(availChoices) <- c(
      "All" ,
      paste0("Download individual reports for each ", groupVar, " group zipped into one file"),
      paste0("Individual report (choose one ", groupVar, " group)" )#,
    #  paste0("Multiple ", groupVar, " groups in a single report")
    )
  }
    
  selectInput("programsInReport",
              paste0("Data subgroup(s) for report"),
              choices =  availChoices,
              selected = availChoices[[1]],
              multiple = FALSE
  )
})

# output$chooseMultipleGroups <- renderUI({
#   if (is.null(uploadedTables())) return(NULL)
#   if (is.null(input$programsInReport)) return(NULL)
#   if (input$programsInReport != "multigroup") return(NULL)
#   if (is.null(input$multGroupsInReport)){
#     label <- "Choose groups"
#   } else {
#     label <- "Edit groups"
#   }
#   actionButton("chooseMultiGroups", label)
#   
# })

createSelectProgramUI <- function(selectInputName){
  groupVar <- finalGroupChoice()
  groupNames <- as.character(sort(unique(errorTable()$errorDetail[[groupVar]])))
  # for now, remove "All" from program names in choose program UI since these are general errors
  # that should only be included in All Programs report **May want to change this later** JUDY
  groupNames <- groupNames[groupNames != "All"]
  names(groupNames) <- groupNames
  allGroups <- unique(na.omit(uploadedTables()[[indexTableName]][[groupVar]]))
  allGroups <- allGroups[!is_blank_or_NA_elements(allGroups)]
  if (length(allGroups) > length(groupNames)){
    errorFreeGroups <- setdiff(allGroups, groupNames)
    note <- tags$h5("Note that the following",
                    makeItPluralOrNot("group", length(errorFreeGroups)),
                    "in the dataset had no errors: ",
                    paste(errorFreeGroups, collapse = ", "))
  } else {note <-  NULL}
  
  if (length(groupNames) == 1) {
    chooseGroup <- selectInput(selectInputName,
                                 tags$p("Errors from", groupVar, "group", tags$b(groupNames), "will be included in this report"),
                                 choices =  groupNames,
                                 selected = groupNames
    )
  } else {
    programChoices <- list(
      "all",
      "allZip",
      groupNames
    )
    names(programChoices) <- c(
      "All" ,
      paste0("Download individual error reports for each ", groupVar, " group zipped into one file"),
      paste0("Individual report (choose one ", groupVar, " group)" ) 
    )
  
    chooseGroup <- selectInput(selectInputName,
                                 paste0("Which ", groupVar, " would you like to include in this report?"),
                                 choices =  programChoices,
                                 selected = "all",
                                 multiple = FALSE
    )
  }
  
  return(
    tagList(
      chooseGroup,
      note
    )
  )
}


output$programsToIncludeErrorCSV <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(uploadedTables())) return(NULL)
  if (nrow(errorTable()$errorDetail) == 0){
    return(h4("No errors to download"))
  }
  selectProgramUI <- createSelectProgramUI("programsInErrorCSV")
})

output$histOptions<- renderUI({
  
  if (input$includeHistograms == FALSE) return(NULL)

  if (is.null(input$histoRange)){
    selection <- "defaultRange"
  } else {
    selection <- input$histoRange
  }
  
  options <- tagList(
    selectInput("histoRange","Choose years to include in histograms",
                choices = c("Years: 2000 - present" = "defaultRange",
                            "Choose a custom range of years" = "chooseRange"),
                selected = selection)
  )
  if (is.null(input$histoRange)){
    return(
      box("Date Histogram Options",status = "primary", width = 12, options))
  }
  if (input$histoRange=="chooseRange"){
    moreOptions <- options
    moreOptions[["chooseYears"]] <- sliderInput("yearsToPlot",
                                                "Date range to include in histograms:",
                                                min = 1985, max = as.numeric(format(Sys.Date(), "%Y")),
                                                sep = "",
                                                value = c(2000,max = as.numeric(format(Sys.Date(), "%Y"))),
                                                step = 1)
    return(
      box("Date Histogram Options", status = "primary", width = 12, moreOptions))
  }
  if (input$histoRange == "defaultRange"){
    return(
      box("Date Histogram Options", status = "primary", width = 12, options))
  }
})

# observeEvent(input$multContinue,{
#   if (is.null(input$multGroupsInReport))
#     browser()
#   removeModal()
# })


# observeEvent(input$chooseMultiGroups,{
#   if (input$programsInReport != "multigroup") return(NULL)
#   groupNames <- finalGroupLevels()
#   showModal(modalDialog(
#     easyClose = FALSE,
#     title = paste0("Choose ", finalGroupChoice(), " groups for report"),
#     
#     wellPanel(
#       checkboxGroupInput("multGroupsInReport",
#                          "Choose the groups you would like to include in this report",
#                          groupNames)#,
#       #  multiple = TRUE)
#     ),
#     footer = tagList(
#       #  actionButton("Cancel", label = "Continue working"),
#       actionButton("multContinue", label = "Continue")
#     ),
#     fade = FALSE))
# })



# Display download buttons for reports once data quality checks are complete-----
output$downloadReport <- renderUI({
  if (is.null(infile())){
    return(NULL)
  }
  if (is.null(errorTable()[[1]])){
    return(NULL)
  }
  if (is.null(input$programsInReport)){
    return(NULL)
  }
  # if (input$programsInReport == "multigroup" && is.null(input$multGroupsInReport)){
  #   return(NULL)
  # }

  if (input$reportType == "pdf"){
    if (input$programsInReport == "all"){ #%in% c("all", "multigroup")){
      return(downloadButton("reportpdf", "Generate summary PDF report"))
    }
    else if (input$programsInReport == "allZip"){
      return(downloadButton("reportZipAll", "Generate ZIP file of individual summary PDF reports"))
      
    }
    else {
      groupVar <- finalGroupChoice()
      group <- input$programsInReport
      return(downloadButton("reportOneProgramPDF", 
                            paste0("Generate summary PDF report for ", groupVar, ": ", group)))
    }
  }
  if (input$reportType == "html"){
    
    if (input$programsInReport == "all"){ # %in% c("all", "multigroup")){
      return(downloadButton("reporthtml", "Generate summary HTML report"))
    }
    else if (input$programsInReport == "allZip"){
      return(downloadButton("reportZipAll", "Generate ZIP file of individual summary HTML reports"))
    }
    else {
      groupVar <- finalGroupChoice()
      group <- input$programsInReport
      return(downloadButton("reportOneProgramHTML", 
                            paste0("Generate summary HTML report for ", groupVar, ": ", group)))
    }
  }
})




