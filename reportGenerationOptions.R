
# determine if user prefers one program in report or all---------------------------------------------------------  
output$programsToInclude <- renderUI({
  if (is.null(uploadedTables())) return(NULL)
  tblBAS <- formattedTables()$tblBAS
  programNames <- as.character(sort(unique(tblBAS$PROGRAM)))
  names(programNames) <- programNames
  
  if (length(programNames)==1){
    availChoices = programNames
  }
  else availChoices =  c(
    "All Programs" = "all",
    "Generate individual reports for each Program, zipped into one output file" = "allZip",
    programNames
  ) 
  selectInput("programsInReport",
              "Which Program(s) would you like to include in this report?",
              choices =  availChoices,
              selected = availChoices[[1]],
              multiple = FALSE
  )
})

createSelectProgramUI <- function(selectInputName){
  programNames <- as.character(sort(unique(errorTable()$errorDetail$PROGRAM)))
  names(programNames) <- programNames
  allPrograms <- unique(na.omit(uploadedTables()$tblBAS$PROGRAM))
  if (length(allPrograms) > length(programNames)){
    errorFreePrograms <- setdiff(allPrograms, programNames)
    note <- tags$h5("Note that the following program(s) in the dataset had no errors: ",
                    paste(errorFreePrograms, collapse = ", "))
  } else {note <-  NULL}
  
  if (length(programNames) == 1) {
    chooseProgram <- selectInput("programsInErrorCSV",
                                 paste0("Errors from Program ", programNames, " will be included in this report"),
                                 choices =  programNames,
                                 selected = programNames
    )
  } else {
    programChoices <- c(
      "All Programs" = "all",
      "Download individual error reports for each program, zipped into one file" = "allZip",
      programNames
    )
    chooseProgram <- selectInput(selectInputName,
                                 "Which Program(s) would you like to include in this report?",
                                 choices =  programChoices,
                                 selected = "all",
                                 multiple = FALSE
    )
  }
  
  return(
    tagList(
      chooseProgram,
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

output$programsToIncludeErrorHTML <-renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(uploadedTables())) return(NULL)
  if (nrow(errorTable()$errorDetail) == 0){
    return(h4("No errors to download"))
  }
  selectProgramUI <- createSelectProgramUI("programsInErrorHTML")
})

output$histOptions<- renderUI({
  if (resetFileInput$reset) return(NULL)
  if (is.null(formattedTables())) return(NULL)
  if (input$includeHistograms == FALSE) return(NULL)
  if (is.null(input$fullOrRange)){
    fullOrRange <- "fullRange"
  } else {fullOrRange <- input$fullOrRange}
  if (is.null(input$factorForHistogram)){
    factorForHistogram <- "None"
  }
  
  options <- tagList(
    selectInput("fullOrRange","Choose years to include in histograms",
                choices = c("The full range of years of data" = "fullRange",
                            "Choose a custom range of years" = "chooseRange"),
                selected = fullOrRange)
    
  )
  if (is.null(input$fullOrRange)){
    return(
      box("Date Histogram Options",status = "primary", width = 12, options))
  }
  if (input$fullOrRange=="chooseRange"){
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
  if (input$fullOrRange == "fullRange"){
    return(
      box("Date Histogram Options", status = "primary", width = 12, options))
  }
})


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
  if (input$reportType == "pdf"){
    if (input$programsInReport == "all"){
      return(downloadButton("reportpdf", "Generate summary pdf report"))
    }
    else if (input$programsInReport == "allZip"){
      return(downloadButton("reportZipAll", "Generate ZIP file of individual summary pdf reports"))
      
    }
    else {
      program <- input$programsInReport
      return(downloadButton("reportOneProgramPDF", 
                            paste0("Generate summary pdf report of PROGRAM ", program)))
    }
  }
  if (input$reportType == "html"){
    
    if (input$programsInReport == "all"){
      return(downloadButton("reporthtml", "Generate summary html report"))
    }
    else if (input$programsInReport == "allZip"){
      return(downloadButton("reportZipAll", "Generate ZIP file of individual summary html reports"))
    }
    else {
      program <- input$programsInReport
      return(downloadButton("reportOneProgramHTML", 
                            paste0("Generate summary HTML report of PROGRAM ", program)))
    }
  }
})

output$downloadErrorHTML <- renderUI({
  if (is.null(errorTable())) return(NULL)
  if (nrow(errorTable()$errorDetail) == 0) return(NULL)
  if (nrow(errorTable()$errorDetail) > maxErrorsForHTML) return(NULL)
  downloadButton('reportErrorsInHTML', 'Download HTML report of error detail')
})




