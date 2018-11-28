output$reportErrors <- downloadHandler(
  filename = "errorDetail.zip",
  content = function(file) {
    reportMessage()
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = "errorDetail",
                                             report_format = "csv",
                                             stringsAsFactors = FALSE
                                           ))
    errors <- errorTable()$errorDetail

    if (input$separateTables) {
      tableNames <- unique(errors$table)
      csv_filenames <- c()
      dir <- tempfile("error")
      dir.create(dir)
      for (tableName in tableNames) {
        errorsToWrite <- errors %>% filter(table == tableName)
        if (nrow(errorsToWrite) == 0){
          errorsToWrite <- data.frame("Message" = "No errors found")
        } else {
          emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
          errorsToWrite <- errorsToWrite[!emptyCols]
        }
        csv_filename <- paste0('all_', tableName, '_errorDetail.csv')
        csv_filenames <- c(csv_filenames, csv_filename)
        csv_path <- file.path(dir, csv_filename)
        write_csv(errorsToWrite, csv_path)
      }
      old_wd <- getwd()
      setwd(dir)
      zip::zip(file, csv_filenames)
      setwd(old_wd)
      unlink(dir, recursive = TRUE)
      
      
    } else {
      errorsToWrite <- errors
      if (nrow(errorsToWrite) == 0){
        errorsToWrite <- data.frame("Message" = "No errors found")
      } else {
        emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
        errorsToWrite <- errorsToWrite[!emptyCols]
      }
      dir <- tempdir()
      csv_filename <- paste0('all_errorDetail.csv')
      csv_path <- file.path(dir, csv_filename)
      write_csv(errorsToWrite, csv_path)
      old_wd <- getwd()
      setwd(dir)
      zip::zip(file, csv_filename)
      setwd(old_wd)
      unlink(csv_path)
    }
    
    removeModal()
  }
)

output$reportZipAllErrors <-  downloadHandler(
  filename = "errorDetailAllPrograms.zip",
  content = function(file) {
    reportMessage()
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = "errorDetailEachProgram",
                                             report_format = "csv",
                                             stringsAsFactors = FALSE
                                           ))
    errors <- errorTable()$errorDetail
    programNames <- unique(na.omit(errors$PROGRAM))
    tableNames <- as.character(unique(errors$table))
    parent_dir <- tempfile("error")
    dir.create(parent_dir)
    if (input$separateTables) {
      for (programName in programNames) {
        cleanProgramName <- sanitizeNames(programName)
        csv_filenames <- c()
        # JUDY possibly sanitize program names
        program_dir <- file.path(parent_dir, cleanProgramName)
        dir.create(program_dir)
        for (tableName in tableNames) {
          errorsToWrite <- errors %>% filter(PROGRAM == programName & table == tableName)
          if (nrow(errorsToWrite) == 0){
            errorsToWrite <- data.frame("Message" = "No errors found")
          } else {
            emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
            errorsToWrite <- errorsToWrite[!emptyCols]
          }
          csv_filename <- paste0(cleanProgramName, '_', tableName, '_errorDetail.csv')
          csv_filenames <- c(csv_filenames, csv_filename)
          csv_path <- file.path(program_dir, csv_filename)
          write_csv(errorsToWrite, csv_path)
        }
      }
      old_wd <- getwd()
      setwd(parent_dir)
      zip::zip(file, sanitizeNames(programNames))
      setwd(old_wd)
    } else {
      csv_filenames <- c()
      for (programName in programNames) {
        errorsToWrite <- errors %>% filter(PROGRAM == programName)
        if (nrow(errorsToWrite) == 0){
          errorsToWrite <- data.frame("Message" = "No errors found")
        } else {
          emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
          errorsToWrite <- errorsToWrite[!emptyCols]
        }
        cleanProgramName <- sanitizeNames(programName)
        
        csv_filename <- paste0(cleanProgramName, '_errorDetail.csv')
        csv_filenames <- c(csv_filenames, csv_filename)
        csv_path <- file.path(parent_dir, csv_filename)
        write_csv(errorsToWrite, csv_path)
        
      }
      old_wd <- getwd()
      setwd(parent_dir)
      zip::zip(file, csv_filenames)
      setwd(old_wd)
    }
    unlink(parent_dir, recursive = TRUE)
    
    removeModal()
  }
)


output$reportOneProgramErrors <- downloadHandler(
  filename = function() {paste0(sanitizeNames(input$programsInErrorCSV), "_errorDetail.zip") },
  content = function(file) {
    reportMessage()
    errors <- errorTable()$errorDetail
    programName <- input$programsInErrorCSV
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = paste0("errorDetail",sanitizeNames(programName)),
                                             report_format = "csv",
                                             stringsAsFactors = FALSE
                                           ))
    
    
    if (input$separateTables) {
      tableNames <- unique(errors$table)
      csv_filenames <- c()
      dir <- tempfile("error")
      dir.create(dir)
      for (tableName in tableNames) {
        errorsToWrite <- errors %>% filter(PROGRAM == programName & table == tableName)
        if (nrow(errorsToWrite) == 0){
          errorsToWrite <- data.frame("Message" = "No errors found")
        } else {
          emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
          errorsToWrite <- errorsToWrite[!emptyCols]
        }
        csv_filename <- paste0(sanitizeNames(programName), '_', tableName, '_errorDetail.csv')
        csv_filenames <- c(csv_filenames, csv_filename)
        csv_path <- file.path(dir, csv_filename)
        write_csv(errorsToWrite, csv_path)
      }
      
      old_wd <- getwd()
      setwd(dir)
      zip::zip(file, csv_filenames)
      setwd(old_wd)
      unlink(dir, recursive = TRUE)
    } else {
      errorsToWrite <- errors %>% filter(PROGRAM == programName)
      if (nrow(errorsToWrite) == 0){
        errorsToWrite <- data.frame("Message" = "No errors found")
      } else {
        emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
        errorsToWrite <- errorsToWrite[!emptyCols]
      }
      dir <- tempdir()
      csv_filename <- paste0(sanitizeNames(input$programsInErrorCSV), '_errorDetail.csv')
      csv_path <- file.path(dir, csv_filename)
      write_csv(errorsToWrite, csv_path)
      old_wd <- getwd()
      setwd(dir)
      zip::zip(file, csv_filename)
      setwd(old_wd)
      unlink(csv_path)
    }
    
    removeModal()
  }
)