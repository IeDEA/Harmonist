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
  filename = "errorDetailAllGroups.zip",
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
    groupVar <- finalGroupChoice()
    groupNames <- unique(na.omit(errors[[groupVar]]))
    tableNames <- as.character(unique(errors$table))
    parent_dir <- tempfile("error")
    dir.create(parent_dir)
    if (input$separateTables) {
      for (groupName in groupNames) {
        cleanGroupName <- sanitizeNames(groupName)
        csv_filenames <- c()
        # JUDY possibly sanitize program names
        print(paste0("groupname=",groupName))
        group_dir <- file.path(parent_dir, cleanGroupName)
        print(paste0("about to create new dir, grou_dir=",group_dir))
        dir.create(group_dir)
        for (tableName in tableNames) {
          errorsToWrite <- errors %>% filter( (!! rlang::sym(groupVar)) == groupName & table == tableName)
          if (nrow(errorsToWrite) == 0){
            errorsToWrite <- data.frame("Message" = "No errors found")
          } else {
            emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
            errorsToWrite <- errorsToWrite[!emptyCols]
          }
          csv_filename <- paste0(cleanGroupName, '_', tableName, '_errorDetail.csv')
          csv_filenames <- c(csv_filenames, csv_filename)
          csv_path <- file.path(group_dir, csv_filename)
          write_csv(errorsToWrite, csv_path)
        }
      }
      old_wd <- getwd()
      setwd(parent_dir)
      zip::zip(file, sanitizeNames(groupNames))
      setwd(old_wd)
    } else {
      csv_filenames <- c()
      for (groupName in groupNames) {
        errorsToWrite <- errors %>% filter( (!! rlang::sym(groupVar)) == groupName)
        if (nrow(errorsToWrite) == 0){
          errorsToWrite <- data.frame("Message" = "No errors found")
        } else {
          emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
          errorsToWrite <- errorsToWrite[!emptyCols]
        }
        cleanGroupName <- sanitizeNames(groupName)
        
        csv_filename <- paste0(cleanGroupName, '_errorDetail.csv')
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
    groupVar <- finalGroupChoice()
    groupName <- input$programsInErrorCSV
    trackDetailsForREDCap$reports <- rbind(trackDetailsForREDCap$reports,
                                           data.frame(
                                             action_ts = as.character(Sys.time()),
                                             action_step = "downloadreport",
                                             report_type = paste0("errorDetail",sanitizeNames(groupName)),
                                             report_format = "csv",
                                             stringsAsFactors = FALSE
                                           ))
    
    
    if (input$separateTables) {
      tableNames <- unique(errors$table)
      csv_filenames <- c()
      dir <- tempfile("error")
      dir.create(dir)
      for (tableName in tableNames) {
        errorsToWrite <- errors %>% filter( (!! rlang::sym(groupVar)) == groupName & table == tableName)
        if (nrow(errorsToWrite) == 0){
          errorsToWrite <- data.frame("Message" = "No errors found")
        } else {
          emptyCols <- sapply(errorsToWrite, function(x) all(x=="", na.rm = TRUE))
          errorsToWrite <- errorsToWrite[!emptyCols]
        }
        csv_filename <- paste0(sanitizeNames(groupName), '_', tableName, '_errorDetail.csv')
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
      errorsToWrite <- errors %>% filter( (!! rlang::sym(groupVar)) == groupName)
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