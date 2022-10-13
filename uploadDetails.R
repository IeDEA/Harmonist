# createListsOfTablesAndVariables ------------------------------------------------------------
createListsOfTablesAndVariablesNoHub <- function(tableList, tableDef){
  tableNameList <- set_names(names(tableList), names(tableList))
  numberOfRecords <- sapply(tableList, nrow)
  # create variable to count deprecated variables
  deprecated_count <- 0
  
  variableNameListByTable <- lapply(tableNameList, function(x){names(tableDef[[x]]$variables)})
  variableDefs <- lapply(tableNameList, function(x){tableDef[[x]]$variables})

  #create a list of des variables by table, highlight deprecated variables with red text
  #
  des_variables_list <- list()
  deprecated_list <- list()
  
  for (tableName in tableNameList){
    # First, make list of des variables that were found in uploaded dataset

    des_variables <- intersect(names(variableDefs[[tableName]]), names(tableList[[tableName]]))
    
    # Get list of deprecated variables in table using variable_status in tableDef:
    # 0, DRAFT (variable under consideration)
    # 1, Active
    # 2, DEPRECATED (variable retired from use)
    deprecatedVariables <- sapply(variableDefs[[tableName]], function(var){
      value <- var[["variable_status"]]
      !is.null(value) && value == "2"
    })
    
    deprecatedVariables <- intersect(des_variables, names(which(deprecatedVariables)))
    
    if (!is_empty(deprecatedVariables)){
      # keep track of total number of deprecated variables
      deprecated_count <- deprecated_count + length(deprecatedVariables)
      deprecated_list[[tableName]] <- deprecatedVariables
      active_des_variables <- des_variables[!des_variables %in% deprecatedVariables]
      des_variables <- paste0(paste(active_des_variables, collapse = ", "), ", ",
                              paste0('<span style="color:#dd4b39">', deprecatedVariables,'</span>', 
                                     collapse = ", ")
      )
    } else des_variables <- paste(des_variables, collapse = ", ")
    des_variables_list[[tableName]] <- des_variables
  }
  
  #create a list of variables in each table that are not found in the des for that table
  non_des_variables <- mapply(function(x, y) {
    variablesInTable <- names(x)
    non_des <- variablesInTable[which(!variablesInTable %in% y)]
    return(non_des)
  }, x =tableList, y =variableNameListByTable, SIMPLIFY = FALSE)
  
  # *IF user is responding to data request,
  # create a list of variables that were shared but not requested


  
  notRequestedCount <- 0
  
  #create summary table. Only include NonDES column if those variables exist
  if (is_empty(unlist(non_des_variables))){
    non_des_count <- 0
    toDisplay <- tibble("Table" = tableBadge(names(tableList)), 
                        "Records" = numberOfRecords,
                        "networkName datamodel_abbrev Variables" = unlist(des_variables_list))
  } else {
    all_non_des <- unique(unlist(non_des_variables))
    non_des_count <- length(all_non_des[!all_non_des %in% approvedExtraVariables])
    non_des_to_display <- lapply(non_des_variables, 
                                 function(x){
                                   if (length(x) < maxExtraVar){ # in definitions
                                     paste0(x, collapse = ", ")
                                   } else {
                                     paste0(length(x), " additional data columns")
                                   }
                                 }
    )
    
    toDisplay <- tibble("Table" = tableBadge(names(tableList)), 
                        "Records" = numberOfRecords,
                        "networkName datamodel_abbrev Variables" = unlist(des_variables_list),
                        "Non-datamodel_abbrev Variables" = unlist(non_des_to_display))
  }
  
  names(toDisplay) <- gsub("networkName", networkName,
                           gsub("datamodel_abbrev", projectDef$datamodel_abbrev,
                                names(toDisplay)))
  
  # return details for uploadTab upload summary
  return(
    list(
      non_des_count = non_des_count,
      main_des_variables = removeHTML(des_variables_list),
      extra_des_variables = NULL,
      non_des_variables = non_des_variables,
      variableSummaryToDisplay = toDisplay,
      deprecated_count = deprecated_count,
      deprecated_list = deprecated_list
    )
  ) 
}

###################################################################################
# createListsOfTablesAndVariablesHub ------------------------------------------------------------
# 
# if the user is responding to a data request, highlight non-requested variables
##################################################################################

createListsOfTablesAndVariablesHub <- function(tableList, tableDef){
  # we already know user is responding to data request.
  tableNameList <- set_names(names(tableList), names(tableList))
  numberOfRecords <- sapply(tableList, nrow)
  # create variable to count deprecated variables
  deprecated_count <- 0
  
  datarequest <- userDetails()$uploadconcept_mr
  requestedData <- concept()$tablefields
  requestedTables <- names(requestedData)
  
  variableNameListByTable <- lapply(tableNameList, function(x){names(tableDef[[x]]$variables)})
  variableDefs <- lapply(tableNameList, function(x){tableDef[[x]]$variables})

  #create a list of requested des variables by table, highlight deprecated variables with red text
  #
  main_des_variables_list <- list()
  deprecated_list <- list()

  not_requested <- list()
  notrequestedDES <- list()

  for (tableName in tableNameList){
    # First, make list of des variables that were found in uploaded dataset

    des_variables <- intersect(names(variableDefs[[tableName]]), names(tableList[[tableName]]))

    # Only if responding to data request:
    # if this table wasn't requested, all of its variables are in the not requested category
    # if the table was requested, group variables into requested and not requested categories
    if (!tableName %in% requestedTables){
      main_des_variables <- NULL
      notrequestedDES <- des_variables
    } else {
      requestedColumns <- requestedData[[tableName]]
      main_des_variables <- intersect(des_variables, requestedColumns)
      notrequestedDES <- des_variables[!des_variables %in% requestedColumns]
    }
    
    # Get list of deprecated variables in table using variable_status in tableDef:
    # 0, DRAFT (variable under consideration)
    # 1, Active
    # 2, DEPRECATED (variable retired from use)
    deprecatedVariables <- sapply(variableDefs[[tableName]], function(var){
      value <- var[["variable_status"]]
      !is.null(value) && value == "2"
    })
    
    deprecatedVariables <- intersect(des_variables, names(which(deprecatedVariables)))

    if (!is_empty(deprecatedVariables)){
      # keep track of total number of deprecated variables
      deprecated_count <- deprecated_count + length(deprecatedVariables)
      deprecated_list[[tableName]] <- deprecatedVariables
      
      deprecatedRequested <- main_des_variables[main_des_variables %in% deprecatedVariables]
  
      active_requested_des_variables <- main_des_variables[!main_des_variables %in% deprecatedVariables]
      
      if (is_empty(deprecatedRequested)){
        main_des_variables <- paste0(paste(active_requested_des_variables, collapse = ", "))
      } else {
        main_des_variables <- paste0(paste(active_requested_des_variables, collapse = ", "), ", ",
                                     paste0('<span style="color:#dd4b39">', deprecatedRequested,'</span>', 
                                            collapse = ", "))
      }
      
      # if the number of deprecated requested variables is less than the total number of deprecated
      # then some non requested variables must be deprecated
      if (length(deprecatedRequested) < length(deprecatedVariables)){
        active_not_requested <- notrequestedDES[!notrequestedDES %in% deprecatedVariables]
        deprecatedNotRequested <- notrequestedDES[notrequestedDES %in% deprecatedVariables]
      } else {
        active_not_requested <- notrequestedDES
        deprecatedNotRequested <- NULL
      }
      notrequestedDES <- paste0(paste(active_not_requested, collapse = ", "), ", ",
                                paste0('<span style="color:#dd4b39">', deprecatedNotRequested,'</span>', 
                                       collapse = ", "))
      
    } else {
      main_des_variables <- paste(main_des_variables, collapse = ", ")
      notrequestedDES <- paste(notrequestedDES, collapse = ", ")
    }
    main_des_variables_list[[tableName]] <- main_des_variables
    not_requested[[tableName]] <- notrequestedDES
  }
  
  #create a list of variables in each table that are not found in the des for that table
  non_des_variables <- mapply(function(x, y) {
    variablesInTable <- names(x)
    non_des <- variablesInTable[which(!variablesInTable %in% y)]
    return(non_des)
  }, x =tableList, y =variableNameListByTable, SIMPLIFY = FALSE)
  

  nonDES <- !is_empty(unlist(non_des_variables))
  nonRequested <- !is_empty(unlist(not_requested))
  
  #create first 3 columns of summary table. 

  toDisplay <- tibble(
    "tableName" = names(tableList),
    "Table" = tableBadge(names(tableList)), 
    "Records" = numberOfRecords,
    "networkName datamodel_abbrev Requested Variables" = unlist(main_des_variables_list))
  
  if (nonRequested){
    toDisplay <- left_join(toDisplay,
                           tibble("tableName" = names(not_requested),
                                  "Variables Not Requested by mrnum" = unlist(not_requested)
                           ),
                           by = "tableName")
  }
  
  if (nonDES){
    non_des_to_display <- lapply(non_des_variables, 
                                 function(x){
                                   if (length(x) < maxExtraVar){ # in definitions
                                     paste0(x, collapse = ", ")
                                   } else {
                                     paste0(length(x), " additional data columns")
                                   }
                                 }
    )
    toDisplay <- left_join(toDisplay,
                           tibble("tableName" = names(non_des_to_display),
                                  "Non-datamodel_abbrev Variables" = unlist(non_des_to_display)
                           ),
                           by = "tableName")
  }
  
    all_non_des <- unique(unlist(non_des_variables))
    non_des_count <- length(all_non_des[!all_non_des %in% approvedExtraVariables])
    
    toDisplay <- toDisplay %>% select(-tableName)

    
    # toDisplay <- tibble("Table" = tableBadge(names(tableList)), 
    #                     "Records" = numberOfRecords,
    #                     "Requested networkName datamodel_abbrev Variables" = unlist(des_variables_list),
    #                     "Variables not requested by" = unlist(nonrequested_list),
    #                     "Non-networkName datamodel_abbrev Variables" = unlist(non_des_to_display))
  
  
    names(toDisplay) <- gsub("networkName", networkName,
                             gsub("datamodel_abbrev", projectDef$datamodel_abbrev,
                                  gsub("mrnum", datarequest, names(toDisplay))))  

  # return details for uploadTab upload summary
  return(
    list(
      non_des_count = non_des_count,
      main_des_variables = removeHTML(main_des_variables_list),
      extra_des_variables = removeHTML(not_requested),
      non_des_variables = non_des_variables,
      variableSummaryToDisplay = toDisplay,
      deprecated_count = deprecated_count,
      deprecated_list = deprecated_list
    )
  ) 
}
