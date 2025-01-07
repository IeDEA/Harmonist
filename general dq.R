# rule: if var 1 is not blank/missing, then var 2 should not be blank/missing.
# Attribute error to var 1 or var 2?
message <- paste0("If redcap_repeat_instrument is non-missing then ",
                  "redcap_repeat_instance should not be blank.")

checkAgreement <- function(errorFrame, var1name, var2name, tableName, resources, 
                           attributeindex = 1, missingLabel = "Missing"){
  if (!tableName %in% names(resources$formattedTables)) return(errorFrame)
  if (!var1name %in% names(resouces$formattedTables[[tableName]])) return(errorFrame)
  if (!var2name %in% names(resouces$formattedTables[[tableName]])) return(errorFrame)
  
  # now we know the table and variables are present
  
  # this message will accompany each error:
  
  message <- paste0("If ",
                    var1name,
                    " has a value (is not blank/missing), then ",
                    var2name,
                    " should not be blank.")


    thisTable <- resources$formattedTables[[tableName]]
    
    # Find records with variable 1 complete
    # case 1: var1 is a coded variable
    if (tableDef[[tableName]]$variables[[var1name]]$has_codes == "Y"){
      badTable <- thisTable %>% 
        filter(!! rlang::sym(var1name) != missingLabel)
    } else if (mode(thisTable[[var1name]] == "character")){
      # case 2: var1 is character but non-coded
      badTable <- thisTable %>% 
        filter(!!rlang::sym(var1name) != "")
    } else {
      # case 1: var1 is numeric but non-coded
      badTable <- thisTable %>% 
        filter(!is.na(!!rlang::sym(var1name)))
    }
    # Now, among those records, which have missing variable 2?
    # case 1: var2 is a coded variable
    if (tableDef[[tableName]]$variables[[var2name]]$has_codes == "Y"){
      badTable <- badTable %>% 
        filter(!! rlang::sym(var2name) == missingLabel)
    } else if (mode(thisTable[[var2name]] == "character")){
      # case 2: var2 is character but non-coded
      badTable <- badTable %>% 
        filter(!!rlang::sym(var2name) == "")
    } else {
      # case 1: var2 is numeric but non-coded
      badTable <- badTable %>% 
        filter(is.na(!!rlang::sym(var2name)))
    }
    
    if (nrow(badTable) == 0) return(errorFrame)
    
    errorFrame <- addToErrorFrame(resources$formattedTables[[indexTableName]],
                                  resources$finalGroupChoice, 
                                  errorFrame, 
                                  badTable, 
                                  var1name, 
                                  tableName, "Variable agreement",
                                  errorCode = "2.3j", "Error", message, 
                                  error_field2 = var2name, 
                                  error2 = as.character(badTable[[var2name]]))
    
   return(errorFrame) 
  }