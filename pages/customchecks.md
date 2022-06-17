# Guidance for adding custom data quality checks to the Harmonist Data Toolkit

### General function format for R function checkName
```
checkName <- function(resources){  
   …  
  # the final line should return an error data frame, if any errors  
  return(newErrors)  
}
```

### Description of `resources` argument  

`resources` is a list (of lists) which includes:

* `resources$uploadedTables` - a list of data frames containing the uploaded tables that match the data model. In each data frame (table), each row is an observation, and each column is a variable in the data model for that table. The values in the columns are unaltered from the original data. To access data in tblBAS, for example, use resources$uploadedTables$tblBAS 

* `resources$formattedTables` - a list of data frames containing the uploaded tables and variables matched to respective formats, codes, etc. Differs from uploadedTables in the following ways:
    - Date values that don’t match the required format are replaced with NA
    - Coded fields are replaced with the label (e.g., Yes instead of 1)
    - Coded fields with values that are not valid codes are labeled “Invalid”
    - Coded fields with missing values are labeled “Missing”
    - Numeric fields that contain non-numeric data are replaced with NA

* `resources$uploadList` - a list of character vectors with details of which tables were shared that match the data request (if the Toolkit is linked to data requests in a Harmonist Hub), the data model, which tables have a patient identifier in each record, etc.   

### Description of `newErrors` data frame that must be returned from a data quality check function

Each record in the `newErrors` data frame describes an error detected in the dataset. This [document](pages/errorSpreadsheetGuide.pdf) describes the columns in the error data frame.

First, within the data quality check function, create a data frame with one record per error. The columns are described in the document linked above. Here is an example. The heading of the first column will be the patient grouping variable defined for the research network (in Harmonist0C). 
* The errorCodes for custom data quality checks should begin with the number 5 or higher  (5.1, 5.12, 7.5, etc)
* Severity options are “Warning”, “Error”, and “Critical”.
* Note that the function should return NULL or an empty tibble() if no errors of that type are found.


### How to call new data quality checks in the Toolkit

To add a function called `newCheck`, for example, edit customChecks.R by adding a call to your new function and assigning the returned error detail to `newErrors`:
  
  ```	
	newErrors <- newCheck(resources)
  ```
	
Then add the `newErrors` data frame to the list of data frames of errors (`errorFrame*):

For example, if the errors detailed in `newErrors` are related to an Invalid Code for MODE in tblBAS, we can create a unique value to use to add newError to the errorFrame list:
```
index <- paste0(“tblBAS”,”MODE”,”Invalid code”) 
errorFrame[[index]] <- newErrors
```
