output$helpTabUI <- renderUI({
  fluidPage(
    tags$h3(class = "row_text title",
            "Help", backToHubMessage()),
    tags$h5(class = "row_text subtitle",
            ""),
      tags$p("Download the ",
             tags$a("IeDEA Harmonist Error Spreadsheet Guide ", href = "errorSpreadsheetGuide.pdf", target="_blank"), " to better understand the columns of the error detail spreadsheet available in Step 2."),
      tags$p("View the ", 
             tags$a("IeDEA Harmonist Data Toolkit Video ", href = "https://youtu.be/pL_RRhvzX-w", target="_blank"), " for a demonstration.")
  )
  
})