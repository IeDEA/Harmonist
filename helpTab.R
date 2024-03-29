output$helpTabUI <- renderUI({
  fluidPage(
    tags$h3(class = "row_text title",
            "Help", backToHubMessage()),
    tags$p(class = "faqHeader", "Helpful resources for Harmonist Data Toolkit users:",
        tags$ul(class = "faq",
           tags$li(tags$a("Toolkit Quick Reference Guide for IeDEA Data Managers", 
                          href = "toolkitOnePageRef.pdf", target = "_blank")),
           tags$li(tags$a("Harmonist Data Quality Checks Overview", 
                          href = "dataQualityChecks.pdf", target = "_blank")), 
           tags$li(tags$a("Harmonist Error Spreadsheet Guide ", 
                          href = "errorSpreadsheetGuide.pdf", target="_blank")), 
           tags$li(tags$a("Harmonist Data Toolkit Demonstration Video ", 
                          href = "https://youtu.be/pL_RRhvzX-w", target="_blank")),
           tags$li("Answers to frequently asked questions (below)")
           )
          # " to better understand the columns of the error detail spreadsheet available in Step 2."),
           ),
    fluidRow(
      htmlOutput("toolkitFAQ", seamless = "seamless")
    )
    
  )
  
})

output$toolkitFAQ <- renderUI({
  tags$iframe(src = faq_url,
              height="900", width=875, style = "border:0")
})
