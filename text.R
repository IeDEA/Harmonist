step1Subtitle <- "In this step, choose the files containing your IeDEA tables to check for data quality. After files are uploaded, review the table summarizing uploaded files and variables"

step2Subtitle <- "View interactive summary of errors and download detailed results of data quality checks to review offline."

step3Subtitle <- "Download reports summarizing uploaded dataset."

welcomeText <- tagList(
  tags$p("This toolkit is designed to facilitate data quality checking, reporting, and sharing of HIV/AIDS observational datasets that conform to the ", a("IeDEA Data Exchange Standard (IeDEA DES)", 
                    href="http://iedeades.org", target="_blank"), " for approved research concepts in the IeDEA network. The toolkit may be accessed in two ways:"),
  tags$ul(
    tags$li(tags$b("Visiting ",a("IeDEA Harmonist Data Toolkit",
                                href="http://iedeadata.org/iedea-harmonist", target="_blank"),
            " directly.")," Available options: Upload IeDEA tables, conduct and review data quality checks, download reports, and view plots of variable in shared tables. When the Toolkit session ends (either by closing the browser or selecting Exit from the menu) the uploaded tables are deleted."),
    tags$li(tags$b("Logging in to the ",
            a(" IeDEA Hub", href="http://iedeahub.org", target="_blank"),
             " and choosing an active data request (concept).")," An additional option is available in this case: If the data quality checks are successful, the dataset can be submitted to secure cloud storage for  retrieval by the author of the selected concept. All files will be deleted after retrieval or 30 days. ")))

stepsText <- tagList(
  tags$p("Download this ",
         tags$a("Sample dataset ", href = "sampleTables.zip"), 
         " to test the Toolkit and learn about its functions. ",
         "View the ", 
                tags$a("IeDEA Harmonist Data Toolkit Video ", href = "https://youtu.be/pL_RRhvzX-w", target="_blank"), " for a demonstration."),
  tags$ul(
    tags$li(
      tags$b("Step 1: Upload tables"), " Browse and select your files containing the IeDEA tables you would like to check for data quality"
    ),
    tags$li(
      tags$b("Step 2: Review data checks")," Use the interactive summary of possible inconsistencies found in the dataset and download spreadsheets to review details offline"
    ),
    tags$li(
      tags$b("Step 3: Create summary")," Choose the desired report format and report contents and generate reports summarizing the dataset you uploaded"
    ),
    tags$li(
      tags$b("Step 4: Submit data"), " This option is available only if you entered the toolkit by selecting an active data request the IeDEA Hub. You may explain remaining data quality errors or warnings and click to share your dataset with the investigator who created the data request."
    ),
    tags$li(
      tags$b("Visualize data"), " Choose tables and variables to plot to aid in better understanding your dataset"
    )
  )
)
          