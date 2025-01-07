# changelog Tab ----------------------------------------------------------------

output$changelogTabUI <- renderUI({
  # if REDCap was not responding when app loaded, link to changelog is ""
  # so just return null
  print(versionLinkText)
  if (versionLinkText == "") return(NULL)
  
  # otherwise, we know that REDCap was responding earlier, attempt to constuct
  changelogInfo <- getAllRecords(tokenForHarmonistChangelog, 'HarmonistChangelog', 
                                 fields = 'version_num, release_d, major_features, 
                                 changes')
  # if REDCap didn't respond this time...
  if (inherits(changelogInfo, "postFailure")){
    changelogpage <- fluidPage(
      tags$style(HTML(".shiny-html-output { padding-left: 12px;}")),
      tags$h3(class = "row_text title",
              "Changelog", backToHubMessage()),
      tags$h5(class = "row_text subtitle",
              "Sorry, we are having trouble loading this page. Please try again later.")
    )
  } else {
    changelogInfo <- rbindlist(changelogInfo, fill=TRUE)
    colnames(changelogInfo) <- c('Version Number', 'Release Date', 'Major Feature Additions', 'Changes') # change colnames to label
    # next line removes info about num of lines but preserves search box
    changelogInfo <- changelogInfo %>% arrange(`Version Number`) %>% 
      datatable(rownames = FALSE, escape = FALSE, options = list(dom = 'ft')) %>% formatRound(1, 1)
    changelogpage <- fluidPage(
      tags$style(HTML(".shiny-html-output { padding-left: 12px;}")),
      tags$h3(class = "row_text title",
              "Changelog", backToHubMessage()),
      tags$h5(class = "row_text subtitle",
              "This page describes the release versions of the Harmonist Data Toolkit and documents key changes and additions to the platform"),
      
      renderDataTable({
        changelogInfo
      })
    )
  }    
  
  return(changelogpage)
  
})

observeEvent(input$changelogLink,{
  updateTabItems(session,"tabs", "changelog")
})
