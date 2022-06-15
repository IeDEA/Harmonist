# changelog Tab ----------------------------------------------------------------

fields <- c("version_num", "release_d", "major_features", "changes")
changelogInfo <- redcap_read_oneshot(redcap_uri = redcap_url,
                                token = tokenForHarmonistChangelog,
                                raw_or_label_headers = 'label',
                                fields = fields)$data

changelogInfo <- cbind(changelogInfo[1], format(changelogInfo[2], "%Y-%m-%d"), changelogInfo[3], changelogInfo[4])
changelogInfo <- changelogInfo %>% arrange(-`Version Number`) %>% 
  datatable(rownames = FALSE, escape = FALSE) %>% formatRound(1, 1)


output$changelogTabUI <- renderUI({
  fluidPage(
    tags$style(HTML(".shiny-html-output { padding-left: 12px;}")),
    tags$h3(class = "row_text title",
            "Change Log", backToHubMessage()),
    tags$h5(class = "row_text subtitle",
            "This page describes the release versions of the IeDEA Harmonist Toolkit and documents key changes and additions to the platform"),

    output$changelogTabTable <- renderDataTable({
      changelogInfo
    }
   ) 
  )
})

observeEvent(input$changelogLink,{
  updateTabItems(session,"tabs", "changelog")
})


