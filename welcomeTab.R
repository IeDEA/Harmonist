output$welcomeUI <- renderUI({
  fluidPage(
    tags$h3(class = "row_text title",tags$strong(span("Introduction ", class = "text-grey")),
            " Toolkit Overview", backToHubMessage()),
    fluidRow(
      class = "rowNoActiveDataRequest",
      box(
        solidHeader = TRUE,
    #    title = "IeDEA Harmonist Data Toolkit",
        width = 12,
        welcomeText
      )
    ),
    fluidRow(
      class = "rowNoActiveDataRequest",
      box(
        solidHeader = TRUE,
        title = "Data Toolkit Steps",
        width = 12,
        stepsText
      )
    ),
    actionButton("fromWelcomeToStep1", "Continue to Step 1: Upload Tables", class="container-left btn-success")
  )
})