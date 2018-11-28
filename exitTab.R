
output$exitUI <- renderUI({
  if (is.null(uploadList())) return(NULL)
  if (!is.null(uploadList())) return(fluidPage(actionButton("initialExitButton","Exit Data Toolkit")))
})

observeEvent(
  {input$initialExitButton
    },{
  initialExitFunctions()
})

observeEvent(
  {input$exitaftersubmit
  },{
    initialExitFunctions()
  })

initialExitFunctions <- function(){
  reloadGuardOn(FALSE)
  if (hubInfo$fromHub){
    hub_data_url <- paste0(plugin_url, "?token=",
                           userDetails()$uploadhub_token,# "&option=dat",  JUDY revisit this
                           "&pid=58325")
    exitButton <- tags$a("Yes, Exit Data Toolkit", href = hub_data_url, class = "btn btn-default")
  } else exitButton <- actionButton("exitNoRequest", "Yes, Exit Data Toolkit")
  showModal(modalDialog(
    title = div(tags$b("Exit the Harmonist Data Toolkit"), style = "color: #605ea6;"),
    fluidRow(
      box(
        width = 12,
        tags$p("Are you sure? All shared files will be deleted from memory")
      )
    ),
    # fluidRow(column(3,offset = 1,uiOutput("downloadErrorDetail")))
    footer = tagList(
      actionButton("continue", "Continue using Data Toolkit"),
      exitButton
    )
  ))
}



exitButtonClicked <- reactiveVal(FALSE)



exitActions <- function(){
  resetFileInput$reset <- TRUE
  lapply(isolate(reactiveValuesToList(trackDetailsForREDCap)), postProgressMultiple)
  postProgress(list(action_step = "errordetail", viewdetail_count = isolate(errorDetailViewCount())))
  postProgress(list(action_step = "logout"))
  if (hubInfo$fromHub){
    # delete record from Harmonist 18
   # browser()
   # deleteOneRecord(tokenForHarmonist18, userDetails()$decryptedToken)
    hubInfo$fromHub <- FALSE
    session$reload()
  }
  else {
    session$reload()
    
    gc()
    # session$close()
  }
}

observeEvent(input$exitNoRequest,{
  removeModal()
  exitButtonClicked(TRUE)
  
  exitActions()
})

observeEvent(input$exitButton,{
  removeModal()
  exitButtonClicked(TRUE)
  
  exitActions()
})

observeEvent(input$idleContinue,{
  lastActivity(Sys.time())
  reloadGuardOn(TRUE)
  removeModal()
})

observeEvent(input$idleExit, {
  removeModal()
  exitActions()
})

observeEvent(input$continue,{
  reloadGuardOn(TRUE)
  removeModal()
})

session$onSessionEnded(function(){
  if (isolate(exitButtonClicked())) return(NULL)
  if (is.null(isolate(infile()))){
    session$reload()  #or not if fromHub
    return(NULL)
  }
  lapply(isolate(reactiveValuesToList(trackDetailsForREDCap)), postProgressMultiple)
  postProgress(list(action_step = "errordetail", viewdetail_count = isolate(errorDetailViewCount())))
  postProgress(list(action_step = "session_end"))
  gc()
  resetFileInput$reset <- TRUE
  exitButtonClicked(FALSE)
  #session$close()
})