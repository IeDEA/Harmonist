
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


cleanupFiles <- function(object) {
  if (is.null(object)) return()

  if (!is.null(object$datapath)) {
    # object is input$loaded
    unlink(object$datapath)
  } else if (!is.null(object$files)) {
    # object is infile
    unlink(object$files$datapath)
    if (!is.null(object$zipfile)) {
      unlink(object$zipfile)
    }
  }
  usage[[isolate(session$token)]] <<- NULL
}

exitActions <- function(){
  reloadGuardOn(FALSE)
  cat("Session:", sessionID(),"inside exitActions", 
      as.character(now()), "\n", sep = " ", file = stderr())
  #resetFileInput$reset <- TRUE

  lapply(isolate(reactiveValuesToList(trackDetailsForREDCap)), postProgressMultiple)
  #postProgress(list(action_step = "errordetail", viewdetail_count = isolate(errorDetailViewCount())))
  cat("Session:", sessionID(),"inside exitActions, about to add logout step to redcap", 
      as.character(now()), "\n", sep = " ", file = stderr())
  sessionDuration <- round(difftime(isolate(lastActivity()), isolate(sessionStartTime()), units = "mins"), digits = 1)
  postProgress(list(action_step = "logout", session_mins = as.character(sessionDuration)))
  cat("Session:", sessionID(),"inside exitActions, finished adding logout step to redcap", 
      as.character(now()), "\n", sep = " ", file = stderr())
  cat("Session:", sessionID(),"about to clean up files", 
      as.character(now()), "\n", sep = " ", file = stderr())
  cleanupFiles(isolate(infile()))
  cat("Session:", sessionID(),"about to reload session from exitActions", 
      as.character(now()), "\n", sep = " ", file = stderr())

  if (hubInfo$fromHub){
    hubInfo$fromHub <- FALSE
  }
  cat("reloadGuard:", isolate(reloadGuardOn()), "\n")
  session$reload()
  session$close()
  sessionOver(TRUE)
}

observeEvent(input$exitNoRequest,{
  removeModal()
  exitButtonClicked(TRUE)
  cat("Session:", sessionID(),"Initiating exitActions because exitnorequest button clicked", 
      as.character(now()), "\n", sep = " ", file = stderr())
  reloadGuardOn(FALSE)
  exitActions()
})

observeEvent(input$exitButton,{
  reloadGuardOn(FALSE)
  removeModal()
  exitButtonClicked(TRUE)
  cat("Session:", sessionID(),"Initiating exitActions because exitbutton clicked", 
      now(), "\n", sep = " ", file = stderr())
  exitActions()
})

observeEvent(input$idleContinue,{
  lastActivity(Sys.time())
  reloadGuardOn(TRUE)
  removeModal()
})

observeEvent(input$idleExit, {
  reloadGuardOn(FALSE)
  removeModal()
  cat("Session:", sessionID(),"Initiating exitActions because exitfromidle button clicked", 
      as.character(now()), "\n", sep = " ", file = stderr())
  exitActions()
})

observeEvent(input$continue,{
  reloadGuardOn(TRUE)
  removeModal()
})

session$onSessionEnded(function(){
  if (isolate(exitButtonClicked())) {
    cat("Session:", isolate(sessionID()),"inside onSessionEnded although exitButtonClicked. sessionOver:", 
        isolate(sessionOver()), "\n", sep = " ", file = stderr())
    cat("Session:", isolate(sessionID()),"inside onSessionEnded reloading session", 
        as.character(now()), "\n", sep = " ", file = stderr())
    session$reload()
    #session$close()
    return(NULL)
  }
  if (isolate(sessionOver())){
    cat("Session:", isolate(sessionID()),"inside onSessionEnded after idle too long although exitActions already completed. sessionOver:", 
        isolate(sessionOver()), "\n", sep = " ", file = stderr())
    cat("Session:", isolate(sessionID()),"inside onSessionEnded reloading session", 
        as.character(now()), "\n", sep = " ", file = stderr())
    session$reload()
    #session$close()
    return(NULL)
  }
  cat("Session:", isolate(sessionID()),"Initiating onSessionEnded", 
      as.character(now()), "\n", sep = " ", file = stderr())
  cat("Session:", isolate(sessionID()),"In onSessionEnded about to post details to REDCap", 
      as.character(now()), "\n", sep = " ", file = stderr())
  
  lapply(isolate(reactiveValuesToList(trackDetailsForREDCap)), postProgressMultiple)
  if (isolate(errorDetailViewCount()) > 0){
    postProgress(list(action_step = "errordetail", viewdetail_count = isolate(errorDetailViewCount())))
  }
  
  sessionDuration <- round(difftime(isolate(lastActivity()), isolate(sessionStartTime()), units = "mins"), digits = 1)
  postProgress(list(action_step = "session_end", session_mins = as.character(sessionDuration)))

  cleanupFiles(isolate(infile()))
  cat("Session:", isolate(sessionID()),"Still in onSessionEnded after cleaning files", 
      as.character(now()), "\n", sep = " ", file = stderr())

  if (is.null(isolate(infile()))){
    cat("Session:", isolate(sessionID()),"Infile() is null, about to reload session", 
        as.character(now()), "\n", sep = " ", file = stderr())
    session$reload()  #or not if fromHub
    return(NULL)
  }

  gc()
  resetFileInput$reset <- TRUE
  exitButtonClicked(FALSE)
  cat("Session:", isolate(sessionID()),"At end of onSessionEnded", 
      as.character(now()), "\n", sep = " ", file = stderr())
 # session$reload()
  #session$close()
})
