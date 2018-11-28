updateModal <- function(message, n=0, lastMessages=NULL){
  lastActivity(Sys.time())
  gc()
  if (!is.null(lastMessages)){
    checkList <- lapply(lastMessages,function(x){return(div(icon("check"),span(x, style = "color: green; font-size: large")))})
  }
  else checkList <- NULL
  
  if (n!=0){
    currentMessage <- paste0(message, " (Quality check #",n, " of 16)")
  } else currentMessage <- message
  print(paste0("Check: ", message, " ",Sys.time()))
  showModal(modalDialog(easyClose = FALSE,
                        title = "Progress", size = 'l',
                        wellPanel(
                          div(tagList(checkList)),
                          #div(icon("check"),
                          #    span(lastMessages, style = "color: green; font-size: x-large")),
                          # div(tags$img(src = "ajax-loader.gif"),
                          #     span(paste0(message," Quality check ",n, " of 15"), style = "color: blue; font-size: x-large")
                          div(    icon("spinner", class = "fa-spin"),
                              span(currentMessage, style = "color: blue; font-size: large")
                          )
                        ),
                        footer = NULL, 
                        fade = FALSE))
  
  return(list(num = n+1, lastMessages = c(lastMessages, message)))
}

reportModal <- function(titleText = "Report generation", 
                        message = "The requested report is being generated...",
                        tableMessage = NULL,
                        programMessage = NULL){
  showModal(modalDialog(easyClose = TRUE, #JUDY change back to false
                        title = titleText, size = 'l',
                        div(    icon("spinner", class = "fa-spin"),
                            span(message, style = "color: blue; font-size: x-large")
                        ),
                        tableMessage,
                        programMessage,
                        footer = NULL, 
                        fade = FALSE))
}


errorMessageModal <- function(messageHeader = "",message = "", textForREDCap = "", messageTable = NULL,
                              secondaryMessage = "Please upload corrected dataset"){
  lastActivity(Sys.time())

  if (textForREDCap == ""){
    errormsg <- messageHeader
  } else errormsg <- textForREDCap
  postProgress(list(action_step = "errormsg", errormsg = errormsg))
  showModal(modalDialog(easyClose = FALSE,
                        title = span(icon("danger"),"Error"),
                        wellPanel(
                          tags$h3(messageHeader),
                          tags$p(message),
                          tags$br(),
                          renderTable({
                            messageTable
                          },
                          rownames = TRUE
                          )
                        ),
                        tags$h4(secondaryMessage),
                        footer = actionButton("close_error_modal", label = "Close", class = "color_btn"), 
                        fade = FALSE))
}

uploadMissingRequested <- function(missingList, uploadedList){
  messageHeader <-  "Requested Tables Not Detected"
  message <-  tagList(
    tags$p("The following tables requested by Data Request", userDetails()$uploadconcept_mr,
           "are missing:"),
    listOfMissingTables,
    tags$br(),
    tags$p("You uploaded the following tables:"),
    listOfTables
  )
  showModal(modalDialog(easyClose = FALSE,
                        title = span(icon("danger"),"Error"),
                        wellPanel(
                          tags$h3(messageHeader),
                          tags$p(message)
                        ),
                        footer = 
                          tagList(
                            actionButton("uploadContinue", 
                                         label = "Log out of Hub and continue with this dataset"),
                            actionButton("uploadStop", 
                                         label = paste0("Return to active data request ", mr, 
                                                        " and upload all requested tables and variables"))
                            ), 
                        fade = FALSE))
}

observeEvent("uploadContinue", {
  
})

observeEvent("uploadStop",{
  
})

observeEvent(input$close_error_modal, {
  removeModal()
})

fileReadError <- function(extension, fileName = inputLink$name[index], errorMessage){
  errorMessageModal(
    messageHeader = "Error Reading File",
    message = tagList(
      tags$p("Failure when attempting to read", strong(fileName),". Please confirm that", strong(fileName), "is a valid", extension, "file")
    )
  )
}

idleWarningModal <- function(idleTime){
  
  showModal(modalDialog(easyClose = FALSE,
                        title = paste0("Application idle ", idleTime, " minutes"),
                        wellPanel(
                          tags$h3("Would you like to continue using the IeDEA Harmonist Data Toolkit?")
                        ),
                        footer = tagList(
                          actionButton("idleContinue", label = "Continue working"),
                          actionButton("idleExit", label = "Delete any uploaded data and exit")
                        ),
                        fade = FALSE))
}

idleExitModal <- function(idleTime){
  
  showModal(modalDialog(easyClose = TRUE,
                        title = paste0("Application idle ", idleTime, " minutes"),
                        wellPanel(
                          tags$h3("For the protection of your data, this session is ending and any uploaded data will be deleted")
                        ),
                        footer = NULL,
                        fade = FALSE))
}



informNoConcept <- function(mr){
  showModal(modalDialog(easyClose = FALSE,
                        title = paste0("Test data toolkit with sample data?"),
                        tags$p(paste0(
                          "You are currently logged in through the IeDEA Hub to respond to Data Request ", mr,
                          ". If you choose to test the data toolkit with sample data you will be logged out from the hub and will need to start a new session to upload your data for Data Request ", mr,".")),
                        footer = tagList(
                          actionButton("testContinue", label = "Log out of Hub and test with sample data"),
                          actionButton("testStop", label = paste0("Return to active data request ", mr))
                        ),
                        fade = FALSE))
}
