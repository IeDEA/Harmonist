# use mymodal instead of modalDialog when it's important for modal to be wide enough for table
mymodal <- function (..., title = NULL, footer = modalButton("Dismiss"), 
                     size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE, idcss = "") 
{
  size <- match.arg(size)
  cls <- if (fade) 
    "modal fade"
  else "modal"
  div(id = "shiny-modal", class = cls, tabindex = "-1", `data-backdrop` = if (!easyClose) 
    "static", `data-keyboard` = if (!easyClose) 
      "false", div(class = paste("modal-dialog", idcss), class = switch(size, 
                                                                        s = "modal-sm", 
                                                                        m = NULL, 
                                                                        l = "modal-lg"), 
                   div(class = "modal-content", 
                       if (!is.null(title)) 
                         div(class = "modal-header", tags$h4(class = "modal-title", 
                                                             title)
                         ), 
                       div(class = "modal-body", ...), 
                       if (!is.null(footer)) 
                         div(class = "modal-footer", footer))
      ), 
    tags$script("$('#shiny-modal').modal().focus();"))
}


updateModal <- function(message, n=0, lastMessages=NULL,
                        title = "Data Quality Checks",
                        subtitle = "The toolkit is checking your dataset.",
                        currentTableName = NULL){
  # the service function will give Shiny a place to tell a new user that the
  # application is busy (appBusy is TRUE during checks)
  if (appBusy){
    # httpuv::service(1)
  }
  
  lastActivity(Sys.time())
  cat("Session:", sessionID(),"updateModal:",  unlist(message), "time =",
      as.character(now()), "\n", sep = " ", file = stderr())
  gc()
  if (!is.null(lastMessages)){
    checkList <- lapply(lastMessages,
                        function(x){return(div(icon("check"),span(x, style = "font-size: large")))})
  }
  else checkList <- NULL
  
  if (!is.null(currentTableName)) tableMessage <- tags$p("    Checking table", currentTableName)
  else tableMessage <- NULL
  
  if (n!=0){
    additionalMessage <- tags$em("(Quality check #",n, " of 17)")
  } else additionalMessage <- NULL
  print(paste0("Check: ", message, " ",Sys.time()))
  showModal(tags$div(id="dataQualityChecks",modalDialog(
    easyClose = FALSE,
    title = title, size = 'l',
    tags$em(subtitle),
    wellPanel(
      div(tagList(checkList)),
      
      div(    icon("spinner", class = "fa-spin"),
              span(message, additionalMessage, class = "text-blue", style = "font-size: large")
      ),
      tableMessage
    ),
    footer = NULL, 
    fade = FALSE)))
  return(list(num = n+1, lastMessages = c(lastMessages, message)))
}

reportModal <- function(titleText = "Report generation", 
                        message = "The requested report is being generated...",
                        tableMessage = NULL,
                        programMessage = NULL){
  showModal(tags$div(id="dataQualityChecks",
                     modalDialog(
                       easyClose = FALSE,
                       title = titleText, size = 'm',
                       div(    icon("spinner", class = "fa-spin"),
                               span(message, class = "text-blue")
                       ),
                       tableMessage,
                       programMessage,
                     #  footer = NULL, 
                       fade = FALSE))
  )
}


errorMessageModal <- function(messageHeader = "",message = "", textForREDCap = "", messageTable = NULL,
                              secondaryMessage = "Please upload corrected dataset"){
  lastActivity(Sys.time())
  cat("Session:", isolate(sessionID())," error Message: ",
      messageHeader, unlist(message), textForREDCap,
      sep = " ",
      "\n", file = stderr())  
  
  if (textForREDCap == ""){
    errormsg <- messageHeader
  } else errormsg <- textForREDCap
  postProgress(list(action_step = "errormsg", errormsg = errormsg))
  showModal(
    tags$div(id="errorModal",(
      modalDialog(
        easyClose = FALSE,
        title = span(icon("exclamation-triangle"),"Error"),
        wellPanel(
          tags$h3(messageHeader),
          tags$p(message),
          tags$br(),
          renderTable({
            messageTable
          },
          rownames = FALSE,
          sanitize.text.function = function(x) x #this allows escape html in renderTable
          )
        ),
        tags$h4(secondaryMessage),
        footer = actionButton("close_error_modal", label = "Close", class = "color_btn"), 
        fade = FALSE))))
}

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
                          tags$h3("Would you like to continue using the Harmonist Data Toolkit?")
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
                          "You are currently logged in through the ",
                          networkName,
                          " Hub to respond to Data Request ", mr,
                          ". If you choose to test the data toolkit with sample data you will be logged out from the hub and will need to start a new session to upload your data for Data Request ", mr,".")),
                        footer = tagList(
                          actionButton("testContinue", label = "Log out of Hub and test with sample data"),
                          actionButton("testStop", label = paste0("Return to active data request ", mr))
                        ),
                        fade = FALSE))
}



