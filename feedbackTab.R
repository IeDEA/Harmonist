# feedback Tab ----------------------------------------------------------------
output$feedbackTabUI <- renderUI({
  fluidPage(
    tags$h3(class = "row_text title",
            "Provide feedback", backToHubMessage()),
    tags$h5(class = "row_text subtitle",
            "Use the space below to send anonymous feedback to the Harmonist team."),
    box(width = 8,
     
        textAreaInput("emailBody","Enter message:"),
        textInput("userEmail", "(Optional) If you would like a response, enter your email address:"),
        actionButton("emailJudy", "Send message to Harmonist Developer"))
  )
})

observeEvent(input$emailJudy,{
  lastActivity(Sys.time())
  emailMessage <- isolate(input$emailBody)
  userEmail <- isolate(input$userEmail)
  if (emailMessage!=""){
    postFeedback(emailMessage, userEmail)
    #sendEmail(emailMessage)
    updateTextAreaInput(session,"emailBody",
                        label =  "Message sent. To provide additional feedback, enter new message below:",
                        value = "")
    updateTextInput(session, "userEmail", value = "")
  }
  
})