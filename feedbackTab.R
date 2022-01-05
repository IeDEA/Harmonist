# feedback Tab ----------------------------------------------------------------
output$feedbackTabUI <- renderUI({
  fluidPage(
    tags$h3(class = "row_text title",
            "Provide feedback", backToHubMessage()),
    tags$h5(class = "row_text subtitle",
            "The Harmonist team appreciates your feedback."),
    box(width = 8,
        
        tags$p(tags$a("Contact us", href="mailto:harmonist@vumc.org", target="_blank"))
     
       # textAreaInput("emailBody","Enter message:"),
      #  textInput("userEmail", "(Optional) If you would like a response, enter your email address:"),
       # actionButton("emailJudy", "Send message to Harmonist Developer"))
    )
  )
})

# observeEvent(input$emailJudy,{
#   lastActivity(Sys.time())
#   emailMessage <- isolate(input$emailBody)
#   userEmail <- isolate(input$userEmail)
#   if (emailMessage!=""){
#     postFeedback(emailMessage, userEmail)
#     #sendEmail(emailMessage)
#     updateTextAreaInput(session,"emailBody",
#                         label =  "Message sent. To provide additional feedback, enter new message below:",
#                         value = "")
#     updateTextInput(session, "userEmail", value = "")
#   }
#   
# })