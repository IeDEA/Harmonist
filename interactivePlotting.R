output$plotUI <- renderUI({

  if (resetFileInput$reset) {
    return(fluidPage(fluidRow(tags$h3(class = "row_text title", notReadyMessage))))
  }
  if (is.null(errorTable())){
    return(fluidPage(fluidRow(tags$h3(class = "row_text title", notReadyMessage))))
  } 
  if (is.null(errorTable()[[1]])){
    return(fluidPage(fluidRow(tags$h3(class = "row_text title", notReadyMessage))))
  }
  fluidPage(
    tags$h3(class = "row_text title",
            "Visualize data", backToHubMessage()),
    tags$h5(class = "row_text subtitle",
            "After selecting the desired table and variable(s) to include in your graph, click Generate graph"),
    fluidRow(column(3,uiOutput("chooseTable")),
             column(2,uiOutput("selectVar1") ),
             column(3,uiOutput("selectVar2") ),
             column(3,uiOutput("chooseOutliers") )),
    
    # div(
    #   checkboxInput("removeOutliers", "Remove Outliers", value = FALSE),
    #   style = "margin: 0 0 0 20px;"
    # ),
    fluidRow(style = "margin: 0 0 5px 2px;", uiOutput("outlierMsg")),
    
    fluidRow(column(3,uiOutput("plotit"))),
    fluidRow(
      column(12, box(width = 12, height = "700px",
                     plotlyOutput("plot", height = 600))))
  )
})

# interactive plot exploration code -----------------------------------------------------   
output$chooseTable <- renderUI({
  
  return(selectInput("table2plot", "Select a table to investigate interactively", 
                     choices = tablesAndVariables$tablesToCheck))
  
})

output$selectVar1 <- renderUI({
  if (resetFileInput$reset) return(NULL)
  
  if ((is.null(infile())) | is.null(formattedTables())| is.null(input$table2plot)){
    return(NULL)
  } else {
    choices1 <- names(formattedTables()[[input$table2plot]])
    choices1 <- choices1[!(choices1 %in% c(patientVar,"recordIndex"))]
    return(selectInput("var1", "Select a variable to plot", choices = choices1))
  }
})


observeEvent(input$var1,{
  output$selectVar2 <- renderUI({
    if (resetFileInput$reset) return(NULL)
    if ((is.null(infile())) || is.null(formattedTables()) || is.null(input$table2plot) || is.null(input$var1)) return(NULL)
    
    dataset <- formattedTables()[[input$table2plot]]
    # first check to see if chosen variable is a lab value, force the user to indicate units
    if (!is.null(input$var1) && endsWith(input$var1,"_V")){
      labName <- strsplit(input$var1,"_V")[[1]]
    }
    # some lab values might not require units; only require units if they exist 
    if (!is.null(input$var1) && endsWith(input$var1,"_V") && (exists(paste0(labName,"_U"), dataset))){
      choices2 <- levels(dataset[[paste0(labName,"_U")]])
      message <- paste0("Select the desired units for ",input$var1)
    } else {
      temp <- sapply(dataset, is.factor) | sapply(dataset, is.character)
      # eliminate variables that are all na as choices
      temp <- temp & sapply(dataset, function(x){any(!is.na(x))})
      choices2 <- names(which(temp))
      choices2 <- c("None", choices2[choices2 != patientVar])
      #For now remove _D_A variables from plotting
      choices2 <- choices2[!endsWith(choices2,"D_A")]
      choices2 <- choices2[choices2 != input$var1]
      message <- "Select a categorical variable to group data"
    }
    
    selectInput("var2", message,
                choices = choices2)
  })
})

observeEvent(input$var1,{
  output$chooseOutliers <- renderUI({
    return(selectInput("chooseOutliers", "Remove Outliers", 
                     choices = c("None", "Less", "More")))
  
  })
})


output$plotit <- renderUI({
  if (resetFileInput$reset) return(NULL)
  if ((is.null(infile())) | is.null(formattedTables()) | is.null(input$table2plot)) return(NULL)
  else
    actionButton("plotButton","Generate graph")
})

p <- eventReactive(input$plotButton,{
  lastActivity(Sys.time())
  
  trackDetailsForREDCap$plots <- rbind(trackDetailsForREDCap$plots,
                                       data.frame( 
                                         action_step = "createplot",
                                         action_ts = Sys.time(),
                                         plot_table = input$table2plot,
                                         plot_var1 = input$var1,
                                         plot_var2 = input$var2,
                                         stringsAsFactors = FALSE))
  dataset <- formattedTables()[[input$table2plot]]
  
  plotOptions <- theme(axis.title.x = element_text(margin = margin(t = -10))) +
    theme(axis.text.x = element_text(angle = xAxisLabelAngle)) 
  
  # if first variable is discrete plot a bar chart 
  if (is.factor(get(input$var1, dataset)) || 
      is.character(get(input$var1, dataset)) ||
      is.integer(get(input$var1, dataset))){
      
      # shinyjs::hide(id = "removeOutliers")
      shinyjs::hide(id = "chooseOutliers")

    if (input$var2 == "None"){
      p <- ggplot(dataset,
                  aes_string(input$var1)) + 
        geom_bar(fill = "blue") + theme(legend.position = "none") + 
        plotOptions 
      
      outlier_message <- ""
      output$outlierMsg <- renderUI({ 
        outlier_message
      })
    }
    else {
      p <- ggplot(dataset,
                  aes_string(input$var1, fill= input$var2)) + 
        geom_bar() +
        # scale_fill_manual(values = getPalette ) +
        # scale_fill_viridis_d(na.value = "gray50") +
        scale_fill_brewer(palette = "Paired", na.value = "gray50") +
        
        plotOptions
      
      outlier_message <- ""
      output$outlierMsg <- renderUI({ 
        outlier_message
      })
    }
  }
  else{ #if first variable is continuous plot a histogram
    # if var1 is a lab value (_V) see if var2 is units for that lab
    # shinyjs::show(id = "removeOutliers")
    shinyjs::show(id = "chooseOutliers")

    labWithUnits <- FALSE
    if (endsWith(input$var1, "_V")){
      labName <- strsplit(input$var1,"_V")[[1]]
      unitsName <- paste0(labName,"_U")
      if (exists(unitsName, dataset)){
        labWithUnits <- TRUE
      }
    }
    
    if (labWithUnits){
      datasetToPlot <- dataset %>% filter(!!rlang::sym(unitsName) == input$var2) 
      
      sub_data <- datasetToPlot[,input$var1]
      
      removeOutliers = input$chooseOutliers
      if (removeOutliers != 'None') {
        outlier_level = input$chooseOutliers
        # if removeOutliers is checked perform remove outliers function
        data_no_outlier <- IQR.outliers(sub_data, outlier_level)
        dataset_no_outlier_data <- as.data.frame(data_no_outlier)
        colnames(dataset_no_outlier_data) <- input$var1

        p <- ggplot(dataset_no_outlier_data,
                    aes_string(input$var1)) + 
                              # text= "paste("input$var1)) + 
          geom_histogram(fill = "blue") + theme(legend.position = "none") +
          plotOptions
          
          if (!is.null(data_no_outlier)) {
            outlier_length <- length(unlist(sub_data)) - length(unlist(data_no_outlier))
            outlier_message <- paste("Outlier(s): ", outlier_length)
          } else {
            outlier_message <- "Outlier(s): No outliers found"
            
          }
          
          output$outlierMsg <- renderUI({ 
            outlier_message
          })
      } else { 
        # if removeOutliers is not checked do not use remove outliers funciton
        datasetToPlot <- dataset %>% filter(!!rlang::sym(unitsName) == input$var2) 
       
        p <- ggplot(datasetToPlot,
                    aes_string(input$var1)) + 
        geom_histogram(fill = "blue") + theme(legend.position = "none") +
        plotOptions

        outlier_message <- ""

        output$outlierMsg <- renderUI({ 
          outlier_message
        })
      }
    }
    else if (input$var2=="None"){
      sub_data <- dataset[,input$var1]
      
      # removeOutliers = input$removeOutliers
      removeOutliers = input$chooseOutliers
      if (removeOutliers != 'None') {
        outlier_level = input$chooseOutliers
        # if removeOutliers is checked perform remove outliers function
        data_no_outlier <- IQR.outliers(sub_data, outlier_level)
        dataset_no_outlier_data <- as.data.frame(data_no_outlier)
        colnames(dataset_no_outlier_data) <- input$var1

        p <- ggplot(dataset_no_outlier_data,
                    aes_string(input$var1)) + 
                              # text= "paste("input$var1)) + 
          geom_histogram(fill = "blue") + theme(legend.position = "none") +
          plotOptions
          
          if (!is.null(data_no_outlier)) {
            outlier_length <- length(unlist(sub_data)) - length(unlist(data_no_outlier))
            outlier_message <- paste("Outlier(s): ", outlier_length)
            # outlier_message <- input$chooseOutliers
          } else {
            outlier_message <- "Outlier(s): No outliers found"
          }
          
          output$outlierMsg <- renderUI({ 
            outlier_message
          })
      } else {
        # if removeOutliers is not checked do not use remove outliers funciton
        p <- ggplot(dataset, aes_string(input$var1)) + 
                            # text= "paste("input$var1)) + 
        geom_histogram(fill = "blue") + theme(legend.position = "none") +
        plotOptions

        outlier_message <- ""
        
        output$outlierMsg <- renderUI({ 
          outlier_message
        })
      }
      
    }
    else {
      # var2 (categorical) is specified and is not lab units
      sub_data <- dataset[,c(input$var1,input$var2)]
      removeOutliers = input$chooseOutliers
      if (removeOutliers != 'None') {
        outlier_level = input$chooseOutliers
        # if removeOutliers is checked perform remove outliers function
        data_no_outlier <- IQR.outliers.second(sub_data, outlier_level)
        
        p <- ggplot(data_no_outlier,
                    aes_string(input$var1, fill= input$var2)) +
                    scale_fill_brewer(palette = "Paired", na.value = "gray50") +
          geom_histogram() + # theme(legend.position = "none") +
          plotOptions
          
          if (!is.null(data_no_outlier)) {
            outlier_length <- (length(unlist(sub_data)) - length(unlist(data_no_outlier))) / 2 # divide by two because there are two columns
            outlier_message <- paste("Outlier(s): ", outlier_length)
          } else {
            outlier_message <- "Outlier(s): No outliers found"
          }
          
          output$outlierMsg <- renderUI({ 
            outlier_message
          })
      } else {
        # if removeOutliers is not checked do not use remove outliers funciton
        p <- ggplot(dataset,
                  aes_string(input$var1, fill= input$var2)) + 
        geom_histogram() + 
        #scale_fill_viridis_d(na.value = "gray50") +
        scale_fill_brewer(palette = "Paired", na.value = "gray50") + 
        plotOptions

        outlier_message <- ""
        
        output$outlierMsg <- renderUI({ 
          outlier_message
        })
      }
      
    }
  }
  if (endsWith(input$var1, "_D")){
    p2 <- ggplotly(p, tooltip = c( "y"))
  } else {
    p2 <- ggplotly(p) #, tooltip = c("x","y"))
  }
  
  p2$elementId <- NULL #this is to avoid the warning Ignoring explicitly provided widget ID; Shiny doesn't use them
  return(p2)
})


output$plot <- renderPlotly({
  if (resetFileInput$reset) return(NULL)
  p()
})

