# This code was written by Jeremy Benn Associates Limited as part of the Multivariate Event Modeller tool. 
# It is released under the Open Government Licence 
# (https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
# Copyright (C) 2023 Environment Agency

# This program is free software; you can redistribute it and/or modify it under the terms 
# of the Open Government Licence.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY. 
# See the Open Government Licence for more details.


############################## Import libraries ##############################
library(shiny)
library(evd)
library(ismev)
library(texmex)
library(stringr)
library(lmomco)
library(plotly)
library(data.table)
library(rhandsontable)
library(shinyFiles)
library(shinyjs)

################ Clear memory, increase maximum upload size and suppress warnings ###############
rm(list = ls())
options(shiny.maxRequestSize = 800 * 1024 ^ 2)
options(shiny.sanitize.errors = FALSE)
options(warn = -1)

############################## Source functions ##############################
source("Functions.R")

############################### Set parameters ###############################
Expert = FALSE
Value = "Value"  # This is referred to as 'Level' in some of the code e.g. AEPtoLevel

###################### Find out if the tool is running locally ######################
is_local <- Sys.getenv('SHINY_PORT') == ""

##################################### shinyServer function ###########################################
# Define server logic required to build the app
shinyServer(function(input, output, session) {
  # If 'Expert' is set to FALSE, hide the option to change the threshold
  observe({
    if (Expert == FALSE) {
      shinyjs::hide("threshold")
      shinyjs::hide("thresholdDeclust")
    } else{
      shinyjs::show("threshold")
      shinyjs::show("thresholdDeclust")
    }
  })
  
################################ Overview tab #####################################
  
  # Create a warning pop-up message if file being uploaded is not a RDS
  observeEvent(input$savedModel, {
    if (!str_sub(input$savedModel$name, -4) == ".RDS") {
      showModal(modalDialog("File must be in .RDS format. Please refresh the MEM to upload data."))
    }
  })
  
  ### Load previously-saved model
  savedModelData <- eventReactive(input$savedModel, {
    savedModelFile <- input$savedModel
    
    if (is.null(savedModelFile))
      # return null if input data set is empty
    {
      return(NULL)
    }
    
    else if (str_sub(input$savedModel$name, -4) == ".RDS") {
      readRDS(savedModelFile$datapath)
    }  # read the data from a .RDS file if the file type is correct
    else {
      # otherwise return null
      return(NULL)
    }
  })
  
  ### Create a reactive output which exists if the file is uploaded - this can then be used as a condition
  ### for later steps
  output$savedModelUploaded <- reactive({
    return(!is.null(savedModelData()))
  })
  outputOptions(output, 'savedModelUploaded', suspendWhenHidden = FALSE)
  
  ### Save file name of input data to an output to print to screen
  output$savedModelFileName <- renderUI({
    validate(# check that the saved model input is not empty and if it is return this output as empty as well
      need(input$savedModel != "", ""))
    
    savedModelFile <- input$savedModel
    savedModelFileName <-
      savedModelFile$name  # extract the uploaded file name
    paste0("You have uploaded ", savedModelFileName)
  })
  
  ################################### Tab 1 - Input data ###################################
  ### Move to the 'Input data' tab once the user has clicked 'Create a new joint probability model'
  observe({
    if (input$newModel > 0) {
      updateTabsetPanel(session, "tabsetPanelID", selected = "1 Input data")
    }
  })
  
  ####################### Main panel before data loaded #######################
  ### Import data reactively based on the user selecting a file to upload
  ### Use validate to ensure that any outputs conditional on the input do not show an R
  ### error message before the data have been uploaded - if the input file is empty then
  ### "Please select a data set" is displayed as a warning message
  ### Import data into 'inData'
  inData <- reactive({
    if (input$savedMod) {
      # if the 'Load saved model' button has been clicked
      savedModel <-
        savedModelData()  # set the input data to be input data from the saved model
      data.frame(savedModel$userData, stringsAsFactors = FALSE)  # convert to data frame
    } else{
      # else, if the 'Load saved model' button has not been clicked
      validate(need(input$file1 != "", "Please select a data set"))
      
      inFile <-
        input$file1  # set the input data to be the data uploaded in the 'Input data' tab
      
      if (is.null(input$file1))
        # return null if input data set is empty
        return(NULL)
      
      inData <-
        read.csv(inFile$datapath,
                 header = TRUE,
                 stringsAsFactors = FALSE)  # read the data from a .csv file
      
      if (input$dataType == "ts") {
        # if the user has selected the data type to be time series
        colnames(inData)[1] <-
          "Date"  # change the first column name to 'Date' in case it is not already
        
      } else if (input$dataType == "declust") {
        # else, if the data type is declustered
        # if the file extension is correct, read the data in normally
        if (str_sub(input$file1$name,-4) == ".csv") {
          Index <- c(1:nrow(inData))  # add an index column of numbers
          inData <- data.frame(Date = Index, inData)
          
        } else {
          # if the file type is incorrect the csv cannot be read in properly so create 
          # a placeholder dataframe to prevent the app from crashing
          colnames(inData)[1] <- "PlaceHolder"
        }
        
      }
      inData  # return inData
    }
  })
  
  output$fileUpload <- renderUI({
    ### Set up a file input to allow the user to upload their data
    fileInput(
      'file1',
      'Input data',
      accept = c(
        'text/csv',
        'text/comma-separated-values,text/plain',
        '.csv'
      )
    )
  })
  
  
  # Once a csv file has been uploaded, produce pop-up warning messages for incorrect formats
  observeEvent(input$file1, {
    # Create a pop-up warning message if file being uploaded is not a csv
    if (str_sub(input$file1$name, -4) != ".csv") {
      showModal(
        modalDialog(
          "File must be in .csv format. If you would like to load a previously created model, please refesh the MEM and upload the model in the overview tab."
        )
      )
    }
    
    
    # Create a pop-up warning message if file being uploaded appears to have an incorrect format based on the selected data type
    if (input$dataType == "ts") {
      # Time series data should have "/" characters in the date column, otherwise the data is likely to be declustered data
      if (length(grep("/", as.character(inData()$Date)[1:5])) == 0) {
        showModal(
          modalDialog(
            "You have selected to load time series data sampling all observations but this looks like observations sampled at peak surge events. This may cause errors so please check before continuing and refresh the MEM if you need to change the data type or the input data."
          )
        )
      }
    } else if (input$dataType == "declust") {
      # The upload process for declustered data adds an index column. If the data contains a date column this will be in the second column. Check this by looking for "/" in the second column
      if (length(grep("/", inData()[1:5, 2])) > 0) {
        showModal(
          modalDialog(
            "You have selected to load observations sampled at peak surge events but this looks like time series data sampling all observations. This may cause errors so please check before continuing and refresh the MEM if you need to change the data type or the input data."
          )
        )
      }
    }
  })
  
  
  ### Create a reactive output which exists if a file has been uploaded - this can then be used as a condition
  ### for later steps
  output$fileUploaded <- reactive({
    return(ncol(inData()) > 0 &
             ncol(inData()) < 12)  # output exists if input data exist with 1-11 columns (i.e. up to the max of 10 variables)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  
  ### Create a reactive output which exists if a file has been uploaded with 
  ### too many dimensions - can be used to give an error (looks for ncol > 12
  ### if the data looks to be time series but selected as 'declustered' because
  ### an extra row is automatically added)
  output$fileTooManyDim <- reactive({
    return(ncol(inData()) > 11 & !(length(grep("/", inData()[1:5,2])) > 0 & input$dataType == "declust") | 
             ncol(inData()) > 12 & (length(grep("/", inData()[1:5,2])) > 0 & input$dataType == "declust"))
  })
  outputOptions(output, 'fileTooManyDim', suspendWhenHidden = FALSE)

  
  ### Create a reactive output which exists if a file type is CSV
  output$fileTypeCSV <- reactive({
    return(str_sub(input$file1$name, -4) == ".csv")
  })
  outputOptions(output, 'fileTypeCSV', suspendWhenHidden = FALSE)
  
  
  ### Create a text output if the data look like time series data but 'declustered' has been selected
  output$looksTS <- renderText({
    validate(
      need(length(grep("/", inData()[1:5,2])) > 0, ""),  # if declustered is selected but actually time series, 
      need(input$dataType == "declust", "")  # there will be two index/date columns - if second column contains a '/', 
    )  # it is likely to be a date - if so, write out the below message
    "You have selected to load observations sampled at peak surge events but this looks like time series data sampling
     all observations. This may cause errors so please check before continuing and refresh the MEM if you need to 
     change the data type or the input data."
  })
    
  
  ### Create a text output if the data look like declustered data but 'time series' has been selected
  output$looksDC <- renderText({
    validate(
      need(length(grep("/", as.character(inData()$Date)[1:5])) == 0, ""),  # check whether the first column looks like 
      need(input$dataType == "ts", "")  # a date (whether it contains a '/'?) - if it doesn't then write out below message
    )
    "You have selected to load time series data sampling all observations but this looks like observations sampled
     at peak surge events. This may cause errors so please check before continuing and refresh the MEM if you 
     need to change the data type or the input data."
  })
  
  
  ### Save file name of input data to an output to print to screen
  output$inFileName <- renderUI({
    validate(# check that the file input is not empty and if it is return this output as empty as well
      need(input$file1 != "", ""))
    
    inFile <- input$file1
    inFileName <- inFile$name  # extract the file name
    paste0("You have uploaded ", inFileName)
  })
  
  
  ############### Conditional panel on left-hand side based on data being loaded ################
  ### Summary statistics
  # Creates error message pop up if the user attempts to enter values of year which are too large
  # (large values can cause graph issues as R runs out of memory)
  observeEvent(input$numYearsDeclust, {
    if (!is.na(input$numYearsDeclust) &
        input$numYearsDeclust >= 1000) {
      showModal(modalDialog("Length of observed record must be less than 1000 years. Entering the
                            wrong number of years may invalidate your analysis."))
    } else if (!is.na(input$numYearsDeclust) &
               input$numYearsDeclust < 0) {
      showModal(modalDialog("Length of observed record must be positive."))
    }
  })

  ### Write out the effective number of years of data 
  output$numYears <- renderText({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    if (input$dataType == "ts") {  # if data type is time series
      nyears <- length(inData()[,1])/365  # number of years is the number of records divided by 365  
      nyears <- sprintf('%.2f', nyears)  # print to 2 decimal places
    } else if (input$dataType == "declust") {  # if data type is declustered
      validate(  # need the user to enter the number of years otherwise give warning message
      need(is.na(input$numYearsDeclust) == FALSE, "Enter length of observed record in years"))
      validate(need(input$numYearsDeclust < 1000, "Length of observed record must be less than 1000 years."))# values over 8006 will cause graphs issues as R runs out of memory 
      validate(need(input$numYearsDeclust > 0, "Number of years cannot be negative"))
      nyears <- input$numYearsDeclust
      nyears <- sprintf('%.2f', nyears)
    }
    paste("Number of years: ", nyears, sep = "")
  })
  
  ### Write out the effective number of years of data 
  output$numYears_savedModel <- renderText({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    if (input$dataType == "ts") {  # if data type is time series
      nyears <- length(data.frame(savedModelData()$model$Transformed)[,1])/365  # number of years is the number of records divided by 365  
      nyears <- sprintf('%.2f', nyears)  # print to 2 decimal places
    } else if (input$dataType == "declust") {  # if data type is declustered
      validate(  # need the user to enter the number of years otherwise give warning message
        need(is.na(input$numYearsDeclust) == FALSE, "Enter length of observed record in years"))
      validate(need(input$numYearsDeclust < 1000, "Length of observed record must be less than 1000 years."))# values over 8006 will cause graphs issues as R runs out of memory 
      validate(need(input$numYearsDeclust > 0, "Number of years cannot be negative"))
      nyears <- input$numYearsDeclust
      nyears <- sprintf('%.2f', nyears)
    }
    paste("Number of years: ", nyears, sep = "")
  })
  
  ### Write out the number of variables 
  output$numVar <- renderText({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    nVar <- ncol(inData()) - 1  # number of columns minus 1 (Date/Index)
    paste("Number of variables: ", nVar, sep = "")  # this is correct for time series and declustered data
  })
  
  ### Write out the number of variables 
  output$numVar_savedModel <- renderText({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    nVar <- ncol(inData()) - 1  # number of columns minus 1 (Date/Index)
    paste("Number of variables: ", nVar, sep = "")  # this is correct for time series and declustered data
  })
  
  ### Write out the number of threshold exceedances per gauge (declustered)
  output$thrExceedDC <- renderText({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    validate(need(sum(is.na(inData())) == 0, "")) # need complete data
    thr <- input$thresholdDeclust  # set to 0.975 - disappears when Expert=FALSE
    thrEx <- (1 - thr)*length(inData()$Date)
    thrEx <- sprintf('%1.f', thrEx)
    paste("Number of threshold exceedances per variable: ", thrEx, sep = "")
  })
  
  ### Write out the number of threshold exceedances per gauge (declustered)
  output$thrExceedDC_savedModel <- renderText({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    validate(need(sum(is.na(inData())) == 0, "")) # need complete data (should not be possible to have missing values in saved model)
    thr <- input$thresholdDeclust  # set to 0.975 - disappears when Expert=FALSE
    thrEx <- (1 - thr)*length(inData()$Date)
    thrEx <- sprintf('%1.f', thrEx)
    paste("Number of threshold exceedances per variable: ", thrEx, sep = "")
  })

  ### Write out the number of threshold exceedances per gauge (time series)
  output$thrExceedTS <- renderTable({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    validate(need(sum(is.na(inData())) == 0, "")) # need complete data
    thr <- input$threshold  # 0.975 or user input depending on whether Expert is set to TRUE or FALSE
    numIndEvents <- No.Ind.Events(inData()[,-1], thr)  # Number of independent events
    var.names <- names(inData()[,-1])
    data.frame(Variable = var.names, "Number of independent events" = numIndEvents, check.names = FALSE)}, 
    caption = "<span style='color:#333333'> Number of independent events per variable after the MEM has declustered the data:",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )

  ### Write out the number of threshold exceedances per gauge (time series)
  output$thrExceedTS_savedModel <- renderTable({
    validate(
      need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
    )
    validate(need(sum(is.na(inData())) == 0, "")) # need complete data (should not be possible to have missing values in saved model)
    thr <- input$threshold  # 0.975 or user input depending on whether Expert is set to TRUE or FALSE
    numIndEvents <- No.Ind.Events(savedModelData()$model$Transformed, thr)  # Number of independent events
    var.names <- names(inData()[,-1])
    data.frame(Variable = var.names, "Number of independent events" = numIndEvents, check.names = FALSE)}, 
    caption = "<span style='color:#333333'> Number of independent events per variable after the MEM has declustered the data:",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )

  # Create inputs for variable labels
  output$userDefinedValueLabels <- renderUI({
    tags <- tagList()
    for (i in 1:(ncol(inData()) - 1)) {
      name = colnames(inData()[i + 1])
      tags[[i]] <- textInput(paste0('value', name), 
                                paste0(name, " label"),
                                paste0(name))
    }
    tags
  })
  
  # Output the user defined labels at all times so they are still accessible to the graphs when the input data tab is skipped
  outputOptions(output,"userDefinedValueLabels", suspendWhenHidden = FALSE) 
  
  
  ###################### Main panel after data loaded ########################
  ### Create time series plots of the input data using plotly
  observe({
    output$timeseries <- renderPlotly({
      if (input$dataType == "ts") {
        # if data type is time series
        validate(need(length(grep("/", as.character(inData()$Date)[1:5])) != 0, 
                      "Unable to plot...")) # do not plot if the data doesn't contain "/" in the date column (this indicates that the data is declustered)
        validate(need(sum(is.na(inData())) == 0, 
                      "Missing values exist: unable to plot.")) # do not plot if there are missing values
        
        thr <- input$threshold  # set threshold
        ts.df <- inData()
        ts.df[, 1] <-
          as.Date(ts.df[, 1], format = "%d/%m/%Y")  # convert the first column to date format
        colnames(ts.df)[1] <-
          "Date"  # set the name of the first column to "Date" so this can be used as a label on the x-axis
        vars <-
          setdiff(names(ts.df), "Date")  # extract the variable names from the other column names
        ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
        ### as a function which is then applied over each variable before putting them into a subplot environment)
        
        plots <- lapply(vars, function(var) {
          thrName = paste0(input$threshold * 100, "th percentile")  # set percentile label
          
          # Extract the user defined label
          ylabel = input[[paste0("value", var)]]
          graphTitle = input$userDefinedInputGraphTitle
          
          plot_ly(
            ts.df,
            x = ~ Date,
            y = ~ get(var),
            name = var,
            type = 'scatter',
            mode = 'lines',
            showlegend = FALSE
          ) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = ylabel),
                   title = graphTitle) %>%
            add_trace(
              x = c(min(ts.df$Date), max(ts.df$Date)),
              y = c(quantile(ts.df[[var]], thr), quantile(ts.df[[var]], thr)),
              name = thrName,
              mode = "lines",
              line = list(color = "rgb(0,0,0)"),
              showlegend = FALSE
            )
        })
        subplot(
          plots,
          nrows = length(plots),
          shareX = TRUE,
          titleX = TRUE,
          titleY = TRUE
        ) %>% layout(height = 600 * round(length(vars) / 3))
      } else if (input$dataType == "declust") {
        # if data type is declustered
        
        validate(need(length(grep("/", inData()[1:5, 2])) == 0, 
                      "Unable to plot...")) # do not plot if the data contains "/" in the second column (this indicates that the data is time series)
        validate(need(sum(is.na(inData())) == 0, 
                      "Missing values exist: unable to plot.")) # do not plot if there are missing values
        
        thr <- input$thresholdDeclust  # set threshold
        ts.df <- inData()
        vars <-
          setdiff(names(ts.df), "Date")  # extract the variable names from the other column names
        colnames(ts.df)[1] <- "Index"
        
        
        ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
        ### as a function which is then applied over each variable before putting them into a subplot environment)
        plots <- lapply(vars, function(var) {
          thrName = paste0(input$thresholdDeclust * 100, "th percentile")  # set percentile label
          
          # Extract the user defined label
          ylabel = input[[paste0("value", var)]]
          graphTitle = input$userDefinedInputGraphTitle
          
          plot_ly(
            ts.df,
            x =  ~ Index,
            y = ~ get(var),
            name = var,
            type = 'scatter',
            mode = 'markers',
            showlegend = FALSE
          ) %>%
            layout(xaxis = list(title = "Index number"),
                   yaxis = list(title = ylabel),
                   title = graphTitle) %>% 
            add_trace(
                     x = c(min(ts.df$Index), max(ts.df$Index)),
                     y = c(quantile(ts.df[[var]], thr), quantile(ts.df[[var]], thr)),
                     name = thrName,
                     mode = "lines",
                     line = list(color = "rgb(0,0,0)"),
                     showlegend = FALSE
                   )
        })
        subplot(
          plots,
          nrows = length(plots),
          shareX = TRUE,
          titleX = TRUE,
          titleY = TRUE
        ) %>% layout(height = 600 * round(length(vars) / 3))
      }
    })
    })
    
    #### Once data are uploaded (inData is no longer null), activate the '2 Time lag' tab
    observe({
      if (!is.null(inData()) && input$dataType == "ts") { 
        session$sendCustomMessage('activeTabs', '2 Time lag')
      }
    })
    
    ################################### Tab 2 - Time lag ###################################
    ############################# Main panel #############################
    # Create a pop-up warning message if declustered data is uploaded and the user attempts to access the time lag tab
    observe(
      if (input$dataType == "declust" && input$tabsetPanelID == "2 Time lag") {
        showModal(
          modalDialog(
            "The time lag feature is unavailable for observations sampled at peak surge events."
          )
        )
      }
    )
    
    # Extract the lag parameters data if a user-saved model has been loaded
    lagTable.two <- 
      eventReactive(!is.null(savedModelData()), {
      savedModel <- savedModelData()
      savedModel$lagTableSave
    })
    
    ### Update 'Use lagged data for analysis' checkbox based on loaded user-saved model
    observeEvent(!is.null(savedModelData()), {
      lagTickVal = savedModelData()$lagTickSave
      updateCheckboxInput(session, "lagTick", value = lagTickVal)
    })
    
    ### Set up an empty reactiveValues object which will store the values from the table
    lagTable <- reactiveValues(values = NULL)
    
    ### Create the time lag table
    output$lag <- renderRHandsontable({
      Var.names <- names(inData()[, -1])  # extract variable names
        lagTable <-
          data.frame(
            Days = rep(0, length(Var.names)),
            stringsAsFactors = FALSE
          )
      ### Create the table
      rht = rhandsontable(
        lagTable,
        readOnly = FALSE,
        rowHeaders = Var.names,
        rowHeaderWidth = 150,
        selectCallback = TRUE,
        digits = 0,
        col_highlight = 1) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric(cols = "Days", min = 0, max = (nrow(inData()) - 2), 
                             exclude = NA, allowInvalid = TRUE) %>%
        hot_col("Days", format = "0")
      rht  # return the table
    })
    
    ### Create the time lag table after it has been updated
    output$lagAfter <- renderRHandsontable({
      Var.names <- names(inData()[, -1])  # extract variable names
      lagTable_After <-
        data.frame(
          Days = hot_to_r(input$lag),
          stringsAsFactors = FALSE
        )
      ### Create the table
      rht = rhandsontable(
        lagTable_After,
        readOnly = TRUE,
        rowHeaders = Var.names,
        rowHeaderWidth = 150,
        selectCallback = TRUE,
        digits = 0,
        col_highlight = 1) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric(cols = "Days", min = 0, max = (nrow(inData()) - 2), 
                             exclude = NA, allowInvalid = TRUE) %>%
        hot_col("Days", format = "0")
      rht  # return the table
    })
    
    output$lagSaved <- renderRHandsontable({
      Var.names <- names(inData()[, -1])  # extract variable names
      lagTable_Saved <- t(data.frame(
        lagTable.two(),
        stringsAsFactors = FALSE
        ))
        
      ### Create the table
      rht = rhandsontable(
        lagTable_Saved,
        readOnly = TRUE,
        rowHeaders = Var.names,
        colHeaders = "Days",
        rowHeaderWidth = 150,
        selectCallback = TRUE,
        digits = 0,
        col_highlight = 1) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      rht  # return the table
    })
    
    ### Set lag table to always exist so user can skip lag tab without breaking graphs on later tabs
    output$lagExists <- reactive({
      return(!is.null(input$lag))
    })
    outputOptions(output, 'lagExists', suspendWhenHidden = FALSE)
        
    ### Transform the data to be consistent with the lag parameters specified
    observeEvent(input$lag, {  # the data is transformed every time a value is entered into the lag rhandsontable table
      if (!input$savedMod) {  # if a saved model has not been uploaded
        if (!is.na(any(unlist(input$lag$data)))) {  # only update if none of the values in the table have been left blank 
          # Extract the lag parameters
          lagParameters_1 <- data.frame(c(0)) # create a zero data frame
          colnames(lagParameters_1) <- "Days"
          lagParameters_2 <- hot_to_r(input$lag) # extract lag values from the lag table 
          lagParameters <- rbind(lagParameters_1, lagParameters_2) # bind the lag table together with the zero so the date column doesn't get shifted  
          lagParameters <- as.data.frame(lagParameters)
          
          ts.df <- inData() # extract data
          data <- data.frame(ts.df)  # extract the data with the date
          setDT(data)[, names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table, doesn't shift the date column
          ts.lag.df <- cbind(ts.df[1:nrow(data),1], data)  # bind the date back to the shifted data
          colnames(ts.lag.df)[1] <- "Original_Date"
          ts.lag.df <- data.frame(ts.lag.df)
          
          
          ### Create time series plots of the input data using plotly
          output$timelag <- renderPlotly({
            if (input$dataType == "ts" && !input$savedMod) {  # if the data type is time series and no saved model has been uploaded
              # if data type is time series
              validate(need(length(grep("/", as.character(inData()$Date)[1:5])) != 0, 
                            "Unable to plot...")) # do not plot if the data doesn't contain "/" in the date column (this indicates that the data is declustered)
              
              thr <- input$threshold  # set threshold
              colnames(ts.lag.df)[2] <-
                "Date"  # set the name of the second column to "Date" so this can be used as a label on the x-axis
              ts.lag.df$Date <-
                as.Date(ts.lag.df$Date, format = "%d/%m/%Y")  # convert the second column to date format
              ts.lag.df$Original_Date <-
                as.Date(ts.lag.df$Original_Date, format = "%d/%m/%Y")  # convert the first column to date format
               
              
              vars <-
                setdiff(names(ts.lag.df), c("Original_Date","Date"))  # extract the variable names from the other column names
              ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
              ### as a function which is then applied over each variable before putting them into a subplot environment)
              plots <- lapply(vars, function(var) {
                ### Create an original date column by subtracting number of NA entries in each column (present from shift function earlier)
                ts.lag.df$Original_Date <- ts.lag.df$Date - sum(is.na(ts.lag.df[,which(colnames(ts.lag.df) == var)]))
                thrName = paste0(input$threshold * 100, "th percentile")  # set percentile label
                
                # Extract the user defined label
                ylabel = input[[paste0("value", var)]]
                graphTitle = input$userDefinedLagGraphTitle
                
                plot_ly(
                  ts.lag.df,
                  x = ~ Date,
                  y = ~ get(var),
                  name = var,
                  type = 'scatter',
                  text = ~paste0(var, ": ", get(var), 
                                 "<br>Original Date: ", format(Original_Date, "%d/%m/%Y"), 
                                 "<br>Lagged Date: ", format(Date, "%d/%m/%Y")), # format hover text in a readable way
                  hoverinfo = 'text',
                  mode = 'lines',
                  showlegend = FALSE
                ) %>%
                  layout(xaxis = list(title = "Date"),
                         yaxis = list(title = ylabel),
                         title = graphTitle) %>%
                add_trace(
                  y = c(quantile(ts.lag.df[[var]], thr, na.rm = TRUE)),
                  name = thrName,
                  mode = "lines",
                  hoverinfo = "y",
                  line = list(color = "rgb(0,0,0)"),
                  showlegend = FALSE
                )
              })
              subplot(
                plots,
                nrows = length(plots),
                shareX = TRUE,
                titleX = TRUE,
                titleY = TRUE
              ) %>% layout(height = 600 * round(length(vars) / 3))
            } else if (input$dataType == "declust") {
              # if data type is declustered
              
              validate(need(length(grep("/", inData()[1:5, 2])) == 0, 
                            "Unable to plot...")) # do not plot if the data contains "/" in the second column (this indicates that the data is time series)
              
              thr <- input$thresholdDeclust  # set threshold
              ts.df <- inData()
              vars <-
                setdiff(names(ts.df), "Date")  # extract the variable names from the other column names
              colnames(ts.df)[1] <- "Index"
              ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
              ### as a function which is then applied over each variable before putting them into a subplot environment)
              plots <- lapply(vars, function(var) {
                thrName = paste0(input$thresholdDeclust * 100, "th percentile")  # set percentile label
                
                # Extract the user defined label
                ylabel = input[[paste0("value", var)]]
                graphTitle = input$userDefinedLagGraphTitle
                
                plot_ly( 
                  ts.df,
                  x = ~ Index,
                  y = ~ get(var),
                  name = var,
                  type = 'scatter',
                  mode = 'markers',
                  showlegend = FALSE
                ) %>%
                  layout(xaxis = list(title = "Index number"),
                         yaxis = list(title = ylabel),
                         title = graphTitle) %>% 
                  add_trace(
                         x = c(min(ts.df$Index), max(ts.df$Index)),
                         y = c(quantile(ts.df[[var]], thr), quantile(ts.df[[var]], thr)),
                         name = thrName,
                         mode = "lines",
                         line = list(color = "rgb(0,0,0)"),
                         showlegend = FALSE
                         )
              })
              subplot(
                plots,
                nrows = length(plots),
                shareX = TRUE,
                titleX = TRUE,
                titleY = TRUE
              ) %>% layout(height = 600 * round(length(vars) / 3))
            }
          })
        } else{
          NULL
        }
      }
    })
    
    ###################### Main panel after saved model loaded ########################

    ### Create time series plots of the input data using plotly
    output$timelag_savedModel <- renderPlotly({
      if (input$dataType == "ts" && input$savedMod && input$lagTick == TRUE) {  # if the data is time series, a saved model has been uploaded and lagged data selected
        # if data type is time series
        validate(need(length(grep("/", as.character(inData()$Date)[1:5])) != 0, 
                      "Unable to plot..."))  # do not plot if the data doesn't contain "/" in the date column (this indicates that the data is declustered)
        
        # Extract the lag parameters
        lagParameters_1 <- data.frame(c(0)) # create a zero data frame 
        colnames(lagParameters_1) <- "Days"
        lagParameters_2 <- data.frame(unlist(savedModelData()$lagTableSave)) # extract lag values from the lag table in the saved model
        colnames(lagParameters_2) <- "Days"
        lagParameters <- rbind(lagParameters_1, lagParameters_2) # bind the lag table together with the zero so the date column doesn't get shifted
        lagParameters <- as.data.frame(lagParameters)
        
        ts.df <- savedModelData()$userData  # extract the data from the saved model
        data <- data.frame(ts.df)  # extract the data with the date
        setDT(data)[,names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table, doesn't shift date column
        ts.lag.df <- cbind(ts.df[1:nrow(data),1], data)  # bind the date back to the shifted data
        colnames(ts.lag.df)[1] <- "Original_Date"
        ts.lag.df <- data.frame(ts.lag.df)
        
        thr <- input$threshold  # set threshold
        colnames(ts.lag.df)[2] <-
          "Date"  # set the name of the second column to "Date" so this can be used as a label on the x-axis
        ts.lag.df$Date <-
          as.Date(ts.lag.df$Date, format = "%d/%m/%Y")  # convert the second column to date format
        ts.lag.df$Original_Date <-
          as.Date(ts.lag.df$Original_Date, format = "%d/%m/%Y")  # convert the first column to date format
        
        vars <-
          setdiff(names(ts.lag.df), c("Original_Date","Date"))  # extract the variable names from the other column names
        ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
        ### as a function which is then applied over each variable before putting them into a subplot environment)
        plots <- lapply(vars, function(var) {
          ### Create an original date column by subtracting the number of NA entries in each column (present from shift function earlier)
          ts.lag.df$Original_Date <- ts.lag.df$Date - sum(is.na(ts.lag.df[,which(colnames(ts.lag.df) == var)]))
          thrName = paste0(input$threshold * 100, "th percentile")  # set percentile label
          
          # Extract the user defined label
          ylabel = input[[paste0("value", var)]]
          graphTitle = input$userDefinedLagGraphTitle

          plot_ly(
            ts.lag.df,
            x = ~ Date,
            y = ~ get(var),
            name = var,
            type = 'scatter',
            text = ~paste0(var, ": ", get(var), 
                           "<br>Original Date: ", format(Original_Date, "%d/%m/%Y"), 
                           "<br>Lagged Date: ", format(Date, "%d/%m/%Y")), # format hover text in a readable way
            hoverinfo = 'text',
            mode = 'lines',
            showlegend = FALSE
          ) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = ylabel),
                   title = graphTitle) %>%
            add_trace(
              y = c(quantile(ts.lag.df[[var]], thr, na.rm = TRUE)),
              name = thrName,
              mode = "lines",
              hoverinfo = "y",
              line = list(color = "rgb(0,0,0)"),
              showlegend = FALSE
            )
        })
        subplot(
          plots,
          nrows = length(plots),
          shareX = TRUE,
          titleX = TRUE,
          titleY = TRUE
        ) %>% layout(height = 600 * round(length(vars) / 3))
      } else if (input$dataType == "ts" && input$savedMod && input$lagTick == FALSE) {  # if the data is time series, a saved model has been uploaded and lagged data not selected
        # if data type is time series
        validate(need(length(grep("/", as.character(inData()$Date)[1:5])) != 0, 
                      "Unable to plot...")) # do not plot if the data doesn't contain "/" in the date column (this indicates that the data is declustered)
        
        thr <- input$threshold  # set threshold
        ts.df <- inData()
        ts.df[, 1] <-
          as.Date(ts.df[, 1], format = "%d/%m/%Y")  # convert the first column to date format
        colnames(ts.df)[1] <-
          "Date"  # set the name of the first column to "Date" so this can be used as a label on the x-axis
        vars <-
          setdiff(names(ts.df), "Date")  # extract the variable names from the other column names
        ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
        ### as a function which is then applied over each variable before putting them into a subplot environment)
        plots <- lapply(vars, function(var) {
          thrName = paste0(input$threshold * 100, "th percentile")  # set percentile label
         
          # Extract the user defined label
          ylabel = input[[paste0("value", var)]]
          graphTitle = input$userDefinedLagGraphTitle
          
          plot_ly(
            ts.df,
            x = ~ Date,
            y = ~ get(var),
            name = var,
            type = 'scatter',
            mode = 'lines',
            showlegend = FALSE
          ) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = ylabel),
                   title = graphTitle) %>%
            add_trace(
              x = c(min(ts.df$Date), max(ts.df$Date)),
              y = c(quantile(ts.df[[var]], thr), quantile(ts.df[[var]], thr)),
              name = thrName,
              mode = "lines",
              line = list(color = "rgb(0,0,0)"),
              showlegend = FALSE
            )
        })
        subplot(
          plots,
          nrows = length(plots),
          shareX = TRUE,
          titleX = TRUE,
          titleY = TRUE
        ) %>% layout(height = 600 * round(length(vars) / 3))
      } else if (input$dataType == "declust") {
        # if data type is declustered
        
        validate(need(length(grep("/", inData()[1:5, 2])) == 0, 
                      "Unable to plot...")) # do not plot if the data contains "/" in the second column (this indicates that the data is time series)
        
        thr <- input$thresholdDeclust  # set threshold
        ts.df <- inData()
        vars <-
          setdiff(names(ts.df), "Date")  # extract the variable names from the other column names
        colnames(ts.df)[1] <- "Index"
        ### Create time series plots for each variable and add the threshold as a trace (the plotting is set
        ### as a function which is then applied over each variable before putting them into a subplot environment)
        plots <- lapply(vars, function(var) {
          thrName = paste0(input$thresholdDeclust * 100, "th percentile")  # set percentile label
          
          # Extract the user defined label
          ylabel = input[[paste0("value", var)]]
          graphTitle = input$userDefinedLagGraphTitle
          
          plot_ly(
            ts.df,
            x =  ~ Index,
            y = ~ get(var),
            name = var,
            type = 'scatter',
            mode = 'markers',
            showlegend = FALSE
          ) %>%
            layout(xaxis = list(title = "Index number"),
                   yaxis = list(title = ylabel),
                   title = graphTitle) %>% 
              add_trace(
                     x = c(min(ts.df$Index), max(ts.df$Index)),
                     y = c(quantile(ts.df[[var]], thr), quantile(ts.df[[var]], thr)),
                     name = thrName,
                     mode = "lines",
                     line = list(color = "rgb(0,0,0)"),
                     showlegend = FALSE
                   )
        })
        subplot(
          plots,
          nrows = length(plots),
          shareX = TRUE,
          titleX = TRUE,
          titleY = TRUE
        ) %>% layout(height = 600 * round(length(vars) / 3))
      }
    })
    
    
    # Set lag parameters to use in various places throughout the script
    lagParameters <- reactive({
      if (input$savedMod) {
        # if the 'Load saved model' button has been clicked
        lagParameters <- unlist(savedModelData()$lagTableSave)
      } else{
        # else, if the 'Load saved model' button has not been clicked
        validate(need(!is.na(any(unlist(input$lag$data))), ""))  # need complete lag table
        
        lagParameters <- hot_to_r(input$lag)
        lagParameters <- t(as.data.frame(lagParameters))
      }
    })
      
    
    #### Once data are uploaded (inData is no longer null), activate the '3 Build model' tab
    observe({
      if (!is.null(inData())) { 
        session$sendCustomMessage('activeTabs', '3 Build model')
      }
    })
    
  
    ################################## Tab 3 - Build model #######################################
    
    # Create a pop-up warning message if tab 3 is accessed but time lag table is not complete
    lagWarning <- observe(
      if (is.na(any(unlist(input$lag$data))) && input$tabsetPanelID == "3 Build model") {
        showModal(
          modalDialog(
            "Please complete the time lag table before accessing the build model panel."
          )
        )
      }
    )
    
    ######### Initial view before modelling and simulation - plots of the user input data #########
    
    ##### Main panel #####
    
    # Set marker size and marker opacity values so can be easily changed for the whole script
    marker_size <- 5
    marker_opacity <- 0.7
    pairs_opacity <- 0.2
    
    ### Create the standardised data reactive on the input data (observed data) 
    ### Create here so that it can be used in the plots below 
    # Use lagged data if the 'Use lagged data for analysis' box is ticked (do not need to account 
    # for saved model at this point because model is saved after simulation so will always be the
    # version after simulation)
    std_data <- reactive(
      if (input$lagTick == TRUE) {
      # Extract the lag parameters
      ts.df <- inData()  # extract the uploaded data
      data <- ts.df[2:ncol(ts.df)]  # extract just the data without the date
      setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table 
      ts.lag.df <- cbind(ts.df[1], data)  #  bind the shifted data with the original date 
      ts.lag.df <- na.omit(ts.lag.df)  # remove rows which contain NA values
      prob.data <- apply(ts.lag.df[, -1], 2, emp.cdf)
      lap.data <- apply(prob.data, 2, lap.quant.func)
      } else {
        prob.data <- apply(inData()[, -1], 2, emp.cdf)
        lap.data <- apply(prob.data, 2, lap.quant.func)
      }
    )
    
    ### Create the pairs plot (scatter plots) for all gauges
    ### Use a switch to set whether to use the original or standardised data based on the user selection
    output$pairs <- renderPlot({
      
      # Prevent plotting if lag table is incomplete
      validate(need(!is.na(any(unlist(input$lag$data))), ""))
      
      if (input$dataType == "declust") {
        # if data type is declustered
        validate(
          need(
            length(grep("/", inData()[1:5, 2])) == 0,
            "Unable to plot. You have selected to load time series data sampling all observations but this looks like observations sampled at peak surge events. This may cause errors so please check before continuing and refresh the MEM if you need to change the data type or the input data."
          )
        )
      }  # do not plot if the data contains "/" in the second column (this indicates that the data is time series)
      
      #  Extract the user defined label
      graphTitle = paste("Observed data", input$userDefinedBuildModelTopGraphTitle)
      
      # If the uploaded data is time series and no saved model uploaded
      if (input$dataType == "ts") {  # && !input$savedMod) { 
        # Extract the lag parameters (required so that plots on Tab 3 load if Tab 2 has not been opened)
        # (Different from the interactive plot due to inclusion/exclusion of date column)
        lagParameters_1 <- data.frame(c(0)) # create a zero data frame 
        colnames(lagParameters_1) <- "Days"
        lagParameters_2 <- hot_to_r(input$lag) # extract lag data from the lag table 
        if(is.null(lagParameters_2)) {
           lagParameters <- as.data.frame(lagParameters_1)
        } else {
          lagParameters <- as.data.frame(lagParameters_2)
        }
        
        ts.df <- inData()  # extract the data which has been uploaded
        data <- ts.df[2:ncol(ts.df)]  # extract the data without the date
        setDT(data)[,names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table
        ts.lag.df <- cbind(ts.df[1], data)  # bind the shifted data with the original date
        ts.lag.df <- na.omit(ts.lag.df)  # remove rows which contain NA values
        
        if (input$lagTick == TRUE) {  # if the lag tick box has been ticked
          sca <- switch(input$Scale,
                        "Orig" = ts.lag.df[, -1],  # use lagged data
                        "Std" = std_data())
          } else {  # if the lag tick box has not been ticked
            sca <- switch(input$Scale,
                          "Orig" = inData()[, -1], # use uploaded data
                          "Std" = std_data())
          }
      } else {
        sca <- switch(input$Scale,
                      "Orig" = inData()[, -1],  # use uploaded data
                      "Std" = std_data())
      }
      
      # Extract variable labels
        variableLabels <- tagList()
        for (i in 1:(ncol(inData()) - 1)) {
          name = colnames(inData()[i + 1])
          variableLabels[i] <- input[[paste0('value', name)]]

        }

      pairs(sca,
            pch = 20,
            col = alpha("#4E4A47", pairs_opacity),
            main = graphTitle,
            labels = variableLabels)
    })
    
    ### Create the interactive scatter plot for user-selected gauges
    output$pair_interactive <- renderPlotly({
      # Prevent plotting if lag table is incomplete
      validate(need(!is.na(any(unlist(input$lag$data))), ""))
      var1 <- input$pairOptions1  # select the first variable to plot
      var2 <-
        input$pairOptions2  # select the second variable to plot
      validate(need(!is.null(var1) &&
                      !is.null(var2), "Plot loading..."))  # show message until plots appear
      
      # Extract the user defined labels
      var1Label = input[[paste0("value", var1)]]
      var2Label = input[[paste0("value", var2)]]
      graphTitle = input$userDefinedBuildModelBottomGraphTitle
      
      ### Use a switch to set whether to use the original or standardised data based on the user selection (this is essentially a zoomed-in version of the pairs plot and can zoom in/out on this plot)
      if (input$dataType == "ts") {  # if the data uploaded is time series
        # Extract the lag parameters
        lagParameters_1 <- data.frame(c(0))  # create a zero data frame 
        colnames(lagParameters_1) <- "Days"
        lagParameters_2 <- hot_to_r(input$lag)  # extract lag data from the lag table 
        lagParameters <- rbind(lagParameters_1, lagParameters_2)  # bind the lag table together with the zero so the date column doesn't get shifted
        lagParameters <- as.data.frame(lagParameters)
        
        ts.df <- inData()  # extract the uploaded data
        data <- data.frame(ts.df)  # extract the data with the date
        setDT(data)[,names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table, doesn't shift date column 
        ts.lag.df <- cbind(ts.df[1:nrow(data),1], data)  # bind the shifted data to the original data
        colnames(ts.lag.df)[1] <- "Original_Date"
        colnames(ts.lag.df)[2] <- "Date"
        ts.lag.df$Date <-
          as.Date(ts.lag.df$Date, format = "%d/%m/%Y")  # convert the second column to date format
        ts.lag.df$Original_Date <-
          as.Date(ts.lag.df$Original_Date, format = "%d/%m/%Y")  # convert the first column to date format
        ts.lag.df <- data.frame(ts.lag.df)
        
        ### Create an original date column for each variable by subtracting the number of NA entries in each column (present from shift function earlier)
        ts.lag.df$Var1Date <- ts.lag.df$Date - sum(is.na(ts.lag.df[var1]))
        ts.lag.df$Var2Date <- ts.lag.df$Date - sum(is.na(ts.lag.df[var2]))
        
        if (input$lagTick == TRUE) {  # if the lag tick box is ticked then use the lagged data for the scale
          scaZoom <- switch(input$Scale,
                            "Orig" = ts.lag.df[, c("Date", var1, var2)],  # include date column for showing date on plot labels
                            "Std" = std_data()[, c(var1, var2)]
                            )
          } else {
            scaZoom <- switch(input$Scale,
                              "Orig" = inData()[, c("Date", var1, var2)],  # include date column for showing date on plot labels, use uploaded data
                              "Std" = std_data()[, c(var1, var2)]
            )
            }
      } else {
        scaZoom <- switch(input$Scale,
                          "Orig" = inData()[, c("Date", var1, var2)],  # include date column for showing date on plot labels
                          "Std" = std_data()[, c(var1, var2)]
        )
      }
      
      
      if (input$Scale == "Orig" && input$lagTick == FALSE) {
        # if the selected scale is 'Original' and lag is not being used, plot with date markers
        plot.df <- data.frame(scaZoom[, 1], scaZoom[, 2], scaZoom[, 3])
        colnames(plot.df) <- c("Date", "x", "y")
        plot_ly(
          plot.df,
          x =  ~ x,
          y =  ~ y,
          text = ~ Date,
          type = 'scatter',
          mode = "markers",
          marker = list(
            color = "#4E4A47",
            size = marker_size,
            opacity = marker_opacity
          )
        ) %>%
          layout(xaxis = list(title = var1Label), 
                 yaxis = list(title = var2Label),
                 title = graphTitle)
      } else if (input$Scale == "Orig" && input$lagTick == TRUE) {
        # if the selected scale is 'Original' and lag is being used, plot with date markers
        plot.df <- data.frame(scaZoom[, 1], scaZoom[, 2], scaZoom[, 3])
        colnames(plot.df) <- c("Date", "x", "y")
        plot_ly(
          plot.df, 
          x=~x, 
          y=~y, 
          text = ~paste0("(", x, ", ", y, ")", 
                         '<br>', var1Label, " Original Date: ", format(ts.lag.df$Var1Date, "%d/%m/%Y"), 
                         '<br>', var2Label, " Original Date: ", format(ts.lag.df$Var2Date, "%d/%m/%Y"), 
                         '<br>', "Lagged Date: ", format(ts.lag.df$Date, "%d/%m/%Y")), # format hover text in a readable way
          hoverinfo = 'text',  
          type = 'scatter', 
          mode = "markers",  
          marker = list(
            color = "#4E4A47", 
            size = marker_size, 
            opacity = marker_opacity
          )
        ) %>% 
          layout(xaxis = list(title = var1Label), 
                 yaxis = list(title = var2Label), 
                 title = graphTitle)
      } else if (input$Scale == "Std") {
        # if the selected scale is 'Standardised', plot without date markers
        plot.df <- data.frame(scaZoom[, 1], scaZoom[, 2])
        colnames(plot.df) <- c("x", "y")
        plot_ly(
          plot.df,
          x =  ~ x,
          y =  ~ y,
          type = 'scatter',
          mode = "markers",
          marker = list(
            color = "#4E4A47",
            size = marker_size,
            opacity = marker_opacity
          )
        ) %>%
          layout(xaxis = list(title = var1Label), 
                 yaxis = list(title = var2Label),
                 title = graphTitle)
      }
    })
      
    ### Set up the controls for the interactive scatter plot to allow users to specify which variables to plot
    output$pairControls1 <- renderUI({
      validate(
        need(input$file1 != "" || input$savedModel != "", ""),  # need either input data or a saved model loaded otherwise this is empty
        need(!is.na(any(unlist(input$lag$data))), ""))  # need complete lag table
      variables <- names(inData()[,-1])
      selectInput("pairOptions1", "", variables, selected = variables[1]) 
    })
  
    output$pairControls2 <- renderUI({
      validate(
        need(input$file1 != "" || input$savedModel != "", ""),  # need either input data or a saved model loaded otherwise this is empty
        need(!is.na(any(unlist(input$lag$data))), ""))  # need complete lag table
      variables <- names(inData()[,-1])
      selectInput("pairOptions2", "", variables, selected = variables[2])
    })
  
    ### Set up an output based on the Chi (originally Chi bar) calculation giving an indication of dependence
    output$chibar <- renderText({
      validate(need(!is.na(any(unlist(input$lag$data))), ""))  # need complete lag table
      var1 <- input$pairOptions1
      var2 <- input$pairOptions2
      validate(need(!is.null(var1) && !is.null(var2), ""))  # don't show anything until the variables have been recognised and the plots appear 
      # If "Use lagged data for analysis" box is ticked then use lagged data to estimate Chi
      # (this is pre saved model so does not need to work for the saved model)
      if (input$lagTick == TRUE) {  # if the "Use lagged data" box is ticked
        # Extract the lag parameters
        lagParameters <- hot_to_r(input$lag)
        lagParameters <- as.data.frame(lagParameters)
        ts.df <- inData()  # extract the uploaded data
        data <- ts.df[2:ncol(ts.df)]  # extract just the data without the date column
        setDT(data)[,names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table
        ts.lag.df <- cbind(ts.df[1], data)  # bind the shifted data to the original date column
        ts.lag.df <- na.omit(ts.lag.df)  # remove rows which contain NA values
        chi.est <- ext.dep.function.chi(ts.lag.df, c(var1, var2))  # Chi estimate, using lagged data
      } else {
        chi.est <- ext.dep.function.chi(inData(), c(var1, var2))  # Chi estimate, using the uploaded data
      }
      if (chi.est <= 0.6) {
        paste("The Chi estimate for ", var1, " and ", var2, " is ", round(chi.est,2), ". This value can be used in 
                  the joint probability desk study approach from the Defra / Environment Agency technical report FD2308/TR2. 
              Extreme events at ", var1, " and ", var2, " are unlikely to occur together. Please check that this is reflected in the simulated data.", sep = "")
      } else {
        paste("The Chi estimate for ", var1, " and ", var2, " is ", round(chi.est,2), ". This value can be used in 
                  the joint probability desk study approach from the Defra / Environment Agency technical report FD2308/TR2. 
              Extreme events at ", var1, " and ", var2, " are likely to occur together. Please check that this is reflected in the simulated data.", sep = "")
      }
    })
    
    
    ############################ Right-hand side panel #################################
    # Toggle off buttons if lag table is incomplete
    observe({
      toggleState("Step2model", !is.na(any(unlist(input$lag$data))), "")
    })
    
    ### Fit model - reactive on the 'Create dependence structure' button being clicked
    HTmodel <- eventReactive(input$Step2model, {
      if (input$dataType == "ts") {  # if the data is time series
        if (input$lagTick == TRUE && !input$savedMod) {  # if the "Use lagged data" box has been ticked and no saved model has been uploaded
        # Extract the lag parameters
        ts.df <- inData()  # extract the uploaded data
        data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
        setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table 
        dat <- cbind(ts.df[1], data)  # bind the shifted data with the original date column
        dat <- na.omit(dat)  # remove rows containing NA values
        dat <- dat[,-1]  # drop the date column
      } else{
        dat <- inData()[,-1]
        }
      } else {
        dat <- inData()[,-1]
        }
      ### Set the parameters based on the data type
      if (input$dataType == "ts") {
        declustered = FALSE
        thr <- input$threshold
      } else if (input$dataType == "declust") {
        declustered = TRUE
        thr <- input$thresholdDeclust
      }
      HT.model.fit(data = dat,
                   threshold = thr,
                   declustered = declustered)
    })
    
    ### Create a dummy output when model has run - can then base reactive events on this
    output$HTmodelDone <- reactive({
      return(!is.null(HTmodel()))
    })
    outputOptions(output, 'HTmodelDone', suspendWhenHidden = FALSE)
    
    ### Import a user-saved model - reactive on the saved model data existing (not null)
    HT.model.two <- eventReactive(!is.null(savedModelData()), {
      validate(need(!is.null(savedModelData()), ""))
      savedModel <- savedModelData()
      savedModel$model  # extract the dependence structure from the saved model
    })
    
    ### Check when a previously-created model has been loaded - used as a condition in the ui script
    output$HTmodelDoneTwo <- reactive({
      return(!is.null(HT.model.two()))
    })
    outputOptions(output, 'HTmodelDoneTwo', suspendWhenHidden = FALSE)
    
    ### Create a warning output if the model doesn't converge
    output$convergenceWarning <- reactive({
      if (input$lagTick == TRUE) {  # if the "Use lagged data" box is ticked
        # Extract the lag parameters
        ts.df <- inData()  # extract the uploaded data
        data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
        setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
        dat <- cbind(ts.df[1], data)  # bind the data with the date column
        dat <- na.omit(dat)  # remove rows containing NA values
        dat <- dat[,-1]  # remove the date column
      } else{
      dat <- inData()[, -1]  # extract the uploaded data
      }
      ### Set the parameters based on the data type
      if (input$dataType == "ts") {
        declustered = FALSE
        thr <- input$threshold
      } else if (input$dataType == "declust") {
        declustered = TRUE
        thr <- input$thresholdDeclust
      }
      outmessage <- warningMessage(dat, thr, declustered)
      outmessage
    })
    
    
    #################### After dependence structure has been created ####################
    
    # Provide warning messages for the number of years to simulate input
    output$eventYearsWarning <- renderText({
      if (!is.numeric(input$eventYears)) {
        "Warning: Input box should only contain numeric characters."
      } else if (input$eventYears %% 1 != 0) {
      } else if (input$eventYears > 100000) {
        "Warning: The Multivariate Event Modeller tool may not be able to simulate this many years of data. It has been tested up to 100,000 years. If you decide to proceed, please be aware that the tool may work slowly, break, or behave unexpectedly. The higher the number of years selected, the more likely it is to break. It is recommended to incrementally increase the number of years."
        "Warning: Number of years to simulate should be entered as an integer."
      } else if (input$eventYears < 1) {
        "Warning: Number of years to simulate cannot be less than one."
      }
    })
    
    ### Simulate a set of synthetic events - reactive on the 'Simulate events' button being clicked
    HT.sim <- eventReactive(input$Step2eventset, {
      if (input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked
        # Extract the lag parameters
        ts.df <- inData()  # extract the saved model
        data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
        setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
        dat <- cbind(ts.df[1], data)  # bind the lagged data with the date column
        data.use <- na.omit(dat)  # remove any rows with NA values
        data.use <- data.use[,-1]  # drop the date column
      } else {
        data.use <- inData()[,-1]
      }
      no.years.sim <- input$eventYears
      ### Select dependence structure model to use as input
      if (input$Step2model) {
        # if a model has just been created then use that
        HTmodelSelect <- HTmodel()
      } else if (!is.null(savedModelData())) {
        # else if a saved model has been loaded then use that
        HTmodelSelect <- HT.model.two()
      }
      ### Set the parameters based on the data type
      if (input$dataType == "ts") {
        declustered = FALSE
        thr <- input$threshold
      } else if (input$dataType == "declust") {
        declustered = TRUE
        thr <- input$thresholdDeclust
        no.years.data <-
          input$numYearsDeclust
      }
      HT.sim <- HT.simulation(
        results = HTmodelSelect,
        no.years.sim = no.years.sim,
        name.variables = names(data.use),
        observed.data = data.use,
        threshold = thr,
        declustered = declustered,
        no.years.data = no.years.data
      )
    })
    
    ### Create an output to identify when the simulation has run - used as a condition in the ui script
    output$HTsimDone <- reactive({
      return(!is.null(HT.sim()))
    })
    outputOptions(output, 'HTsimDone', suspendWhenHidden = FALSE)
    
    ### Import a user-saved model - reactive on the saved model data existing (not null)
    HT.sim.two <- eventReactive(!is.null(savedModelData()), {
      savedModel <- savedModelData()
      savedModel$events  # extract the event set from the saved model
    })
    
    ### If loaded a user-saved model, update the numeric input for number of years to simulate to reflect correct number
    ### of years simulated, update the project name and owner and update the data type to reflect the saved model
    observeEvent(!is.null(savedModelData()), {
      numSimYears <- HT.sim.two()$simYears
      updateNumericInput(session, "eventYears", value = numSimYears)
      projName = savedModelData()$projectName
      projOwner = savedModelData()$projectOwner
      datType = savedModelData()$dataType
      updateTextInput(session, "projectName", value = projName)
      updateTextInput(session, "projectOwner", value = projOwner)
      updateRadioButtons(session, "dataType", selected = datType)
      if (datType == "declust") {
        numYearsCoastal = savedModelData()$numYearsDeclust
        updateNumericInput(session, "numYearsDeclust", value = numYearsCoastal)
      }
    })

    
    ################# Centre panel of plots after the simulation has run - options for plotting both #################
    ########### observed (user-inputted) and simulated data on their original scale and standardised scale ###########
    
    ##### Main panel #####
    
    ### Create the pairs plot (scatter plots) for all gauges
    ### Use a switch to set whether to use the original or standardised data based on the user selection
    output$pairs_postSim <- renderPlot({
      validate(need(length(input$origSimData) > 0, "No data selected"))  # need at least one of the display options to be selected
      
      ### Set a 'Plot loading' statement until the app recognises the variables (same time as the plots appear)
      var1 <- input$pairOptions1_postSim
      var2 <- input$pairOptions2_postSim
      validate(need(!is.null(var1) &&
                      !is.null(var2), "Plot loading..."))
      
      # Extract the user defined labels
      var1Label = input[[paste0("value", var1)]]
      var2Label = input[[paste0("value", var2)]]
      
      variableLabels <- tagList()
      for (i in 1:(ncol(inData()) - 1)) {
        name = colnames(inData()[i + 1])
        variableLabels[i] <- input[[paste0('value', name)]]
        
      }

      ### Select which simulated data to use
      if (input$Step2eventset) {
        # if some has just been created then use that
        HT.simSelect <- HT.sim()
        numSimYears <- HT.simSelect$simYears
        HT.simSelect <-
          thin.data.function(HT.simSelect, input$pcThinned)  # thin the data - user selects percentage of data to view
      } else if (!is.null(savedModelData())) {
        # else if a user-saved model has been loaded then use that
        HT.simSelect <- HT.sim.two()
        numSimYears <- HT.simSelect$simYears
        HT.simSelect <-
          thin.data.function(HT.simSelect, input$pcThinned)  # thin the data
      }
      
      ### If both observed and simulated data are selected for plotting
      if (length(input$origSimData) == 2) {
        # Set title
        graphTitle <- 
          paste(
            "Observed data and",
            format(
              numSimYears,
              big.mark = ",",
              scientific = FALSE,
              trim = TRUE
            ),
            "years' worth of simulated events",
            input$userDefinedBuildModelTopGraphTitle, 
            sep = " "
          )
        # Set the observed data and add a 'Type' column
        if (input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked
          # Extract the lag parameters
          ts.df <- inData()  # extract the uploaded data
          data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
          setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table 
          dat <- cbind(ts.df[1], data)  # bind the lagged data with the original date column
          dat <- na.omit(dat)  # remove any rows that contain NA values
          origData <-
            as.data.frame(cbind(dat[, -1], rep("Observed", length(dat[, 1]))))
        } else{
        origData <-
          as.data.frame(cbind(inData()[, -1], rep("Observed", 
                                                  length(inData()[, 1]))))
        }
        colnames(origData)[ncol(origData)] <- "Type"
        # Set the simulated data, add a 'Type' column and ensure that the data are numeric (not factors)
        simData <-
          as.data.frame(cbind(HT.simSelect[["Value"]][, -1], rep("Simulated", length(HT.simSelect[["Value"]][, 1]))))
        simData[, 1:(ncol(simData) - 1)] <-
          apply(simData[, 1:(ncol(simData) - 1)], 2, function(x) {
            as.numeric(as.character(x))
          })
        colnames(simData)[ncol(simData)] <- "Type"
        # Set the observed data on a standardised scale and add a 'Type' column
        origDataStd <-
          as.data.frame(cbind(std_data(), rep("Observed", length(std_data()[, 1]))))
        colnames(origDataStd)[ncol(origDataStd)] <- "Type"
        # Set the simulated data on a standardised scale and add a 'Type' column
        simDataStd <-
          as.data.frame(cbind(HT.simSelect$Standardised, 
                              rep("Simulated", length(HT.simSelect$Standardised[, 1]))))
        colnames(simDataStd)[ncol(simDataStd)] <- "Type"
        # Combine data for original scale and standardised scale - put the simulated data first so that it plots underneath the observed
        allData <-
          rbind(simData, origData)  # all data on the original scale
        allDataStd <-
          rbind(simDataStd, origDataStd)  # all data on the standardised scale
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        sca_postSim <- switch(input$ScalePostSim,
                              "Orig" = allData,
                              "Std" = allDataStd)
        ### Add a column of colours
        cols <- character(nrow(sca_postSim))
        cols[] <- "#4E4A47"  # set all colours to be black
          cols[sca_postSim$Type == "Simulated"] <-
            "#0D9DDB"  # change the colour of the simulated data rows
            sca_postSim <-
              apply(sca_postSim, 2, function(x) {
                as.numeric(as.character(x))
              })  # ensure this is numeric data rather than factors so that the standardised plots are correct
            pairs(sca_postSim[, c(1:(ncol(sca_postSim) - 1))],
                  pch = 20,
                  col = alpha(cols, pairs_opacity),
                  main = graphTitle,
                  labels = variableLabels)  # plot all the variables (not the 'Type' column)
          ### Else if only 'Observed data' selected for plotting
        } else if (input$origSimData == "origData") {
          graphTitle = paste("Observed data", input$userDefinedBuildModelTopGraphTitle)  # set title 
          if (input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked
            # Extract the lag parameters
            ts.df <- inData()  # extract the uploaded data
            data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
            setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
            dat <- cbind(ts.df[1], data)  # bind the lagged data with the original date column
            dat <- na.omit(dat)  # remove any rows which contain NA values
            sca_postSim <- switch(input$ScalePostSim,
                                  "Orig" = dat[, -1],
                                  "Std" = std_data())
          } else{
            ### Use a switch to set whether to use the original or standardised data based on the user selection
            sca_postSim <- switch(input$ScalePostSim,
                                  "Orig" = inData()[, -1],
                                  "Std" = std_data())
            }
          pairs(
            sca_postSim,
            pch = 20,
            col = alpha("#4E4A47", pairs_opacity),
            main = graphTitle,
            labels = variableLabels
            )
        ### Else if only 'Simulated data' selected for plotting
      } else if (input$origSimData == "simData") {
        graphTitle <- 
          paste(
            format(
              numSimYears,
              big.mark = ",",
              scientific = FALSE,
              trim = TRUE
              ),
            "years' worth of simulated events", 
            input$userDefinedBuildModelTopGraphTitle, 
            sep = " "
            )
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        sca_postSim <- switch(input$ScalePostSim,
                              "Orig" = HT.simSelect[["Value"]][, -1],
                              "Std" = HT.simSelect$Standardised)
        sca_postSim <-
          apply(sca_postSim, 2, function(x) {
            as.numeric(as.character(x))
            })  # ensure this is numeric before plotting
        pairs(
          sca_postSim,
          pch = 20,
          col = alpha("#0D9DDB", pairs_opacity),
          main = graphTitle,
          labels = variableLabels
        )
        }
      })
    
    ### Create the interactive scatter plot for user-selected gauges
    output$pair_interactive_postSim <- renderPlotly({
      validate(need(length(input$origSimData) > 0, "No data selected"))  # need at least one of the display options to be selected
      
      var1 <-
        input$pairOptions1_postSim  # select the first variable to plot
      var2 <-
        input$pairOptions2_postSim  # select the second variable to plot
      validate(need(!is.null(var1) &&
                      !is.null(var2), "Plot loading..."))  # show message until plots appear
      
      # Extract the user defined labels
      var1Label = input[[paste0("value", var1)]]
      var2Label = input[[paste0("value", var2)]]
      graphTitle = input$userDefinedBuildModelBottomGraphTitle
      
      ### Select which simulated data to use
      if (input$Step2eventset) {
        # if some has just been created then use that
        HT.simSelect2 <- HT.sim()
        HT.simSelect2 <-
          thin.data.function(HT.simSelect2, input$pcThinned)  # thin the data - user selects percentage of data to view
      } else if (!is.null(savedModelData())) {
        # else if a user-saved model has been loaded then use that
        HT.simSelect2 <- HT.sim.two()
        HT.simSelect2 <-
          thin.data.function(HT.simSelect2, input$pcThinned)  # thin the data
      }
      
      ### If both observed and simulated data are selected for plotting
      if (length(input$origSimData) == 2) {
        if (input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked
          # Extract the lag parameters
          ts.df <- inData()  # extract the uploaded data
          data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
          setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table 
          dat <- cbind(ts.df[1], data)  # bind the shifted data with the original date column
          dat <- na.omit(dat)  # remove any rows with NA values
          origData <-
            as.data.frame(cbind(dat, rep("Observed", length(dat[, 1]))))
          } else {
            # Set the observed data and add a 'Type' column
            origData <-
              as.data.frame(cbind(inData(), rep("Observed", length(inData()[, 1]))))
            }
        colnames(origData)[ncol(origData)] <- "Type"
        # Set the simulated data, add a 'Type' column and ensure that the data are numeric (not factors)
        simData <-
          as.data.frame(cbind(HT.simSelect2[["Value"]], 
                              rep("Simulated", length(HT.simSelect2[["Value"]][, 1]))))
        simData[, 2:(ncol(simData) - 1)] <-
          apply(simData[, 2:(ncol(simData) - 1)], 2, function(x) {
            as.numeric(as.character(x))
          })
        colnames(simData)[ncol(simData)] <- "Type"
        # Set the observed data on a standardised scale and add a 'Type' column
        origDataStd <-
          as.data.frame(cbind(std_data(), rep("Observed", length(std_data()[, 1]))))
        colnames(origDataStd)[ncol(origDataStd)] <- "Type"
        # Set the simulated data on a standardised scale and add a 'Type' column
        simDataStd <-
          as.data.frame(cbind(HT.simSelect2$Standardised, 
                              rep("Simulated", length(HT.simSelect2$Standardised[, 1]))))
        colnames(simDataStd)[ncol(simDataStd)] <- "Type"
        # Combine data for original scale and standardised scale - put the simulated data first so that it plots underneath the observed
        allData <-
          rbind(simData, origData)  # all data on the original scale
        allData$Date[which(allData$Type == "Simulated")] <- ""  # remove dates for the simulated data
        allDataStd <-
          rbind(simDataStd, origDataStd)  # all data on the standardised scale
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        scaZoom_postSim <- switch(input$ScalePostSim,
                                  "Orig" = allData[, c("Date", var1, var2, "Type")],
                                  # include date column for showing date on plot labels
                                  "Std" = allDataStd[, c(var1, var2, "Type")])
        
        ### If 'Original' scale is selected then plot with dates
        if (input$ScalePostSim == "Orig") {
          plot.df <-
            data.frame(
              scaZoom_postSim[, 1],
              scaZoom_postSim[, 2],
              scaZoom_postSim[, 3],
              scaZoom_postSim[, 4],
              stringsAsFactors = FALSE
            )
          colnames(plot.df) <- c("Date", "x", "y", "Type")
          plot.df$Type <-
            factor(plot.df$Type, levels = c("Simulated", "Observed"))
          plot_ly(
            plot.df,
            x =  ~ x,
            y =  ~ y,
            type = 'scatter',
            mode = "markers",
            text = ~ Date,
            color = ~ Type,
            colors = c("#0D9DDB", "#4E4A47"),
            marker = list(size = marker_size, opacity = marker_opacity)
          ) %>%
            layout(
              xaxis = list(title = var1Label),
              yaxis = list(title = var2Label),
              title = graphTitle,
              legend = list(traceorder = "reversed")
            )
          
          ### Else if 'Standardised' scale is selected then plot without dates
        } else if (input$ScalePostSim == "Std") {
          plot.df <-
            data.frame(scaZoom_postSim[, 1],
                       scaZoom_postSim[, 2],
                       scaZoom_postSim[, 3],
                       stringsAsFactors = FALSE)
          colnames(plot.df) <- c("x", "y", "Type")
          plot.df$Type <-
            factor(plot.df$Type, levels = c("Simulated", "Observed"))
          plot_ly(
            plot.df,
            x =  ~ x,
            y =  ~ y,
            type = 'scatter',
            mode = "markers",
            color = ~ Type,
            colors = c("#0D9DDB", "#4E4A47"),
            marker = list(size = marker_size, opacity = marker_opacity)
          ) %>%
            layout(
              xaxis = list(title = var1Label,
                           type = "linear"),
              yaxis = list(title = var2Label,
                           type = "linear"),
              title = graphTitle,
              legend = list(traceorder = "reversed")
            )
        }          

      ### Else if only 'Observed data' selected for plotting
      } else if (input$origSimData == "origData") {
        # (A separate version for saved model is used here as easier due to the lagParameters setup)
        if (input$lagTick == TRUE && !input$savedMod) {  # if the "Use lagged data" box has been ticked and no saved model has been uploaded 
          ### Extract the lag parameters
          lagParameters_1 <- data.frame(c(0))  # create a zero data frame
          colnames(lagParameters_1) <- "Days"
          lagParameters_2 <- hot_to_r(input$lag)  # extract lag values from the lag table 
          lagParameters <- rbind(lagParameters_1, lagParameters_2)  # bind the lag table together with the zero so the date column doesn't get shifted
          lagParameters <- as.data.frame(lagParameters)
          
          ts.df <- inData()  # extract data
          data <- data.frame(ts.df)  # extract the data with the date column
          setDT(data)[,names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table, doesn't shift date column
          dat <- cbind(ts.df[1:row(data),1], data)  # bind the lagged data with the original date column
          
          colnames(dat)[1] <- "Original_Date"
          colnames(dat)[2] <- "Date"
          dat$Date <-
            as.Date(dat$Date, format = "%d/%m/%Y")  # convert the second column to date format
          dat$Original_Date <-
            as.Date(dat$Original_Date, format = "%d/%m/%Y")  # convert the first column to date format
          dat <- data.frame(dat)
          
          ### Create an original date column for each variable by subtracting the number of NA entries in each column (present from shift function earlier)
          dat$Var1Date <- dat$Date - sum(is.na(dat[var1]))
          dat$Var2Date <- dat$Date - sum(is.na(dat[var2]))
          
          ### Use a switch to set whether to use the original or standardised data based on the user selection
          scaZoom_postSim <- switch(input$ScalePostSim,
                                    "Orig" = dat[, c("Date", var1, var2)],
                                    "Std" = std_data()[, c(var1, var2)])
        } else if (input$savedMod && input$dataType == "ts" && input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked and a saved model has been uploaded
          # Extract the lag parameters
          lagParameters_1 <- data.frame(c(0)) # create a zero data frame
          colnames(lagParameters_1) <- "Days"
          lagParameters_2 <- data.frame(unlist(savedModelData()$lagTableSave)) # extract lag values from the lag table in the saved model
          colnames(lagParameters_2) <- "Days"
          lagParameters <- rbind(lagParameters_1, lagParameters_2) # bind the lag table together with the zero so the date column doesn't get shifted
          lagParameters <- as.data.frame(lagParameters)

          ts.df <- savedModelData()$userData  # extract the data from the saved model
          data <- data.frame(ts.df)  # extract the data with the date column
          setDT(data)[,names(data) := Map(shift, .SD, t(lagParameters))]  # shift the data by the amount specified in the lag table
          dat <- cbind(ts.df[1:nrow(data), 1], data)  # bind the lagged data with the original date column
          colnames(dat)[1] <- "Original_Date"
          dat <- data.frame(dat)

          colnames(dat)[2] <-
            "Date"  # set the name of the second column to "Date" so this can be used as a label on the x-axis
          dat$Date <-
            as.Date(dat$Date, format = "%d/%m/%Y")  # convert the second column to date format
          dat$Original_Date <-
            as.Date(dat$Original_Date, format = "%d/%m/%Y")  # convert the first column to date format

          ### Create an original date column for each variable by subtracting the number of NA entries in each column (present from shift function earlier)
          dat$Var1Date <- dat$Date - sum(is.na(dat[var1]))
          dat$Var2Date <- dat$Date - sum(is.na(dat[var2]))

          ### Use a switch to set whether to use the original or standardised data based on the user selection
          scaZoom_postSim <- switch(input$ScalePostSim,
                                    "Orig" = dat[, c("Date", var1, var2)],
                                    "Std" = std_data()[, c(var1, var2)])
        } else {
            ### Use a switch to set whether to use the original or standardised data based on the user selection
            scaZoom_postSim <- switch(input$ScalePostSim,
                                "Orig" = inData()[, c("Date", var1, var2)],
                                "Std" = std_data()[, c(var1, var2)])
        }
        ### If 'Original' scale and lag is selected then plot with original and lagged dates
        if (input$ScalePostSim == "Orig" && input$lagTick == TRUE) {
          plot.df <-
            data.frame(scaZoom_postSim[, 1], scaZoom_postSim[, 2], scaZoom_postSim[, 3])
          colnames(plot.df) <- c("Date", "x", "y")
          plot_ly(
            plot.df,
            x =  ~ x,
            y =  ~ y,
            type = 'scatter',
            text = ~ paste0("(", x, ", ", y, ")", 
                            '<br>', var1Label, " Original Date: ", format(dat$Var1Date, "%d/%m/%Y"), 
                            '<br>', var2Label, " Original Date: ", format(dat$Var2Date, "%d/%m/%Y"), 
                            '<br>', "Lagged Date: ", format(dat$Date, "%d/%m/%Y")), # format hover text in readable way
            hoverinfo = 'text',
            mode = "markers",
            marker = list(
              color = "#4E4A47",
              size = marker_size,
              opacity = marker_opacity
            )
          ) %>%
            layout(
              xaxis = list(title = var1Label,
                           type = "linear"),
              yaxis = list(title = var2Label,
                           type = "linear"),
              title = graphTitle
            )
          ### If 'Original' scale and no lag is selected then plot with dates
        } else if (input$ScalePostSim == "Orig" && input$lagTick == FALSE) {
            plot.df <-
              data.frame(scaZoom_postSim[, 1], scaZoom_postSim[, 2], scaZoom_postSim[, 3])
            colnames(plot.df) <- c("Date", "x", "y")
            plot_ly(
              plot.df,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              text = ~ Date,
              mode = "markers",
              marker = list(
                color = "#4E4A47",
                size = marker_size,
                opacity = marker_opacity
              )
            ) %>%
              layout(
                xaxis = list(title = var1Label,
                             type = "linear"),
                yaxis = list(title = var2Label,
                             type = "linear"),
                title = graphTitle
              )
            
          ### Else if 'Standardised' scale is selected then plot without dates
          } else if (input$ScalePostSim == "Std") {
            plot.df <- data.frame(scaZoom_postSim[, 1], scaZoom_postSim[, 2])
            colnames(plot.df) <- c("x", "y")
            plot_ly(
              plot.df,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              marker = list(
                color = "#4E4A47",
                size = marker_size,
                opacity = marker_opacity
                )
              ) %>%
              layout(
                xaxis = list(title = var1Label,
                             type = "linear"),
                yaxis = list(title = var2Label,
                             type = "linear"), 
              title = graphTitle
            )
        }
        
      ### Else if only 'Simulated data' selected for plotting
      } else if (input$origSimData == "simData") {
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        scaZoom_postSim <- switch(input$ScalePostSim,
                                  "Orig" = HT.simSelect2[["Value"]][, c(var1, var2)],
                                  "Std" = HT.simSelect2$Standardised[, c(var1, var2)])
        
        plot.df.sim <-
          data.frame(scaZoom_postSim[, 1], scaZoom_postSim[, 2])
        colnames(plot.df.sim) <- c("x", "y")
        plot_ly(
          plot.df.sim,
          x =  ~ x,
          y =  ~ y,
          type = 'scatter',
          mode = "markers",
          marker = list(
            color = "#0D9DDB",
            size = marker_size,
            opacity = marker_opacity
          )
        ) %>%
          layout(
            xaxis = list(title = var1Label,
                         type = "linear"),
            yaxis = list(title = var2Label,
                         type = "linear"),
            title = graphTitle
          )
      }
    })
    
    ### Set up the controls for the interactive scatter plot to allow users to specify which variables to plot
    output$pairControls1_postSim <- renderUI({
      validate(need(input$file1 != "" || input$savedModel != "", ""))  # need either input data or a saved model loaded otherwise this is empty
      variables <- names(inData()[,-1])
      selectInput("pairOptions1_postSim", "", variables, selected = variables[1])  
    })
      
    output$pairControls2_postSim <- renderUI({
      validate(need(input$file1 != "" || input$savedModel != "", ""))  # need either input data or a saved model loaded otherwise this is empty
      variables <- names(inData()[,-1])
      selectInput("pairOptions2_postSim", "", variables, selected = variables[2])
    })

    ### Set up an output based on the Chi (originally Chi bar) calculation giving an indication of dependence
    output$chibar2 <- renderText({
      var1 <- input$pairOptions1_postSim
      var2 <- input$pairOptions2_postSim
      validate(need(!is.null(var1) && !is.null(var2), ""))  # don't show anything until the variables have been recognised and the plots appear
      if (input$lagTick == TRUE && !input$savedMod) {  # if the "use lagged data" box is ticked and no saved model has been uploaded
        # Extract the lag parameters
        ts.df <- inData()  # extract the uploaded data
        data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column 
        setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
        dat <- cbind(ts.df[1], data)  # bind the lagged data with the original date column
        dat <- na.omit(dat)  # remove any rows with NA values
        chi.est <- ext.dep.function.chi(dat, c(var1, var2))  # Chi estimate
      } else{
        chi.est <- ext.dep.function.chi(inData(), c(var1, var2))  # Chi estimate
      }
      if (chi.est <= 0.6) {
        paste("The Chi estimate for ", var1, " and ", var2, " is ", round(chi.est,2), ". This value can be used in 
              the joint probability desk study approach from the Defra / Environment Agency technical report FD2308/TR2. 
              Extreme events at ", var1, " and ", var2, " are unlikely to occur together. Please check that this is reflected in the simulated data.", sep = "")
      }
      else{
        paste("The Chi estimate for ", var1, " and ", var2, " is ", round(chi.est,2), ". This value can be used in 
              the joint probability desk study approach from the Defra / Environment Agency technical report FD2308/TR2. 
              Extreme events at ", var1, " and ", var2, " are likely to occur together. Please check that this is reflected in the simulated data.", sep = "")
      }
    })
    
    
    ############################ Right-hand side panel #################################
    ### Check when a previously-created model has been loaded - used as a condition in the ui script
    output$HTsimDoneTwo <- reactive({
      length(HT.sim.two()) > 0
    })
    outputOptions(output, 'HTsimDoneTwo', suspendWhenHidden = FALSE)
    
    ### Once a simulated event set exists then activate the remaining tabs
    ### If just created
    observe({
      if (!is.null(HT.sim())) {
        session$sendCustomMessage('activeTabs', '4 Marginal analysis')
        session$sendCustomMessage('activeTabs', '5 Joint probability analysis')
        session$sendCustomMessage('activeTabs', '6 Export results')
      }
    })
    
    ### If loaded from a user-saved model
    observe({
      if (length(HT.sim.two()) > 0) {
        session$sendCustomMessage('activeTabs', '4 Marginal analysis')
        session$sendCustomMessage('activeTabs', '5 Joint probability analysis')
        session$sendCustomMessage('activeTabs', '6 Export results')
      }
    })
    
    ### Save data
    
    ### shinyFiles - this doesn't save anything but stops buttons needing to be double-clicked
    # Tells shinyFiles to search any available drive/folder
    roots <- getVolumes()
    shinyFileSave(input, "save", roots = roots, session = session)
    
    
    ### Set up saving functions
    expr <- function(x)
      eval(parse(text = paste0("input$", x)))
    
    ServerSideFileSaver <- function(input, output, type, session, roots, 
                                    inputHandle, inputTextBox, inputFileName, inputSaveButtonHandle, 
                                    outputFeedbackHandle, msgSuccess, msgError)
      {
      
      observeEvent(expr(inputHandle), {
        chosenDir <- choose.dir()
        updateTextInput(session = session, inputTextBox, value =  chosenDir)  # write the chosen folder/directory path to the text input box
      })
      
      ### Logic triggered by the save file button event
      observeEvent(
        expr(inputSaveButtonHandle),
        {
          destination <- expr(inputTextBox)
          saveName <- expr(inputFileName)
          
          ### Set up option to save as either a .RDS file or a .csv file
          if (type == "RDS") {
            file <- paste0(destination, "//", saveName, ".RDS")
            test <- try(saveRDS(input, file))
          } else if (type == "csv") {
            file <- paste0(destination, "//", saveName, ".csv")
            test <-
              try(write.csv(as.data.frame(input()), file, row.names = F))
          }
          
          ### Set up error message if the saving does not complete successfully
          if (class(test) == "try-error") {
            msg <- paste(msgError)
          } else {
            msg <- paste(msgSuccess, file, "at", Sys.time())
          }
          eval(parse(text = paste0(
            "output$", outputFeedbackHandle, " <- renderText(msg)"
          )))
        })
      return(NULL)
    }
    
    # Show and hide different elements depending on whether the MEM is hosted locally or on shinyapps.io
    observe({
      if (is_local == FALSE) {
        shinyjs::show("savePanel2")
        shinyjs::hide("savePanel")
      } else{
        shinyjs::hide("savePanel2")
        shinyjs::show("savePanel")
      }
    })
    
    ### Select dependence structure (model) to save
    HTmodelSelectSave <- reactive({
      if (input$Step2model) {
        # if a model has just been created then select that one
        HTmodelSelect <- HTmodel()
      } else if (!is.null(savedModelData())) {
        # else if a saved model has been loaded then select that one
        HTmodelSelect <- HT.model.two()
      }
      HTmodelSelect
    })
    
    ### Save input data, dependence structure, event set, lag parameters, project information and data type as a .RDS file
    ### When hosted locally
        ServerSideFileSaver(
          input = list(
          userData = inData(),
          model = HTmodelSelectSave(),
          events = HT.sim(),
          lagTableSave = input$lag$data,
          lagTickSave = input$lagTick,
          projectName = input$projectName,
          projectOwner = input$projectOwner,
          dataType = input$dataType,
          numYearsDeclust = input$numYearsDeclust
        ), 
        output,
        type = "RDS",
        session,
        roots,
        inputHandle = "directory",
        inputTextBox = "saveModelFolder",
        inputFileName = "saveModelName",
        inputSaveButtonHandle = "ActionButtonSaveModel",
        outputFeedbackHandle = "tO_ModelSaveFeedback",
        msgSuccess  = "Model saved to",
        msgError = "Error saving file. Check directory exists and that you have permission to write to it."
        )
     
    ### When hosted on shinyapps.io
    output$saveProject <- downloadHandler(filename <-
                                            paste("Project", "RDS", sep = "."),
                                          
                                          content <- function(file) {
                                            saveRDS(
                                              list(
                                                userData = inData(),
                                                model = HTmodelSelectSave(),
                                                events = HT.sim(),
                                                lagTableSave = input$lag$data,
                                                lagTickSave = input$lagTick,
                                                projectName = input$projectName,
                                                projectOwner = input$projectOwner,
                                                dataType = input$dataType,
                                                numYearsDeclust = input$numYearsDeclust
                                              ),
                                              file
                                            )
                                          }
                                          )

    

    
    ################################ Tab 4 - Marginal analysis ################################
    ### Set up user marginals data (including dependence structure model, updated simulated event set and marginal parameters) from a user-saved model
    HT.userMarginals.two <-
      eventReactive(!is.null(savedModelData()), {
        savedModel <- savedModelData()
        savedModel$marginalSim  # extract the marginals data
      })
    ### Create a dummy output when this exists - used as a condition in the ui script
    output$HTuserMarginalsDoneTwo <- reactive({
      length(HT.userMarginals.two()) > 0
    })
    outputOptions(output, 'HTuserMarginalsDoneTwo', suspendWhenHidden = FALSE)
    
    ### Extract the marginal parameters data if a user-saved model has been loaded
    HT.userMarginalsTable.two <-
      eventReactive(!is.null(savedModelData()), {
        savedModel <- savedModelData()
        savedModel$margParamTable
      })
    
    ### Create a dummy output when this exists - used as a condition in the ui script
    output$HTuserMarginalsTableTwo <- reactive({
      length(HT.userMarginalsTable.two()) > 0
    })
    outputOptions(output, 'HTuserMarginalsTableTwo', suspendWhenHidden = FALSE)
  
    ### Update 'Input and use you own marginal parameters' checkbox based on loaded user-saved model
    observeEvent(!is.null(savedModelData()), {
      userMarg = savedModelData()$marginals
      updateCheckboxInput(session, "showTable", value = userMarg)
    })
    
    
    ### Plot AEP curves
    output$AEPcurve <- renderPlotly({
      validate(need(length(input$marginalOptions) > 0, "No data selected"))  # need at least one of display options to be ticked
      var = input$marginalVariable  # select variable to plot
      validate(need(!is.null(var), "Plot loading..."))  # set 'plot loading' text
      
      Var.names = names(inData()[, -1])  # set variable names
      
      # Extract the user defined label 
      valueLabel = input$userDefinedValueLabel
      graphTitle = paste(input$userDefinedMarginalAnalysisGraphTitle, var)
      
      ### Set up data
      
      ### Select which simulated data to use
      if (input$Step2eventset) {
        # if some has just been created then use that
        HT.simSelect <- HT.sim()
      } else if (!is.null(savedModelData())) {
        # else if a user-saved model has been loaded then use that
        HT.simSelect <- HT.sim.two()
      }
      
      ### Automatically-determined marginals
      aep.eval <- seq(0.5, 10 ^ -5, length.out = 10 ^ 4)
      AEPlevelDF <-
        AEP.Level.Calc.GP(aep = aep.eval,
                          parameters = HT.simSelect$GPDParameters,
                          Var.names)
      
      ### User-supplied marginals
      # If the 'Input and use your own marginal parameters' box is checked
      if (input$showTable == TRUE) {
        ### If no values have been entered into the marginals table and the loaded data marginals table is null then set default values in the table
        if (is.null(input$userMarginals) &&
            is.null(HT.userMarginalsTable.two())) {
          userMarginalVals <-
            data.frame(
              Location = rep(0, length(Var.names)),
              Scale = rep(1, length(Var.names)),
              Shape = rep(0, length(Var.names)),
              row.names = Var.names,
              stringsAsFactors = FALSE
            )
          
          
          ### Else if values have been entered into the marginals table then read those in to plot
        } else if (!is.null(input$userMarginals$changes$changes)) {
          userMarginalVals <- hot_to_r(input$userMarginals)
          
          # Update the userMarginalVals row headers so format is correct for glog.quant
          userMarginalRowHeaders <-
            c()  # set up empty vector for row headers (input data column headers excluding date)
          numUserMarginalRowHeaders <-
            length(input$userMarginals$params$rowHeaders)  # number of headers
          userMarginalRowHeaders[1:numUserMarginalRowHeaders] <-
            input$userMarginals$params$rowHeaders[1:numUserMarginalRowHeaders]
          rownames(userMarginalVals) <- userMarginalRowHeaders
          userAEPLevelDF <- glog.quant(aep.eval, userMarginalVals)
          
          ### Else if no values have been entered into the marginals table but saved marginals have been loaded then extract those
        } else if (is.null(input$userMarginals$changes$changes) &&
                   !is.null(HT.userMarginalsTable.two())) {
          marginalsTableData <-
            HT.userMarginalsTable.two()$data  # extract the relevant data
          Locs <- c()
          Sc <- c()
          Sh <- c()
          # set up empty location, scale and shape parameters
          for (i in 1:length(Var.names)) {
            # loop through the variables and extract the parameters for each
            Locs[i] <- marginalsTableData[[i]][[1]]
            Sc[i] <- marginalsTableData[[i]][[2]]
            Sh[i] <- marginalsTableData[[i]][[3]]
          }
          userMarginalVals <-
            data.frame(
              Location = Locs,
              Scale = Sc,
              Shape = Sh,
              stringsAsFactors = FALSE
            )  # combine parameters into one data frame
          row.names(userMarginalVals) <- Var.names
          userAEPLevelDF <- glog.quant(aep.eval, userMarginalVals)
        }
      }
      
      ### Plotting
      
      ### If both display options are selected and user marginal parameters exist plot both data sets
      
      if (length(input$marginalOptions) == 2 &&
          exists("userAEPLevelDF")) {
        allPlot <-
          data.frame(AEPlevelDF[, which(colnames(AEPlevelDF) == var)],
                     userAEPLevelDF[, which(colnames(userAEPLevelDF) == var)],
                     aep.eval * 100)
        colnames(allPlot) <- c("appLevel", "userLevel", "AEP")
        
        plot_ly(
          allPlot,
          x = ~ appLevel,
          y = ~ AEP,
          type = 'scatter',
          name = "Auto",
          mode = "lines",
          line = list(color = "#6E273D")
        ) %>%
          layout(
            xaxis = list(title = valueLabel, type = "log"),
            yaxis = list(title = "AEP (%)"),
            title = graphTitle
          ) %>%
          add_trace(
            x = ~ userLevel,
            y = ~ AEP,
            name = "User",
            mode = "lines",
            line = list(color = "#88B29F")
          )
        ### Else if 'Auto' is selected or both are selected but user marginals do not exist then just plot the automatically-determined marginals
      } else if (input$marginalOptions == "appMA" ||
                 (length(input$marginalOptions) == 2 &&
                  exists("userAEPLevelDF") == FALSE)) {
        varPlot <-
          data.frame(AEPlevelDF[, which(colnames(AEPlevelDF) == var)], aep.eval * 100)
        colnames(varPlot) <- c("appLevel", "AEP")
        plot_ly(
          varPlot,
          x =  ~ appLevel,
          y =  ~ AEP,
          type = 'scatter',
          name = "Auto",
          mode = "lines",
          line = list(color = "#6E273D")
        ) %>%
          layout(
            xaxis = list(title = valueLabel, type = "log"),
            yaxis = list(title = "AEP (%)"),
            title = graphTitle,
            showlegend = TRUE
          )
        ### Else if 'User' is selected but user marginals do not exist then leave plot empty
      } else if (input$marginalOptions == "userMA" &&
                 exists("userAEPLevelDF") == FALSE) {
        plotly_empty()
        ### Else if 'User' is selected and user marginals exist then plot the user-supplied marginals AEP curve
      } else if (input$marginalOptions == "userMA" &&
                 exists("userAEPLevelDF")) {
        userLines <-
          data.frame(userAEPLevelDF[, which(colnames(userAEPLevelDF) == var)], aep.eval * 100)
        colnames(userLines) <- c("userLevel", "AEP")
        
        plot_ly(
          userLines,
          x =  ~ userLevel,
          y =  ~ AEP,
          type = 'scatter',
          mode = "lines",
          line = list(color = "#88B29F")
        ) %>%
          layout(
            xaxis = list(title = valueLabel, type = "log"),
            yaxis = list(title = "AEP (%)"),
            title = graphTitle
          )
      }
    })
    
    ### Set up the control for the AEP curves to allow users to specify which variable to plot
    output$marginalVariableControl <- renderUI({
      validate(
        need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
      )
      variables <- names(inData()[,-1])  # extract variable names
        selectInput("marginalVariable", h4("Select variable to plot"), variables, selected = variables[1])
    })
    
    output$marginalOptionsControl <- renderUI({
      if (input$dataType == "ts") {
        allchoices = list(
          "Automatically-determined marginals (Auto)" = "appMA",
          "User-supplied marginals (User)" = "userMA"
        )
        checkboxGroupInput(
          "marginalOptions",
          label = h4("Display options"),
          choices = allchoices,
          selected = "appMA"
        )
      } else if (input$dataType == "declust") {
        allchoices = list("User-supplied marginals (User)" = "userMA")
        checkboxGroupInput("marginalOptions",
                           label = h4("Display options"),
                           choices = allchoices)
      }
    })
    
    ### Create the marginal parameters table
    output$userMarginals <- renderRHandsontable({
      Var.names <- names(inData()[, -1])  # extract variable names
      ### If an event set has just been simulated or a saved model has been loaded without a marginals table then create a default table
      if (input$Step2eventset ||
          (!is.null(savedModelData()) &&
           is.null(HT.userMarginalsTable.two()))) {
        marginalsTable <-
          data.frame(
            Location = rep(0, length(Var.names)),
            Scale = rep(1, length(Var.names)),
            Shape = rep(0, length(Var.names)),
            stringsAsFactors = FALSE
          )
        ### Else if a saved model has been loaded containing a marginals table then restore that data to the table
      } else if (!is.null(savedModelData()) &&
                 !is.null(HT.userMarginalsTable.two())) {
        marginalsTableData <- HT.userMarginalsTable.two()$data
        Locs <- c()
        Sc <- c()
        Sh <- c()
        
        for (i in 1:length(Var.names)) {
          # extract the parameter values for each variable
          Locs[i] <- marginalsTableData[[i]][[1]]
          Sc[i] <- marginalsTableData[[i]][[2]]
          Sh[i] <- marginalsTableData[[i]][[3]]
        }
        marginalsTable <-
          data.frame(
            Location = Locs,
            Scale = Sc,
            Shape = Sh,
            stringsAsFactors = FALSE
          )  # create a data frame containing the variables
      }
      
      ### Create the table
      rht = rhandsontable(
        marginalsTable,
        readOnly = FALSE,
        rowHeaders = Var.names,
        rowHeaderWidth = 150,
        selectCallback = TRUE,
        digits = 4,
        col_highlight = 1
      ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric(cols = "Location", allowInvalid = FALSE) %>%
        hot_validate_numeric(cols = "Scale", min = 0.000001) %>%
        hot_validate_numeric(cols = "Shape",
                             min = -0.999999,
                             max = 0.999999)
      rht  # return the table
    })
    
    ### Transform the simulation outputs to be consistent with the user marginal parameters
    ### Reactive on the 'Update simulation outputs' button being clicked
    HTuserMarginals <- eventReactive(input$useUserMarginals, {
      validate(need(!is.null(input$userMarginals$changes$changes), ""))  # user marginals must exist
      
      ### Set which dependence structure model to use
      if (input$Step2model) {
        # if one has just been created then use that
        HTmodelSelect <- HTmodel()
      } else if (!is.null(savedModelData())) {
        # else if a user-saved model has been loaded then use that
        HTmodelSelect <- HT.model.two()
      }
      ### Select which simulated data to use
      if (input$Step2eventset) {
        # if some has just been created then use that
        HT.simSelect <- HT.sim()
        numSimYears <-
          HT.simSelect$simYears  # extract the number of years simulated
      } else if (!is.null(savedModelData())) {
        # else if a user-saved model has been loaded then use that
        HT.simSelect <- HT.sim.two()
        numSimYears <-
          HT.simSelect$simYears  # extract the number of years simulated
      }
      
      ### Extract the marginal parameters
      marginalParameters <- hot_to_r(input$userMarginals)
      marginalParameters <- as.data.frame(marginalParameters)
      
      ### Set parameters depending on data type
      if (input$dataType == "ts") {
        declustered = FALSE
        thr <- input$threshold
      } else if (input$dataType == "declust") {
        declustered = TRUE
        thr <- input$thresholdDeclust
        no.years.data <- input$numYearsDeclust
      }
      
      HTuserMarginals <-
        HT.userMarginals(
          HTmodelSelect,
          HT.simSelect,
          inData()[, -1],
          marginalParameters,
          no.years.data,
          thr,
          declustered
        )
      
    })
    
    ### Create a dummy output when this has run (once the output is not null) - used as a condition in the ui script
    output$HTuserMarginalsDone <- reactive({
      return(!is.null(HTuserMarginals()))
    })
    outputOptions(output, 'HTuserMarginalsDone', suspendWhenHidden = FALSE)
    
    ### Save the information on the marginals tab
    # Show and hide different elements depending on whether the MEM is hosted locally or on shinyapps.io
    observe({
      if (is_local == FALSE) {
        shinyjs::show("savePanelMarginal2")
        shinyjs::hide("savePanelMarginal")
      } else{
        shinyjs::hide("savePanelMarginal2")
        shinyjs::show("savePanelMarginal")
      }
    })
    
    ### Select Tab 3 - Build model simulation outputs to save
    HTsimSelectSave <- reactive({
      if (input$Step2model) {
        # if some have just been created then select that
        HTsimSelect <- HT.sim()
      } else if (!is.null(savedModelData())) {
        # else if a saved model has been loaded then select that
        HTsimSelect <- HT.sim.two()
      }
      HTsimSelect
    })
    
    ### Save same as before as well as whether the 'Input and use your own marginal parameters' checkbox is ticked,
    ### the user marginals table and the updated simulation outputs as a .RDS file, Lag table values dependent on whether a saved model has been uploaded
    ### When hosted locally
    observe({
      if (is.null(input$savedModel) && !is.null(input$file1)) {
        ServerSideFileSaver(
          input = list(
            userData = inData(),
            model = HTmodelSelectSave(),
            events = HTsimSelectSave(),
            lagTableSave = input$lag$data,
            lagTickSave = input$lagTick,
            projectName = input$projectName,
            projectOwner = input$projectOwner,
            dataType = input$dataType,
            marginals = input$showTable,
            margParamTable = input$userMarginals,
            marginalSim = HTuserMarginals(),
            numYearsDeclust = input$numYearsDeclust
            ),
          output,
          type = "RDS",
          session,
          roots,
          inputHandle = "directory3",
          inputTextBox = "saveModelFolder3",
          inputFileName = "saveModelName3",
          inputSaveButtonHandle = "ActionButtonSaveModel3",
          outputFeedbackHandle = "tO_ModelSaveFeedback3",
          msgSuccess  = "Model saved to",
          msgError = "Error saving file. Check directory exists and that you have permission to write to it."
        )
        } else if (!is.null(input$savedModel)) {
          ServerSideFileSaver(
            input = list(
            userData = inData(),
            model = HTmodelSelectSave(),
            events = HTsimSelectSave(),
            lagTableSave = savedModelData()$lagTableSave,
            lagTickSave = input$lagTick,
            projectName = savedModelData()$projectName,
            projectOwner = savedModelData()$projectOwner,
            dataType = input$dataType,
            marginals = input$showTable,
            margParamTable = input$userMarginals,
            marginalSim = HTuserMarginals(),
            numYearsDeclust = input$numYearsDeclust
            ),
            output,
            type = "RDS",
            session,
            roots,
            inputHandle = "directory3",
            inputTextBox = "saveModelFolder3",
            inputFileName = "saveModelName3",
            inputSaveButtonHandle = "ActionButtonSaveModel3",
            outputFeedbackHandle = "tO_ModelSaveFeedback3",
            msgSuccess  = "Model saved to",
            msgError = "Error saving file. Check directory exists and that you have permission to write to it."
          )
          }
      })
      
    ### When hosted on shinyapps.io
    observe({
    if (is.null(input$savedModel) && !is.null(input$file1)) {
    output$saveProject2 <- downloadHandler(filename <-
                                             paste("Project2", "RDS", sep = "."),
                                           
                                           content <- function(file) {
                                             saveRDS(
                                               list(
                                                 userData = inData(),
                                                 model = HTmodelSelectSave(),
                                                 events = HTsimSelectSave(),
                                                 lagTableSave = input$lag$data,
                                                 lagTickSave = input$lagTick,
                                                 projectName = input$projectName,
                                                 projectOwner = input$projectOwner,
                                                 dataType = input$dataType,
                                                 marginals = input$showTable,
                                                 margParamTable = input$userMarginals,
                                                 marginalSim = HTuserMarginals(),
                                                 numYearsDeclust = input$numYearsDeclust
                                               ),
                                               file
                                             )
                                           }
    )
                                             } else if (!is.null(input$savedModel)) {
    output$saveProject2 <- downloadHandler(filename <-
                                             paste("Project2", "RDS", sep = "."),
                                                                                      
                                              content <- function(file) {
                                               saveRDS(
                                                 list(
                                                   userData = inData(),
                                                   model = HTmodelSelectSave(),
                                                   events = HTsimSelectSave(),
                                                   lagTableSave = savedModelData()$lagTableSave,
                                                   lagTickSave = input$lagTick,
                                                   projectName = savedModelData()$projectName,
                                                   projectOwner = savedModelData()$projectOwner,
                                                   dataType = input$dataType,
                                                   marginals = input$showTable,
                                                   margParamTable = input$userMarginals,
                                                   marginalSim = HTuserMarginals(),
                                                   numYearsDeclust = input$numYearsDeclust
                                                 ),
                                                 file
                                               )
                                              }
    )
                                           }
    })
    
    
    ################################# Tab 5 - Joint probability analysis #################################
    
    ##### Main panel #####
    
    ### Extract the 'Value' column from the joint probability table
    ### This will be used to plot these as lines and is reactive on the table changing
    varLines <- reactive({
      Var.names = names(inData()[,-1])  # extract variable names
      ### If the joint probability table exists, take values from there
      if (!is.null(input$hot)) {
        dt = JPdf$value
        eval(parse(text = paste0("t(dt$", Value, ")")))  # extract the 'Value' column
        ### Otherwise set default values by creating the default table
      } else {
        eval(parse(
          text = paste0(
            "dt = data.frame('AEP (%)' = rep(0, length(Var.names())), ",
            Value,
            " = rep(0, length(Var.names())), stringsAsFactors = FALSE, check.names = FALSE)"
          )
        ))
        row.names(dt) <- Var.names
        eval(parse(text = paste0("t(dt$", Value, ")")))  # extract the 'Value' column
      }
    })
    
    ### Plotting (similar to 'Build model' tab with additional event value lines and option to disable automatic plot updating)
    
    ### Create the pairs plot (scatter plots) for all gauges
    output$pairs_postSim_step3 <- renderPlot({
      validate(need(length(input$origSimData_step3) > 0, "No data selected"))  # need at least one of the display options to be selected
      
      ### Set a 'Plot loading' statement until the app recognises the variables (same time as the plots appear)
      ### Stops R warning message appearing while plot is loading
      var1 <- input$pairOptions1_postSim_step3
      var2 <- input$pairOptions2_postSim_step3
      validate(need(!is.null(var1) &&
                      !is.null(var2), "Plot loading..."))
      
      Var.names = names(inData()[, -1])  # extract variable names
      
      variableLabels <- tagList()
      for (i in 1:(ncol(inData()) - 1)) {
        name = colnames(inData()[i + 1])
        variableLabels[i] <- input[[paste0('value', name)]]
      }

      
      ### Select simulation data dependent on whether the 'Input and use your own marginal parameters' box is ticked on the 'Marginal analysis' tab
      ### and based on whether a simulation has just been run or whether a user-saved model has been loaded
      if (input$showTable == FALSE) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
          numSimYears <-
            HT.simSelect$simYears  # extract number of years of data simulated
          HT.simSelect <-
            thin.data.function(HT.simSelect, input$pcThinned_step3)  # thin the data
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
          numSimYears <-
            HT.simSelect$simYears  # extract number of years of data simulated
          HT.simSelect <-
            thin.data.function(HT.simSelect, input$pcThinned_step3)  # thin the data
        }
      } else if (input$showTable == TRUE) {
        # if using user-supplied marginals
        ### If a simulation has just been updated then use the simulation outputs that have just been calculated
        if (input$useUserMarginals) {
          HT.simSelect <- HTuserMarginals()
          numSimYears <-
            HT.simSelect$simYears  # extract number of years of data simulated
          HT.simSelect <-
            thin.data.function(HT.simSelect, input$pcThinned_step3)  # thin the data
          ### Else if a saved model has been loaded with updated simulation outputs then use those
        } else if (!is.null(savedModelData()) &&
                   length(HT.userMarginals.two()) > 0) {
          HT.simSelect <- HT.userMarginals.two()
          numSimYears <-
            HT.simSelect$simYears  # extract number of years of data simulated
          HT.simSelect <-
            thin.data.function(HT.simSelect, input$pcThinned_step3)  # thin the data
        }
      }
      
      ### Select which dependence structure model to use
      if (input$Step2model) {
        # if one has just been created then select that
        HTmodelSelect <- HTmodel()
      } else if (!is.null(savedModelData())) {
        # else if a saved model has been loaded then select that
        HTmodelSelect <- HT.model.two()
      }
      
      ### If both observed and simulated data are selected for plotting
      if (length(input$origSimData_step3) == 2) {
        ### Set plot title based on whether auto or user marginals are selected
        if (input$showTable == FALSE) { 
          graphTitle <-
            paste(
              "Observed data and",
              format(
                numSimYears,
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
              ),
              "years' worth of simulated events \n with automatically-determined marginals",
              input$userDefinedJointProbabilityTopGraphTitle, 
              sep = " "
            )
        } else if (input$showTable == TRUE) {
          graphTitle <-
            paste(
              "Observed data and",
              format(
                numSimYears,
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
              ),
              "years' worth of simulated events \n with user-supplied marginals",
              input$userDefinedJointProbabilityTopGraphTitle,
              sep = " "
            )
        }
        if (input$lagTick == TRUE) {  # if the lag tick box has been ticked
          # Extract the lag parameters
          ts.df <- inData()  # extract the uploaded data
          data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
          setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
          dat <- cbind(ts.df[1], data)  # bind the shifted data with the original date column
          dat <- na.omit(dat)  # remove any rows with NA values
          # Set the observed data and add a 'Type' column
          origData <-
            as.data.frame(cbind(dat[, -1], rep("Observed", length(dat[, 1]))))
        } else{
        # Set the observed data and add a 'Type' column
        origData <-
          as.data.frame(cbind(inData()[, -1], rep("Observed", length(inData()[, 1]))))
        }
        colnames(origData)[ncol(origData)] <- "Type"
        # Set the simulated data, add a 'Type' column and ensure that the data are numeric (not factors)
        simData <-
          as.data.frame(cbind(HT.simSelect[["Value"]][, -1], rep("Simulated", length(HT.simSelect[["Value"]][, 1]))))
        simData[, 1:(ncol(simData) - 1)] <-
          apply(simData[, 1:(ncol(simData) - 1)], 2, function(x) {
            as.numeric(as.character(x))
          })
        colnames(simData)[ncol(simData)] <- "Type"
        # Set the observed data on a standardised scale and add a 'Type' column
        origDataStd <-
          as.data.frame(cbind(std_data(), rep("Observed", length(std_data()[, 1]))))
        colnames(origDataStd)[ncol(origDataStd)] <- "Type"
        # Set the simulated data on a standardised scale and add a 'Type' column
        simDataStd <-
          as.data.frame(cbind(HT.simSelect$Standardised, 
                              rep("Simulated", length(HT.simSelect$Standardised[, 1])
          )))
        colnames(simDataStd)[ncol(simDataStd)] <- "Type"
        # Combine data for original scale and standardised scale - put the simulated data first so that it plots underneath the observed
        allData <-
          rbind(simData, origData)  # all data on the original scale
        allDataStd <-
          rbind(simDataStd, origDataStd)  # all data on the standardised scale
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        sca_postSim <- switch(input$ScalePostSim_step3,
                              "Orig" = allData,
                              "Std" = allDataStd)
        ### If 'Original' scale is selected then plot with the event values added as lines and remove simulated data below the marginal thresholds
        if (input$ScalePostSim_step3 == "Orig") {
          a.sim <-
            data.frame(sca_postSim[which(sca_postSim$Type == "Simulated"), c(1:(ncol(sca_postSim) -
                                                                                  1))])  # simulated data
          a.obs <-
            data.frame(sca_postSim[which(sca_postSim$Type == "Observed"), c(1:(ncol(sca_postSim) -
                                                                                 1))])  # observed data
          a <-
            data.frame(sca_postSim[, c(1:(ncol(sca_postSim) - 1))])  # all data
          ### Find the marginal threshold
          marg.thresh.vec <- rep(NA, ncol(inData()[, -1]))
          for (i in 1:length(marg.thresh.vec)) {
            marg.thresh.vec[i] <-
              as.numeric(HTmodelSelect$MarginalParameter[[i]]$threshold)
          }
          ### Set a grid index
          idx <-
            subset(expand.grid(y = 1:length(a), x = 1:length(a)), x != y)
          ### Create a function for plotting the data for each pair of variables in separate panels based on the grid index above
          ### If 'Disable automatic plot updating' is not ticked
          if (!input$pausePlots) {
            i <- 1
            fn.panel <- function(x, y, ...) {
              marg.thresh.select <-
                c(marg.thresh.vec[idx[i, 'x']], marg.thresh.vec[idx[i, 'y']])
              data.comb <-
                cbind(a.sim[, idx[i, 'x']], a.sim[, idx[i, 'y']])
              data.obs.selection <-
                cbind(a.obs[, idx[i, 'x']], a.obs[, idx[i, 'y']])
              # Select those points in the joint tail region
              data.comb.exc <-
                data.comb[c(data.comb[, 1] >= marg.thresh.select[1] |
                              data.comb[, 2] >= marg.thresh.select[2]), ]
              obs.sim.comb <- rbind(data.comb.exc, data.obs.selection)
              # Create a column of colours
              cols <- character(nrow(sca_postSim))
              cols[] <- "#0D9DDB"
                cols[-c(1:nrow(data.comb.exc))] <- "#4E4A47"
                  points(obs.sim.comb,

                         cex = 1,
                         pch = 20,
                         col = alpha(cols, pairs_opacity))
                  # Visualise the non-extreme region and the joint tail region
                  polygon(
                    c(-100, marg.thresh.select[1], marg.thresh.select[1], -100),
                    c(-100, -100, marg.thresh.select[2], marg.thresh.select[2]),
                    col = "#A9A19C"
                  )  # grey box
                  abline(h = varLines()[, idx[i, 'y']], v = varLines()[, idx[i, 'x']])  # event values as lines
                  box()
                  i <<- i + 1
            }
            ### Else if 'Disable automatic plot updating' is ticked then create a dummy data set to plot containing
            ### the maximum and minimum values from above
          } else{
            i <- 1
            fn.panel <- function(x, y, ...) {
              dummy.data <-
                rbind(apply(a[, c(idx[i, 'x'], idx[i, 'y'])], 2, min), apply(a[, c(idx[i, 'x'], idx[i, 'y'])], 2, max))
              points(
                dummy.data,
                cex = 1,
                pch = 20,
                col = "#A9A19C"
              )
              # Add the event values as lines
              abline(h = varLines()[, idx[i, 'y']], v = varLines()[, idx[i, 'x']])
              box()
              i <<- i + 1
            }
          }

          
          # Create the pairs plot (scatter plots) for all variables
          pairs(a, panel = fn.panel, main = graphTitle, labels = variableLabels)
          
          ### Else if 'Standardised' scale is selected then plot without event values but remove the simulated data below the marginal thresholds
        } else if (input$ScalePostSim_step3 == "Std") {
          ### Ensure the data are numeric not factors
          sca_postSim[, 1:(ncol(sca_postSim) - 1)] <-
            apply(sca_postSim[, 1:(ncol(sca_postSim) - 1)], 2, function(x) {
              as.numeric(as.character(x))
            })
          b.sim <-
            data.frame(sca_postSim[which(sca_postSim$Type == "Simulated"), c(1:(ncol(sca_postSim) -
                                                                                  1))])  # simulated data
          b.obs <-
            data.frame(sca_postSim[which(sca_postSim$Type == "Observed"), c(1:(ncol(sca_postSim) -
                                                                                 1))])  # observed data
          b <-
            data.frame(sca_postSim[, c(1:(ncol(sca_postSim) - 1))])  # all data
          
          # Dependence thresholds
          dep.thresh.vec <- unlist(HTmodelSelect$DepThreshold)
          
          # Set a grid index for all pairs of variables
          idx <-
            subset(expand.grid(y = 1:length(b), x = 1:length(b)), x != y)
          i <- 1
          ### Create a function for plotting the data for each pair of variables in separate panels based on the grid index above
          # Plot points within each panel
          fn.panel <- function(x, y, ...) {
            dep.thresh.select <-
              c(dep.thresh.vec[idx[i, 'x']], dep.thresh.vec[idx[i, 'y']])
            data.comb <- cbind(b.sim[, idx[i, 'x']], b.sim[, idx[i, 'y']])
            data.obs.selection <-
              cbind(b.obs[, idx[i, 'x']], b.obs[, idx[i, 'y']])
            # Select those points in the joint tail region
            data.comb.exc <-
              data.comb[c(data.comb[, 1] >= dep.thresh.select[1] |
                            data.comb[, 2] >= dep.thresh.select[2]), ]
            obs.sim.comb <- rbind(data.comb.exc, data.obs.selection)
            # Add a column of colours
            cols <- character(nrow(sca_postSim))
            cols[] <- "#0D9DDB"
              cols[-c(1:nrow(data.comb.exc))] <- "#4E4A47"
                points(obs.sim.comb,
                       cex = 1,
                       pch = 20,
                       col = alpha(cols, pairs_opacity))
                # Visualise the non-extreme region
                polygon(
                  c(-100, dep.thresh.select[1], dep.thresh.select[1], -100),
                  c(-100, -100, dep.thresh.select[2], dep.thresh.select[2]),
                  col = "#A9A19C"
                )
                box()
                i <<- i + 1
          }
          # Create the pairs plot (scatter plots) for all variables
          pairs(b, panel = fn.panel, main = graphTitle, labels = variableLabels)
        }
        ### Else if observed data selected for plotting
      } else if (input$origSimData_step3 == "origData") {
        graphTitle <- paste("Observed data", input$userDefinedJointProbabilityTopGraphTitle)  
        if (input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked
          # Extract the lag parameters
          ts.df <- inData()  # extract the uploaded data
          data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
          setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
          dat <- cbind(ts.df[1], data)  # bind the lagged data with the original date column
          dat <- na.omit(dat)  # remove any rows with NA values
          ### Use a switch to set whether to use the original or standardised data based on the user selection
          sca <- switch(input$ScalePostSim_step3,
                        "Orig" = dat[, -1],
                        "Std" = std_data())
        } else{
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        sca <- switch(input$ScalePostSim_step3,
                      "Orig" = inData()[, -1],
                      "Std" = std_data())
        }
        ### If 'Original' scale selected
        if (input$ScalePostSim_step3 == "Orig") {
          a <- sca  # observed data
          # Set a grid index
          idx <-
            subset(expand.grid(y = 1:length(a), x = 1:length(a)), x != y)
          ### If 'Disable automatic plot updating' is not ticked
          if (!input$pausePlots) {
            i <- 1
            # Create function for plotting data in separate panels
            fn.panel <- function(x, y, ...) {
              points(a[, idx[i, 'x']],
                     a[, idx[i, 'y']],
                     cex = 1,
                     pch = 20,
                     col = alpha("#4E4A47", pairs_opacity))
              abline(h = varLines()[, idx[i, 'y']], v = varLines()[, idx[i, 'x']])
              i <<- i + 1
            }
          }
          ### Else if 'Disable automatic plot updating' is ticked then create a dummy data set to plot which contains
          ### the maximum and minimum values from above
          else{
            i <- 1
            # Create function for plotting data in separate panels
            fn.panel <- function(x, y, ...) {
              dummy.data <-
                rbind(apply(a[, c(idx[i, 'x'], idx[i, 'y'])], 2, min), apply(a[, c(idx[i, 'x'], idx[i, 'y'])], 2, max))
              points(
                dummy.data,
                cex = 1,
                pch = 20,
                col = "#A9A19C"
              )
              abline(h = varLines()[, idx[i, 'y']], v = varLines()[, idx[i, 'x']])
              box()
              i <<- i + 1
            }
          }
          # Create the pairs plot (scatter plots) for all variables
          pairs(sca, panel = fn.panel, main = graphTitle, labels = variableLabels)
          ### Else if 'Standardised' scale selected plot data without event values as lines
        } else if (input$ScalePostSim_step3 == "Std") {
          pairs(sca,
                pch = 20,
                col = alpha("#4E4A47", pairs_opacity),
                main = graphTitle, labels = variableLabels)
        }
        ### Else if simulated data selected for plotting
      } else if (input$origSimData_step3 == "simData") {
        ### Set titles dependent on whether user-supplied marginals are being used
        if (input$showTable == FALSE) {
          graphTitle <- 
            paste(
              format(
                numSimYears,
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
              ),
              "years' worth of simulated events \n with automatically-determined marginals",
              input$userDefinedJointProbabilityTopGraphTitle, 
              sep = " "
            )
        } else if (input$showTable == TRUE) {
          graphTitle <- 
            paste(
              format(
                numSimYears,
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
              ),
              "years' worth of simulated events \n with user-supplied marginals",
              input$userDefinedJointProbabilityTopGraphTitle, 
              sep = " "
            )
        }
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        sca_postSim <- switch(input$ScalePostSim_step3,
                              "Orig" = HT.simSelect[["Value"]][, -1],
                              "Std" = HT.simSelect$Standardised)
        # Ensure data are numeric rather than factors
        sca_postSim <-
          apply(sca_postSim, 2, function(x) {
            as.numeric(as.character(x))
          })
        ### If 'Original' scale selected
        if (input$ScalePostSim_step3 == "Orig") {
          a <- data.frame(sca_postSim)  # simulated data
          
          # Access values of the threshold from the model fitting code
          marg.thresh.vec <- rep(NA, ncol(inData()[, -1]))
          for (i in 1:length(marg.thresh.vec)) {
            marg.thresh.vec[i] <-
              as.numeric(HTmodelSelect$MarginalParameter[[i]]$threshold)
          }
          # Set a grid index
          idx <-
            subset(expand.grid(y = 1:length(a), x = 1:length(a)), x != y)
          ### If 'Disable automatic plot updating' is not ticked
          if (!input$pausePlots) {
            i <- 1
            # Create function for plotting data in separate panels
            fn.panel <- function(x, y, ...) {
              marg.thresh.select <-
                c(marg.thresh.vec[idx[i, 'x']], marg.thresh.vec[idx[i, 'y']])
              # Select the relevant variables in the simulated data set
              data.comb <- cbind(a[, idx[i, 'x']], a[, idx[i, 'y']])
              # Select those points in the joint tail region
              data.comb.exc <-
                data.comb[c(data.comb[, 1] >= marg.thresh.select[1] |
                              data.comb[, 2] >= marg.thresh.select[2]), ]
              points(
                data.comb.exc,
                cex = 1,
                pch = 20,
                col = alpha("#0D9DDB", pairs_opacity)
              )
              # Visualise the non-extreme region (grey box) and the joint tail region
              polygon(
                c(-100, marg.thresh.select[1], marg.thresh.select[1], -100),
                c(-100, -100, marg.thresh.select[2], marg.thresh.select[2]),
                col = "#A9A19C"
              )
              abline(h = varLines()[, idx[i, 'y']], v = varLines()[, idx[i, 'x']])  # add event values as lines
              box()
              i <<- i + 1
            }
          }
          ### Else if 'Disable automatic plot updating' is ticked then create a dummy data set to plot which contains
          ### the maximum and minimum values from above
          else{
            i <- 1
            # Create function for plotting data in separate panels
            fn.panel <- function(x, y, ...) {
              dummy.data <-
                rbind(apply(a[, c(idx[i, 'x'], idx[i, 'y'])], 2, min), apply(a[, c(idx[i, 'x'], idx[i, 'y'])], 2, max))
              points(
                dummy.data,
                cex = 1,
                pch = 20,
                col = "#A9A19C"
              )
              abline(h = varLines()[, idx[i, 'y']], v = varLines()[, idx[i, 'x']])  # add event values as lines
              box()
              i <<- i + 1
            }
          }
          # Create the pairs plot (scatter plots) for all variables
          pairs(a, panel = fn.panel, main = graphTitle, labels = variableLabels)
          ### Else if 'Standardised' scale
        } else if (input$ScalePostSim_step3 == "Std") {
          # Ensure data are numeric not factors
          b <-
            apply(HT.simSelect$Standardised, 2, function(x) {
              as.numeric(as.character(x))
            })
          b <- as.data.frame(b)
          # Dependence thresholds
          dep.thresh.vec <- unlist(HTmodelSelect$DepThreshold)
          # Set a grid index for all pairs of variables
          idx <-
            subset(expand.grid(y = 1:length(b), x = 1:length(b)), x != y)
          i <- 1
          # Create function for plotting data in separate panels
          fn.panel <- function(x, y, ...) {
            dep.thresh.select <-
              c(dep.thresh.vec[idx[i, 'x']], dep.thresh.vec[idx[i, 'y']])
            # Select the relevant variables in the simulated data set
            data.comb <- cbind(b[, idx[i, 'x']], b[, idx[i, 'y']])
            data.comb.exc <-
              data.comb[c(data.comb[, 1] >= dep.thresh.select[1] |
                            data.comb[, 2] >= dep.thresh.select[2]), ]
            points(
              data.comb.exc,
              cex = 1,
              pch = 20,
              col = alpha("#0D9DDB", pairs_opacity)
            )
            polygon(
              c(-100, dep.thresh.select[1], dep.thresh.select[1], -100),
              c(-100, -100, dep.thresh.select[2], dep.thresh.select[2]),
              col = "#A9A19C"
            )
            box()
            i <<- i + 1
          }
          # Create the pairs plot (scatter plots) for all variables
          pairs(b, panel = fn.panel, main = graphTitle, labels = variableLabels)
        }
      }
    })  
    
    
    ### Find values of events in joint probability table to plot as lines on the interactive plot
    ### This is reactive on the values changing
    lineLevels <- reactive({
      Var.names = names(inData()[, -1])  # names of variables
      ### If a value has been entered/changed in the joint probability table then take the values from here
      if (!is.null(input$hot)) {
        dt = JPdf$value
        ### If the user has not entered a value into the joint probability table then use the default values
      } else{
        eval(parse(
          text = paste0(
            "dt = data.frame('AEP (%)' = rep(0, length(Var.names())), ",
            Value,
            " = rep(0, length(Var.names())), stringsAsFactors = FALSE, check.names = FALSE)"
          )
        ))  # check.names = FALSE allows spaces in the names
        row.names(dt) <- Var.names
        dt
      }
    })
    
    ### Extract the appropriate rows from lineLevels
    ### Value of line for first selected variable
    var1Line <- reactive({
      var1 <-
        input$pairOptions1_postSim_step3  # extract the name of the first selected variable
      Var.names = names(inData()[, -1])
      eval(parse(
        text = paste0(
          "lineLevels()$",
          Value,
          "[which(row.names(lineLevels()) == var1)]"
        )
      ))
    })
    
    ### Value of line for second selected variable
    var2Line <- reactive({
      var2 <-
        input$pairOptions2_postSim_step3  # extract the name of the second selected variable
      Var.names = names(inData()[, -1])
      eval(parse(
        text = paste0(
          "lineLevels()$",
          Value,
          "[which(row.names(lineLevels()) == var2)]"
        )
      ))
    })
    
    ### Interactive plot
    output$pair_interactive_postSim_step3 <- renderPlotly({
      validate(need(length(input$origSimData_step3) > 0, "No data selected"))  # need at least one display option selected
      ### Set selected variables
      var1 <- input$pairOptions1_postSim_step3
      var2 <- input$pairOptions2_postSim_step3
      validate(need(!is.null(var1) &&
                      !is.null(var2), "Plot loading..."))  # Set plot loading message
      
      #extract the user defined labels
      var1Label = input[[paste0("value", var1)]]
      var2Label = input[[paste0("value", var2)]]
      graphTitle = input$userDefinedJointProbabilityBottomGraphTitle
      
      Var.names = names(inData()[, -1])  # extract variable names
      
      ### Select simulation data dependent on whether the 'Input and use your own marginal parameters' box is ticked on the 'Marginal analysis' tab
      ### and based on whether a simulation has just been run or whether a user-saved model has been loaded
      if (input$showTable == FALSE) {
        if (input$Step2eventset) {
          HT.simSelect2 <- HT.sim()
          HT.simSelect2 <-
            thin.data.function(HT.simSelect2, input$pcThinned_step3)  # thin the data
        } else if (!is.null(savedModelData())) {
          HT.simSelect2 <- HT.sim.two()
          HT.simSelect2 <-
            thin.data.function(HT.simSelect2, input$pcThinned_step3)  # thin the data
        }
      } else if (input$showTable == TRUE) {
        # if using user-supplied marginals
        ### If a simulation has just been updated then use the simulation outputs that have just been calculated
        if (input$useUserMarginals) {
          HT.simSelect2 <- HTuserMarginals()
          HT.simSelect2 <-
            thin.data.function(HT.simSelect2, input$pcThinned_step3)  # thin the data
          ### Else if a saved model has been loaded with updated simulation outputs then use those
        } else if (!is.null(savedModelData()) &&
                   length(HT.userMarginals.two()) > 0) {
          HT.simSelect2 <- HT.userMarginals.two()
          HT.simSelect2 <-
            thin.data.function(HT.simSelect2, input$pcThinned_step3)  # thin the data
        }
      }
      
      ### Select which dependence structure model to use
      if (input$Step2model) {
        # if one has just been created then select that
        HTmodelSelect <- HTmodel()
      } else if (!is.null(savedModelData())) {
        # else if a saved model has been loaded then select that
        HTmodelSelect <- HT.model.two()
      }
      
      ### If both observed and simulated data are selected for plotting
      if (length(input$origSimData_step3) == 2) {
        if (input$lagTick == TRUE) {  # if the lag tick box has been ticked
          # extract the lag parameters
          ts.df <- inData()  # extract the uploaded data 
          data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
          setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag  
          dat <- cbind(ts.df[1], data)  # bind the lagged data with the original date column
          dat <- na.omit(dat)  # removes rows with NA values in them
          # Set the observed data and add a 'Type' column
          origData <-
            as.data.frame(cbind(dat[, -1], rep("Observed", length(dat[, 1]))))
        } else {
        # Set the observed data and add a 'Type' column
        origData <-
          as.data.frame(cbind(inData()[, -1], rep("Observed", length(inData()[, 1]))))
        }
        colnames(origData)[ncol(origData)] <- "Type"
        # Set the simulated data, add a 'Type' column and ensure that the data are numeric (not factors)
        simData <-
          as.data.frame(cbind(HT.simSelect2[["Value"]][, -1], 
                              rep("Simulated", length(HT.simSelect2[["Value"]][, 1])
          )))
        simData[, 1:(ncol(simData) - 1)] <-
          apply(simData[, 1:(ncol(simData) - 1)], 2, function(x) {
            as.numeric(as.character(x))
          })
        colnames(simData)[ncol(simData)] <- "Type"
        # Set the observed data on a standardised scale and add a 'Type' column
        origDataStd <-
          as.data.frame(cbind(std_data(), rep("Observed", length(std_data()[, 1]))))
        colnames(origDataStd)[ncol(origDataStd)] <- "Type"
        # Set the simulated data on a standardised scale and add a 'Type' column
        simDataStd <-
          as.data.frame(cbind(HT.simSelect2$Standardised, 
                              rep("Simulated", length(HT.simSelect2$Standardised[, 1])
          )))  # simulated data standardised
        colnames(simDataStd)[ncol(simDataStd)] <- "Type"
        # Combine data for original scale and standardised scale - put the simulated data first so that it plots underneath the observed
        allData <-
          rbind(simData, origData)  # all data on the original scale
        allDataStd <-
          rbind(simDataStd, origDataStd)  # all data on the standardised scale
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        scaZoom_postSim <- switch(input$ScalePostSim_step3,
                                  "Orig" = allData[, c(var1, var2, "Type")],
                                  "Std" = allDataStd[, c(var1, var2, "Type")])
        ### If 'Original' scale is selected then plot with the event values added as lines and remove simulated data below the marginal thresholds
        if (input$ScalePostSim_step3 == "Orig") {
          a <- scaZoom_postSim  # all data
          a.sim <-
            data.frame(a[which(a$Type == "Simulated"), ])  # simulated data
          a.obs <-
            data.frame(a[which(a$Type == "Observed"), ])  # observed data
          ### Find the marginal thresholds
          # Access values of the threshold from the model fitting code
          marg.thresh.vec <- rep(NA, ncol(inData()[, -1]))
          for (i in 1:length(marg.thresh.vec)) {
            marg.thresh.vec[i] <-
              as.numeric(HTmodelSelect$MarginalParameter[[i]]$threshold)
          }
          names(marg.thresh.vec) <- Var.names
          marg.thresh.select <-
            c(marg.thresh.vec[var1], marg.thresh.vec[var2])
          data.comb.exc <-
            a.sim[c(a.sim[, var1] >= marg.thresh.select[1] |
                      a.sim[, var2] >= marg.thresh.select[2]), ]  # keep type column
          data.comb.exc <- data.frame(data.comb.exc)
          # Combine the above simulated events (above the marginal thresholds) with the observed for plotting
          obs.sim.comb <- rbind(data.comb.exc, a.obs)
          plot.df <-
            data.frame(obs.sim.comb[, 1],
                       obs.sim.comb[, 2],
                       obs.sim.comb[, 3],
                       stringsAsFactors = FALSE)
          colnames(plot.df) <- c("x", "y", "Type")
          plot.df$Type <-
            factor(plot.df$Type, levels = c("Simulated", "Observed"))
          
          ### If 'Disable automatic plot updating' is not ticked then plot all the data, event values as lines and the non-extremes area as a grey box
          if (!input$pausePlots) {
            plot_ly(
              plot.df,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              color = ~ Type,
              colors = c("#0D9DDB", "#4E4A47"),
              marker = list(size = marker_size, opacity = marker_opacity)
            ) %>%
              layout(
                xaxis = list(title = var1Label),
                yaxis = list(title = var2Label),
                title = graphTitle, 
                legend = list(traceorder = "reversed")
              ) %>% 
              layout(shapes = list(
                list(
                  type = "rect",
                  fillcolor = "#A9A19C",
                  line = list(color = "#A9A19C"),
                  opacity = 0.8,
                  # Set minimum values to be 5% below the minimum observed or simulated event
                  x0 = min(plot.df$x) - abs(0.05 * min(plot.df$x)),
                  x1 = marg.thresh.select[1],
                  xref = "x",
                  y0 = min(plot.df$y) - abs(0.05 * min(plot.df$y)),
                  y1 = marg.thresh.select[2],
                  yref = "y"
                )
              )) %>%
              add_trace(
                x = c(min(as.numeric(plot.df$x)), max(as.numeric(plot.df$x)) * 1.1),
                y = ~ c(var2Line(), var2Line()),
                type = 'scatter',
                name = 'Value',
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                showlegend = FALSE,
                inherit = FALSE
              ) %>%
              add_trace(
                x = c(var1Line(), var1Line()),
                y = ~ c(min(as.numeric(plot.df$y)), max(as.numeric(plot.df$y)) * 1.1),
                type = 'scatter',
                name = 'Value',
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                inherit = FALSE
              )
            ### Else if 'Disable automatic plot updating' is ticked then create a dummy data set to plot which contains
            ### the maximum and minimum values from above as well as the event values as lines
          } else{
            dummy.df <-
              data.frame(
                x = c(min(plot.df$x), max(plot.df$x)),
                y = c(min(plot.df$y), max(plot.df$y)),
                stringsAsFactors = FALSE
              )
            plot_ly(
              dummy.df,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              showlegend = FALSE,
              name = "",
              marker = list(color = "#A9A19C")
            ) %>%
              layout(
                xaxis = list(title = var1Label),
                yaxis = list(title = var2Label),
                title = graphTitle, 
                legend = list(traceorder = "reversed")
              ) %>%
              add_trace(
                x = c(min(as.numeric(plot.df$x)), max(as.numeric(plot.df$x)) * 1.1),
                y = c(var2Line(), var2Line()),
                name = Value,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                showlegend = FALSE
              ) %>%
              add_trace(
                x = c(var1Line(), var1Line()),
                y = c(min(as.numeric(plot.df$y)), max(as.numeric(plot.df$y)) * 1.1),
                name = Value,
                mode = "lines",
                line = list(color = "rgb(0,0,0)")
              )
          }
          ### Else if 'Standardised' scale is selected then plot without event values but remove the simulated data below the marginal thresholds
        } else if (input$ScalePostSim_step3 == "Std") {
          ### Ensure the data are numeric not factors
          scaZoom_postSim[, 1:2] <-
            apply(scaZoom_postSim[, 1:2], 2, function(x) {
              as.numeric(as.character(x))
            })
          b <- data.frame(scaZoom_postSim)  # all data
          b.sim <-
            data.frame(b[which(b$Type == "Simulated"), ])  # simulated data
          b.obs <-
            data.frame(b[which(b$Type == "Observed"), ])  # observed data
          
          # Dependence thresholds
          dep.thresh.vec <- unlist(HTmodelSelect$DepThreshold)
          names(dep.thresh.vec) <- Var.names
          dep.thresh.select <-
            c(dep.thresh.vec[var1], dep.thresh.vec[var2])
          
          # Select those points in the joint tail region
          data.comb.exc <-
            b.sim[c(b.sim[, var1] >= dep.thresh.select[1] |
                      b.sim[, var2] >= dep.thresh.select[2]), ]  # keep type column
          
          data.comb.exc <- data.frame(data.comb.exc)
          # Combine the above simulated events (above the marginal thresholds) with the observed for plotting
          obs.sim.comb <- rbind(data.comb.exc, b.obs)
          plot.df <-
            data.frame(obs.sim.comb[, 1],
                       obs.sim.comb[, 2],
                       obs.sim.comb[, 3],
                       stringsAsFactors = FALSE)
          colnames(plot.df) <- c("x", "y", "Type")
          plot.df$Type <-
            factor(plot.df$Type, levels = c("Simulated", "Observed"))
          plot_ly(
            plot.df,
            x =  ~ x,
            y =  ~ y,
            type = 'scatter',
            mode = "markers",
            color = ~ Type,
            colors = c("#0D9DDB", "#4E4A47"),
            marker = list(size = marker_size, opacity = marker_opacity)
          ) %>%
            layout(
              xaxis = list(title = var1Label),
              yaxis = list(title = var2Label),
              title = graphTitle,
              legend = list(traceorder = "reversed")
            ) %>%
            layout(shapes = list(
              # add non-extreme region
              list(
                type = "rect",
                fillcolor = "#A9A19C",
                line = list(color = "#A9A19C"),
                opacity = 0.8,
                # blue
                # Set minimum values to be 5% below the minimum observed or simulated event
                x0 = min(plot.df$x) - abs(0.05 * min(plot.df$x)),
                x1 = dep.thresh.select[1],
                xref = "x",
                y0 = min(plot.df$y) - abs(0.05 * min(plot.df$y)),
                y1 = dep.thresh.select[2],
                yref = "y"
              )
            ))
        }
        ### Else if observed data selected for plotting
      } else if (input$origSimData_step3 == "origData") {
        if (input$lagTick == TRUE) {  # if the "Use lagged data" box has been ticked
          # Extract the lag parameters
          ts.df <- inData()  # extract the uploaded data
          data <- ts.df[2:ncol(ts.df)]  # extract the data without the date column
          setDT(data)[,names(data) := Map(shift, .SD, lagParameters())]  # shift the data by the amount specified in the lag table
          dat <- cbind(ts.df[1], data)  # binds the lagged data with the original date column
          dat <- na.omit(dat)  # removes rows with NA values
          ### Use a switch to set whether to use the original or standardised data based on the user selection
          scaZoom <- switch(input$ScalePostSim_step3,
                            "Orig" = dat[, c(var1, var2)],
                            "Std" = std_data()[, c(var1, var2)])
        } else {
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        scaZoom <- switch(input$ScalePostSim_step3,
                          "Orig" = inData()[, c(var1, var2)],
                          "Std" = std_data()[, c(var1, var2)])
        }
        ### Ensure data are numeric not factors
        scaZoom <-
          apply(scaZoom, 2, function(x) {
            as.numeric(as.character(x))
          })
        plot.df.orig <- data.frame(scaZoom[, 1], scaZoom[, 2])
        colnames(plot.df.orig) <- c("x", "y")
        ### If 'Original' scale selected
        if (input$ScalePostSim_step3 == "Orig") {
          var1name = Value  # add label for event value line
          var2name = Value  # add label for event value line
          ### If 'Disable automatic plot updating' is not ticked plot the observed data and the event value lines
          if (!input$pausePlots) {
            plot_ly(
              plot.df.orig,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              marker = list(
                color = "#4E4A47",
                size = marker_size,
                opacity = marker_opacity
              ),
              showlegend = FALSE,
              name = "Observed"
            ) %>%
              layout(xaxis = list(title = var1Label), 
                     yaxis = list(title = var2Label),
                     title = graphTitle) %>%
              add_trace(
                x = c(min(plot.df.orig$x), max(plot.df.orig$x) * 1.1),
                y = c(var2Line(), var2Line()),
                type = 'scatter',
                name = var2name,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                inherit = FALSE
              ) %>%
              add_trace(
                x = c(var1Line(), var1Line()),
                y = c(min(plot.df.orig$y), max(plot.df.orig$y) * 1.1),
                type = 'scatter',
                name = var1name,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                showlegend = FALSE,
                inherit = FALSE
              )
            ### Else if 'Disable automatic plot updating' is ticked then create a dummy data set to plot which contains
            ### the maximum and minimum values from above and the event value lines
          } else{
            dummy.df.orig <-
              data.frame(
                x = c(min(plot.df.orig$x), max(plot.df.orig$x)),
                y = c(min(plot.df.orig$y), max(plot.df.orig$y)),
                stringsAsFactors = FALSE
              )
            plot_ly(
              dummy.df.orig,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              marker = list(color = "#A9A19C"),
              showlegend = FALSE,
              name = ""
            ) %>%
              layout(xaxis = list(title = var1Label), 
                     yaxis = list(title = var2Label),
                     title = graphTitle) %>%
              add_trace(
                x = c(min(plot.df.orig$x), max(plot.df.orig$x) * 1.1),
                y = c(var2Line(), var2Line()),
                name = var2name,
                mode = "lines",
                line = list(color = "rgb(0,0,0)")
              ) %>%
              add_trace(
                x = c(var1Line(), var1Line()),
                y = c(min(plot.df.orig$y), max(plot.df.orig$y) * 1.1),
                name = var1name,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                showlegend = FALSE
              )
          }
        ### Else if 'Standardised' scale selected for plotting then just plot the standardised data
        } else if (input$ScalePostSim_step3 == "Std") {
          plot_ly(
            plot.df.orig,
            x =  ~ x,
            y =  ~ y,
            type = 'scatter',
            mode = "markers",
            marker = list(
              color = "#4E4A47",
              size = marker_size,
              opacity = marker_opacity
            ),
            showlegend = FALSE
          ) %>% layout(
            xaxis = list(title = var1Label),
            yaxis = list(title = var2Label),
            title = graphTitle
          )
        }
        ### Else if simulated data selected for plotting
      } else if (input$origSimData_step3 == "simData") {
        ### Use a switch to set whether to use the original or standardised data based on the user selection
        scaZoom_postSim <- switch(
          input$ScalePostSim_step3,
          "Orig" = HT.simSelect2[["Value"]][, c(var1, var2)],
          "Std" = HT.simSelect2$Standardised[, c(var1, var2)]
        )
        # Ensure data are numeric rather than factors
        scaZoom_postSim <-
          apply(scaZoom_postSim, 2, function(x) {
            as.numeric(as.character(x))
          })
        ### If 'Original' scale selected
        if (input$ScalePostSim_step3 == "Orig") {
          a <- scaZoom_postSim  # simulated data
          # Access values of the threshold from the model fitting code
          marg.thresh.vec <- rep(NA, ncol(inData()[, -1]))
          for (i in 1:length(marg.thresh.vec)) {
            marg.thresh.vec[i] <-
              as.numeric(HTmodelSelect$MarginalParameter[[i]]$threshold)
          }
          names(marg.thresh.vec) <- Var.names
          marg.thresh.select <-
            c(marg.thresh.vec[var1], marg.thresh.vec[var2])
          
          # Select those points in the joint tail region
          data.comb.exc <-
            a[c(a[, var1] >= marg.thresh.select[1] |
                  a[, var2] >= marg.thresh.select[2]), ]
          plot.df.sim <- data.frame(data.comb.exc)
          colnames(plot.df.sim) <- c("x", "y")
          ### If 'Disable automatic plot updating' is not ticked then plot the simulated data, the non-extreme region and the event value lines
          if (!input$pausePlots) {
            plot_ly(
              plot.df.sim,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              marker = list(
                color = "#0D9DDB",
                size = marker_size,
                opacity = marker_opacity
              ),
              name = "Simulated",
              showlegend = FALSE
            ) %>%
              layout(xaxis = list(title = var1Label),
                     yaxis = list(title = var2Label),
                     title = graphTitle) %>%
              layout(shapes = list(
                # add non-extreme region as a grey box
                list(
                  type = "rect",
                  fillcolor = "#A9A19C",
                  line = list(color = "#A9A19C"),
                  opacity = 0.8,
                  # 0.3
                  # Set minimum values to be 5% below the minimum observed or simulated event
                  x0 = min(plot.df.sim$x) - abs(0.05 * min(plot.df.sim$x)),
                  x1 = marg.thresh.select[1],
                  xref = "x",
                  y0 = min(plot.df.sim$y) - abs(0.05 * min(plot.df.sim$y)),
                  y1 = marg.thresh.select[2],
                  yref = "y"
                )
              )) %>% 
              add_trace(
                x = c(min(plot.df.sim$x), max(plot.df.sim$x) * 1.1),
                y = c(var2Line(), var2Line()),
                type = 'scatter',
                name = Value,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                showlegend = FALSE,
                inherit = FALSE
              ) %>%
              add_trace(
                x = c(var1Line(), var1Line()),
                y = c(min(plot.df.sim$y), max(plot.df.sim$y) * 1.1),
                type = 'scatter',
                name = Value,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                inherit = FALSE
              )
            ### Else if 'Disable automatic plot updating' is ticked then create a dummy data set to plot which contains
            ### the maximum and minimum values from above and plot the event value lines
          } else{
            dummy.df.sim <-
              data.frame(
                x = c(min(plot.df.sim$x), max(plot.df.sim$x)),
                y = c(min(plot.df.sim$y), max(plot.df.sim$y)),
                stringsAsFactors = FALSE
              )
            plot_ly(
              dummy.df.sim,
              x =  ~ x,
              y =  ~ y,
              type = 'scatter',
              mode = "markers",
              showlegend = FALSE,
              marker = list(color = "#A9A19C"),
              name = ""
            ) %>% layout(xaxis = list(title = var1Label), 
                         yaxis = list(title = var2Label), 
                         title = graphTitle) %>%
              add_trace(
                x = c(min(plot.df.sim$x), max(plot.df.sim$x) * 1.1),
                y = c(var2Line(), var2Line()),
                name = Value,
                mode = "lines",
                line = list(color = "rgb(0,0,0)"),
                showlegend = FALSE
              ) %>%
              add_trace(
                x = c(var1Line(), var1Line()),
                y = c(min(plot.df.sim$y), max(plot.df.sim$y) * 1.1),
                name = Value,
                mode = "lines",
                line = list(color = "rgb(0,0,0)")
              )
          }
          ### Else if 'Standardised' scale selected for plotting
        } else if (input$ScalePostSim_step3 == "Std") {
          b <- data.frame(scaZoom_postSim)
          # Dependence thresholds
          dep.thresh.vec <- unlist(HTmodelSelect$DepThreshold)
          names(dep.thresh.vec) <- Var.names
          dep.thresh.select <-
            c(dep.thresh.vec[var1], dep.thresh.vec[var2])
          
          # Select those points in the joint tail region
          data.comb.exc <-
            b[c(b[, var1] >= dep.thresh.select[1] |
                  b[, var2] >= dep.thresh.select[2]), ]
          plot.df.sim <- data.frame(data.comb.exc)
          colnames(plot.df.sim) <- c("x", "y")
          plot_ly(
            plot.df.sim,
            x =  ~ x,
            y =  ~ y,
            type = 'scatter',
            mode = "markers",
            marker = list(
              color = "#0D9DDB",
              size = marker_size,
              opacity = marker_opacity
            ),
            showlegend = FALSE
          ) %>% 
            layout(xaxis = list(title = var1Label),
                   yaxis = list(title = var2Label),
                   title = graphTitle) %>% 
            layout(shapes = list(
              # add the non-extreme region as a grey box
              list(
                type = "rect",
                fillcolor = "#A9A19C",
                line = list(color = "#A9A19C"),
                opacity = 0.8,
                # Set minimum values to be 5% below the minimum observed or simulated event
                x0 = min(plot.df.sim$x) - abs(0.05 * min(plot.df.sim$x)),
                x1 = dep.thresh.select[1],
                xref = "x",
                y0 =  min(plot.df.sim$y) - abs(0.05 * min(plot.df.sim$y)),
                y1 = dep.thresh.select[2],
                yref = "y"
              )
            ))
        }
      }
    })

    
    ### Set up the controls for the interactive scatter plot to allow users to specify which variables to plot 
    output$pairControls1_postSim_step3 <- renderUI({
      validate(need(input$file1 != "" || input$savedModel != "", ""))  # need either input data or a saved model loaded otherwise this is empty
     
      variables <- names(inData()[,-1])  # extract variable names
      selectInput("pairOptions1_postSim_step3", "", variables, selected = variables[1])  # default is set to the first variable
    })
    
    output$pairControls2_postSim_step3 <- renderUI({
      validate(
        need(input$file1 != "" || input$savedModel != "", "")  # need either input data or a saved model loaded otherwise this is empty
      )
      variables <- names(inData()[,-1])  # extract variable names
      selectInput("pairOptions2_postSim_step3", "", variables, selected = variables[2])  # default is set to the second variable
    })

    ######## Joint probability analysis ########
    ### Joint Probability calculation
    # Toggle off buttons if lag table is incomplete
    observe({
      toggleState("JPgo", !is.na(any(unlist(input$lag$data))), "")
    })
    ### Reactive on the 'Calculate joint probability' button being pressed
    calcJP <- eventReactive(input$JPgo, {
      dt = JPdf$value
      
      ### Select simulation data dependent on whether the 'Input and use your own marginal parameters' box is ticked on the 'Marginal analysis' tab
      ### and based on whether a simulation has just been run or whether a user-saved model has been loaded
      if (input$showTable == FALSE) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
        }
      } else if (input$showTable == TRUE) {
        # if using user-supplied marginals
        ### If a simulation has just been updated then use the simulation outputs that have just been calculated
        if (input$useUserMarginals) {
          HT.simSelect <- HTuserMarginals()
          ### Else if a saved model has been loaded with updated simulation outputs then use those
        } else if (!is.null(savedModelData()) &&
                   length(HT.userMarginals.two()) > 0) {
          HT.simSelect <- HT.userMarginals.two()
        }
      }
      
      ### Extract event set data
      eventsetData <-
        list(ReturnPeriod = HT.simSelect$ReturnPeriod,
             Level = HT.simSelect$Value)  # includes date columns
      
      ### Extract the event from the joint probability table
      eval(parse(text = paste0("inputEvent <- dt$", Value)))
      
      ### Calculate the joint probability
      joint.rp.func.year(
        data = eventsetData,
        vec.interest = c(inputEvent),
        scale = "Level",
        years.data = input$eventYears
      )
    })
  
    ### Set up the joint probability table to be reactive to changes and update the other column automatically
    ### Set up the variable names as a reactive
    Var.names <- reactive({
      names(inData()[, -1])
    })
    
    ### Set up an empty reactiveValues object which will store the values from the table
    JPdf <- reactiveValues(value = NULL)
  
    ### If automatically-determined marginals are being used
    observe({
      if (input$showTable == FALSE) {  # if the box has not been ticked to input marginal parameters
        Var.names <- names(inData()[, -1])
        minData <-
          apply(inData()[, -1], 2, min)  # find the minimum observed values for each variable
        ### If no values have been entered into the table then set the default values
        if (is.null(input$hot)) {
          ### If the data are observations sampled at peak surge events then set the AEP column to a default of NA
          ### Else if the data are time series sampling all observations then set the AEP column to a default of 99.99
          if (input$dataType == "declust") {
            eval(parse(
              text = paste0(
                "JPdf$value <- data.frame('AEP (%)' = rep(NA, length(Var.names())), ",
                Value,
                " = c(minData), stringsAsFactors = FALSE, check.names=FALSE)"
              )
            ))
          } else{
            eval(parse(
              text = paste0(
                "JPdf$value <- data.frame('AEP (%)' = rep(99.99, length(Var.names())), ",
                Value,
                " = c(minData), stringsAsFactors = FALSE, check.names=FALSE)"
              )
            ))
          }
        } else {
          # else if values have been entered into the table then extract those values
          if (input$Step2eventset) {
            HT.simSelect <- HT.sim()
          } else if (!is.null(savedModelData())) {
            HT.simSelect <- HT.sim.two()
          }
          DF = hot_to_r(input$hot)  # convert table to R data frame
          ### Set whether data are declustered
          if (input$dataType == "ts") {
            declustered = FALSE
          } else if (input$dataType == "declust") {
            declustered = TRUE
          }
          # Only update table if changes are detected
          if (!is.null(input$hot$changes$changes)) {
            ### If the first column (AEP (%)) has been updated then calculate the corresponding values and update column
            if (input$hot$changes$changes[[1]][[2]] == 0) {
              eval(parse(
                text = paste0(
                  "DF$",
                  Value,
                  " <- c(t(round(AEPtoLevel.GP(HT.simSelect$GPDParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
                )
              ))
            }
            ### If the second column (Value) has been updated then calculate the corresponding AEP (%) and update column
            if (input$hot$changes$changes[[1]][[2]] == 1) {
              eval(parse(
                text = paste0(
                  "DF$'AEP (%)' <- 100*c(t(round(LeveltoAEP.GP(HT.simSelect$GPDParameters, DF$",
                  Value,
                  ", Var.names, c(minData), declustered),digits=3)))"
                )
              ))
            }
            JPdf$value <- DF
          }
        }
      }
    })
    
    # Recalculate joint probability table values when the "Input and use your own marginal parameters" box is toggled on and off
    observeEvent(input$showTable,{ 
      
      # If the box is unticked
      if (input$showTable == FALSE && !is.null(input$hot)) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
        }
        
        DF = hot_to_r(input$hot)  # convert table to R data frame
        
        ### Set whether data are declustered
        if (input$dataType == "ts") {
          declustered = FALSE
        } else if (input$dataType == "declust") {
          declustered = TRUE
        }
        
        ### If the first column (AEP (%)) has been updated then calculate the corresponding values and update column
          eval(parse(
            text = paste0(
              "DF$",
              Value,
              " <- c(t(round(AEPtoLevel.GP(HT.simSelect$GPDParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
            )
          ))
        JPdf$value <- DF
      }
      
      # If the box is ticked and there are user marginals
      if (input$showTable == TRUE && !is.null(input$hot) && !is.null(input$userMarginals)) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
        }
        
        DF = hot_to_r(input$hot)  # convert table to R data frame
        ### Set whether data are declustered
        if (input$dataType == "ts") {
          declustered = FALSE
        } else if (input$dataType == "declust") {
          declustered = TRUE
        }
        
        marginalParameters <-
          hot_to_r(input$userMarginals) # convert marginal parameters table to R data frame
        marginalParameters <- as.data.frame(marginalParameters)
        
        ### If the first column (AEP (%)) has been updated then calculate the corresponding values and update column
        eval(parse(
          text = paste0(
            "DF$",
            Value,
            " <- c(t(round(AEPtoLevel.GLO(marginalParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
          )
        ))
        JPdf$value <- DF
      }
      
    })
    
    # If the marginals table has been edited (JP table should update automatically when the user marginals are
    # changed in the previous tab (without ticking the 'use user marginals' box or pressing the 'update simulation' button)
    observeEvent(!is.null(input$userMarginals$changes$changes),{
      
      if (input$showTable == TRUE && !is.null(input$hot) && !is.null(input$userMarginals) && is.null(input$hot$changes$changes)) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
        }
        
        DF = hot_to_r(input$hot)  # convert table to R data frame
        ### Set whether data are declustered
        if (input$dataType == "ts") {
          declustered = FALSE
        } else if (input$dataType == "declust") {
          declustered = TRUE
        }
        
        marginalParameters <-
          hot_to_r(input$userMarginals) # convert marginal parameters table to R data frame
        marginalParameters <- as.data.frame(marginalParameters)
        
        ### If the first column (AEP (%)) has been updated then calculate the corresponding values and update column
        eval(parse(
          text = paste0(
            "DF$",
            Value,
            " <- c(t(round(AEPtoLevel.GLO(marginalParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
          )
        ))
        JPdf$value <- DF
      }
    })
    
    i_temp <- reactiveValues(value = 1)
    
    output$i_counter <- renderUI({
      i_temp <- i_temp$value
    })

    ### If user-supplied marginals are being used and have been entered by the user (i.e. they exist)
    observe({
      if (input$showTable == TRUE &&
          !is.null(input$userMarginals)) {
        # this is not dependent on the updated simulation so nothing else required
        Var.names <- names(inData()[, -1])
        minData <-
          apply(inData()[, -1], 2, min) # find the minimum observed values for each variable
        ### If no values have been entered into the table then use default values
        if (is.null(input$hot)) {
          eval(parse(
            text = paste0(
              "JPdf$value <- data.frame('AEP (%)' = rep(99.99, length(Var.names())), ",
              Value,
              " = c(minData), stringsAsFactors = FALSE, check.names=FALSE)"
            )
          ))
        } else {
          # else if values have been entered then use those
          if (input$Step2eventset) {
            HT.simSelect <- HT.sim()
          } else if (!is.null(savedModelData())) {
            HT.simSelect <- HT.sim.two()
          }
          DF = hot_to_r(input$hot) # convert table to R data frame
          marginalParameters <-
            hot_to_r(input$userMarginals) # convert marginal parameters table to R data frame
          marginalParameters <- as.data.frame(marginalParameters)
          if (!is.null(input$hot$changes$changes)) {  # if the joint probability analysis table has been changed.
            ### If the first column (AEP (%)) has been updated then calculate the corresponding values and update column
            if (input$hot$changes$changes[[1]][[2]] == 0) {
              eval(parse(
                text = paste0(
                  "DF$",
                  Value,
                  " <- c(t(round(AEPtoLevel.GLO(marginalParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
                )
              ))
            }
            ### If the second column (Value) has been updated then calculate the corresponding AEP (%) and update column
            if (input$hot$changes$changes[[1]][[2]] == 1) {
              eval(parse(
                text = paste0(
                  "DF$'AEP (%)' <- 100*c(t(round(LeveltoAEP.GLO(marginalParameters, DF$",
                  Value,
                  ", Var.names, c(minData)),digits=3)))"
                )
              ))
            }
            JPdf$value <- DF
          } else if (is.null(input$hot$changes$changes) && (input$useUserMarginals[1] == i_temp$value)) {
            i_temp$value <- i_temp$value + 1
            eval(parse(
              text = paste0(
                "DF$",
                Value,
                " <- c(t(round(AEPtoLevel.GLO(marginalParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
              )
            ))
            JPdf$value <- DF
          }
        }
      }
    })
    
    # If a saved model has been loaded with user marginals
    observeEvent(input$savedModel,
      {
        # Initialise the joint probability table when a saved model is uploaded
        if (savedModelData()$dataType == "ts" &&
            !is.null(HT.userMarginalsTable.two()$data)) { #if marginals exist
          # this is not dependent on the updated simulation so nothing else required
          Var.names <- names(inData()[,-1])
          minData <-
            apply(inData()[,-1], 2, min) # find the minimum observed values for each variable
          ### If no values have been entered into the table then use default values
          
          ### Extract user marginal parameters from loaded model
          marginalsTableData <- HT.userMarginalsTable.two()$data
          Locs <- c()
          Sc <- c()
          Sh <- c()
      
          for (i in 1:length(Var.names)) {
            # extract the parameter values for each variable
            Locs[i] <- marginalsTableData[[i]][[1]]
            Sc[i] <- marginalsTableData[[i]][[2]]
            Sh[i] <- marginalsTableData[[i]][[3]]
          }

          ### Add parameters to a data frame
          marginalParameters <-
            data.frame(
              Location = Locs,
              Scale = Sc,
              Shape = Sh,
              stringsAsFactors = FALSE
            )
          
          # Update JP table values
          eval(parse(
            text = paste0(
              "JPdf$value <- data.frame('AEP (%)' = rep(99.99, length(Var.names())), ",
              Value,
              " = c(t(round(AEPtoLevel.GLO(marginalParameters, JPdf$value$'AEP (%)'/100, Var.names, c(minData)),digits=3))), stringsAsFactors = FALSE, check.names=FALSE)"
            )
          ))
          row.names(JPdf$value) <- Var.names()
      
    }})
    
    ### If user-supplied marginals are being used but no marginals have been entered but some have been loaded in a saved model
    observe({
      if (input$showTable == TRUE &&
          is.null(input$userMarginals$changes$changes) &&
          !is.null(HT.userMarginalsTable.two()) &&
          !is.null(input$hot$changes$changes) && #if changes have been made to the joint probability table
                   savedModelData()$dataType == "ts" 
          ) {
        
        Var.names <- names(inData()[, -1])
        minData <-
          apply(inData()[, -1], 2, min) # find the minimum observed values for each variable

        ### Extract user marginal parameters from loaded model
        marginalsTableData <- HT.userMarginalsTable.two()$data
        Locs <- c()
        Sc <- c()
        Sh <- c()
        
        for (i in 1:length(Var.names)) {
          # extract the parameter values for each variable
          Locs[i] <- marginalsTableData[[i]][[1]]
          Sc[i] <- marginalsTableData[[i]][[2]]
          Sh[i] <- marginalsTableData[[i]][[3]]
        }
        ### Add parameters to a data frame
        marginalParameters <-
          data.frame(
            Location = Locs,
            Scale = Sc,
            Shape = Sh,
            stringsAsFactors = FALSE
          )

          # If values have been entered then use those
          if (input$Step2eventset) {
            HT.simSelect <- HT.sim()
          } else if (!is.null(savedModelData())) {
            HT.simSelect <- HT.sim.two()
          }
          DF = hot_to_r(input$hot)  # convert table to R data frame
          
          ### If the first column (AEP (%)) has been updated then calculate the corresponding values and update column
            if (input$hot$changes$changes[[1]][[2]] == 0) {
              eval(parse(
                text = paste0(
                  "DF$",
                  Value,
                  " <- c(t(round(AEPtoLevel.GLO(marginalParameters, DF$'AEP (%)'/100, Var.names, c(minData)),digits=3)))"
                )
              ))
            }
            ### If the second column (Value) has been updated then calculate the corresponding AEP (%) and update column
            if (input$hot$changes$changes[[1]][[2]] == 1) {
              eval(parse(
                text = paste0(
                  "DF$'AEP (%)' <- 100*c(t(round(LeveltoAEP.GLO(marginalParameters, DF$",
                  Value,
                  ", Var.names, c(minData)),digits=3)))"
                )
              ))
            }
            JPdf$value <- DF
          
        }
      
    })
    
    ### Create the table
    output$hot <- renderRHandsontable({
      Var.names <- names(inData()[, -1])  # extract variable names
      
      JPdf <-
        JPdf$value  # extract the 'value' from the joint probability data frame
      
      ### Create the table using the joint probability data frame
      if (input$dataType == "declust"  && input$showTable == FALSE) {
        rht = rhandsontable(
          JPdf,
          readOnly = FALSE,
          rowHeaders = Var.names,
          rowHeaderWidth = 150,
          selectCallback = TRUE,
          digits = 4
        ) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col("AEP (%)", readOnly = TRUE, type = "numeric") %>%
          hot_validate_numeric(cols = "Value", allowInvalid = FALSE)
        
      } else{
        rht = rhandsontable(
          JPdf,
          readOnly = FALSE,
          rowHeaders = Var.names,
          rowHeaderWidth = 150,
          selectCallback = TRUE,
          digits = 4
        ) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_validate_numeric(cols = "AEP (%)", min = 0, max = 99.9) %>%
          hot_validate_numeric(cols = "Value", allowInvalid = FALSE)
      }
      rht
    })
    
    ### Output the joint probability results
    output$JPout <- renderText({
      
      ### 1/AEP (return period)
      JPtest <- round(1 / calcJP()$JPCalc)
      
      ### Select simulation data dependent on whether the 'Input and use your own marginal parameters' box is ticked on the 'Marginal analysis' tab
      ### and based on whether a simulation has just been run or whether a user-saved model has been loaded
      if (input$showTable == FALSE) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
        }
      } else if (input$showTable == TRUE) {
        # if using user-supplied marginals
        ### If a simulation has just been updated then use the simulation outputs that have just been calculated
        if (input$useUserMarginals) {
          HT.simSelect <- HTuserMarginals()
          ### Else if a saved model has been loaded with updated simulation outputs then use those
        } else if (!is.null(savedModelData()) &&
                   length(HT.userMarginals.two()) > 0) {
          HT.simSelect <- HT.userMarginals.two()
        }
      }
      
      p <- calcJP()$JPCalc  # AEP
      n <- nrow(HT.simSelect$Value)  # number of simulated events
      if (JPtest == 1) {
        out1 <-
          paste0(
            "The joint probability of the event in the table in any given year based on a simulation of ",
            format(
              HT.simSelect$simYears,
              big.mark = ",",
              scientific = FALSE,
              trim = TRUE
            ),
            " years is nearly 1 in ",
            format(
              JPtest,
              big.mark = ",",
              scientific = FALSE,
              trim = TRUE
            ),
            " (",
            p * 100,
            "% chance)."
          )
      } else{
        out1 <-
          paste0(
            "The joint probability of the event in the table in any given year based on a simulation of ",
            format(
              HT.simSelect$simYears,
              big.mark = ",",
              scientific = FALSE,
              trim = TRUE
            ),
            " years is 1 in ",
            format(
              JPtest,
              big.mark = ",",
              scientific = FALSE,
              trim = TRUE
            ),
            " (",
            p * 100,
            "% chance)."
          )
      }
      ### Write out the results
      if (calcJP()$NoEvents == 1) {
        out2 <-
          paste(
            "There is",
            calcJP()$NoEvents,
            "simulated event that exceeds your event. It is recommended that at
                    least 10 simulated events exceed your event to account for uncertainty in the joint probability
                    estimate. If this is not the case, you can simulate a larger event set."
          )
      } else{
        out2 <-
          paste(
            "There are",
            calcJP()$NoEvents,
            "simulated events that exceed your event. It is recommended that at
                    least 10 simulated events exceed your event to account for uncertainty in the joint probability
                    estimate. If this is not the case, you can simulate a larger event set."
          )
      }
      out3 <-
        "The encounter probabilities for your event are given in the table below. If a value of 100% is shown,
               this should be interpreted as meaning a very high probability, i.e. close to 100% but not necessarily
               exactly 100%. The converse applies for values displaying as 0%."
      HTML(paste(strong(out1), out2, out3, sep = '<br/><br/>'))
    })
  
    ### Output the encounter probabilities
    output$encounterProbs <- renderTable(### Select simulation data dependent on whether the 'Input and use your own marginal parameters' box is ticked on the 'Marginal analysis' tab
      ### and based on whether a simulation has just been run or whether a user-saved model has been loaded
      if (input$showTable == FALSE) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
          encounter.prob.func(calcJP()$JPCalc, input$eventYears, c(1, 5, 10, 25, 50))
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
          encounter.prob.func(calcJP()$JPCalc, input$eventYears, c(1, 5, 10, 25, 50))
        }
      } else if (input$showTable == TRUE) {
        # if using user-supplied marginals
        ### If a simulation has just been updated then use the simulation outputs that have just been calculated
        if (input$useUserMarginals) {
          HT.simSelect <- HTuserMarginals()
          encounter.prob.func(calcJP()$JPCalc, input$eventYears, c(1, 5, 10, 25, 50))
          ### Else if a saved model has been loaded with updated simulation outputs then use those
        } else if (!is.null(savedModelData()) &&
                   length(HT.userMarginals.two()) > 0) {
          HT.simSelect <- HT.userMarginals.two()
          encounter.prob.func(calcJP()$JPCalc, input$eventYears, c(1, 5, 10, 25, 50))
        }
      }, align = 'c')
    
    
    ##################################### Tab 6 Export results #####################################
    # If hosted locally then show the embedded videos. If hosted on shinyapps.io then show the download links
    observe({
      if (is_local == FALSE) {
        shinyjs::show("exportPanel2")
        shinyjs::hide("exportPanel")
      } else{
        shinyjs::hide("exportPanel2")
        shinyjs::show("exportPanel")
      }
    })
    
    ### Select event set to save
    ### Need two if statements so they update correctly
    HTsimValues <- reactive({
      ### If automatically-determined marginals are being used, select the appropriate simulation data
      if (input$showTable == FALSE) {
        if (input$Step2eventset) {
          HT.simSelect <- HT.sim()
        } else if (!is.null(savedModelData())) {
          HT.simSelect <- HT.sim.two()
        }
        HT.simSelect$Value  # extract the simulated event set
        ### If user-supplied marginals are being used
      } else if (input$showTable == TRUE) {
        ### If simulation outputs have just been updated then use those
        if (input$useUserMarginals) {
          HT.simSelect <- HTuserMarginals()
          ### Else if a saved model has been loaded with updated simulation outputs then use those
        } else if (!is.null(savedModelData()) &&
                   length(HT.userMarginals.two()) > 0) {
          HT.simSelect <- HT.userMarginals.two()
        }
        HT.simSelect$Value
      }
    })
    
    ### Save the event set as a .csv file
    # When hosted locally
    ServerSideFileSaver(
      input = HTsimValues,
      output,
      type = "csv",
      session,
      roots,
      inputHandle = "directory2",
      inputTextBox = "saveModelFolder2",
      inputFileName = "saveModelName2",
      inputSaveButtonHandle = "ActionButtonSaveModel2",
      outputFeedbackHandle = "tO_ModelSaveFeedback2",
      msgSuccess = "Data exported to",
      msgError = "Error saving file. Check directory exists and that you have permission to write to it."
    )
    
    # When hosted on shinyapps.io
    output$saveEventSet <- downloadHandler(filename <-
                                             paste("Event_set", "csv", sep = "."),
                                           
                                           content <- function(file) {
                                             write.csv(as.data.frame(HTsimValues()), file, row.names = F)
                                           },
                                           contentType = "text/csv")
    
    ##################################################### Tab 7 (FAQ tab) #####################################################
    # If hosted locally then show the embedded videos. If hosted on shinyapps.io then show the download links
    # (these only work online although this includes browser mode when opened from RStudio - although download is very slow).
    observe({
      if (is_local == FALSE) {
        shinyjs::show("downloadvideo")
        shinyjs::show("downloadvideo2")
        shinyjs::hide("downloadvideo3")
        shinyjs::hide("downloadvideo4")
      } else{
        shinyjs::hide("downloadvideo")
        shinyjs::hide("downloadvideo2")
        shinyjs::show("downloadvideo3")
        shinyjs::show("downloadvideo4")
      }
    })
    
 
    output$downloadvideo <- downloadHandler(filename <-
                                              paste("JP_workshop", "mp4", sep = "."),
                                            
                                            content <- function(file) {
                                              file.copy("www/JPworkshop.mp4", file)
                                            },
                                            contentType = "video/mp4")
    
    output$downloadvideo2 <- downloadHandler(
      filename <- paste("WEM_Toolbox_Talk", "mp4", sep = "."),
      
      content <- function(file) {
        file.copy("www/WEMToolboxTalk.mp4", file)
      },
      contentType = "video/mp4"
    )
    
    
    output$downloadvideo3 <- renderUI({
      h5(
        "Joint probability workshop (webinar) - Autumn 2017",
        br(),
        tags$video(
          src = "JPworkshop.mp4",
          type = "video/mp4",
          autoplay = NA,
          controls = NA
        )
      )
    })
    
    output$downloadvideo4 <- renderUI({
      h5(
        "WEM Toolbox Talk: Planning for the Risk of Widespread Flooding - December 2017",
        br(),
        tags$video(
          src = "WEMToolboxTalk.mp4",
          type = "video/mp4",
          autoplay = NA,
          controls = NA
        )
      )
    })
          
})