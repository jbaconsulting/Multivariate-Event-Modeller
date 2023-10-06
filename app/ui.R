# This code was written by Jeremy Benn Associates Limited as part of the Multivariate Event Modeller tool. 
# It is released under the Open Government Licence 
# (https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
# Copyright (C) 2023 Environment Agency

# This program is free software; you can redistribute it and/or modify it under the terms 
# of the Open Government Licence.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY. 
# See the Open Government Licence for more details.


###################### Find out if the tool is running locally ######################
is_local <- Sys.getenv('SHINY_PORT') == ""

### Ensure tool opens in RStudio viewer window when run locally
if(is_local){
  options(shiny.launch.browser = .rs.invokeShinyWindowViewer) 
}

################ Import libraries that need to be in the ui (otherwise can cause errors) #################
library(shiny)
library(plotly)
library(rhandsontable)
library(shinyFiles)
  
################ Set up the tabs ##################
### These contain the UI information for the main panel of each tab

########## Overview tab ##########
### This is mainly introductory text
tab.Panel.README <- tabPanel(
  "Overview",  # tab label
  h1("Multivariate Event Modeller"),
  h2("Introduction"),
  p("The joint probability of two or more variables being \"extreme\" is relevant in Flood and Coastal Risk Management 
    (FCRM) in various contexts, including:"),
  p(tags$ul(  # bullet points
    tags$li("Assessing the likelihood of extreme peak flow events on multiple tributaries of a river to help in developing scenarios for a whole-catchment model"),
    tags$li("Placing recent or historical floods in context by estimating the combined likelihood of extreme flows, water levels, rainfall, wave or wind observations at one or more locations"),
    tags$li("Assessing the likelihood of combinations of extreme river flows, storm surge and possibly other relevant variables"),
    tags$li("Modelling the chance of combinations of extreme conditions occurring together in related variables such as soil moisture content, rainfall accumulations and river flows")
  )),
  p("Previous", a("Defra and Environment Agency guidance", href="http://www.estuary-guide.net/pdfs/FD2308_3429_TRP.pdf"), "describes
    methods for joint probability calculations for certain pairs of variables, based on statistical models supported by", 
    HTML(paste0(a("specialist software", href="https://eprints.hrwallingford.com/404/"), "."))),
  p("More recently,", a("new statistical methods", href = "http://onlinelibrary.wiley.com/doi/10.1111/j.1467-9868.2004.02050.x/abstract"),
    "have been developed that allow for a more general, data-driven analysis of the joint probability of extreme events in combinations of multiple variables. 
    Applications include regional and national scale assessments of the", HTML(paste0(a("probability of widespread flooding in rivers",
                                                                                        href="http://onlinelibrary.wiley.com/doi/10.1111/j.1753-318X.2010.01081.x/abstract"), ",")), "and joint probability analysis of",
    HTML(paste0(a("extreme surge and waves at the coast", href="http://www.sciencedirect.com/science/article/pii/S0378383914000210"), "."))),
  p("These methods have been documented and tested through a series of", HTML(paste0(a("Environment Agency research reports", 
                                                                                       href = "https://www.gov.uk/flood-and-coastal-erosion-risk-management-research-reports/spatial-coherence-risk-of-widespread-flooding"))), 
    "and were applied in the development of the", HTML(paste0(a("2017 National Risk Assessment scenarios", href = "https://www.gov.uk/flood-and-coastal-erosion-risk-management-research-reports/planning-for-the-risk-of-widespread-flooding"), "."))),
  p("The Multivariate Event Modeller (MEM) implements the new methods with user-supplied data sets to estimate the joint probability of extreme events in combinations of up to 10 variables."),
  p("The MEM is designed for joint probability analysis of extremes in time series data for up
    to 10 variables. Additionally, the MEM handles a special case where data have been sampled for extreme surge events to
    illustrate the extension of coastal joint probability methods to incorporate spatial dependence."),
  hr(),
  h2("Assumptions and limitations"),
  p("Assumptions and limitations of the MEM are discussed in Section 1.2 of the", 
    a("user guide", href="https://assets.publishing.service.gov.uk/media/60364280d3bf7f0aaf64f109/Planning_for_the_risk_of_widespread_flooding_-_user_guide__1_.pdf"), 
    "and further information can also be found in the FAQs tab. Note that a key functionality added since the user guide 
    was written is the ability to lag data. The assumptions and limitations include the following."),
  p(tags$ul(  # bullet points
    tags$li("The MEM requires either time series data at a daily resolution or observations sampled at peak surge events."),
    tags$li("There is a limit of 10 variables."),
    tags$li("The MEM cannot handle missing data."),
    tags$li("The MEM is intended for general multivariate analysis and for providing insight. It can support setting
            inflows / boundary conditions for hydraulic model scenarios but consideration should be given to sampling 
            uncertainties, the robustness of the input data and timing issues. It is recommended that the MEM be applied
            and compared with other methods."),
    tags$li("The MEM is not intended to be used for detailed design calculations for individual locations, where 
            methods such as those of the Flood Estimation Handbook would be more appropriate. The MEM may, however, 
            provide additional supporting evidence about spatial or multivariable joint events."),
    tags$li("The MEM does not directly tabulate a range of combinations for a specified joint probability."),
    tags$li("The MEM may not be suitable for all types of hydrological data, e.g. data from chalk catchments.")
  )),
  hr(),
  h2("How to use the Multivariate Event Modeller"),
  p("Work sequentially through the tabs on this page. After completing each tab you can switch back to a previous step at any time."),
  p("You can start a fresh analysis by pressing the ‘Reload the Shiny application’ button or the browser refresh button (or F5 when using some browsers) at any time."),
  p("Full instructions are provided in the", 
    HTML(paste0(a("user guide", href="https://assets.publishing.service.gov.uk/media/60364280d3bf7f0aaf64f109/Planning_for_the_risk_of_widespread_flooding_-_user_guide__1_.pdf"),
                ". Since the user guide was written, functionality has been added to enable a lag to be applied to one or more of the
                variables. Additionally, the user can now edit plot titles and labels. Some can be completely changed, whereas others
                can only be partially edited because certain elements of the automatically-generated title provide key information  
                about the data being plotted."))),
  h3("1. Input data"),
  p("You can choose either to upload time series data representing all observed values for up to 10 variables, or time series that have been sampled
    specifically to extract the values of skew surge, wave height and wind speed for peak surge events."),
  p("The former case (all observed values) allows for a generic joint probability analysis. The latter case (data sampled for peak surge events)
    can be used for the purposes of demonstrating the handling of spatially dependent coastal variables."),
  p("In both cases the data for all variables to be analysed should be supplied as columns in one .csv file."),
  h4("Data requirements for time series data sampling all observations"),
  p(tags$ul(  # bullet points
    tags$li("The input data must be in the form of concurrent observations of each variable, with no missing data."),
    tags$li("The first column must be a date in DD/MM/YYYY format (e.g. \"01/12/2010\" for 1st December 2010)."),
    tags$li("The other columns are variables."),
    tags$li("Each row is a set of observations on the same day. You can use daily average values, or the maximum observation on a day. It does not matter how you define “same day” (e.g. midnight-midnight or 09:00-08:59) as long as the definition is consistent."),
    tags$li("The first row is a header that contains 'Date' followed by the names of each of the variables (which can be a mix of letters a-z and numbers 0-9).")
  )),
  h4("Data requirements for observations sampled at peak surge events"),
  p(tags$ul(  # bullet points
    tags$li("Each column must represent a variable. A date column should not be included."),
    tags$li("Each row is a set of observations representing the same event."),
    tags$li("The first row is a header that contains the names of each of the variables (which can be a mix of letters a-z and numbers 0-9, however see below)."),
    tags$li("The name in the header row for variables representing skew surge must contain the string ‘SkewSurge’, which can be embedded in a longer name (e.g. ‘SkewSurge_JP26’).")
  )),
  h3("2. Time lag"),
  p("This provides an option to add a time lag to one or more of the variables in your time series 
    using a combination of lag values. This applies a constant time shift to the whole record for 
    each lagged variable. This is an optional tab; the default is to not apply lag."),
  h3("3. Build model"),
  p("Build a joint probability model in two stages:"),
  p(tags$ul(  # bullet points
    tags$li("Create the dependence structure (a set of statistical relationships that model how likely each pair of variables are to be extreme at the same time)."),
    tags$li("Simulate a set of synthetic extreme events from these relationships that will be used to estimate the joint probability of an event you want to analyse.")
  )),
  h3("4. Marginal analysis"),
  p("View the marginal distribution automatically determined by the MEM as an annual exceedance probability curve. 
    There is also an option to input your own marginal parameters from a generalised logistic distribution, which can then 
    be used in the joint probability analysis."),
  p("Note that if the option to load data sampled for peak surge events has been chosen then the MEM will not automatically 
    determine annual exceedance probabilities. However, you may still supply your own marginal models."),
  h3("5. Joint probability analysis"),
  p("Enter a combination of values for each of your variables and visualise this event in comparison with both your original data and with the model."),
  p("The joint probability of the combination of values can be calculated."),
  h3("6. Export"),
  p("Download results from the joint probability analysis."),
  ### Set up conditional panel for initial view before any files have been uploaded or the 'Load saved model' button has been clicked
  conditionalPanel("!input.savedMod && !output.fileUploaded",
                   hr(),  # horizontal line
                   h2("Create or load model"), 
                   p("Please select whether you would like to create a new joint probability model or load a previously-created model:"),
                   tags$head(  # change background colour of button
                     tags$style(HTML('#newModel{background-color:#74C4D7}'))
                   ),
                   # Action button to click to create a new joint probability model (move to next tab)
                   actionButton("newModel", "Create a new joint probability model"),
                   # Action button to click to bring up the functionality to load a saved model
                   actionButton("savedMod", "Load saved model")#,
  ),
  
  ### Set up conditional panel for if the 'Load saved model' button has been clicked and no files have been uploaded
  conditionalPanel("input.savedMod && !output.fileUploaded",
                   hr(),
                   # Set up a file input to allow the user to upload their data
                   fileInput('savedModel', h4('Load saved model:'),  
                             accept=c('.RDS'))
  ),
  
  ### Set up conditional panel for if the 'Load saved model' button has been clicked and data have been uploaded
  conditionalPanel("input.savedMod && output.fileUploaded",
                   hr(),
                   ### Write out the name of the uploaded model
                   strong(uiOutput("savedModelFileName"))
  ),
  
  hr(),  # horizontal line
  h2("Acknowledgements"),
  p("The Multivariate Event Modeller was initially developed as part of Environment Agency project SC140002 (Spatial Joint Probability for FCRM and National
    Risk Assessment), within the Defra/Environment Agency/Natural Resources Wales Joint Flood and Coastal Risk Management R&D programme.")
)  # end of tab

########## Tab 1 - Input data ##########
tab.Panel.DataDiagnostics <- tabPanel(
  "1 Input data",  # label the tab
  ### Set up a conditional panel based on this tab being selected and no file uploaded and no selection to load a saved model
  conditionalPanel(condition = "input.tabsetPanelID=='1 Input data' && !output.fileUploaded && !input.savedMod",
                   br(),
                   p("Format requirements for input data are set out on the 'Overview' tab. Please select from the options 
                      below:"),
                   ### Set up radio buttons for selecting data type
                   radioButtons("dataType", label = "Data type", choices = c("Time series data sampling all observations" = "ts", "Observations sampled at peak surge events" = "declust")),
                   ### Set up a file input to allow the user to upload their data
                   uiOutput("fileUpload"),
                   p("If you would like to load a previously created model in .RDS format, please use the \"Load saved model\" button on the overview tab."),
                   ### Set up a conditional panel based on the uploaded file containing too many variables
                   conditionalPanel(condition = "output.fileTooManyDim",
                                    p("You have uploaded a file with more than 10 variables. Please upload
                                      a file with 10 variables or fewer."))
  ),
  
  ### Set up a conditional panel to give a warning if the user selected to load a saved model but no file has been uploaded
  conditionalPanel(condition = "input.tabsetPanelID=='1 Input data' && !output.fileUploaded && input.savedMod",                 
                   br(),
                   p("You selected to load a saved model on the previous tab. Please refresh the MEM to input new data.")
  ),
  
  ### Set up a conditional panel based on the wrong file type
  conditionalPanel(condition = "input.tabsetPanelID=='1 Input data' && output.fileUploaded && !output.fileTypeCSV && !output.savedModelUploaded",
                   br(),
                   p("You have uploaded an incompatible file type. Please refresh the MEM and input data as a .csv file.")),
  
  ### Set up a conditional panel based on a file being uploaded
  conditionalPanel(  
    condition = "input.tabsetPanelID=='1 Input data' && output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded)", 
    br(),    
    p("The plots below show the time series data that you have uploaded for each variable. The horizontal line shows the threshold
      above which data points are treated as extreme. Data above the threshold will be modelled using the conditional extremes 
      joint probability approach. The plot labels and title can be edited using the panel on the left-hand side."),
    p("The joint probability analysis relies upon the quality of the input data being as good as possible, especially for extreme values above the threshold. 
      Guidance on assessing data quality can be found in the Environment Agency’s Flood Estimation Guidelines."),
    hr(),  # add horizontal line
  
  
    ### Set up a conditional panel to provide an option for selecting the threshold percentile (with a default of 97.5%)
    ### if the data type is time series
    conditionalPanel(condition = "input.dataType=='ts'", {
                  div(style = "margin-top: 0px",
                      numericInput("threshold", label = "Select threshold percentile", value = 0.975, min=0, step=0.005))
    }),
    
    ### Set up a conditional panel to select threshold percentile and enter number of years of data if data type is declustered
    conditionalPanel(condition = "input.dataType == 'declust'", {
      fluidRow(
        column(6,
               div(style = "margin-top: 0px",
                   numericInput("numYearsDeclust", label = "Enter length of observed record in years", value = NA, min=0, step=1))
        ),
        column(6,
               div(style = "margin-top: 0px",  # set min to 0.55 - don't want less than 0.5 due to theory and 0.5 is 0 on the Laplace scale
                   numericInput("thresholdDeclust", label = "Select threshold percentile", value = 0.975, min=0.55, step=0.005))
        )
      )
    }),
    
    ### Next row
    fluidRow(
      ### Give warning if data look inconsistent with selected data type
      strong(span(textOutput("looksTS"), style = "color:red")),
      strong(span(textOutput("looksDC"), style = "color:red")),
      br(),
        ### Plot time series data
        plotlyOutput("timeseries", height = "100%")
      )
  )
)

########## Tab 2 - Time lag ##########
tab.Panel.Lag <- tabPanel(
  "2 Time lag",  # label the tab
  ### Set up a conditional panel based on a file being uploaded and it not being declustered
  conditionalPanel(
    condition = "input.tabsetPanelID=='2 Time lag' && output.fileUploaded && input.dataType !== 'declust' && (output.fileTypeCSV || output.savedModelUploaded) ",
    br(),
    p("This tab allows a time lag to be added to one or more of your input data series. This applies a constant time shift to the whole record. 
      This could be used if you wish to account for the lag between river gauges that tend to peak on different days, for example. 
      The lagged data are used in the analysis in the following tabs if you tick the checkbox. Only the remaining overlapping data are used."),
    p("The plots below show the time series data you uploaded for each variable and will adjust automatically depending on the lag in the table. The horizontal line shows the threshold
      above which data points are treated as extreme; the threshold adjusts when lag is applied. Data above the threshold will be modelled using the conditional extremes joint probability approach."),
    hr()  # add horizontal line
  ),
  
  ### Next row
  conditionalPanel( condition = "input.tabsetPanelID=='2 Time lag' && output.savedModelUploaded && input.dataType !== 'declust' && (output.fileTypeCSV || output.savedModelUploaded) ", 
    ### Plot time series data if a saved model has been uploaded
    plotlyOutput("timelag_savedModel", height = "100%")
  ),
  
  ### Next row
  conditionalPanel( condition = "input.tabsetPanelID=='2 Time lag' && !output.savedModelUploaded && input.dataType !== 'declust' && (output.fileTypeCSV || output.savedModelUploaded) ", 
                    ### Plot time series data if a saved model has not been uploaded
                    plotlyOutput("timelag", height = "100%")
  )
)


########## Tab 3 - Build model ##########
tab.Panel.StepTwo <- tabPanel(
  "3 Build model",  # label the tab
  br(),
  p("This tab shows the relationships between each pair of variables that you have uploaded. The plot controls on the 
    left allow you to choose whether to view your data on its original measurement scale (e.g. water level or flow rate), 
    or on a mathematically transformed scale that emphasises the extreme values and standardises across differences
    in the absolute physical scale. If simulated data are included on the plot, only the lagged date will be displayed 
    when hovering  over the data in the lower plot."),
  p("The panel on the right hand side of the plots guides you through the process of creating a joint probability model."),
  hr(),  # add horizontal line
  
  ### Split the next row into two columns; one for plotting and one for model building
  ### Column containing plots
    column(8, 
           
           ### Set up conditional panel if the wrong file type has been uploaded
           conditionalPanel("!output.fileUploaded && !output.fileTypeCSV && !output.savedModelUploaded",
                            fluidRow(
                              wellPanel(
                                "You have uploaded an incompatible file type. Please refresh the MEM and input data as a .csv file."
                              )
                            )
           ),
           
           ### Set up a conditional panel based on the correct file type being uploaded
           ### but the simulation not being complete
           conditionalPanel("output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded) && !output.HTsimDone  && !output.HTsimDoneTwo",
                fluidRow(
                  wellPanel(
                    # Plot the initial pairs plot (scatter plots) for all variables using the observed data
                    plotOutput("pairs")  
                  )
                ),
           
                fluidRow(
                  wellPanel(
                    # Plot the initial interactive scatter plot using the observed data
                    plotlyOutput("pair_interactive"), 
                    br(),
                    textOutput("chibar")  # position Chi output below the plots
                  )
                )
           ),
           
          ### Set up a conditional panel based on the simulation being complete (including from 
          ### loading a saved model - second condition)
          conditionalPanel("(output.HTsimDone || output.HTsimDoneTwo)",  
                 fluidRow(
                   wellPanel(
                     # Plot the pairs plot (scatter plots) for all variables using the observed and simulated data
                     plotOutput("pairs_postSim")   
                   )
                 ),

                 fluidRow(
                   wellPanel(
                     # Plot the interactive scatter plot using the observed and simulated data
                      plotlyOutput("pair_interactive_postSim"),
                      br(),
                      textOutput("chibar2")  # position Chi output below the plots
                    )
                 )
          )
    ),  # end of column
  
    ### Column containing the model build
    column(4,
           ### Set up a conditional panel to give a warning if data type is declustered but the user has not 
           ### entered the number of years of data 
           conditionalPanel("input.dataType == 'declust' && !input.numYearsDeclust",
                           p(strong("Please enter the length of the observed record in years on the 'Input data' tab 
                                    before creating the model.")), style = "color:red"),
           
           ### Set up a conditional panel to show the first step of the model build if time series data have been
           ### uploaded (with the correct file type) or declustered data have been uploaded (with the correct file 
           ### type) and the number of years has been entered
           conditionalPanel("output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded) && (input.dataType == 'ts' || (input.dataType == 'declust' && input.numYearsDeclust))",
                           p("The first step in creating a joint probability model is to fit a set of relationships between each pair of variables.
                           This creates a model of the dependence structure."),
                           p("If your data contains erroneous outliers then the fitting function may fail and produce an error.
                             If this happens, you will need to check your data and remove the erroneous value(s)."),
                           # Only show the button if a dependence model has not already been created or loaded via a saved model
                           conditionalPanel(condition = "!input.Step2model && !output.savedModelUploaded", 
                                            p("Note that once the button has been pressed, then you can no longer modify your lag settings."),
                                            actionButton("Step2model", "Create dependence structure")  # action button to click to model the data
                                            ),  
                           uiOutput('HTmodelDone'),  # ui output to act as an indication of whether the modelling has finished - not a visible output
                           hr()  # add horizontal line
                           ),
           
           ### Set up a conditional panel to appear once the dependence structure has been created (or loaded from a saved model)
           conditionalPanel("output.HTmodelDone || output.HTmodelDoneTwo",
                          p("Dependence structure created."),
                          strong(span(textOutput("convergenceWarning"), style ="color:red")),  # give warning if the model has not converged
                          hr(),  # add horizontal line
                          p("The second step is to simulate a large random sample of extreme events, using the
                          model to extrapolate beyond the range of your data. The model will simulate events within a long, synthetic period
                          of record. A larger simulated sample will give you a more precise
                            analysis of joint probabilities, but there is a trade-off because a large simulation may also take some time. 
                            We suggest simulating a minimum of 10,000 years of synthetic events."),
                          strong(span(textOutput("eventYearsWarning"), style = "color:red")),
                          br(),
                          # Only show the simulation button if an event set has not already been simulated or loaded via a saved model
                          conditionalPanel(condition = "!input.Step2eventset && !output.savedModelUploaded",
                                           # Add input for users to enter the number of years of data they would like to simulate
                                           numericInput("eventYears", "Enter number of years to simulate", 1000, min = 1000, max = 10000000, step = 1000),
                                           p("Note that once the button has been pressed you will not be able to simulate a different number of years without restarting the MEM tool."),
                                           actionButton("Step2eventset", "Simulate events")  # action button to click to simulate the event set
                                           ),  
                          uiOutput('HTsimDone')  # ui output to indicate when the simulation is complete - not a visible output
                          ),
         
           ### Set up a conditional panel to appear once the simulation is complete (or loaded from a saved model)
           conditionalPanel("output.HTsimDone || output.HTsimDoneTwo",
                   hr(),  # add horizontal line
                   p("Event simulation completed. If you would like to simulate a different number of years, you must refresh 
                     the MEM tool and upload your data as a .csv file."), 
                   hr(),
                   p("The plots have been updated to include the events simulated using the conditional extremes model.
                     The cloud of simulated points should look similar to the cloud of observed points, but should extend
                     beyond the range of the original data set so as to include more extreme events than in the observations."),
                   p("The joint probability analysis will require some of the simulated data to be more extreme than events you
                     wish to analyse. If this is not the case, you can repeat the simulation for a larger number of years."),
                   ### Add option to save model (dependence structure and simulation outputs)
                   div(id = "savePanel",
                   wellPanel(
                    h4("Save model"),
                    p("This panel can be used to save the model and simulated events for loading back in later."),
                    # Action button to click to select a directory
                    actionButton('directory', label='Choose location for saved model'),  
                    br(),br(),  # line breaks
                    # Text input to write folder path for saving model
                    textInput("saveModelFolder", label="Destination folder to save model:", placeholder = "Enter destination folder or choose above"),
                    # Text input to write filename for saved model
                    textInput("saveModelName", label="Filename for saved model:", placeholder = "Enter filename for saved model, e.g. \"Model 1\""),
                    # Action button to click to save the model
                    actionButton("ActionButtonSaveModel", "Save model now"),
                    br(),br(),
                    textOutput('tO_ModelSaveFeedback')  # text output saying whether the model has been saved or whether there has been an error
                  )),
                  div(id = "savePanel2",
                      wellPanel(
                        h4("Save model"),
                        p("Click below to save the model and simulated events for loading back in later."),
                        # Action button to click to select a directory
                        downloadLink("saveProject", "Save model"),
                        br()
                      ))
          )  # end of conditional panel 
    )  # end of column 
)  # end of tab

############ Tab 4 - Marginal analysis ############
tab.Panel.Marginals <- tabPanel(
  "4 Marginal analysis",  # label the tab
  br(),
  p("On this tab you can view the marginal distribution used in the joint probability analysis. The automatic marginal analysis in the 
    MEM fits a generalised Pareto distribution to peaks over threshold data for each variable. This tab additionally gives 
    you the option to input your own marginal parameters from a generalised logistic distribution (e.g. the marginal parameters 
    obtained from a Flood Estimation Handbook flood frequency analysis)."),
  p("The two choices of marginal analysis can be compared by plotting the annual exceedance probability curves. Either 
    of the two marginal analyses can be used in the 'Joint probability analysis' tab."),
  conditionalPanel("input.dataType=='declust'",
                   p("You are analysing data sampled on peak surge events. The MEM will not automatically determine a
                     marginal model for annual exceedance probabilities (and hence the plot below will be blank). 
                     However, you may supply a marginal model by ticking the box below.")
                   ),
  hr(),  # horizontal line
  ### Set up conditional panel for the whole tab based on simulation outputs existing
  conditionalPanel("output.HTsimDone || output.HTsimDoneTwo",
                   ### Main panel ###
                   ### First column contains plot
                   column(8,
                          wellPanel(
                            plotlyOutput("AEPcurve")  # output for AEP curve
                          )
                   ),
                   ### Second column contains option for including user-supplied marginal parameters
                   column(4,
                          conditionalPanel("input.dataType=='ts'",
                                           p("Tick the box if you would like to enter your own marginal parameters from a generalised logistic
                                             distribution. If the box is unticked the existing marginal analysis will be used in the joint 
                                             probability analysis.")
                                           ),
                          conditionalPanel("input.dataType=='declust'",
                                           p("Tick the box if you would like to enter your own marginal parameters from a generalised logistic
                                             distribution.")
                                           ),
                          # Add checkbox for user to select whether to input and use their own parameters
                          checkboxInput("showTable", "Input and use your own marginal parameters"),
                          # Set up conditional panel for if the checkbox has been ticked
                          conditionalPanel("input.showTable == true",
                                           strong("Values should be numeric. Values for scale should be greater than 0, and values for shape should be greater than -1 and less than 1.
                                                  Values outside these limits will not be used in analysis."),
                                           rHandsontableOutput("userMarginals"),  # output the user marginals table
                                           hr(),  # horizontal line
                                           p("Click the button to update the simulation outputs to use your generalised logistic marginal 
                                             parameters. You will need to click the button again if you change your marginal parameters."),
                                           # Action button to click to update simulation outputs to be consistent with user-supplied marginals 
                                           actionButton("useUserMarginals", "Update simulation outputs"),
                                           ### Give warning message if the user tries to update the simulation outputs without inputting any marginal parameters
                                           conditionalPanel("input.useUserMarginals && !input.userMarginals.changes.changes", 
                                                            strong(span("Please enter marginal parameters for all gauges or update your existing parameters.", style ="color:red"))),
                                           ### Conditional panel based on the simulation outputs having been updated
                                           conditionalPanel("output.HTuserMarginalsDone || output.HTuserMarginalsDoneTwo",
                                                            br(),
                                                            p("The simulated data have been updated."),
                                                            ### Add option to save model with the user marginals data
                                                            div(id = "savePanelMarginal",
                                                              wellPanel(
                                                              h4("Save model"),
                                                              p("This panel can be used to save your marginal parameters and the updated simulation 
                                                                outputs as well as the outputs from the 'Build model' tab for loading back in later."),
                                                              # downloadLink("saveProject2", "Save model"),
                                                              # Action button to click to select a directory
                                                              actionButton('directory3',  label='Choose location for saved model'),
                                                              br(),br(),
                                                              # Text input to write folder path for saving model
                                                              textInput("saveModelFolder3", label="Destination folder to save model:", placeholder = "Enter destination folder or choose above"),
                                                              # Text input to write filename for saved model
                                                              textInput("saveModelName3", label="Filename for saved model:", placeholder = "Enter filename for saved model, e.g. \"Model 1\""),
                                                              # Action button to click to save the model
                                                              actionButton("ActionButtonSaveModel3", "Save model now"),
                                                              br(),br(),
                                                              textOutput('tO_ModelSaveFeedback3')  # text output saying whether the model has been saved or whether there has been an error
                                                            )),
                                                            div(id = "savePanelMarginal2",
                                                                wellPanel(
                                                                  h4("Save model"),
                                                                  p("Click below to save your marginal parameters and the updated simulation 
                                                                outputs as well as the outputs from the 'Build model' tab for loading back in later."),
                                                                  # Action button to click to select a directory
                                                                  downloadLink("saveProject2", "Save model"),
                                                                  br()
                                                                ))
                                           )  # end of conditional panel for once simulation outputs have been updated
                          )  # end of conditional panel for if checkbox is ticked
                   )  # end of column
  )  # end of conditional panel for whole tab
)  # end of tab

########### Tab 5 - Joint probability analysis ###########
tab.Panel.StepThree <- tabPanel(
  "5 Joint probability analysis",  # label the tab
  br(),
  conditionalPanel("input.dataType=='ts'",
                   p("On this tab you can assess the joint probability that a set of extreme values in each of your 
                     variables could be exceeded in any one event. You can define an event by entering values
                     of either the AEP (%) or the physical measurement for each of your variables in the table below. 
                     The other column will automatically update and the physical measurement values are shown as lines 
                     on the plots when viewing on the original measurement scale. The grey boxes on the plots indicate 
                     the non-extreme regions for each pair of variables."),
                   p("The MEM will compare your specified event to the events simulated from the model and use this 
                     information to estimate the joint probability. It will do this by calculating the proportion of 
                     years in which simulated events are more extreme than the values you enter in the table.")
                   ),
  conditionalPanel("input.dataType=='declust'",
                   p("On this tab you can assess the joint probability that a set of extreme values in each of your 
                     variables could be exceeded in any one event. You can define an event by entering 
                     values of the physical measurement for each of your variables in the table below. If you have 
                     defined marginal models (see the 'Marginal analysis' tab) then you will also be able to see and set 
                     the corresponding annual exceedance probabilities. The physical measurement values are shown 
                     as lines on the plots when viewing on the original measurement scale. The grey boxes on the 
                     plots indicate the non-extreme regions for each pair of variables."),
                   p("The MEM will compare your specified event to the events simulated from the model and use this 
                     information to estimate the joint probability. It will do this by calculating the proportion of 
                     years in which simulated events are more extreme than the values you enter in the table.")
                   ),
  hr(),  # horizontal line
  ### Set up first column to contain plots
  column(8,
         ### Add a warning conditional on the marginal analysis tab checkbox being selected but no updated simulation outputs existing (including from a saved model)
         conditionalPanel("input.showTable == true && !output.HTuserMarginalsDone && !output.HTuserMarginalsDoneTwo",
                          p(strong("You have selected to use your own marginal parameters for the joint probability
                                   analysis. Please ensure that you have entered parameters for all gauges and 
                                   updated the simulation outputs (a message will appear on the previous tab
                                   once this has been completed). To use the default marginal parameters, untick the 
                                   ‘Input your own marginal parameters’ checkbox on the previous tab."))),
         ### Conditional panel based on simulation data existing and user marginals not selected or user marginals selected and updated simulation outputs exist (including from a saved model)
         conditionalPanel("(output.HTsimDone || output.HTsimDoneTwo) && (input.showTable == false || input.showTable == true && (output.HTuserMarginalsDone || output.HTuserMarginalsDoneTwo))",                  
                          ### Set up first row to be the pairs plot (scatter plots) for all variables   
                          fluidRow(
                            wellPanel(
                              plotOutput("pairs_postSim_step3")
                            )
                          ),
                          ### Set up next row for the interactive scatter plot  
                          fluidRow(
                            wellPanel(
                              plotlyOutput("pair_interactive_postSim_step3")
                            )
                          )
         )  # end of conditional panel
  ),  # end of column
  
  ### Set up second column to contain the joint probability analysis options
  column(4,
         conditionalPanel("input.dataType=='ts'",
         p("Enter a combination of annual exceedance probabilities as percentages or physical measurement values in the table to 
           specify the event you want to analyse. Use NAs for variables for which you have no data. The input value or value associated 
           with the AEP will be shown on the plots when viewing on the original measurement scale. When this tab first loads, each 
           variable is set to its minimum value in the original inputs and the corresponding AEP defaults to an initial estimate of 99.99%."),
         br(),
         p("Note that you cannot set AEPs to be more than 99.9%.")),
         conditionalPanel("input.dataType=='declust'",
                          p("Enter a combination of physical measurement values in the table to specify the event you want to analyse. 
                            Use NAs for variables for which you have no data. If you have specified a marginal model then you can also see and 
                            set the corresponding AEPs (when this tab first loads, each variable is set to its minimum value in the original 
                            inputs and the corresponding AEP defaults to an initial estimate of 99.99%).")),
         ### Set up conditional panel to give a warning message if selected to use user-supplied marginals but there are no marginal parameters
         ### (if simulation has just been created and no user marginals or if a model has been loaded and there are no user marginals entered or in the saved model),
         conditionalPanel("(output.HTsimDone && !output.HTsimDoneTwo && input.showTable == true && !input.userMarginals) || 
                  (output.HTsimDoneTwo && input.showTable == true && !input.userMarginals && !output.HTuserMarginalsTableTwo)",        
                 p(strong("You have selected to use your own marginal parameters but have not
                                  entered any into the table on the previous tab."))),
         ### Set up conditions for showing the joint probability table
         ### (if simulation outputs exist and not using user marginals 
         ### or simulation outputs exist and using user marginals and they exist
         ### or a model has been loaded containing simulation outputs and user marginals)
         conditionalPanel("((output.HTsimDone || output.HTsimDoneTwo) && input.showTable == false) || 
                        ((output.HTsimDone || output.HTsimDoneTwo) && input.showTable == true && input.userMarginals) ||
                        (output.HTsimDoneTwo && input.showTable == true && output.HTuserMarginalsTableTwo)",
                        rHandsontableOutput("hot")
         ),
         br(),
         br(),
         p("Click the button to calculate the joint probability between the gauges for which you have entered data."),
         p("Remember to click the button again to calculate new joint probabilities if you simulate a different number
           of events, or if you change any values in the table or change any options in the 'Marginal analysis' tab."),
         # Action button to click to calculate joint probability
         p(actionButton("JPgo", "Calculate joint probability")),
         br(),
         ### Set conditions for displaying joint probability calculation outputs
         conditionalPanel(condition = "input.JPgo",
                         wellPanel(
                           p(htmlOutput("JPout", inline = TRUE)),
                           tableOutput('encounterProbs')
                         )
         )
  )  # end of column 
)  # end of tab

########### Tab 6 - Export results ###########
tab.Panel.StepFour <- tabPanel(
  "6 Export results",  # label for tab
  br(),
  ### Set up option for saving event set
    div(id = "exportPanel",
      wellPanel(
       h4("Save event set"),
       p("This panel can be used to save the event set. This will be the automatically-simulated event set unless you have 
         selected to use user-supplied marginal parameters, in which case it will be the updated event set which is consistent
         with your generalised logistic parameters. This is based on whether the 'Input and use your own marginal parameters' 
         box is ticked on the 'Marginal analysis' tab."),
       # Action button to click to select a directory
       actionButton('directory2', label='Choose location for saved model'),
       br(),br(),
       # Text input to write folder path for saving model
       textInput("saveModelFolder2", label="", placeholder = "Enter destination folder or choose above"),
       # Text input to write filename for saved model
       textInput("saveModelName2", label="", placeholder = "Enter filename for saved model, e.g. \"Model 1\""),
       # Action button to click to save the model
       actionButton("ActionButtonSaveModel2", "Save event set"),
       br(),br(),
       textOutput('tO_ModelSaveFeedback2'),  # text output saying whether the model has been saved or whether there has been an error
       br()
     )),
    div(id = "exportPanel2",
        wellPanel(
                  h4("Save event set"),
                  p("Click below to save the event set. This will be the automatically-simulated event set unless you have 
                    selected to use user-supplied marginal parameters, in which case it will be the updated event set which is consistent
                    with your generalised logistic parameters. This is based on whether the 'Input and use your own marginal parameters' 
                    box is ticked on the 'Marginal analysis' tab."),
                  # Action button to click to select a directory
                  downloadLink("saveEventSet", "Save event set"),
                  br()
                  ))
)

########### Tab 7 - FAQs ###########
tab.Panel.Help <- tabPanel(
  "FAQs", #label for tab
  br(),
  h2("Frequently Asked Questions"),
  h4("Q: Why should the input data not contain missing values and how can I deal with missing data?"),
  p("The joint probability method requires pairs of concurrent observations to be compared, otherwise no information is available about dependence. Time series with missing data will therefore require those periods to be infilled (for example, by hydrological continuous simulation modelling or regression models) before use in the joint probability analysis."),
  h4("Q: Why is there a limitation of 10 variables?"),
  p("The choice of 10 dimensions was agreed as a compromise between allowing a flexible exploration of joint probabilities and the computational demands of working with higher dimensional spaces, which increase dramatically with additional dimensions."),
  h4("Q: Can the MEM provide a range of combinations for a specified joint probability?"),
  p("This is not a feature of the MEM and would require much more involved computation, since a very large number of combinations of marginal values could exist for any one, specified joint probability."),
  h4("Q: Can I specify an AEP at one site and simulate a scenario at the other gauges?"),
  p("The MEM does not provide a scenario based on an AEP being specified at one site. There are likely to be many potential scenarios and these can be explored outside the MEM using the exported simulated event set. The relevant column can be sorted and simulated events above the required threshold can be selected."),
  h4("Q: What if I get an unrealistic joint probability?"),
  p("An unrealistic joint probability value may occur where extreme events are not easily defined, such as in chalk catchments, where an extreme event may last for months. This affects the dependence structure and hence the joint probability. The MEM may not be suitable for this type of data (also see next question)."),
  h4("Q: Can the MEM handle all types of hydrological data? "),
  p("The MEM can handle a number of hydrological datasets; it assumes that, for a given variable, the duration of extreme events will last for a maximum of a week. This choice of window of a week is consistent with the analysis performed in the spatial joint probability for FCRM and NRA work. If events typically last longer than a week, the MEM will be insufficient in capturing the extremal behaviour of these time series."),
  p("An example of a hydrological time series that would require further interpretation for use with the MEM is data from chalk catchments; important characteristics of these data are that the catchments are responding slowly and events are hard to define from solely analysing the hydrograph. If the MEM is used to model data from a chalk catchment, the resulting joint probability calculation may be inappropriate or not a good descriptor of the particular sources of flood risk in the catchment. A likely reason for this is that the return periods for the data are poorly estimated or that 'extreme events' are not well-defined."),
  p("One potential solution for modelling chalk catchments is to de-trend the data before performing a joint probability analysis (for example, removing any seasonality from the data)."),
  p("If the MEM is used, it is recommended that the user supplies their own marginal parameters as the marginals determined automatically within the MEM tool may be incorrect.")
)

########## Combine the tabs into a tabset panel ##########
tabset.Panel <- tabsetPanel(id="tabsetPanelID",
                            tab.Panel.README,
                            tab.Panel.DataDiagnostics,
                            tab.Panel.Lag,
                            tab.Panel.StepTwo,
                            tab.Panel.Marginals,
                            tab.Panel.StepThree,
                            tab.Panel.StepFour,
                            tab.Panel.Help,
                            selected = "Overview"  # select which tab will appear when app is loaded 
)


########## Set up the UI using the above tabs and a title panel and a well panel ##########
### Set most of the tabs to show as disabled initially
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$script("
        window.onload = function() {
                        $('#tabsetPanelID a:contains(\"2 Time lag\")').parent().addClass('disabled');
                        $('#tabsetPanelID a:contains(\"3 Build model\")').parent().addClass('disabled');
                        $('#tabsetPanelID a:contains(\"4 Marginal analysis\")').parent().addClass('disabled');
                        $('#tabsetPanelID a:contains(\"5 Joint probability analysis\")').parent().addClass('disabled');
                        $('#tabsetPanelID a:contains(\"6 Export results\")').parent().addClass('disabled');
                        $('#tabsetPanelID a:contains(\"FAQ\")').parent();
                        };
                        
                        Shiny.addCustomMessageHandler('activeTabs', function(tab_label) {
                        $('#tabsetPanelID a:contains(\"' + tab_label + '\")').parent().removeClass('disabled');
                        });
                        ")),

  titlePanel("", windowTitle = "Multivariate Event Modeller"),  # set the text to display in the tab in the web browser 
  br(),
  
  ### Set up the app with two columns; a side column of width 3/12 which contains the plotting controls 
  ### and a main panel of width 9/12 which contains the plots and analysis steps (the tabs set out above)
  
  fluidRow(
    column(3, 
           ### Set up the panel in the top left corner
           wellPanel(style = "background-color: #74C4D7",
                     h1("Multivariate Event Modeller", align = "center"),
                     hr(),
                     textInput("projectName", "Project name:"),
                     textInput("projectOwner", "Project owner:")
           ),
           
           ### Set up conditional panel for if the 'Input data' tab is selected and data have been uploaded in correct file format
           conditionalPanel(condition = "input.tabsetPanelID=='1 Input data' && output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded)",
                        strong(uiOutput("inFileName")),  # write the name of the data set uploaded
                        hr(),  # horizontal line
                        h3("Summary statistics"),
                        h5(textOutput("numYears")),  # output the effective number of years
                        h5(textOutput("numVar")),  # output the number of variables 
                        # Output the number of independent events
                        # For time series data sampling all observations
                        conditionalPanel("input.dataType=='ts'",
                                         tableOutput("thrExceedTS")
                                         ),
                        # For observations sampled at peak surge events (declustered data), output the number of threshold exceedances based on the selected threshold 
                        conditionalPanel("input.dataType=='declust'",
                                         h5(textOutput("thrExceedDC"))
                                         ),

                        h3("User defined labels"),
                        p("This section allows you to adjust the labels and title for the graphs. Labels set here will be
                          used in the following tabs."),
                        p("You can format your user defined labels using html tags. 
                          For example, you can write", HTML(paste0("m", tags$sup("2"))), 
                          "as \"m<sup>2</sup>\",", HTML(paste0("m", tags$sup("3"), "/s")), 
                          "as \"m<sup>3</sup>/s\" or", HTML(paste0("x", tags$sub("2"))), 
                          "as \"x<sub>2</sub>\". For a new line, use the <br> tag, for 
                          example \"A really <br> long label\"."),
                        textInput("userDefinedInputGraphTitle", label = "Input data graphs title", value = "Input data"),
                        
                        uiOutput('userDefinedValueLabels'),
                      ), 

           
           ### Set up conditional panel for if the 'Time lag' tab is selected, no saved model has been uploaded, the data is not declustered, and the "Create dependence structure" button has not been pressed
           conditionalPanel(h3("Add lag to the time series"),
                            p("Enter a combination of time lags quantified in days. Use 0 for variables to which you do not wish to apply lag. The graphs on the right will be updated automatically with your inputted values, however, ", 
                            strong("you must tick the checkbox to use these values in the remaining tabs for your analysis.")),
                            condition = "input.tabsetPanelID=='2 Time lag' && output.fileUploaded && input.dataType !== 'declust' && !input.Step2model && !output.savedModelUploaded", rHandsontableOutput("lag"), #rhandsontable to allow lagging of the data
                            # Add checkbox for user to select whether to input and use their own parameters
                            checkboxInput("lagTick", "Use lagged data for analysis")
                            ),
           
           ### Set up conditional panel for if the 'Time lag' tab is selected, no saved model has been uploaded, data is not declustered, tick box has been ticked, and the "Create dependence structure" button has not been pressed
           conditionalPanel(h3("Add lag to the time series"),
                            condition = "input.tabsetPanelID=='2 Time lag' && input.dataType !== 'declust' && input.Step2model && input.lagTick && !output.savedModelUploaded", rHandsontableOutput("lagAfter"), #rhandsontable to allow lagging of the data
                            p("You have opted to use these lag values for your analysis. Please refresh the MEM tool to use different lag values or no lag."),
           ),
           
           ### Set up conditional panel for if the 'Time lag' tab is selected, no saved model has been uploaded, data is not declustered, tick box has not been ticked, and the "Create dependence structure" button has been pressed
           conditionalPanel(h3("Add lag to the time series"),
                            condition = "input.tabsetPanelID=='2 Time lag' && input.dataType !== 'declust' && input.Step2model && !input.lagTick",
                            p("You have opted not to use lag for your analysis; to use lag, please refresh the MEM tool."),
           ),
           
           ### Set up conditional panel for if the 'Time lag' tab is selected, saved model has been uploaded and the lag tick box has been ticked
           conditionalPanel(h3("Add lag to the time series"),
                            condition = "input.tabsetPanelID=='2 Time lag' && input.dataType !== 'declust' && output.savedModelUploaded && input.lagTick", rHandsontableOutput("lagSaved"), #rhandsontable to allow lagging of the data
                            p("You have opted to use these lag values for your analysis. Please create a new model to use alternative values."),
           ),
           
           ### Set up conditional panel for if the 'Time lag' tab is selected, saved model has been uploaded and the lag tick box has not been ticked
           conditionalPanel(h3("Add lag to the time series"),
                            condition = "input.tabsetPanelID=='2 Time lag' && input.dataType !== 'declust' && output.savedModelUploaded && !input.lagTick", #rhandsontable to allow lagging of the data
                            p("You have opted not to use lag for your analysis; to use lag, please create a new model."),
           ),
           
           conditionalPanel(condition = "input.tabsetPanelID=='2 Time lag' && output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded)&& input.dataType !== 'declust' ",
                            h3("User defined labels"),
                            p("This section allows you to adjust the title for the graphs."),
                            p("You can format your user defined labels using html tags. 
                              For example, you can write", HTML(paste0("m", tags$sup("2"))), 
                              "as \"m<sup>2</sup>\",", HTML(paste0("m", tags$sup("3"), "/s")), 
                              "as \"m<sup>3</sup>/s\" or", HTML(paste0("x", tags$sub("2"))), 
                              "as \"x<sub>2</sub>\". For a new line, use the <br> tag, for 
                              example \"A really <br> long label\"."),
                            textInput("userDefinedLagGraphTitle", label = "Lagged data graphs title", value = "Lagged data"),),
           
           
           
           ### Set up conditional panel for if the 'Build model' tab is selected and data have been uploaded in
           ### the correct file type but no simulation outputs exist
           conditionalPanel(condition = "input.tabsetPanelID=='3 Build model' && output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded) && !output.HTsimDone && !output.HTsimDoneTwo",
                            # Provide option for user to change the scale for viewing the data
                            h3("Plot controls"),
                            selectInput("Scale", label = h4("Select plotting scale"), choices = list("Original scale" = "Orig", "Standardised scale" = "Std"), selected = 1),
                            br(),
                            h4("Inspect variables"),
                            # Split the row into two columns for the pair controls which are used for selecting which variables to plot on the interactive plot 
                            fluidRow(
                              column(6,
                                     uiOutput("pairControls1")),
                              column(6,
                                     uiOutput("pairControls2"))
                            )
           ),
           
           ### Set up conditional panel for if the 'Build model' tab is selected and simulation outputs exist
           conditionalPanel(condition = "input.tabsetPanelID=='3 Build model' && (output.HTsimDone || output.HTsimDoneTwo)",
                            # Provide option for user to change the scale for viewing the data (post simulation)
                            h3("Plot controls"),
                            selectInput("ScalePostSim", label = h4("Select plotting scale"), choices = list("Original scale" = "Orig", "Standardised scale" = "Std"), selected = 1),
                            # Set up a checkbox group which allows the user to choose which data to plot (observed or simulated)
                            checkboxGroupInput("origSimData", 
                                   label = h4("Display options"), 
                                   choices = list("Observed data" = "origData", 
                                                  "Simulated data" = "simData"), selected = c("origData", "simData")),
                            br(),
                            h4("Inspect variables"),
                            # Split the row into two columns for the pair controls which are used for selecting which variables to plot on the interactive plot  
                            fluidRow(
                              column(6,
                                     uiOutput("pairControls1_postSim")),
                              column(6,
                                     uiOutput("pairControls2_postSim"))
                            ),
                            # Add a slider input for the user to select the percentage of the data to plot
                            sliderInput("pcThinned", label = h4("Data thinning"), min = 5, max = 100, value = 10, step = 5),  # not started at zero otherwise there would be no data and might produce an error when plotting
                            p("Adjusting this slider will randomly thin a percentage of the simulated data to aid comparison between observed and simulated values, and to make the plots refresh faster. This will not affect the calculations and 
                              is purely for visual purposes.")
           ),
           conditionalPanel(condition = "input.tabsetPanelID=='3 Build model' && output.fileUploaded && (output.fileTypeCSV || output.savedModelUploaded)",
                              h3("User defined labels"),
                            p("This section allows you to adjust the titles for the graphs."),
                            p("You can format your user defined labels for the bottom plot using html tags. 
                              For example, you can write", HTML(paste0("m", tags$sup("2"))), 
                              "as \"m<sup>2</sup>\",", HTML(paste0("m", tags$sup("3"), "/s")), 
                              "as \"m<sup>3</sup>/s\" or", HTML(paste0("x", tags$sub("2"))), 
                              "as \"x<sub>2</sub>\". For a new line, use the <br> tag, for 
                              example \"A really <br> long label\"."),
                            textInput("userDefinedBuildModelTopGraphTitle", label = "Upper plot title", value = ""),
                            textInput("userDefinedBuildModelBottomGraphTitle", label = "Lower plot title", value = ""),
                              ),
           
           ### Set up conditional panel for if the 'Marginal analysis' tab is selected and simulation outputs exist
           conditionalPanel(condition = "input.tabsetPanelID=='4 Marginal analysis' && (output.HTsimDone || output.HTsimDoneTwo)",
                            # Set up an output for the user to select which variable to plot
                            h3("Plot controls"),
                            uiOutput("marginalVariableControl"),
                            # Set up an output for a checkbox group which allows the user to choose which data to plot (auto and/or user marginals)
                            uiOutput("marginalOptionsControl"),
                            h3("User defined labels"),
                            
                            p("This section allows you to adjust the label and title for the graph."),
                            p("You can format your user defined labels using html tags. 
                              For example, you can write", HTML(paste0("m", tags$sup("2"))), 
                              "as \"m<sup>2</sup>\",", HTML(paste0("m", tags$sup("3"), "/s")), 
                              "as \"m<sup>3</sup>/s\" or", HTML(paste0("x", tags$sub("2"))), 
                              "as \"x<sub>2</sub>\". For a new line, use the <br> tag, for 
                              example \"A really <br> long label\"."),
                            textInput("userDefinedMarginalAnalysisGraphTitle", label = "Marginal analysis graph title", value = "Marginal analysis for"),
                            textInput("userDefinedValueLabel", label = "Value label", value = "Value")
                         
                            
           ),
           
           ### Set up conditional panel for if the 'Joint probability analysis' tab is selected and simulation outputs exist
           conditionalPanel(condition = "input.tabsetPanelID=='5 Joint probability analysis' && (output.HTsimDone || output.HTsimDoneTwo)",
                            # Provide option for user to change the scale for viewing the data
                            h3("Plot controls"),
                            selectInput("ScalePostSim_step3", label = h4("Select plotting scale"), choices = list("Original scale" = "Orig", "Standardised scale" = "Std"), selected = 1),
                            # Set up a checkbox group which allows the user to choose which data to plot (observed or simulated)
                            checkboxGroupInput("origSimData_step3", 
                                          label = h4("Display options"), 
                                          choices = list("Observed data" = "origData", 
                                                         "Simulated data" = "simData"), selected = c("origData", "simData")),
                            # Set up an option to disable automatic plot updating
                            checkboxInput("pausePlots", label = "Disable automatic plot updating", value=FALSE),
                            p("Disabling the automatic plot updating when viewing in original scale will enable the table on the right to
                              be edited more quickly."),
                            br(),
                            h4("Inspect variables"),
                            # Split the row into two columns for the pair controls which are used for selecting which variables to plot on the interactive plot  
                            fluidRow(
                              column(6,
                                     uiOutput("pairControls1_postSim_step3")),
                              column(6,
                                     uiOutput("pairControls2_postSim_step3"))
                            ),
                            # Add a slider input for the user to select the percentage of the data to plot
                            sliderInput("pcThinned_step3", label = h4("Data thinning"), min = 5, max = 100, value = 10, step = 5),
                            p("Adjusting this slider will randomly thin a percentage of the simulated data to aid comparison between observed and simulated values, and to make the plots refresh faster. This will not affect the calculations and 
                            is purely for visual purposes."),
                            
                            h3("User defined labels"),
                            p("This section allows you to adjust the titles for the graphs."),
                            p("You can format your user defined labels for the bottom plot using html tags. 
                              For example, you can write", HTML(paste0("m", tags$sup("2"))), 
                              "as \"m<sup>2</sup>\",", HTML(paste0("m", tags$sup("3"), "/s")), 
                              "as \"m<sup>3</sup>/s\" or", HTML(paste0("x", tags$sub("2"))), 
                              "as \"x<sub>2</sub>\". For a new line, use the <br> tag, for 
                              example \"A really <br> long label\"."),
                            textInput("userDefinedJointProbabilityTopGraphTitle", label = "Upper plot title", value = ""),
                            textInput("userDefinedJointProbabilityBottomGraphTitle", label = "Lower plot title", value = "")
                            
           )

    ),  # end of column
    
    ### Set the tabs
    column(9, tabset.Panel, type = "tabs")
  )  # end of fluid row

))  # end of fluid row and shinyUI 