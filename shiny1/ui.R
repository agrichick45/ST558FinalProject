###############################################################################
#
# This is the UI R script mapping agricultural intensification in the Midwest
#
# Author:  Mandy Liesch
# Email:  amliesch@ncsu.edu
#
###############################################################################

library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(mapdeck)
library(markdown)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(rgeos)
library(maptools)
library(leaflet)
library(htmltools)
library(shiny)
library(plotly)
library(shiny)
library(tidyverse)
library(caret)
library(rsample)
library(rpart)	
library(rpart.plot)




###############################################################################
#
# Start of Shiny UI
#
###############################################################################

shinyUI(navbarPage(

  
  titlePanel("Agricultural Intensification Rates"),
  #Set up the interactive sidebar panel
  # Create tabs for the different sections.
  tabsetPanel(
    
###############################################################################
#
# About Tab
#
###############################################################################
    # Create a tab for the About section.
    tabPanel(
      # Add a title.
      title="About",
      # Create a main panel for the About tab.
      mainPanel("This is the About Tab",
      ),
    ),
###############################################################################
#
# Dynamic Mapping Tab
#
###############################################################################

    tabPanel(
      # Add a title.
      title="Dynamic Maps",
      mainPanel(leafletOutput("mymap"),
      )),
###############################################################################
#
# Interactive Heatmap
#
###############################################################################
    tabPanel(
      # Add a title.
      title="Interactive Heatmap",
      # Create a side panel
      sidebarLayout(
    #add the background data, and the web link
      sidebarPanel(
        "This will be for value interaction and a scatter plot"

      ),
      mainPanel(
      plotlyOutput("heatmap", width = "100%", height="600px"),
      
      # Use the top portion for visualization.
      h3("Visualization"),
      # Use user input to determine which plot to show.
      conditionalPanel(
        condition = "input.plotType == 'histogram'",
        plotlyOutput("histogram")
      ),                     
      conditionalPanel(
        condition = "input.plotType == 'scatterPlot'",
        plotlyOutput("scatter")
      ),
    ))
    ),

###############################################################################
#
# Modeling Tab with Dropdown Tabs
#
###############################################################################
    # Create the Modeling tab with 3 sub-tabs.
    navbarMenu(
      
      # Add a title.
      title="Modeling",
###############################################################################
#
# Modeling Info Section
#
###############################################################################
      tabPanel(
        # Give it a title,
        title = "Modeling Info",
        mainPanel(fluidPage(
          # Give an overview of the modeling excercise.
          br(),
          h4("Goals of the Modeling Secion"),
        ))),

###############################################################################
#
# Model Exploration
#
###############################################################################

        tabPanel(
      # Add a title.
        title="Models",
      # Create a side panel
      sidebarPanel(
        h3("Choose a Model"
        ),
        radioButtons("modelType", 
                     label = "Choose a Model:", 
                     choices = c("Single Regression Tree",
                                 "Random Forest",
                                 "Stepwise Selection Regression"
                                 ), 
                     selected = character(0)
        ),
###############################################################################
#
# Regression Tree Conditional Inputs
#
###############################################################################
        
            conditionalPanel(condition = "input.modelType == 'Single Regression Tree'",
              h3("Select Parameters for Fit"
                        ),
              checkboxGroupInput("regTreeVars", 
                        label = "Check at Least 5 Parameters for Entry in Regression Model",
                        choices = names(mergedData)[-c(1:3)]
                        ),
              h3("Select Cross Validation Fit Options"),
              "A Stratified Training Sample Set of 80% was prepared:",
                                      
              sliderInput("numFolds", 
                  label = "Minimum of 2 Folds, Maximum of 10:",
                  min = 2, max = 10, value = 5
                                      ),
              actionButton("regTreeVars", label = "Generate Model"),
            ),       
  
###############################################################################
#
# Random Forests Modelling
#
###############################################################################
            conditionalPanel(condition = "input.modelType == 'Random Forest'",
                 h3("Select Parameters for Fit"
                 ),
                 checkboxGroupInput("forestVars", 
                                    label = "Select at Least 5 Variables for Random Forests",
                                    choices = names(mergedData)[-c(1:3)]
                 ),
                 h3("Select Tree Fit Options"
                 ),
                 sliderInput("mtry", 
                             label = "Choose the default number of trees:",
                             min = 2, max = 15, value = 5
                 ),
                 sliderInput("numFolds", 
                             label = "Minimum of 2 Folds, Maximum of 10:",
                             min = 2, max = 10, value = 5
                 ),
                 actionButton("forestVars", label = "Generate Model")
                ),
  

###############################################################################
#
# Stepwise Backwards Regression Model
#
###############################################################################
            conditionalPanel(condition = "input.modelType == 'Stepwise Selection Regression'",
                 h3("Select Parameters for Fit",
                 ),
                 selectInput("stepChoice", 
                                    label = "Select at Least 5 Variables for Random Forests",
                                    choices = c("forward", "backward", "both")
                 ),
                 actionButton("stepChoice", label = "Generate Model")
                
                ),
                ),

###############################################################################
#
# Main Panel Conditions
#
###############################################################################

            mainPanel(fluidPage(
                conditionalPanel(condition = "input.modelType == 'Single Regression Tree'",
                   htmlOutput("regressionTitle"),
                   h4("Regression Tree Model Fit Summary:"),
                   verbatimTextOutput("summaryMulti"),
                   br(),
                   h4("Test Model Fit Statistics:"),
                   verbatimTextOutput("regFitStats")
                    ),
                conditionalPanel(condition = "input.modelType == 'Random Forest'",
                   htmlOutput("randomForestTitle"),
                   h4("Random Forest Model Fit Summary:"),
                   plotOutput("summaryRF"),
                   br(),
                   h4("Test Model Fit Statistics:"),
                   verbatimTextOutput("RFFitStats")
                    ),
                conditionalPanel(condition = "input.modelType == 'Stepwise Selection Regression'",
                   htmlOutput("stepTitle"),
                   h4("Stepwise Model Fit Summary:"),
                   plotOutput("summaryStep"),
                   br(),
                   h4("Test Model Fit Statistics:"),
                   verbatimTextOutput("stepFitStats")
                    )
                  ))
        ),

###############################################################################
#
# Model Fitting Section
#
###############################################################################

        tabPanel(
        # Add a title for the sub tab.
        title = "Model Fitting",
        mainPanel("This is the model Fitting panel")
        # Allow the user to set a random seed between -1000 and 1000.
        ),

    ),
###############################################################################
#
# Data Tab
#
###############################################################################
    tabPanel(
      # Add a title.
      title="Data",
      # Create a main panel for the About tab.
      mainPanel("This is the Data Subset and Download Tab")
     ) 
  ))
)



