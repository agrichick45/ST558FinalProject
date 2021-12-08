###############################################################################
#
# This is the UI R script mapping agricultural intensification in the Midwest
#
# Author:  Mandy Liesch
# Email:  amliesch@ncsu.edu
#
###############################################################################
library(leaps)
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
library(png)
library(summarytools)
library(rattle)
library(gbm)

###############################################################################
#
# Start of Shiny UI
#
###############################################################################
mergedData <- readRDS("./data/mergedData.rds")
agIntenSlope<-readRDS("./data/agIntenSlope.rds")


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
      mainPanel( 
        includeMarkdown("about.md"),
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
                                 "Boosting Regression"
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
                                    choices = names(train.tree)[-c(1:3)]
                 ),
                 h3("Select Cross Validation Fit Options"),
                 "A Stratified Training Sample Set of 80% was prepared:",
                 
                 sliderInput("numFolds", 
                             label = "Minimum of 2 Folds, Maximum of 10:",
                             min = 2, max = 10, value = 5
                 ),
                 actionButton("runRegTree", label = "Generate Model"),
),       
###############################################################################
#
# Random Forests Modelling
#
###############################################################################
conditionalPanel(condition = "input.modelType == 'Random Forest'",
                 h3("Select Parameters for Fit"
                 ),
                 checkboxGroupInput("rfVars", 
                                    label = "Select at Least 5 Variables for Random Forests",
                                    choices = names(train.tree)[-c(1:3)]
                 ),
                 h3("Select Tree Fit Options"
                 ),

                 sliderInput("numFolds", 
                             label = "Minimum of 2 Folds, Maximum of 10:",
                             min = 2, max = 10, value = 5
                 ),
                 actionButton("runForest", label = "Generate Model")
),
  

###############################################################################
#
# Boosting Model
#
###############################################################################
          conditionalPanel(condition = "input.modelType == 'Boosting Regression'",
                 h3("Select Parameters for Fit",
                 ),
                 checkboxGroupInput("bagVars", 
                                    label = "Select at Least 5 Variables for Boosting",
                                    choices = names(train.tree)[-c(1:3)]),
  
                 sliderInput("numFolds", 
                             label = "Minimum of 2 Folds, Maximum of 10:",
                             min = 2, max = 10, value = 5
                 ),
                 
                 actionButton("boostRun", label = "Generate Model")
                 
),
      ),

###############################################################################
#
# Main Panel Conditions
#
###############################################################################

            mainPanel(fluidPage(
                conditionalPanel(condition = "input.modelType == 'Single Regression Tree'",
                  htmlOutput("regressTreeTitle"),
                  h4("Regression Tree Model Fit Summary:"),
                  verbatimTextOutput("summary.Tree"),
                  br(),
                  h4("Test Model Fit Statistics:"),
                  verbatimTextOutput("tree.Fit.Stats")
                ),
                conditionalPanel(condition = "input.modelType == 'Random Forest'",
                                 htmlOutput("forestTitle"),
                                 h4("Random Forest Model Fit Summary:"),
                                 plotOutput("summary.RF"),
                                 br(),
                                 h4("Test Model Fit Statistics:"),
                                 verbatimTextOutput("RFFitStats")
                ),
                conditionalPanel(condition = "input.modelType == 'Boosting Regression'",
                                 htmlOutput("BoostTitle"),
                                 h4("Boosting Regression:"),
                                 plotOutput("summaryBoost"),
                                 br(),
                                 h4("Test Model Fit Statistics:"),
                                 verbatimTextOutput("boostFitStats")
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
  sidebarPanel(
    radioButtons("predictionModel", 
                 label = "Choose Model:", 
                 choices = c("Single Regression Tree",
                             "Random Forest",
                             "Boosted Regression Tree"), 
                 selected = character(0)
    ),
  ),
  mainPanel(fluidPage(
    
    
    
  ),
  ),
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
      # Create a side panel.
      sidebarPanel(
        # Create a filter for the states of interest.
        selectInput(
          inputId = "selectedStates",
          label = "Filter by State(s)",
          choices = unique(mergedData$State),
          selected = unique(mergedData$State),
          multiple = TRUE,
          selectize = TRUE
        ),
        # Create a filter for the counties by initial intensity.
        selectInput(
          inputId = "initialIntense",
          label = "Filter by 1997 Agricultural Intensity",
          choices = c("Low", "Medium", "High"),
          selected = c("Low", "Medium", "High"),
          multiple = TRUE,
          selectize = TRUE
        ),
        # Create a filter for the counties to display by winner.
        selectInput(
          inputId = "selectedIntense",
          label = "Filter by Agricultural Intensification",
          choices = c("Low", "Medium", "High"),
          selected = c("Low", "Medium", "High"),
          multiple = TRUE,
          selectize = TRUE
        ),
        # Create a download button to download the data set.
        sidebarPanel(downloadButton("downloadData", "Download")
                     )
      ),
      # Create a main panel for the About tab.
      dataTableOutput(outputId = "tab")
     ) 
  )
)
)


