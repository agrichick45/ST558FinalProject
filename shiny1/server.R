###############################################################################
#
# This is the Server R script mapping agricultural intensification in the Midwest
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
library(stringr)
library(ggridges)


###############################################################################
#
# Process and setup the environment.
#
###############################################################################

#Load in the GIS Shapefile for dynamic models
countyData<-readShapeSpatial("countyshape.shp")

#Load the RDS Values for the shapefile and the larger set for models
mergedData<-readRDS("./data/mergedData.rds")
agIntenSlope<-readRDS("./data/agIntenSlope.rds")

plotDataset<-mergedData
plotDataset$NCHS_URCS_2013<-as.character(mergedData$NCHS_URCS_2013)
#train.boost<- train.tree[3:43]
#test.boost<- test.tree[3:43]
###############################################################################
#
# Map and Leaflet Setup
#
###############################################################################

#Create the tag for the Title
MapInfo <- tags$p(tags$style("<p {font-size:12px} />"),
                  tags$b("Agricultural Intensification: Rate of Change (Slope)"))

#Merge the Intensification Shapefile with the Geospatial Data Frame
dataMap<-merge(countyData, agIntenSlope, by.x="GEOID_1", by.y="GEOID")

#Create the interactive popup.
popup <- paste0("<strong>", dataMap$NAME, "</strong><br /> 
            Total Market Value: $", dataMap$totMarkSlope, "<br />
            Operations with Income > $500,000: ", dataMap$opgr500kSlope,  "<br /> Farms > 2000 Acres: ",
                dataMap$slopelargeCropOp, "<br /> Animal Density: ", dataMap$Adens_Slope, "<br />
            Percent of Sales > $500,000: ",
                dataMap$perMarkSlope)

#Manipulate data for dynamic correlation model
trimData<-mergedData[7:48]
cmat<-cor(trimData)

#Set up the interactive heatmap figure
fig <- plot_ly(
  x = c(colnames(cmat)), y = c(row.names(cmat)),
  z = cmat, type = "heatmap"
)
###############################################################################
#
#  Training and Testing Dataset
#
###############################################################################


# Get the location and name of the image for the About tab.
image1997 <- paste0("agIntenInt.png")
rateOfChange <- paste0("agIntenSlope.png")


mergedData$PerCroplandLoss<-NULL

mergedData[is.null(mergedData)]<-0
index <- createDataPartition(mergedData$PerCroplandGain,
                             p = 0.8, 
                             list = FALSE, 
                             times = 1)
train.tree <- mergedData[ index,]
test.tree  <- mergedData[-index,]





###############################################################################
#
# Server Shiny Backend Function
#
###############################################################################


server <- function(input,output, session){
  
  dataSet <- reactive({
    data=dataMap
    selectClass<-input$SB
  })  
  output$mymap <- renderLeaflet({
      pal <- colorNumeric(palette = "Reds", domain = dataMap$Adens_Slope, na.color = NA)
      dataMap %>%
      leaflet(width = "100%") %>% # Interactive mapping package
      addProviderTiles(provider = "CartoDB.Positron") %>% # Changes base map
      setView(-81.110757, 38.712046, zoom = 4) %>%
      addPolygons(  
                  popup = ~ popup,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal(Adens_Slope)) %>%
      addLegend("bottomright", 
                  pal = pal, 
                  values = ~ totMarkSlope,
                  title = "Animal Stocking Density:",
                  "Rate of Change",
                            opacity = 1) %>% # Creates legend
      addControl(MapInfo, position = "topright")

      })
    
    
    ## server.R
    output$heatmap <- renderPlotly({plot_ly(x = c(colnames(cmat)), y = c(row.names(cmat)), z = cmat, colors = "Reds", type = "heatmap")})

###############################################################################
#
#  Interactive ggPlot functions 
#
###############################################################################
    data(plotDataset)
    
    varsX<-names(plotDataset[7:48])
    varsY<-names(plotDataset[4:6])
    varsZ<-c(names(plotDataset[5:6]), "NULL")

    output$outplot <- renderPlot({
      ggplot(plotDataset, aes_string(x = input$x, y= input$y, fill=input$z)) +
        geom_density_ridges(alpha = 3/10) +
        theme_ridges() + 
        theme(legend.position = "right")
    })
    
###############################################################################
#
#  Regression Tree 
#
###############################################################################
 


trainRegTreeModel <- eventReactive(input$runRegTree, {
  # Create a Progress object
  progress <- Progress$new()
  # Ensure the Progress object closes upon exiting this reactive, even if
  # there is an error.
  on.exit(progress$close())
  # Set the message to the user while cross-validation is running.
  progress$set(message = "Waiting for Results",
               detail = "Don't Sleep...")
  
  
  # Grab the predictor variables to be used in the model from the user input
  vars <- unlist(input$regTreeVars)
  
  
  # Fit a Tree Model using cross validation
  train.control <- trainControl(method = "cv", number = input$numFolds)
  tree.Fit <- train(PerCroplandGain ~ .,
                    data = train.tree[,c(c("PerCroplandGain"), vars)],
                    method = 'rpart',
                    trControl = train.control,
                    na.action = na.exclude)
  
  # Save the fitted model in a folder.
  saveRDS(tree.Fit, "./Models/reg-tree-model.rds")
                  
  tree.Plot<- "tree.Fit <- readRDS('./Models/reg-tree-model.rds'); 
                    rattle::fancyRpartPlot(tree.Fit$finalModel)"
  
  
  
  # Output a plot of the Regression Tree
  
  
  tree.yhat <- predict(tree.Fit, newdata = test.tree)
  tree.Fit.Stats <- mean((tree.yhat-test.tree$PerCroplandGain)^2)
  
  # Return all objects as a list
  list(summary = tree.Plot, fitStats = tree.Fit.Stats)
})

output$treeTitle <- renderUI({
  trainRegTreeModel()
  h5(strong("Regression Tree Model Complete."))
})

output$summary.Tree <- renderPlot({
  eval(parse(text=trainRegTreeModel()$summary))
})

output$tree.Fit.Stats <- renderPrint({
  trainRegTreeModel()$fitStats
})

trainRFModel <- eventReactive(input$runForest, {
  
  # Create a Progress object
  progress <- Progress$new()
  # Ensure the Progress object closes upon exiting this reactive, even if
  # there is an error.
  on.exit(progress$close())
  # Set the message to the user while cross-validation is running.
  progress$set(message = "Random Forests Take Forever",
               detail = "I recommend a beer...")
  
  # Grab the predictor variables to be used in the model from the user input
  vars <- unlist(input$rfVars)
  
  # Fit a Random Forest Model using cross validation
  train.control <- trainControl(method = "cv", number = input$numFolds)
  rfFit <- train(PerCroplandGain ~ ., 
                 data = train.tree[,c(c("PerCroplandGain"), vars)],
                 method = 'rf',
                 # Needed to retrieve variable importance 
                 importance = TRUE,
                 trControl = train.control, 
                 na.action = na.exclude)
                 
                 
                 
                 # Save the fitted model in a folder.
                 saveRDS(rfFit, "./Models/rf-model.rds")
                 
                 import <- varImp(rfFit)
                 importPlot <- as_tibble(import$importance, rownames = "var")
                 importance <- importPlot %>% arrange(desc(Overall))
                 
                 # Output a plot of the variable importance from the Random Forest Model
                 # (Top 20 predictor variables only)
                 rfImpPlot <- ggplot(importance[1:15,],
                                     aes(x = reorder(var, Overall), 
                                         y = Overall, fill = Overall)) +
                   geom_col() +
                   coord_flip() +
                   theme(legend.position = "none") +
                   labs(x = "Variables",  
                        y = "RF Importance", 
                        title ="Importance of Top 15 Variables")
                 
                 rf.yHat <- predict(rfFit, newdata = test.tree)
                 rf.Stats <- mean((rf.yHat-test.tree$PerCroplandGain)^2)
                 
                 # Return all objects as a list
                 list(summary = rfImpPlot, fitStats = rf.Stats)
})
  
  output$forestTitle <- renderUI({
    trainRFModel()
    h5(strong("Model training is complete."))
  })
  
  output$summary.RF <- renderPlot({
    trainRFModel()$summary
  })
  
  output$RFFitStats <- renderPrint({
    trainRFModel()$fitStats
  })

  trainBoostedModel <- eventReactive(input$boostRun, {
    # Create a Progress object
    progress <- Progress$new()
    # Ensure the Progress object closes upon exiting this reactive, even if
    # there is an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Also takes time",
                 detail = "Find Chocolate?")
    # Grab the predictor variables to be used in the model from the user input
    vars <- unlist(input$bagVars)
    

    # Fit a stepwise Regression Model
    Boost.model <- gbm(PerCroplandGain~. , 
                        data = train.tree[,c(c("PerCroplandGain"), vars)],
                        ) 
    
    saveRDS(Boost.model, "./Models/boosted-tree.rds")
  
    
    summBoost<- summary(Boost.model)
    importPlot <- as_tibble(summBoost)
  
    boost.yhat <- predict(Boost.model, newdata = test.tree)
    boost.Fit.Stats <- mean((boost.yhat-test.tree$PerCroplandGain)^2)
    
    list(summary = importPlot, fitStats = boost.Fit.Stats)
    
  })
  
  output$BoostTitle <- renderUI({
    trainBoostedModel()
    h5(strong("Model training is complete."))
  })
  
  output$summaryBoost <- renderTable({
    eval(parse(text=trainBoostedModel()$summary))
  })
  
  output$boostFitStats <- renderPrint({
    trainBoostedModel()$fitStats
  })
  
  
  

###############################################################################
#
#  Active Prediction Variable
#
###############################################################################
} #Close the output server model