###############################################################################
#
# Process and setup the environment.
#
###############################################################################

#Load in the GIS Shapefile for dynamic models
countyData<-readShapeSpatial("countyshape.shp")

#Load the RDS Values for the shapefile and the larger set for models
mergedData <- readRDS("./data/mergedData.rds")
agIntenSlope<-readRDS("./data/agIntenSlope.rds")

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

# Get the location and name of the image for the About tab.
image1997 <- paste0("agIntenInt.png")
rateOfChange <- paste0("agIntenSlope.png")
###############################################################################
#
# Create the Testing and Training Dataset
#
###############################################################################


index <- createDataPartition(mergedData$PerCroplandGain,
                                  p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train.tree <- mergedData[ index,]
test.tree  <- mergedData[-index,]

train.tree$PerCroplandLoss<-NULL
test.tree$PerCroplandLoss<-NULL

train.tree<-train.tree[5:47]
test.tree<-test.tree[5:47]
#remove all non important data for tree and forests models
trimmedData<-mergedData[5:47]
trimmedData$PerCroplandLoss<-NULL
trimmedData[is.na(trimmedData)]<-0



#Create a subset of training for the regression models
train.reg <- train.tree[3:42]
test.reg <- test.tree[3:42]

train.reg$PerCroplandLoss<-NULL
test.reg$PerCroplandLoss<-NULL


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
               detail = "Pick a lower CV for faster results...")
  
  
  # Grab the predictor variables to be used in the model from the user input
  vars <- unlist(input$regTreeVars)
  
  # Fit a Classification Tree Model using cross validation
  train.control <- trainControl(method = "cv", number = input$numFolds)
  tree.Fit <- train(PerCroplandGain ~ .,
                    data = train.tree[,c(c("PerCroplandGain"), vars)],
                    method = 'rpart',
                    trControl = train.control,
                    na.action = na.exclude)
                    

  
  # Save the fitted model in a folder.
  saveRDS(tree.Fit, "./Models/reg-tree-model.rds")
  
  # Output a plot of the Regression Tree
  tree.Summary <- "rattle::fancyRpartPlot(tree.Fit$finalModel)" 
                  
  
  tree.yhat <- predict(tree.Fit, newdata = test.tree)
  tree.Fit.Stats <- mean((tree.yhat-test.tree$PerCroplandGain)^2)
  
  # Return all objects as a list
  list(summary = tree.Summary, fitStats = tree.Fit.Stats)
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
  mtry=as.numeric(input$mtry)
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

  trainStepwiseModel <- eventReactive(input$stepRun, {
    
    # Create a Progress object
    progress <- Progress$new()
    # Ensure the Progress object closes upon exiting this reactive, even if
    # there is an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Calculation in progress",
                 detail = "This should be a short one...")
    
    # Grab the predictor variables to be used in the model from the user input
    vars <- unlist(input$regVars)
    
    # Fit a stepwise Regression Model

    step.model <- lm(PerCroplandGain~. , 
                        data = train.reg[,c(c("PerCroplandGain"), vars)],
                        ) 
    
    saveRDS(step.model, "./Models/step-model.rds")
    
    regSummary <- summary(step.model)
    
    reg.yhat <- predict(step.model, newdata = test.reg)
    reg.Fit.Stats <- mean((reg.yhat-test.reg$PerCroplandGain)^2)
    
    list(summary = regSummary, fitStats = reg.Fit.Stats)
  })
  
  output$stepTitle <- renderUI({
    trainStepwiseModel()
    h5(strong("Model training is complete."))
  })
  
  output$summaryStep <- renderPrint({
    trainStepwiseModel()$summary
  })
  
  output$stepFitStats <- renderPrint({
    trainStepwiseModel()$fitStats
  })
  
  
  

###############################################################################
#
#  Active Prediction Variable
#
###############################################################################
} #Close the output server model