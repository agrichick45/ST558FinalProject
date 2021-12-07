###############################################################################
#
# Process and setup the environment.
#
###############################################################################


countyData<-readShapeSpatial("countyshape.shp")
agIntenSlope<-read.csv("agIntenSlope.csv")
agIntenSlope$GEOID<-agIntenSlope$ï..GEOID
agIntenSlope$ï..GEOID<-NULL
agIntenSlope[is.na(agIntenSlope)]<-0

mergedData<-read_csv("mergedData.csv")
mergedData[is.na(mergedData)]<-0
#Convert to Factors
mergedData$NCHS_URCS_2013<-as.factor(mergedData$NCHS_URCS_2013)
mergedData$InitialCluster<-as.factor(mergedData$InitialCluster)
mergedData$Megacluster<-as.factor(mergedData$Megacluster)

MapInfo <- tags$p(tags$style("<p {font-size:12px} />"),
                  tags$b("Agricultural Intensification: Rate of Change (Slope)"))

set.seed(10)
trimData<-mergedData[5:47]
trimData[is.na(trimData)]<-0
index <- initial_split(trimData,
                       prop = 0.8)
train <- training(index)
test <- testing(index)

trimData<-mergedData[7:48]
cmat<-cor(trimData)

fig <- plot_ly(
  x = c(colnames(cmat)), y = c(row.names(cmat)),
  z = cmat, type = "heatmap"
)

# Get the location and name of the image for the About tab.
image1997 <- paste0("agIntenInt.png")
rateOfChange <- paste0("agIntenSlope.png")

dataMap<-merge(countyData, agIntenSlope, by.x="GEOID_1", by.y="GEOID")

popup <- paste0("<strong>", dataMap$NAME, "</strong><br /> 
            Total Market Value: $", dataMap$totMarkSlope, "<br />
            Operations with Income > $500,000: ", dataMap$opgr500kSlope,  "<br /> Farms > 2000 Acres: ",
                dataMap$slopelargeCropOp, "<br /> Animal Density: ", dataMap$Adens_Slope, "<br />
            Percent of Sales > $500,000: ",
                dataMap$perMarkSlope)



server <- function(input,output, session){
  
  dataSet <- reactive({
    data=dataMap
    selectClass<-input$SB
  })  
  output$mymap <- renderLeaflet({
      pal <- colorNumeric(palette = "Reds", domain = dataMap$totMarkSlope, na.color = NA)
      dataMap %>%
      leaflet(width = "100%") %>% # Interactive mapping package
      addProviderTiles(provider = "CartoDB.Positron") %>% # Changes base map
      setView(-81.110757, 38.712046, zoom = 4) %>%
      addPolygons(  
                  popup = ~ popup,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal(totMarkSlope)) %>%
      addLegend("bottomright", 
                  pal = pal, 
                  values = ~ totMarkSlope,
                  title = "Total Market Rate of Change",
                            opacity = 1) %>% # Creates legend
      addControl(MapInfo, position = "topright")

      })
    
    
    ## server.R
    output$heatmap <- renderPlotly({plot_ly(x = c(colnames(cmat)), y = c(row.names(cmat)), z = cmat, colors = "Reds", type = "heatmap")})
    }



