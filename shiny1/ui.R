
shinyUI(navbarPage(

  titlePanel("Agricultural Intensification Rates"),
  #Set up the interactive sidebar panel
  # Create tabs for the different sections.
  tabsetPanel(
    # Create a tab for the About section.
    tabPanel(
      # Add a title.
      title="About",
      # Create a main panel for the About tab.
      mainPanel("This is the About Tab",
      ),
    ),
    tabPanel(
      # Add a title.
      title="Dynamic Maps",
      mainPanel(leafletOutput("mymap"),
      )),
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
    # Create the Modeling tab with 3 sub-tabs.
    navbarMenu(
      
      # Add a title.
      title="Modeling",
      
      # Add the Modeling Info tab.
      tabPanel(
        # Give it a title,
        title = "Modeling Info",
        mainPanel(fluidPage(
          # Give an overview of the modeling excercise.
          br(),
          h4("Goals of the Modeling Secion"),
        ))),
        tabPanel(
      # Add a title.
        title="Models",
      # Create a side panel
        mainPanel("This the modeling panel"),
        ),
        tabPanel(
        # Add a title for the sub tab.
        title = "Model Fitting",
        mainPanel("This is the model Fitting panel")),
        # Allow the user to set a random seed between -1000 and 1000.
    ),
    tabPanel(
      # Add a title.
      title="Data",
      # Create a main panel for the About tab.
      mainPanel("This is the Data Subset and Download Tab",
      )),
  ))
)



