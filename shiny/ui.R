shinyUI(navbarPage(
  
  # Add a title.
  title = "Agricultural Intensification in the Mississippi and Ohio River Areas",
  # Add a theme.
  theme = shinytheme("flatly"),
  
  # Create tabs for the different sections.
  tabsetPanel(
    
    # Create a tab for the About section.
    tabPanel(
      # Add a title.
      title="About",
      # Create a main panel for the About tab.
      mainPanel(" This is the main panel with the about tab"),
        h3("The Purpose of this App"),
        h3("The Data"),
        h3("The Tabs"),
    ),
    
    # Create the Data page.
    tabPanel(
      # Add a title.
      title="Data",
      # Create a side panel.
      sidebarPanel("This is where we add the sidebar panel options"
      ),
      
      mainPanel(
        "This is the main panel to data"
      )
    ),
    
    # Create a tab for the data exploration.
    tabPanel(
      # Add a title.
      title = "Data Exploration",
      # Create a sidbar for user inputs.
      sidebarPanel("This is the Data Exploration Panel"
      ),
      mainPanel(
        "This is the main panel to data exploration"
      )
    ),
    
    # Create the Modeling tab with 3 sub-tabs.
    navbarMenu(
      
      # Add a title.
      title="Modeling",
      
      # Add the Modeling Info tab.
      tabPanel(
        # Give it a title,
        title = "Modeling Info",
        mainPanel(fluidPage("This is where the modeling info goes"
        ))
      ),
      
      # Add a tab for fitting the models.
      tabPanel(
        # Add a title for the sub tab.
        title = "Model Fitting",
        #Set up the Sidebar Panel
        sidebarPanel("This is the sidebar panel"),
        # Create the main panel to hold model performances and 
        # summaries.
        mainPanel(
          # Show the test-set accuracies.
        )
      ),
      # Create the prediction tab.
      tabPanel(
        # Add a title.
        title = "Prediction",
        # Create a sidebar for the user to play with inputs.
        sidebarPanel("This is a sidebar panel"
        ),
        mainPanel(
          # This is my main panel.
        )
      )
    )
  )
))	