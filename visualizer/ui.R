# Load libraries ---------------------------------------------------------------
library(shiny)
library(shinythemes)

# Plot libraries for outputs
require(rCharts)
require(leaflet)
require(DT)
require(plotly)

# Adjustments
h3.align <- 'center'

# Shiny UI ---------------------------------------------------------------------
shinyUI(navbarPage(
  title = "Fisheries Dashboard",
  
  # Pick a bootstrap theme from https://rstudio.github.io/shinythemes/
  theme = shinytheme("flatly"),
  
  # Analytics tab panel --------------------------------------------------        
  tabPanel(
    title = "Analytics",
    
    # 
    sidebarPanel(width=3,
                 h3("Customize Variables", align = h3.align),
                 h6("Make selections and watch the graphs and data update", align="top"),
                 uiOutput("yearSlider"),
                 uiOutput("unitPriceSlider"),
                 uiOutput("cat1Controls"),
                 uiOutput("cat2Controls"),
                 p(actionButton(inputId = "reset", 
                                label = "Reset Fields", 
                                icon = icon("refresh")
                 ), align = "center")
    ),
    
    mainPanel(
      tabsetPanel(
        
        # Analysis Tab -----------------------------------------
        tabPanel(
          p(icon("area-chart"), "Analysis"),
          
          # Fatalities Over Time By Category
         
          fluidRow(
            column(6, h4("View Incidents by Year"), 
                   p(showOutput("primaryBikeCatOut", "nvd3"))),
          
            column(6,h4("View Incidents by Year and Type"),
                   p(showOutput("secondaryBikeCatOut", "nvd3")))
          ),
          
          # Detailed Information
          fluidRow(h3("Aggregated Information")),
          fluidRow(
            column(6,
                   
                   
                   showOutput("topNout", "nvd3"))
          )),  # End Analysis Tab
        
        # Data Tab ---------------------------------------------
        tabPanel(
          p(icon("table"), "Data"),
          
          fluidRow(
            column(6, h3("Search, Filter & Download Data", align='left')),
            column(6, downloadButton('downloadData', 'Download', class="pull-right"))
            
          ),
          hr(),
          fluidRow(
            dataTableOutput(outputId="table")
            
          )) # End Data Tab
      )
    )
  ), # End Analytics Tab Panel
  
  # About Tab Panel ------------------------------------------------------           
  tabPanel("Supporting Docs",
           mainPanel(column(8, offset = 2, includeMarkdown("about.md"))
           )
  ) # End About Tab Panel
  
) # End navbarPage
) # End shinyUI