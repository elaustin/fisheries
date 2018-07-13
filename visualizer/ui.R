

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
                 h6("Select variables and watch the graphs and data update", align="top"),
                 uiOutput("yearSlider"),
                 uiOutput("casesSlider"),
                 uiOutput("cat1Controls"),
                 radioButtons(
                   inputId="cat2radio",
                   label="Injury Type Selection:",
                   choices=list(
                     "All",
                     "Select"
                   ), inline=T, selected="All"),
                   conditionalPanel(
                     condition = "input.cat2radio != 'All'",
                     uiOutput("cat2Controls")),
                 radioButtons(
                   inputId="radio",
                   label="State Selection:",
                   choices=list(
                     "All",
                     "Select"
                   ),
                   selected="All", inline = T),
                 conditionalPanel(
                   condition = "input.radio != 'All'",
                   uiOutput("stateControls")),
                 
                 p(actionButton(inputId = "reset", 
                                label = "Reset Fields", 
                                icon = icon("refresh")
                 ), align = "center")
    ),
    
    mainPanel(
     
      tabsetPanel(
        
        
        # Fatalities Tab -----------------------------------------
        tabPanel(
          p(icon("area-chart"), "Events by Year"),
          # Detailed Information
          h4(textOutput("textTitle2")),
          showOutput("CasesbyYear",  "nvd3"),
          showOutput("CasesbyYearAll",  "nvd3")),  # End Fatalities Tab
        
        #Fatal Events
        tabPanel(
          p(icon("area-chart"), "Fatal"),
          h4(textOutput("textTitle1")),
          showOutput("casestypeout",  "nvd3")),
        
        #Non-Fatal Events
        tabPanel(
          p(icon("area-chart"), "Non-Fatal"),
          h4("Injury Sources"),
          
          showOutput("nf_source","nvd3"),
          tags$hr(),
          h4("Injury Events"),
          showOutput("nf_event","nvd3")
          
          ),
          
        
        #Map
        tabPanel(
          p(icon("area-chart"), "Event Map"),
          #(textOutput("textTitle1")),
          #showOutput("casestypeout",  "nvd3"),
          leafletOutput("FatalbyLocation")
          #p("Fatal Accidents over Time"),
          #
          
          # Fatalities Over Time By Category
          
          
          # fluidRow(
          #   column(6, h4("View Incidents by Year"), 
          #          p(showOutput("cases.year.out", "nvd3"))),
          # 
          #   column(6,h4("View Incidents by Year and Type"),
          #          p(showOutput("secondaryBikeCatOut", "nvd3")))
          # ),
          
          # Detailed Information
          #h4("All Fatalities by Year"),
          #showOutput("CasesbyYear",  "nvd3")
          )
        
     # End Data Tab
      )
    )
  ), # End Analytics Tab Panel
  
  # About Tab Panel ------------------------------------------------------           
  tabPanel("Supporting Docs",
           mainPanel(column(8, offset = 2, includeMarkdown("about.md"))
           )
  ), # End About Tab Panel
  
  # Data Tab ---------------------------------------------
  tabPanel("Data",
           tabsetPanel(
             
             tabPanel(
               p(icon("table"), "Fatalities"),
               
               fluidRow(
                 dataTableOutput(outputId="table", width = "90%")
                 
               )),
             
  
  tabPanel(
    p(icon("table"), "Non-Fatal Source"),
    
    fluidRow(
      dataTableOutput(outputId="nf_source_table")
      
    ))
  ))
  
) # End navbarPage
) # End shinyUI
