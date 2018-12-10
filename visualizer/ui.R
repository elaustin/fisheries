#Allowing the "enter" key to work on the app
jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

# Adjustments
h3.align <- 'center'

# Shiny UI ---------------------------------------------------------------------
shinyUI(navbarPage(
  title = "Fisheries Dashboard",
  
  # Pick a bootstrap theme from https://rstudio.github.io/shinythemes/
  theme = shinytheme("flatly"),
  
  # Analytics tab panel --------------------------------------------------        
  tabPanel(
    tags$head(tags$script(HTML(jscode))),
    title = "Password",
    tagAppendAttributes(
      textInput("key", "Please type your password", ""),
      `data-proxy-click` = "goButton"),
    actionButton("goButton", "Submit Password"),
    br(),
    hr(),
    "After entering your password select one of the other tabs (ex. Analytics).",
    br(),
    "If you enter the incorrect password, please refresh this page.",
    br(),
    h5(a("Please provide feedback via this form.", 
         href = "https://goo.gl/forms/FG3e8sXOI6X6DFhb2"))),
  tabPanel(
    title = "Analytics",
    # 
    sidebarPanel(width=3,
                 h3("Customize Variables", align = h3.align),
                 h6("Make selections and watch the graphs and data update", align="top"),
                 uiOutput("yearSlider"),
                 uiOutput("casesSlider"),
                 uiOutput("cat1Controls"),
                 uiOutput("cat2Controls"),
                 uiOutput("stateControls"),
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
          showOutput("CasesbyYear",  "nvd3")),  # End Fatalities Tab
        
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
    p(icon("table"), "Fatalities Data"),
    
    fluidRow(
      column(6, h3("Search, Filter & Download Data", align='left')),
      column(6, downloadButton('downloadData', 'Download', class="pull-right"))
      
    ),
    hr(),
    fluidRow(
      dataTableOutput(outputId="table")
      
    ))
  ))
  
) # End navbarPage
) # End shinyUI
