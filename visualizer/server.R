
#Import Data
west_coast_fatal <- fread("west_coast_fatal_state.csv")
west_coast_fatal[, Outcome := "Fatal"]
setnames(west_coast_fatal, "Incident Type", "Type")
west_coast_fatal[, Cases := as.numeric(as.character(Fatalities))]

#add non-fatal
west_coast_nonfatal <- fread("west_coast_non_fatal_state.csv")

#merge fatal and nonfatal by State
fatal_state = west_coast_fatal[, list(`Fatalities` =
                                 sum(Fatalities)), by=c("Year","State","Type")]
setkey(fatal_state, Year, State)
setkey(west_coast_nonfatal, Year, State)

merge_state = merge(fatal_state, west_coast_nonfatal, all=T)

setnames(merge_state, "Fatalities", "Fatal")
setnames(merge_state, "Nonfatal Injuries", "Non-Fatal")

merge_state_melt = melt(merge_state,
                        measure.vars = c("Fatal","Non-Fatal"),
     value.name = "Cases", variable.name = "Outcome")

merge_state_melt = merge_state_melt[!is.na(Cases),]

non_fatal_source = fread("west_coast_non_fatal_source.csv")
non_fatal_event = fread("west_coast_non_fatal_event.csv")

# 
# # Setup inputs
cat1 <- sort(unique(merge_state_melt$Outcome))
cat2 <- c(sort(unique(west_coast_fatal$`Type`)))
stateCat <- c(sort(unique(c(west_coast_fatal$State,merge_state_melt$State) )))
yearMax <- max(west_coast_fatal$Year)
yearMin <- min(west_coast_fatal$Year)
fatalitiesMin <- min(west_coast_fatal$Fatalities)
fatalitiesMax <- max(west_coast_fatal$Fatalities)
nonfatalMin <- min(merge_state_melt$`Cases`, na.rm=T)
nonfatalMax <- max(merge_state_melt$`Cases`, na.rm=T)

# 
# typesList <- cat2
# 
# Server logic -----------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Setup reactive inputs ------------------------------------------------

  output$yearSlider <- renderUI({
    sliderInput(inputId = "year.in",
                label   = "Year Filter",
                min     = yearMin,
                max     = yearMax,
                step    = 1,
                value   = c(yearMin, yearMax),
                ticks   = T,
                sep     = "")
  })
  
   output$casesSlider <- renderUI({
    sliderInput(inputId = "cases.in",
                label   = "Minimum Cases",
                min     = min(fatalitiesMin, nonfatalMin),
                max     = max(fatalitiesMax , nonfatalMax),
                step    = 1,
                value   = 0,
                ticks   = T,
                sep     = "")
  })

  output$cat1Controls <- renderUI({
    checkboxGroupInput('cat1', label = "Fatal or Non-Fatal",
                       choices = cat1, selected = cat1)
  })



  output$cat2Controls <- renderUI({
    checkboxGroupInput('cat2', label = "Injury Type",
                       choices = cat2, selected = cat2)
  })

  output$stateControls <- renderUI({
    checkboxGroupInput('state.in', label = "State",
                       choices = stateCat, selected = stateCat)
  })

  observe({
    if(input$reset != 0) {
      updateCheckboxGroupInput(session, "cat1", choices = cat1, selected=cat1)
      updateCheckboxGroupInput(session, "cat2", choices = cat2, selected=cat2)
      updateSliderInput(session, "cases.in", val = c(min(fatalitiesMin, nonfatalMin),
                                                     max(fatalitiesMax, nonfatalMax)))
      updateCheckboxGroupInput(session, "state.in", choices = stateCat, selected = stateCat)
      updateSliderInput(session, "year.in", val = c(yearMin, yearMax))
    }

  })
  



  # Data Reactivity ------------------------------------------------------

  west_coast_fatal.filtered <- reactive({
    # Error handling
    if (is.null(input$year.in) |
        is.null(input$cat1)    |
        is.null(input$cat2)    |
        is.null(input$cases.in)|
        is.null(input$state.in)
    ) {
      return(NULL)
    }

    west_coast_fatal[Year     >=   input$year.in[1] &
             Year    <=   input$year.in[2]&
             Outcome %in% input$cat1 & 
             State %in% input$state.in &
             Type %in% input$cat2,]

  })
  
  merge_state_melt.filtered <- reactive({
    # Error handling
    if (is.null(input$year.in) |
        is.null(input$cat1)    |
        is.null(input$cat2)    |
        is.null(input$cases.in)|
        is.null(input$state.in)
    ) {
      return(NULL)
    }
    
    merge_state_melt[Year     >=   input$year.in[1] &
                       Year    <=   input$year.in[2]&
                       Type %in% c(input$cat2,NA) &
                       Outcome %in% input$cat1 & 
                       State %in% input$state.in,]
    
  })
  
  non_fatal_event.filtered <- reactive({
    # Error handling
    if (is.null(input$year.in) 
        
    ) {
      return(NULL)
    }
    
    non_fatal_event[Year     >=   input$year.in[1] &
                       Year    <=   input$year.in[2]]
    
  })
  
  non_fatal_source.filtered <- reactive({
    # Error handling
    if (is.null(input$year.in) 
        
    ) {
      return(NULL)
    }
    
    non_fatal_source[Year     >=   input$year.in[1] &
                      Year    <=   input$year.in[2]]
    
  })

  cat1SalesByYear <- reactive({
    if(!is.null(west_coast_fatal.filtered())){
    temp = west_coast_fatal.filtered()[,list(casestotal =  sum(Cases)), 
                                by=c("Year","State","Type")]
    temp[, Year := as.numeric(as.character(Year))]
    setkey(temp, "Year", "State","Type")
    
    temp
    }
  })

  fatalitiesByType <- reactive({
    if(!is.null(west_coast_fatal.filtered())){
    temp1 = west_coast_fatal.filtered()
    temp2 = west_coast_fatal.filtered()
    temp1 = temp1[, list(casestotal = sum(Cases)),
                  by=c("Year","State","Type")]
    temp2 = temp2[, list(casestotal = sum(Cases)),
                  by=c("Year","State","Type")]
    temp1[casestotal<input$cases.in[1], `Type` := "Aggregated"]
    temp2 =  temp2[casestotal<input$cases.in[1], ]
    temp2[, casestotal := 0]
    temp1 = temp1[, list(casestotal = sum(casestotal)),
                  by=c("Year","State","Type")]
    temp = rbindlist(list(temp1, temp2[,c("Year","State","Type","casestotal"), with=F]))
    
    #use expand.grid to fill in all options
    
    allpossible= 
      expand.grid(Year = input$year.in[1]:input$year.in[2], 
                  State= input$state.in[!input$state.in=="Unknown/transit"], 
                  Type = c(input$cat2, "Aggregated"), 
                casestotal = 0)
    
    temp = rbindlist(list(temp, allpossible))
    
    temp = temp[, list(casestotal = sum(casestotal)),
                  by=c("State","Type")]
    
    setkey(temp, "Type", "State")
    
    temp
    }
  })
  
  numbersBystate <- reactive({
    if(!is.null(merge_state_melt.filtered ())){
      temp1 = temp2 = merge_state_melt.filtered ()
      temp1 = temp1[, list(casestotal = sum(Cases)),
                    by=c("Year","State","Outcome")]
      temp2 = temp2[, list(casestotal = sum(Cases)),
                    by=c("Year","State","Outcome")]
      temp1[casestotal<input$cases.in[1], `Type` := "Aggregated"]
      temp2 =  temp2[casestotal<input$cases.in[1], ]
      temp2[, casestotal := 0]
      temp1 = temp1[, list(casestotal = sum(casestotal)),
                    by=c("Year","State","Type")]
      temp = rbindlist(list(temp1, temp2[,c("Year","State","Outcome","casestotal"), with=F]))
      
      #use expand.grid to fill in all options
      
      allpossible= 
        expand.grid(Year = input$year.in[1]:input$year.in[2], 
                    State= c(input$state.in), 
                    Type = c(input$cat2,"Aggregated"), 
                    casestotal = 0)
      
      temp = rbindlist(list(temp, allpossible))
      
      temp = temp[, list(casestotal = sum(casestotal)),
                  by=c("Year","State")]
      
      setkey(temp, Year)
      temp
    }
  })
  
  numbersBystateMap <- reactive({
    if(!is.null(merge_state_melt.filtered ())){
      temp1 = merge_state_melt.filtered ()
      temp2 = merge_state_melt.filtered ()
      temp1 = temp1[, list(casestotal = sum(Cases)),
                    by=c("Year","State","Outcome")]
      temp2 = temp2[, list(casestotal = sum(Cases)),
                    by=c("Year","State","Outcome")]
      temp1[casestotal<input$cases.in[1], `Type` := "Aggregated"]
      temp2 =  temp2[casestotal<input$cases.in[1], ]
      temp2[, casestotal := 0]
      temp1 = temp1[, list(casestotal = sum(casestotal)),
                    by=c("Year","State","Outcome")]
      temp = rbindlist(list(temp1, temp2[,c("Year","State","Outcome","casestotal"), with=F]))
      
      #use expand.grid to fill in all options
      
      allpossible= 
        expand.grid(Year = input$year.in[1]:input$year.in[2], 
                    State= c(input$state.in, "Unknown/transit"), 
                    Type = c(input$cat1), 
                    casestotal = 0)
      
      temp = rbindlist(list(temp, allpossible))
      
      temp = temp[, list(Cases = sum(casestotal)),
                  by=c("Year","State","Outcome")]
      
      setkey(temp, Year)
      temp
    }
  })
  



  topNsales <- reactive({
    # Helper function
    temp = data.table(west_coast_fatal.filtered())[,list(sum = sum(`Fatalities`)),
                                       by=`Type`]
    temp

  })

  # Plots on Analysis Tab ------------------------------------------------

  output$textTitle1 =renderText(paste(ifelse(is.na(input$cat1[2]),
                                             input$cat1[1], 
                                             input$cat1[1]),
                                             #paste(input$cat1[1], " and ", 
                                              #     input$cat1[2])),
                                      "Event Types from",
                                      input$year.in[1], "to",
                                      input$year.in[2]))
  
  output$textTitle2 =renderText(paste("Number of",  
                                      ifelse(is.na(input$cat1[2]),
                                                     input$cat1[1], 
                                                     paste(input$cat1[1], " and ", 
                                                           input$cat1[2])),
                                      "events per year:"))
  
  
                                      
  
  # rChart - Sales By Category1
  output$casestypeout <- renderChart({
    # Error handling
    if (is.null(west_coast_fatal.filtered())) return(rCharts$new())

    casesyearoutDF <- nPlot(
      casestotal ~ State,
      group =  "Type",
      data = fatalitiesByType(),
      type = "multiBarHorizontalChart",
      dom = "casestypeout",
      width=800
    )

    casesyearoutDF$chart(margin = list(left = 85))
    casesyearoutDF$yAxis(axisLabel = "Cases", width = 100)
    casesyearoutDF$xAxis(axisLabel = "", width = 70)
    casesyearoutDF$chart(showControls = FALSE)
    # casesyearoutDF$chart(stacked = T)
    # casesyearoutDF$set(dom = "casesyearout")
    # casesyearoutDF$show('iframesrc', cdn = FALSE)

    return(casesyearoutDF)
  })

  output$CasesbyYear <- renderChart({
    # Error handling
    if (is.null(merge_state_melt.filtered())) return(rCharts$new())

    CasesbyYearDF <- nPlot(
      casestotal ~ Year ,
      group = "State",
      data = numbersBystate(),
      type = "lineChart",
      dom = "CasesbyYear",
      width= 800

    )
    
    CasesbyYearDF$chart(margin = list(left = 85))
    CasesbyYearDF$yAxis(axisLabel = "Cases", width = 100)
    #CasesbyYearDF$xAxis(axisLabel = "Year", width = 80)
    #CasesbyYearDF$chart(stacked = T)
    CasesbyYearDF
  })
  
  output$nf_source <- renderChart({
    
    if (is.null(non_fatal_source.filtered()))
      return(rCharts$new())
    
    textX = paste("Non-Fatal Cases from",
                    input$year.in[1], "to",
                    input$year.in[2])
    
    nf_sourceDF <- nPlot(
      x = "Source" ,
      y = "V1",
      group = "Source",
      data = non_fatal_source.filtered()[,sum(`Nonfatal Injuries`),by=Source],
      type = "multiBarHorizontalChart",
      dom = "nf_source",
      width= 800
      
    )
    #nf_sourceDF$chart(donut = TRUE)
    nf_sourceDF$chart(showControls = F)
    nf_sourceDF$chart(stacked = T)
    nf_sourceDF$yAxis(axisLabel = textX)
    nf_sourceDF$setTemplate(afterScript='<style> svg text {font-size: 9px;}</style>')
    nf_sourceDF$chart(margin = list(left = 200))
    nf_sourceDF
    
  }
  )
  
  output$nf_event <- renderChart({
    
    if (is.null(non_fatal_event.filtered()))
      return(rCharts$new())
    
    textX = paste("Non-Fatal Cases from",
                  input$year.in[1], "to",
                  input$year.in[2])
    
    nf_eventDF <- nPlot(
      x = "Event" ,
      y = "V1",
      group = "Event",
      data = non_fatal_event.filtered()[,sum(`Nonfatal Injuries`),by=Event],
      type = "multiBarHorizontalChart",
      dom = "nf_event",
      width= 800
      
    )
    
    nf_eventDF$chart(showControls = F)
    nf_eventDF$chart(stacked = T)
    nf_eventDF$yAxis(axisLabel = textX)
    nf_eventDF$setTemplate(afterScript='<style> svg text {font-size: 9px;}</style>')
    nf_eventDF$chart(margin = list(left = 200))
    nf_eventDF
    
  }
  )

#
  # # Leaflet - Customers by Location
output$FatalbyLocation <- renderLeaflet({
  # Error handling
  if (is.null(numbersBystateMap ())) return(NULL)
  if(is.null(input$cat1)) return(NULL)
  mapdata = numbersBystateMap()
  # mapdata = merge_state_melt
  unkown.gps = c( -135, 53)
  california.gps = c( -127, 39.093265)
  oregon.gps = c(-127, 45.496367)
  washington.gps = c(-127, 47.951875)
  mapdatalocations = data.table(State = c("California", "Oregon", 
                                          "Washington","Unknown/transit"),
                                Longitude  = c(california.gps[1],
                                               oregon.gps[1],
                                               washington.gps[1],
                                               unkown.gps[1]),
                                Latitude  = c(california.gps[2],
                                              oregon.gps[2],
                                              washington.gps[2],
                                              unkown.gps[2]))
  
  mapdata = mapdata[,list(cases= sum(Cases)), by=c("State","Outcome")]
  setkey(mapdata, State)
  setkey(mapdatalocations, State)
  mapdata = mapdatalocations[mapdata]
  mapdata[Outcome == "Fatal", Longitude := Longitude - 3]
  
  x = mapdata$cases
  x = (scale(x,center=min(x),scale=diff(range(x)))+.2)*20
  
  oceanIcons <- iconList(
    `Non-Fatal` = makeIcon("ferry-18.png", 18, 18),
    `Fatal` = makeIcon("danger-24.png", 24, 24)
  )
  
  textMap = paste(ifelse(is.na(input$cat1[2]),
                          input$cat1[1],
                   paste(input$cat1[1], " and ",
                       input$cat1[2])),
                   "Event Types from",
                   input$year.in[1], "to",
                   input$year.in[2])
  
  pal <- colorFactor(c("red","blue"), mapdata$Outcome)
  
  

  leaflet(mapdata) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = mean(mapdatalocations$Longitude),
                       lat = mean(mapdatalocations$Latitude),
                       zoom=4) %>%
    addCircleMarkers(lng = ~Longitude,
               lat = ~Latitude,
               color = ~pal(mapdata$Outcome),
               radius = x,
               stroke=F
               ) %>%
    addMarkers(lng = ~Longitude,
               lat = ~Latitude,
               #color = ~pal(Outcome),
               icon = oceanIcons[mapdata$Outcome],
               #stroke = F,
               #weight = 2,
               #radius = x, 
               label  = lapply(paste(mapdata$Outcome,
                             mapdata$State, "<br>",
                                 mapdata$cases,"Cases"), HTML),
               labelOptions = labelOptions(style = 
                                             list("border" = "none",
                                                  "font-weight" = "normal", 
                                                  padding = "1px 4px"),
                                           textsize = "12px",
                                           direction = "auto"))  %>%
  addControl(strong(textMap), position = "topright")
  
})
  #
#   # # Plotly - Sales By State        
#   # output$salesByStateOut <- renderPlotly({
#   #   # Error handling
#   #   if (is.null(west_coast_fatal.filtered())) return(NULL)
#   #   
#   #   # give state boundaries a white border
#   #   l <- list(color = toRGB("black"), width = 1)
#   #   
#   #   # specify some map projection/options
#   #   g <- list(
#   #     scope = 'usa',
#   #     projection = list(type = 'albers usa'),
#   #     showlakes = TRUE,
#   #     lakecolor = toRGB('white')
#   #   )
#   #   
#   #   plot_ly(salesByState(), z = total.sales, text = hover, 
#   #           locations = bikeshop.state, type = 'choropleth',
#   #           locationmode = 'USA-states', color = total.sales, colors = 'Blues',
#   #           marker = list(line = l), colorbar = list(title = "USD")) %>%
#   #     layout(geo = g)
#   #   
#   # })        
#   # 
#   # # Plotly - Top N Products
#   output$topNout <- renderChart({
#     
#     # Error handling
#     if (is.null(topNsales())) return(rCharts$new())
#     
#     cat2DF <- nPlot(
#       sum ~ `Type`,
#       data = topNsales(),
#       type = "discreteBarChart",
#       dom = "topNout",
#       width=500
#      
#     )
#     cat2DF$xAxis(axisLabel = "Fatalities", width = 80)
#     cat2DF
#   })
#     
#     
#   # # Plotly - Dynamic Graphs By Unit Price
#   # output$salesByUnitPriceOut <- renderPlotly({
#   #   # Error handling
#   #   if (is.null(west_coast_fatal.filtered())) return(NULL)
#   #   
#   #   # Get selection (add 3 to offset for first three columns)
#   #   colIndex <- which(selectorList == input$salesVsQty.in) + 3
#   #   
#   #   gg <- ggplot(data=salesByUnitPrice(), aes_string(x=colnames(salesByUnitPrice()[3]), y=colnames(salesByUnitPrice()[colIndex]))) + 
#   #     geom_point(aes(text = paste("Model:", product)), size = 1.5) + 
#   #     geom_smooth(aes(color= category2, fill = category2)) + 
#   #     facet_wrap(~category2) +
#   #     theme(legend.position = "none",
#   #           axis.text.x  = element_text(angle=45)) +
#   #     scale_x_continuous(labels = scales::dollar) + 
#   #     scale_y_continuous(labels = ifelse(
#   #       colIndex == 4, dollar, 
#   #       ifelse(colIndex == 5, comma, percent)
#   #     )) +
#   #     xlab("Unit Price") +
#   #     ylab(input$salesVsQty.in)
#   #   
#   #   p <- ggplotly(gg)
#   #   
#   #   p %>%
#   #     layout(autosize = F, height = 650, width = 600, margin=list(l=150, b=110))
#   # })
#   # 
#   # 
#   # 
#   # Data and button on Data Tab-------------------------------------------
# 
#  
  output$table <- DT::renderDataTable({cat1SalesByYear()[casestotal>=input$cases.in[1],]},
                                      rownames = F,
                                      options = list(bFilter = FALSE,
                                                     iDisplayLength = 10)
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(west_coast_fatal.filtered(), file, row.names=FALSE)
    }
  )
#   
#   
 })