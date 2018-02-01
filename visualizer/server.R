#orders.extended <- read.data() # helper.R function

orders.extended <- fread("West Coast Fatalities.csv")


orders.extended <- melt(orders.extended, 
                        id.vars = "Year", 
                        variable.name = "Type",
                        value.name = "Cases")
orders.extended[grep("Fatal",Type), Outcome := "Fatal"]
orders.extended[ , Type := sub("Fatal ","",Type)]

# Setup inputs
cat1 <- sort(unique(orders.extended$Outcome))
cat2 <- c(sort(unique(orders.extended$Type)))
yearMax <- max(orders.extended$Year)
yearMin <- min(orders.extended$Year)
casesMin <- min(orders.extended$Cases)
casesMax <- max(orders.extended$Cases)
casesPriceMin <- min(orders.extended$Cases)
typesList <- c("Vessel Disasters", "Fall Overboard",   "Onboard Injury",
                  "Onshore Injury",   "Diving Injury")

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
  
  output$unitPriceSlider <- renderUI({
    sliderInput(inputId = "price.in", 
                label   = "Minimum Cases", 
                min     = casesMin, 
                max     = casesMax, 
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
  
  observe({
    if(input$reset != 0) {
      updateCheckboxGroupInput(session, "cat1", choices = cat1, selected=cat1)
      updateCheckboxGroupInput(session, "cat2", choices = cat2, selected=cat2)
      updateSliderInput(session, "price.in", val = c(casesMin, casesMax))
      updateSliderInput(session, "year.in", val = c(yearMin, yearMax))
    }
    
  })
  
  output$nProductsTextbox <- renderUI({
    numericInput("nProducts.in", "Top N Injuries", length(typesList), min = 0, 
                 max = unique(orders.extended.filtered()$Type))
  })
  
  output$salesVsQtySelector <- renderUI({
    selectInput("salesVsQty.in", "Select Measure", 
                choices = selectorList, 
                selected = selectorList[1])
  })
  
  # Data Reactivity ------------------------------------------------------
  
  orders.extended.filtered <- reactive({
    # Error handling 
    if (is.null(input$year.in) | 
        is.null(input$cat1)    | 
        is.null(input$cat2)    |
        is.null(input$price.in)
    ) {
      return(NULL)
    } 
    
    orders.extended[Year      >=   input$year.in[1] &
             Year      <=   input$year.in[2]&
             #Cases     >=   input$price.in[1]&
             #Cases     <=   input$price.in[2]&
             Outcome %in% input$cat1&
             Type %in% input$cat2,]
    
    
    
  })
  
  cat1SalesByYear <- reactive({
    orders.extended.filtered() %>%
      group_by(Outcome, Year) %>%
      summarize(cases.total = sum(Cases))
  })
  
  cat2SalesByYear <- reactive({
    temp1 = data.table(orders.extended.filtered())[,
                                                  cases.total := sum(Cases)
                                                  ,by=c("Type","Year")]
    
    temp2 = data.table(orders.extended.filtered())[,
                                                   cases.total := sum(Cases)
                                                   ,by=c("Type","Year")]
    temp1[cases.total<input$price.in[1], Type := "Aggregated"]
    temp2 =  temp2[cases.total<input$price.in[1], ]
    temp2[, cases.total := 0]
    temp1 = temp1[, list(cases.total = sum(Cases)),
                by=c("Year","Type")]
    temp = rbindlist(list(temp1, temp2[,c("Year","Type","cases.total"), with=F]))
    
    setkey(temp, Year)
    temp
  })
  
 
  
  topNsales <- reactive({
    # Helper function
    temp = orders.extended.filtered()[,list(sum = sum(Cases)),by=Type]
    temp
    
  })
  
  # Plots on Analysis Tab ------------------------------------------------
  
  # rChart - Sales By Category1
  output$primaryBikeCatOut <- renderChart({
    # Error handling
    if (is.null(orders.extended.filtered())) return(rCharts$new())
    
    cat1SalesByYearOutDF <- nPlot(
      cases.total ~ Year,
      group =  "Outcome",
      data = cat1SalesByYear(),
      type = "multiBarChart",
      dom = "primaryBikeCatOut",
      width= 500
     
    )
    
    cat1SalesByYearOutDF$chart(margin = list(left = 85))
    cat1SalesByYearOutDF$yAxis(axisLabel = "Cases", width = 80)
    cat1SalesByYearOutDF$xAxis(axisLabel = "Year", width = 70)
    cat1SalesByYearOutDF$chart(stacked = T)
    
    cat1SalesByYearOutDF
  })
  
  # rCharts - Sales By Category2
  output$secondaryBikeCatOut <- renderChart({
    # Error handling
    if (is.null(orders.extended.filtered())) return(rCharts$new())
    
    cat2SalesByYearOutDF <- nPlot(
      cases.total ~ Year,
      group = "Type",
      data = cat2SalesByYear(),
      type = "multiBarHorizontalChart",
      dom = "secondaryBikeCatOut",
      width= 500
      
    )
    cat2SalesByYearOutDF$yAxis(axisLabel = "Fatalities", width = 80)
    cat2SalesByYearOutDF$chart(stacked = T)
    cat2SalesByYearOutDF
  })
  
  # # Leaflet - Customers by Location
  # output$salesByLocOut <- renderLeaflet({
  #   # Error handling
  #   if (is.null(orders.extended.filtered())) return(NULL)
  #   
  #   leaflet(salesByLocation()) %>% 
  #     addProviderTiles("CartoDB.Positron") %>%
  #     addMarkers(lng = ~longitude, 
  #                lat = ~latitude,
  #                popup = ~popup) %>%
  #     addCircles(lng = ~longitude, 
  #                lat = ~latitude, 
  #                weight = 2,
  #                radius = ~(total.sales)^0.775)
  # })
  # 
  # # Plotly - Sales By State        
  # output$salesByStateOut <- renderPlotly({
  #   # Error handling
  #   if (is.null(orders.extended.filtered())) return(NULL)
  #   
  #   # give state boundaries a white border
  #   l <- list(color = toRGB("black"), width = 1)
  #   
  #   # specify some map projection/options
  #   g <- list(
  #     scope = 'usa',
  #     projection = list(type = 'albers usa'),
  #     showlakes = TRUE,
  #     lakecolor = toRGB('white')
  #   )
  #   
  #   plot_ly(salesByState(), z = total.sales, text = hover, 
  #           locations = bikeshop.state, type = 'choropleth',
  #           locationmode = 'USA-states', color = total.sales, colors = 'Blues',
  #           marker = list(line = l), colorbar = list(title = "USD")) %>%
  #     layout(geo = g)
  #   
  # })        
  # 
  # # Plotly - Top N Products
  output$topNout <- renderChart({
    
    # Error handling
    if (is.null(topNsales())) return(rCharts$new())
    
    cat2DF <- nPlot(
      sum ~ Type,
      data = topNsales(),
      type = "discreteBarChart",
      dom = "topNout",
      width=500
     
    )
    cat2DF$xAxis(axisLabel = "Fatalities", width = 80)
    cat2DF
  })
    
    
  # # Plotly - Dynamic Graphs By Unit Price
  # output$salesByUnitPriceOut <- renderPlotly({
  #   # Error handling
  #   if (is.null(orders.extended.filtered())) return(NULL)
  #   
  #   # Get selection (add 3 to offset for first three columns)
  #   colIndex <- which(selectorList == input$salesVsQty.in) + 3
  #   
  #   gg <- ggplot(data=salesByUnitPrice(), aes_string(x=colnames(salesByUnitPrice()[3]), y=colnames(salesByUnitPrice()[colIndex]))) + 
  #     geom_point(aes(text = paste("Model:", product)), size = 1.5) + 
  #     geom_smooth(aes(color= category2, fill = category2)) + 
  #     facet_wrap(~category2) +
  #     theme(legend.position = "none",
  #           axis.text.x  = element_text(angle=45)) +
  #     scale_x_continuous(labels = scales::dollar) + 
  #     scale_y_continuous(labels = ifelse(
  #       colIndex == 4, dollar, 
  #       ifelse(colIndex == 5, comma, percent)
  #     )) +
  #     xlab("Unit Price") +
  #     ylab(input$salesVsQty.in)
  #   
  #   p <- ggplotly(gg)
  #   
  #   p %>%
  #     layout(autosize = F, height = 650, width = 600, margin=list(l=150, b=110))
  # })
  # 
  # 
  # 
  # Data and button on Data Tab-------------------------------------------

 
  output$table <- DT::renderDataTable({cat2SalesByYear()[cases.total>=input$price.in[1],]},
                                      rownames = F,
                                      options = list(bFilter = FALSE,
                                                     iDisplayLength = 10)
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(orders.extended.filtered(), file, row.names=FALSE)
    }
  )
  
  
})