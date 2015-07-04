
library(shiny)
library(leaflet)
source('functions.R')

shinyServer(function(input, output) {
  
  output$mapIntro <- renderLeaflet({
    MakeIntroMap(input$cityIntro)
  })
               
  output$distIntro <- renderChart({
    p <- PlotDist(input$cityIntro)
    p$set(dom = "distIntro", width = 300, height = 225)
    p
  })
  
  output$censusData <- renderDataTable(
    census[input$censusVars],
    options = list(pageLength = 7, pagingType = "simple", lengthChange = FALSE)
  )
  
  output$mapCities <- renderLeaflet({
    MapCities(input$cityForCities, FALSE)
  })

  output$mapZips <- renderLeaflet({
    MapZipsAndPoints(input$cityForZips)
  })
  
  output$mapGrids <- renderLeaflet({
    MapSquaresAndPoints(input$cityForGrids)
  })
  
  output$data1 <- renderDataTable(
      data[input$dataVars1],
      options = list(pageLength = 9, pagingType = "simple", lengthChange = FALSE)
  )
  
  output$data2 <- renderDataTable(
      data[input$dataVars2],
      options = list(pageLength = 9, pagingType = "simple", lengthChange = FALSE)
  )
  
  output$distribution <- renderChart({
      p <- PlotDist(input$cityExPlots1, TRUE)
      p$set(dom = "distribution", height = 300, width = 300)
      p
  })
  
  output$mean2var <- renderChart({
      p <- MakeStatsPlot(input$cityExPlots1)
      p$set(dom = "mean2var", height = 300, width = 300)
      p
  })
  
  output$mean <- renderText({
    filter(stats, cityName == input$cityExPlots1)$mean
  })
  
  output$variance <- renderText({
    filter(stats, cityName == input$cityExPlots1)$var
  })
  
  output$byPopDens <- renderChart({
      p <- PlotByPopDens(input$cityExPlots2, input$smoother)
      p$set(dom = "byPopDens", height = 300, width = 500)
      p
  })
  
  output$byOtherVars <- renderChart({
      p <- PlotByOtherVars(input$cityExPlots2, input$smoother)
      p$set(dom = "byOtherVars", height = 1000, width = 400)
      p
  })
  
  output$popDensByVars <- renderChart({
      p <- PlotPopDensByVars(input$cityExPlots2, input$smoother)
      p$set(dom = "popDensByVars", height = 1000, width = 400)
      p
  })
  
})