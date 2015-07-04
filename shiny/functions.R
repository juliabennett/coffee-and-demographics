
library(sp)
library(rgdal) 
library(rgeos)
library(dplyr)
library(reshape)
library(rCharts)
library(leaflet)

# Read files saved by makeCoffeeData.R for making maps and plots.
cityShapes <- readOGR("cityShapes", "cityShapes")
squares <- readOGR("squares", "squares")
zipShapes <- readOGR("zipShapes", layer = "zipShapes") 
intTests <- read.csv("intTests.csv", stringsAsFactors = FALSE)
census <- read.csv("census.csv", stringsAsFactors = FALSE)
cafes <- read.csv("cafes.csv")
data <- read.csv("data.csv", stringsAsFactors = FALSE) 

FindIntersectingZips <- function(city) { 
  # Finds all ZIP codes that intersect a city. 
  intTestString <- filter(intTests, cityName == city)$zips # Use saved results
  indexStrings <- unlist(strsplit(intTestString, split = ";"))
  zipShapes[as.numeric(indexStrings), ]
}

FindCoffee <- function(city, scope = "more") {
  # Finds all coffee shops near a city. 
  #  If scope = "more", then it includes all coffee shops that are contained in 
  #  a ZIP code intersecting this city. If scope = "less", then it only includes
  #  coffee shops that are contained in either city boundaries or in the "good"
  #  grid associated to this city. 
  if (scope == "more"){
    intTestString <- filter(intTests, cityName == city)$coffeeInZips
  } else if (scope == "less") {
    intTestString <- filter(intTests, cityName == city)$coffeeInSquaresOrCity
  }
  indexStrings <- unlist(strsplit(intTestString, split = ";"))
  cafes[as.numeric(indexStrings), ]
}

MapCities <- function(city, factualAtt = TRUE) {
  # Creates a map displaying city boundaries. 
  
  dataCurr <- filter(data, cityName == city)
  cityShape <- cityShapes[cityShapes$NAMELSAD == dataCurr$placeName[1], ]
  
  tile = "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
  if (factualAtt){
    att = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> 
          &copy; <a href="http://cartodb.com/attributions">CartoDB</a> 
          | Places data &copy; Factual Inc. 
          (<a href = "http://www.factual.com">http://www.factual.com</a>)'
  }
  else {
    att = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> 
          &copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
  }
  
  map <- addTiles(leaflet(), tile, attribution = att)
  map <- setView(map, dataCurr$long[1] - .13, dataCurr$lat[1], zoom = 11)
  map <- addPolygons(map, data = cityShape, opacity = .6, weight = 1.5, 
                     color = "#00711F", popup = city)
  map
}

MapZipsAndPoints <- function(city) {
  # Creates a map displaying city boundaries, ZIP code boundaries, and coffee shop locations.
  
  dataCurr <- filter(data, cityName == city)
  cityShape <- cityShapes[cityShapes$NAMELSAD == dataCurr$placeName[1], ]
  
  zipShapesCurr <- FindIntersectingZips(city)
  points <- FindCoffee(city, "more")
  
  map <- MapCities(city)
  map <- addPolygons(map, data = zipShapesCurr, opacity = .6, weight = 1.5, 
                     color = "#C9A500", popup = zipShapesCurr$ZCTA5CE10)
  map <- addCircles(map, data = points, radius = 75, stroke = FALSE,
                    color = "#6E07E8", fillOpacity = 1, popup = points$name)
  map
}

MapSquaresAndPoints <- function(city) { 
  #Creates a map displaying city boundaries, boundaries for both a "bad" and 
  #  "good" grid approximately covering this city, and coffee shop locations.
  
  dataCurr <- filter(data, cityName == city)
  cityShape <- cityShapes[cityShapes$NAMELSAD == dataCurr$placeName[1], ]
  
  zipShapesCurr <- FindIntersectingZips(city)
  points <- FindCoffee(city, "more")
  
  gridPoly <- squares[squares$placeName == dataCurr$placeName[1], ]
  gridPolyGood <- gridPoly[gridPoly$goodSquare == 1, ] 
  
  squarePop <- paste("This square has", dataCurr$countInSquare, "coffee shops.")
  squarePop[dataCurr$countInSquare == 1] <- paste("This square has 1 coffee shop.")
  
  map <- MapCities(city)
  map <- addPolygons(map, data = gridPoly, color = "#8D998D", weight = .75, opacity = .4, 
                     popup = "This square isn't included in the final grid.")
  map <- addPolygons(map, data = gridPolyGood, color = "#1C1C1C", weight = 1, 
                     opacity = 0.25, popup =  squarePop)
  map <- addCircles(map, data = points, radius = 75, stroke = FALSE,
                    color = "#6E07E8", fillOpacity = 1, popup = points$name)
  map
}

MakeIntroMap <- function(city) {
  #Creates a map displaying city boundaries, boundaries for a "good" grid 
  #  approximately covering this city, and coffee shop locations.
  
  dataCurr <- filter(data, cityName == city)
  cityShape <- cityShapes[cityShapes$NAMELSAD == dataCurr$placeName[1], ]
  
  points <- FindCoffee(city, "less")
  
  gridPoly <- squares[squares$placeName == dataCurr$placeName[1], ]
  gridPolyGood <- gridPoly[gridPoly$goodSquare == 1, ] 
  
  squarePop <- paste("This square has", dataCurr$countInSquare, "coffee shops.")
  squarePop[dataCurr$countInSquare == 1] <- paste("This square has 1 coffee shop.")
  
  map <- MapCities(city)
  map <- addPolygons(map, data = gridPolyGood, color = "#1C1C1C", weight = 1, 
                     opacity = 0.25, popup =  squarePop)
  map <- addCircles(map, data = points, radius = 75, stroke = FALSE,
                    color = "#6E07E8", fillOpacity = 1, popup = points$name)
  map
}

MakePMFData <- function(city, comparison) {
  # Creates data for plotting the PMF of a city's coffee shop distribution.
  # Also produces data for the related negative binomial distribution when comparison = TRUE. 
  
  dataCurr <- filter(data, cityName == city)
  pmf <- summarise(group_by(dataCurr, countInSquare), 
                   percent = 100*(n()/nrow(dataCurr)))
  pmf <- merge(pmf, data.frame(countInSquare = 0:15), all.y = TRUE) # Creates a y-value for every x-value in domain.
  pmf[is.na(pmf)] <- 0
  pmf["source"] <- "observed"
  
  if (comparison) {
    mean <- mean(dataCurr$countInSquare)
    var <- var(dataCurr$countInSquare)
    shape <- mean*mean/(var - mean)
    
    density <- dnbinom(0:15, size = shape, mu = mean)
    comparisonPMF <- data.frame(countInSquare = 0:15, percent = 100*density)

    comparisonPMF["source"] <- "negative binomial PMF"
    pmf <- rbind(pmf, comparisonPMF)
  }
  
  names(pmf)[1] <- "count"
  pmf
}

PlotDist <- function(city, comparison = FALSE) {
  # Plots the PMF of a city's coffee shop distribution.
  #  Also plots the PMF of the related negative binomial distribution if comparison = TRUE. 
  
  pmf <- MakePMFData(city, comparison)
  
  p <- rPlot(percent ~ count, data = pmf, type = 'line', color = "source")
  if (comparison) {
    p$layer(percent ~ count, data = filter(pmf, source != 'observed'), 
            type = 'point',  color = "source", size = list(const = 2.5)) 
  }
  p$layer(percent ~ count, data = filter(pmf, source == 'observed'), 
          type = 'point',  color = "source", size = list(const = 2.5))
  
  p$guides(color = list(scale = "#! function(value){
                                      color_mapping = {observed: '#6e07e8', 
                                                       'negative binomial PMF': '#999999'}
                                      return color_mapping[value];                  
                                    } !#"),
           x = list(title = "count of coffee shops", min = -1, max = 15), 
           y = list(title = "percent of squares", min = -1, max = 100))
  
  p$set(title = paste0("Coffee shop distribution in ", city, "."), 
        legendPosition = "none")
  
  p
}

# Save for use in computations.
stats <- summarise(group_by(data, cityName, popDens), 
                   mean = mean(countInSquare), 
                   variance = var(countInSquare))

# Complete computations in advance for MakeStatsPlot().
meanRange <- seq(min(stats$mean), max(stats$mean), length.out = 50)
statsPredicted <- data.frame(mean = meanRange)
model <- lm(variance ~ I(mean*mean) + offset(mean) + 0, data = stats)
statsPredicted$variance <- predict(model, newdata = statsPredicted)

MakeStatsPlot <- function(city) { 
  # Plots variance vs. mean for all coffee shop distributions. 
  #   Highlights the point corresponding to the given city.
  
  statsForCity <- data.frame(filter(stats, cityName == city))
  
  tt <- "#! function(item) {return(item.cityName + ': (' + item.mean + ', ' + item.variance + ')')} !#"
  
  p <- rPlot(variance ~ mean, data = stats, type = "point", size = list(const = 4), 
             color = list(const = "#999999"), tooltip = tt)
  p$layer(variance ~ mean, data = statsPredicted, type = "line", 
          color = list(const = "#000000"))
  p$layer(variance ~ mean, data = statsForCity, type = "point", 
          size = list(const = 5), color = list(const = "#6E07E8"), tooltip = tt)
  
  p$guides(x = list(min = -1, max = 27), y = list(min = -50))
  p$set(title = "Coffee shop distributions: variance vs. mean.")
  
  p
}

# Complete computations in advance for PlotByPopDens().
predictedByPopDens <- data.frame(popDens = seq(0, 30000, length.out = 1000))
model <- lm(mean ~ popDens, data = stats)
predictedByPopDens$linear <- predict(model, newdata = predictedByPopDens)
model <- lm(I(log(mean)) ~ popDens, data = stats)
predictedByPopDens$exp <- exp(predict(model, newdata = predictedByPopDens))

PlotByPopDens <- function(city, smoother) {
  # Plots the means of all coffee shop distributions against population density
  #  and includes a fitted curve of the form indicated by smoother.
  #  Highlights the point corresponding to the given city.
  
  forCity <- data.frame(filter(stats, cityName == city))
  
  tt <- "#! function(item) {return(item.cityName + ': (' + item.popDens + ', ' + item.mean + ')')} !#"
  
  p <- rPlot(mean ~ popDens, data = stats, type = "point", 
             opacity = list(const = .5), color = list(const = "#999999"), 
             size = list(const = 4), tooltip = tt)
  if (smoother == "linear"){
    p$layer(linear ~ popDens, data = predictedByPopDens, type = "line", 
            color = list(const = "#000000"))
  } else{
    p$layer(exp ~ popDens, data = predictedByPopDens, type = "line", 
            color = list(const = "#000000"))
  }
  p$layer(mean ~ popDens, data = forCity, type = "point", size = list(const = 5), 
          color = list(const = "#6e07e8"), tooltip = tt)
  
  p$guides(x = list(min = -1, max = 30000, 
                    title = "population density (people per square mile)"), 
           y = list(min = -1, max = 30, title = "mean"))
  
  p$set(title = "Coffee shop distributions: mean vs. population density.")
  
  p
}

# Create a melted dataframe that stores both scaled and original values.
vars <- c("cityName", "hispOrLatPerc", "blackOrAfrAmPerc", "whitePerc", "asianPerc", 
          "professionalPerc", "helpersPerc", "artsAndRecPerc", 
          "notDrivePerc", "collegePerc", "medIncome", "areaMiles")
idVars <- c("cityName", "popDens", "mean")
varsForFaceting <- setdiff(vars, idVars)
censusAndMeans <- merge(stats[idVars], census[vars]) 
melted <- melt(censusAndMeans, id.vars = idVars)
scaled <- cbind(censusAndMeans[idVars], scale(censusAndMeans[varsForFaceting]))
meltedAndScaled <- melt(scaled, id.vars = idVars)
names(meltedAndScaled)[5] <-  "valueScaled"
forPlottingByVars <- merge(melted, meltedAndScaled)

# Create a melted dataframe that stores predictions from various models.
range <- expand.grid(valueScaled = seq(-3.5, 6.5, length.out = 25), 
                     variable = varsForFaceting)
predictedByVars <- data.frame(range)
predictedByVars[c("meanExp", "meanLine", "popDensExp", "popDensLine")] <- NA
for (var in varsForFaceting) {
  forModeling <- filter(forPlottingByVars, variable == var)
  ndata <- filter(predictedByVars, variable == var)
  indices <- predictedByVars$variable == var
  
  model <- lm(I(log(mean)) ~ valueScaled, data = forModeling)
  predictedByVars[indices, ]$meanExp <- exp(predict(model, newdata = ndata))
  
  model <- lm(mean ~ valueScaled, data = forModeling)
  predictedByVars[indices, ]$meanLine <- predict(model, newdata = ndata)
  
  model <- lm(I(log(popDens)) ~ valueScaled, data = forModeling)
  predictedByVars[indices, ]$popDensExp <- exp(predict(model, newdata = ndata))
  
  model <- lm(popDens ~ valueScaled, data = forModeling)
  predictedByVars[indices, ]$popDensLine <- predict(model, newdata = ndata)
}

# Save script for formatting facet titles.
formatting <- "#! function(facetObject) {
                    var titles = {
                      areaMiles: 'vs. areaMiles',
                      hispOrLatPerc: 'vs. hispOrLatPerc',
                      blackOrAfrAmPerc: 'vs. blackOrAfrAmPerc',
                      whitePerc: 'vs. whitePerc',
                      asianPerc: 'vs. asianPerc',
                      professionalPerc: 'vs. professionalPerc', 
                      helpersPerc: 'vs. helpersPerc',
                      artsAndRecPerc: 'vs. artsAndRecPerc',
                      notDrivePerc: 'vs. notDrivePerc',
                      collegePerc: 'vs. collegePerc',
                      medIncome: 'vs. medIncome'
                    }
                    return(titles[facetObject.variable])
                  } !#"

PlotByOtherVars <- function(city, smoother) {
  # Plots the means of all coffee shop distributions against various demographic
  #  variables and includes fitted curves of the form indicated by smoother.
  #  Highlights the point corresponding to the given city.
  
  forCity <- filter(forPlottingByVars, cityName == city)
  
  tt <- "#! function(item) {return(item.cityName + ': (' + item.value + ', ' + item.mean + ')')} !#"
  
  p <- rPlot(mean ~ valueScaled, data = forPlottingByVars, type = "point",
             size = list(const = 2.5), color = list(const = "#999999"), 
             opacity = list(const = .7),  tooltip = tt)
  if (smoother == "linear") {
      p$layer(meanLine ~ valueScaled, data = predictedByVars, type = "line", 
              color = list(const = "#000000"))
  } else{
      p$layer(meanExp ~ valueScaled, data = predictedByVars, type = "line", 
              color = list(const = "#000000"))
  }
  p$layer(mean ~ valueScaled, data = forCity, type = "point", 
          size = list(const = 5), color = list(const = "#6e07e8"), tooltip = tt)
  
  p$guides(x = list(renderLabel = FALSE, renderTick = FALSE, title = "", 
                    min = -3.5, max = 6.5),
           y = list(title = "mean", min = -1, max = 30))
  p$set(title = "Coffee shop distributions: mean vs. various demographic variables.")
  p$facet(var = "variable", type = "wrap", rows = 6, formatter = formatting)
  
  p
}

PlotPopDensByVars <- function(city, smoother) {
  # Plots population density against various demographic variables and includes 
  #  fitted curves of the form indicated by smoother. 
  #  Highlights the point corresponding to the given city.
  
  forCity <- filter(forPlottingByVars, cityName == city)
  
  tt <- "#! function(item) {return(item.cityName + ': (' + item.value + ', ' + item.popDens + ')')} !#"
  
  p <- rPlot(popDens ~ valueScaled, data = forPlottingByVars, type = "point",
             size = list(const = 2.5), color = list(const = "#999999"), 
             opacity = list(const = .7),  tooltip = tt)
  if (smoother == "linear") {
    p$layer(popDensLine ~ valueScaled, data = predictedByVars, type = "line", 
            color = list(const = "#000000"))
  } else {
    p$layer(popDensExp ~ valueScaled, data = predictedByVars, type = "line", 
            color = list(const = "#000000"))
  }
  p$layer(popDens ~ valueScaled, data = forCity, type = "point", 
          size = list(const = 5), color = list(const = "#6e07e8"), tooltip = tt)
  
  p$guides(x = list(renderLabel = FALSE, renderTick = FALSE, title = "", 
                    min = -3.5, max = 6.5),
           y = list(title = "population density (people per square mile)", 
                    min = -1000, max = 30000))
  p$set(title = "Population density vs. various demographic variables.")
  p$facet(var = "variable", type = "wrap", rows = 6, formatter = formatting)
  
  p
}