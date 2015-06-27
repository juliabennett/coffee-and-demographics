
library(dplyr)
library(maps)
library(sp)
library(rgdal) 
library(rgeos)
library(maptools)
library(acs)
library(jsonlite)

DownloadShapes <- function(url, exdir, layer) {
  # Checks if a shapefile exists and downloads it from the US Census if needed.
  if (!file.exists(paste0(exdir, "/", layer, ".shp"))) {
    file = "temporary.zip"
    download.file(url, file, method = "curl")
    unzip(file, exdir = exdir)
    file.remove(file)
  }
}

MergePolys <- function(shapes1, shapes2) {
  # Merges two SpatialPolygonsDataFrames that have the same CRS. Allows shapes1 to be NULL.
  
  lowerID <- length(shapes1) + 1
  upperID <- length(shapes1) + length(shapes2)
  shapes2 <- spChFIDs(shapes2, as.character(lowerID:upperID))
  
  if (is.null(shapes1)) {
    shapes <- shapes2
  } else {
    shapes1 <- spChFIDs(shapes1, as.character(1:length(shapes1)))
    shapes <- spRbind(shapes1, shapes2)
  }
  
  shapes
}

ChooseTopPopCities <- function(num = 50) {
  # Uses data from the 2013 US Census to find US 'places' with the largest population. 
  # Assumes that data from the following link has been saved with default options
  #  and default names in the working directory:
  #  http://factfinder.census.gov/bkmk/table/1.0/en/PEP/2013/PEPANNRSIP.US12A
  
  pops <- read.csv("PEP_2013_PEPANNRSIP.US12A/PEP_2013_PEPANNRSIP.US12A_with_ann.csv", 
                   skip = 1, nrow = num, stringsAsFactors = FALSE)
  names(pops)[ncol(pops)] <- "popIn2013" # Give the final column a simple name.
  cities <- transmute(pops,
                      placeName = sub(",.*", "", Geography.2),
                      totalFP = substring(Target.Geo.Id, 10), 
                      stateFP = substr(totalFP, 1, 2), 
                      rank = Rank, 
                      pop = popIn2013)
  
  data(state.fips) # Load data needed to get state abbreviations.
  forMerging <- transmute(state.fips, 
                          stateFP = sprintf("%02s", fips), 
                          stateAbb = as.character(abb))
  forMerging <- forMerging[!duplicated(forMerging), ]
  cities <- merge(cities, forMerging) # Creates a column with state abbreviations.
  cities$stateFP <- NULL # This column was only needed for merging.
  
  cities
}

MakeCityPolys <- function(totalFPs) { 
  # Downloads, reads, and combines data from the 2013 US Census describing US 'places'.
  
  stateFPs <- unique(substring(totalFPs, 1, 2))
  
  cityShapes <- NULL
  for (fp in stateFPs) {
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2013/PLACE/tl_2013_", 
                  fp, "_place.zip")
    layer <- paste0("tl_2013_", fp, "_place")
    DownloadShapes(url, "cities", layer)
    cityShapesNew <- readOGR("cities/", layer = layer) 
    cityShapes <- MergePolys(cityShapes, cityShapesNew)
  }
  
  indices <- paste0(cityShapes$STATEFP, cityShapes$PLACEFP) %in% totalFPs
  cityShapes[indices, ]
}

GetZips <- function(shapes) {
  # Finds all US ZIP codes that intersect a SpatialPolygonsDataFrame.
  #  Uses 2013 ZIP code data from the US Census website. 
  
  url <- "http://www2.census.gov/geo/tiger/TIGER2013/ZCTA5/tl_2013_us_zcta510.zip"
  layer <-  "tl_2013_us_zcta510"
  DownloadShapes(url, "zips", layer)
  zipShapes <- readOGR("zips", layer = layer)
  
  if (proj4string(shapes) != proj4string(zipShapes)) {
    shapes <- spTransform(shapes, CRS(proj4string(zipShapes)))
  }
  
  intTest <- gIntersects(shapes, zipShapes, byid = TRUE, returnDense = FALSE)
  indices <- unique(unlist(intTest))
  
  as.character(zipShapes$ZCTA5CE10[indices])
}

MakeRoughGrid <- function(shape, sideLength) {
  # Creates a rectangular grid covering a SpatialPolygon by squares.
  
  p4s <- CRS("+proj=lcc +lat_1=40.66666666666666 
             +lat_2=41.03333333333333 +lat_0=40.16666666666666 
             +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
             +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") 
  shape <- spTransform(shape, p4s) # Transforms to proj4string with units in feet.
  
  box <- bbox(shape)   
  size = c(5280, 5280)*sideLength
  grid <- GridTopology(cellcentre.offset = box[, 1] + (size/2), 
                       cellsize = size, 
                       cells.dim = ceiling(diff(t(box))/size)) 
  
  as.SpatialPolygons.GridTopology(grid, p4s)
}

MakeCountyShapes <- function() {
  # Downloads and reads 2013 US Census data describing county boundaries.
  url <- "http://www2.census.gov/geo/tiger/TIGER2013/COUNTY/tl_2013_us_county.zip"
  layer <- "tl_2013_us_county"
  DownloadShapes(url, 'counties', layer)
  readOGR("counties", layer = layer)
}

# Save county boundaries for use during computations.
countyShapes <- MakeCountyShapes() 
p4sFromCensus <- proj4string(countyShapes) 

GetCountyFPs <- function(shape) {
  # Finds FIPS codes for all US counties that intersect the SpatialPolygonsDataFrame.
  #  Uses 2013 county data from the US Census website.
  if (proj4string(shape) != proj4string(countyShapes)) {
    shape <- spTransform(shape, CRS(proj4string(countyShapes)))
  }
  intTest <- gIntersects(shape, countyShapes, byid = TRUE, returnDense = FALSE)
  indices <- unlist(intTest)
  paste0(countyShapes$STATEFP, countyShapes$COUNTYFP)[indices]
}

MakeWaterShapes <- function(countyFPs) {
  # Downloads and reads 2013 US Census data describing all bodies of water in specified counties.
  water <- NULL
  for (fp in countyFPs){
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2013/AREAWATER/tl_2013_", 
                  fp, "_areawater.zip")
    layer <-  paste0("tl_2013_", fp, "_areawater")
    DownloadShapes(url, "water", layer)
    waterNew <- readOGR("water", layer = layer)
    water <- MergePolys(water, waterNew) 
  }
  water
}

SelectGoodSquares <- function(shapes1, shapes2) {
  # Determines which SpatialPolygons in shapes2 have a center that is contained 
  #   in shapes1 but is not contained in a US body of water. 
  # Uses 2013 water data from the US Census website.
  
  if (proj4string(shapes1) != p4sFromCensus) {
    shapes1 <- spTransform(shapes1, CRS(p4sFromCensus))
  }
  
  centers <- data.frame(coordinates(shapes2))
  coordinates(centers) <- c("X1", "X2")
  proj4string(centers) <- CRS(proj4string(shapes2))
  if (proj4string(centers) != p4sFromCensus) {
    centers <- spTransform(centers, CRS(p4sFromCensus))
  }
  
  countyFPs <- GetCountyFPs(shapes1) # Determine which counties intersect shapes1.
  water <- MakeWaterShapes(countyFPs) # Get water info for each of these counties.
  
  inBoundaries <- unlist(gCovers(shapes1, centers, byid = TRUE, returnDense = FALSE))
  inWater <- unlist(gCovers(water, centers, byid = TRUE, returnDense = FALSE))
  goodIndices <- setdiff(inBoundaries, inWater)
  
  goodIndices 
}

MakeNiceGrid <- function(shape, sideLength) {
  # Creates a rectangular grid covering the SpatialPolygon by squares. 
  #  Tests each square to determine if its center is contained in the 
  #  SpatialPolygon but not contained in a US body of water. 
  # Uses 2013 Census data describing bodies of water.
  gridPoly <- MakeRoughGrid(shape, sideLength)
  gridPolyData <- data.frame(squareID = names(gridPoly), 
                             goodSquare = FALSE)
  goodIndices <- SelectGoodSquares(shape, gridPoly)
  gridPolyData$goodSquare[goodIndices] <- TRUE
  SpatialPolygonsDataFrame(gridPoly, gridPolyData, match.ID = "squareID")
}

MakeNiceGridsAcrossCities <- function(cityShapes, sideLength) {
  # Compiles the results of calling MakeNiceGrid() on each entry in 
  #   a SpatialPolygonDataFrame describing US 'places'.
  squares <- NULL
  for (i in 1:length(cityShapes)) {
    cityShape <- cityShapes[i, ]
    gridPolyDF <- MakeNiceGrid(cityShape, sideLength)
    gridPolyDF$placeName <- cityShape$NAMELSAD
    squares <- MergePolys(squares, gridPolyDF)
  }
  squares
}

ComputeCounts <- function(squares, points) {
  # Counts the number of points that are contained inside each square.
  
  points <- filter(points, !is.na(latitude), !is.na(longitude))
  points <- points[!duplicated(points), ]
  coordinates(points) <- c("longitude", "latitude")
  proj4string(points) <- CRS("+proj=longlat + datum=WGS84")
  if (proj4string(points) != proj4string(squares)) {
    points <- spTransform(points, CRS(proj4string(squares)))
  }
  
  counts <- data.frame(placeName = squares$placeName, 
                       squareID = squares$squareID, 
                       countInSquare = NA)
  intTest <- gCovers(squares, points, byid = TRUE)
  for (i in 1:length(squares)) {
    counts$countInSquare[i] <- sum(intTest[, i])
  }
  
  counts    
}

RetrieveCensusData <- function(totalFPs) {
  # Uses the ACS package to compile selected demographic variables for US 'places'.
  
  api.key.install(key = "<api key here>") #enter US Census API key
  census <- data.frame(totalFP = totalFPs)
  
  for (i in 1:length(totalFPs)) {
    geo <- geo.make(state = as.numeric(substr(totalFPs[i], 1, 2)), 
                    place = as.numeric(substring(totalFPs[i], 3)))
    
    fetched <- acs.fetch(endyear = 2013,
                         geography = geo, 
                         table.num = "B03003",
                         span = 3)
    census$hispOrLatPerc[i] <- 100*(estimate(fetched)[1,3]/estimate(fetched)[1,1])
    
    fetched <- acs.fetch(endyear= 2013, 
                         geography = geo, 
                         table.num = "B02001", 
                         span = 3)
    census$blackOrAfrAmPerc[i] <- 100*(estimate(fetched)[3]/estimate(fetched)[1])
    census$whitePerc[i] <- 100*(estimate(fetched)[2]/estimate(fetched)[1])
    census$asianPerc[i] <- 100*(estimate(fetched)[5]/estimate(fetched)[1])
    
    fetched <- acs.fetch(endyear= 2013, 
                         geography = geo, 
                         table.num = "B08126", 
                         span = 3)
    census$professionalPerc[i] <- 100*(estimate(fetched)[10]/estimate(fetched)[1])
    census$helpersPerc[i] <- 100*(estimate(fetched)[11]/estimate(fetched)[1])
    census$artsAndRecPerc[i] <- 100*(estimate(fetched)[12]/estimate(fetched)[1])
    census$notDrivePerc[i] <- 100*((estimate(fetched)[46] + estimate(fetched)[61] + 
                                estimate(fetched)[76])/estimate(fetched)[1])
    
    fetched <- acs.fetch(endyear = 2013,
                         geography = geo, 
                         table.num = "B14001", 
                         span = 3)
    census$collegePerc[i] = 100*((estimate(fetched)[8] + 
                                estimate(fetched)[9])/estimate(fetched)[1])
    
    fetched <- acs.fetch(endyear = 2013, 
                         geography = geo, 
                         table.num = "B19113", 
                         span = 3)
    census$medIncome[i] <- estimate(fetched)[1]
  }
  
  census
}

FindPointsInShapes <- function(points, shapes1, shapes2 = NULL) {
  # Determines which entries in points are contained in either shapes1 or shapes2. 
  
  pointsSpatial <- points
  coordinates(pointsSpatial) <- c("longitude", "latitude")
  proj4string(pointsSpatial) <- CRS("+proj=longlat + datum=WGS84")
  if (proj4string(pointsSpatial) != proj4string(shapes1)) {
    pointsSpatial <- spTransform(pointsSpatial, CRS(proj4string(shapes1)))
  }
  
  intTest1 <- gCovers(shapes1, pointsSpatial, byid = TRUE, returnDense = FALSE)
  intTest2 <- NULL
  if (!is.null(shapes2)) {
    if (proj4string(shapes2) != proj4string(shapes1)){
      shapes2 <- spTransform(shapes2, CRS(proj4string(shapes1)))
    }
    intTest2 <- gCovers(shapes2, pointsSpatial, byid = TRUE, returnDense = FALSE)
  }
  
  union(unlist(intTest1), unlist(intTest2))
}


# Compile city info.
cities <- ChooseTopPopCities()
cityShapes <- MakeCityPolys(cities$totalFP)
writeOGR(cityShapes, 
         "shiny/cityShapes", 
         "cityShapes", 
         driver = "ESRI Shapefile") # Save for shiny app.
for (i in 1:nrow(cities)) {
  cityShape <- cityShapes[cityShapes$NAMELSAD == cities$placeName[i], ]
  cities$cityName[i] <- as.character(cityShape$NAME)
  cityCenter <- coordinates(cityShape)
  cities$long[i] <- cityCenter[1]
  cities$lat[i] <- cityCenter[2]
}

# Correction for SF:
index <- cities$cityName == "San Francisco"
cities$lat[index] <- 37.7833
cities$long[index] <- -122.4167

# Correction for long names:
cities$cityName[cities$cityName == "Indianapolis city (balance)"] <- "Indianapolis"
cities$cityName[cities$cityName == "Louisville/Jefferson County metro government (balance)"] <- "Louisville"
cities$cityName[cities$cityName == "Nashville-Davidson metropolitan government (balance)"] <- "Nashville"

# Write ZIP codes to a file for use in API query.
zipcodes <- GetZips(cityShapes)
write.csv(zipcodes, "zips.csv")

# Run API query in Python.
system("python factualAPI.py")

# Clean up file generated by factualAPI.py.
fromAPI <- fromJSON("cafes.txt", flatten = TRUE)
cafes <- transmute(fromAPI, latitude, longitude, name)
cafes <- filter(cafes, !is.na(latitude), !is.na(longitude), !is.na(name))
cafes <- cafes[!duplicated(cafes), ]
write.csv(cafes, 'shiny/cafes.csv', row.names = FALSE) # Save for shiny app.

# Make grids and compute coffee shop counts.
squares <- MakeNiceGridsAcrossCities(cityShapes, 1.25)
squares <- spTransform(squares, CRS(proj4string(cityShapes))) 
writeOGR(squares, "shiny/squares", "squares", driver = "ESRI Shapefile") # Save for shiny app.
counts <- ComputeCounts(squares[squares$goodSquare, ], cafes)
counts <- merge(counts, cities)

# Compile census data.
rawCensusData <- RetrieveCensusData(cities$totalFP)
census <- merge(cities, rawCensusData)
for (i in 1:nrow(census)) {
  census$area[i] <- cityShapes[cityShapes$NAMELSAD == census$placeName[i], ]$ALAND
}
census <- mutate(census, areaMiles = area * 3.86102e-7, popDens = pop/areaMiles)
write.csv(census, 'shiny/census.csv', row.names = FALSE) # Save for shiny app.

# Merge to get final dataset.
data <- merge(counts, census)
write.csv(data, 'shiny/data.csv', row.names = FALSE) # Save for shiny app.

# Save ZIP code shapefiles for shiny app.
zipShapes <- readOGR("zips", layer = "tl_2013_us_zcta510")
zipShapes <- zipShapes[zipShapes$ZCTA5CE10 %in% zipcodes, ]
writeOGR(zipShapes, "shiny/zipShapes", "zipShapes", driver = "ESRI Shapefile")

# Save intersection results as strings to make shiny app faster.
intTests <- cities[c("cityName", "placeName")]
zipIntTest <- gIntersects(cityShapes, zipShapes, byid = TRUE, returnDense = FALSE)
names(zipIntTest) <- cityShapes$NAMELSAD
for (i in 1:50) {
  place <- intTests$placeName[i]
  zips <- zipIntTest[[place]]
  intTests$zips[i] <- paste(zips, collapse = ";")
  
  coffeeInZips <- FindPointsInShapes(cafes, zipShapes[zips, ])
  intTests$coffeeInZips[i] <- paste(coffeeInZips, collapse = ";")
  
  cityShape <- cityShapes[cityShapes$NAMELSAD == place, ]
  gridPolyGood <- squares[squares$placeName == place & squares$goodSquare, ]
  coffeeInSquaresOrCity <- FindPointsInShapes(cafes, gridPolyGood, cityShape)
  intTests$coffeeInSquaresOrCity[i] <- paste(coffeeInSquaresOrCity, collapse = ";")
}
write.csv(intTests, "shiny/intTests.csv")




