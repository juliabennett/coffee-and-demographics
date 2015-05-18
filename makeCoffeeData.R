
library(dplyr)
library(maps)
library(sp)
library(rgdal) 
library(rgeos)
library(maptools)
library(acs)
library(jsonlite)

ChooseTopPopCities <- function(num = 50) {
  #Uses data from the 2013 US Census to sort US 'places' by population and returns
  #  information for those among the highest ranked using the threshold provided.   
  #  Assumes that data from the following link has been saved with default options
  #  and default names in the working directory:
  #  http://factfinder.census.gov/bkmk/table/1.0/en/PEP/2013/PEPANNRSIP.US12A
  pops <- read.csv("PEP_2013_PEPANNRSIP.US12A/PEP_2013_PEPANNRSIP.US12A_with_ann.csv", 
                   skip = 1, nrow = num, stringsAsFactors = FALSE)
  names(pops)[ncol(pops)] <- "popIn2013" #give the final column a simple name
  cities <- transmute(pops,
                      placeName = sub(",.*", "", Geography.2),
                      totalFP = substring(Target.Geo.Id, 10), 
                      stateFP = substr(totalFP, 1, 2), 
                      rank = Rank, 
                      pop = popIn2013)
  data(state.fips) #load data needed to get state abbreviations
  forMerging <- transmute(state.fips, 
                          stateFP = sprintf("%02s", fips), 
                          stateAbb = as.character(abb))
  forMerging <- forMerging[!duplicated(forMerging), ]
  cities <- merge(cities, forMerging) #creates a column with state abbreviations
  cities$stateFP <- NULL #this column was only needed for merging
  cities
}

DownloadShapes <- function(url, exdir, layer) {
  #Downloads the specified shapefile from the US Census if it doesn't already exist.
  if (!file.exists(paste0(exdir, "/", layer, ".shp"))) {
    file = "temporary.zip"
    download.file(url, file, method = "curl")
    unzip(file, exdir = exdir)
    file.remove(file)
  }
}

MergePolys <- function(shapes1, shapes2) {
  #Merges two SpatialPolygonsDataFrames that have the same CRS. 
  #  Allows overlapping IDs and allows shapes1 to be NULL. 
  #  Always assigns new IDs before merging. 
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

MakeCityPolys <- function(totalFPs) { 
  #Downloads 2013 US Census shapefiles describing every 'place' whose FIPS code 
  #  is an element of totalFPs. Reads these files to a single SpatialPolygonsDataFrame.
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
  #Compiles a vector containing all US ZIP codes that intersect at least one 
  #  SpatialPolygon in the given SpatialPolygonsDataFrame. 
  #  Uses 2013 ZIP code data from the US Census. 
  url <- "http://www2.census.gov/geo/tiger/TIGER2013/ZCTA5/tl_2013_us_zcta510.zip"
  layer <-  "tl_2013_us_zcta510"
  DownloadShapes(url, "zips", layer)
  zipShapes <- readOGR("zips", layer = layer)
  if (proj4string(shapes) != proj4string(zipShapes)) {
    shapes <- spTransform(shapes, CRS(proj4string(zipShapes)))
  }
  intTest <- gIntersects(shapes, zipShapes, byid = TRUE, returnDense = FALSE)
  indices <- unique(unlist(intTest))
  zipcodes <- as.character(zipShapes$ZCTA5CE10[indices])
  zipcodes
}

MakeRoughGrid <- function(shape, length) {
  #Creates a rectangular grid that covers the given SpatialPolygon by squares with
  #  the specified side length (in miles). Returns this grid as class SpatialPolygons.
  p4s <- CRS("+proj=lcc +lat_1=40.66666666666666 
             +lat_2=41.03333333333333 +lat_0=40.16666666666666 
             +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
             +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") 
  shape <- spTransform(shape, p4s) #transforms to standard proj4string with units in feet
  box <- bbox(shape)   
  size = c(5280, 5280)*length
  grid <- GridTopology(cellcentre.offset = box[, 1] + (size/2), 
                       cellsize = size, 
                       cells.dim = ceiling(diff(t(box))/size)) 
  gridPoly <- as.SpatialPolygons.GridTopology(grid, p4s)
  gridPoly
}

MakeCountyShapes <- function() {
  #Dowloads the 2013 US Census shapefile describing county boundaries and
  #  reads it to a SpatialPolygonsDataFrame
  url <- "http://www2.census.gov/geo/tiger/TIGER2013/COUNTY/tl_2013_us_county.zip"
  layer <- "tl_2013_us_county"
  DownloadShapes(url, 'counties', layer)
  countyShapes <- readOGR("counties", layer = layer)
  countyShapes
}

#save for use during computations
countyShapes <- MakeCountyShapes() 
p4sFromCensus <- proj4string(countyShapes) #default for all US Census shapefiles

GetCountyFPs <- function(shape) {
  #Compiles a vector of all FIPS county codes corresponding to counties that 
  #  intersect the given SpatialPolygon. 
  #  Uses 2013 county data from the US Census website.
  if (proj4string(shape) != proj4string(countyShapes)) {
    shape <- spTransform(shape, CRS(proj4string(countyShapes)))
  }
  intTest <- gIntersects(shape, countyShapes, byid = TRUE, returnDense = FALSE)
  indices <- unlist(intTest)
  countyFPs <- paste0(countyShapes$STATEFP, countyShapes$COUNTYFP)[indices]
  countyFPs
}

MakeWaterShapes <- function(countyFPs) {
  #Downloads 2013 US Census shapefiles describing bodies of water in each county 
  #  whose FIPS code is an element of countyFPs. 
  #  Reads these files to a single SpatialPolygonsDataFrame.
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
  # Given two objects of class SpatialPolygons, returns indices for the 
  #  SpatialPolygons in shapes2 whose center is contained in shapes1 but is not 
  #  contained in a US body of water. Uses 2013 Census data describing bodies of water.
  #  Originally designed for the case when shapes2 is a grid of squares. 
  if (proj4string(shapes1) != p4sFromCensus) {
    shapes1 <- spTransform(shapes1, CRS(p4sFromCensus))
  }
  centers <- data.frame(coordinates(shapes2))
  coordinates(centers) <- c("X1", "X2")
  proj4string(centers) <- CRS(proj4string(shapes2))
  if (proj4string(centers) != p4sFromCensus) {
    centers <- spTransform(centers, CRS(p4sFromCensus))
  }
  countyFPs <- GetCountyFPs(shapes1) #determine what counties intersect shapes1
  water <- MakeWaterShapes(countyFPs) #get water info for each of these counties
  inBoundaries <- unlist(gCovers(shapes1, centers, byid = TRUE, returnDense = FALSE))
  inWater <- unlist(gCovers(water, centers, byid = TRUE, returnDense = FALSE))
  goodIndices <- setdiff(inBoundaries, inWater)
  goodIndices 
}

MakeNiceGrid <- function(shape, length) {
  #Creates a rectangular grid that covers the given SpatialPolygon by squares with
  #  the specified side length (in miles). Returns this grid as class SpatialPolygonsDataFrame.
  #  Indicates if each square meets the following criterion: its center lies inside
  #  of the SpatialPolygon but is not contained in a US body of water.
  #  Uses 2013 Census data describing bodies of water.
  gridPoly <- MakeRoughGrid(shape, length)
  gridPolyData <- data.frame(squareID = names(gridPoly), 
                             goodSquare = FALSE)
  goodIndices <- SelectGoodSquares(shape, gridPoly)
  gridPolyData$goodSquare[goodIndices] <- TRUE
  gridPolyDF <- SpatialPolygonsDataFrame(gridPoly, gridPolyData, match.ID = "squareID")
  gridPolyDF
}

MakeNiceGridsAcrossCities <- function(cityShapes, length) {
  #Takes a SpatialPolygonsDataFrame returned by MakeCityPolys() and calls MakeNiceGrid()
  #  on each SpatialPolygon using the length specified (in miles). Compiles the results 
  #  into a new SpatialPolygonsDataFrame.
  squares <- NULL
  for (i in 1:length(cityShapes)) {
    cityShape <- cityShapes[i, ]
    gridPolyDF <- MakeNiceGrid(cityShape, length)
    gridPolyDF$placeName <- cityShape$NAMELSAD
    squares <- MergePolys(squares, gridPolyDF)
  }
  squares
}

ComputeCounts <- function(squares, points) {
  #Takes an object returned by MakeNiceGridsAcrossCities() (possibly after subsetting)
  #  and a dataframe describing points by latitude and longitude columns. Counts 
  #  the number of points that lie inside each square and returns the results in 
  #  a new dataframe.
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
  #Uses the ACS package to compile selected demographic variables for the US 'places' 
  #  whose FIPs code is contained in totalFPs.
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

#will be used later for storing data to help create nice maps quickly in shiny app
FindPointsInShapes <- function(points, shapes1, shapes2 = NULL) {
  #Assumes that points is a dataframe describing points by latitude and longitude 
  #  columns and that shapes1 and shapes2 are of class SpatialPolygons. Returns 
  #  indices identifying the rows that correspond to points contained in at least 
  #  one SpatialPolygon in either shapes1 or shapes2. 
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
  indices <- union(unlist(intTest1), unlist(intTest2))
  indices
}


#compile city info
cities <- ChooseTopPopCities()
cityShapes <- MakeCityPolys(cities$totalFP)
writeOGR(cityShapes, "shiny/cityShapes", "cityShapes", driver = "ESRI Shapefile") #save for shiny app
for (i in 1:nrow(cities)) {
  cityShape <- cityShapes[cityShapes$NAMELSAD == cities$placeName[i], ]
  cities$cityName[i] <- as.character(cityShape$NAME)
  cityCenter <- coordinates(cityShape)
  cities$long[i] <- cityCenter[1]
  cities$lat[i] <- cityCenter[2]
}

#correction for SF:
index <- cities$cityName == "San Francisco"
cities$lat[index] <- 37.7833
cities$long[index] <- -122.4167
#correction for long names:
cities$cityName[cities$cityName == "Indianapolis city (balance)"] <- "Indianapolis"
cities$cityName[cities$cityName == "Louisville/Jefferson County metro government (balance)"] <- "Louisville"
cities$cityName[cities$cityName == "Nashville-Davidson metropolitan government (balance)"] <- "Nashville"

#write ZIP codes to a file for use in API query
zipcodes <- GetZips(cityShapes)
write.csv(zipcodes, "zips.csv")

#run API query in Python
system("python factualAPI.py")

#clean up file generated by factualAPI.py
fromAPI <- fromJSON("cafes.txt", flatten = TRUE)
cafes <- transmute(fromAPI, latitude, longitude, name)
cafes <- filter(cafes, !is.na(latitude), !is.na(longitude), !is.na(name))
cafes <- cafes[!duplicated(cafes), ]
write.csv(cafes, 'shiny/cafes.csv', row.names = FALSE) #save for shiny app

#make grids and compute coffee shop counts
squares <- MakeNiceGridsAcrossCities(cityShapes, 1.25)
squares <- spTransform(squares, CRS(proj4string(cityShapes))) 
writeOGR(squares, "shiny/squares", "squares", driver = "ESRI Shapefile") #save for shiny app
counts <- ComputeCounts(squares[squares$goodSquare, ], cafes)
counts <- merge(counts, cities)

#compile census data
rawCensusData <- RetrieveCensusData(cities$totalFP)
census <- merge(cities, rawCensusData)
for (i in 1:nrow(census)) {
  census$area[i] <- cityShapes[cityShapes$NAMELSAD == census$placeName[i], ]$ALAND
}
census <- mutate(census, areaMiles = area * 3.86102e-7, popDens = pop/areaMiles)
write.csv(census, 'shiny/census.csv', row.names = FALSE) #save for shiny app

#merge to get final dataset
data <- merge(counts, census)
write.csv(data, 'shiny/data.csv', row.names = FALSE) #save for shiny app

#read ZIP code shapefile that was downloaded during computation and save for shiny app
zipShapes <- readOGR("zips", layer = "tl_2013_us_zcta510")
zipShapes <- zipShapes[zipShapes$ZCTA5CE10 %in% zipcodes, ]
writeOGR(zipShapes, "shiny/zipShapes", "zipShapes", driver = "ESRI Shapefile")

#save intersection results as strings to make shiny app faster
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




