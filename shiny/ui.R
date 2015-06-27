
library(shiny)
library(rCharts)
library(leaflet)
library(dplyr)

census <- read.csv("census.csv", stringsAsFactors = FALSE)
census <- arrange(census, cityName)

cityNamesWithStates <- paste(census$cityName, census$stateAbb, sep = ", ")
cityList <- list()
for (i in 1:50){
  cityList[cityNamesWithStates[i]] <- census$cityName[i] 
}

censusVars <- c("cityName", "stateAbb", "hispOrLatPerc", "blackOrAfrAmPerc", "whitePerc", 
                "asianPerc", "professionalPerc", "helpersPerc", "artsAndRecPerc", 
                "notDrivePerc", "collegePerc", "medIncome")
dataVars <- c("countInSquare", censusVars, "areaMiles", "popDens")


# Make UI. 
shinyUI(navbarPage(h4("Coffee & Demographics"), windowTitle = "Coffee & Demographics", 
                   
  footer = div(class = "footer", 
             div(class = "container", 
               tags$ul(class="list list-inline text-center text-muted",
                 tags$li(class = "inset-h text-heavy", "Julia Bennett"),
                 tags$li(class = "inset-h", 
                   a(class = "link", "juliacbennett@gmail.com", 
                     href = "mailto:juliacbennett@gmail.com") 
                 ),
                 tags$li(class = "inset-h", 
                   a(class = "link", "github.com/juliabennett", 
                     href = "https://github.com/juliabennett")
                 )
               )
             )
           ),
  
  tabPanel("Fusion",
    tags$head(
        includeCSS("styles.css"),
        tags$script(src = "http://logitank.net/eugo.js"),
        tags$link(rel="icon", type="image/x-icon", href="favicon.ico")
    ),
    div(class = "container relative",
      leafletOutput("mapIntro", height = 450),
      absolutePanel(id = "controls",
        h2("Finding coffee..."),
        selectInput("cityIntro", 
                    label = h4("Choose a city:"),
                    choices = cityList, 
                    selected = "Austin"
        ),
        br(),
        showOutput("distIntro", "polycharts")
      ),
      helpText(align = "right", "(If an interactive map doesn't eventually appear 
              above, please refresh!)")
    ),
    div(class = "container",  
      h2("Fusion"),
      p("When searching for wisdom from data, creating an interesting and 
        appropriate dataset is an important foundational step. With this 
        in mind, I'm going to choose a specific example and carefully illustrate 
        one approach to  fusing two of my favorite types of data:"),
      tags$ol(
        tags$li(
          tags$strong("Location, location, location!"), 
          "There are a spectacular number of interesting data sources that 
          provide location information for occurrences of a particular event, 
          type of business, existence of an environmental factor, 
          and so much more. Frequently, these locations are given in the form of 
          latitude and longitude coordinates."
        ),
        tags$li(
          tags$strong("Demographics."),
          "The US Census is bursting at the seams with ",
          a("widely varied", 
            href = "http://factfinder.census.gov/bkmk/table/1.0/en/ACS/13_3YR/DP03"),
          " and ", 
          a("eerily specific", 
            href = "http://factfinder.census.gov/bkmk/table/1.0/en/ACS/13_5YR/B13002C"), 
          "demographic characteristics, each of which is available at many 
          different levels of geography. There is typically a shapefile (we'll 
          talk about these later!) defining the regions on which these values 
          are estimated. "
        )
      ),
      p("I", 
        tags$em("really"), 
        "like coffee shops, so my example will create a dataset that addresses 
        how the spread and density of coffee shops in major cities is related to 
        their demographic characteristics. Check out the map above for a preview."
      ),
      p("My goal is to thoroughly describe a method for compiling data of this form
        that's effective, easy to interpret, and very hands-on. Along the way, I'll shamelessly take 
        advantage of this opportunity to create fun, interactive maps."
      )
    ),
    div(class = "container",
      h2("A tiny bit of fine print:"),
      p("The data used in this project was last updated on May 13, 2015. In particular, 
        the displayed information about coffee shops should be treated as a snapshot 
        of a moment in time and should not be considered a current representation of 
        existing coffee shops. All coffee shop information 
        came from the",
        a("factual.com", href = "http://www.factual.com"), 
        " API. All demographic characteristics and regional boundaries were compiled 
        using data provided by the US Census (partly through an API). I'll make every
        attempt to credit and explain these various data sources along the way. "
      )
    )
  ),
  
  tabPanel("Getting started",
    div(class = "container", 
      h2("What are we working towards?"), 
      p("I focused on the 50 cities with the highest population in the United States. 
        My objective was to associate two things to each of these cities: 1) demographic 
        characteristics,  and 2) a probability distribution providing information 
        about the spread and density of coffee shops."
      ),
      p("I'll describe the main tools that I used, but I won't include any 
        actual code here. If you're eager for more details, all of the code is", 
        a(" available on my GitHub.", href = "https://github.com/juliabennett/coffee-and-demographics"),  
        "With the exception of one tiny API query written in Python, this 
        project was entirely run in R."
      )
    ),
    div(class = "container", 
      h2("Step 1: Making sense of the US Census"), 
      p("The great thing about the US Census is the magnitude of data available, 
        but getting started is a bit daunting. To help reduce the learning 
        curve, I'll take this opportunity to carefully cover the basics." 
      ),
      p("I'm going to focus on American Community Survey (ACS) estimates, as 
        these are by far the most useful type of census dataset available. 
        It turns out that there are three elementary but fundamental vocabulary 
        words:"
      ), 
      tags$ol(
        tags$li(
          tags$strong("Variable"), 
          "refers (as expected) to the value that is being estimated."
        ),
        tags$li(
          tags$strong("Geography"), 
          "refers to the type of region for which this value is being estimated, 
          e.g. census block, census tract, state, or place."
        ),
        tags$li(
          tags$strong("Span"), 
          "refers to the span (either 1, 3, or 5 years) of the survey that is 
          estimating this value."
        )
      ),
      p("Each variable is only available for certain geographies and is only 
        estimated for certain spans. Fortunately, there is a magical website that 
        brings all of this  information together:",
        a("FactFinder.census.gov/", href="http://www.factfinder.census.gov/"),
        ". From there, you can explore what variables are available for what 
        geographies from what spans, 
        all from a convenient, user-friendly menu that's organized by category. 
        Also, that website provides a ", 
        a("handy guide", 
          href="http://www.census.gov/acs/www/guidance_for_data_users/estimates/"),
        " for deciding which span is appropriate."), 
      p("While the FactFinder website is 
        invaluable for learning about what data is available, downloading is best 
        done with the ", 
        code("acs"), 
        "package for R. There is a  ", 
        a("great pdf", 
          href = "http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf"),
        " describing exactly how to use this package. "
      )
    ),
    div(class = "container",
      h2("Back to our example"),
      p("Rather than work with cities, the US Census uses 'places'. For the 
        purposes of this project, each city will 
        just be defined by its corresponding 'place' geography."
      ),
      p("I used the 2013 Population Estimates available from the FactFinder 
        website to choose the 50 cities with the highest population. Following 
        the process described above, I then selected a few variables that seemed 
        relevant to my coffee problem and computed values for each from the 2013 
        3-year ACS estimates. Here's the dataset I ended up with:"
      ),
      br(),
      h4(align = "center", "Selected Demographic Characteristics by City"),
      sidebarLayout(
        sidebarPanel(
          helpText("Protip: choose < 5 variables to keep the layout looking nice."),
          checkboxGroupInput('censusVars', 
                             label = 'Choose some variables:', 
                             choices = censusVars,
                             selected = c("cityName", "asianPerc", 
                                          "collegePerc", "medIncome")
          )
        ),
        mainPanel(
          dataTableOutput("censusData")
        )
      ),
      helpText("Attributes: This product uses the Census Bureau Data API but is 
               not endorsed or certified by the Census Bureau.")
    ),
    div(class = "container",
      h2("Variable definitions"),
      p("For the sake of completeness, here are definitions for the variables 
        shown in the dataset above:"
      ),
      tags$ul(
        tags$li("cityName: name of the city corresponding to the 'place' 
                geography used by the US Census to compute variable estimates"),
        tags$li("stateAbb: abbreviation for the state containing this 'place'"),
        tags$li("hispOrLatPerc: percent of the population that has Hispanic or Latino 
                origins"),
        tags$li("blackOrAfrAmPerc: percent of the population whose race is Black or 
                African American alone"),
        tags$li("whitePerc: percent of the population whose race is White alone"),
        tags$li("asianPerc: percent of the population whose race is Asian alone"),
        tags$li("professionalPerc: percent of the population whose job is in either  
                professional, scientific, management, administrative, or waste 
                management services."),
        tags$li("helpersPerc: percent of the population whose job is in either
                educational services, health care assistance, or social assistance"),
        tags$li("artsAndRecPerc:  percent of the population whose job is in 
                either arts, entertainment, recreation, accommodation services, 
                or food services"),
        tags$li("notDrivePerc: percent of the population that does not work from 
                home, commute to work by driving, or commute to work by carpooling"),
        tags$li("collegePerc: percent of the population enrolled in college, 
                graduate school, or professional school"),
        tags$li("medIncome: median family income in the past 12 months (in 2013 
                inflation-adjusted dollars)")
      ),
      p("The specific table numbers used to compute the values displayed above 
        can be easily found in my code."
      )
    )
  ),  
  
  tabPanel("Shapefiles", 
    div(class = "container",
      h2("Assembling the spatial puzzle pieces"),
      p("Next, I'm going to walk through the steps that I took to build the 
        maps shown immediately below. After playing with some of the features, keep 
        reading to learn more about the process these maps are describing and how they 
        were made. "
      ),
      br(),
      div(class = "relative", 
          leafletOutput("mapZips", height = 350),
          absolutePanel(class = "smallControls", 
                        h3("City Boundaries, ZIP Code Boundaries, & Coffee Shops"),
                        selectInput("cityForZips", 
                                    label = h4("Choose a city:"),
                                    choices = cityList, 
                                    selected = "Austin"
                        )
          )
      ),
      helpText(align = "right", "(If an interactive map doesn't eventually appear 
               above, please refresh!)")
    ),
    div(class = "container",
      h2("Shapefiles in R"), 
      p("First question:", 
        tags$em("what are shapefiles?"),
        "A shapefile actually refers to a collection of files that describes 
        geospatial data in a way that mapping software can understand."
      ),
      p("The key to working with shapefiles in R is paying attention to coordinate 
        reference systems (CRS's). Usually stored in a .prj file in the shapefile 
        directory, a CRS tells R how to translate geospatial coordinates to physical 
        information (e.g. how to turn latitude and longitude pairs into points 
        on the globe). So if you're ever trying to map a spatial object, make sure 
        you've specified the correct CRS. If you're ever trying to compare two 
        spatial objects, make sure they have the same CRS. If you're ever trying 
        to read/store spatial data, make sure you're reading/storing with the 
        correct CRS. I could go on, but I think you get the idea. Once you 
        understand this though, working with shapefiles in R is pretty straightforward."
      ),
      p("It turns out that R has very powerful, easy-to-use packages for 
        working with shapefiles. I'll introduce the ones I found most useful as 
        we go, illustrating a few of their key features along the way."
      )
    ),
    div(class = "container",
      h2("TIGER (meow?)"),
      p("Before doing any further analysis, I needed to determine what boundaries 
        the US Census used to estimate the values I just downloaded. Enter the 
        most bad-ass tiger around: Topologically Integrated Geographic Encoding 
        and Referencing (TIGER). This is the format that the US Census uses to 
        store their geospatial data. You can download TIGER shapefiles from the 
        US Census for a wide array of different geographic features from an", 
        a("online database,", href = "https://www.census.gov/geo/maps-data/data/tiger-line.html"), 
        "and it's easy to write scripts that access it automatically."
      ),
      p("How do you work with these shapefiles in R? The ",  code("sp"), "package is 
        the base package for handling geospatial data in R. It provides
        classes for storing shapefile data in convenient spatial objects. Among 
        other things, these classes keep track of the associated CRS. It's 
        certainly worth mentioning  the function", 
        code("readOGR()"), " from the", code('rgdal'), 
        "package. This function reads a shapefile to one of these objects and 
        automatically assign the correct CRS from the .prj file saved in the 
        shapefile directory."
      ),
      p("I downloaded the 2013 US Census TIGER shapefiles describing 'place' 
        boundaries and used these packages to store this information in a nice 
        spatial object. Recall that I defined each city I was interested in by 
        exactly these boundaries. The", 
        code("leaflet"), 
        "package easily and beautifully plots spatial objects, allowing us to 
        take a look at the results:"
      ),
      br(),
      div(class = "relative",
        leafletOutput("mapCities", height = 350),
        absolutePanel(class = "smallControls", 
          h3("City Boundaries"),
          selectInput("cityForCities", 
                      label = h4("Choose a city:"),
                      choices = cityList, 
                      selected = "Austin"
          )
        )
      ), 
      helpText(align = "right", "(If an interactive map doesn't eventually appear 
              above, please refresh!)")
    ),
    div(class = "container",
      h2("Where are the coffee shops?"),
      p("The API available from", 
        a("factual.com", href = "http://factual.com"), 
        " offers access to data about coffee shop locations (although the 
        definition of coffee shop is certainly vague). In order to query this API 
        by ZIP code, I required a list of all ZIP codes that intersect one 
        of my cities."
      ),
      p("After experimenting with several seemingly unreliable ZIP code databases, 
        I downloaded the 2013 US Census TIGER shapefile describing ZIP code 
        boundaries. As before, I loaded this into R. So I then had
        spatial objects describing both city and ZIP code boundaries. The ", 
        code("rgeos"), 
        "package has a family of functions for determining if two spatial objects 
        overlap in various ways. Applying the version that tests for intersections, 
        it was easy (but a little bit slow) to 
        learn which ZIP codes intersect one 
        of the cities I was interested in. Of course, I was careful to check that 
        everything had the same CRS!"),
      p("Armed with this information, I 
        queried (in Python) this API to obtain latitude and longitude 
        coordinates for every coffee shop in one of these ZIP codes. This process 
        seems widely applicable to working with APIs that provide 
        location information by ZIP code (or anytime you're interested in 
        determining what ZIP codes intersect a given region!), and it exposes one 
        of the many reasons that I'm excited about shapefiles."
      ),
      p("The map shown at the top of this page uses the", 
        code("leaflet"),
        "package again to illustrate this procedure."
      )
    )
  ),
  
  tabPanel("Gridifying", 
    div(class = "container",
      h2("Why make a grid?"),
      p("At this point, I had demographic characteristics for each city that I 
        was investigating and spatial objects describing their boundaries. I also 
        had latitude and longitude coordinates for every coffee shop that could 
        possibly lie within these city boundaries.  As the final step in the 
        creation of this dataset, I broke each city into a grid of small squares and 
        counted the number of coffee shops in each square."
      ),
      p("The resulting counts defined a probability distribution describing how 
        coffee shops are spread out across each city. More specifically, if I'm 
        randomly dropped into one of these cities, the corresponding distribution 
        describes the likelihood of landing in a square with any particular number 
        of coffee shops."
      )
    ),
    div(class = "container",
      h2("The final product"),
      p("Before going into detail about how these counts were computed and turned 
        into a nice dataset, here's a peek at the final product and the maps it 
        came from:"
      ),
      br(),
      div(class = "relative",
        leafletOutput("mapGrids", height = 350),
        absolutePanel(class = "smallControls", width = 300, 
          h3("Grids & Coffee Shops"),
          selectInput("cityForGrids", 
                      label = h4("Choose a city:"),
                      choices = cityList, 
                      selected = "Austin"
          )
        )
      ),
      helpText(align = "right", "(If an interactive map doesn't eventually appear 
              above, please refresh!)"),
      br(),
      h4(align = "center", "Coffee Shop Counts by Square with Selected City Demographics"),
      sidebarLayout(
        sidebarPanel(
          helpText("Protip: choose < 5 variables to keep the layout looking nice."),
          checkboxGroupInput('dataVars1', 
                             label = 'Choose some variables:',
                             choices = dataVars,
                             selected = c("countInSquare", "cityName", 
                                          "helpersPerc", "notDrivePerc")
          )
        ),
        mainPanel(
          dataTableOutput("data1")
        )
      ),
      helpText("Attributes: This product uses the Census Bureau Data API but is not 
               endorsed or certified by the Census Bureau. This product also
               uses data from the following source: Places data © Factual Inc. (",
               a("http://www.factual.com", href = "http://www.factual.com"),
               ")."
      )
    ),
    div(class = "container",
      h2("How do you make grids in R?"),
      p("To obtain these counts, my first task was simply to cover each city 
        with a large, rectangular grid (initially allowing many squares to lie 
        outside city boundaries). Again, the ", 
        code("sp"), 
        " package came to the rescue. It provides nice functions and classes for 
        working with grids in R. However, the key to completing this task was 
        transforming the way each city was stored so that it had a CRS that 
        accepted units in feet before trying to make any grids. Switching 
        between CRS's required the ", 
        code("rgdal"), 
        "package again."
      )
    ),
    div(class = "container",
      h2("Excluding bad squares"),
      p("Next, it was necessary to exclude squares from these grids so that the 
        remaining squares were mostly contained inside city boundaries and didn't 
        include large bodies of water, all without losing too much land from each 
        city. Unfortunately, city and water boundaries certainly don't look like 
        nice, convex polygons. Also, many city boundaries include miles of ocean 
        or lake."
      ),
      p("This is where I had to summon a little bit of cleverness. Before I could 
        go any further, I clearly needed access to spatial objects 
        storing water boundaries. It turns out that the US Census offers TIGER shapefiles 
        describing all bodies of water in the US, but they can only be downloaded 
        by county. So I downloaded the 2013 US Census TIGER shapefile describing 
        all county boundaries and then completed the following steps for each city:"),
      tags$ol(
        tags$li("I started with the rectangular grid over this city that was created 
                earlier."),
        tags$li("Using the newly-downloaded shapefile describing county boundaries, 
                I followed the same procedure I described for ZIP codes to determine 
                what counties intersect this city."),
        tags$li("I then downloaded 2013 US Census shapefiles describing water boundaries 
                for each of these counties."),
        tags$li("Finally, I removed each square from the original grid whose center 
                was either outside of city boundaries or was contained in one of 
                these bodies of water. Calling again on that helpful family of 
                functions from the ", 
                code("rgeos"), 
                "package for comparing spatial objects, it was easy
                to determine when a square met this description and should be discarded. 
                Note that this last step required three spatial objects: one describing 
                this city, one describing squares in the original grid, and one 
                describing the relevant bodies of water. I obviously made sure 
                that all three had the same CRS before doing any comparisons. ")
      ),
      p("Through trial and error, I chose the size of my squares to be as large as 
        possible while only incorrectly adding or losing a small amount of land 
        across all 50 cities. I ended up with squares whose sides have length equal 
        to 1.25 miles. As I'll discuss later, this choice certainly effects what 
        conclusions can be made using the subsequent dataset."
      ),
      p("I now had grids approximating the shape of each city. These were stored as 
        spatial objects describing the remaining grid squares. Check out the map 
        at the top of this page to see what the initial 
        grids looked like and compare these to the squares that made the final cut. "
      )
    ),
    div(class = "container", 
      h2("Putting it all together"),
      p("The only remaining chore was counting the number of coffee shops in each 
        square. After saving the latitude and longitude information for each coffee 
        shop to an appropriate spatial object, this was easily completed using that 
        great class of functions from the ", 
        code("rgeos"), 
        "package once more to test for containment. This also required the ", 
        code('rgdal'), 
        "package yet again to ensure that everything in sight had the same CRS."
      ),
      p("I then created a dataset with an observation for each square, recording 
        both the number of coffee shops it contains and the demographic information 
        I had previously downloaded for the city it lives in."
      ),
      p("As you might notice in the dataset shown above, I also added two new 
        demographic variables: areaMiles and popDens. The variable areaMiles 
        describes the land area of each city (in square miles). This information 
        was provided in the downloaded shapefiles and automatically stored as an 
        attribute in the spatial objects I created in R, so I simply extracted 
        these values and added them to my dataset. The variable popDens 
        (measured in people per square mile) was then obtained by dividing the 
        population estimates I started with by the values stored in areaMiles.
        I didn't include a variable for population estimates because this would
        simply be a product of these two new variables."
      )
    )
  ),
  
  tabPanel("Finished dataset",
    div(class = "container",
      h2("The results of fusion"),
      p("Now that I've finished my example of the fusion process, I'll", 
        tags$em("very"),  
        "briefly discuss how the resulting dataset might be 
        put to use. For ease of referencing, I'll start by including another copy 
        of the completed product:"
      ),
      br(),
      h4(align = "center", "Coffee Shop Counts by Square with Selected City Demographics"),
      sidebarLayout(
        sidebarPanel(
          helpText("Protip: choose < 5 variables to keep the layout looking nice."),
          checkboxGroupInput('dataVars2', 
                             label = 'Choose some variables:',
                             choices = dataVars, 
                             selected = c("countInSquare", "cityName", 
                                          "helpersPerc", "notDrivePerc")
          )
        ),
        mainPanel(
          dataTableOutput("data2")
        )
      ),
      helpText("Attributes: This product uses the Census Bureau Data API but is not 
               endorsed or certified by the Census Bureau. This product also
               uses data from the following source: Places data © Factual Inc. (",
               a("http://www.factual.com", href = "http://www.factual.com"),
               ")."
      )
    ),
    div(class = "container",
      h2("Exploratory plots: part 1"),
      p("Consider the result of randomly choosing a square from one of the grids
        I've defined and counting the number of coffee shops in that square. 
        For each city, the probability distribution corresponding to this process 
        is captured by our new dataset. Let's take a closer look at these observed 
        distributions:"
      ),
      br(),
      fluidRow(
        column(2, offset = 1,
          selectInput("cityExPlots1", 
                      label = h4("City:"),
                      choices = cityList, 
                      selected = "Austin"
          ),
          br(),
          h4("Mean:"),
          textOutput("mean"),
          br(),
          h4("Variance:"),
          textOutput("variance")
        ),
        column(3, 
          showOutput("distribution", "polycharts")
        ),
        column(3, offset = 1,
          showOutput("mean2var", "polycharts")
        )
      )
    ),
    div(class = "container", 
      p("The plots shown on the left illustrate the similarities between the 
        shape of these observed distributions and the shape of negative binomial 
        distributions realizing the same means and variances. The plot on the 
        right demonstrates that the relationship between the observed means and 
        variances can roughly be described by a quadratic curve of the form:"
      ),
      p(align = "center", "variance = mean + constant * mean^2."),
      p("These two pieces of information suggest that these observed distributions 
        act very much like a family of negative binomial distributions with a fixed 
        shape parameter."
      ),
      br(),
      h2("Exploratory plots: part 2"),
      p("Next, let's investigate how the means of these distributions behave with 
        respect to the demographic characteristics included in this dataset:"
      ),
      fluidRow(
        column(3, offset = 1,
          br(), 
          br(), 
          br(),
          selectInput("cityExPlots2", 
                      label = h4("City:"),
                      choices = cityList, 
                      selected = "Austin"
          ),
          selectInput("smoother", 
                      label = h4("Fitted curve?"),
                      choices = c("exponential", "linear"),
                      selected = "exponential"
          )
        ),
        column(5, 
          showOutput("byPopDens", "polycharts")
        )
      ), 
      br(),
      fluidRow(
        column(4, 
          showOutput("byOtherVars", "polycharts")
        ),
        column(4, offset = 2, 
          showOutput("popDensByVars", "polycharts")
        )
      )
    ),
    div(class = "container",
      p("As expected, there is a clear correlation between these means and 
        population density. More notably, there are also subtle relationships 
        between these means and some other demographic variables. Furthermore, many 
        of these other variables display only a minimal association 
        with population density. So it might be interesting to study models that 
        describe the observed  distributions using some of these other demographic 
        variables after accounting for population density. For example, the 
        following variables are good candidates: collegePerc, professionalPerc, 
        maybe whitePerc, and maybe medIncome. (If I were taking this analysis any 
        further, I would also investigate how these candidates are related to 
        eachother and more carefully consider their associations with population 
        density.) "
      )
    ),
    div(class = "container",
      h2("Conclusions"),
      p("Based on our short exploratory analysis, this data is well-suited to 
        a simple GLM . Using only demographic characteristics, a GLM would attempt 
        to model both the mean and shape of the distribution associated to each city."
      ),
      p("The most direct approach might be trying to fit a model that relies 
        on population density alone, but then ask if this model can be significantly 
        improved by including some of the other demographic variables 
        that were identified as good candidates earlier. 
        If successful, this would nicely quantify relationships between 
        these variables and the spread and density of coffee shops across major cities."
      ),
      p("As mentioned earlier, the results of this analysis will likely depend on the 
        size of the squares that were used to define the relevant grids. While 
        similarly sized squares will produce similar results, dramatically altering 
        the square size changes what information is being captured. This should be 
        viewed as an advantage, as it would be interesting to perform analysis 
        for different square sizes and compare the results. For example, this might 
        identify some variable that is consistently an important component of each 
        resulting model. Of course, it would be necessary to vary the family of 
        distributions with the square size."
      ),
      p("It's worth noting that this dataset makes no attempt to characterize 
        clustering information about coffee shops, e.g. the number of distinct 
        areas that have a high density of coffee shops or the size of such areas. 
        Rather, the models that I'm proposing are after the average behavior across 
        these cities."
      )
    ),
    div(class = "container",
      h2("The End."),
      p("I'll stop here and save more in-depth analysis 
        for another setting. Here's the moral that I'm aiming for: the process 
        I described can be applied to extract clear and powerful datasets by 
        fusing together two different sources of information. 
        Of course, there are many alternative approaches and the 
        possibilities are endless! I also hope that I've convinced you that R is 
        a fun and convenient setting to work with geospatial 
        data and the US Census, and maybe even provided you with some guidance 
        for getting started. "
      )
    )
   )
))