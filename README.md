
#coffee-and-demographics

This is code for a Shiny app. Check it out here:  [juliaben.net/t/coffee/](http://juliaben.net/t/coffee/).

The script makeCoffeeData.R defines some handy functions for working with geospatial data in R. Head over to the shiny app to read about how these can be used. 

No data is included in this repository, but everything can be reproduced by running makeCoffeeData.R in the same directory as the folder shiny/ and the script factualAPI.py. This scripts downloads most of the data it needs from the internet, but it does require [this table](http://factfinder.census.gov/bkmk/table/1.0/en/PEP/2013/PEPANNRSIP.US12A) to be downloaded and saved in the working directory. Also, appropriate API keys must be placed into the code.

