# Load packages 

# nodig voor xlsx package
options(java.parameters = "-Xmx36000m") 

# function to check whether package is installed
packages <- function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,dependencies = T)
    require(x,character.only=TRUE)
  } }
# Load packages 

packages(stringr)
packages(plyr)
packages(dplyr)
packages(sp) # zit ook in raster
packages(ggthemes)
packages(reshape2)
packages(mapproj)
packages(knitr)# pdf maken van tabellen
packages(devtools)
packages(ggseas)
packages(scales)
packages(broom)
packages(shiny)
packages(leaflet)
packages(plotly)
suppressMessages(library(plotly))
packages(rgdal)
packages(shinythemes)
packages(DT)
packages(ggplot2)
suppressMessages(library(ggplot2))
suppressMessages(library(RColorBrewer))
packages(data.table)
packages(ggmap)
packages(gtools)
packages(kableExtra)
packages(knitr)
