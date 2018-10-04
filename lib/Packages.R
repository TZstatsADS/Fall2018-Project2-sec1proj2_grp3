packages.used=c("shiny", "shinydashboard", "leaflet", 
                "leaflet.extras", "shinythemes", "shinyWidgets",
                "DT", "htmltools", "data.table", 
                "devtools", "sp", "dplyr",
                "tigris", "tm", "maptools",'broom',"httr","rgdal","RColorBrewer","XML","tidyr","ggplot2","zipcode","geosphere","ggmap","tidyr")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

# load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(htmltools)
library(data.table)
library(devtools)
library(MASS)
library(dplyr)
library(tigris)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)
library(XML)
library(DT)
library(tidyr)
library(ggplot2)
library(zipcode)
set.seed(1)
