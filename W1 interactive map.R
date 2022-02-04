## Exercise 1
#Describe a problem or question in your field that lends itself to spatial analysis.


#In general my field is concerned with how people interact with technologies - for example we, at one point looked into Google Maps and how it may be manipulated, by one user carrying several devices through the highway, feeding maps with data saying there was a lot of traffic at the specific point. However, these data were manipulated before put into the map and therefore somewhat spatial analysis for people to use for navigating when driving.  

## Exercise 2
#List data layers that you think are necessary to answer your question/solve your problem. Describe examples of two or three of your listed layers and see if you can find them on the internet.

#World street map 

## Exercise 3: Option 1
#Make a Map: Your colleague has found some ruins during a hike in the Blue Mountains and recorded the coordinates of structures on her phone (RCFeature2018.csv). She would like to map her points but has no computer or mapping skills. Can you make a map that she can work with using only a browser? She needs an interactive map that she can download to her computer and use straightaway.

#Firstly I install leaflet and htmlwidget

install.packages("leaflet")
install.packages("htmlwidget")


#Then loading in the libraries

library(leaflet)
library(htmlwidgets)
library(tidyverse)

#Then I want to load the CSV into my working space 

RecCor <- read.csv("RCFeature2018.csv")




### Exercise 3.1.
#Create a standalone .html map in Leaflet showing at least basic topography and relief, and load in the table of points. Make sure she can see the FeatureID, FeatureType and Description attributes when she hovers over the point markers. 
#In the following code I have also added a minimap, measure and clustered the markers. 
#I'm not completely sure I think the clusters makes very much sense here, as they are very closely related to the area of the Blue Mountains. However it does illustrate the pattern of where more of the ruins are located. 


l_BM <- leaflet() %>%   # assign the base location to an object
  setView(150.2900,-33.75800, zoom = 13)


esri <- grep("^Esri", providers, value = TRUE)

for (provider in esri) {
  l_BM <- l_BM %>% addProviderTiles(provider, group = provider)
}

BMmap <- l_BM %>%
  addLayersControl(baseGroups = names(esri),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
             position = "bottomright") %>%
  addMarkers(lng = RecCor$Longitude, 
             lat = RecCor$Latitude,
             clusterOptions = markerClusterOptions(),
             popup = paste("ID:", RecCor$FeatureID,"<br>",
                           "Type:",RecCor$FeatureType, "<br>",
                           "Desciption:",RecCor$Description)) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% 
  htmlwidgets::onRender("
                        function(el, x) {
                        var myMap = this;
                        myMap.on('baselayerchange',
                        function (e) {
                        myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                        }") %>% 
  addControl("", position = "topright")

BMmap

saveWidget(BMmap, "BMmap.html", selfcontained = TRUE)



### Exercise 3.3.
#Explore differentiating the markers (e.g. by size using Accuracy field)
#This doesn't work fully - it seems to only registrer the ones that I haven't made specifics for. 

#start by finding the unique values from the RecCor df:
unique(RecCor$Accuracy)

#saves the values:
Accuracy <- c(3.0,0.9,0.8,0.7,1.0,16.0,1.1,6.0,1.2,1.4,0.6)

#Writes my function to color the markers differentiated by the Accuracy values:

getColor <- function(Accuracy){
  case_when(
    Accuracy == 3.0 ~ "green",
    Accuracy == 0.9 ~"blue",
    Accuracy == 0.8 ~ "yellow",
    Accuracy == 0.7 ~ "brown",
    Accuracy == 1.0 ~ "black",
    Accuracy == 16.0 ~ "white",
    Accuracy == 1.1 ~ "orange",
    Accuracy == 6.0 ~ "red",
    Accuracy == 1.2 ~ "purple",
    Accuracy == 1.4 ~ "pink",
    Accuracy == 0.6 ~ "grey"
  )
}
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(RecCor$Accuracy)
)

ruinsMap <- leaflet(RecCor) %>% addTiles() %>%
  addAwesomeMarkers(lng = RecCor$Longitude, 
                    lat = RecCor$Latitude,
                    icon=icons)


ruinsMap

