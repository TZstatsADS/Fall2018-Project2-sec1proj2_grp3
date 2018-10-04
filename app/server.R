library(shiny)
library(leaflet)
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
library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)


load("../output/markets.RData")
load("../output/sub.station.RData")
load("../output/bus.stop.RData")
load("../output/nyc.RData")
load("../output/rank.Rdata")
load("../output/rent.Rdata")
load("../output/region_rent.Rdata")
#load("../output/rank_all.Rdata")
load("../output/restaurant.Rdata")



source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")
load("../output/housing.RData")


color <- list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
              color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
              color3 = c("#F7FCF5","#74C476", "#005A32"))
bin <- list(bin1 = c(0,500,1000,1500,2000,2500,3000), bin2 = c(0,1,2,3,4,5,6,7))
pal <- colorBin(color[[1]], bins = bin[[1]])


shinyServer(function(input, output,session) {
  
  #Esri.WorldTopoMap
  #########main map######
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      addProviderTiles("Stamen.TonerLite")%>%
      addMarkers(data=housing,
               lng=~lng,
               lat=~lat,
               clusterOptions=markerClusterOptions(),
               group="housing_cluster"
    )
  })
  
  
  
  #############Housing#############
  
  
  # filter housing data:
  
  housingFilter=reactive({
    bedroom_filter=housing$bedrooms>=input$min_bedrooms 
    bathroom_filter=housing$bathrooms>=input$min_bathrooms
    #price_filter=housing$price>=input$min_price & housing$price<=input$max_price
    price_filter2 =  housing$price>= input$price[1] & housing$price <= input$price[2]
    filter=bedroom_filter & bathroom_filter & price_filter2
    
    return(housing[filter,])
  })
  
  # show data in the map:
  observe({leafletProxy("map")%>%clearGroup("housing_cluster")%>%
      addMarkers(data=housingFilter(),
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })
  # show current status of icons:
  
  showStatus=reactive({
    if (is.null(input$map_bounds)){
      return("cloud")
      
    }
    else{
      if(input$map_zoom<16){
        return('cloud')
      }
      else{
        return('details')
      }
    }
  })
  # hide and show clouds 
  observe({
    if(showStatus()=="cloud"){
      
      leafletProxy("map") %>%showGroup("housing_cluster")%>%clearGroup("new_added")
    }
    else{
      leafletProxy("map") %>%hideGroup("housing_cluster")
      
    }
  })
  
  # show housing details when zoom to one specific level
  
  observe({
    if(showStatus()=="details"){
      if(nrow(marksInBounds())!=0){
        leafletProxy("map")%>%clearGroup(group="new_added")%>% 
          addCircleMarkers(data=marksInBounds(),
                           lat=~lat,
                           lng=~lng,
                           label=~as.character(price),
                           radius=5,
                           stroke=FALSE,
                           fillColor = "green",
                           fillOpacity=0.7,
                           group="new_added",
                           labelOptions = labelOptions(
                             noHide = T,
                             offset=c(20,-15),
                             opacity=0.7,
                             direction="left",
                             style=list(
                               background="green",
                               color="white"  
                             )
                           )
          )
      }
      else{
        leafletProxy("map")%>%clearGroup(group="new_added")
      }
      
      
      
      
    }
    
    
    
    
  })
  
  # get the housing data in the bounds
  marksInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(housing[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    return(
      subset(housingFilter(),
             lat>= latRng[1] & lat <= latRng[2] &
               lng >= lngRng[1] & lng <= lngRng[2])
    )
  })
  
  # sort housing in current zoom level
  
  observe({
    
    housing_sort=marksInBounds()
    
    if(nrow(housing_sort)!=0){
      
      action=apply(housing_sort,1,function(r){
        addr=r["addr"]
        lat=r["lat"]
        lng=r["lng"]
        paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",addr,'</a>')   
      }
      )
      
      housing_sort$addr=action
      output$rank <- renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")][order(housing_sort$price,decreasing = TRUE),],escape=FALSE)
      
      
      
      
    }
    else{
      
      output$rank=renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")])
    }
    
  })
  
  output$x4 = renderPrint({
    s = input$rank_rows_selected
    if (length(s)) {
      cat('You selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  # When point in map is hovered, show a popup with housing info
  observe({
    
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    if(showStatus()=="details"){
      isolate({
        showPopupHover(event$lat, event$lng,housing=housingFilter())
      })  
    }
    
  })
  
  # mouseout the point and cancel popup
  observe({
    
    event <- input$map_marker_mouseout
    if (is.null(event))
      return()
    
    isolate({
      leafletProxy("map") %>% clearPopups()
    })
  })
  
  # click name to go to that point
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      
      
      
      lat <- as.numeric(input$goto$lat)
      lng <- as.numeric(input$goto$lng)
      
      map %>% setView(lng = lng, lat = lat, zoom = 16)
    })
  })
  # hover the list to show info
  observe({
    if (is.null(input$showPop))
      return()
    isolate({
      remove=as.numeric(input$showPop$remove)
      map <- leafletProxy("map")
      
      if(remove==0){
        
        
        
        lat <- as.numeric(input$showPop$lat)
        lng <- as.numeric(input$showPop$lng)
        showPopupHover(lat, lng,housingFilter())   
      }
      else{
        map %>% clearPopups()
      }
      
      
    })
  })
  

  #############Search###############
  observeEvent(input$button1,{
    url = paste0('http://maps.google.com/maps/api/geocode/xml?address=',input$location,'&sensor=false')
    doc = xmlTreeParse(url) 
    root = xmlRoot(doc) 
    lati = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lat']])) 
    long = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lng']]))
    
    leafletProxy("map") %>%
      setView(lng=long, lat=lati,zoom=15)%>%
      addMarkers(lng=long,lat=lati,layerId = "1",icon=icons(
        iconUrl = "../output/icons8-Location-50.png",iconWidth = 25, iconHeight = 25))
  })
  #################Clear Choices############
  observeEvent(input$button2,{
    proxy<-leafletProxy("map")
    proxy %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      removeMarker(layerId="1") %>%
      addMarkers(data=housing,
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster")
    updateTextInput(session, inputId="location", value = "")
  }
  
  )
  
  #############Clear button###########
  observeEvent(input$clear, {
    leafletProxy('map')%>% setView(lng = -73.971035, lat = 40.775659, zoom = 12)
    
  })
  
  
  ############Subway##############
  observeEvent(input$Subway,{
    p<-input$Subway
    proxy<-leafletProxy("map")
    
    if(p==TRUE){
      proxy %>% 
        addMarkers(data=sub.station, ~lng, ~lat,label = ~info,icon=icons(
          iconUrl = "../output/icons8-Bus-48.png",
          iconWidth = 7, iconHeight = 7),group="subway")
    }
    else proxy%>%clearGroup(group="subway")
    
  })
  
  ###############bus###############
  observeEvent(input$Bus,{
    p<-input$Bus
    proxy<-leafletProxy("map")
    
    if(p==TRUE){
      proxy %>% 
        addMarkers(data=bus.stop, ~lng, ~lat,label = ~info,icon=icons(
          iconUrl = "../output/icons8-Bus-48.png",
          iconWidth = 7, iconHeight = 7),layerId=as.character(bus.stop$info))
    }
    else proxy%>%removeMarker(layerId=as.character(bus.stop$info))
    
  })
  
  
  ##############Market#####################
  observeEvent(input$Market,{
    p<- input$Market
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy%>%
        addMarkers(lat=markets$latitude, lng=markets$longitude,icon=icons(
          iconUrl = "../output/icons8-Shopping Cart-48.png",
          iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),layerId=as.character(markets$License.Number))
    }
    else{
      proxy %>%
        removeMarker(layerId=as.character(markets$License.Number))
    }
  })
  
  ##############Resturant#####################
  observeEvent(input$Restaurant,{
    p<- input$Restaurant
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy%>%
        addMarkers(lat=restaurant$lat, lng=restaurant$lon,icon=icons(
          iconUrl = "../output/icons8-French Fries-96.png",
          iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),layerId=as.character(restaurant$CAMIS))
    }
    else{
      proxy %>%
        removeMarker(layerId=as.character(restaurant$CAMIS))
    }
  })
  

  ##############Crime#####################
  observeEvent(input$Crime,{
    p<- input$Crime
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy %>%
        addPolygons(data=nyc, fillColor = ~pal(count), color = 'grey', weight = 1,
                    fillOpacity = .6)%>%
        addLegend(pal = pal, values = nyc$count,position="bottomleft")
    }
    else proxy%>%clearShapes()%>%clearControls()
    
  })
 
  
  ##########################################################################
  ## Panel 3: compare ######################################################
  ########################################################################## 
  
#  observeEvent(input$click_jump_next,{
#    if(input$click_jump_next){
#     updateTabsetPanel(session, "inTabset",selected = "Compare")
#    }
#  })
  
  
})#shiney server