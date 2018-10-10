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
library("base64enc")
library(XML)
library(DT)
library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library("formattable")


#####
library(ggmap)
load("../output/price.RData")
load("../output/avg_price_zip.RData")
load("../output/subdat.RData")
load("../output/nycmarket.RData")
load("../output/nycrent.RData")
load("../output/nycparking.RData")
rank_all <- read.csv("../data/rank_all.csv",as.is = T)
########

load("../output/markets.RData")
load("../output/sub.station.RData")
load("../output/bus.stop.RData")
load("../output/nyc.RData")
load("../output/rank.RData")
load("../output/rent.RData")
load("../output/region_rent.RData")
load("../output/restaurant.RData")
#load("../output/restaurant_details.RData")
#housing = read.csv("../data/housing.original.csv")
market = read.csv("../data/market_dxy.csv",as.is = T)
#rank_zip = read.csv("../data/rank_zip.csv",as.is = T)
housing_final = read.csv("../data/housing_final.csv")
museum = read.csv("../data/museums.csv",as.is = T)
theater = read.csv("../data/theater.csv",as.is = T)


source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")
load("../output/housing.RData")


color <- list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
              color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
              color3 = c("#F7FCF5","#74C476", "#005A32"))
bin <- list(bin1 = c(0,500,1000,1500,2000,2500,3000), bin2 = c(0,1,2,3,4,5,6,7))
bin2 <- list(bin1 = c(0,50,100,150,200,250,300), bin2 = c(0,1,2,3,4,5,6,7))
bin3 <- list(bin1 = c(0,1000,2000,3000,4000,5000,6000), bin2 = c(0,1,2,3,4,5,6,7))
bin4 <- list(bin1 = c(0,20,40,60,80,100,120), bin2 = c(0,1,2,3,4,5,6,7))
pal <- colorBin(color[[1]], bins = bin[[1]])
pal2 <- colorBin(color[[1]], bins = bin2[[1]])
pal3 <- colorBin(color[[1]], bins = bin3[[1]])
pal4 <- colorBin(color[[1]], bins = bin4[[1]])


shinyServer(function(input, output,session) {
  #################################################################
  ##### Panel 1 : summary  ########################################
  #################################################################
  posi <- NULL
  output$map1 <- renderLeaflet({
    leaflet()%>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 13)%>%
      addProviderTiles("Stamen.TonerLite")
    
  })
  
  ## Panel *: heat map###########################################
  
  
  ## Panel *: click on any area, popup text about this zipcode area's information#########
  #posi<-reactive({input$map1_shape_click})
  observeEvent(input$Preference,{
    p<- input$Preference
    
    proxy<-leafletProxy("map1")
    if (p=="Crime"){
      proxy%>%clearShapes()%>%clearControls()
      proxy %>%
        addPolygons(data=nyc, fillColor = ~pal(count), color = 'grey', weight = 1,
                    fillOpacity = .6)%>%
        addLegend(pal = pal, values = nyc$count,position="topright")
    }
    #else proxy%>%clearShapes()%>%clearControls()
    else if(p=="Ave. rent"){
      proxy%>%clearShapes()%>%clearControls()
      proxy %>%
        addPolygons(data=nycrent, fillColor = ~pal(count/2), color = 'grey', weight = 1,
                    fillOpacity = .6)%>%
        addLegend(pal = pal3, values = (nycrent$count/2),position="topright")
    }
    else if(p=="Market"){
      proxy%>%clearShapes()%>%clearControls()
      proxy %>%
        addPolygons(data=nycmarket, fillColor = ~pal(count*10), color = 'grey', weight = 1,  
                    fillOpacity = .6)%>%
        addLegend(pal = pal2, values = (nycmarket$count)*10,position="topright")
    }
    else if(p=="Garage"){
      proxy%>%clearShapes()%>%clearControls()
      proxy %>%
        addPolygons(data=nycparking, fillColor = ~pal(count*25), color = 'grey', weight = 1,  
                    fillOpacity = .6)%>%
        addLegend(pal = pal4, values = (nycparking$count*25),position="topright")
    }
    
    
  })
  
  observeEvent(input$map1_shape_click, {
    ## track
    if(input$click_multi == FALSE) leafletProxy('map1') %>%clearGroup("click")
    click <- input$map1_shape_click
    posi <<- reactive({input$map1_shape_click})
    #posi <<- reactiveV({input$map1_shape_click})
    leafletProxy('map1')%>%
      addMarkers(click$lng, click$lat, group="click", icon=list(iconUrl='icon/leaves.png',iconSize=c(60,60)))
    
    ##info
    
    #####*********#####
    #zip_sel<-as.character(revgeocode(as.numeric(c(click$lng,click$lat)),output="more")$postal_code)
    #zip<-paste("ZIPCODE: ",zip_sel)
    #price_avg<-paste("Average Price: $",avg_price_zip.df[avg_price_zip.df$region==zip_sel,"value"],sep="")
    #studio_avg<-paste("Studio: $",price[price$region==zip_sel&price$type=="Studio","avg"],sep="")
    #OneB_avg<-paste("1B: $",price[price$region==zip_sel&price$type=="OneBedroom","avg"],sep="")
    #TwoB_avg<-paste("2B: $",price[price$region==zip_sel&price$type=="TwoBedroom","avg"],sep="")
    #ThreeB_avg<-paste("3B: $",price[price$region==zip_sel&price$type=="ThreeBedroom","avg"],sep="")
    #FourB_avg<-paste("4B: $",price[price$region==zip_sel&price$type=="fOURbEDROOM","avg"],sep="")
    #transportation_rank<-paste("Transportation Rank: ",rank_all[rank_all$zipcode==zip_sel,"ranking.trans"],sep="")
    #amenities_rank<-paste("Amenities Rank: ",rank_all[rank_all$zipcode==zip_sel,"ranking.amenities"],sep="")
    #crime_rank<-paste("Crime Rank: ",rank_all[rank_all$zipcode==zip_sel,"ranking.crime"],sep="")
    
    
    leafletProxy("map1")%>%
      setView(click$lng,click$lat,zoom=14,options=list(animate=TRUE))
    
    leafletProxy("map")%>%
      setView(click$lng,click$lat,zoom=15,options=list(animate=TRUE))
    
    #output$zip_text<-renderText({zip})
    #output$avgprice_text<-renderText({price_avg})
    #output$avgstudio_text<-renderText({studio_avg})
    #output$avg1b_text<-renderText(({OneB_avg}))
    #output$avg2b_text<-renderText(({TwoB_avg}))
    #output$avg3b_text<-renderText(({ThreeB_avg}))
    #output$avg4b_text<-renderText(({FourB_avg}))
    #output$transportation_text<-renderText({transportation_rank})
    #output$amenities_text<-renderText({amenities_rank})
    #output$crime_text<-renderText({crime_rank})
    
  })
  
  ## Panel *: Return to big view##################################
  observeEvent(input$click_reset_buttom,{
    if(input$click_reset_buttom){
      leafletProxy("map1")%>%
        setView(lng = -73.98928, lat = 40.75042, zoom = 13)%>% 
        clearPopups()
      posi <<- NULL
      leafletProxy("map")%>%
        setView(-73.971035,40.775659,zoom=13,options=list(animate=TRUE))
      #####debug line#####
      #debug_posi <- paste(posi())
      #output$debug <- renderText({debug_posi})
    }
  })
  
  ## Panel 1: to panel 2
  observeEvent(input$click_jump_next,{
    if(input$click_jump_next){
      updateTabsetPanel(session, "inTabset",selected = "Housing Explorer")
    }
  })
  
  
  #Esri.WorldTopoMap
  #########main map######
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
  #     addProviderTiles("Stamen.TonerLite")%>%
  #     addMarkers(data=housing,
  #              lng=~lng,
  #              lat=~lat,
  #              clusterOptions=markerClusterOptions(),
  #              group="housing_cluster"
  #   )
  # })
  
  output$map <- renderLeaflet({
    if(is.null(posi)){
      leaflet() %>%
        setView(lng = -73.971035,
                lat = 40.775659 , zoom = 13) %>%
        #setView(lng = posi2lng, lat = posi2lat, zoom = 12) %>%
        addProviderTiles("Stamen.TonerLite")%>%
        addMarkers(data=housing,
                   lng=~lng,
                   lat=~lat,
                   clusterOptions=markerClusterOptions(),
                   group="housing_cluster")}
    else{leaflet() %>%
        
        setView(lng = posi()$lng, lat = posi()$lat, zoom = 15) %>%
        addProviderTiles("Stamen.TonerLite")%>%
        addMarkers(data=housing,
                   lng=~lng,
                   lat=~lat,
                   clusterOptions=markerClusterOptions(),
                   group="housing_cluster")}
    
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
      output$rank <- renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")][order(housing_sort$price,decreasing = TRUE),],escape=FALSE,
                                     callback = JS(
                                       'table.on("click.dt", "tr", function() {
                                        tabs = $(".tabbable .nav.nav-tabs li a");
                                        $(tabs[1]).click();})'))
      
      
      
      
    }
    else{
      
      output$rank=renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")],
                                  callback = JS(
                                    'table.on("click.dt", "tr", function() {
                                        tabs = $(".tabbable .nav.nav-tabs li a");
                                        $(tabs[1]).click();})'))
    }
    
  })
  
  # output$x4 = renderPrint({
  #   s = input$rank_rows_selected
  #   if (length(s)) {
  #     cat('You selected:\n\n')
  #     cat(s$addr, sep = ', ')
  #   }
  # })
  
  
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
  
  observeEvent(input$button2, {
    updateSelectInput(session, "market_type", selected =  "")
  })
  
  observeEvent(input$market_type, {
    if("Pharmacy" %in% input$market_type) leafletProxy("map") %>% showGroup("pha")
    else{leafletProxy("map") %>% hideGroup("pha")}
    if("Grocery" %in% input$market_type) leafletProxy("map") %>% showGroup("gro")
    else{leafletProxy("map") %>% hideGroup("gro")}
  }, ignoreNULL = FALSE)
  

  
  observeEvent(input$market_type,{leafletProxy("map", data = market[which(market$type == 'Pharmacy'),]) %>%
    addCircleMarkers(~lon,~lat,popup = ~paste("Name:",name,"<br/>",
                                              "Zipcode:",zip,"<br/>" ,# warning appears
                                              "Address:",address), color = "#5C821A", stroke = FALSE, fillOpacity = 1,radius = 5,  group = "pha")
  leafletProxy("map", data = market[which(market$type == 'Grocery'),]) %>%
    addCircleMarkers(~lon,~lat,popup =~paste("Name:",name,"<br/>",
                                             "Zipcode:",zip,"<br/>" ,# warning appears
                                             "Address:",address), color = "#C6D166", stroke = FALSE, fillOpacity = 1,radius = 5,  group = "gro")
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
  
  
  
  ##clear all
  observeEvent(input$button2, {
    updateSelectInput(session, "restaurant_type", selected =  "")
  })
                                            
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'Chinese'),]) %>%
    addCircleMarkers(~lon,~lat,color = "#FFEC5C", popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                                                 "Tel:",restaurant$PHONE,"<br/>",
                                                                 "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                                                 "Address:",restaurant$geoAddress), stroke = FALSE, fillOpacity = 0.5,radius = 5, group  = "chin")
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'American'),]) %>%
    addCircleMarkers(~lon,~lat,color = "#C8000A", popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                                                 "Tel:",restaurant$PHONE,"<br/>",
                                                                 "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                                                 "Address:",restaurant$geoAddress),stroke = FALSE, fillOpacity = 0.5,radius = 5, group = "amer")
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'Italian'),]) %>%
    addCircleMarkers(~lon,~lat,color = "#F9BA32", popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                                                 "Tel:",restaurant$PHONE,"<br/>",
                                                                 "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                                                 "Address:",restaurant$geoAddress),stroke = FALSE, fillOpacity = 0.5,radius = 5,  group = "ita")
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'Japanese'),]) %>%
    addCircleMarkers(~lon,~lat,color = "#D35C37", popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                                                 "Tel:",restaurant$PHONE,"<br/>",
                                                                 "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                                                 "Address:",restaurant$geoAddress),stroke = FALSE, fillOpacity = 0.5,radius = 5,  group = "jap")
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'Pizza'),]) %>%
    addCircleMarkers(~lon,~lat,color = "#E8A735", popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                                                 "Tel:",restaurant$PHONE,"<br/>",
                                                                 "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                                                 "Address:",restaurant$geoAddress),stroke = FALSE, fillOpacity = 0.5,radius = 5,  group = "piz")
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'Others'),]) %>%
    addCircleMarkers(~lon,~lat,popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                              "Tel:",restaurant$PHONE,"<br/>",
                                              "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                              "Address:",restaurant$geoAddress), color = "#E2C499", stroke = FALSE, fillOpacity = 0.5,radius = 5,  group = "oth")
  leafletProxy("map", data = restaurant[which(restaurant$CUISINE.DESCRIPTION == 'Cafe'),]) %>%
    addCircleMarkers(~lon,~lat,popup = ~paste("Name:",restaurant$DBA,"<br/>",
                                              "Tel:",restaurant$PHONE,"<br/>",
                                              "Zipcode:",restaurant$ZIPCODE,"<br/>" ,# warning appears
                                              "Address:",restaurant$geoAddress), color = "#E2C499", stroke = FALSE, fillOpacity = 0.5,radius = 5,  group = "caf")
  

  ## food
  observeEvent(input$restaurant_type, {
    if("Chinese" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("chin")
    else{leafletProxy("map") %>% hideGroup("chin")}
    if("American" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("amer")
    else{leafletProxy("map") %>% hideGroup("amer")}
    if("Italian" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("ita")
    else{leafletProxy("map") %>% hideGroup("ita")}
    if("Japanese" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("jap")
    else{leafletProxy("map") %>% hideGroup("jap")}
    if("Pizza" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("piz")
    else{leafletProxy("map") %>% hideGroup("piz")}
    if("Cafe" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("caf")
    else{leafletProxy("map") %>% hideGroup("caf")}
    if("Others" %in% input$restaurant_type) leafletProxy("map") %>% showGroup("oth")
    else{leafletProxy("map") %>% hideGroup("oth")}
  }, ignoreNULL = FALSE)
  
  
  

  

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
  
  
  #########Theater&Museum######################

  # observeEvent(input$Bus,{
  #   p<-input$Bus
  #   proxy<-leafletProxy("map")
  #
  #   if(p==TRUE){
  #     proxy %>%
  #       addMarkers(data=bus.stop, ~lng, ~lat,label = ~info,icon=icons(
  #         iconUrl = "../output/icons8-Bus-48.png",
  #         iconWidth = 7, iconHeight = 7),layerId=as.character(bus.stop$info))
  #   }
  #   else proxy%>%removeMarker(layerId=as.character(bus.stop$info))
  #
  # })
  #
  observeEvent(input$Museum, {
    p = input$Museum
    proxy<-leafletProxy("map")
    if(p==TRUE){

      proxy%>%
        addCircleMarkers(data=museum,~longtitude,
                   ~latitude, color="#3c1982",
                   label=paste('Name:',museum$NAME, '<br/>',
                               'Tel:', museum$TEL, '<br/>',
                               'Zip:', museum$ZIP, '<br/>',
                               #'Website:', a(museum$URL, href=museum$URL), '<br/>',
                               'Website:', museum$URL, '<br/>',
                               'Add:', museum$ADRESS1, '<br/>'
                   ),stroke = FALSE, fillOpacity = 0.5,radius = 5,

                   group="mus"

        )}

      else proxy%>%clearGroup(group="mus")
    })





      observeEvent(input$Theater, {
        p = input$Theater
        proxy<-leafletProxy("map")
        if(p==TRUE){

        proxy%>%
        addCircleMarkers(data=theater,~longtitude,
                   ~latitude, color = "#18E82",
                   popup=paste('Name:', theater$NAME, '<br/>',
                               'Tel:', theater$TEL, '<br/>',
                               'Zip:', theater$ZIP, '<br/>',
                               'Website:',theater$URL, '<br/>',
                               'Add:', theater$ADDRESS1, '<br/>'),
                   stroke = FALSE, fillOpacity = 0.5,radius = 5, group="the")


        }
        else proxy%>%clearGroup(group="the")

        })
      
     
  
     
  
  ##########################################################################
  ## Panel 3: compare ######################################################
  ########################################################################## 
  
   observeEvent(input$click_jump_next,{
    if(input$click_jump_next){
    updateTabsetPanel(session, "inTabset",selected = "Compare")
   }
     })
  
  observe({

    housing_sort=marksInBounds()
    output$filtered_data <- DT::renderDataTable({
      selected <- input$rank_rows_selected
      if(is.null(selected)){
        housing_final
      } else {
        housing_final[housing_final$addr %in% housing_sort$addr[selected], ]
      }
    })
  })
  
  
  # #Reviews Image
  # image1 <- sprintf("data:image/png;base64,%s", base64encode("../app/star1.png"))
  # image2 <- sprintf("data:image/png;base64,%s", base64encode("../app/star2.png"))
  # image3 <- sprintf("data:image/png;base64,%s", base64encode("../app/star3.png"))
  # image4 <- sprintf("data:image/png;base64,%s", base64encode("../app/star4.png"))
  # 
  # carr_select <- function(x= c()){
  #   y <- c()
  #   for(i in 1:5){
  #     if(x[i] == 1){y[i] <- image1}
  #     if(x[i] == 2){y[i] <- image2}
  #     if(x[i] == 3){y[i] <- image3}
  #     if(x[i] == 4){y[i] <- image4}
  #   }
  #   return(y)
  # 
  # }
  # 
  # observe({
  #   housing_sort=marksInBounds()
  #   output$filtered_data <- renderFormattable({
  #   selected = input$rank_rows_selected
  #   data_rate = housing[housing$addr %in% housing_sort$addr[selected], ]
  #   df = data.frame(
  #     'Rating Type' = c("Address","Rent Price","Bedrooms","Restaurant","Store","Club&Bar","Transport",  "Art&Ent"),
  #     'Rating_Scores' = as.numeric(data_rate[,2:5,8:12])
  #   )
  #   names(df) <- c("Rating Type", "Rating Scores")
  #   
  # 
  #   image_tile <- formatter("img",
  #                           src = x ~ carr_select(x))
  # 
  #   formattable(df,'Rating Scores' = image_tile)
  #   })
  # })

  
})#shiney server