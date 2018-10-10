#install.packages('leaflet.extras')
#install.packages("shinythemes")
#install.packages("shinyWidgets")
#install.packages("htmltools")
#install.packages("DT")

library(data.table)
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(htmltools)
shinyUI(
  
  
  
  fluidPage(includeCSS("style.css"),
            navbarPage(p(class="h","Manhattan Off-Campus Housing"),id = "inTabset",
                       #theme=shinythemes::shinytheme("spacelab"),
                       fluid=T,
                       
                       #####################################1. Home##############################################           
                       tabPanel("All about map",
                                div(class="outer",
                                    tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                    leafletOutput("map1", width = "100%", height = "100%"),
                                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                                  top = 80, left = 10, height = "auto",width = 243,
                                                  
                                                  h3("Select Features",align="center"),
                                                  #checkboxInput("Crime", label = "Crime",value= T),
                                                  #checkboxInput("Ave_rent", label = "Ave. rent", value = F ),
                                                  selectInput("Preference",
                                                              label = "Preference",
                                                              choices = c("Crime",
                                                                          "Ave. rent",
                                                                          "Market",
                                                                          "Garage"),
                                                              selected="Crime"),
                                                  #checkboxInput("Bus", label = "Bus",value= FALSE),
                                                  #checkboxInput("Subway",label="Subway",value = FALSE),
                                                  #checkboxInput("Market", label = "Market",value = FALSE),
                                                  #checkboxInput("Restaurant", label = "Restaurant",value= FALSE),
                                                  hr(),
                                                  h3("Click a Place on the Heatmap",align="center"),
                                                  hr(),
                                                  #h4(textOutput("zip_text"),align="left"),
                                                  #h4(textOutput("avgprice_text"),align="left"),
                                                  #h4(textOutput("avgstudio_text"),align="left"),
                                                  #h4(textOutput("avg1b_text"),align="left"),
                                                  #h4(textOutput("avg2b_text"),align="left"),
                                                  #h4(textOutput("avg3b_text"),align="left"),
                                                  #h4(textOutput("avg4b_text"),align="left"),
                                                  #h4(textOutput("transportation_text"),align="left"),
                                                  #h4(textOutput("amenities_text"),align="left"),
                                                  #h4(textOutput("crime_text"),align="left"),
                                                  
                                                  ######debug line#####
                                                  h4(textOutput("debug"),align="left")
                                                  ,
                                                  #hr(),
                                                  #h5("Next step",align="center"),
                                                  actionButton("click_reset_buttom", "Reset"),
                                                  actionButton("click_jump_next","Check community details"),
                                                  hr(),
                                                  checkboxInput("click_multi","Show Your Trace", value = T)
                                                  
                                                  
                                    ))
                                
                       ),
                       ##################################2.2map###########################################
                       
                       tabPanel("Housing Explorer", icon = icon("map"),
                                
                                fluidRow(
                                  column(3,
                                         h1("Compare Places You Select"),
    
                                         fluidRow(
                                           column(2,
                                                  div(id = "action",actionButton("no_rec2", "Reset"))),
                                           column(1,offset = 2,
                                                  div(actionButton("click_jump_next","View Compare"))
                                           ))),
                                  
                                  column(2, verbatimTextOutput('x4')
                                  ),
                                  
                                  #tags$div(id="searchBar",
                                  #         column(width=1,
                                  #                style = "width:270px;display:inline-block;margin-right: 0px;margin-bottom:0px;margin-top:0px;padding-right:0px",
                                  #                textInput(inputId="location",label="", value="", placeholder = "search your location...")
                                  #         ),
                                  #         column(width=1,
                                  #                style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 0px;left:0px;bottom:5px;padding-left:0px",
                                  #                actionButton("button1",label="", icon = icon("search")))),
                                                  #,style="padding:12px; font-size:100%;color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                           
                                
                                  
                                  column(width=1,
                                         style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 150px",
                                         dropdownButton(circle = FALSE,
                                                        label="Rent Price", status = "default",
                                           sliderInput("price","Rental Price", min=0,max=100000,step = 1000,
                                                     value=c(10000,20000))
                                         
                                  )
                                  ),
                                  
                                  column(width=1,
                                         style="margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 10px",
                                         dropdownButton(circle = FALSE,
                                                      label="Restaurant", status = "default",
                                                      selectInput("restaurant_type","Restaurant type",
                                                       c("Food I Like"="",list("American", "Chinese", "Italian", "Japanese", "Pizza", "Others")), multiple=TRUE))
                                  ),

                                  column(width=1, 
                                         style="margin-top: 25px;display:inline-block;margin-right: 10px;margin-left: 10px",
                                         dropdownButton(circle = FALSE,
                                                        label = "Bedrooms", status = "default",
                                                        selectInput(inputId="min_bedrooms", label="choose", choices = c("studio"=0,"1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6)
                                                                    
                                                        ))
                                         # selectInput(inputId="min_bedrooms", label="",choices = c("min bedroom"=0,"studio"=1,"1b"=2,"2b"=3,"3b"=4,"4b"=5,"5b"=6,"6b"=7))
                                  ),
                                  
                                  column(width=1,
                                         style = "margin-top: 25px;display:inline-block;margin-right: 10px;margin-left: 10px",
                                         dropdownButton(circle = FALSE,
                                                        label = "Bathroom", status = "default",
                                                        selectInput(inputId="min_bathrooms", label="choose", choices = c("studio"=0,"1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6)
                                                                    
                                                        )
                                         )),
                                  column(width=1, 
                                         style = "margin-top: 25px;display:inline-block;margin-right: 10px;margin-left: 10px",
                                         actionButton("button2",label="Reset" 
                                                      #,style="padding:12px; font-size:80%;color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                         ))
                                  
                                  
                                  
                                 
                                ),
                                
                                hr(),
                                
                               
                                mainPanel(
                                  
                                  fluidRow(
                                    
                                    column(7, 
                                           br(),
                                           br(),
                                           # h3("current rank"),
                                           dataTableOutput("rank")
                                           
                                    ),
                                    
                                  
                                    
                                    column(5,
                                           leafletOutput("map", width = "220%", height = 650),
                                           
                                           absolutePanel(id="legend",
                                                         fixed = TRUE,
                                                         draggable = TRUE, top = 160, left = "auto", right = 80, bottom = "auto",
                                                         width = 125, height = 215,
                                                         
                                                         h5("Select Features"),
                                                         checkboxInput("Crime", label = "Crime",value= FALSE),
                                                         checkboxInput("Bus", label = "Bus",value= FALSE),
                                                         checkboxInput("Subway",label="Subway",value = FALSE),
                                                         checkboxInput("Market", label = "Market",value = FALSE),
                                                         checkboxInput("Restaurant", label = "Restaurant",value= FALSE)                                 
                                                         
                                           )#abs panel
                                           
                                    )#column
                                  )#row
                                )#main panel
                       )#tab panel
            )#navbar page
            )# fluidpage
  )#ui

  