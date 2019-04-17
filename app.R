library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(foreign)
library(rgdal)
library(RColorBrewer)


setwd("D:/UPenn/GAFL531/Project/App")


df = read.csv('data_clean.csv')

df1 = df[df$latitude< 0.0,]
df1 = df1[df1$longitude> 0.0,]

server <- function(input, output) {


  
  # df1  = df1[0:500,]
  #-------------------------
  # Maps
  #-------------------------
  poly <-
    readOGR("tza_admbnda_adm2_20181019", layer = "tza_admbnda_adm2_20181019", encoding = "UTF-8")
  
  poly <- spTransform(poly, CRS("+init=epsg:4326"))
  
  

  
  #Defining Leaflet plot
  
  pal <- colorFactor("viridis", df1$status_group)
  
  map <- leaflet(data = poly) %>%
    addTiles() %>%
    setView(lng=34.91, lat=-5.0 , zoom=5) %>%
    addProviderTiles("CartoDB.Positron") %>% 
    addCircleMarkers(lng = df1$longitude, lat = df1$latitude,
                     radius = 2, stroke = FALSE, fillOpacity = 0.4, fillColor = pal(df1$status_group)) %>%
    addLegend("bottomleft", pal=pal, values=df1$status_group ,
              layerId="colorLegend")

  output$map <- renderLeaflet(map)
  
  
  #Observing input for modifying leaflet app
  
  observe({
    statusSelection <- input$status
   
    if (statusSelection != "All"){
      dfsubset = df1[df1$status_group==statusSelection]
    }else{
      dfsubset = df1
    }
    
    
    
    leafletProxy("map", data = dfsubset) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = dfsubset$longitude, lat = dfsubset$latitude,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(dfsubset$status_group)) %>%
      addLegend("bottomleft", pal=pal, values=dfsubset$status_group, 
                layerId="colorLegend")
  })
  
  
}


ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Place Matters",
                  titlePanel("Introducing the Shiny App"),
                  br(),
                    
                  sidebarLayout(
                    
                    mainPanel(
                      leafletOutput("map", width = "100%", height = "650px"),width = 10
                    ),
                    sidebarPanel(
                      selectInput("status", "Status:", 
                                  choices=c("All","functional","functional needs repair","non functional")),
                      hr(),
                      helpText("Data from AT&T (1961) The World's Telephones.")
                      , width=2
                    )
                    
                  )
                  
                )
                 
)


shinyApp(ui = ui, server = server)
