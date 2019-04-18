library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(foreign)
library(rgdal)
library(RColorBrewer)
library(leaflet.extras)


setwd("D:/UPenn/GAFL531/Project/App")


df = read.csv('data_clean.csv')

df1 = df[df$latitude< 0.0,]
df1 = df1[df1$longitude> 0.0,]



a = c("#FFEDA0" ,"#FEB24C" ,"#F03B20")

b =c( "#E5F5E0" ,"#A1D99B" ,"#31A354")

c = c( "#DEEBF7" ,"#9ECAE1", "#3182BD")



getRadius <- function(zoom){
  
  if (zoom < 7){
    return(2)
  }else if (zoom < 10){
    return(4)
  }else{
    return(5)
  }
}

getBlur <- function(zoom){
  
  if (zoom < 7){
    return(2)
  }else if (zoom < 10){
    return(4)
  }else{
    return(5)
  }
}

server <- function(input, output) {


  
  # df1  = df1[0:500,]
  #-------------------------
  # Maps
  #-------------------------
  # poly <-
  #   readOGR("tza_admbnda_adm3_20181019", layer = "tza_admbnda_adm3_20181019", encoding = "UTF-8")
  # 
  # poly <- spTransform(poly, CRS("+init=epsg:4326"))
  
  

  
  #Defining Leaflet plot
  
  pal <- colorFactor("viridis", df1$status_group)
  
  
  
  dfFunc = df1[df1$status_group=='functional',]
  dfNonFunc = df1[df1$status_group=='non functional',]
  dfRepair = df1[df1$status_group=='functional needs repair',]
  
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng=34.91, lat=-5.0 , zoom=7) %>%
    addHeatmap(lng = dfFunc$longitude, lat = dfFunc$latitude, gradient = b, radius = 2, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 1) %>%
    addHeatmap(lng = dfNonFunc$longitude, lat = dfNonFunc$latitude, gradient = a, radius = 2, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 2) %>%
    addHeatmap(lng = dfNonFunc$longitude, lat = dfNonFunc$latitude, gradient = a, radius = 2, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 3)
   

  output$map <- renderLeaflet(map)
  
  
  #Observing input for modifying leaflet app
  
  observe({
    statusSelection <- input$status
    
    zoom  = input$map_zoom
    
    if(is.null(zoom)){
      zoom = 5
    }
    
    
  
   
   radius = getRadius(zoom)
   
   
   if(zoom<10){
     
     ### Heatmap
     
     leafletProxy("map") %>% clearHeatmap() %>% clearMarkers()
     
     
     leafletProxy("map") %>%
     { if(1 %in% statusSelection) addHeatmap(., lng = df1[df1$status_group=='functional',]$longitude, lat = df1[df1$status_group=='functional',]$latitude, gradient = a, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 1) else . } %>%
     { if(2 %in% statusSelection) addHeatmap(., lng = df1[df1$status_group=='functional needs repair',]$longitude, lat = df1[df1$status_group=='functional needs repair',]$latitude, gradient = b, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 2) else . } %>%
     { if(3 %in% statusSelection) addHeatmap(., lng = df1[df1$status_group=='non functional',]$longitude, lat = df1[df1$status_group=='non functional',]$latitude, gradient = c, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 3) else . }
       
     
   }else{
     
     ###Circle Markers
     
     bounds = input$map_bounds
     
     df2  = df1[ (df1$latitude > bounds$south) & (df1$latitude < bounds$north) & (df1$longitude > bounds$west) & (df1$longitude < bounds$east),]
     
     leafletProxy("map") %>% clearHeatmap() %>% clearMarkers() %>%
     { if(1 %in% statusSelection) addCircleMarkers(., lng = df2[df2$status_group=='functional',]$longitude, lat = df2[df2$status_group=='functional',]$latitude,
                        radius = 5, stroke = FALSE, fillOpacity = 0.8, fillColor = a[3], 
                        popup = "sdfddf", layerId = 4) else . }%>%
     { if(2 %in% statusSelection) addCircleMarkers(., lng = df2[df2$status_group=='functional needs repair',]$longitude, lat = df2[df2$status_group=='functional needs repair',]$latitude,
                      radius = 5, stroke = FALSE, fillOpacity = 0.8, fillColor = b[3], 
                      popup = "sdfddf", layerId = 5) else . }%>%
     { if(3 %in% statusSelection) addCircleMarkers(., lng = df2[df2$status_group=='non functional',]$longitude, lat = df2[df2$status_group=='non functional',]$latitude,
                      radius = 5, stroke = FALSE, fillOpacity = 0.8, fillColor = c[3], 
                      popup = "sdfddf", layerId = 6) else . } 
       
     
   }
   
   
   
   
   

  })
  
  
}


ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Place Matters",

                  sidebarLayout(
                    
                    
                    mainPanel(
                      leafletOutput("map", width = "100%", height = "650px"),width = 10
                    ),
                    sidebarPanel(
                      checkboxGroupInput("status", label = "Status", 
                                         choices = list("Functional" = 1, "Needs Repair" = 2, "Non Functional" = 3),
                                         selected = c(1,2,3)),
                      
                      selectInput("status1", "Status:", 
                                  choices=c("All","functional","functional needs repair","non functional")),
                      hr(),
                      helpText("Data from AT&T (1961) The World's Telephones.")
                      , width=2
                    )
                    
                  )
                  
                )
                 
)


shinyApp(ui = ui, server = server)




# 
# gradient = colorNumeric("Blues",domain=NULL)
# addHeatmap(lng = dfFunc$longitude, lat = dfFunc$latitude, intensity = replicate(nrow(dfFunc),1) , radius = 30000, minOpacity = 0.1, blur=1, layerId = 1) %>%
# addHeatmap(lng = dfNonFunc$longitude, lat = dfNonFunc$latitude, intensity = replicate(nrow(dfNonFunc),1) ,radius = 10000, minOpacity = 0.1, blur=1, layerId = 2)
# addWebGLHeatmap(lng = dfFunc$longitude, lat = dfFunc$latitude, size = 10000, opacity = 0.5, layerId = 1, gradientTexture = 'skyline') %>%
# addWebGLHeatmap(lng = dfNonFunc$longitude, lat = dfNonFunc$latitude, size = 10000, opacity = 0.5, layerId = 2, gradientTexture = 'deep-sea')
# addProviderTiles("CartoDB.Positron") %>% 
# addPolygons(fillColor = 'blue')
# addCircleMarkers(lng = df1$longitude, lat = df1$latitude,
#                  radius = 2, stroke = FALSE, fillOpacity = 0.8, fillColor = pal(df1$status_group),
#                  layerId = 1)

# addLegend("bottomleft", pal=pal, values=df1$status_group ,
#           layerId="colorLegend")


# leafletProxy("map") %>%
#   clearHeatmap()%>%
#   addHeatmap(lng = dfFunc$longitude, lat = dfFunc$latitude, gradient = b, radius = 2, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 1) %>%
#   addHeatmap(lng = dfNonFunc$longitude, lat = dfNonFunc$latitude, gradient = a, radius = 2, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 2) %>%
#   addHeatmap(lng = dfNonFunc$longitude, lat = dfNonFunc$latitude, gradient = a, radius = 2, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 3)
#   


# 
#   # addCircleMarkers(lng = dfsubset$longitude, lat = dfsubset$latitude, radius=2,
#   #            stroke=FALSE, fillOpacity=0.4, fillColor=pal(dfsubset$status_group),
#   #            layerId = 1)


# 
# if (1 %in% statusSelection){
#   
#   subset = df1[df1$status_group =='functional',]
#   leafletProxy("map") %>% 
#     addHeatmap(lng = subset$longitude, lat = subset$latitude, gradient = b, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 1) 
# }
# 
# if (2 %in% statusSelection){
#   
#   subset = df1[df1$status_group =='functional needs repair',]
#   leafletProxy("map") %>% 
#     addHeatmap(lng = subset$longitude, lat = subset$latitude, gradient = a, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 2) 
# }
# 
# if (3 %in% statusSelection){
#   
#   subset = df1[df1$status_group =='non functional',]
#   leafletProxy("map") %>% 
#     addHeatmap(lng = subset$longitude, lat = subset$latitude, gradient = c, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 3) 
# }