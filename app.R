library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(foreign)
library(rgdal)
library(RColorBrewer)
library(leaflet.extras)
library(ggplot2)


setwd("D:/UPenn/GAFL531/Project/App")


df = read.csv('data_clean.csv')
df = df[df$latitude< 0.0,]
df = df[df$longitude> 0.0,]

mdl = readRDS('rfModel.rds')

a = c( "#E5F5E0" ,"#A1D99B" ,"#31A354")   #Functional
b =c( "#DEEBF7" ,"#9ECAE1", "#3182BD")   # REpair
c = c("#FFEDA0" ,"#FEB24C" ,"#F03B20")  # Non Functional

quantityLvls = c("All","Dry","Enough","Insufficient","Seasonal","Unknown")
basinLvls = c("All", "Internal", "Lake Nyasa", "Lake Rukwa", "Lake Tanganyika",  "Lake Victoria", "Pangani", "Rufiji", "Ruvuma / Southern Coast", "Wami / Ruvu")    
extractionLvls= c("All", "Gravity","Handpump","Motorpump","Other","Rope pump", "Submersible", "Wind-powered")

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

conv <- function(x){
  
  if(x=="functional"){
    return(1.0)
  }else if (x=='non functional'){
    return(0.0)
  }else{
    return(0.5)
  }
}



server <- function(input, output) {


  # poly <-
  #   readOGR("tza_admbnda_adm3_20181019", layer = "tza_admbnda_adm3_20181019", encoding = "UTF-8")
  # 
  # poly <- spTransform(poly, CRS("+init=epsg:3857"))
  
  

  
  #Defining Leaflet plot
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng=34.91, lat=-5.0 , zoom=6) %>%
    addLegend("bottomleft", colors=c(a[3],b[3],c[3]), labels=c("Functional","Needs Repair","Non Functional") , title = "Pump Status",
              layerId="colorLegend")

  output$map <- renderLeaflet(map)
  
  sub = df
  
  
  #Creating data subset
  
  subsetDf <- reactive({
    
    sub = df
    
    if (input$quantity != "All"){
      sub = sub[sub$quantity == tolower(input$quantity),]
    }
    
    if (input$basin != "All"){
      sub = sub[sub$basin == input$basin,]
    }
    
    if (input$extraction != "All"){
      sub = sub[sub$extraction_type_class == tolower(input$extraction),]
    }
    
    statusList = c()
    if( 1 %in% input$status){ statusList = c("functional", statusList)}
    if( 2 %in% input$status){ statusList = c("functional needs repair", statusList)}
    if( 3 %in% input$status){ statusList = c("non functional", statusList)}
    
    sub = sub[sub$status_group %in% statusList, ]
    
    sub = sub[ (sub$gps_height > input$height[1]) & (sub$gps_height < input$height[2]),]
    
    sub
    
    
  })
  
  
  output$composition <- renderPlot({
    sub  = subsetDf()
    
    bounds = input$map_bounds
    sub  = sub[ (sub$latitude > bounds$south) & (sub$latitude < bounds$north) & (sub$longitude > bounds$west) & (sub$longitude < bounds$east),]
    
    w=table(sub$status_group)/nrow(sub)
    w = data.frame(w)
    w$c = c("A","B","C")
    print("Printing w")
    print(w)
    
    ggplot(data=w, aes(x=Var1, y=Freq, fill=c)) +
      geom_bar(stat="identity")+
      scale_fill_manual("legend", values = c("A" = "#93D7A3", "B" = "#93BADC", "C" = "#FE988C"))+
      theme(legend.position="none",axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            plot.background = element_blank())+
      xlab("Status") + ylab("Percent")
    
    
  }, bg="transparent")
  
  #Observing input for modifying leaflet app
  observe({
    
    statusSelection <- input$status
    zoom  = input$map_zoom
    
    if(is.null(zoom)){
      zoom = 6
    }
    
   radius = getRadius(zoom)
   bounds = input$map_bounds
   
   
   if(zoom<14){
     
     ### Heatmap
     
     leafletProxy("map") %>% clearHeatmap() %>% clearMarkers()
     
     sub = subsetDf()
     sub  = sub[ (sub$latitude > bounds$south) & (sub$latitude < bounds$north) & (sub$longitude > bounds$west) & (sub$longitude < bounds$east),]
     
     # output$composition <- renderText({
     #   paste0('You have selected: ', input$selectfile)
     # })
     
     print(nrow(sub))
     
     
     leafletProxy("map") %>%
     { if(1 %in% statusSelection) addHeatmap(., lng = sub[sub$status_group=='functional',]$longitude, lat = sub[sub$status_group=='functional',]$latitude, gradient = a, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 1) else . } %>%
     { if(2 %in% statusSelection) addHeatmap(., lng = sub[sub$status_group=='functional needs repair',]$longitude, lat = sub[sub$status_group=='functional needs repair',]$latitude, gradient = b, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 2) else . } %>%
     { if(3 %in% statusSelection) addHeatmap(., lng = sub[sub$status_group=='non functional',]$longitude, lat = sub[sub$status_group=='non functional',]$latitude, gradient = c, radius = radius, blur = 1, minOpacity = 0.2, max = 0.5, layerId = 3) else . }
       
     
   }else{
     
     ###Circle Markers
     
     bounds = input$map_bounds
     
     sub = subsetDf()
     print(nrow(sub))
     
     popup <- paste0("<strong>Quantity: </strong>", 
                           sub$quantity, 
                           "<br><strong>Extraction Type : </strong>",
                           sub$extraction_type_class,
                           "<br><strong>Waterpoint type : </strong>",
                           sub$waterpoint_type_group,
                           "<br><strong>Population : </strong>",
                           sub$population)
     
     #Subset based on map bounds
     sub  = sub[ (sub$latitude > bounds$south) & (sub$latitude < bounds$north) & (sub$longitude > bounds$west) & (sub$longitude < bounds$east),]
     
     
     
     leafletProxy("map") %>% clearHeatmap() %>% clearMarkers() %>%
     { if(1 %in% statusSelection) addCircleMarkers(., lng = sub[sub$status_group=='functional',]$longitude, lat = sub[sub$status_group=='functional',]$latitude,
                        radius = 5, stroke = FALSE, fillOpacity = 0.8, fillColor = a[3], 
                        popup = popup, layerId = 4) else . }%>%
     { if(2 %in% statusSelection) addCircleMarkers(., lng = sub[sub$status_group=='functional needs repair',]$longitude, lat = sub[sub$status_group=='functional needs repair',]$latitude,
                      radius = 5, stroke = FALSE, fillOpacity = 0.8, fillColor = b[3], 
                      popup = popup, layerId = 5) else . }%>%
     { if(3 %in% statusSelection) addCircleMarkers(., lng = sub[sub$status_group=='non functional',]$longitude, lat = sub[sub$status_group=='non functional',]$latitude,
                      radius = 5, stroke = FALSE, fillOpacity = 0.8, fillColor = c[3], 
                      popup = popup, layerId = 6) else . } 
       
     
   }
   
   
   
   
   

  })
  
  
  ## Load RF model
  
  abc = data.frame(mdl$importance)
  abc = add_rownames(abc,'Variable')
  abc$Variable <- factor(abc$Variable, levels = abc$Variable[order(abc$MeanDecreaseGini)])
  
  output$varPlot = renderPlot({

    ggplot(abc, aes(x=Variable, y=MeanDecreaseGini,fill=MeanDecreaseGini)) +
      geom_bar(stat='identity') +
      scale_fill_continuous() +
      ylab("Variable Importance") +
      xlab("Variable ") +
      coord_flip()+
      theme(legend.position = "none",
            text = element_text(size = 20)
            )
      

  })
  
  
  
  ################## PREDICTION #########################
  test = df[1,]
  levels(test$quantity) = mdl$forest$xlevels$quantity
  levels(test$scheme_management) = mdl$forest$xlevels$scheme_management
  levels(test$extraction_type_class) = mdl$forest$xlevels$extraction_type_class
  levels(test$funder) = mdl$forest$xlevels$funder
  
  pred = predict(mdl,test,predict.all =TRUE)
  
  pred = c(pred$individual)
  print(pred)
  pred=sapply(c(pred), conv)
  
  
  predDataInput <- eventReactive(input$action,{
    
    
    test$quantity[] = tolower(input$pred_quantity)
    
    print(input$pred_scheme)
    
    test$scheme_management[] = input$pred_scheme

    test$extraction_type_class[] = tolower(input$pred_extraction)

    test$funder[] = input$pred_funder

    test$basin[] = input$pred_basin

    test$installer[] = input$pred_installer

    test$payment[] = input$pred_payment

    test$source[] = input$pred_source

    test$quality_group[] = input$pred_quality

    test$gps_height[] = as.integer(input$pred_gps)

    test$amount_tsh[] = as.integer(input$pred_amount)
    
    
    ##################################################
    
    # column(3,
    #        selectInput("pred_quantity", "Quantity", choices=quantityLvls[2:length(quantityLvls)]),
    #        selectInput("pred_extraction", "Extraction Type", choices=extractionLvls[2:length(extractionLvls)]),
    #        selectInput("pred_funder", "Funder", choices=mdl$forest$xlevels$funder),
    #        selectInput("pred_basin", "Basin", choices=mdl$forest$xlevels$basin)
    # ),
    # 
    # column(4, offset = 1,
    #        selectInput("pred_installer", "Installer", choices=mdl$forest$xlevels$installer),
    #        selectInput("pred_scheme", "Scheme", choices=mdl$forest$xlevels$scheme_management),
    #        selectInput("pred_permit", "Permit", choices=mdl$forest$xlevels$permit),
    #        selectInput("pred_payment", "Payment", choices=mdl$forest$xlevels$payment)
    # ),
    # 
    # column(4,
    #        selectInput("pred_source", "Source", choices=mdl$forest$xlevels$source),
    #        selectInput("pred_quality", "Quality", choices=mdl$forest$xlevels$quality_group),
    #        sliderInput("pred_gps","GPS Height",min = -90, max = 2800),
    #        sliderInput("pred_amount","Amount",min = 0, max = 300000)
           
    ##################################################################
    
    
    pred = predict(mdl,test,predict.all =TRUE)
    
    pred
    
  })
  
  output$prediction = renderText({
    pred = predDataInput()
    predAgg = as.character(pred$aggregate[1])
    
    if (predAgg =='functional'){
      predAgg = "Functional"
    }else if (predAgg =='non functional'){
      predAgg = "Non Functional"
    }else{
      predAgg = "Functional Needs Repair"
    }
    
  })
  
  
  
  # output$rfPred = renderPlot({
  #   
  #   r <- raster(xmn = 0, xmx = 10, ymn = 0, ymx = 5, nrows = 5, ncols = 10)
  #   r[] = predDataInput()
  #   plot(r,axes=FALSE, box=FALSE,legend=FALSE,col=c("#93D7A3","#FE988C"))
  #   plot(rasterToPolygons(r), add=TRUE, border='white', lwd=2) 
  #   
  # })
  
  
  observeEvent(input$action, {
    
    
    
    output$rfPred = renderPlot({
      r <- raster(xmn = 0, xmx = 25, ymn = 0, ymx = 2, nrows = 2, ncols = 25)
      
      pred = predDataInput()
      pred = c(pred$individual)  
      pred=sapply(c(pred), conv)
      
      r[] = pred
      plot(r,axes=FALSE, box=FALSE,legend=FALSE,breaks=c(0.0,0.25,0.75,1.0),col=c("#FE988C","#93BADC","#93D7A3"))
      plot(rasterToPolygons(r), add=TRUE, border='white', lwd=2) 
    })
    
  })
  
  

  
}


ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Water Pumps",
                  
                  tabPanel("Map",
                    
                    sidebarLayout(
                      
                      
                      mainPanel(
                        leafletOutput("map", width = "100%", height = "600px"),
                        
                        absolutePanel(bottom = 20, right = 20, width = 300,
                                      draggable = TRUE,
                                      plotOutput(outputId ="composition", height='120px', width = '200px')),
                        width = 10
                      ),
                      sidebarPanel(
                        checkboxGroupInput("status", label = "Status", 
                                           choices = list("Functional" = 1, "Needs Repair" = 2, "Non Functional" = 3),
                                           selected = c(1,2,3)),
                        hr(),
                        
                        
                        selectInput("quantity", "Quantity ", choices=quantityLvls),
                        sliderInput("height","GPS Height  ", min = -90, max=2800, value = c(-90,2800)),
                        selectInput("basin", "Basin", choices=basinLvls),
                        selectInput("extraction", "Extraction Type", choices=extractionLvls)
                    
                        , width=2
                      )
                      
                    )
                    
                  ),
                  
                  tabPanel("Var Importance",
                           h2("Random Forest Variable Importance"),
                           
                           sidebarLayout(
                             mainPanel(plotOutput("varPlot",  height='600px'),width = 10),
                             
                             sidebarPanel(h6("Random Forest calculates the predictive power of each variable by calculating the mean decrease in Node Impurity caused by each
                                             variable at all nodes across the Random Forest model. This provides a robust and generalizable way to estimate the effect of each
                                             variable in predicting for the target variable"), width = 2,columns = 2)
                           )
                           
                           
                          
                          ),
                  
                  tabPanel("Prediction",
                           
                           
                           sidebarLayout(
                             mainPanel(
                               plotOutput("rfPred", height = "200px", width = "100%")
                             ),
                             sidebarPanel(
                               h4(" Prediction "),
                               h1(textOutput("prediction")),
                               
                               hr(),
                               actionButton("action", label = "Run")
                             )
                           ),
                           wellPanel(width = 12, 
                                     fluidRow(
                                       column(3,
                                              selectInput("pred_quantity", "Quantity", choices=quantityLvls[2:length(quantityLvls)]),
                                              selectInput("pred_extraction", "Extraction Type", choices=extractionLvls[2:length(extractionLvls)]),
                                              selectInput("pred_funder", "Funder", choices=mdl$forest$xlevels$funder),
                                              selectInput("pred_basin", "Basin", choices=mdl$forest$xlevels$basin)
                                       ),
                                       
                                       column(4, offset = 1,
                                              selectInput("pred_installer", "Installer", choices=mdl$forest$xlevels$installer),
                                              selectInput("pred_scheme", "Scheme", choices=mdl$forest$xlevels$scheme_management),
                                              selectInput("pred_permit", "Permit", choices=mdl$forest$xlevels$permit),
                                              selectInput("pred_payment", "Payment", choices=mdl$forest$xlevels$payment)
                                       ),
                                       
                                       column(4,
                                              selectInput("pred_source", "Source", choices=mdl$forest$xlevels$source),
                                              selectInput("pred_quality", "Quality", choices=mdl$forest$xlevels$quality_group),
                                              sliderInput("pred_gps","GPS Height",min = -90, max = 2800,val = 100),
                                              sliderInput("pred_amount","Amount",min = 0, max = 300000, val = 200)
                                              
                                       )
                                       
                                     )
                           )
                  )
                )
)


shinyApp(ui = ui, server = server)


