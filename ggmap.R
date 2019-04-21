library(ggmap)
library(ggplot2)
library(sf)



register_google(key = "AIzaSyCaV_Yea8FIzsm0ZHyzzikODUPVS_06g_c")

p <- ggmap(get_googlemap(center = c(lon = 36.8, lat = -3.44),
                         zoom = 8, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
p


shp <- read_sf("tza_admbnda_adm2_20181019/tza_admbnda_adm2_20181019.shp")

p+geom_sf(shp)


test_map_uk <- ggmap::get_map(location = unname(st_bbox(shp)), source = "stamen")


  

  
 
 ggmap(get_googlemap(center = c(lon = 36.8, lat = -3.44),
                     zoom = 5, scale = 2,
                     maptype ='terrain',
                     color = 'color'))+
   geom_sf(data = shp, aes(fill = ADM1_EN) , inherit.aes = FALSE)+
   theme(legend.position="none")
 
 
 ggmap(test_map_uk)+
   geom_sf(data = shp, aes(fill = ADM1_EN) , inherit.aes = FALSE)+
   theme(legend.position="none")
 