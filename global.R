# Cargar librerias####
  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  library(shinydashboard)
  library(leaflet)
  library(sp)
  library(dplyr)
  library(tidyr)
  library(geosphere)
  library(plotly)
  library(heatmaply)
  library(sf)
  library(streamgraph)
  library(RColorBrewer)
  library(jsonlite)
  library(readr)
        
# Cargar datos####
  
  # Restricciones
    oxford <- readRDS("oxford.rds")
    names(oxford)[3:336] <-  format(as.Date(as.numeric(names(oxford)[3:336]),
                                         origin = "1899-12-30"), "%m/%d/%Y")

  # Comercio exterior
    
    paises <- st_read("countries.geojson")
    
    # Treemap
    treemap <- readRDS("treemap.rds")
    
    comercio_exterior <- treemap %>% spread(Flow,Value)
    comercio_exterior[is.na(comercio_exterior)] <- 0
    
  # Mercados mayoristas
    
    mercados <- readRDS("Precios_Mercados.rds")
    mercados <- mercados %>% gather("Semana","Precio", 4:55)
    
  # Casos Covid
    casos <- readRDS("casos.rds")
    
  # Comercio interior
    
    precios_origen <- readRDS("Precios_Origen.rds")
    
    comunidades<- st_read("comunidades.gpkg")
    
    consumo <- readRDS("Consumo.rds")
    
    ventas_online <- readRDS("Ventas_internet.rds")
    
    trends <- readRDS("trends.rds")
    
  # Imágenes disponibles Sentinel hub
    
    date_Sentinel_1 <- fromJSON("https://services.sentinel-hub.com/ogc/wfs/dee36465-4f99-433a-a020-2eb546d27a22?REQUEST=GetFeature&TYPENAMES=S2.TILE&OUTPUTFORMAT=application/json&BBOX=-3.669927956359361,40.3600659717162,-3.669927956359362,40.3600659717163&SRSNAME=EPSG:4326&TIME=2019-04-01/2020-11-30")  
    
    date_Sentinel_2 <- fromJSON("https://services.sentinel-hub.com/ogc/wfs/dee36465-4f99-433a-a020-2eb546d27a22?REQUEST=GetFeature&TYPENAMES=S2.TILE&OUTPUTFORMAT=application/json&BBOX=-3.669927956359361,40.3600659717162,-3.669927956359362,40.3600659717163&SRSNAME=EPSG:4326&TIME=2019-04-01/2020-03-21")  
    
    date_Sentinel_3 <- fromJSON("https://services.sentinel-hub.com/ogc/wfs/dee36465-4f99-433a-a020-2eb546d27a22?REQUEST=GetFeature&TYPENAMES=S2.TILE&OUTPUTFORMAT=application/json&BBOX=-3.669927956359361,40.3600659717162,-3.669927956359362,40.3600659717163&SRSNAME=EPSG:4326&TIME=2019-01-01/2019-06-30")  
    
    date_Sentinel <- unique(c(date_Sentinel_1$features$properties$date, 
                                  date_Sentinel_2$features$properties$date,
                                  date_Sentinel_3$features$properties$date))       
    
    # Puntos de interés
    
    Puntos_interes <- data.frame (name  = c("Mercamadrid",	"Mercabarna",	"Mercavalencia",
                                "Mercasevilla",	"Mercabilbao",	"Frutos rojos"),
                      lat = c(40.36, 41.33,	39.44,	37.39,	43.23,	37.18),
                      long = c(-3.67, 2.12,	-0.36,	-5.94,	-2.89,	-6.85))
    
    
    
    
    
  
  
