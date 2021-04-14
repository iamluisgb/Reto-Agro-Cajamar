

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Restricciones ####
  
    interactiveHeatmap <- shiny::reactive({
      oxford <- filter(oxford, CountryName == input$Restriction_country)
      
      rownames(oxford) <- oxford$Fecha
      oxford$Fecha <- NULL    
      oxford$CountryName <- NULL
      
          p <- heatmaply(oxford, 
                    dendrogram = "none",
                    xlab = "Fecha", ylab = "", 
                    colors =prettyGraphs::add.alpha(brewer.pal(n=4, name="Reds"),0.7),
                    margins = c(100,100,100,100),
                    titleX = TRUE,
                    label_names = c("Medida", "Fecha:", "Valor"),
                    fontsize_row = 8, fontsize_col = 5,
                    labCol = colnames(oxford),
                    labRow = rownames(oxford),
                    showticklabels = c(FALSE, TRUE),
                    heatmap_layers = theme(axis.line=element_blank())
           )
          p
      })
    
    output$restricciones <- renderPlotly({
      interactiveHeatmap()
      
    })
    
  # Streamplot situación Covid ####
    output$country_selector <- renderUI({
      casos <- filter(casos, continente %in% input$continent_select )
      
      pickerInput("country_select", label = "País:",
                            options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún país!"),
                            choices = unique(casos$Pais),
                            multiple = T,
                            selected = c("España", "Francia", "Italia", "Reino Unido", "Estados Unidos de América", "india"))
      
      })
    
    output$streamPlot <- renderStreamgraph({
      
      casos <- filter(casos, Pais %in% input$country_select )
      
     if(input$outcome_select == "Muertes (total)"){
       casos$Value <- casos$Muertes_14_Dias
     } else if (input$outcome_select == "Casos (total)"){
       casos$Value <- casos$Casos_14_Dias
     }
      casos$Value <- round(casos$Value, 0)
      
      streamgraph(casos, "Pais", "Value", "Fecha", interactive=TRUE, interpolate="cardinal") %>%
        sg_axis_x(20, "Fecha", "%m/%Y") %>%
        sg_fill_brewer("RdBu") %>% 
        sg_legend(TRUE, "País: ")
      
    })
  
  # Comercio exterior mapa ####
  
    df_Comercio <- reactive({ 
      
      Comercio <- filter(Comercio_,  Indicador == input$Indicator & Year == input$Year 
                         & Producto %in% input$Product & Flow ==input$Flow 
                         & Mes %in% input$Month)
      
      return(Comercio)
    })

    output$mymap <- renderLeaflet({
      
      datos <- comercio_exterior
      
      datos <- datos %>% filter(Indicator == input$Map_Indicator & 
                                  Nombre_producto %in% input$Map_Product &
                                  Year == input$Map_Year) %>%
        group_by(Indicator, ISO_A3, Country_Name) %>% summarise(Exportaciones =sum(Exportaciones), 
                                                Importaciones = sum(Importaciones))
      
      if(input$Map_Flow == "Importaciones"){
        datos$Value <- datos$Importaciones
      } else if (input$Map_Flow == "Exportaciones"){
        datos$Value <- datos$Exportaciones
      }
      
      pal <- colorNumeric("Blues", domain = datos$Value,1:5) #paleta de colores para el mapa
      
      table <- sp::merge(paises, datos, by = "ISO_A3") 
      
      if (input$Map_Indicator == "Valor (€)"){
        mark = "€"
      }else{
        mark = "kg"
      }
      
      map <- leaflet(data = table) %>%
        setView(-3.66992, 40.36006, zoom = 3)%>% 
        addPolygons(
          layerId = ~ISO_A3,
          fillOpacity = 0.9,
          fillColor = ~pal(Value),
          color = "#BDBDC3",
          weight = 1, popup = ~paste(
            "<b>",  "</b>", Country_Name, ": " ,prettyNum(round(Value),big.mark = ","), mark, "<br>"))%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addLegend("bottomright", pal = pal, values = ~Value,
                  title = ~paste("<small>",input$Map_Indicator,"</small>") )
      
      
      
      
     
    })
    
    output$reactive_count <- renderText({
  
      Comercio <- group_by(df_Comercio(), Pais, Latitud, Longitud) %>% summarise(Valor = sum(Valor))
      paste0(prettyNum(sum(Comercio$Valor), big.mark=",")," ", input$Indicator)
    })
    
    
    # Treemap Comercio Exterior####
    

    output$treemap <- renderPlotly({
      
      datos <- treemap
      
      datos <- datos %>% filter(Indicator ==input$Treemap_Indicator & 
                                  Flow == input$Treemap_Flow & Year == input$Treemap_Year &
                                  Country_Name %in% input$Treemap_Country)
      
      datos <- datos %>% group_by(Grupo, Nombre_producto) %>% summarise(Value= sum(Value))
      
      datos <- as.data.frame(datos)
      
      datos_2 <- datos %>% group_by(Grupo) %>% summarise(Value= sum(Value))
      
      datos_2$Nombre_producto <- datos_2$Grupo
      
      datos_2$Grupo <- ""
      
      datos <- dplyr::union(datos,datos_2)
      
      datos[is.na(datos)] <- 0
      
      fig <-  datos  %>% plot_ly(
        type='treemap',
        labels= ~Nombre_producto,
        parents = ~Grupo,
        values = ~Value) %>%
        layout(colorway = prettyGraphs::add.alpha(rev(brewer.pal(n=10, name="RdBu")),0.9))
      
      ggplotly(fig)
      
    })
    
    output$reactive_count_treemap <- renderText({
      
      datos <- treemap %>% filter(Indicator ==input$Treemap_Indicator & 
                                  Flow == input$Treemap_Flow & Year == input$Treemap_Year &
                                  Country_Name %in% input$Treemap_Country) %>% summarise(Value = sum(Value))
      if(input$Treemap_Indicator == "Valor (€)"){
        paste0(input$Treemap_Flow, " ", "totales: ", prettyNum(sum(datos$Value), big.mark=","),"  €")
      }else{
        paste0(input$Treemap_Flow, " ", "totales: ", prettyNum(sum(datos$Value), big.mark=","),"  kg")
      }
     
       
    })
    
    # Comercio Exterior Evolución ####
    output$evolution <-renderPlotly({ 
      
      
      datos <- comercio_exterior %>% filter(Indicator == input$Evolution_Indicator & 
                                  Country_Name %in% input$Evolution_Country & 
                                  Nombre_producto %in% input$Evolution_Product) %>%
        group_by(Indicator, Year) %>% summarise(Exportaciones =sum(Exportaciones), 
                                                Importaciones = sum(Importaciones))
      
      datos <- as.data.frame(datos)
      
      datos$Exportaciones <- as.numeric(datos$Exportaciones)
      datos$Importaciones <- as.numeric(datos$Importaciones)
      
      datos$Saldo <- datos$Exportaciones - datos$Importaciones 
      
      FF <- plot_ly() %>% add_trace(x = datos$Year, y = datos$Importaciones, 
                    name = "Importaciones", type = 'bar', 
                    marker = list( color = 'rgb(186, 47, 64)')) %>%
        add_trace(x = datos$Year, y = datos$Exportaciones, 
                  name = "Exportaciones",  type='bar',
                  marker = list( color = 'rgb(22, 96, 167)')) %>%
        add_trace(x = datos$Year, y = datos$Saldo , name = "Saldo", mode='line',
                  type='scatter', color = I('black')) %>%
        layout(yaxis=list(tickformat=','))
      
      if(input$Evolution_Indicator == "€"){
        FF %>%  layout(yaxis=list(title= "Euros"))
      } else{
        FF %>%  layout(yaxis=list(title= "kilogramos"))
      }
        
    })
    
    # Precios en origen #####
    
    output$Origen_Type <- renderUI({
      df <- filter(precios_origen, Producto == input$Origen_Product) 
      pickerInput("Origen_type_selector", "Tipo: ",
                  multiple = FALSE,
                  unique(df$Tipo))
    })
    
    output$Origen_Category <- renderUI({
      df <- filter(precios_origen, Producto == input$Origen_Product) 
      pickerInput("Origen_category_selector", "Categoría: ",
                  multiple = FALSE,
                  unique(df$Categoria))
    })
    
    output$Origen_Position <- renderUI({
      df <- filter(precios_origen, Producto == input$Origen_Product) 
      pickerInput("Origen_position_selector", "Posición: ",
                  multiple = FALSE,
                  unique(df$Posicion))
    })
    
    output$origen <-renderPlotly({  
     df <- precios_origen %>% filter(Producto == input$Origen_Product
                  & Tipo == input$Origen_type_selector & 
                    Categoria == input$Origen_category_selector 
                  & Posicion == input$Origen_position_selector) %>%
       arrange(Semana)
     
      
      plot_ly(x = df$Semana, y = df$`2018`, 
              type = 'scatter', mode = 'line', connectgaps = FALSE, name = "2018") %>%
        add_trace(x = df$Semana, y = df$`2019`, 
                  type = 'scatter', mode = 'line', connectgaps = FALSE, name = "2019")%>%
        add_trace(x = df$Semana, y = df$`2020`, 
                  type = 'scatter', mode = 'line', connectgaps = FALSE, name = "2020") %>%
        layout(margin = list(b=150, l=100),paper_bgcolor='transparent', 
               plot_bgcolor='transparent', xaxis =  list(title = "Semana"),
               yaxis = list(title = "€/kg"),
               legend=list(title=list(text='<b>Año</b>')))
      
      
      })
    
    
    
    # Mercados mayoristas #####
    
    output$mercados <-renderPlotly({ 
      mercados<- filter(mercados, Productos == input$Product_select, Mercado == input$Market_select)
      mercados$Fecha <- NULL
      mercados$Semana <- as.numeric(mercados$Semana)
        
      mercados <- mercados %>% group_by(Mercado,Productos, Semana, Year) %>% summarise(Precio = mean(Precio)) %>%
        spread(Year, Precio)
      
      mercados[is.na(mercados)] <- 0
    
      RR <- plot_ly(data =mercados, x = 1:52, y = mercados$`2018`, 
                    type = 'scatter', mode = 'line', connectgaps = FALSE, name = "2018")%>%
        add_trace(data =mercados, x = 1:52, y = mercados$`2019`, 
                  type = 'scatter', mode = 'line', connectgaps = FALSE, name = "2019")%>%
        add_trace(data =mercados, x = 1:52, y = mercados$`2020`, 
                  type = 'scatter', mode = 'line', connectgaps = FALSE, name = "2020")%>%
        layout(margin = list(b=150, l=100),paper_bgcolor='transparent', 
               plot_bgcolor='transparent', xaxis =  list(title = "Semana"),
               yaxis = list(title = "€/kg"))%>% 
        layout(legend=list(title=list(text='<b>Año</b>')))
      
      ggplotly(RR)
    })
    
    # Consumo interior ####
    
    observeEvent(input$Consumption_Indicator, {
      if (input$Consumption_Indicator %in% c("Penetración (%)",
                                       "Consumo per capita kg", "Gasto per capita kg")) {
        updatePickerInput(session = session, inputId = "Consumption_CCAA", 
                          selected = "Andalucía" )
        updatePickerInput(session = session, inputId = "Consumption_Product", 
                          choices =sort(unique(ventas_online$Producto)), 
                          selected = "Aguacate" )
      }
    })
    
    
    output$consumo <-renderPlotly({ 
      
      
      consumo_df <- filter(consumo, Producto %in% input$Consumption_Product & 
                             CCAA %in% input$Consumption_CCAA
                           & Indicador == input$Consumption_Indicator)
      
      consumo_df <- group_by(consumo_df, Year, Mes) %>% summarise(Valor = sum(valor)) %>% 
        spread(Year, Valor)
      
      
      consumo_df[is.na(consumo_df)] <- 0
      
      order <- data.frame("Mes" = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                         "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"), "Order"= 1:12)
      
      consumo_df <- left_join(consumo_df, order)
      
      consumo_df <-consumo_df[order(consumo_df$Order),]
      
      consumo_df$Mes <- factor(consumo_df$Mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                                                          "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
      
      RR <- plot_ly(data =consumo_df, x = consumo_df$Mes, y = consumo_df$`2018`, 
                    type = 'scatter', mode = 'line', name = "2018")%>%
        add_trace(data =consumo_df, x = consumo_df$Mes, y = consumo_df$`2019`, 
                  type = 'scatter', mode = 'line',  name = "2019")%>%
        add_trace(data =consumo_df, x = consumo_df$Mes, y = consumo_df$`2020`, 
                  type = 'scatter', mode = 'line', name = "2020")%>%
        layout(yaxis=list(tickformat=',d', title=input$Consumption_Indicator))%>% 
        layout(margin = list(b=150, l=100),paper_bgcolor='transparent', 
               plot_bgcolor='transparent', xaxis =  list(title = "Mes"),
               yaxis = list(title = input$Consumption_Indicator))%>%
        layout(yaxis=list(tickformat=','), legend=list(title=list(text='<b>Año</b>')))
      
      ggplotly(RR)
      
      })
    
    # Ventas online ####
    
    observeEvent(input$sales_Indicator, {
      if (input$sales_Indicator %in% c("Precio medio (€/kg)", "Penetración (%)")) {
        updatePickerInput(session = session, inputId = "Sales_Product", 
                          choices =sort(unique(ventas_online$Producto)), 
                          selected = "Aguacate" )
      }
    })
    
    output$ventas_online <-renderPlotly({ 
      
      ventas_df <- filter(ventas_online, Producto %in% input$Sales_Product & 
                           Indicador == input$sales_Indicator)
      
      ventas_df <- group_by(ventas_df, Year, Mes) %>% summarise(Valor = sum(Valor)) %>% 
        spread(Year, Valor)
      
      
      ventas_df[is.na(ventas_df)] <- 0
      
      order <- data.frame("Mes" = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio"), "Order"= 1:6)
      
      ventas_df <- left_join(ventas_df, order)
      
      ventas_df <-ventas_df[order(ventas_df$Order),]
      
      ventas_df$Mes <- factor(ventas_df$Mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio"))
      
     
       plot_ly(data =ventas_online, x = ventas_df$Mes, y = ventas_df$`2019`, 
              type = 'scatter', mode = 'line', name = "2019")%>%
         add_trace(data =ventas_df, x = ventas_df$Mes, y = ventas_df$`2020`, 
                   type = 'scatter', mode = 'line',  name = "2020")%>% 
         layout(yaxis=list(tickformat=',d', title=input$sales_Indicator),
                legend=list(title=list(text='<b>Año</b>')))
      
    })
    
    # Google Trends ####
      
    output$google_trends <-renderPlotly({
      
      plot_ly(data =trends, x = trends$Fecha, y = trends$verduras, 
              type = 'scatter', mode = 'line', name = "'verduras'")%>%
        add_trace(x = trends$Fecha, y = trends$frutas, 
                  type = 'scatter', mode = 'line',  name = "frutas")%>% 
        add_trace(x = trends$Fecha, y = trends$`vitamina c`, 
                  type = 'scatter', mode = 'line',  name = "'vitamina c'")%>%
        add_trace(x = trends$Fecha, y = trends$`sintomas covid`, 
                  type = 'scatter', mode = 'line',  name = "'sintomas covid'")%>%
        add_trace(x = trends$Fecha, y = trends$`confinamiento covid`, 
                  type = 'scatter', mode = 'line',  name = "'confinamiento'")%>%
        layout(yaxis=list(tickformat=',d', title="Interés generado"),
               xaxis = list(
                 type = 'date',
                 tickformat = "%d/%m/%Y"
               ),
               legend=list(title=list(text='<b>Términos de búsqueda</b>')))
      
      
      
    })
    
    
    
    
    # Satélite ####
    
    output$map_satellite <- renderLeaflet({
      
      
      leaflet()%>% addProviderTiles('Esri.WorldImagery')%>% 
        #setView(-3.66992, 40.36006, zoom = 14) %>%
       addMarkers(data = Puntos_interes,~long, ~lat, popup = ~name ) 
      
    })
    
    observe({
      direccion <- paste("https://services.sentinel-hub.com/ogc/wms/dee36465-4f99-433a-a020-2eb546d27a22?REQUEST=GetMap&TIME=", input$Satellite_Date, sep="")
      
      if (input$Satellite_Layer == "Color verdadero") {
        layer <- "TRUE-COLOR-S2L2A"
      }
      if (input$Satellite_Layer == "Detectar camiones"){
        layer <- "CAMIONES"
      }
      
      
   map_satellite <- leafletProxy("map_satellite")%>%clearControls()%>%clearTiles()%>%
     addProviderTiles('Esri.WorldImagery') %>% 
      addWMSTiles(direccion,layers = layer, options = WMSTileOptions(opacity=0.8)) 
      
      
    })
    
    # Prueba mapa ####
    output$map_cnsumo <- renderLeaflet({
      m <- leaflet() %>%
        addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")
    })
    
})
