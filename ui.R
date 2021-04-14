


shinyUI(
    bootstrapPage(
        navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                   HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Food and Farm Analitycs</a>'), id="nav",
                   windowTitle = "Food and Farm Analytics",
                   tabPanel("Sobre este sitio", 
                            includeMarkdown("Analisis.Rmd"),
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 160, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://digitalagri.es/', tags$img(src='digiagri.png',height='40',width='60'))),
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='http://www.uco.es/', tags$img(src='image.png',height='40',width='100'))),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                         onclick = sprintf("window.open('%s')", 
                                                                           "https://twitter.com/intent/tweet?text=%C2%BFc%C3%B3mo%20ha%20afectado%20el%20coronavirus%20al%20mercado%20de las frutas y verduras?%20Aqu%C3%AD%20lo%20puedes%20ver&url=https://app.fnfanalytics.com/&hashtags=coronavirus,agro")))),
                   
                   tabPanel("Covid",
                            tags$div(
                              tags$h2("Situación Covid"),
                              "“Es un nuevo patógeno altamente contagioso, que puede expandirse deprisa y debe considerarse capaz de causar un enorme impacto social, 
                              económico y sanitario en cualquier lugar. No es SARS y no es gripe”. Esto decía la Organización Mundial de la Salud en el informe de su 
                              misión en China del 24 de febrero de 2020. El 11 de marzo se declaró oficialmente como pandemia.",
                              tags$br(),tags$br(),
                              "El gráfico muestra el número de muertes o de casos diarios que se han ido confirmando oficialmente desde el inicio de la epidemia, por países y continentes.",
                              tags$br(),tags$br(),
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                  pickerInput("continent_select", label = "Continente:",
                                              choices = unique(casos$continente),
                                              options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún continente!"),
                                              multiple = T,
                                              selected = c("Europa", "América", "Asia")),
                                  uiOutput("country_selector"),
                                  pickerInput("outcome_select", "Indicador:",   
                                              choices = c("Casos (total)", "Muertes (total)"), 
                                              selected = c("Muertes (total)"),
                                              multiple = FALSE),
                                  
                                ),
                                mainPanel(width = 9,
                                          streamgraphOutput("streamPlot"))
                                
                              ),
                              tags$br(),tags$br(), tags$h2("Restricciones"),
                              "Desde el inicio de la pandemia, los gobiernos han ido tomando medidas intentando encontrar el equilibrio entre contener el número de casos y permitir la actividad. A continuación se puede observar la evolución de las principales medidas tomadas en España.",
                              tags$br(),tags$br(),
                              "En el siguiente gráfico se muestran la evolución de las principales medidas tomadas en los distintos países del mundo: 0 no existe restricción y 4 restricción severa.",
                              tags$br(),
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                pickerInput("Restriction_country", "País:",   
                                            choices = c(unique(oxford$CountryName)), 
                                            selected = c("España"),
                                            multiple = FALSE)),
                              mainPanel(width = 10,
                                plotlyOutput("restricciones", width = "100%", height = "600px")))
                              
                            ),
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 160, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://digitalagri.es/', tags$img(src='digiagri.png',height='40',width='60'))),
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='http://www.uco.es/', tags$img(src='image.png',height='40',width='100'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                          actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                       onclick = sprintf("window.open('%s')", 
                                                                         "https://twitter.com/intent/tweet?text=%C2%BFc%C3%B3mo%20ha%20afectado%20el%20coronavirus%20al%20mercado%20de las frutas y verduras?%20Aqu%C3%AD%20lo%20puedes%20ver&url=https://app.fnfanalytics.com/&hashtags=coronavirus,agro")))
                   ),
                   tabPanel("Comercio Exterior",
                            tags$head(includeCSS("styles.css")),
                            tags$h2("El comercio exterior en España"),
                            "Descubre cuáles son los productos más exportados e importados de España tanto en valor como en cantidad. ",
                            br(),
                            tags$div(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             pickerInput("Treemap_Indicator", label = "Indicador",
                                                         choices = unique(treemap$Indicator),
                                                         multiple = F,
                                                         selected = "€"),
                                             pickerInput("Treemap_Flow", label = "Flujo",
                                                         choices = unique(treemap$Flow),
                                                         multiple = F,
                                                         selected = "Exportaciones"),
                                             pickerInput("Treemap_Year", label = "Año",
                                                         choices = unique(treemap$Year),
                                                         multiple = F,
                                                         selected = 2020),
                                             pickerInput("Treemap_Country", label = "Países",
                                                         choices = unique(treemap$Country_Name),
                                                         options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún país!"),
                                                         multiple = T,
                                                         selected = unique(treemap$Country_Name)),
                                             ),
                                mainPanel(width = 9,
                                          h4(textOutput("reactive_count_treemap"), align = "right"),
                                          plotlyOutput("treemap", width = "100%") )
                              ),
                              tags$h2("Saldo del Comercio Exterior los últimos años"),
                              "Observa la evolución del comercio desde el año 2012.",
                              sidebarLayout(sidebarPanel(width = 3, 
                                              pickerInput("Evolution_Indicator", label = "Indicador",
                                                            choices = unique(treemap$Indicator),
                                                            multiple = F,
                                                            selected = "100 kg"),
                                              pickerInput("Evolution_Product", label = "Productos",
                                                          choices = sort(unique(treemap$Nombre_producto)),
                                                          options = list(`actions-box` = TRUE,`none-selected-text` = "¡Por favor, selecciona algún producto"),
                                                          multiple = T,
                                                          selected =unique(treemap$Nombre_producto)),
                                              pickerInput("Evolution_Country", label = "Países",
                                                          choices = unique(treemap$Country_Name),
                                                          options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún país!"),
                                                          multiple = T,
                                                          selected = unique(treemap$Country_Name)),
                                              
                                                         ),
                                            mainPanel(width = 9, 
                                            plotlyOutput("evolution", width = "100%"))),              
                              tags$h2("Análisis de los flujos comerciales"),
                              "Investiga cuáles son los principales socios comerciales de España. ",
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                               pickerInput("Map_Indicator", label = "Indicador",
                                                           choices = unique(treemap$Indicator),
                                                           multiple = F,
                                                           selected = "€"),
                                               pickerInput("Map_Flow", label = "Flujo",
                                                           choices = unique(treemap$Flow),
                                                           multiple = F,
                                                           selected = "Exportaciones"),
                                               pickerInput("Map_Year", label = "Año",
                                                           choices = sort(unique(treemap$Year)),
                                                           multiple = F,
                                                           selected = 2020),
                                               pickerInput("Map_Product", label = "Productos",
                                                           choices = sort(unique(treemap$Nombre_producto)),
                                                           options = list(`actions-box` = TRUE,`none-selected-text` = "¡Por favor, selecciona algún producto"),
                                                           multiple = T,
                                                           selected =unique(treemap$Nombre_producto)) ,
                                               h3(textOutput("reactive_case_counts"), align = "right"),
                                               h4(textOutput("reactive_death_count"), align = "right"),
                                               h6(textOutput("clean_date_reactive"), align = "right"),
                                               h6(textOutput("reactive_country_count"), align = "right")
                                  ),
                                  mainPanel(width = 9,
                                            leafletOutput("mymap"),
                                  )
                                )
                                ),
                                absolutePanel(id = "logo", class = "card", bottom = 20, left = 160, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://digitalagri.es/', tags$img(src='digiagri.png',height='40',width='60'))),
                                absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                                tags$a(href='http://www.uco.es/', tags$img(src='image.png',height='40',width='100'))),
                                
                                absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                              actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                           onclick = sprintf("window.open('%s')", 
                                                                             "https://twitter.com/intent/tweet?text=%C2%BFc%C3%B3mo%20ha%20afectado%20el%20coronavirus%20al%20mercado%20de las frutas y verduras?%20Aqu%C3%AD%20lo%20puedes%20ver&url=https://app.fnfanalytics.com/&hashtags=coronavirus,agro")))
                            ),
                   tabPanel("Comercio y consumo interior",
                            tags$h2("Precios en origen"),
                            "Los primeros eslabones de la cadena agroalimentaria perciben los conocidos como precios en origen 
                            y pueden ser de 2 tipos: ",
                            br(),
                            "    - Origen: Es el precio de producto en árbol o en campo sin incluir los costes de 
                            recolección, transporte y centro de manipulación y sin IVA.",
                            br(),
                            "    - Mayoristas en origen: Precios obtenidos a partir de los centros de manipulación. ",
                            tags$div(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             pickerInput("Origen_Product", "Producto:",   
                                                         choices = unique(precios_origen$Producto), 
                                                         multiple = FALSE,
                                                         selected = "Pimiento"),
                                             uiOutput("Origen_Type"),
                                             uiOutput("Origen_Category"),
                                             uiOutput("Origen_Position")
                                             ),
                                mainPanel(width = 9,
                                          plotlyOutput("origen", width = "100%")
                                )
                              )),
                            tags$h2("Precios en los mercados mayoristas en destino"),
                            "Son los precios obtenidos a partir de la colaboración de los Mercados Mayoristas más importantes de España.", 
                            tags$div(
                              sidebarLayout(
                              sidebarPanel(width = 3,
                                           pickerInput("Market_select", "Mercado:",   
                                                       choices = unique(mercados$Mercado), 
                                                       selected = c("Mercamadrid"),
                                                       multiple = FALSE),
                                           pickerInput("Product_select", "Producto:",   
                                                       choices = sort(unique(mercados$Productos)), 
                                                       multiple = FALSE,
                                                       selected = "Lechugas")),
                                mainPanel(width = 9,
                                            plotlyOutput("mercados", width = "100%")
                                    )
                                ),
                              tags$h2("Consumo interior"), 
                              "Todo lo relacionado con los consumidores. Se observan 2 subidas de la demanda: La primera cuando inica el estado de alarma y 
                              la segunda después del verano, al inicio de la segunda ola de la pandemia.",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             shiny::span(tags$i(h6("Si se selecciona el precio medio o el gasto o consumo per capita como
                                                                   indicador seleccionar solamente 1 Comunidad Autónoma y  1 producto. ")), style="color:#045a8d"),
                                             pickerInput("Consumption_Indicator", "Indicador:",   
                                                         choices = unique(consumo$Indicador), 
                                                         selected = "€",
                                                         multiple = FALSE),
                                             pickerInput("Consumption_CCAA", label = "Comunidades Autónomas",
                                                         choices = unique(consumo$CCAA),
                                                         options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún país!"),
                                                         multiple = T,
                                                         selected = unique(consumo$CCAA)),
                                             pickerInput("Consumption_Product", label = "Producto",
                                                         choices = sort(unique(consumo$Producto)),
                                                         options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún país!"),
                                                         multiple = T,
                                                         selected = unique(consumo$Producto))
                                             ),
                                mainPanel(width = 9,
                                          plotlyOutput("consumo", width = "100%") 
                                         
                                ),
                              ),
                              tags$h2("Ventas online"), 
                              "La pandemia también ha provocado cambios en los canales de compra utilizados por los consumidores, 
                              que han dado un salto importante hacia la digitalización. ",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             shiny::span(tags$i(h6("Si se selecciona el precio medio o la penetración como indicador seleccionar solamente 1 producto. ")), style="color:#045a8d"),
                                             pickerInput("sales_Indicator", label = "Indicador",
                                                         choices = unique(ventas_online$Indicador)),
                                             pickerInput("Sales_Product", label = "Producto",
                                                         choices = sort(unique(ventas_online$Producto)),
                                                         options = list(`actions-box` = TRUE, `none-selected-text` = "¡Por favor, selecciona algún producto!"),
                                                         multiple = T,
                                                         selected = unique(ventas_online$Producto))
                                             
                                ),
                                mainPanel(width = 9,
                                  plotlyOutput("ventas_online", width = "100%") 
                                )
                            ),
                            tags$h2("Google Trends"),
                            "Se puede utilizar Google Trends para conocer en tiempo real qué piensan los consumidores, permite conocer que interés generan en Google ciertos términos. 
                            Esta herramienta cobra especial interés en momentos de incertidumbre como los vividos en la pandemia. Hay que tener en cuenta que hay que buscar los términos como los buscan los usuarios, 
                            a veces eso incluye  búsquedas con faltas de ortografía como en el caso de 'sintomas covid' en vez de 'síntomas covid'.",
                            plotlyOutput("google_trends", width = "100%"),
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 160, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://digitalagri.es/', tags$img(src='digiagri.png',height='40',width='60'))),
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='http://www.uco.es/', tags$img(src='image.png',height='40',width='100'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                          actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                       onclick = sprintf("window.open('%s')", 
                                                                         "https://twitter.com/intent/tweet?text=%C2%BFc%C3%B3mo%20ha%20afectado%20el%20coronavirus%20al%20mercado%20de las frutas y verduras?%20Aqu%C3%AD%20lo%20puedes%20ver&url=https://app.fnfanalytics.com/&hashtags=coronavirus,agro")))
                   )),
                   tabPanel("Satélite", 
                            div(class="outer",
                                tags$head(includeCSS("styles.css")),
                                leafletOutput("map_satellite", width="100%", height="100%"),
                                absolutePanel(id = "controls", class = "panel panel-default",
                                              top = 75, left = 55,  fixed=TRUE,
                                              draggable = TRUE,width = "250",height = "auto",
                                        
                                             
                                              pickerInput("Satellite_Layer", label = "Capa",
                                                          choices = c("Color verdadero", "Detectar camiones"),
                                                          multiple = F),
                                              pickerInput("Satellite_Date", label = "Fecha",
                                                          choices = date_Sentinel,
                                                          multiple = F, 
                                                          selected = "2020-05-05"))
                                             
                            ))
        )
    )
)

