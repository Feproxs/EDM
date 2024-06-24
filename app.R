#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('shinythemes')
#install.packages('s2')
library(s2)
#install.packages('sf', version = "1.0-1")
#install.packages('deb')
#install.packages('raster')
#install.packages('spData')
#getOption("repos")
#options(repos = c(CRAN = "https://cran.rstudio.com/"))
#update.packages(ask = FALSE)
#R.version.string

## LIBRERIAS NECESARIAS 
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library (raster) # Imágenes raster
library (spData) # Datos espaciales
library(leaflet)
#install.packages("shinyjs")
library(shinyjs)

options(LC_NUMERIC = "en_US.UTF-8") # set decimal mark to "." for parsing

## DATOS USADOS Y TRANSFORMACIÓN DE ESTOS

r <- read_csv2("calidad_aire.csv")

r <- r %>% mutate(Year = as.numeric(format(Fecha,'%Y')))
r <- r %>% rename("dia_semana" = "Dia de la semana",
                  "dia_mes" = "Dia del mes",
                  "velocidad_viento" = "Velocidad del viento",
                  "direccion_viento" = "Direccion del viento",
                  "humidad_relativa" = "Humidad relativa",
                  "radiacion_solar" = "Radiacion solar",
                  "velocidad_max_viento" = "Velocidad maxima del viento"
)

r$Year <- as.factor(r$Year)
r$dia_semana <- as.factor(r$dia_semana)
r$velocidad_max_viento <- as.numeric(r$velocidad_max_viento)	
r$CO <- as.numeric(r$CO)	
r$C7H8 <- as.numeric(r$C7H8)	
r$C6H6 <- as.numeric(r$C6H6)	
r$Ruido <- as.numeric(r$Ruido)	
r$C8H10 <- as.numeric(r$C8H10)	
r$Precipitacion <- as.numeric(r$Precipitacion)	
r$velocidad_viento <- as.numeric(r$velocidad_viento)	
r$`As (ng/m³)` <- as.numeric(r$`As (ng/m³)`)	
r$`Ni (ng/m³)` <- as.numeric(r$`Ni (ng/m³)`)	
r$`Cd (ng/m³)` <- as.numeric(r$`Cd (ng/m³)`)	
r$`Pb (ng/m³)` <- as.numeric(r$`Pb (ng/m³)`)	
r$`B(a)p (ng/m³)` <- as.numeric(r$`B(a)p (ng/m³)`)

`%nin%` <- Negate(`%in%`)	

## SHAPEFILES PARA AYUDARNOS EN LEAFLET
#Primera parte
zv <- st_read("zonas-verdes/zonas-verdes.shp")

buffer <- st_read("buffer_cont_movilidad.shp")

zv_2 <- st_geometry(zv)

bf <- st_geometry(buffer)

#Segunda parte:
est <- st_read("estaciones-atmosfericas/estaciones.shx")

bicis <- st_read("punts-mesura-bicis-espires-electromagnetiques-puntos-medida-bicis-espiras-electr/bicis.shp")

est_2 <- st_geometry(est)

bicis_2 <- st_geometry(bicis)

#buffer del "zonas-verdes":

bf_2 <- st_buffer(zv, dist = 1000)

#Ultima:
diferencia <- st_read("diference.shp")



# DEFINICIÓN DEL UI PARA NUESTRA APLICACIÓN SHINNY
ui <- navbarPage(theme = shinytheme("cerulean"),
                 shinyjs::useShinyjs(),
                 title="Contaminación Valencia",
                 tabPanel("Vista de datos",
                          sidebarLayout(
                            sidebarPanel(h1("Variables"),
                                         checkboxGroupInput("show_vars", "Elige columnas que quieres mostrar:",
                                                            names(r), selected = names(r[,1:5]), inline=TRUE)),
                            mainPanel(h1("Tabla del dataset"),
                                      # fluidRow(
                                      #   column(4,
                                      #          selectInput("table_estation",
                                      #                      "Estacion:",
                                      #                      c("Todas",
                                      #                        unique(as.character(r$Estacion))))
                                      #   )
                                      #   
                                      #   ),
                                      DT::dataTableOutput("mytable1"),
                                      strong("Nota:"),
                                      p("Las columnas vacías significan la falta de datos para el parámetro elegido."))
                          )
                 ),
              
                 
                 ## EVOLUCION TEMPORAL
                 
                 tabPanel("Evolución temporal",
                          sidebarLayout(
                            sidebarPanel(h1("Parámetros"),
                                         dateRangeInput("date_evol", "Elige rango de fechas:",
                                                        min=min(r$Fecha), max=max(r$Fecha), 
                                                        start="2022-01-01", 
                                                        end=max(r$Fecha)),
                                         selectInput("date_var_y","Elige vairable Y:",
                                                     choices=setdiff(names(r), 
                                                                     c("Id", "Fecha","dia_semana",
                                                                       "dia_mes","Estacion","Fecha creacion",
                                                                       "Fecha baja","Year"))),
                                         
                                         selectizeInput("selectize_evolucion", "Elige Estaciones:", multiple=TRUE, 
                                                        choices=c("TODOS",unique(r$Estacion)), 
                                                        selected=c("TODOS")),
                                         selectInput("statistic","En el caso de elegir TODAS las Estaciones, 
                                                     ¿Que método quieres usar?",
                                                     choices=c("mean","median",
                                                               "max","min"))
                            ),
                            
                            mainPanel(p("EVOLUCIÓN PARA UNA/TODAS LAS ESTACIONES"),
                                      #IMPORTANTE
                                      #Añadir aparte un selectize para elwgir estacion. Si elegimos estacion ya no habrá que 
                                      #Hacer ni group by ni summarise...
                                      plotOutput("date_plot"))
                          )),
                 
                 ## LEAFLET:
                 
                 tabPanel("Mapas Leaflet",
                          sidebarLayout(
                            sidebarPanel(h1("Parámetros"),p("Las capas selecionadas se superpondran una encima de otra. 
                                                            Es aconsejable unicamente elegir las capas necesarias, 
                                                            ya que el tiempo de carga es bastante elevado."),
                                         selectizeInput("layerType", "Elige Capas:",
                                                        multiple=TRUE,
                                                        choices=c("Zonas Verdes",
                                                                  "Tránsito Vehicular",
                                                                  "Coches VS Zonas Verdes",
                                                                  "Sensores",
                                                                  "Puestos Bicicletas",
                                                                  "Mayor Contaminación"))),
                            mainPanel(h1("Resultados"),
                                      leafletOutput("map"),
                                      h2("Índice"),
                                      strong("Zonas Verdes:"),
                                      p("Todo lugar acondicionado con hierba, 
                                      flores, árboles, bancos u otros elementos decorativos 
                                      o de mobiliario urbano, destinado al adorno o al uso 
                                      por parte de las personas."),
                                      strong("Tránsito Vehicular:"),
                                      p("Cantidad de tránsito automovilístico 
                                      por la zona indicada, el gradiente de color indica
                                      la frecuencia de paso de los vehículos."),
                                      strong("Coches VS Zonas Verdes:"),
                                      p("Zonas de tránsito de coches junto a las zonas verdes."),
                                      strong("Sensores:"),
                                      p("Sensores: Localización de los sensores de diferentes partículas en el aire (contaminación)."),
                                      strong("Puestos Bicicletas:"),
                                      p("Zonas donde se ubican paradas de bicicletas como valenbici."),
                                      strong("Mayor contaminación:"),
                                      p("Tránsito de coches excluyendo las areas coincidentes con las zonas verdes."))
                          ))
                 
)


# DEFINICIÓN DEL SERVIDOR PARA EJECUTAR EL UI
server <- function(input, output) {
  
  
  
  observe({
    if (length(input$show_vars) == 0) {
      showNotification("Selecta al menos una variable.", type = "error")
    }
  })
  

 
  #Output tabla:
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(r[, input$show_vars, drop = FALSE])
  })
  #Opciones de max. 12 vairables:
  #Revisar como se ha hecho
  observeEvent(input$show_vars, {
    if(length(input$show_vars) > 12) {
      showNotification("Solo se pueden seleccionar hasta 12 opciones.", type = "error")
    }
  })
  
  
  
  #Evolucion:
  
  observe({
    if (length(input$selectize_evolucion) == 0) {
      showNotification("Selecta al menos una estacion", type = "error")
    }
  })
  
  observeEvent(input$selectize_evolucion, {
    if(length(input$selectize_evolucion) == 1 && all(input$selectize_evolucion == "TODOS")) {
      shinyjs::enable(id = "statistic")
    } else {
      shinyjs::disable(id = "statistic")
    }
  })
  
  tabla <- reactive({
    data <- r %>% dplyr::select(c(Fecha, input$date_var_y, Estacion)) %>% rename("variable" = input$date_var_y)
    return(data)
  })
  
  output$date_plot <- renderPlot({
    
    if(length(input$selectize_evolucion) == 1 && all(input$selectize_evolucion == "TODOS")) {
      if(input$statistic == "mean"){
        tabla() %>% filter(Fecha >= input$date_evol[1] & Fecha <= input$date_evol[2]) %>% 
          group_by(Fecha) %>% summarise(mean=mean(variable, na.rm=TRUE)) %>%
          ggplot(aes(x=Fecha,y=mean)) + geom_line()
      }
      else if(input$statistic == "median"){
        tabla() %>% 
          filter(Fecha >= input$date_evol[1] & Fecha <= input$date_evol[2]) %>% 
          group_by(Fecha) %>% 
          summarise(median_value = median(variable, na.rm=TRUE)) %>%
          ggplot(aes(x = Fecha, y = median_value)) + 
          geom_line()
        
      }
      else if(input$statistic == "max"){
        tabla() %>% 
          filter(Fecha >= input$date_evol[1] & Fecha <= input$date_evol[2]) %>% 
          group_by(Fecha) %>% 
          summarise(max_value = max(variable, na.rm=TRUE)) %>%
          ggplot(aes(x = Fecha, y = max_value)) + 
          geom_line()
        
      }
      else if(input$statistic == "min"){
        tabla() %>% 
          filter(Fecha >= input$date_evol[1] & Fecha <= input$date_evol[2]) %>% 
          group_by(Fecha) %>% 
          summarise(min_value = min(variable, na.rm=TRUE)) %>%
          ggplot(aes(x = Fecha, y = min_value)) + 
          geom_line()
      }
    }
    else{
      tabla() %>% filter(Fecha >= input$date_evol[1] & Fecha <= input$date_evol[2] & Estacion %in% input$selectize_evolucion) %>% 
        ggplot(aes(x=Fecha,y=variable, color=Estacion)) + geom_line()
    }
    
  })
  
  ##LEAFLET
  
  msd <- reactive({
    msd8 <- leaflet(options = leafletOptions(minZoom = 12, maxZoom = 20))
    msd8 <- addTiles(msd8)
    msd8 <- setView(msd8,lng = -0.38, lat = 39.46, zoom = 12)
    
    return(msd8)
    
  })
  output$map <- renderLeaflet({
    
    resultado <- msd()
    
    if ("Zonas Verdes" %in% input$layerType){
      
      resultado <- resultado %>% addPolygons(
        
        data = zv,
        color = "darkgreen",
        fillOpacity = 0.4,
        weight = 1.5
      ) 
    }
    
    if("Tránsito Vehicular" %in% input$layerType){
      colores <- colorNumeric(palette = "Blues", domain = buffer$imd)
      
      iconoCoche <- makeIcon(
        iconUrl = 'coche.png',
        iconWidth = 25, iconHeight = 25,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      resultado <- resultado %>% addPolygons(
        
        data = buffer,
        fillColor = colores(buffer$imd),
        color = colores(buffer$imd),
        fillOpacity = 0.01,
        opacity = 0.01) %>% addMarkers(
          
          data = buffer,
          lng = as.numeric(buffer$longitud),
          lat = as.numeric(buffer$latitud),
          icon = iconoCoche,
          popup = as.character(unique(buffer$punto_mues))
        )
      
    }
    if("Coches VS Zonas Verdes" %in% input$layerType){
      colores <- colorNumeric(palette = "Blues", domain = buffer$imd)
      
      resultado <- resultado %>% addPolygons(
        
        data = buffer,
        fillColor = colores(buffer$imd),
        color = colores(buffer$imd),
        fillOpacity = 0.01,
        opacity = 0.01) %>% addPolygons(
          
          data = zv,
          color = "darkgreen",
          fillOpacity = 0.4,
          weight = 1.5
        )
    }
    
    if("Sensores" %in% input$layerType){
      iconoEst <- makeIcon(
        iconUrl = 'iconoest.png',
        iconWidth = 40, iconHeight = 40,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      resultado <- resultado %>% addMarkers(
        data = est,
        icon = iconoEst)
    }
    if("Puestos Bicicletas" %in% input$layerType){
      
      iconoBici3d <- makeIcon(
        iconUrl = 'iconobici3d.png',
        iconWidth = 40, iconHeight = 40,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      resultado <- resultado %>% addMarkers(
        data = bicis,
        icon = iconoBici3d)
    }
    if("Mayor Contaminación" %in% input$layerType){
      colores <- colorNumeric(palette = "Reds", domain = diferencia$imd)
      
      
      resultado <- resultado %>% addPolygons(
        
        data = diferencia,
        fillColor = colores(diferencia$imd),
        color = colores(diferencia$imd),
        fillOpacity = 0.01,
        opacity = 0.01)
    }
    
    resultado
    
  })
  
}

# EJECUTAMOS LA APLICACIÓN 
shinyApp(ui = ui, server = server)
