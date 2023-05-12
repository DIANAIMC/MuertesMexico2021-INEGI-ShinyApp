library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(sf)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(forcats)
library(extrafont)
library(packcircles)

getwd()
setwd("C:/Users/52552/Downloads")

font_import()
loadfonts(device = "win")
windowsFonts()


data <- read.csv("conjunto_de_datos_defunciones_registradas_2021.csv")
mxpolig <- st_read("MapaMexico.geojson")
estados <- read.csv("estadosMx.csv")
estados

"""
  DataFrame auxiliar para el despliege del mapa
"""
recuento_estados <- aggregate(x = list(Registros = 1, 
                                       Accidental = ifelse(data$presunto == 1, 1, 0),
                                       Homicidio = ifelse(data$presunto == 2, 1, 0),
                                       Suicidio = ifelse(data$presunto == 3, 1, 0),
                                       Guerra = ifelse(data$presunto == 5, 1, 0),
                                       Natural = ifelse(data$presunto == 8, 1, 0)
), 
by = list(clave = data$ent_regis), 
FUN = sum)
recuento_estados <- cbind(Estados = mxpolig$ENTIDAD, recuento_estados)
recuento_estados <- cbind(recuento_estados, Poblacion = estados$POB)

recuento_estados$densidad_accidente <- ( recuento_estados$Accidental / recuento_estados$Poblacion ) * 100000
recuento_estados$densidad_homicidio <- ( recuento_estados$Homicidio / recuento_estados$Poblacion ) * 100000
recuento_estados$densidad_suicidio <- ( recuento_estados$Suicidio / recuento_estados$Poblacion ) * 100000
recuento_estados$densidad_guerra <- ( recuento_estados$Guerra / recuento_estados$Poblacion ) * 1000000
recuento_estados$densidad_natural <- ( recuento_estados$Natural / recuento_estados$Poblacion ) * 100000

colores <- c("#C60B0B","#DD4F28","#D58936","#FFDD76","#DADFA7","#C1D070")
colores1 <- c("#D58936","#FFDD76","#DADFA7")

bins_h <- c(2, 15, 30, 50, 65, 80, 90, 110)
palHomicidio <- colorBin(colores, domain = recuento_estados$densidad_homicidio, bins=bins_h, reverse = TRUE, pretty = F)

bins_n <- c(500, 640, 710, 750, 870, 990, 1110, 1400)
recuento_estados$densidad_natural
palNatural <- colorBin(colores, domain = recuento_estados$densidad_natural, bins=bins_n, reverse = TRUE, pretty = F)

bins_a <- c(15, 20, 25, 30, 35, 40, 50, 60)
palAccidental <- colorBin(colores, domain = recuento_estados$densidad_accidente, bins=bins_a, reverse = TRUE, pretty = F)

bins_s <- c(1, 3, 5, 7, 9, 11, 13, 16)
palSuicidio <- colorBin(colores, domain = recuento_estados$densidad_suicidio, bins=bins_s, reverse = TRUE, pretty = F)

bins_g <- c(0, 1, 2, 15)
palGuerra <- colorBin(colores1, domain = recuento_estados$densidad_guerra, bins=bins_g, reverse = TRUE, pretty = F)

"""
  DataFrame auxiliar para la distribución de muertes por edades agrupadas
"""
recuento_edad <- aggregate(x = list(total = 1, 
                                    Hombre = ifelse(data$sexo == 1, 1, 0),
                                    Mujer = ifelse(data$sexo == 2, 1, 0)
), 
by = list(edad_agru = data$edad_agru), 
FUN = sum)
recuento_edad$CVE <- factor(recuento_edad$edad_agru, levels = c(1:30),
                            labels = c("Menores de un año", "De un año", "De 2 años", "De 3 años",
                                       "De 4 años", "De 5 a 9 años", "De 10 a 14 años", "De 15 a 19 años",
                                       "De 20 a 24 años", "De 25 a 29 años", "De 30 a 34 años", "De 35 a 39 años",
                                       "De 40 a 44 años", "De 45 a 49 años", "De 50 a 54 años", "De 55 a 59 años",
                                       "De 60 a 64 años", "De 65 a 69 años", "De 70 a 74 años", "De 75 a 79 años",
                                       "De 80 a 84 años", "De 85 a 89 años", "De 90 a 94 años", "De 95 a 99 años",
                                       "De 100 a 104 años", "De 105 a 109 años", "De 110 a 114 años", "De 115 a 119 años",
                                       "De 120 años", "No especificada"))
colnames(recuento_edad)[colnames(recuento_edad) == "anio_regis"] <- "total"

"""
  DataFrame auxiliar para obtener muertes por ocupación
"""
recuento_ocupacion <- aggregate(anio_regis ~ ocupacion, data, FUN = length)
recuento_ocupacion$CVE <- factor(recuento_ocupacion$ocupacion, levels = c(1,2,3,4,5,6,7,8,9,10,11,97,98,99),
                                 labels = c("Funcionarios, directores y jefes",
                                            "Profesionistas y técnicos",
                                            "Trabajadores en actividades administrativas",
                                            "Comerciantes, empleados y agentes de ventas",
                                            "Trabajadores personales y vigilancia",
                                            "Trabajadores en actividades agropecuarias",
                                            "Trabajadores artesanales",
                                            "Operadores de maquinaria industrial y conductores",
                                            "Trabajadores en actividades elementales y de apoyo",
                                            "Busca trabajo",
                                            "No trabaja",
                                            "No aplica a menores de 5",
                                            "Insuficientemente especificada",
                                            "No especificada"
                                 ))
colnames(recuento_ocupacion)[colnames(recuento_ocupacion) == "anio_regis"] <- "total"
recuento_ocupacion$CVE <- reorder(recuento_ocupacion$CVE, recuento_ocupacion$total)

"""
  DataFrames auxiliares para mostrar la distribución de muertes en hombres y mujeres por tipo de muerte
"""
recuento_edad_sexo_tipo <- as.data.frame(
  aggregate(anio_ocur ~ edad_agru + sexo + presunto, data = data, FUN = function(x) length(x))
)
colnames(recuento_edad_sexo_tipo)[colnames(recuento_edad_sexo_tipo) == "anio_ocur"] <- "total"
edad_agrupada <- read.csv("edad_agrupada.csv")
edad_agrupada$DESCRIP <- gsub("a\xf1os", "años", edad_agrupada$DESCRIP)
edad_agrupada$DESCRIP <- gsub("a\xf1o", "año", edad_agrupada$DESCRIP)
colnames(edad_agrupada)[colnames(edad_agrupada) == "CVE"] <- "edad_agru"
recuento_edad_sexo_tipo <- merge(recuento_edad_sexo_tipo, edad_agrupada, by = "edad_agru")
colnames(recuento_edad_sexo_tipo)[colnames(recuento_edad_sexo_tipo) == "DESCRIP"] <- "edad"
sexo <- read.csv("sexo.csv")
colnames(sexo)[colnames(sexo) == "CVE"] <- "sexo"
recuento_edad_sexo_tipo <- merge(recuento_edad_sexo_tipo, sexo, by="sexo")
recuento_edad_sexo_tipo <- subset(recuento_edad_sexo_tipo, select = -sexo)
colnames(recuento_edad_sexo_tipo)[colnames(recuento_edad_sexo_tipo) == "DESCRIP"] <- "sexo"
presunta <- read.csv("presunta_defuncion_violenta.csv")
colnames(presunta)[colnames(presunta) == "CVE"] <- "presunto"
recuento_edad_sexo_tipo <- merge(recuento_edad_sexo_tipo, presunta, by="presunto")
recuento_edad_sexo_tipo <- subset(recuento_edad_sexo_tipo, select = -presunto)
colnames(recuento_edad_sexo_tipo)[colnames(recuento_edad_sexo_tipo) == "DESCRIP"] <- "presunto"

recuento_tipo_accidente <- filter(recuento_edad_sexo_tipo, presunto == "Accidente")
recuento_tipo_accidente <- arrange(recuento_tipo_accidente, edad_agru)
recuento_tipo_homicidio <- filter(recuento_edad_sexo_tipo, presunto == "Homicidio")
recuento_tipo_homicidio <- arrange(recuento_tipo_homicidio, edad_agru)
recuento_tipo_suicidio <- filter(recuento_edad_sexo_tipo, presunto == "Suicidio")
recuento_tipo_suicidio <- arrange(recuento_tipo_suicidio, edad_agru)
recuento_tipo_guerra <- filter(recuento_edad_sexo_tipo, presunto == "Operaciones legales y de guerra")
recuento_tipo_guerra <- arrange(recuento_tipo_guerra, edad_agru)
recuento_tipo_natural <- filter(recuento_edad_sexo_tipo, presunto == "No aplica para muerte natural")
recuento_tipo_natural <- arrange(recuento_tipo_natural, edad_agru)

"""
  DataFrames auxiliares para graficar el parentezco del agresor con la víctima
"""
recuento_agresor <- as.data.frame(
  aggregate(anio_ocur ~ sexo + par_agre, data = data, FUN = function(x) length(x))
)
colnames(recuento_agresor)[colnames(recuento_agresor) == "anio_ocur"] <- "total"
recuento_agresor <- merge(recuento_agresor, sexo, by="sexo")
recuento_agresor <- subset(recuento_agresor, select = -sexo)
colnames(recuento_agresor)[colnames(recuento_agresor) == "DESCRIP"] <- "sexo"
parentezco <- read.csv("parentesco_agresor.csv")
parentezco$DESCRIP <- gsub("\xf1", "ñ", parentezco$DESCRIP)
parentezco$DESCRIP <- gsub("\xe9", "é", parentezco$DESCRIP)
parentezco$DESCRIP <- gsub("\xf3", "ó", parentezco$DESCRIP)
parentezco$DESCRIP <- gsub("\xed", "í", parentezco$DESCRIP)
parentezco$DESCRIP <- gsub(" ", "", parentezco$DESCRIP)
colnames(parentezco)[colnames(parentezco) == "CVE"] <- "par_agre"
recuento_agresor <- merge(recuento_agresor, parentezco, by="par_agre")
recuento_agresor <- subset(recuento_agresor, select = -par_agre)
colnames(recuento_agresor)[colnames(recuento_agresor) == "DESCRIP"] <- "agresor"
recuento_agresor <- recuento_agresor[recuento_agresor$agresor != "Noespecificado", ]
recuento_agresor <- recuento_agresor[recuento_agresor$agresor != "Noaplica", ]
recuento_agresor <- recuento_agresor[recuento_agresor$agresor != "Ninguno", ]
homicidio_mujeres <- sum(data$presunto == 2 & data$sexo == 2)
homicidio_hombres <- sum(data$presunto == 2 & data$sexo == 1)
recuento_agresor$total <- ifelse(recuento_agresor$sexo == "Hombre", 
                                 recuento_agresor$total / homicidio_hombres, 
                                 recuento_agresor$total / homicidio_mujeres)

packing <- circleProgressiveLayout(recuento_agresor$total, sizetype='area')
recuento_agresor <- cbind(recuento_agresor, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

recuento_agresor <- recuento_agresor %>% 
  mutate(id = row_number())
dat.gg$sexo <- recuento_agresor$sexo[match(dat.gg$id, recuento_agresor$id)]

"""
  Auxiliar para las graficar las principales causas de muerte en hombres y mujeres
"""
causas <- read.csv("causa_defuncion.csv")
df4 <- data %>%
  rename(causa_def_new = causa_def)
df5 <- causas %>%
  rename(causa_def_new = cve)
df_joined2 <- merge(df4, df5, by = "causa_def_new")
df_subset <- subset(df_joined2, select = c("sexo", "descrip"))
df_grouped <- df_subset %>%
  group_by(sexo, descrip) %>% count()
### PARA HOMBRES
df_grouped_hombre <- df_grouped %>% filter(sexo==1)  %>% rename(cuenta=n, causa=descrip) %>% arrange(desc(cuenta)) %>% head(10)
df_grouped_hombre$causa <- c('COVID-19 virus identificado', "Infarto al miocardio", "COVID-19 virus no identificado",
                             'Neumonía', 'Diabetes no insulinodependiente con complicaciones', 'Agresión con armas de fuego',
                             'Diabetes no insulinodependiente complicación múltiple', 'Cirrosis de higado', 
                             'Cirrosis hepática alcohólica', 'Diabetes no insulinodependiente complicación renal' )
df_grouped_hombre <- df_grouped_hombre %>% arrange(desc(cuenta))
ggplot(df_grouped_hombre, aes(x=cuenta, y=reorder(causa, cuenta, sum))) + geom_bar(stat="identity", fill="#F08D84") 
### PARA MUJERES
df_grouped <- df_subset %>%
  group_by(sexo, descrip) %>% count()
df_grouped_mujer <- df_grouped %>% filter(sexo==2)  %>% rename(cuenta=n, causa=descrip) %>% arrange(desc(cuenta)) %>% head(10)
df_grouped_mujer$causa <- c('Infarto al miocardio', "COVID-19 virus identificado", "COVID-19 virus no identificado",
                            'Diabetes no insulinodependiente con complicación', 'Neumonía', 'Diabetes no insulinodependiente con complicación renal',
                            'Diabetes no insulinodependiente complicación múltiple', 'Cancer de mama', 
                            'Cirrosis de higado', 'Diabetes mellitus' )

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Proyecto final"
  ),
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                menuItem("   General", tabName = "menu1", icon = icon("dashboard")),
                menuItem("   Por sexo", tabName = "menu2", icon = icon("user")),
                menuItem("   Datos", tabName = "menu3", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("menu1", h1("Muertes en México 2021 (INEGI)"),
              fluidRow(
                column(width = 3, selectInput("filtroMapa", label = "Filtrar muertes por tipo:", 
                                              choices = c("Natural", "Accidental", "Homicidio", "Suicidio", "Guerra"), selected = "Accidental")),
                column(width = 7, leafletOutput("miMapa", height = "450px", width = "700")),
              ), 
              div(style= "height:30px"),
              fluidRow(
                column(width = 5, plotOutput("distribucion_general", height="450px")),
                column(width = 7, plotOutput("ocupacion", height="450px"))
              )),
      tabItem("menu2", h1("Muertes en México 2021: hombres y mujeres"), 
              fluidRow(
                column(width = 3, selectInput("sexo", label="Selecciona sexo",
                                              choices =c("Hombres", "Mujeres"), selected="Mujeres")),
                column(width = 7, plotOutput("causas", height="400px"))),
              selectInput("filtrodistribucion", label = "Filtrar distribución por tipo de muerte:", 
                          choices = c("Natural", "Accidental", "Homicidio", "Suicidio", "Guerra"), selected = "Homicidio"),
              fluidRow(column(width = 6, plotOutput("distribucion", height="450px")),
                       column(width = 5, plotOutput("presunto", height="450px"))
              )),
      tabItem("menu3", h1("Tabla interactiva de muertes en México"), DTOutput('tbl'))
    )
  )
)
server <- function(input, output) {
  output$tbl = renderDT(
    recuento_estados, options = list(lengthChange = FALSE)
  )
  # Leaflet con dos marcadores, posicionado en México
  output$miMapa <- renderLeaflet({
    if(input$filtroMapa == "Homicidio"){
      leaflet(mxpolig) %>%
        addTiles() %>%
        addPolygons(stroke = F,
                    smoothFactor = 0.2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    fillColor = ~palHomicidio(recuento_estados$densidad_homicidio),
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = mxpolig$ENTIDAD,
                    labelOptions = labelOptions(direction = "auto")) %>%
        addLegend(position = "bottomleft", pal = palHomicidio, values = ~recuento_estados$densidad_homicidio,
                  title = "Muertes por homicidio en cada 100,000 hab.") %>%
        addPolygons(data = mxpolig, stroke = F, fillOpacity = 0, popup = paste0(
          "<strong> Total de muertes </strong> <br>",
          recuento_estados$Homicidio, "<br>"
        ))
    }else if(input$filtroMapa == "Natural"){
      leaflet(mxpolig) %>%
        addTiles() %>%
        addPolygons(stroke = F,
                    smoothFactor = 0.2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    fillColor = ~palNatural(recuento_estados$densidad_natural),
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = mxpolig$ENTIDAD,
                    labelOptions = labelOptions(direction = "auto")) %>%
        addLegend(position = "bottomleft", pal = palNatural, values = ~recuento_estados$densidad_natural,
                  title = "Muertes naturales en cada 100,000 hab.") %>%
        addPolygons(data = mxpolig, stroke = F, fillOpacity = 0, popup = paste0(
          "<strong> Total de muertes </strong> <br>",
          recuento_estados$Natural, "<br>"
        ))
    } else if(input$filtroMapa == "Accidental"){
      leaflet(mxpolig) %>%
        addTiles() %>%
        addPolygons(stroke = F,
                    smoothFactor = 0.2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    fillColor = ~palAccidental(recuento_estados$densidad_accidente),
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = mxpolig$ENTIDAD,
                    labelOptions = labelOptions(direction = "auto")) %>%
        addLegend(position = "bottomleft", pal = palAccidental, values = ~recuento_estados$densidad_accidente,
                  title = "Muertes accidentales en cada 100,000 hab.") %>%
        addPolygons(data = mxpolig, stroke = F, fillOpacity = 0, popup = paste0(
          "<strong> Total de muertes </strong> <br>",
          recuento_estados$Accidental, "<br>"
        ))
    } else if(input$filtroMapa == "Suicidio"){
      leaflet(mxpolig) %>%
        addTiles() %>%
        addPolygons(stroke = F,
                    smoothFactor = 0.2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    fillColor = ~palSuicidio(recuento_estados$densidad_suicidio),
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = mxpolig$ENTIDAD,
                    labelOptions = labelOptions(direction = "auto")) %>%
        addLegend(position = "bottomleft", pal = palSuicidio, values = ~recuento_estados$densidad_suicidio,
                  title = "Muertes por suicidio en cada 100,000 hab.") %>%
        addPolygons(data = mxpolig, stroke = F, fillOpacity = 0, popup = paste0(
          "<strong> Total de muertes </strong> <br>",
          recuento_estados$Suicidio, "<br>"
        ))
    } else if(input$filtroMapa == "Guerra"){
      leaflet(mxpolig) %>%
        addTiles() %>%
        addPolygons(stroke = F,
                    smoothFactor = 0.2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    fillColor = ~palGuerra(recuento_estados$densidad_guerra),
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = mxpolig$ENTIDAD,
                    labelOptions = labelOptions(direction = "auto")) %>%
        addLegend(position = "bottomleft", pal = palGuerra, values = ~recuento_estados$densidad_guerra,
                  title = "Muertes por guerra en cada millón hab.") %>%
        addPolygons(data = mxpolig, stroke = F, fillOpacity = 0, popup = paste0(
          "<strong> Total de muertes </strong> <br>",
          recuento_estados$Guerra, "<br>"
        ))
    }
  })
  output$distribucion_general=renderPlot({
    ggplot(recuento_edad, aes(x=edad_agru, y = total)) +
      geom_histogram(fill='#F08D84', binwidth = 1,
                     bins = 30, stat = "identity", width = 0.8)+
      scale_x_continuous(breaks = recuento_edad$edad_agru, 
                         labels = recuento_edad$CVE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
            legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
            legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
      labs(
        title = "Distribución de muertes por grupo de edad",
        subtitle = "México 2021",
        y = "Total de muertes",
        x = "Edades agrupadas"
      )
  })
  output$ocupacion=renderPlot({
    ggplot(recuento_ocupacion, aes(x=CVE, y = total)) +
      geom_bar(fill='#FDD07A', binwidth = 1,
               bins = 30, stat = "identity", width = 0.8) +
      coord_flip() +
      scale_x_discrete() +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
            legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
            legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
      labs(
        title = "Total de muertes por ocupación",
        subtitle = "México 2021",
        y = "Total de muertes",
        x = "Ocupaciones"
      )
  })
  output$distribucion=renderPlot({
    if(input$filtrodistribucion == "Homicidio"){
      ggplot(recuento_tipo_homicidio, aes(x=edad_agru, y=total, fill=factor(sexo)))+
        scale_fill_manual(values = c("#FDD07A", "#F08D84", "#9D3851"), labels = c( "Hombre","Mujer", "No especificado")) +
        geom_histogram(stat = "identity", binwidth = 1,
                       bins = 30, width = 0.8)+
        scale_x_continuous(breaks = recuento_tipo_natural$edad_agru, 
                           labels = recuento_tipo_natural$edad) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
        labs(
          title = "Distribución de muertes por homicidio",
          subtitle = "México 2021",
          y = "Total de muertes",
          x = "Edades agrupadas",
          fill = "Sexo")
    } else if(input$filtrodistribucion == "Natural"){
      ggplot(recuento_tipo_natural, aes(x=edad_agru, y=total, fill=factor(sexo)))+
        scale_fill_manual(values = c("#FDD07A", "#F08D84", "#9D3851"), labels = c( "Hombre","Mujer", "No especificado")) +
        geom_histogram(stat = "identity", binwidth = 1,
                       bins = 30, width = 0.8)+
        scale_x_continuous(breaks = recuento_tipo_natural$edad_agru, 
                           labels = recuento_tipo_natural$edad) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
        labs(
          title = "Distribución de muertes naturales",
          subtitle = "México 2021",
          y = "Total de muertes",
          x = "Edades agrupadas",
          fill = "Sexo")
    } else if(input$filtrodistribucion == "Accidental"){
      ggplot(recuento_tipo_accidente, aes(x=edad_agru, y=total, fill=factor(sexo)))+
        scale_fill_manual(values = c("#FDD07A", "#F08D84", "#9D3851"), labels = c( "Hombre","Mujer", "No especificado")) +
        geom_histogram(stat = "identity", binwidth = 1,
                       bins = 30, width = 0.8)+
        scale_x_continuous(breaks = recuento_tipo_natural$edad_agru, 
                           labels = recuento_tipo_natural$edad) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
        labs(
          title = "Distribución de muertes por accidentes",
          subtitle = "México 2021",
          y = "Total de muertes",
          x = "Edades agrupadas",
          fill = "Sexo")
    } else if(input$filtrodistribucion == "Suicidio"){
      ggplot(recuento_tipo_suicidio, aes(x=edad_agru, y=total, fill=factor(sexo)))+
        scale_fill_manual(values = c("#FDD07A", "#F08D84", "#9D3851"), labels = c( "Hombre","Mujer", "No especificado")) +
        geom_histogram(stat = "identity", binwidth = 1,
                       bins = 30, width = 0.8)+
        scale_x_continuous(breaks = recuento_tipo_natural$edad_agru, 
                           labels = recuento_tipo_natural$edad) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
        labs(
          title = "Distribución de muertes por suicidio",
          subtitle = "México 2021",
          y = "Total de muertes",
          x = "Edades agrupadas",
          fill = "Sexo")
    } else if(input$filtrodistribucion == "Guerra"){
      ggplot(recuento_tipo_guerra, aes(x=edad_agru, y=total, fill=factor(sexo)))+
        scale_fill_manual(values = c("#FDD07A", "#F08D84", "#9D3851"), labels = c( "Hombre","Mujer", "No especificado")) +
        geom_histogram(stat = "identity", binwidth = 1,
                       bins = 30, width = 0.8)+
        scale_x_continuous(breaks = recuento_tipo_natural$edad_agru, 
                           labels = recuento_tipo_natural$edad) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
        labs(
          title = "Distribución de muertes por guerra",
          subtitle = "México 2021",
          y = "Total de muertes",
          x = "Edades agrupadas",
          fill = "Sexo")
    }
  })
  output$presunto=renderPlot({
    ggplot() + 
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(sexo)), colour = "black", alpha = 0.6) +
      geom_text(data = recuento_agresor, aes(x, y, size=total, label = agresor)) +
      scale_size_continuous(range = c(1,4)) +
      scale_fill_manual(values = c("#FDD07A", "#F08D84")) +
      theme_void() +
      coord_equal() +
      guides(size = "none") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
            legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
            legend.text = element_text(face = "italic", size = 10, family="Times New Roman"))+
      labs(
        title = "Parentezo del homicida con la víctima",
        y = "Total de muertes",
        x = "Edades agrupadas",
        fill = "Sexo"
      )
  })
  output$causas = renderPlot({
    if(input$sexo == "Hombres"){
      ggplot(df_grouped_hombre, aes(x=cuenta, y=reorder(causa, cuenta, sum))) + geom_bar(stat="identity", fill="#FDD07A") +
        labs(title= "10 principales causas de muerte en hombres", 
             subtitle="México 2021",
             x="Número de muertes", 
             y="Causa de muerte") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"),
              axis.title.x = element_text(face = "bold", size = 10, family="Times New Roman"),
              axis.title.y = element_text(face = "bold", size = 10, family="Times New Roman")
        )
    }
    else if(input$sexo == "Mujeres"){
      ggplot(df_grouped_mujer, aes(x=cuenta, y=reorder(causa, cuenta, sum))) + geom_bar(stat="identity",fill="#F08D84") +
        labs(title= "10 principales causas de muerte en mujeres", 
             subtitle="México 2021",
             x="Número de muertes", 
             y="Causa de muerte") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family="Times New Roman"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 11, family="Times New Roman"),
              legend.title = element_text(face = "bold", size = 11, family="Times New Roman"),
              legend.text = element_text(face = "italic", size = 10, family="Times New Roman"),
              axis.title.x = element_text(face = "bold", size = 10, family="Times New Roman"),
              axis.title.y = element_text(face = "bold", size = 10, family="Times New Roman"))
    }
  })
}

shinyApp(ui,server)
