#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(ISLR)
library(readr)
library(plotly)
library(bslib)

#Datos
datos <- read_csv("college_cords.csv")
datos <- datos %>% filter(Grad.Rate <= 100)
datos$aceptasa <- (datos$Accept/datos$Apps)*100

# Define UI for application 
ui <- fluidPage(
  #Agregando un tema para la aplicación
  theme =  bs_theme(
    bg = "#1C1B2E",        # Fondo oscuro con tono violeta
    fg = "#E8E6F0",        # Texto claro con tinte violeta suave
    primary = "#6959CD",   # Púrpura del código
    secondary = "#7CCD7C", # Tu mismo verde de privadas
    danger = "#CD5C5C",    # Tono equivalente a coral3
    success = "#7CCD7C",   # Verde consistente
    base_font = font_google("Roboto"),
    heading_font = font_google("Raleway"),
    font_scale = 1.05
  ) |> bs_add_rules("
  .well { background-color: #2A2840; border: none; }
  .nav-tabs .nav-link.active { 
    background-color: #6959CD; 
    color: white; 
    border-color: #6959CD; 
  }
  .nav-tabs .nav-link { color: #E8E6F0; }
  .sidebar { border-right: 2px solid #6959CD; }
  h2, h3, h4 { color: #A89FE0; }
  .irs--shiny .irs-bar { background: #6959CD; }
  .irs--shiny .irs-handle { background: #6959CD; }
")
  ,
  # Application title
  titlePanel("Explorador de Universidades de USA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(#el sidebarpanel crea un panel lateral en la zona izquierda, ya sea para escribir o para agregar botones,sliders etc.
      #Pestaña 1:
      conditionalPanel(#El conditional panel sirve especificamente para que cada pestaña de la app tenga un sidebarpanel distinto
        condition = "input.pestanas == 'Análisis de matrícula'",#acá se crea un id que va a ir relacionado al main panel para que identifique en cual pestaña va esta información
        checkboxGroupInput("filtro_tipo","Tipo de Universidad",
                           choices = c("Pública","Privada"),selected = c("Pública","Privada")),
        sliderInput("filtro_matricula","Rango de matrícula ($)",
                    min = min(datos$Outstate), max = max(datos$Outstate),
                    value = c(min(datos$Outstate),max(datos$Outstate))
      )),
      #Pestaña 2:
      conditionalPanel(
        condition = "input.pestanas == 'Relación entre Tasa de aceptación y Tasa de Graduación'",
      #activar/desactivar la línea de tendencia
      checkboxInput("mostrar_tendencia", "Mostar línea de tendencia", value = F),
      hr(),
      #Control 2: Para selecionar el tipo de universidad
      radioButtons("tipo_universidad", "Tipo de Universidad a mostrar:",
                   choices = c("Todas" = "Todas",
                               "Públicas" = "No",
                               "Privadas" = "Yes"),
                   selected = "Todas"),
      br()
      ),
      #Pestaña 3:
      conditionalPanel(
        condition = "input.pestanas == 'Análisis espacial del presupuesto'",
        helpText("Filtrar universidades por inversión económmica institucional"),
        #Control deslizante:
        sliderInput(
          inputId = "rango_expend",
          label = "Rango de Gasto por Estudiante (USD):",
          min = 2000,
          max = 55000,
          value = c(2000, 55000),
          step = 500
        ),
        radioButtons("tipo_universidad_mapa", "Tipo de Universidad (en Mapa):",
                  choices = c("Todas" = "Todas",
                              "Públicas" = "No",
                              "Privadas" = "Yes"),
                  selected = "Todas"),
        hr(),
        #Mini gráfico
        plotOutput("top10Plot", height = "300px")
      ),
      #Pestaña 4:
      conditionalPanel(
        condition = "input.pestanas == 'Gastos estudiantiles: Alojamiento y personales'",
        helpText("Visualización de los datos estudiantiles según su tipo de universidad"),
        br(),
        selectInput(
          inputId = "variable_costos",
          label = "Seleccione algun tipo de gasto",
          choices = c("Alojamiento" = "Room.Board", "Gastos personales" = "Personal"),
          selected = "Room.Board"
        ),
        sliderInput(
          inputId = "bins_costos",
          label = "Cantidades",
          min = 5,
          max = 50,
          value = 25
        ),
        checkboxInput(
          inputId = "separar_tipo",
          label = "Ver tipo de institución",
          value = TRUE
        )
      )
    
    ),
    
  
    mainPanel(
      tabsetPanel(id = "pestanas",
        tabPanel("Análisis de matrícula",
                checkboxInput("ver_puntos","Mostrar puntos individuales por universidad"),
                plotOutput("box_matrícula")
                 ),
        tabPanel("Relación entre Tasa de aceptación y Tasa de Graduación",
      plotOutput("scatterPlot")
      ),
      tabPanel("Análisis espacial del presupuesto",
               leafletOutput("mapa_presupuesto", height = "600px")
               ),
      tabPanel("Gastos estudiantiles: Alojamiento y personales",
      plotOutput("histocostos")
      )
    )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$box_matrícula <- renderPlot({
    
    # primero filtramos los datos para que se pueda hacer el boxplot de manera correcta
    tipos_seleccionados <- c()
    if ("Pública" %in% input$filtro_tipo) 
      tipos_seleccionados <- c(tipos_seleccionados, "No")
    if ("Privada" %in% input$filtro_tipo) 
      tipos_seleccionados <- c(tipos_seleccionados, "Yes")
    
    datos_filtrados_p1 <- datos %>% 
      filter(Private %in% tipos_seleccionados) %>% 
      filter(Outstate >= input$filtro_matricula[1] & Outstate <= input$filtro_matricula[2])
    
   # Grafiquito del boxplot
    boxplot <- ggplot(datos_filtrados_p1, aes(x = Private, y = Outstate, fill = Private)) +
      geom_boxplot(alpha = 0.7,outliers = F) +
      scale_x_discrete(labels = c("No" = "Pública", "Yes" = "Privada")) +
      scale_fill_manual(values = c("No" = "#FF6B6B", "Yes" = "#56E39F"), 
                        labels = c("No" = "Pública", "Yes" = "Privada")) +
      theme_minimal() +
      labs(
        title = "Distribución del Costo de Matrícula Externa",
        x = "Tipo de Institución",
        y = "Costo de Matrícula (USD)",
        fill = "Tipo"
      ) +
      theme(plot.title = element_text(face = "bold", size = 14))
    
    # Agregar los puntos inidivuduales de la dispersión
    if (input$ver_puntos) {
      boxplot <- boxplot + geom_jitter(width = 0.2, alpha = 0.4, color = "#6959CD", size = 1.2)
    }
    
    #acá se muestra al final el gráfico
    boxplot
  })
  output$scatterPlot <- renderPlot({
    
    #Filtar los datos según la selección
    datos_filtrados <- datos
    if(input$tipo_universidad != "Todas"){
      datos_filtrados <- datos %>% filter(Private == input$tipo_universidad)
    }
    
    #Gráfico de disperción 
    p <- ggplot(datos_filtrados, aes(x = aceptasa, y = Grad.Rate))
    
    if(input$tipo_universidad == "Todas"){
      p <- p + geom_point(color ="#6959CD", alpha = 0.6, size = 2) +
        labs(title = "Relación entre Tasa de Aceptación y Tasa de Graduación",
             x = "Tasa de Aceptación (%)",
             y = "Tasa de Graduación (%)")
    } else {
      p <- p + geom_point(aes(color = Private),alpha = 0.8, size = 2)+
      labs( 
        title = "Relación entre Tasa de Aceptación y Tasa de Graduación",
        x = "Tasa de Aceptación (%)",
        y = "Tasa de Gradución (%)",
        color = "Tipo de Universidad"
      ) + 
      scale_colour_manual(
        values = c("No" = "#FF6B6B", "Yes" = "#56E39F"),
        labels = c("No" = "Pública", "Yes" = "Privada"),
        drop = FALSE
      )}
      theme_minimal()
      
    #Condición para la línea de tendencia
    if(input$mostrar_tendencia){
      p <- p + geom_smooth(aes(group = 1),method = "lm", color = "red", se = FALSE, size = 1.2)
    }
    #Muestra el gráfico final
    p
  })
  
output$histocostos <- renderPlot({
p_hist <- ggplot(datos, aes(x = .data[[input$variable_costos]]))

if (input$separar_tipo){
  p_hist <- p_hist + geom_histogram(
    aes(fill = Private),
    bins = input$bins_costos,
    alpha = 0.6,
    position = "identity"
  ) + 
    scale_fill_manual(
      values = c("No" = "#FF6B6B", "Yes" = "#56E39F"),
      labels = c("No" = "Público", "Yes" = "Privado")
    ) + 
    labs(fill = "Tipo de Universidad")
} else {
  p_hist <- p_hist + geom_histogram(
    bins = input$bins_costos,
    fill = "#6959CD",
    color = "snow2",
    alpha = 0.8
  )
}
  
p_hist + theme_minimal() +
  labs(
    title = paste("Distribución de gastos en", ifelse(input$variable_costos == "Room.Board", "Alojamiento", "Gastos personales")),
    x = "Costo estimado en USD",
    y = "Frecuencia"
  )
})

#Parte Anjer (3):
datos_mapa_filtrados <- reactive({
  df <- datos
  
  if (input$tipo_universidad_mapa != "Todas") {
    df <- df %>% filter(Private == input$tipo_universidad_mapa)
  }
  
  df <- df %>% filter(Expend >= input$rango_expend[1] & Expend <= input$rango_expend[2])
  return(df)
})

#Mapa:
output$mapa_presupuesto <- renderLeaflet({
  df_mapa <- datos_mapa_filtrados()
  if (nrow(df_mapa) == 0) return(NULL)
  
  paleta_colores <- colorFactor(palette = if(input$tipo_universidad_mapa == "Todas") c("#6959CD", "#6959CD") else c("#FF6B6B", "#56E39F"), domain = c("No", "Yes"))
  
  leaflet(df_mapa) %>%
    addTiles() %>%
    setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude, radius = 5,
      color = ~paleta_colores(Private), stroke = T, weight = 1, fill = 0.7,
      popup = ~paste0(
        "<b>", Nombre_U, "</b><br>",
        "Tipo: ", ifelse(Private == "Yes", "Privada", "Pública"), "<br>",
        "Gasto por estudiante: $", Expend, "<br>",
        "Matrícula externa: $", Outstate, "<br>",
        "Tasa de Graduación: ", Grad.Rate, "%"
      )
    )
})

#Mini grafico Anjer:

output$top10Plot <- renderPlot({
  df_top <- datos_mapa_filtrados()
  if (nrow(df_top) == 0) return(NULL)
  
  top_10_inst <- df_top %>% arrange(desc(Expend)) %>% head(10)
  
  ggplot(top_10_inst, aes(x = reorder(Nombre_U, Expend), y = Expend, fill = Private)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = if(input$tipo_universidad_mapa == "Todas") c("No" = "#6959CD", "Yes" = "#6959CD") else c("#FF6B6B", "#56E39F")) +
  theme_minimal() +
  labs(title = "Top 10 U. con Mayor Inversión", x = NULL, y = "Gasto por Estudiante (USD)") +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(face = "bold", size = 11), legend.position = "none")
})
}

# Run the application 
shinyApp(ui = ui, server = server)
