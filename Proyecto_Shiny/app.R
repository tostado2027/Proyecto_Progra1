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

#Datos
datos <- read_csv("college_cords.csv")
datos$aceptasa <- (datos$Accept/datos$Apps)*100

# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("Análisis de Universidades: Selectividad vs Graduación"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.pestanas == 'Análisis de matrícula'",
        helpText("aquí voy a poner lo mio")
      ),
      conditionalPanel(
        condition = "input.pestanas == 'Selectividad vs Graduación'",
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
    ),
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "pestanas",
        tabPanel("Análisis de matrícula"),
        tabPanel("Selectividad vs Graduación",
      plotOutput("scatterPlot")
      ),
      tabPanel("tercera parte"),
      tabPanel("Gastos estudiantiles: Alojamiento y personales",
      plotOutput("histocostos")
      )
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    #Filtar los datos según la seleción
    datos_filtrados <- datos
    if(input$tipo_universidad != "Todas"){
      datos_filtrados <- datos %>% filter(Private == input$tipo_universidad)
    }
    
    # Creación de gráfico 
    p <- ggplot(datos_filtrados, aes(x = aceptasa, y = Grad.Rate))
    
    if(input$tipo_universidad == "Todas"){
      p <- p + geom_point(color ="slategray2", alpha = 0.6, size = 2) +
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
        values = c("No" = "coral3", "Yes" = "darkolivegreen2"),
        labels = c("No" = "Pública", "Yes" = "Privada"),
        drop = FALSE
      )}
      theme_minimal()
      
    #condición para la línea de tendencia
    if(input$mostrar_tendencia){
      p <- p + geom_smooth(aes(group = 1),method = "lm", color = "red", se = FALSE, size = 1.2)
    }
    #Mostrar el gráfico final
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
      values = c("No" = "coral3", "Yes" = "darkolivegreen2"),
      labels = c("No" = "Público", "Yes" = "Privado")
    ) + 
    labs(fill = "Tipo de Universidad")
} else {
  p_hist <- p_hist + geom_histogram(
    bins = input$bins_costos,
    fill = "slategray2",
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
}

# Run the application 
shinyApp(ui = ui, server = server)
