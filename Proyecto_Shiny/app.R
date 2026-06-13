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
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterPlot")
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
      p <- p + geom_point(color ="steelblue", alpha = 0.6, size = 2) +
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
        values = c("No" = "lightcoral", "Yes" = "palegreen3"),
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
}

# Run the application 
shinyApp(ui = ui, server = server)
