#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ISLR)
library(readr)
library(ggplot2)
library(tidyverse)

#Datos
datos <- read_csv("college_cords.csv")
#View(datos)
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
      helpText("Nota: La tasa de aceptación se calcula (Aceptados/aplicantes)*100")
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
    # Gráfico de disperción
    p <- ggplot(datos, aes(x = aceptasa, y = Grad.Rate)) +
      geom_point(color ="steelblue", alpha = 0.7, size = 2)+
      labs( title = "Relación entre Tasa de Aceptación y Tasa de Graduación",
            x = "Tasa de Aceptación (%)",
            y = "Tasa de Gradución (%)"
      )+ theme_minimal()
    #condición para la línea de tendencia
    if(input$mostrar_tendencia){
      p <- p + geom_smooth(method = "lm", color = "red", se = FALSE, size = 1.2)
    }
    #Mostrar el gráfico final
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
