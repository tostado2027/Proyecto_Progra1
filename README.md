# Proyecto_Progra1 - Análisis de costos, selectividad y éxito en la educación superior Estadounidense

## Descripción del proyecto
Se utilizó el dataset College del paquete de R "ISLR" con una diferencia; se hizo un proceso de geocodificación con el que se incluyó una variable extra para obtener su latitud y longitud con el paquete de R "tidygeocoder". Por último, se hizo la limpieza de datos manual.
Se utilizaron las librerias tidyverse, leaflet, ISLR, readr, plotly.

### Preguntas a contestar
* ¿Cómo difiere la distribución de los costos de la matrícula externa entre las universidades públicas y privadas?
* ¿Existe una relación entre el nivel de selectividad (tasa de elección) de una universidad y su tasa de graduación?
* ¿Existen patrones geográficos específicos en la distribución de la inversión económica por estudiante entre las universidades privadas y públicas?
* ¿Existe una distribución en la diferencia de los gastos de Alojamientos y Personales segun universidades privadas y públicas?

## Instrucciones de Uso 
1. Clonar repositorio, esto se pegaría en el git bash
``` bash
git clone [https://github.com/tostado2027/Proyecto_Progra1.git](https://github.com/tostado2027/Proyecto_Progra1.git) && cd Proyecto_Progra1
```

2. Cuando inicie la App, estos son los paquetes necesarios, ingreselos a la consola de R:
``` R Console
install.packages(c("shiny", "tidyverse", "leaflet", "ISLR", "readr", "plotly"), dependencies = TRUE)
```
No será hacesario correr en la consola las librerias, ya que vienen en el código de la App.

3. Por último, para ver correr la App, ingrese este codigo en la consola:
``` R Console
shiny::runApp()
```

