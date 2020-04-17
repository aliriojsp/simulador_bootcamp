library(shiny)
library(ggplot2)
library(dplyr)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      "¡Hola! Bienvenido/a a esta herramienta para practicar la generación de insights y predicciones con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
      h2("Crea tu gráfico:"),
      "Genera y customiza gráficos para visualizar la relación lineal entre dos variables de interés" ,
      textInput("title", "Título gráfico", "Relación entre ... y ..."),
      numericInput("size", "Tamaño del punto", 1, 1),
      checkboxInput("fit", "Agregar liñea de tendencia", FALSE),
      # Add a continent dropdown selector
      fileInput('File'),
      selectInput("dependent", "Variable de interÃ©s",
                  choices = colnames(File),
                  multiple = FALSE)
      ,selectInput("independent", "Conductor",
                   choices = colnames(File),
                   multiple = FALSE),
      h2("Simulador:"),
      "¿Cuál es la probabilidad que el candidato logre a tener un buen desempeño durante su primer año en la compañia? Con este simulador,podemos predecir el futuro desempeño de un candidato!",
      selectInput("Genero","Genero (1:Hombre, 2:Mujer)",choices=c(1,2),multiple = FALSE),
      selectInput("Educacion","Educacion (1:Bachelor,2:Master,3:PhD)",choices=c(1,2,3),multiple = FALSE),
      selectInput("ExperienciaPrevia","ExperienciaPrevia (1:Con, 2:Sin)",choices=c(1,2),multiple = FALSE),
      sliderInput("Personalidad_apertura","Personalidad_apertura", min=0,max=100,value=90),
      sliderInput("Personalidad_responsabilidad","Personalidad_responsabilidad", min=0,max=100,value=90),
      sliderInput("Personalidad_extroversion","Personalidad_extroversion", min=0,max=100,value=90),
      sliderInput("Personalidad_amabilidad","Personalidad_amabilidad", min=0,max=100,value=90),
      sliderInput("Competencia_Tecnica","Competencia_Tecnica", min=0,max=5,value=4),
      sliderInput("Competencia_Teamplayer","Competencia_Teamplayer", min=0,max=5,value=4),
      sliderInput("Competencia_PensamientoCritico","Competencia_PensamientoCritico", min=0,max=5,value=4),
      sliderInput("Competencia_Negocio","Competencia_Negocio", min=0,max=5,value=4),
      sliderInput("Competencia_InnovacionYmotivacion","Competencia_InnovacionYmotivacion", min=0,max=5,value=4),
      "*Esta aplicación usa una base de datos creada por M.R. Edwards & K. Edwards. Para más información acerca los materiales de soporte, referimos al libro ´Predictive HR Analytics´"
    ),
    mainPanel(h2("Insight"),plotOutput("plot"),h2("Predicción futuro desempeño candidato"),
              plotOutput("Predictplot")
     )
   )
 )
)
