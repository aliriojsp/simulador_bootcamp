library(shiny)
library(ggplot2)
library(dplyr)

File <- read.csv("https://raw.githubusercontent.com/aliriojsp/simulador_bootcamp/master/File.csv")

fluidPage(
    sidebarLayout(
        sidebarPanel(
            "Â¡Hola! Bienvenido/a a esta herramienta para practicar la generaciÃ³n de insights y predicciones con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
            h2("Crea tu grÃ¡fico:"),
            "Genera y customiza grÃ¡ficos para visualizar la relaciÃ³n lineal entre dos variables de interÃ©s" ,
            textInput("title", "TÃ­tulo grÃ¡fico", "RelaciÃ³n entre ... y ..."),
            numericInput("size", "TamaÃ±o del punto", 1, 1),
            checkboxInput("fit", "Agregar liÃ±ea de tendencia", FALSE),
            # Add a continent dropdown selector
            selectInput("dependent", "Variable de interÃ©s",
                        choices = colnames(File),
                        multiple = FALSE)
            ,selectInput("independent", "Conductor",
                         choices = colnames(File),
                         multiple = FALSE),
            h2("Simulador:"),
            "Â¿CuÃ¡l es la probabilidad que el candidato logre a tener un buen desempeÃ±o durante su primer aÃ±o en la compaÃ±ia? Con este simulador,podemos predecir el futuro desempeÃ±o de un candidato!",
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
            "*Esta aplicaciÃ³n usa una base de datos creada por M.R. Edwards & K. Edwards. Para mÃ¡s informaciÃ³n acerca los materiales de soporte, referimos al libro Â´Predictive HR AnalyticsÂ´"
        ),
        mainPanel(h2("Insight"),plotOutput("plot"),h2("PredicciÃ³n futuro desempeÃ±o candidato"),
                  plotOutput("Predictplot")
        )
    )
)
