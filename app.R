library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(flexdashboard)
library(fastDummies)
library(lubridate)
library(DT)



FileRot <- read.csv("https://github.com/aliriojsp/db/raw/master/File24.csv",fileEncoding="UTF-8-BOM")
FileRot <- data.frame(FileRot)
FileRot <- FileRot %>% mutate(Desempeno_Primerano = as.numeric(Desempeno_Primerano),
                              Desempeno_Primerano = if_else(is.na(Desempeno_Primerano), 3, Desempeno_Primerano))
FileRot <- data.frame(FileRot)
FileRot<-FileRot%>%rename(Género=Genero,"Experiencia Previa"="ExperienciaPrevia","Posgrado"="Posgrado" ,"Gestión del cambio"="Gestion_Cambio","Edad"="Edad","Competencia: Comunicación"="Competencia_Comunicacion",
                          "Competencia: Teamplayer"="Competencia_Teamplayer","Competencia: Pensamiento critico"="Competencia_PensamientoCritico",
                          "Competencia: Negocio"="Competencia_Negocio","Competencia: Innovación y motivación"="Competencia_InnovacionYmotivacion","Desempeño Primer Año"="Desempeno_Primerano", "Edad"="Edad")


shinyApp(
  ui <- navbarPage(column(width = 6, tags$img(src = "https://github.com/aliriojsp/simulador_bootcamp/raw/master/logoweb.png")),
                   tabPanel("Insights", fluidPage(theme = shinytheme("flatly"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      "¡Hola! Bienvenido/a al toolkit analitico para selección y reclutamiento. Esta aplicación es una demo para poder observar el alcance de esta herramienta analitica. Según las necesidades del cliente, Bondi X puede customizar esta herramienta. En esta solapa se puede visualizar como un conductor está relacionado con el variable de interés. En este caso, las caracteristicas de un colaborador exitoso (o alto desempeño) en un rol especifico.",
                                                      h2("Crea tu gráfico:"),
                                                      "Genera y customiza gráficos para visualizar la relación entre el desempeño de los colaboradores y un conductor especifico." ,
                                                      selectInput("driver", "Conductor",
                                                                  choices = c("Género","Experiencia Previa","Posgrado","Gestión del cambio","Edad","Competencia: Comunicación",
                                                                              "Competencia: Teamplayer","Competencia: Pensamiento critico",
                                                                              "Competencia: Negocio","Competencia: Innovación y motivación"),
                                                                  multiple = FALSE)),
                                                    mainPanel(h2("Insight"),plotOutput("plot", height = 700)
                                                    )
                                                  )
                   )
                   ),
                   tabPanel("Simulador", fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         "¡Hola! Bienvenido/a a esta herramienta para practicar la generación de predicciones con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
                         h2("Simulador:"),
                         "Con este simulador,podemos predecir la probabilidad de que un colaborador renuncie",
                         selectInput("Posgrado","Posgrado (0:No ,1:Si)",choices=c(0,1),multiple = FALSE),
                         selectInput("Experiencia Previa","Experiencia Previa (0:Sin, 1:Con)",choices=c(0,1),multiple = FALSE),
                         sliderInput("Comunicacion","Competencia: Comunicación", min=0,max=3,value=2),
                         sliderInput("Teamplayer","Competencia: Teamplayer", min=0,max=5,value=4),
                         sliderInput("critico","Competencia: PensamientoCritico", min=0,max=5,value=4),
                         sliderInput("Negocio","Competencia: Negocio", min=0,max=5,value=4),
                         sliderInput("Innovación","Competencia: Innovacion y motivacion", min=0,max=5,value=4),
                         sliderInput("Edad","Edad", min=20,max=70,value=25)
                       ),
                       mainPanel(h2("Candidato ideal"),plotOutput("predictplot")
                       )
                     )
                   )
                   ),
                   tabPanel("Predicción", fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         h5("En está sección del toolkit se puede revisar y descargar la base de datos con la predicción para cada candidato"),
                         downloadButton("downloadData", "Descargar")),
                       mainPanel(h2("Distribución candidatos"),plotOutput("Predictbarra"),h2("Predicción potencial de candidatos"),dataTableOutput("PredictDatabase")
                       )
                     )
                   )
                   )
                   
                   
  ),
  
  
  
  # Define the server logic
  server <- function(input, output) {
    output$plot <- renderPlot({
      RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      # Subset the gapminder dataset by the chosen continents
      p <- FileRot%>%ggplot(aes(y=`Desempeño Primer Año`, x=get(input$driver)))+geom_smooth(method="lm",se=0)+RemoveGrid+scale_y_continuous(name="Desempeño primer año", limits=c(1,5))+scale_x_continuous(name=input$driver)+theme(
        axis.title.x = element_text(size = 16,face="bold"),axis.text.x = element_text(size = 14),axis.title.y = element_text(size = 16,face="bold"),axis.text.y = element_text(size = 14))+geom_point()
      p
      
    })
    
    
    
    output$predictplot <- renderPlot({
      
      # Subset the gapminder dataset by the chosen continents
      RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      
      # Genero<- c(input$Genero)
      "Posgrado"<-c(input$Posgrado)
      "Experiencia Previa"<- c(input$`Experiencia Previa`)
      "Edad"<-c(input$Edad)
      "Competencia: Comunicación"<-c(input$Comunicacion)
      "Competencia: Teamplayer"<-c(input$Teamplayer)
      "Competencia: Pensamiento critico"<-c(input$critico)
      "Competencia: Negocio"<-c(input$Negocio)
      "Competencia: Innovación y motivación"<-c(input$Innovación)
      
      Frame<- data.frame("Posgrado","Experiencia Previa","Edad","Competencia: Comunicación","Competencia: Teamplayer","Competencia: Pensamiento critico","Competencia: Negocio","Competencia: Innovación y motivación")
      Modelo<-lm(`Desempeño Primer Año`~as.factor(Posgrado)+as.factor(`Experiencia Previa`)+`Edad`+`Competencia: Comunicación`+
                   `Competencia: Teamplayer`+`Competencia: Pensamiento critico`+`Competencia: Negocio`+`Competencia: Innovación y motivación`, data=FileRot)
      Frame$Predict<-predict(Modelo,Frame) 
      
      perf <- ggplot(FileRot, aes(x=`Desempeño Primer Año`))+ geom_density(adjust=2,alpha=0.2,fill="dodgerblue")+RemoveGrid+geom_vline(xintercept=Frame$Predict, size=1.5, color="red")+geom_text(aes(x=Frame$Predict, label="Predicción desempeño candidato", y=0.1), colour="black", angle=90, vjust = 1.2, text=element_text(size=11))+ scale_x_continuous(name="Desempeño colaboradores actuales")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
      
      perf
    }
    )
    
    Database <- reactive({
      Date_Generator<-sample(seq(as.Date('2019/03/01'), as.Date('2020/08/02'), by="day"), nrow(FileRot))
      FileRot$Fecha<-ymd(Date_Generator)
      FileRot<-FileRot%>%arrange(desc(Fecha))
      Secuencia<- c(nrow(FileRot):1)
      FileRot$Candidato<-as.numeric(Secuencia)
      FileRot$Fecha<-Date_Generator
      FileRot<-as.data.frame((FileRot))
      
      Modelo<-lm(`Desempeño Primer Año`~as.factor(Posgrado)+as.factor(`Experiencia Previa`)+`Edad`+`Competencia: Comunicación`+
                   `Competencia: Teamplayer`+`Competencia: Pensamiento critico`+`Competencia: Negocio`+`Competencia: Innovación y motivación`, data=FileRot)
      FileRot$Predict<-round(predict(Modelo),2)
      c <- cut(FileRot$Predict,breaks=c(0,2,4,5))
      FileRot$Potencial[FileRot$Predict < 2] <- "BAJA"
      FileRot$Potencial[FileRot$Predict > 2 & FileRot$Predict < 4] <- "MEDIA"
      FileRot$Potencial[FileRot$Predict >= 4] <- "ALTA"
      FileRot$Potencial<-as.factor(FileRot$Potencial)
      FileRot$Potencial<-factor(FileRot$Potencial, levels = c("BAJA", "MEDIA", "ALTA"))
      FileRot<-FileRot%>%select("Candidato","Fecha","Potencial","Predict","Género","Posgrado","Experiencia Previa","Edad","Competencia: Comunicación","Competencia: Teamplayer","Competencia: Pensamiento critico","Competencia: Negocio","Competencia: Innovación y motivación","Desempeño Primer Año",               
                                "Posgrado","Gestión del cambio")
      
      FileRot
    }  
    )
    
    output$Predictbarra<-renderPlot({
      RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      Data<-Database()
      Data%>%ggplot(aes(Potencial))+geom_bar()+RemoveGrid+coord_flip()
    })
    output$PredictDatabase<-renderDataTable({
      Data<-Database()
      Data
    })
    
    
  }  
)
