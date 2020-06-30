library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(flexdashboard)
library(fastDummies)

FileRot <- read.csv("https://raw.githubusercontent.com/aliriojsp/simulador_bootcamp/master/File.csv")
FileRot <- data.frame(FileRot)
FileRot <- FileRot %>% mutate(Desempeno_Primerano = as.numeric(Desempeno_Primerano),
         Desempeno_Primerano = if_else(is.na(Desempeno_Primerano), 3, Desempeno_Primerano))
results <- fastDummies::dummy_cols(FileRot, select_columns = "Educacion")
FileRot <- results
FileRot <- data.frame(FileRot)

shinyApp(
  ui <- navbarPage("People Analytics",
                   tabPanel("Insights", fluidPage(theme = shinytheme("flatly"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      "¡Hola! Bienvenido/a a esta herramienta para practicar la generación de insights con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
                                                      h2("Crea tu gráfico:"),
                                                      "Genera y customiza gráficos para visualizar la relación lineal entre dos variables de interés" ,
                                                      selectInput("driver", "Conductor",
                                                                  choices = c("Genero","ExperienciaPrevia","Educacion_1","Educacion_2","Educacion_3","Personalidad_apertura","Personalidad_responsabilidad",
                                                                              "Personalidad_extroversion","Personalidad_amabilidad",
                                                                              "Personalidad_neuroticismo","Competencia_Tecnica",
                                                                              "Competencia_Teamplayer","Competencia_PensamientoCritico",
                                                                              "Competencia_Negocio","Competencia_InnovacionYmotivacion"),
                                                                  multiple = FALSE)),
                                                    mainPanel(h2("Insight"),plotOutput("plot", height = 700)
                                                    )
                                                  )
                   )
                   ),
                   
                   
                   tabPanel("Predicción", fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         "¡Hola! Bienvenido/a a esta herramienta para practicar la generación de predicciones con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
                         h2("Simulador:"),
                         "Con este simulador,podemos predecir la probabilidad de que un colaborador renuncie",
                         selectInput("Genero","Genero (1:Hombre, 2:Mujer)",choices=c(1,2),multiple = FALSE),
                         selectInput("Educacion","Educacion (1:ITBA,2:Sin titulo,3:UBA)",choices=c(1,2,3),multiple = FALSE),
                         selectInput("ExperienciaPrevia","ExperienciaPrevia (1:Con, 2:Sin)",choices=c(1,2),multiple = FALSE),
                         sliderInput("Personalidad_apertura","Personalidad_apertura", min=0,max=100,value=90),
                         sliderInput("Personalidad_responsabilidad","Personalidad_responsabilidad", min=0,max=100,value=90),
                         sliderInput("Personalidad_extroversion","Personalidad_extroversion", min=0,max=100,value=90),
                         sliderInput("Personalidad_amabilidad","Personalidad_amabilidad", min=0,max=100,value=90),
                         sliderInput("Personalidad_neuroticismo","Personalidad_neuroticismo", min=0,max=100,value=90),
                         sliderInput("Competencia_Tecnica","Competencia_Tecnica", min=0,max=5,value=4),
                         sliderInput("Competencia_Teamplayer","Competencia_Teamplayer", min=0,max=5,value=4),
                         sliderInput("Competencia_PensamientoCritico","Competencia_PensamientoCritico", min=0,max=5,value=4),
                         sliderInput("Competencia_Negocio","Competencia_Negocio", min=0,max=5,value=4),
                         sliderInput("Competencia_InnovacionYmotivacion","Competencia_InnovacionYmotivacion", min=0,max=5,value=4)
                       ),
                       mainPanel(h2("Candidato ideal"),plotOutput("predictplot")
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
      p <- FileRot%>%ggplot(aes(y=Desempeno_Primerano, x=get(input$driver)))+geom_smooth(method="lm",se=0)+RemoveGrid+scale_y_continuous(name="Desempeño primer año", limits=c(1,5))+scale_x_continuous(name=input$driver)+theme(
        axis.title.x = element_text(size = 16,face="bold"),axis.text.x = element_text(size = 14),axis.title.y = element_text(size = 16,face="bold"),axis.text.y = element_text(size = 14))
      p
      
    })
    
    
    
    output$predictplot <- renderPlot({
      
      # Subset the gapminder dataset by the chosen continents
      RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      
      Genero<- c(input$Genero)
      Educacion<-c(input$Educacion)
      ExperienciaPrevia<- c(input$ExperienciaPrevia)
      Personalidad_apertura<-c(input$Personalidad_apertura)
      Personalidad_responsabilidad<-c(input$Personalidad_responsabilidad)
      Personalidad_extroversion<-c(input$Personalidad_extroversion)
      Personalidad_amabilidad<-c(input$Personalidad_amabilidad)
      Personalidad_neuroticismo<-c(input$Personalidad_neuroticismo)
      Competencia_Tecnica<-c(input$Competencia_Tecnica)
      Competencia_Teamplayer<-c(input$Competencia_Teamplayer)
      Competencia_PensamientoCritico<-c(input$Competencia_PensamientoCritico)
      Competencia_Negocio<-c(input$Competencia_Negocio)
      Competencia_InnovacionYmotivacion<-c(input$Competencia_InnovacionYmotivacion)
      
      Frame<- data.frame(Genero,Educacion,ExperienciaPrevia,Personalidad_apertura,Personalidad_responsabilidad,Personalidad_neuroticismo,Personalidad_extroversion,Personalidad_amabilidad,Competencia_Tecnica,Competencia_Teamplayer,Competencia_PensamientoCritico,Competencia_Negocio,Competencia_InnovacionYmotivacion)
      Modelo<-glm(FileRot$Desempeno_Primerano~as.factor(Genero)+as.factor(Educacion)+as.factor(ExperienciaPrevia)+Personalidad_neuroticismo+Personalidad_apertura+Personalidad_responsabilidad+Personalidad_extroversion+Personalidad_amabilidad+Competencia_Tecnica+
                    Competencia_Teamplayer+Competencia_PensamientoCritico+Competencia_Negocio+Competencia_InnovacionYmotivacion, data=FileRot)
      Frame$Predict<-predict(Modelo,Frame) 
      
      perf <- ggplot(FileRot, aes(x=Desempeno_Primerano))+ geom_density(adjust=2,alpha=0.2,fill="dodgerblue")+RemoveGrid+geom_vline(xintercept=Frame$Predict, size=1.5, color="red")+geom_text(aes(x=Frame$Predict, label="Predicción desempeño candidato", y=0.1), colour="black", angle=90, vjust = 1.2, text=element_text(size=11))+ scale_x_continuous(name="Desempeño colaboradores actuales")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
      
      perf
    }
    )
  }  
)
