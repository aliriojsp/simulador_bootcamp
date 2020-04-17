library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) { 
  
  File <- reactive({File<-read.csv("https://github.com/aliriojsp/simulador_bootcamp/blob/master/File.csv")%>%select(Genero=Gender,Educacion=EducationHighest,ExperienciaPrevia=WorkExperience, Personalidad_apertura="ACPersonalityO", Personalidad_responsabilidad="ACPersonalityC",
  Personalidad_extroversion="ACPersonalityE", Personalidad_amabilidad= "ACPersonalityA", Personalidad_neuroticismo="ACPersonalityN",Competencia_Tecnica="ACRatingINTCOMPA", Competencia_Teamplayer="ACRatingINTCOMPB",Competencia_PensamientoCritico="ACRatingINTCOMPC",Competencia_Negocio="ACRatingINTCOMPD",Competencia_InnovacionYmotivacion="ACRatingINTCOMPE", Desempeno_Primerano="Year1performanceRating")
  })
    
  output$plot <- renderPlot({
    
    # Subset the gapminder dataset by the chosen continents
    RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    p <- ggplot(File, aes(y=File[,input$dependent], x= File[,input$independent])) +
      geom_point(size = input$size, col = "blue") +
      ggtitle(input$title)+RemoveGrid+scale_x_continuous(name=input$independent)+scale_y_continuous(name=input$dependent)+theme(plot.title=element_text(size=14,face="bold"))
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm", se=0)
    }
    p
    
  })
  
  output$Predictplot <- renderPlot({
    
    # Subset the gapminder dataset by the chosen continents
    RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    Genero<- c(input$Genero)
    Educacion<-c(input$Educacion)
    ExperienciaPrevia<- c(input$ExperienciaPrevia)
    Personalidad_apertura<-c(input$Personalidad_apertura)
    Personalidad_responsabilidad<-c(input$Personalidad_responsabilidad)
    Personalidad_extroversión<-c(input$Personalidad_extroversión)
    Personalidad_amabilidad<-c(input$Personalidad_amabilidad)
    Competencia_Tecnica<-c(input$Competencia_Tecnica)
    Competencia_Teamplayer<-c(input$Competencia_Teamplayer)
    Competencia_PensamientoCritico<-c(input$Competencia_PensamientoCritico)
    Competencia_Negocio<-c(input$Competencia_Negocio)
    Competencia_InnovacionYmotivacion<-c(input$Competencia_InnovacionYmotivacion)
    Frame<- data.frame(Genero,Educacion,ExperienciaPrevia,Personalidad_apertura,Personalidad_responsabilidad,Personalidad_extroversion,Personalidad_amabilidad,Competencia_Tecnica,Competencia_Teamplayer,Competencia_PensamientoCritico,Competencia_Negocio,Competencia_InnovacionYmotivacion)
    Modelo<-lm(File$Desempeno_Primerano~as.numeric(Genero)+as.numeric(Educacion)+as.numeric(ExperienciaPrevia)+Personalidad_apertura+Personalidad_responsabilidad+Personalidad_extroversion+Personalidad_amabilidad+Competencia_Tecnica+
                 Competencia_Teamplayer+Competencia_PensamientoCritico+Competencia_Negocio+Competencia_InnovacionYmotivacion,data=File)
    Frame$Predict<-predict(Modelo,Frame)
    
    perf <- ggplot(File, aes(x=Desempeno_Primerano))+ geom_density(adjust=2,alpha=0.2,fill="dodgerblue")+RemoveGrid+geom_vline(xintercept=Frame$Predic, size=1.5, color="red")+geom_text(aes(x=Frame$Predic, label="Predicción desempeño candidato", y=0.1), colour="black", angle=90, vjust = 1.2, text=element_text(size=11))+ scale_x_continuous(name="Desempeño colaboradores actuales")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
    
    perf
    
  })
  
})
