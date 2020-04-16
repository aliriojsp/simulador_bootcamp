library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) {

File<-read.csv("https://raw.githubusercontent.com/aliriojsp/simulador_bootcamp/master/File.csv?token=AHWPS7MLS3FOT5NXZOQD7226TCMQA")%>%select(Género=Gender,Educación=EducationHighest,ExperienciaPrevia=WorkExperience, Personalidad_apertura="ACPersonalityO", Personalidad_responsabilidad="ACPersonalityC",
Personalidad_extroversión="ACPersonalityE", Personalidad_amabilidad= "ACPersonalityA", Personalidad_neuroticismo="ACPersonalityN",Competencia_Técnica="ACRatingINTCOMPA", Competencia_Teamplayer="ACRatingINTCOMPB",Competencia_PensamientoCritico="ACRatingINTCOMPC",Competencia_Negocio="ACRatingINTCOMPD",Competencia_InnovacionYmotivacion="ACRatingINTCOMPE", Desempeño_Primeraño="Year1performanceRating")
  
  
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
    
    Género<- c(input$Género)
    Educación<-c(input$Educación)
    ExperienciaPrevia<- c(input$ExperienciaPrevia)
    Personalidad_apertura<-c(input$Personalidad_apertura)
    Personalidad_responsabilidad<-c(input$Personalidad_responsabilidad)
    Personalidad_extroversión<-c(input$Personalidad_extroversión)
    Personalidad_amabilidad<-c(input$Personalidad_amabilidad)
    Competencia_Técnica<-c(input$Competencia_Técnica)
    Competencia_Teamplayer<-c(input$Competencia_Teamplayer)
    Competencia_PensamientoCritico<-c(input$Competencia_PensamientoCritico)
    Competencia_Negocio<-c(input$Competencia_Negocio)
    Competencia_InnovacionYmotivacion<-c(input$Competencia_InnovacionYmotivacion)
    Frame<- data.frame(Género,Educación,ExperienciaPrevia,Personalidad_apertura,Personalidad_responsabilidad,Personalidad_extroversión,Personalidad_amabilidad,Competencia_Técnica,Competencia_Teamplayer,Competencia_PensamientoCritico,Competencia_Negocio,Competencia_InnovacionYmotivacion)
    Modelo<-lm(File$Desempeño_Primeraño~as.numeric(Género)+as.numeric(Educación)+as.numeric(ExperienciaPrevia)+Personalidad_apertura+Personalidad_responsabilidad+Personalidad_extroversión+Personalidad_amabilidad+Competencia_Técnica+
                 Competencia_Teamplayer+Competencia_PensamientoCritico+Competencia_Negocio+Competencia_InnovacionYmotivacion,data=File)
    Frame$Predict<-predict(Modelo,Frame)
    
    perf <- ggplot(File, aes(x=Desempeño_Primeraño))+ geom_density(adjust=2,alpha=0.2,fill="dodgerblue")+RemoveGrid+geom_vline(xintercept=Frame$Predic, size=1.5, color="red")+geom_text(aes(x=Frame$Predic, label="Predicción desempeño candidato", y=0.1), colour="black", angle=90, vjust = 1.2, text=element_text(size=11))+ scale_x_continuous(name="Desempeño colaboradores actuales")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
    
    perf
    
  })
  
})
