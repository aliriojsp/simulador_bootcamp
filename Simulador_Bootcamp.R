library(shiny)
library(ggplot2)
library(dplyr)
install.packages('rsconnect')

rsconnect::setAccountInfo(name='workspacevdh',
                          token='AEE89BEE4A3ABFA24D2522E5E2E8CB46',
                          secret='kfzeu/ZudH15jzIJwfV8ZNfJzKGl6wWVoi3XuL60')

library(rsconnect)
rsconnect::deployApp('path/to/your/app')

File<-read.delim("C:\\Users\\VDH-DMC1NV2\\Desktop\\Diplomado Tec\\Chapt8\\Chapter8Selection.txt")%>%select(Género=Gender,Educación=EducationHighest,ExperienciaPrevia=WorkExperience, Personalidad_apertura="ACPersonalityO", Personalidad_responsabilidad="ACPersonalityC",       

write.csv(File,"G:\\Mi unidad\\Propuestas Comerciales + Pricing\\DPS People Analytics\\Bootcamp Online\\Materiales de soporte\\File.csv", row.names = FALSE)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      "¡Hola! Bienvenido/a a esta herramienta para practicar la generación de insights y predicciones con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
      h2("Crea tu gráfico:"),
      "Genera y customiza gráficos para visualizar la relación lineal entre dos variables de interés" ,
      textInput("title", "Título gráfico", "Relación entre ... y ..."),
      numericInput("size", "Tamaño del punto", 1, 1),
      checkboxInput("fit", "Agregar liñea de tendencia", FALSE),
      # Add a continent dropdown selector
      selectInput("dependent", "Variable de interés",
                  choices = colnames(File),
                  multiple = FALSE)
      ,selectInput("independent", "Conductor",
                   choices = colnames(File),
                   multiple = FALSE),
      h2("Simulador:"),
      "¿Cuál es la probabilidad que el candidato logre a tener un buen desempeño durante su primer año en la compañia? Con este simulador,podemos predecir el futuro desempeño de un candidato!",
      selectInput("Género","Género (1:Hombre, 2:Mujer)",choices=c(1,2),multiple = FALSE),
      selectInput("Educación","Educación (1:Bachelor,2:Master,3:PhD)",choices=c(1,2,3),multiple = FALSE),
      selectInput("ExperienciaPrevia","ExperienciaPrevia (1:Con, 2:Sin)",choices=c(1,2),multiple = FALSE),
      sliderInput("Personalidad_apertura","Personalidad_apertura", min=0,max=100,value=90),
      sliderInput("Personalidad_responsabilidad","Personalidad_responsabilidad", min=0,max=100,value=90),
      sliderInput("Personalidad_extroversión","Personalidad_extroversión", min=0,max=100,value=90),
      sliderInput("Personalidad_amabilidad","Personalidad_amabilidad", min=0,max=100,value=90),
      sliderInput("Competencia_Técnica","Competencia_Técnica", min=0,max=5,value=4),
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

# Define the server logic
server <- function(input, output) {
  
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
  
}

shinyApp(ui = ui, server = server)