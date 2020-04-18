library(shiny)

library(shiny)
library(ggplot2)
library(dplyr)

File <- read.csv("https://raw.githubusercontent.com/aliriojsp/simulador_bootcamp/master/File.csv")

ui <- fluidPage(
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
        
        Genero<- c(input$Genero)
        Educacion<-c(input$Educacion)
        ExperienciaPrevia<- c(input$ExperienciaPrevia)
        Personalidad_apertura<-c(input$Personalidad_apertura)
        Personalidad_responsabilidad<-c(input$Personalidad_responsabilidad)
        Personalidad_extroversion<-c(input$Personalidad_extroversion)
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
        
        perf <- ggplot(File, aes(x=Desempeno_Primerano))+ geom_density(adjust=2,alpha=0.2,fill="dodgerblue")+RemoveGrid+geom_vline(xintercept=Frame$Predic, size=1.5, color="red")+geom_text(aes(x=Frame$Predic, label="PredicciÃ³n desempeÃ±o candidato", y=0.1), colour="black", angle=90, vjust = 1.2, text=element_text(size=11))+ scale_x_continuous(name="DesempeÃ±o colaboradores actuales")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
        
        perf
        
    })
    
}

shinyApp(ui = ui, server = server)
