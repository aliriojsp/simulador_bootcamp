library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(flexdashboard)
library(fastDummies)
library(lubridate)
library(DT)

FileRot <- read.csv("https://raw.githubusercontent.com/aliriojsp/simulador_bootcamp/master/File.csv",fileEncoding="UTF-8-BOM")
FileRot <- data.frame(FileRot)
FileRot <- FileRot %>% mutate(Desempeno_Primerano = as.numeric(Desempeno_Primerano),
                              Desempeno_Primerano = if_else(is.na(Desempeno_Primerano), 3, Desempeno_Primerano))
results <- fastDummies::dummy_cols(FileRot, select_columns = "Educacion")
FileRot <- results
FileRot <- data.frame(FileRot)
FileRot<-FileRot%>%rename(GÃ©nero=Genero,"Experiencia Previa"="ExperienciaPrevia","EducaciÃ³n 1"="Educacion_1" ,"EducaciÃ³n 2"="Educacion_2","EducaciÃ³n 3"="Educacion_3","Personalidad: Apertura"="Personalidad_apertura","Personalidad: Responsabilidad"="Personalidad_responsabilidad",
                          "Personalidad: Extroversion"="Personalidad_extroversion","Personalidad: Amabilidad"="Personalidad_amabilidad",
                          "Personalidad: Neuroticismo"="Personalidad_neuroticismo","Competencia: TÃ©cnica"="Competencia_Tecnica",
                          "Competencia: Teamplayer"="Competencia_Teamplayer","Competencia: Pensamiento critico"="Competencia_PensamientoCritico",
                          "Competencia: Negocio"="Competencia_Negocio","Competencia: InnovaciÃ³n y motivaciÃ³n"="Competencia_InnovacionYmotivacion","EducaciÃ³n"="Educacion","DesempeÃ±o Primer AÃ±o"="Desempeno_Primerano")

shinyApp(
    ui <- navbarPage(tags$img(src = "Bondi_X_Negativo-Patrones-07.png"),

                     tabPanel("Insights", fluidPage(theme = shinytheme("flatly"),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            "Â¡Hola! Bienvenido/a al toolkit analitico para selecciÃ³n y reclutamiento. Esta aplicaciÃ³n es una demo para poder observar el alcance de esta herramienta analitica. SegÃºn las necesidades del cliente, Bondi X puede customizar esta herramienta. En esta solapa se puede visualizar como un conductor estÃ¡ relacionado con el variable de interÃ©s. En este caso, las caracteristicas de un colaborador exitoso (o alto desempeÃ±o) en un rol especifico.",
                                                            h2("Crea tu grÃ¡fico:"),
                                                            "Genera y customiza grÃ¡ficos para visualizar la relaciÃ³n entre el desempeÃ±o de los colaboradores y un conductor especifico." ,
                                                            selectInput("driver", "Conductor",
                                                                        choices = c("GÃ©nero","Experiencia Previa","EducaciÃ³n 1","EducaciÃ³n 2","EducaciÃ³n 3","Personalidad: Apertura","Personalidad: Responsabilidad",
                                                                                    "Personalidad: Extroversion","Personalidad: Amabilidad",
                                                                                    "Personalidad: Neuroticismo","Competencia: TÃ©cnica",
                                                                                    "Competencia: Teamplayer","Competencia: Pensamiento critico",
                                                                                    "Competencia: Negocio","Competencia: InnovaciÃ³n y motivaciÃ³n"),
                                                                        multiple = FALSE)),
                                                        mainPanel(h2("Insight"),plotOutput("plot", height = 700)
                                                        )
                                                    )
                     )
                     ),
                     tabPanel("Simulador", fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 "Â¡Hola! Bienvenido/a a esta herramienta para practicar la generaciÃ³n de predicciones con datos de RRHH. El objetivo es simular algunas dinamicas claves durante la fase de analisis en un proyecto de people analytics.",
                                 h2("Simulador:"),
                                 "Con este simulador,podemos predecir la probabilidad de que un colaborador renuncie",
                                 selectInput("EducaciÃ³n","EducaciÃ³n (1:Universidad 1,2:Universidad 2,3: Universidad 3)",choices=c(1,2,3),multiple = FALSE),
                                 selectInput("Experiencia Previa","Experiencia Previa (1:Con, 2:Sin)",choices=c(1,2),multiple = FALSE),
                                 sliderInput("Apertura","Personalidad: Apertura", min=0,max=100,value=90),
                                 sliderInput("Responsabilidad","Personalidad: Responsabilidad", min=0,max=100,value=90),
                                 sliderInput("Extroversion","Personalidad: Extroversion", min=0,max=100,value=90),
                                 sliderInput("Amabilidad","Personalidad: Amabilidad", min=0,max=100,value=90),
                                 sliderInput("Neuroticismo","Personalidad: Neuroticismo", min=0,max=100,value=90),
                                 sliderInput("TÃ©cnica","Competencia: Tecnica", min=0,max=5,value=4),
                                 sliderInput("Teamplayer","Competencia: Teamplayer", min=0,max=5,value=4),
                                 sliderInput("critico","Competencia: PensamientoCritico", min=0,max=5,value=4),
                                 sliderInput("Negocio","Competencia: Negocio", min=0,max=5,value=4),
                                 sliderInput("InnovaciÃ³n","Competencia: Innovacion y motivacion", min=0,max=5,value=4)
                             ),
                             mainPanel(h2("Candidato ideal"),plotOutput("predictplot")
                             )
                         )
                     )
                     ),
                     tabPanel("PredicciÃ³n", fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 h5("En estÃ¡ secciÃ³n del toolkit se puede revisar y descargar la base de datos con la predicciÃ³n para cada candidato"),
                                 downloadButton("downloadData", "Descargar")),
                                 mainPanel(h2("DistribuciÃ³n candidatos"),plotOutput("Predictbarra"),h2("PredicciÃ³n potencial de candidatos"),dataTableOutput("PredictDatabase")
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
            p <- FileRot%>%ggplot(aes(y=`DesempeÃ±o Primer AÃ±o`, x=get(input$driver)))+geom_smooth(method="lm",se=0)+RemoveGrid+scale_y_continuous(name="DesempeÃ±o primer aÃ±o", limits=c(1,5))+scale_x_continuous(name=input$driver)+theme(
                axis.title.x = element_text(size = 16,face="bold"),axis.text.x = element_text(size = 14),axis.title.y = element_text(size = 16,face="bold"),axis.text.y = element_text(size = 14))+geom_point()
            p
            
        })
        
        
        
        output$predictplot <- renderPlot({
            
            # Subset the gapminder dataset by the chosen continents
            RemoveGrid<- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
            
            
            # Genero<- c(input$Genero)
            EducaciÃ³n<-c(input$EducaciÃ³n)
            "Experiencia Previa"<- c(input$`Experiencia Previa`)
            "Personalidad: Apertura"<-c(input$Apertura)
            "Personalidad: Responsabilidad"<-c(input$Responsabilidad)
            "Personalidad: Extroversion"<-c(input$Extroversion)
            "Personalidad: Amabilidad"<-c(input$Amabilidad)
            "Personalidad: Neuroticismo"<-c(input$Neuroticismo)
            "Competencia: TÃ©cnica"<-c(input$TÃ©cnica)
            "Competencia: Teamplayer"<-c(input$Teamplayer)
            "Competencia: Pensamiento critico"<-c(input$critico)
            "Competencia: Negocio"<-c(input$Negocio)
            "Competencia: InnovaciÃ³n y motivaciÃ³n"<-c(input$InnovaciÃ³n)
            
            Frame<- data.frame("EducaciÃ³n","Experiencia Previa","Personalidad: Neuroticismo","Personalidad: Apertura","Personalidad: Responsabilidad","Personalidad: Extroversion","Personalidad: Amabilidad","Competencia: TÃ©cnica","Competencia: Teamplayer","Competencia: Pensamiento critico","Competencia: Negocio","Competencia: InnovaciÃ³n y motivaciÃ³n")
            Modelo<-lm(`DesempeÃ±o Primer AÃ±o`~as.factor(EducaciÃ³n)+as.factor(`Experiencia Previa`)+`Personalidad: Neuroticismo`+`Personalidad: Apertura`+`Personalidad: Responsabilidad`+`Personalidad: Extroversion`+`Personalidad: Amabilidad`+`Competencia: TÃ©cnica`+
                            `Competencia: Teamplayer`+`Competencia: Pensamiento critico`+`Competencia: Negocio`+`Competencia: InnovaciÃ³n y motivaciÃ³n`, data=FileRot)
            Frame$Predict<-predict(Modelo,Frame) 
            
            perf <- ggplot(FileRot, aes(x=`DesempeÃ±o Primer AÃ±o`))+ geom_density(adjust=2,alpha=0.2,fill="dodgerblue")+RemoveGrid+geom_vline(xintercept=Frame$Predict, size=1.5, color="red")+geom_text(aes(x=Frame$Predict, label="PredicciÃ³n desempeÃ±o candidato", y=0.1), colour="black", angle=90, vjust = 1.2, text=element_text(size=11))+ scale_x_continuous(name="DesempeÃ±o colaboradores actuales")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
            
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
            
            Modelo<-lm(`DesempeÃ±o Primer AÃ±o`~as.factor(EducaciÃ³n)+as.factor(`Experiencia Previa`)+`Personalidad: Neuroticismo`+`Personalidad: Apertura`+`Personalidad: Responsabilidad`+`Personalidad: Extroversion`+`Personalidad: Amabilidad`+`Competencia: TÃ©cnica`+
                           `Competencia: Teamplayer`+`Competencia: Pensamiento critico`+`Competencia: Negocio`+`Competencia: InnovaciÃ³n y motivaciÃ³n`, data=FileRot)
            FileRot$Predict<-round(predict(Modelo),2)
            c <- cut(FileRot$Predict,breaks=c(0,2,4,5))
            FileRot$Potencial[FileRot$Predict < 2] <- "BAJA"
            FileRot$Potencial[FileRot$Predict > 2 & FileRot$Predict < 4] <- "MEDIA"
            FileRot$Potencial[FileRot$Predict >= 4] <- "ALTA"
            FileRot$Potencial<-as.factor(FileRot$Potencial)
            FileRot$Potencial<-factor(FileRot$Potencial, levels = c("BAJA", "MEDIA", "ALTA"))
            FileRot<-FileRot%>%select("Candidato","Fecha","Potencial","Predict","GÃ©nero","EducaciÃ³n","Experiencia Previa","Personalidad: Apertura","Personalidad: Responsabilidad","Personalidad: Extroversion","Personalidad: Amabilidad",            
                                      "Personalidad: Neuroticismo","Competencia: TÃ©cnica","Competencia: Teamplayer","Competencia: Pensamiento critico","Competencia: Negocio","Competencia: InnovaciÃ³n y motivaciÃ³n","DesempeÃ±o Primer AÃ±o",               
                                      "EducaciÃ³n 1","EducaciÃ³n 2","EducaciÃ³n 3")
            
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
