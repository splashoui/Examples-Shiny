#Author: Mert Kucukkuru

####################################
#           Libraries              #
####################################

library(dplyr)
library(shiny)
library(ggplot2)
##install.packages("shinyWidgets") ##para cambiar el color de fondo!!
library(shinyWidgets)
##install.packages("shinydashboard")
library(shinydashboard)
library(shinyjs)
library(plotly)
## Datasets

dataset <- mpg
faith <- faithful

####################################
#           User Interface          #
####################################

ui <- fluidPage(
    
    titlePanel("Practica 1-Mert Kucukkuru"),
    setBackgroundColor(
        color = c("#F7FBFF", "#2171B5"),
        gradient = "radial",
        direction = c("top", "left")),
    
    
    sidebarLayout( 
        sidebarPanel(
          
          
            
            
            HTML("<h3>Entradas</h3>"),
                
            helpText("Pulsa para usar el dataset faithful. Por defecto se usara MPG"),
            
            checkboxInput("value",label="Usar faithful"),
            
            helpText("La opcion variable solo se usara cuando estemos usando el dataset MPG"),
            
            selectInput(inputId="channel1", label="Variable de MPG",choices = list("Displacement"="displ",
                                                                                   "Cylinders"="cyl",
                                                                                   "City Miles"="cty",
                                                                                   "Highway Miles"="hwy"),
                                                selected = "displ" , multiple= F),
            
            selectInput(inputId="color1",label="Eliga un color",choices = c("Naranja"="Naranja",
                                                                            "Violeta"="Violeta",
                                                                            "Turquesa"="Turquesa"),
                        selected = "Violeta",multiple = F),
            
            
            sliderInput(inputId = "bins1xz",
                        label = "Numero de Bins:",
                        min = 1,
                        max = 50,
                        value = 15)
            
         ),
        
        
       

        mainPanel(
          uiOutput('t1'),
            
            plotlyOutput('distPlot')
        )
    )
)


####################################
#           Server Logic            #
####################################
server <- function(input, output) {
  
  # observe({
  #   if (input$value==TRUE) {
  #     disable("channel1")
  #   } else {
  #     enable("channel1")
  #   }
  # })     #### Aqui , he intentado desactivar el selectinput de MPG , cuando usamos el dataset Faithful,
            ###pero no he podido, despues busco mas sobre eso.
    

  
  
    output$t1 <- renderUI({
    
    if (input$value==TRUE){
      
      #HTML('<strong> <em> <h3> Ahora utilizando el dataset FAITHFUL! ')
      tags$strong()
    }
    
    else {   
      
      HTML( "<strong> <em> <h3> Estas utilizando el dataset 'MPG' de forma predeterminada.Para usar el dataset 'FAITHFUL', 
        haga clic en la casilla de verificacion 'Usar Faithful'!")
    }
  })
  
    
  
  
    output$distPlot <- renderPlotly({
        
      if(input$color1=="Naranja"){
        sColor = "#ff6461"
      }else if(input$color1=="Violeta"){
        sColor = "#58508d"
      }else if(input$color1=="Turquesa"){
        sColor = "#49eaf2"
      }
      
       
    p2 <- faith %>% ggplot()
    if (input$value==TRUE){
      
      
      
                p2 <- p2 + geom_histogram(aes(x=waiting),bins = input$bins1xz,colour="orangered2", fill=sColor) + 
                labs(x="Waiting Time ",y="Count",title=paste("Histogram of Waiting time ",sep = " "),
                subtitle = "Time between eruptions and the duration of the eruption for the Old Faithful geyser in USA.") +  
                theme_bw()+
                theme(axis.title = element_text(size=12,color="orangered2",face="bold"),
                      axis.text = element_text(size=14,color="orangered2",face="bold"))
        }    
        
    else {   
        
      
    p2 <- dataset %>% ggplot()
        if(input$channel1 == "displ"){
        p2 <- p2 + geom_histogram(aes(x=displ),bins = input$bins1xz,colour="royalblue4", fill=sColor) + 
            labs(x="Engine displacement ",y="Count",title="Histogram of Engine displacement(in litres)") +  
            theme_bw()+
            theme(axis.title = element_text(size=12,color="royalblue4",face="bold"),
                  axis.text = element_text(size=14,color="royalblue4",face="bold"))
        
    }else if(input$channel1 == "cyl"){
        p2 <- p2 + geom_histogram(aes(x=cyl),bins = input$bins1xz,colour="seagreen4", fill=sColor) + 
            labs(x="Number of cylinders ",y="Count",title="Histogram of Number of cylinders") +  
            theme_bw()+
            theme(axis.title = element_text(size=12,color="seagreen4",face="bold"),
                  axis.text = element_text(size=14,color="seagreen4",face="bold"))
    }else if(input$channel1 == "cty"){
        p2 <- p2 + geom_histogram(aes(x=cty),bins = input$bins1xz,colour="orange3", fill=sColor) + 
            labs(x="City miles per gallon ",y="Count",title="Histogram of City miles per gallon") +  
            theme_bw()+
            theme(axis.title = element_text(size=12,color="orange3",face="bold"),
                  axis.text = element_text(size=14,color="orange3",face="bold"))
    }else if(input$channel1 == "hwy"){
        p2 <- p2 + geom_histogram(aes(x=hwy),bins = input$bins1xz,colour="orchid4", fill=sColor) + 
            labs(x="Highway miles per gallon ",y="Count",title="Histogram of Highway miles per gallon") +  
            theme_bw()+
            theme(axis.title = element_text(size=12,color="orchid4",face="bold"),
                  axis.text = element_text(size=14,color="orchid4",face="bold"))
    }   
      
    }
    
    p2 <- ggplotly(p2)
    p2
     
})
    
   
}
    

####################################
#           Run the App         #
####################################
shinyApp(ui = ui, server = server)
