####################################
#       Libraries                 #
####################################

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)


####################################
#         DataSets                #
####################################

sunspot <- data.frame(read.csv('sunspots.csv'))
biostats <- data.frame(read.csv(file = 'biostats.csv'))

####################################
#       User Interface              #
####################################
shinyUI(navbarPage(theme= shinytheme("journal") , title = "Practica 2" , id = "navbarPage" , 
                   
                   sidebarLayout(
                       sidebarPanel(
                            conditionalPanel(condition="input.tabs==1",
                                            dateRangeInput("dateRange", 
                                                           label = h5("Fechas"),
                                                           min = min(sunspot$Month),
                                                           max = max(sunspot$Month),
                                                           start = min(sunspot$Month),
                                                           end = max(sunspot$Month),
                                                           format = "dd/mm/yyyy"),
                                            
                                            actionButton(inputId= "act","Actualizar",
                                                         icon = icon("refresh"))
                                            ),
                           
                           conditionalPanel(condition="input.tabs==2",
                                            selectInput('s1',
                                                        'Sexo',
                                                        choices= c(unique(biostats$Sex)), 
                                                        selected = 'M'),
                                            
                                            numericInput('s2','Edad', 
                                                         min = min(biostats$Age) , 
                                                         max = max(biostats$Age)
                                                         , value = trunc(median(biostats$Age))),
                                            
                                            numericInput('s3','Altura', 
                                                         min = min(biostats$Height..in.),
                                                         max = max(biostats$Height..in.), 
                                                         value = trunc(median(biostats$Height..in.)))
                                            ),
                           
                           conditionalPanel(condition="input.tabs==3",
                                            helpText("Seleccione el conjunto de datos sobre el que desea obtener informacion."),
                                            actionButton("dt1","Biostats",icon = icon("copy"),
                                                         style="color: #eb4f34;
                                                         background-color: #FFFFFF; 
                                                         border-color: #2e6da4"),
                                            actionButton("dt2","Sunspots",icon = icon("copy"), 
                                                         style="color: #eb4f34;
                                                         background-color: #FFFFFF; 
                                                         border-color: #2e6da4"))
                       ),
                       

                       mainPanel(
                           tabsetPanel(type='tabs', id = 'tabs',
                                       tabPanel(value=1,"Grafica", plotlyOutput("plot"),icon = icon("chart-line")), 
                                       tabPanel(value=2,"Tabla", DT::dataTableOutput("table"),icon = icon("table")),
                                       tabPanel(value=3,"Informacion", textOutput("tx"),icon = icon("info")))
                                                
                        )
                       )
                      )
                     )
