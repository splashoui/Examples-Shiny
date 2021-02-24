library(shiny)
library(shinythemes)




####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Slope of a line:",
                           
                           tabPanel("Slope Calculator",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input Coordinates</h3>"),
                                      sliderInput("x1", 
                                                  label = "x1", 
                                                  value = 5, 
                                                  min = -50, 
                                                  max = 50),
                                      sliderInput("y1", 
                                                  label = "y1", 
                                                  value = 5, 
                                                  min = -50, 
                                                  max = 50),
                                      sliderInput("x2", 
                                                  label = "x2", 
                                                  value = 5, 
                                                  min = -50, 
                                                  max = 50),
                                      sliderInput("y2", 
                                                  label = "y2", 
                                                  value = 5, 
                                                  min = -50, 
                                                  max = 50),
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata') # Results table
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           
                           tabPanel("About", 
                                    titlePanel("About"), 
                                    div(includeMarkdown("about.md"), 
                                        align="justify")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    m <- (input$y2 - input$y1) / (input$x2 - input$x1)
    m <- data.frame(m)
    names(m) <- "SLOPE"
    print(m)
    
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
