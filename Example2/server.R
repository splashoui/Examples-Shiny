####################################
#         Shiny Server              #
####################################

shinyServer(function(input, output,session) {

####################################
#             GRAPH                #
####################################  
  
  filteredDate <- reactive({
    
      sunspot <- data.frame(read.csv('sunspots.csv'))
      
      sunspot$Month <- as.Date(sunspot$Month) 
    
      req(input$dateRange)     #Check
        
      filter(sunspot, between(Month, as.Date(input$dateRange[1]), as.Date(input$dateRange[2])))

  })
  
  
  output$plot <- renderPlotly({
    
    input$act
    
    isolate (gr <- ggplot(filteredDate(), 
                          aes(x= Month)) +
                          geom_line(aes(y=Sunspots)) + 
                          labs(title="Cambio Anual de Manchas Solares", 
                          y="Sunspots",
                          x="Year"))
      
    gr <- ggplotly(gr)
    gr
    
    })
    
  

  ####################################
  #             TABLE                 #
  ####################################
  
  filtered_df = reactive({
    
    biostats <- data.frame(read.csv(file = 'biostats.csv'))
    
    res <- biostats %>% filter(Sex== input$s1) 
    
    res <- res %>% filter(Age > input$s2) 
    
    res <- res %>% filter(Height..in. > input$s3) 
    
    res 
    
    })
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(filtered_df(),
                  colnames=c("Nombre", "Sexo" ,"Edad" , "Altura", "Peso"),
                  selection="single",rownames = T)

    })
  
  
####################################
#             INFO                  #
#################################### 

  val <- reactiveValues()
  
  observeEvent(input$dt1,{ 
    
       val$tx <- "Biostats.csv:
                  Una tabla que contiene nombres, sexo,
                  edad, altura y peso de 18 personas."
    
    })
  
  observeEvent(input$dt2,{ 
    
    val$tx <-  "Sunspots.csv:
                Representa la variacion de
                las manchas solares desde 1749 hasta 1983."
    
  })
  
  
  output$tx <- renderText({
    
    val$tx

    
  })

  })

  
  
 

