second_dataserver <- function(id, data_list){
  moduleServer(id, function(input,output,session){
    

    
    # generate data
    data <- reactive({
      
      data_list %>% grind(
        intervention = paste(input$pt_target),
        control = paste(input$pt_control),
        chars = paste(input$pt_demographic),
        allocators = paste(input$pt_outcome)
      ) %>% spread()
      
      
    })
    
    
    
    return(data)
    
    
    
    
  })
}




.model2server_plot <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    
    data <- reactive({
      
      data_list %>% grind(
        intervention = paste(input$pt_target),
        control = paste(input$pt_control),
        chars = paste(input$pt_demographic),
        allocators = paste(input$pt_outcome)
      ) %>% spread()
      
      
    })
    
    
    
    # plot
    plot_data <- reactive({
      
      
      
      data() %>% flavor(effect = 50) %>%
        baselayer() %>%
        baseplot() %>% 
        effectlayer()
      
      
      
      
      
    })


    output$plot <- renderPlotly(
      {

        plot_data()[[1]] %>% 
          subplot()

      }


    )
    
    
    
 
    
    
    
    
  }
  
  
  )

  
}