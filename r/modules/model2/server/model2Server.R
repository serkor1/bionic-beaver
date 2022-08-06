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




second_plotserver <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    
    # TODO: Maybe BAZ should be moved inside
    # the effect layer
    plot_data <- reactive(
      data %>%
        flavor(
          effect = 50
        )
    )


    # Generate Plots
    plot_list <- {

      message("Generating Plots")

      plot_data() %>%
        baselayer() %>%
        baseplot()

    }


    final_plot <- plot_list %>%
      effectlayer()


    output$plot <- renderPlotly(
      {

        final_plot %>% 
          subplot()

      }


    )
    
    
    
 
    
    
    
    
  }
  
  
  )

  
}