second_dataserver <- function(id, data_list){
  moduleServer(id, function(input,output,session){
    
    # generate data
    data <- reactive({
      
      data_list %>% grinder(
        intervention = input$pt_target,
        control = input$pt_control,
        chars = input$pt_demographic,
        allocators = input$pt_outcome
      ) %>% foo()
      
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
        baz1(
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

        final_plot %>% subplot()

      }


    )
    
    
    
 
    
    
    
    
  }
  
  
  )

  
}