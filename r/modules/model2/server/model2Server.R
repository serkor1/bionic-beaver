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



.model2server_choices <- function(id) {
  moduleServer(id, function(input,output, session) {
    
    #' Serverinformation
    #' 
    #' This server renders textoutput for chosen 
    #' parameters in the model
    
    internal_foo <- function(demographic) {
      
      #' function information
      #' 
      #' @param demographics a vector of demographic variables
      
      get_len <- length(demographic)
      
      indicator <- demographic %chin% input$pt_demographic
      
      demographic <- demographic[indicator]
      
      fifelse(
        test = length(demographic) == 0 | length(demographic) == get_len,
        yes  = 'Alle valgt',
        no   = paste(
          str_extract_all(input$pt_demographic, pattern = '(?<=_).+'),
          collapse = ", "
        )
      )
      
      
    }
    
    
    # Chosen Parameters
    
    output$chosen_educ <- renderText({
      
      internal_foo(chars[[2]]$feduc)
      
    })
    
    output$chosen_type <- renderText({
      
      internal_foo(chars[[2]]$ftype)
      
    })
    
    
    
    
    # Chosen Groups;
    
    output$chosen_control <- renderText({
      
      fifelse(
        str_detect(input$pt_control, '[:alpha:]'),
        yes = str_replace(input$pt_control, "_", ": "),
        no  = 'Intet valgt'
      )
      
      
    })
    
    
    output$chosen_intervention <- renderText({
      
      fifelse(
        str_detect(input$pt_target, '[:alpha:]'),
        yes = str_replace(input$pt_target, "_", ": "),
        no  = 'Intet valgt'
      )
      
      
    })
    
    
    
    
    
    
  }
  )
} 



.model2server_plot <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    
    
    data <- reactive({
      
      req(input$pt_target)
      req(input$pt_control)
      req(input$pt_outcome)
      
      
      message(
        'Grinding data for model2'
      )
      
      
      message(
        paste(
          'With Parameters:\n',
          'Intervention:', paste(input$pt_target), '\n',
          'Control:',paste(input$pt_control),'\n',
          'Outcomes:',paste(input$pt_outcome, collapse = ","),'\n',
          'Characteristics:', paste(input$pt_demographic, collapse = ",")
        )
      )
      
      
      
      data_list %>% grind(
        intervention = paste(input$pt_target),
        control = paste(input$pt_control),
        chars = paste(input$pt_demographic),
        allocators = paste(input$pt_outcome)
      ) %>% spread()
      
      
    })
    
    
    
    # plot
    baseline_plot <- reactive({
      
      message('Plotting Data')
      
      data() %>% 
        flavor(effect = input$effect) %>%
        baselayer() %>%
        baseplot() %>% 
        effectlayer()
      
      
      
      
      
    })
    
    
    plot_data <- reactive(
      {
        
        baseline_plot() %>%
          plot_layout(
            intervention_color = input$col_intervention,
            control_color = input$col_control,
            background_color = input$col_background
          )
        
      }
    )


    output$plot <- renderPlotly(
      {
        
        # Add valdidation
        validate(
          need(
            input$pt_target,
            message = 'Vælg en målgruppe'
          ),
          need(
            input$pt_control,
            message = 'Vælg en sammenligningsgruppe'
          ),
          need(
            input$pt_outcome,
            message = 'Vælg outcome(s)'
          )
        )

        plot_data()[[1]] %>% 
          subplot()

      }


    )
    
    
    
 
    
    
    
    
  }
  
  
  )

  
}