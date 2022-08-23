# script: Model 1 Dataserver
# objective: Create Data server
# date: 2022-08-23
# author: Serkan Korkmaz


.model1server_data <- function(
    id
) {
  moduleServer(
    id, function(input, output,session){
      
      # Server Start
    
      data <- reactive(
        {
          
          req(input$pt_target)
          req(input$pt_outcome)
          
          message('Grinding data for model1:\n')
          
          grinded_data <- grind(
            data_list = data_list[[1]],
            intervention     = paste(input$pt_target),
            control          = paste(input$pt_control),
            allocators       = paste(input$pt_outcome),
            chars            = paste(input$pt_demographic),
            do_incidence = input$do_incident
          )
          
          
          spread(
            data_list = grinded_data,
            
            # Should the data be in qty, or cost.
            alternate = input$do_cost
            
          )
          
          
        }
      )
      
      
      return(
        data()
      )
      
      # Server End
  }
  )
} 