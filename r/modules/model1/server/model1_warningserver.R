# script: Model1 Warning Server
# objective: Generate Warnings to avoid errors.
# date: 2022-08-23
# author: Serkan Korkmaz

.model1server_warnings <- function(id){
  moduleServer(id, function(input, output, session)
  {
    
    
    observe(
      {
        
        if (isTruthy(input$pt_control) | isTruthy(input$pt_target) ) {
          
          shinyFeedback::feedbackDanger(
            inputId = 'pt_control',
            input$pt_control == input$pt_target,
            text = 'Skift gruppe!',
            icon = NULL
          )
        }
        
        
      }
    )
    
    
    
    
    # observe({
    #   invalidateLater(1000)
    # 
    #   shinyFeedback::feedbackWarning(
    #     inputId = "pt_target",
    #     !isTruthy(input$pt_target) | !str_detect(input$pt_target, '[:alpha:]'),
    #     text = "Vælg Gruppe!",icon = NULL
    #   )
    # 
    #   shinyFeedback::feedbackWarning(
    #     inputId = "pt_control",
    #     !isTruthy(input$pt_control),
    #     text = "Vælg Sammenligningsgruppe!",icon = NULL
    #   )
    # 
    #   shinyFeedback::feedbackWarning(
    #     inputId = "pt_outcome",
    #     !isTruthy(input$pt_outcome),
    #     text = "Vælg Outcome(s)!",icon = NULL
    #   )
    # 
    # 
    # 
    #   
    # 
    # 
    #   if (!isTruthy(input$pt_outcome) & !isTruthy(input$pt_target)){
    #     createAlert(
    #       id = "myalert",
    #       options = list(
    #         title = "Information",
    #         closable = TRUE,
    #         width = 12,
    #         elevations = 4,
    #         status = "primary",
    #         content = "Venter på input..."
    #       )
    #     )
    #   } else {
    #     closeAlert(id = "myalert")
    #   }
    # 
    # })
    
    
    
    
    
    
    
    
    
  }
  
  
  )
}
