# script: Model 1 Choiceserver
# objective: Generate server
# date: 2022-08-23
# author: Serkan Korkmaz


.model1server_choices <- function(id){
  moduleServer(id, function(input, output, session)
  {
    
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
        # no   = paste(
        #   str_extract_all(input$pt_demographic, pattern = '(?<=_).+'),
        #   collapse = ", "
        # )
        no   = paste(
          str_extract_all(demographic, pattern = '(?<=_).+'),
          collapse = ", "
        )
      )
      
      
    }
    
    
    
    
    output$chosen_gender <- renderText({
      
      internal_foo(chars[[1]]$Køn)
      
    })
    
    output$chosen_educ <- renderText({
      
      str_to_sentence(internal_foo(chars[[1]]$Uddannelse))
      
    })
    
    
    output$chosen_labor <- renderText({
      
      internal_foo(chars[[1]]$Arbejdsmarked)
      
    })
    
    output$chosen_age <- renderText({
      
      internal_foo(chars[[1]]$Alder)
      
    })
    
    
    
    output$chosen_target <- renderText({
      
      fifelse(
        str_detect(input$pt_target, '[:alpha:]'),
        yes = str_replace(input$pt_target, "_", ": "),
        no  = 'Intet valgt'
      )
      
    })
    
    output$chosen_control <- renderText({
      
      fifelse(
        str_detect(input$pt_control, '[:alpha:]'),
        yes = str_replace(input$pt_control, "_", ": "),
        no  = 'Den generelle befolkning'
      )
      
    })
    
    
  }
  
  
  )
}