main_plotserver <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    
    intervention_effect <- reactive(
      c(
        input$effect_1,
        input$effect_2,
        input$effect_3,
        input$effect_4,
        input$effect_5
      ) %>% as.numeric()
    )

    map(
      1:length(data()),
      .f = function(i) {
        
        output[[paste0("plot", i)]] <- renderPlotly(
          {
            
           
            
            # TODO: Isolate These
            # so they dont keep running!
            plot_data <- data()[[i]] %>%
              plot_grinder(
                group_value = input$pt_demographic
              )
            
            plot_data  %>%
              do_plot(
                difference = input$do_difference,
                effect = intervention_effect()
              )
          }
        )
        
        
      }
    )
    
    
   
    
    
    
    
    
  }
  
 
  )
  
  
  
  
}




main_dataserver <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    message("In Main Dataserver")
    
    data <- reactive(
      {
        
        data_list %>% grinder(
          intervention     = paste(input$pt_target),
          control          = paste(input$pt_control),
          allocator_vector = paste(input$pt_outcome),
          group_value      =  paste(input$pt_demographic),
          type             = input$do_incident
        )
        
      }
    )
    
    
    
    
    return(
      data
      )
    
    
  }
  )
}





main_infobox <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    
    
    req(input$pt_control)
    req(input$pt_target)
    # Server Information; #####
    #' @param data a reactive list of data.
    #'
    #' @return Rendered Text Outputs

    intervention_effect <- reactive(
      c(
        input$effect_1,
        input$effect_2,
        input$effect_3,
        input$effect_4,
        input$effect_5
      ) %>% as.numeric()
    )
    
    
    
    
    # 
    # 
    # req(input$pt_control)
    # req(input$pt_target)

    


    # Server Logic; #####
    map(
      1:length(data()),
      .f = function(i) {

        # Extract Data by Index
        # as this is consequtive
        data <- data()[[i]] %>%
          info_grinder(
            effect = intervention_effect()
          )
        
        
        
        output[[paste0("infobox",i)]] <- renderUI({
          
          
          vive_infobox(effect = intervention_effect(), data = data)
          
          
        })
        

        
        
        
        

        


      }
    ) 
    
    



  }
  )
}




main_choiceserver <- function(id, data) {
  moduleServer(id, function(input, output,session){
    
    
    # Chosen Parameters; ####
    output$chosen_target <- renderText({
      if (str_detect(input$pt_target, pattern = "[:alpha:]")) {
        
        input$pt_target %>% str_replace("_", ": ")
        
      } else {
        
        "Intet Valgt"
        
      }
      
      
    })
    
    output$chosen_control <- renderText({
      if (is.null(input$pt_control)) {
        "Intet Valgt"
        
      } else {
        
        if (length(input$pt_control) > 1) {
          
          "Matchet Gruppe"
          
        } else {
          
          input$pt_control %>% str_replace("_", ": ")
          
        }
        
        
      }
      
      
      
    })
    
    
    
    chosen_demographics <- reactive({
      input$pt_demographic
    })
    
    
    
    
    
    output$chosen_gender <- renderText({
      
      gender <- chosen_demographics()[chosen_demographics() %chin% c("Mand", "Kvinde")]
      
      if (is.null(chosen_demographics()) | length(gender) == 0) {
        "Begge Køn"
        
      } else {
        # Extract Gender;
        
        
        
        
        fifelse(
          length(gender) > 1,
          yes =  "Begge Køn",
          no = gender[1],
          na = "Begge Køn"
        )
      }
      
      
    })
    
    
    
    output$chosen_age <- renderText(
      {
        if (is.null(chosen_demographics())){
          
          "Alle Aldersgrupper"
          
        } else {
          
          age <- chosen_demographics()[chosen_demographics() %chin% c("[18-49]", "[65+]", "[50-65]")]
          
          
          fifelse(length(age) > 0 & length(age) < 3, yes =  paste(age,collapse = " og "), no = "Alle Aldersgrupper")
          
        }
        
      }
    )
    
    output$chosen_educ <- renderText(
      {
        
        educ <- chosen_demographics()[chosen_demographics() %chin% c("Faglært", "Ufaglært", "Videregående Uddannelse")]
        
        
        if (is.null(chosen_demographics()) | length(educ) == 0){
          
          "Alle uddannelser"
          
        } else {
          
          
          
          fifelse(length(educ) > 0 & length(educ) < 3, yes =  paste(educ,collapse = " og "), no = "Alle uddannelser")
          
        }
        
      }
    )
    
    
    
    output$chosen_labor <- renderText(
      {
        
        socio <- chosen_demographics()[chosen_demographics() %chin% unique(data_list$primary_care$socio)]
        
        
        if (is.null(chosen_demographics()) | length(socio) == 0){
          
          "Alle tilknytninger"
          
        } else {
          
          
          
          fifelse(length(socio) > 0 & length(socio) < 3, yes =  paste(socio,collapse = " og "), no = "Alle tilknytninger")
          
        }
        
      }
    )
    
    
  })
}
