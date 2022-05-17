main_warnings <- function(id){
  moduleServer(id, function(input, output, session)
  {
    
      # Check wether user chose
      # needed inputs and give feedback
      message(paste("Chosen:",input$pt_control, "Truthy:", isTruthy(input$pt_control)))
      
    
    observe(
      {
        if (isTruthy(input$pt_target) & isTruthy(input$pt_control)) {
          
          if (input$pt_target == input$pt_control) {
            
            shinyFeedback::feedbackDanger(
              inputId = "pt_control",
              TRUE,
              text = "Skift gruppe!",icon = NULL
            )
            
          }
          
          
        }
      }
    )
    
   
    
        
        shinyFeedback::feedbackWarning(
          inputId = "pt_control",
          !isTruthy(input$pt_control),
          text = "Vælg Sammenligningsgruppe!",icon = NULL
        )            
        
        
        shinyFeedback::feedbackWarning(
          inputId = "pt_target",
          !isTruthy(input$pt_target),
          text = "Vælg Gruppe!",icon = NULL
        )   
        
        
        
        shinyFeedback::feedbackWarning(
          inputId = "pt_outcome",
          !isTruthy(input$pt_outcome),
          text = "Vælg Outcome(s)!",icon = NULL
        ) 
        
        
        

        
   
      
      
 
    
    
  }
    
    
    )
}


main_effectserver <- function(id) {
  moduleServer(id, function(input,output, session){
    
    intervention_effect <- reactive(
      c(
        input$effect_1,
        input$effect_2,
        input$effect_3,
        input$effect_4,
        input$effect_5
      ) %>% as.numeric()
    )
    
    status <- "success"
    
    if (isTruthy(intervention_effect())) {
      
      # Calculate Mean Effect;
      mean_effect <- mean(
        intervention_effect()
      )
      
      
      
      
      if (mean_effect < 20) {
        
        status <- "success"
        
        closeAlert(
          id = "myalert"
        )
        
      }
      
      if (data.table::between(mean_effect,lower = 20,upper = 50)) {
        
        status <- "warning"
        
        closeAlert(
          id = "myalert"
        )
        
      }
      
      if (data.table::between(mean_effect,lower = 50,upper = 100)) {
        
        status <- "danger"
        
        message("Here")
        
        
        createAlert(
          id = "myalert",
          options = list(
            title = "Alert",
            closable = TRUE,
            width = 12,
            elevations = 10,
            status = "danger",
            content = paste("Din valgte effekt er", mean_effect, "%.")
          )
        )
        
        
        
        
      } 
      
    }
    
    
    
    
    
    
    # Update Effect slider
    updateProgressBar(
      session = session,
      id = "mean_pbar",
      value = mean(intervention_effect()),
      status = "danger"
      )
    
    return(intervention_effect)
    
    
    
    
  })
  
}


main_plotserver <- function(id, data, intervention_effect) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
   
    observeEvent(
      input$col_reset,ignoreInit = TRUE,ignoreNULL = TRUE,
      {

        updatePickerInput(
          inputId = "col_background",selected = "white",
          session = session
        )

        updatePickerInput(
          inputId = "col_intervention",selected = "steelblue",
          session = session
        )


        updatePickerInput(
          inputId = "col_control",selected = "orange",
          session = session
        )

        createAlert(
          id = "myalert",
          options = list(
            title = "Alert",
            closable = TRUE,
            width = 12,
            elevations = 10,
            status = "info",
            content = "Farverne er nulstillet!"
          )
        )


        delay(
          3000,
          closeAlert(
            id = "myalert"
          )
        )



      }
    )
    
    
    
    
    
    
    
    # Create Reactive Value
    chosen_colors <- reactiveValues(
      control_color = input$col_control,
      color_background = input$col_background,
      color_intervention = input$col_intervention

    )
    
    

    map(
      1:length(data()),
      .f = function(i) {
        
        # TODO: Isolate These
        # so they dont keep running!
        message(
          paste("Iteration", i)
        )
        
        
        plot_data <- data()[[i]] %>%
          plot_grinder(
            group_value = input$pt_demographic
          )
        
        output[[paste0("plot", i)]] <- renderPlotly(
          {
            
            
            
              plot_data  %>%
                do_plot(
                  difference = input$do_difference,
                  effect = intervention_effect()
                  ,
                  show_baseline = input$show_baseline,
                  color_intervention = chosen_colors$color_intervention,
                  color_background    = chosen_colors$color_background,
                  color_control      = chosen_colors$control_color
                )
            
            
            
            
            
            
            
            
            
            
            
            
          }
        )
        
        if (isTruthy(plot_data)) {
          
          req(input$pt_target)
          req(input$pt_control)
          req(input$pt_target != input$pt_control)
          
          shinyFeedback::feedbackDanger(
            inputId = "pt_demographic",
            !(isTruthy(plot_data$Control) & isTruthy(plot_data$Intervention)),
            text = "Sammenligningen er problematisk!",
            icon = NULL
          )       
          
          
        }
        
        
        
       
        
        
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
          group_value      = paste(input$pt_demographic),
          type             = input$do_incident,
          cost             = input$do_cost
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
      
      if (isTruthy(input$pt_control)) {
        
        input$pt_control %>% str_replace("_", ": ")
        
      } else {
        
        "Intet Valgt"
        
      }
      
      
      
      # if (is.null(input$pt_control)) {
      #   "Intet Valgt"
      #   
      # } else {
      #   
      #   if (length(input$pt_control) > 1) {
      #     
      #     "Matchet Gruppe"
      #     
      #   } else {
      #     
      #     input$pt_control %>% str_replace("_", ": ")
      #     
      #   }
      #   
      #   
      # }
      
      
      
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
          
          "Alle Uddannelser"
          
        } else {
          
          
          
          fifelse(length(educ) > 0 & length(educ) < 3, yes =  paste(educ,collapse = " og "), no = "Alle uddannelser")
          
        }
        
      }
    )
    
    
    
    output$chosen_labor <- renderText(
      {
        
        socio <- chosen_demographics()[chosen_demographics() %chin% unique(data_list$primary_care$socio)]
        
        
        if (is.null(chosen_demographics()) | length(socio) == 0){
          
          "Alle Tilknytninger"
          
        } else {
          
          
          
          fifelse(length(socio) > 0 & length(socio) < 3, yes =  paste(socio,collapse = " og "), no = "Alle tilknytninger")
          
        }
        
      }
    )
    
    
  })
}
