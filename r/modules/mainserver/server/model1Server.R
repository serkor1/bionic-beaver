# Script; Server Logic
# for the main model.
# main warning logic; #####
main_warnings <- function(id){
  moduleServer(id, function(input, output, session)
  {
    
    # Check wether user chose
    # needed inputs and give feedback
    message(
      paste("Chosen:",input$pt_control, "as Control.", "Truthy:", isTruthy(input$pt_control), "\n")
      )
    
    message(
      paste("Chosen:",input$pt_target, "as Intervention." , "Truthy:", isTruthy(input$pt_target), "\n")
    )
    
    
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

# effect logic; ####
main_effectserver <- function(id, data) {
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
    
    
    effect_data <- reactive(
      {
        data() %>% baz(
          effect = intervention_effect()
        )
      }
    )
    
    
    observe(
      {
        map(
          1:4,
          .f = function(i) {
            
            
            updateProgressBar(
              session = session,
              id = paste0("e_effect",i),
              value = mean(intervention_effect()),
              status = "danger"
            )
            
            updateProgressBar(
              session = session,
              id = paste0("r_effect",i),
              value = mean(effect_data()[[i]]$cdifference, na.rm = TRUE),
              status = "danger"
            )
            
          }
        )
        
      }
    )
    
    
    
    
    
    
    
    
    
    return(
      intervention_effect
    )
    
    
    
    
  })
  
}

# plot logic; #####
main_plotserver <- function(id, data, intervention_effect = NULL, light_mode) {
  moduleServer(id, function(input, output, session) {
    
    
    #' Server Information;
    #' 
    #' @param data ReactiveExpression. This is the data
    #' that are plot ready.
    #' 
    #' @param intervention_effect ReactiveExpression. This
    #' is the intervention effects that the user choses.
    
    
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
        
        # Notify the user that 
        # that the colors are reset.
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
        
        # Autoclose the 
        # the Alert after 3 seconds.
        delay(
          3000,
          closeAlert(
            id = "myalert"
          )
        )
        
        
        
      }
    )
    
    
    
    # observe(
    #   {
    #     
    #     if (!isTRUE(light_mode())){
    #       message("In Ture")
    #       updatePickerInput(
    #         inputId = "col_background",selected = "black",
    #         session = session
    #       )
    #     } else {
    #       
    #       updatePickerInput(
    #         inputId = "col_background",selected = "white",
    #         session = session
    #       )
    #       
    #     }
    #     
    #     
    #   }
    # )
    # 
    # 
    # message(class(data()))
    
    
    plot_data <- reactive(
      data() %>% 
        baz(
          effect = intervention_effect(),
          do_match = as.logical(input$do_match)
          )
      )
    
    
    
      
    
    
    
   
    # Create a container for the chosen
    # colors so the changes are applied globally.
    
  
    library(parallel)
    
    
    
    
    map(
      1:length(data()),
      .f = function(i) {

        # TODO: Isolate These
        # so they dont keep running!
        message(
          paste("Grinding plots. Currently at", i, "of", length(data()), "iterations.")
        )





        output[[paste0("plot", i)]] <- renderPlotly(
          {




            if (isTruthy(input$pt_target) & isTruthy(input$pt_control)) {

              shinyFeedback::feedbackDanger(
                inputId = "pt_demographic",
                !(isTruthy(plot_data()[[i]]$control) & isTruthy(plot_data()[[i]]$intervention)),
                text = "Sammenligningen er problematisk!",
                icon = NULL
              )

            }
            chosen_colors <- reactiveValues(
              control_color = input$col_control,
              color_background = input$col_background,
              color_intervention = input$col_intervention

            )


            plot_data()[[i]]  %>%
              do_plot(
                difference = input$do_difference,
                show_baseline = input$show_baseline,
                color_intervention = chosen_colors$color_intervention,
                color_background    = chosen_colors$color_background,
                color_control      = chosen_colors$control_color
              )


          }
        )






      }
    )
    

    
    
  }
  
  
  )
  
  
  
  
}




# main data logic; ####
main_dataserver <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    data <- reactive(
      {
        
        data_list %>% grinder(
          intervention     = paste(input$pt_target),
          control          = paste(input$pt_control),
          allocator_vector = paste(input$pt_outcome),
          group_value      = paste(input$pt_demographic),
          type             = input$do_incident,
          cost             = input$do_cost
        ) %>% foo()
        
      }
    )
    
    
    message("data server done")
    
    
    return(
      data
    )
    
    
  }
  )
}






# choice logic; #####
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
      
      if (isTruthy(input$do_match)) {
        
        if (input$do_match) {
          
          "Den generelle befolkning"
          
        } else {
          
          if (isTruthy(input$pt_control)) {
            
            input$pt_control %>% str_replace("_", ": ")
            
          } else {
            
            "Intet Valgt"
            
          }
          
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


# table logic; #####
main_tableserver <- function(id, data, intervention_effect){
  moduleServer(
    id,
    function(input,output, session){
      
      
      # This sever Generates a table for 
      # the frontpage
      
      # table_data <- map(
      #   1:length(data()),
      #   .f = function(i) {
      #     
      #     
      #     data()[[i]] %>% foo()
      #     
      #     
      #   }
      # ) %>% rbindlist()
      # 
    
      table_data <- reactive(
        data() %>% baz(effect = intervention_effect(), do_match = as.logical(input$do_match))
        )
      
      map(
        1:length(data()),
        .f = function(i) {
          
          
          message(
            paste("Grinding tables. Currently at", i, "of", length(data()), "iterations.")
          )

          
          
          


          output[[paste0("table",i)]] <- DT::renderDataTable({
            
            data <- table_data()[[i]]


            data[order(allocator)] %>% DT::datatable(
              rownames = FALSE,
              extensions = c("Buttons", "RowGroup"),
              caption = "Tabel",

              options = list(
                pageLength = 8,
                dom = "Bfrtip",
                columnDefs =list(
                  list(
                    className = "dt-head-center dt-center", targets = 1:(ncol(data) - 1)
                  )
                ),
                rowGroup = list(dataSrc = c(1)),
                buttons = c("csv", "excel"),
                fixedHeader = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#5E81AC', 'color': '#fff'});",
                  "}")
              )

              )


          })
          
          
          
         








        }
      )
      
      
      
      
      
      
      
    }
  )
}