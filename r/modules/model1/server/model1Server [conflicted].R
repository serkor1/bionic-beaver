# Script; Server Logic
# for the main model.
# main warning logic; #####
.model1server_warnings <- function(id){
  moduleServer(id, function(input, output, session)
  {
    
    
      

  
      
    
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
    #   shinyFeedback::feedbackDanger(
    #     inputId = 'pt_control',
    #     input$pt_control == input$pt_target,
    #     text = 'Skift gruppe!',
    #     icon = NULL
    #   )
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
        no   = paste(
          input$pt_demographic,
          collapse = ", "
        )
      )
      
      
    }
    
    
    output$chosen_gender <- renderText({
      
      internal_foo(chars[[1]]$køn)
      
    })
    
    output$chosen_educ <- renderText({
      
      internal_foo(chars[[1]]$uddannelse)
      
    })
    
    
    output$chosen_labor <- renderText({
      
      internal_foo(chars[[1]]$arbejdsmarked)
      
    })
    
    output$chosen_age <- renderText({
      
      internal_foo(chars[[1]]$alder)
      
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
        yes = str_replace(input$pt_target, "_", ": "),
        no  = 'Den generelle befolkning'
      )
      
    })
    
    
  }
  
  
  )
}


# plot logic; #####
main_plotserver <- function(id, data, intervention_effect = NULL, light_mode =NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    #' Server Information;
    #' 
    #' @param data ReactiveExpression. This is the data
    #' that are plot ready.
    #' 
    #' @param intervention_effect ReactiveExpression. This
    #' is the intervention effects that the user choses.
    
    
    # observeEvent(
    #   input$col_reset,ignoreInit = TRUE,ignoreNULL = TRUE,
    #   {
    #     
    #     updatePickerInput(
    #       inputId = "col_background",selected = "white",
    #       session = session
    #     )
    #     
    #     updatePickerInput(
    #       inputId = "col_intervention",selected = "steelblue",
    #       session = session
    #     )
    #     
    #     
    #     updatePickerInput(
    #       inputId = "col_control",selected = "orange",
    #       session = session
    #     )
    #     
    #     # Notify the user that 
    #     # that the colors are reset.
    #     createAlert(
    #       id = "myalert",
    #       options = list(
    #         title = "Alert",
    #         closable = TRUE,
    #         width = 12,
    #         elevations = 10,
    #         status = "info",
    #         content = "Farverne er nulstillet!"
    #       )
    #     )
    #     
    #     # Autoclose the 
    #     # the Alert after 3 seconds.
    #     delay(
    #       3000,
    #       closeAlert(
    #         id = "myalert"
    #       )
    #     )
    #     
    #     
    #     
    #   }
    # )
    
    
    
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
    
    
    # TODO: Maybe BAZ should be moved inside
    # the effect layer
    # plot_data <- reactive(
    #   data %>%
    #     flavor(
    #       effect = intervention_effect,
    #       do_match = as.logical(input$do_match)
    #     )
    # )
    # 
    # 
    # 
    # # Generate Plots
    # plot_list <- {
    #   plot_data() %>%
    #     baselayer() %>%
    #     baseplot()
    # }
    # 
    # final_plot <- plot_list %>% 
    #   effectlayer() %>% 
    #   plot_layout()
    
    
    
    
    
    final_plot <- reactive(
      {
        data %>%
          flavor(
            effect = 50,
            do_match = as.logical(input$do_match)
          ) %>%
          baselayer() %>%
          baseplot() %>% 
          effectlayer() %>% 
          plot_layout()
      }
    )
    
    # Create a container for the chosen
    # colors so the changes are applied globally.
    
    
    output$plot1 <- renderPlotly(
      {
        final_plot()[[1]] %>% 
          subplot(
            titleX = TRUE,
            titleY = TRUE,
            shareX = TRUE
          ) 
      }
    )
    
    
    map(
      1:length(final_plot()),
      .f = function(i) {
        
        
        
        
        output[[paste0("plot", i)]] <- renderPlotly(
          {
            
            
            final_plot()[[i]] %>%
              subplot(
                titleX = TRUE,
                titleY = TRUE,
                shareX = TRUE
              )
            
            
            
            
            # if (isTruthy(input$pt_target) & isTruthy(input$pt_control)) {
            #
            #   shinyFeedback::feedbackDanger(
            #     inputId = "pt_demographic",
            #     !(isTruthy(plot_data()[[i]]$control) & isTruthy(plot_data()[[i]]$intervention)),
            #     text = "Sammenligningen er problematisk!",
            #     icon = NULL
            #   )
            #
            # }
            # chosen_colors <- reactiveValues(
            #   control_color = input$col_control,
            #   color_background = input$col_background,
            #   color_intervention = input$col_intervention
            #
            # )
            #
            #
            # plot_data()[[i]]  %>%
            #   do_plot(
            #     difference = input$do_difference,
            #     show_baseline = input$show_baseline,
            #     color_intervention = chosen_colors$color_intervention,
            #     color_background    = chosen_colors$color_background,
            #     color_control      = chosen_colors$control_color
            #   )
            # plotly::plot_ly(
            #   mtcars,
            #   x = ~mpg,
            #   y = ~hp,
            #   type = "scatter",
            #   mode = "markers"
            # )
            
            
          }
        )
        
        
        
        
        
        
        
        
        
      }
    )
    
    
    
    
    
  }
  
  
  )
  
  
  
  
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
        data() %>% flavor(effect =  intervention_effect(), do_match = as.logical(input$do_match))
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





.model1server_plot <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    
    
    data <- reactive(
      {
        
        req(input$pt_target)
        req(input$pt_outcome)
        
        message("Grinding Data:")
        message(
          paste(
            'With Parameters:\n',
            'Intervention:', paste(input$pt_target), '\n',
            'Control:',paste(input$pt_control), is.null(input$pt_control),'\n',
            'Outcomes:',paste(input$pt_outcome, collapse = ","),'\n',
            'Characteristics:', paste(input$pt_demographic, collapse = ","),'\n',
            'Incidence:', input$do_incident, '\n',
            'Alternate:', input$do_cost
                )
        )
        
        
        
        
        data_list %>% grind(
          intervention     = paste(input$pt_target),
          control          = paste(input$pt_control),
          allocators       = paste(input$pt_outcome),
          chars            = paste(input$pt_demographic),
          do_incidence = input$do_incident,
          alternate = input$do_cost
        ) %>% spread()
      }
    )
    
    
    intervention_effect <- reactive(
      c(
        input$effect_1,
        input$effect_2,
        input$effect_3,
        input$effect_4,
        input$effect_5
      ) %>% as.numeric()
    )
    
    
    # plot
    plot_data <- reactive({
      
      message('Plotting Data')
      
      
      data() %>% flavor(effect = intervention_effect())  %>%
        baselayer() %>%
        baseplot() %>%
        effectlayer() %>%
        plot_layout()
      
      
      
      
      
    })
    
    
    chose_outcomes <- reactive(
      {
        
        fcase(
          input$pt_outcome %chin% outcome[[1]]$Overførsel, 'Arbejdsmarked',
          input$pt_outcome %chin% outcome[[1]]$`Primær Sektor`, 'Primær Sektor',
          input$pt_outcome %chin% outcome[[1]]$Psykiatrien, 'Psykiatrien',
          input$pt_outcome %chin% outcome[[1]]$Somatikken, 'Somatik'
          
          
          
        )
        
      }
    )
    
    
    
    
    
    map(
      1:4,
      .f = function(i) {
        
        
       
        
        
        output[[paste0("plot", i)]] <- renderPlotly(
          {
            
          
            
            validate(
                need(
                  input$pt_target,
                  message = 'Vælg en sygdomsgruppe!'
                  ),
                need(
                  input$pt_outcome,
                  message = 'Vælg outcome(s)'
                )
            )
            
            waiter <- waiter::Waiter$new(id = 'plot1')
            waiter$show()
            Sys.sleep(1)
            
          tryCatch(
            plot_data()[[i]] %>%
              subplot(
                titleX = TRUE,
                titleY = TRUE,
                shareX = TRUE
              ),
           error = function(condition) {
              
             validate(
               need(
                 is.null(input$pt_outcome),
                 message ="Error")
             )
              
            },
            warning = function(condition) {
              
              validate(
                need(
                  is.null(input$pt_outcome),
                  message = paste(
                    'Ingen relevante outcome(s) valgt. Se:', paste(chose_outcomes(), collapse = ",")
                  )
                  )
              )
              
            }
          )  
            
           
            
            on.exit(waiter$hide())
            
            # if (isTruthy(input$pt_target) & isTruthy(input$pt_control)) {
            #
            #   shinyFeedback::feedbackDanger(
            #     inputId = "pt_demographic",
            #     !(isTruthy(plot_data()[[i]]$control) & isTruthy(plot_data()[[i]]$intervention)),
            #     text = "Sammenligningen er problematisk!",
            #     icon = NULL
            #   )
            #
            # }
            # chosen_colors <- reactiveValues(
            #   control_color = input$col_control,
            #   color_background = input$col_background,
            #   color_intervention = input$col_intervention
            #
            # )
            #
            #
            # plot_data()[[i]]  %>%
            #   do_plot(
            #     difference = input$do_difference,
            #     show_baseline = input$show_baseline,
            #     color_intervention = chosen_colors$color_intervention,
            #     color_background    = chosen_colors$color_background,
            #     color_control      = chosen_colors$control_color
            #   )
            # plotly::plot_ly(
            #   mtcars,
            #   x = ~mpg,
            #   y = ~hp,
            #   type = "scatter",
            #   mode = "markers"
            # )
            
            
          }
        )
        
        
        
        
        
        
        
        
        
      }
    )
    
    
    
    
    
  }
  )
}