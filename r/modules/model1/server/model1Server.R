# Script; Server Logic
# for the main model.
# main warning logic; #####

.model1server_output <- function(id, data_list, get_switch) {
  moduleServer(id, function(input, output, session) {
    
    
    
    observeEvent(
      get_switch(),
      {
        
        if (isFALSE(get_switch())) {
          
          updateColorPickr(
            inputId = 'col_background',
            value = '#000000',
            session = session,
            action = 'enable'
          )
          
          
          
        } else {
          
          updateColorPickr(
            inputId = 'col_background',
            value = '#FFFFFF',
            session = session,
            action = 'enable'
          )
          
        }
        
        
      }
    )
    
    # Server Start
    data <- reactive(
      {
        
        req(input$pt_target)
        req(input$pt_outcome)
        
        message('Grinding data for model1:\n')
        
        grinded_data <- grind(
          data_list = data_list,
          intervention     = paste(input$pt_target),
          control          = paste(input$pt_control),
          allocators       = paste(input$pt_outcome),
          chars            = paste(input$pt_demographic),
          do_incidence = input$do_incident
        )
        
        
        grinded_data <- spread(
          data_list = grinded_data,
          
          # Should the data be in qty, or cost.
          alternate = input$do_cost
          
        )
        
        
        return(
          grinded_data
        )
        
      }
    )
    
    
    
    # serverwide variables; #####
    intervention_effect <- reactive(
      c(
        input$effect_1,
        input$effect_2,
        input$effect_3,
        input$effect_4,
        input$effect_5
      ) %>% as.numeric()
    )
    
    
    
    flavored_data <- reactive(
      {
        
        
        data() %>% flavor(
          effect = intervention_effect()
          )
        
      }
    )
    
    chose_outcomes <- reactive(
      {
        
        fcase(
          input$pt_outcome %chin% outcome[[1]]$Overførsel, 'Arbejdsmarked',
          input$pt_outcome %chin% outcome[[1]]$`Primær Sektor`, 'Primær Sektor',
          input$pt_outcome %chin% outcome[[1]]$Psykiatrien, 'Psykiatrien',
          input$pt_outcome %chin% outcome[[1]]$Somatikken, 'Somatik',
          input$pt_outcome %chin% outcome[[1]]$Lægemiddelforbrug, 'Præparater'
          
          
          
        )
        
      }
    )
    
    
    
    # render tables; #####
    table_data <- reactive(
      {
        
        # TODO: Create wrapper
        flavored_data() %>% 
          table_baselayer() %>% 
          .model1_table(alternate = input$do_cost)
        
        
      }
    )
    
    
    
    
    
    map(
      1:5,
      .f = function(i) {


        output[[paste0("table",i)]] <- DT::renderDataTable({


          # Validating Input;
          validate(
            need(
              input$pt_target,
              message = 'Vælg en sygdomsgruppe!'
            ),
            need(
              input$pt_outcome,
              message = 'Vælg outcome(s)'
            ),
            need(
              input$pt_target != input$pt_control,
              message = 'Valgte grupper skal være forskellige.'
            )
          )


          message(
            paste("Grinding tables. Currently at", i, "of", 5, "iterations.")
          )



          tryCatch(
            {

              table_data()[[i]]

            },

            error = function(condition) {

              validate(
                need(
                  is.null(input$pt_outcome),
                  message = paste(condition))
              )

            },
            warning = function(condition) {

              validate(
                need(
                  is.null(input$pt_outcome),
                  message = paste(
                    'Ingen relevante outcome(s) valgt. Se under:', paste(chose_outcomes(), collapse = ",")
                  )
                )
              )

            }
          )




        })












      }
    )
    
    
    
    
    
    
    # Render plots; #####
    # plot
    baseline_plot <- reactive({
      
      message('Plotting Data')
      
      
      flavored_data() %>%
        baselayer() %>%
        baseplot() %>%
        effectlayer()


      
      
      
      
      
      
      
    })
    
  
    
    plot_data <- reactive({
      
      
      baseline_plot() %>%
        plot_layout(
          background_color = input$col_background,
          intervention_color = input$col_intervention,
          control_color = input$col_control,
          alternate = input$do_cost
        )

    })
    

      map(
        1:5,
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
                ),
                need(
                  input$pt_target != input$pt_control,
                  message = 'Valgte grupper skal være forskellige.'
                )
              )
              
              # TODO: Check if default is truthy
              # needs to be population by default.
              
              
                


              tryCatch(
                {
                  plot_data()[[i]] %>%
                    subplot(
                      titleX = TRUE,
                      titleY = TRUE,
                      shareX = TRUE
                    )
                },

                error = function(condition) {
                  
                  
                  validate(
                    need(
                      is.null(condition),
                      message = 'Sammenligningen er problematisk.'
                    )
                  )
                  
                  # validate(
                  #   need(
                  #     isTruthy(flavored_data()[[i]]$intervention),
                  #     message = 'Problem'
                  #   )
                  # )

                },
                warning = function(condition) {
                  

                  validate(
                    need(
                      is.null(input$pt_outcome),
                      message = paste(
                        'Ingen relevante outcome(s) valgt. Se:', paste(unique(chose_outcomes()), collapse = ",")
                      )
                    )
                  )
                }
              )








              # if (isTruthy(input$pt_target) & isTruthy(input$pt_control)) {
              #
                # shinyFeedback::feedbackDanger(
                #   inputId = "pt_demographic",
                #   !(isTruthy(plot_data()[[i]]$control) & isTruthy(plot_data()[[i]]$intervention)),
                #   text = "Sammenligningen er problematisk!",
                #   icon = NULL
                # )
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
      
      
      
      
      # Server End #####
  }
  )
}



    
