# script: model1Server
# objective: Collect all relevant server elements
# for model1. 
# NOTE: This doesnt include inessential elements like choices,
# which can be rendered outside this server.
# date: 2022-09-22
# author: Serkan Korkmaz

.model1server_output <- function(id, data_list, get_switch) {
  moduleServer(id, function(input, output, session) {
    
    # server logic; 
    
    
    # Server events; #####
    
    
    # Change axis colors depending
    # on color mode;
    observeEvent(
      get_switch(),
      {
        
        if (isFALSE(get_switch())) {
          
          # Dark Mode:
          updateColorPickr(
            inputId = 'col_background',
            value = '#000000',
            session = session,
            action = 'enable'
          )
          
          
          
        } else {
          
          # Light Mode:
          updateColorPickr(
            inputId = 'col_background',
            value = '#FFFFFF',
            session = session,
            action = 'enable'
          )
          
        }
        
        
      }
    )
    
    
   # reset the value of chosen
   # pt_control.
   # It goes back to NULL when clicked.
   # Otherwise there is no switch between
   # population and specific control values
    shinyjs::onclick(
      id = 'do_match',
      expr = reset(
        id = 'pt_control'
      )
    )
    
    # Generate Data for the server; #####
    data <- reactive(
      {
        
        # Require that at least
        # pt_target and pt_outcome is chosen.
        req(input$pt_target)
        req(input$pt_outcome)
        
        # Verbose:
        # This is mainly to debug, and see how many times 
        # this expression is called
        message('Grinding data for model1:\n')
        
        grinded_data <- grind(
          data_list        = data_list,
          intervention     = paste(input$pt_target),
          control          = paste(input$pt_control),
          allocators       = paste(input$pt_outcome),
          chars            = paste(input$pt_demographic),
          do_incidence     = input$do_incident
        )
        
        
        # Calculate total group size
        # for the chosen disease groups
        group_size <- na.omit(rbindlist(
          grinded_data,
          fill = TRUE
        )[
          ,
          .(
            total_N   = sum(unique(total_n), na.rm = TRUE)
          )
          ,
          by = .(
            assignment_factor
          )
        ])
        
        
        output$n_treatment <- renderText({
          
          formatC(
            x = group_size[
              assignment_factor %chin% c('intervention'),
              .(
                value = total_N
              )
            ]$value,
            big.mark = '.',
            decimal.mark = ','
          )
          
          
        })
        
        output$n_control <- renderText({
          
          if (as.logical(input$do_match)){
            
            value <- group_size[
              assignment_factor %chin% c('population'),
              .(
                value = total_N
              )
            ]$value
            
          } else {
            
            value <- group_size[
              assignment_factor %chin% c('control'),
              .(
                value = total_N
              )
            ]$value
            
            
          }
          
          
          formatC(
            x = value,
            big.mark = '.',
            decimal.mark = ','
          )
          
          
        })
        
        
        # Spread the data and 
        # and return it as a list.
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
    
    # Generate a vector of intervention effects
    # as a reactive numeric object
    intervention_effect <- reactive(
      as.numeric(
        c(
          input$effect_1,
          input$effect_2,
          input$effect_3,
          input$effect_4,
          input$effect_5
        )
      )
    )
    
    
    # Flavor the data
    # with counterfactuals
    flavored_data <- reactive(
      {
        flavor(
          data(),
          effect = intervention_effect()
        )
      }
    )
    
    # generate chosen outcome classes
    # for the model.
    # This is used to guide the user
    # whenever the chosen outcomes doesnt
    # correspond to the chosen tab.
    chose_outcomes <- reactive(
      {
        
        fcase(
          input$pt_outcome %chin% outcome[[1]]$Overførsel, 'Overførsler',
          input$pt_outcome %chin% outcome[[1]]$`Primær Sektor`, 'Primær sundhedssektor',
          input$pt_outcome %chin% outcome[[1]]$Psykiatrien, 'Psykiatrisk hospitalskontakt',
          input$pt_outcome %chin% outcome[[1]]$Somatikken, 'Somatisk hospitalskontakt',
          input$pt_outcome %chin% outcome[[1]]$Lægemiddelforbrug, 'Præparatforbrug'
          
          
          
        )
        
      }
    )
    
    
    validate_input <- function() {
      
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
      
    }
    
    
    # Render Tables; ####
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


          validate_input()


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
    baseline_plot <- reactive({
      effectlayer(
        baseplot(
          baselayer(
            flavored_data()
          )
        )
      )
      
    })
    
  
    
    plot_data <- reactive({
      
        plot_layout(
          
          background_color = input$col_background,
          intervention_color = input$col_intervention,
          control_color = input$col_control,
          alternate = input$do_cost,plot_list = baseline_plot() 
        )

    })
    

      map(
        1:5,
      .f = function(i) {





          output[[paste0("plot", i)]] <- renderPlotly(
            {



              validate_input()
              
              
                


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


            }
          )














      }
    )
      
      

        output$download_files <- downloadHandler(

          filename = function() {

            # WARNING: paste0 breaks the
            # download process.
            paste(
              "output",
              "zip",
              sep="."
            )

          },

          content = function(fname) {

            
            
            

            
            # Download Tables

            pack(
              wrap(
                flavored_data()
              ),
              intervention_name = input$pt_target,
              control_name = input$pt_control,
              filename     = fname,
              char = input$pt_demographic
            )






          },
          contentType = "application/zip"


        )
      
     
      
      
      # Server End #####
  }
  )
}



    
