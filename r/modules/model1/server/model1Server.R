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
        if (get_switch()) {
          
          # Light Mode:
          updateColorPickr(
            inputId = 'col_background',
            value = '#ffffff',
            session = session,
            action = 'enable'
          )
          
        } else {

          # Dark Mode:
          updateColorPickr(
            inputId = 'col_background',
            value = '#6c757d',
            session = session,
            action = 'enable'
          )

        }
      }
    )
    
    
    
    
    shinyjs::onclick(
      id = 'col_reset',
      expr = function(){
        
        # 1) define list
        color_list <- list(
          c('col_control', '#FFA500'),
          c('col_background', fifelse(get_switch(), '#ffffff','#6c757d')),
          c('col_intervention', '#4682B4')
          
        )
        
        
        
        # update colorpickr
        lapply(
          color_list,
          function(x) {
            
            message(
              paste(
                'id:', x[1], 'col:', x[2]
              )
            )
            
            updateColorPickr(
              inputId = x[1],
              session = session,
              action  = 'enable',
              value   = x[2]
            )
            
          }
        )
        
        
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
        
      
        
        # # Verbose:
        # # This is mainly to debug, and see how many times 
        # # this expression is called
        # message('Grinding data for model1:\n')
        
        grinded_data <- pick(
          data_list        = data_list,
          intervention     = paste(input$pt_target),
          control          = paste(input$pt_control)
          
          # ,
          # allocators       = paste(input$pt_outcome),
          # chars            = paste(input$pt_demographic),
          # do_incidence     = input$do_incident
        )
        
        group_size <- na.omit(rbindlist(
          grinded_data,
          fill = TRUE
        )[
          ,
          .(
            total_N   = sum(unique(total_N), na.rm = TRUE)
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


          value <- group_size[
            assignment_factor %chin% c('control'),
            .(
              value = total_N
            )
          ]$value




          formatC(
            x = value,
            big.mark = '.',
            decimal.mark = ','
          )


        })
        
        
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
    
    
    spreaded_data <- reactive({
      
      
      
      # 1) Grind data
      out <- spread(grind(
        data(),
        recipe = list(
          outcome_measure = fifelse(isTRUE(input$do_cost), 'cost', 'qty'),
          incidence = fifelse(isTRUE(input$do_incident), 1, 0)
        )
      ),
      values            = paste(input$pt_demographic))
      
      
      
      
      
      
      return(
        out
      )
      
    })
    
   
    
    # Flavor the data
    # with counterfactuals
    flavored_data <- reactive(
      {
        
        # data <- spread(
        #   spreaded_data()
        # )
        
        flavor(
          spreaded_data(),
          effect = intervention_effect(),
          extrapolate = input$do_decay,
          allocators = paste(input$pt_outcome)
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
        unique(
          fcase(
            input$pt_outcome %chin% outcome[[1]]$Indkomst, 'Indkomst',
            input$pt_outcome %chin% outcome[[1]]$`Primær sektor`, 'Primær sundhedssektor',
            input$pt_outcome %chin% outcome[[1]]$Psykiatrien, 'Psykiatrisk hospitalskontakt',
            input$pt_outcome %chin% outcome[[1]]$Somatikken, 'Somatisk hospitalskontakt',
            default =  'Receptpligtig medicin'
            
            
            
          )
        )
        
        
      }
    )
    
    
    
    # Add data counter
    # if the given element 
    # has 0 rows, then the relevant outcome havent
    # been chosen
    available_row <- reactive(
      {
        map(
          flavored_data(),
          nrow
        )
      }
    )
    
    
    
    validate_input <- function() {
      
      # Validating Input;
      validate(
        need(
          input$pt_target,
          message = 'Vælg en sygdomsgruppe.'
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
          table_baselayer()
        
        
      }
    )
    
    
    
    
    
    map(
      1:5,
      .f = function(i) {
        
        output[[paste0("table",i)]] <- renderUI(
          {
            
            validate_input()
            
            
            
            validate(
              need(
                available_row()[[i]] != 0,
                message = paste(
                  'Ingen relevante outcome(s) valgt. Se under:', paste(chose_outcomes(), collapse = ", ")
                )
                )
              
            )
            
            
            
            
            tabsetPanel(
              vertical = FALSE,
              .list = lapply(
                unique(table_data()[[i]]$Kategori),
                function(x) {
                  
                  # grab data
                  data <- table_data()[[i]][Kategori %chin% x]
                  
                  
                  
                  
                  pretty_table <- lapply(
                    split(data, 1:nrow(data)),
                    function(x) {
                      
                      
                      
                      
                      
                      x[
                        !is.na(`Forventet effekt (%)`)
                        ,
                        `Forventet effekt (%)` := HTML(
                          paste(
                            progressBar(
                              id = 's',
                              value = `Forventet effekt (%)` * 100, 
                              status = "pink", 
                              size = "l",display_pct = TRUE,
                              total = 100)
                          )
                        )
                        ,
                      ]
                      
                      
                      x[is.na(x)] <- ''
                      
                      
                      return(
                        x
                      )
                      
                    }
                  )
                  
                  
                  
                  
                  
                  tabPanel(
                    title = x,
                    # h5(x),
                    
                    column(
                      width = 12,
                      fluidRow(
                        bs4Table(
                          pretty_table,
                          bordered = TRUE,
                          striped = TRUE,
                          width = 12
                        )
                      )
                    )
                    
                    
                    
                    
                  )
                  
                  
                  
                  
                  
                }
              )
            )
            
            
            
            
            
            # tryCatch(
            #   {
            #     
            #     
            #     tabsetPanel(
            #       vertical = FALSE,
            #       .list = lapply(
            #         unique(table_data()[[i]]$Kategori),
            #         function(x) {
            #           
            #           # grab data
            #           data <- table_data()[[i]][Kategori %chin% x]
            #           
            #         
            #           
            #           
            #           pretty_table <- lapply(
            #             split(data, 1:nrow(data)),
            #             function(x) {
            #               
            #               
            #               
            #               
            #               
            #               x[
            #                 !is.na(`Forventet effekt (%)`)
            #                 ,
            #                 `Forventet effekt (%)` := HTML(
            #                   paste(
            #                     progressBar(
            #                       id = 's',
            #                       value = `Forventet effekt (%)` * 100, 
            #                       status = "pink", 
            #                       size = "l",display_pct = TRUE,
            #                       total = 100)
            #                     )
            #                   )
            #                 ,
            #               ]
            #               
            #               
            #               x[is.na(x)] <- ''
            # 
            #               
            #               return(
            #                 x
            #               )
            #                                         
            #             }
            #           )
            #           
            #           
            #           
            #           
            #           
            #           tabPanel(
            #             title = x,
            #             # h5(x),
            #             
            #             column(
            #               width = 12,
            #               fluidRow(
            #                 bs4Table(
            #                   pretty_table,
            #                   bordered = TRUE,
            #                   striped = TRUE,
            #                   width = 12
            #                 )
            #               )
            #             )
            #             
            #            
            #             
            #             
            #           )
            #           
            #          
            #           
            #           
            #           
            #         }
            #       )
            #     )
            #     
            #     # bs4Carousel(
            #     #   id = paste0('car_model', i),
            #     #   width = 12,
            #     #   indicators = TRUE,
            #     #   .list = lapply(
            #     #     unique(table_data()[[i]]$Outcome),
            #     #     function(x) {
            #     #       
            #     #       
            #     #       
            #     #       
            #     #       
            #     #       carouselItem(
            #     #         caption = NULL,
            #     #         h5(x),
            #     #         bs4Table(
            #     #           table_data()[[i]][Outcome %chin% x],
            #     #           bordered = TRUE,
            #     #           width = 12
            #     #         )
            #     #         
            #     #         
            #     #       )
            #     #       
            #     #       
            #     #       
            #     #     }
            #     #   )
            #     # )
            #     
            #   },
            #   
            #   error = function(condition) {
            #     
            #     validate(
            #       need(
            #         is.null(input$pt_outcome),
            #         message = paste(condition))
            #     )
            #     
            #   },
            #   warning = function(condition) {
            #     
            #     validate(
            #       need(
            #         is.null(input$pt_outcome),
            #         message = paste(
            #           'Ingen relevante outcome(s) valgt. Se under:', paste(chose_outcomes(), collapse = ",")
            #         )
            #       )
            #     )
            #     
            #   }
            # )
            
            
            
            
            
            
           
            
          }
        )
        
        









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
          alternate = input$do_cost,
          plot_list = baseline_plot() 
        )

    })
    

    #   map(
    #     1:5,
    #   .f = function(i) {
    # 
    # 
    # 
    # 
    # 
    #       output[[paste0("plot", i)]] <- renderPlotly(
    #         {
    # 
    # 
    # 
    #           validate_input()
    #           
    #           
    #             
    # 
    # 
    #           tryCatch(
    #             {
    #               plot_data()[[i]] %>%
    #                 subplot(
    #                   titleX = TRUE,
    #                   titleY = TRUE,
    #                   shareX = TRUE
    #                 )
    #             },
    # 
    #             error = function(condition) {
    #               
    #               
    #               validate(
    #                 need(
    #                   is.null(condition),
    #                   message = paste('Sammenligningen er problematisk.', '\n', condition)
    #                 )
    #               )
    # 
    #             },
    #             warning = function(condition) {
    #               
    # 
    #               validate(
    #                 need(
    #                   is.null(input$pt_outcome),
    #                   message = paste(
    #                     'Ingen relevante outcome(s) valgt. Se:', paste(unique(chose_outcomes()), collapse = ",")
    #                   )
    #                 )
    #               )
    #             }
    #           )
    # 
    # 
    #         }
    #       )
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    #   }
    # )
      
    map(
      1:5,
      .f = function(i) {





        output[[paste0("plot", i)]] <- renderUI(
          {



            validate_input()
            
            validate(
              need(
                available_row()[[i]] != 0,
                message = paste(
                  'Ingen relevante outcome(s) valgt. Se under:', paste(chose_outcomes(), collapse = ", ")
                )
              )
              
            )
            
            tabsetPanel(
              vertical = FALSE,
              .list = lapply(
                seq_along(plot_data()[[i]]),
                function(x) {
                  
                  
                  
                  tabPanel(
                    title = str_split(
                      names(plot_data()[[i]][x]),
                      pattern =  '_',
                      simplify = TRUE
                    )[,2],
                    # h5(x),
                    
                    column(
                      width = 12,
                      fluidRow(
                        plot_data()[[i]][x]
                      )
                    )
                    
                    
                    
                    
                  )
                  
                  
                  
                }
              )
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



    
