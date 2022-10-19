# script: baselayer
# objective: create functions that generates baseplots
# for the data
# author: Serkan Korkmaz
# date: 2022-06-16



.layout_model1 <- function(
    plot_list,
    background_color = 'white',
    intervention_color = '#4682B4',
    control_color = '#FFA500',
    alternate = TRUE
    ) {
  
  
  
  
  
  
  map(plot_list,
      function(plot) {
        map(plot,
            function(plot) {
              
             
              
              # Generate Y axis Text; #####
              get_data <- plotly_data(plot)


              get_allocator <- str_to_sentence(str_remove(
                unique(get_data$allocator),
                "[:graph:]*[:blank:]*[:graph:]*_"
              ))
              
              
              

              yaxis_text <- fcase(
                inherits(get_data, c("primary_care")),
                fifelse(
                  alternate,
                  no = "Gennemsnitlig besøg pr. person",
                  yes ="Gennemsnitlig omkostninger pr. person"
                  ),
                inherits(get_data, c(
                  "psychiatric_care", "somatic_care"
                )),
                fifelse(
                  alternate,
                  no = "Gennemsnitlig sengedage pr. person",
                  yes ="Gennemsnitlig omkostninger pr. person"
                )
                ,
                inherits(get_data, c("transfers")),
                fifelse(
                  alternate,
                  no = "Gennemsnitlig antal uger pr. person",
                  yes ="Gennemsnitlig omkostninger pr. person"
                ),
                default = fifelse(
                  alternate,
                  no = "Gennemsnitlig antal recepter pr. person",
                  yes ="Gennemsnitlig omkostninger pr. person"
                )
              )

              yaxis_text <- paste(yaxis_text, '\n',
                                  get_allocator)
              
              
              
              plot <- plot %>% plotly::style(
                line = list(
                  color = control_color
                ), 
                marker = list(
                  color = control_color
                ),
                traces = 3
              ) %>% plotly::style(
                line = list(
                  color = intervention_color
                ), 
                marker = list(
                  color = intervention_color
                ),
                traces = c(2)
              ) %>% plotly::style(
                traces = 4,
                line = list(
                  dash = "dot",
                  color = intervention_color
                ),
                marker = list(
                  color = intervention_color
                )
              )
              
              
              
              
              # Add Layout; #####
              plotly::layout(
                plot,
                paper_bgcolor = '#ffffff00',
                plot_bgcolor='#ffffff00',
                font = list(
                  size = 14,
                  color = background_color
                ),
                
                yaxis = list(
                  showgrid = FALSE,
                  zeroline = TRUE,
                  title = yaxis_text
                ),
                
                xaxis = list(
                  showgrid = FALSE,
                  zeroline = FALSE,
                  range = list(-2, 5),
                  title = 'Tid'
                ),
                
                
                legend = list(title = "Gruppe",
                              orientation = 'h'),
                
                annotations = list(
                  text = "Baselineår",
                  yref = "paper",
                  x = 0,
                  y = 1,
                  showarrow = FALSE,
                  font = list(
                    size = 14,
                    color = background_color)
                ),
                
                shapes = list(
                  list(
                    type = "line",
                    line = list(
                      color = background_color,
                      dash  = "dot"
                      ),
                    y0 = 0,
                    y1 = 0.975,
                    yref = "paper",
                    # i.e. y as a proportion of visible region
                    x0 = 0,
                    x1 = 0,
                    layer = "above"
                  )
                )
                
              )
              
            })
        
        
        
      }

      )
  
}



.layout_model2 <- function(
    plot_list,
    background_color = 'white',
    intervention_color = '#4682B4',
    control_color = '#FFA500'  
) {
  
  
  map(
    plot_list,
    .f = function(plot) {
      
     plot <-  plot %>% style(
        marker = list(
          color = intervention_color,
          line = list(
            color = 'black',
            width = 1.5
          )
        ),
        traces = c(2,4)
      ) %>% style(
        traces = 3,
        marker = list(
          color = control_color,
          line = list(
            color = 'black',
            width = 1.5
          )
        )
      )
     
     
     plotly::layout(
       plot,
       paper_bgcolor = '#ffffff00',
       plot_bgcolor='#ffffff00',
       font = list(
         size = 14,
         color = background_color
       )
     )
      
      
    }
  )
  
  
  
}


# Add layout;

plot_layout <- function(
    plot_list,
    background_color = 'white',
    intervention_color = '#4682B4',
    control_color = '#FFA500',
    alternate = FALSE
    ) {
  
  if (inherits(plot_list, 'model1')) {
   
    
    
    return(
      plot_list %>% .layout_model1(
        background_color = background_color,
        intervention_color = intervention_color,
        control_color = control_color,
        alternate = alternate
      )
    )
    
  
  } 
  
  if (inherits(plot_list, 'model2')) {
    
    
    return(
      plot_list %>% .layout_model2(
        intervention_color = intervention_color,
        control_color = control_color,
        background_color = background_color
      )
    )
    
    
    
  }
  
}
