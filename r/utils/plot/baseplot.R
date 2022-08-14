# script: 
# objective: 
# author: Serkan Korkmaz
# date: 2022-06-16

.baseplot_model1 <- function(
  plot = NULL,
  intervention_color = 'steelblue',
  control_color = 'orange'
  ) {
  
  # TODO:
  # 1) Add Baseline Option
  # 2) Add Plot Names of Y axis
  # 3) Move Legend and print only one.

  
  
  #' function information
  #' 
  #' @param plot a list of plots
  #' 
  #' @returns a nested list of plots
  #' based on the types
  
  plot <- map(
    plot,
    .f = function(plot) {
      
      # Add counter here.
      
      plot %>% map(
        .f = function(plot) {
          
          
          
          plot %>% 
            # Add intervention group
            # trace.
            add_trace(
            x = ~x,
            y = ~intervention,
            line = list(
              color = intervention_color
            ),
            marker = list(
              color = intervention_color
            ),
            name = 'Valgt Gruppe'
          ) %>% 
            # Add control gorup
            # trace.
            add_trace(
            x = ~x,
            y = ~control,
            line = list(
              color = control_color
            ),
            marker = list(
              color = control_color
            ),
            name = 'Sammenligningsgruppe'
          )
          
        }
      )
      
      
      
    }
  )
  
  # return; ####
  return(
    structure(plot, class = "model1")
  )
}



.baseplot_model2 <- function(
    plot = NULL
) {
  
  #' function information
  #' 
  #' @param plot a list of plots
  #' 
  #' @returns a nested list of plots
  #' based on the types
  
  
  # TODO:
  # 1) Add Baseline Option
  # 2) Add Plot Names of Y axis
  # 3) Move Legend and print only one.
  
  plot <- map(
    plot,
    .f = function(plot) {
      
      plot %>% add_trace(
        y = "Valgt Gruppe",
        x = ~intervention,
        marker = list(
                      
                      line = list(color = 'black',
                                  
                                  width = 1.5))
        ) %>% add_trace(
        y = "Sammenligningsgruppe",
        x = ~control,
        marker = list(
                      
                      line = list(color = 'black',
                                  
                                  width = 1.5))
      )
      
    }
  )
  
  # return; ####
  return(
    structure(plot, class = "model2")
  )
}




baseplot <- function(plot) {
  
  #' function information
  #' 
  #' @param plot a list of plots
  #' 
  #' @returns a nested list of plots
  #' based on the types
  
  # function logic; ####
  if (inherits(plot, "model1")){
    
    
    plot <- .baseplot_model1(plot = plot)
    
  } else {
    
    plot <- .baseplot_model2(plot = plot)
  }
  
  # return; ####
  return(
    plot
  )
}