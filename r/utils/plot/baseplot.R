# script: 
# objective: 
# author: Serkan Korkmaz
# date: 2022-06-16

.baseplot_model1 <- function(
  plot = NULL
  ) {
  
  #' function information
  #' 
  #' @param plot a list of plots
  #' 
  #' @returns a nested list of plots
  #' based on the types
  
  plot <- map(
    plot,
    .f = function(plot) {
      
      plot %>% map(
        .f = function(plot) {
          
          plot %>% add_trace(
            x = ~x,
            y = ~intervention
          ) %>% add_trace(
            x = ~x,
            y = ~control
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
  
  plot <- map(
    plot,
    .f = function(plot) {
      
      plot %>% add_trace(
        y = "Valgt Gruppe",
        x = ~intervention
      ) %>% add_trace(
        y = "Sammenligningsgruppe",
        x = ~control
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
    
    message("In base 1")
    
    plot <- .baseplot_model1(plot = plot)
    
  } else {
    
    plot <- .baseplot_model2(plot = plot)
  }
  
  # return; ####
  return(
    plot
  )
}