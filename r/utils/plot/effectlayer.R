# script: effect layer
# objective: Add an additional layer of effects
# to the existing plots
# author: Serkan Korkmaz
# date: 2022-06-16


.effectlayer_model1 <- function(plot){
  
  #' function information
  #' 
  #' 
  
  # function logic; ####
  plot <- map(
    plot,
    .f = function(plot) {
      
      plot %>% map(
        .f = function(plot) {
          
          plot %>% add_trace(
            x = ~x,
            y = ~cintervention
          ) 
          
          
        }
      )
      
    }
  )
  
  # return; ####
  return(
    plot
  )
  
}




.effectlayer_model2 <- function(plot) {
  
  #' function information
  #' 
  
  # function logic; ####
  plot <- map(
    plot,
    .f = function(plot) {
      
      plot %>% add_trace(
        x = ~cintervention,
        y = "Kontrafaktisk Værdi"
      ) 
      

    }
  )
  
  # return; #####
  return(plot)
  
  
}



effectlayer <- function(plot) {
  
  #' Information
  
  # function logic; ####
  if (inherits(plot, "model1")){
    
   
    plot <- .effectlayer_model1(
      plot = plot
    )
    
    
  } else {
    
    plot <- .effectlayer_model2(
      plot = plot
      )
  }
  
  # return statement
  return(
    plot
  )
  
  
}
