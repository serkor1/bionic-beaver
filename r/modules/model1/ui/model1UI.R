# script: model1UI
# objective: Collect all UI elements of Model1
# in a single wrapper function
# date: 2022-07-22
# author: Serkan Korkmaz


model1UI <- function(
    id,
    output
)
{
  
  #' function information
  #' 
  #' This function has the following
  #' @import model1_body which includes
  #' all the content for model1.
  
  # generate namespace; #####
  ns <- NS(id)
  
  
  # function logic; ####
  
  ui_body <- .model1UI_body(
    id, output
  )
  
  
  # return statement; ####
  return(
    list(
      body       = ui_body
    )
    
  )
  
  
}