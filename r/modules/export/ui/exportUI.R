# script: export Module UI
# objective: This script is a wrapper script of the existing 
# ui elements.
# date: 2022-07-25
# author: Serkan Korkmaz

exportUI <- function(
    id,
    output,
    input
) {
  
  #' function information
  #' 
  #' 
  #' 
  #' 
  #' 
  
  # function logic;
  ui_body <- .exportUI_body(
    id = id,
    input = input,
    output = output
  )
  
  # return statement
  return(
    list(
      body = ui_body
    )
  )
  
  
  
}