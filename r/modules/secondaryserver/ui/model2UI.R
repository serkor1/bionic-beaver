# script: model2UI
# objective: Collect all UI elements of
# the second model
# author: Serkan Korkmaz
# date: 2022-06-12


model2UI <- function(id, output) {
  
  # generate namesace
  ns <- NS(id)
  
  # generate body
  ui_body <- model2UI_body(
    id,
    output
  )
  
  
  return(
    list(
      body = ui_body
    )
  )
  
  
}