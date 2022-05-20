# script; UI of
# the main model.


model1UI <- function(id, output) {
  
  # Generate Namespace
  ns <- NS(id)
  
  
  # Generate Sidebar; ####
  ui_sidebar <- model1UI_options(id, output)
  
  # Generate Hader; ####
  ui_header <- model1UI_header(id, output)
  
  ui_body <- model1UI_body(id, output)
  
  
  ui_performance <- NULL
  
  
  #   model1UI_performance(
  #   id,output
  # )
  #   
  #   
  
  
  
  
  return(
    list(
      sidebar    = ui_sidebar,
      body       = ui_body,
      header     = ui_header,
      performance = ui_performance
    )
    
  )
  
  
}