# script: Development Tools
# objective: This script is for development tools
# only, not related to the app it self.
# author: Serkan Korkmaz
# date: 2022-06-17


dev_load_data <- function(get_all = TRUE, developper_mode = TRUE) {
 
   
  #' function information
  #' 
  #' This function loads all the
  #' data.
  
  
  # load data; #####
  data_list <- preload_data(
    developper_mode = developper_mode
  )
  
  # load parameters; #####
  

  
  return(
    data_list
  )
  
}




dev_load_char <- function(
    data_list,
    model = 'model1'
    ) {
  
  
  .extract_element <- function(element) {
    
    unlist(
      map(
        element,
        .f = function(element) {
          
          sample(
            
            element,
            
            min(
              length(element),
              2
            )
          )
          
        }
      )
    )
    
    
    
    
  }
  
  
  load_parameters <- .gen_option(
    data_list = data_list
  )
  
  
  chars      <- load_parameters$chars
  assignment <- load_parameters$assignment
  outcome    <- load_parameters$outcome
  lookup     <- .gen_lookup(data_list)
  
  
  if (str_detect(model, 'model1')) {
    
    list(
      chars = sample(.extract_element(chars[[1]]),3),
      assignment = sample(.extract_element(assignment[[1]]),2),
      outcome = .extract_element(outcome[[1]]),
      lookup = lookup[[1]]
    )
    
  } else {
    
    list(
      chars = .extract_element(chars[[2]]),
      assignment = .extract_element(assignment[[2]]),
      outcome = .extract_element(outcome[[2]]),
      lookup = lookup[[2]]
    )
    
  }
  
  
  
  
}


