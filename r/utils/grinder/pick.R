# script: scr_pick
# date: Sat Oct 22 18:56:50 2022
# author: Serkan Korkmaz
# objective: Create a class of functions
# that picks data for further

# model 1 pick; ####

.model1_pick <- function(
  data_list = data_list,
  intervention,
  control   = NULL
  ) {
  
  #' function information
  #' 
  #' 
  #' @param data_list a list of data.tables for
  #' model 1.
  
  # 1) Extract the class of the
  # list for appending at return
  extract_class <- class(data_list)
  
  
  data_list <- map(
    data_list,
    function(element) {
      
      # 1) Filter data
      # according to choices
      element <- element[
        assignment %chin% c(
          intervention,
          control,
          paste0(
            'matched_', intervention
          )
        )
      ]
      
      # 2) Classify Data
      # accoringly
      element[
        ,
        assignment_factor := fcase(
          assignment %chin% intervention, 'intervention',
          assignment %chin% control, 'control',
          default = fifelse(
            isTRUE(str_detect(control, '[:alpha:]+')), 'population', 'control'
          )
        )
        ,
      ]
      
      return(
        element
      )
      
    }
  )
  
  
  return(
    structure(
      data_list,
      class = extract_class
    )
  )
  
}




# wrapper for the picker; ####
pick <- function(
    data_list = data_list,
    intervention,
    control   = NULL
) {
  
  if (inherits(data_list, 'model1')) {
    
    data_list <- .model1_pick(
      data_list     = data_list,
      intervention  = intervention,
      control       = control
    )
    
    
  }
  
  
  return(
    data_list
  )
  
}