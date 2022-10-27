# script: 01-scr_pick
# date: Sat Oct 22 18:56:50 2022
# author: Serkan Korkmaz
# objective: Create a class of picker functions
# that filters the chosen assignments.

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
  #' 
  #' @param intervention a character vector of length 1.
  #' has to be provided.
  #' 
  #' @param control a character vector of length 1. NULL
  #' by default which choses the general population.
  
  # function logic; #####
  
  # 1) Extract the class of the
  # list for appending at return and
  # indicator for chosen control
  extract_class <- class(data_list)
  indicator     <- isTRUE(
    str_detect(control, '[:alpha:]+')
    )
  
  # 2) Elementwise picking
  # of intervention and control.
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
          
          # If Control is NULL (Empty), then 
          # default will come in play and
          # output control for the matched
          # population. Otherwise it is
          # called population such that it can be
          # plotted and presented along side 
          # the chosen control.
          default = fifelse(
            indicator,
            yes = 'population',
            no = 'control'
          )
        )
        ,
      ]
      
      return(
        element
      )
      
    }
  )
  
  
  # return statement; ####
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
  
  #' function information
  #' 
  #' 
  #' @param data_list a list of data.tables for
  #' model 1.
  #' 
  #' @param intervention a character vector of length 1.
  #' has to be provided.
  #' 
  #' @param control a character vector of length 1. NULL
  #' by default which choses the general population.
  message('Picking data')
  
  # function logic; ####
  if (inherits(data_list, 'model1')) {
    
    data_list <- .model1_pick(
      data_list     = data_list,
      intervention  = intervention,
      control       = control
    )
    
    
  }
  
  # return statement; #####
  return(
    data_list
  )
  
}