# script: pt_counter
# authour: Serkan Korkmaz
# date: 2022-11-30
# description: Add a patient counter
# based on chosen parameters
# prelims; ####

pt_counter <- function(
  values = NULL,
  intervention,
  control = NULL,
  incidence = 1
  ) {
  
  
  #' function information
  #' 
  #' 
  #' @param values all chosen characteristics
  #' based on the model
  #' 
  #' @param intervention a character vector
  #' of length 1, with the chosen intervention group
  #' cant be empty
  #' 
  #' @param control a character vector of length 1. Can be NULL.
  #' if NULL the general population is chosen
  
  # function logic; #####
  
  # 1) Extract the class of the
  # list for appending at return and
  # indicator for chosen control
  indicator     <- isTRUE(
    str_detect(control, '[:alpha:]+')
  )
  
  # 1) Take a shallow copy
  # of the data and filter
  # according to chosen parameters
  data <- copy(
    pt_list
  )[
    assignment %chin% c(
      intervention,
      control,
      paste0(
        'matched_', intervention
      )
    )
  ]
  
  # 2) Classify data
  # accoringly
  data[
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
  
  # 3) Add type indicator
  # to avoid NA trap
  data[
    str_detect(assignment, 'matched_'),
    type := incidence
  ]
  
  # 4) Filter data
  # according to type
  # if 1 is chosem
  if (incidence == 1) {
    data <- data[
      type == 1
    ]
  }
  
  
  # 5) Filter according to chosen
  # values if any
  if (isTruthy(values)){
    
    data <- data[
      id %in% extract_id(
        lookup = lookup[[1]][[2]],
        values = values
      )
    ]
    
  }
  
  # 6) Prepare data to
  # return
  data[
    ,
    .(
      obs = sum(
        # Was unique
        unique(
          total_n
        ),
        na.rm = TRUE
      )
    )
    ,
    by = .(
      assignment_factor
    )
  ]
  
  
}


