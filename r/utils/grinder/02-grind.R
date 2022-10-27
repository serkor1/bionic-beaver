# script: 02-grind
# date: Thu Oct 27 09:02:41 2022
# author: Serkan Korkmaz
# objective: Create a class of functions
# that grinds the picked data for further 
# manipulation.


# model1 grinder; #####
.model1_grind <- function(
    data_list,
    recipe = list(
      outcome_measure = 'qty', 
      incidence = 1
    )
) {
  
  
  #' function information
  #' 
  #' @param data_list a list of data.tables from
  #' model 1.
  #' 
  #' @param recipe a named list of chosen
  #' filtering parameters on the form:
  #' @param outcome_measure a chacter vector of length 1, either
  #' qty or cost.
  #' @param incidence a numeric vector of length 1, either 1 (Incidence). 
  
  
  # function logic; ####
  
  # 1) Generate incidence
  # indicator according the the 
  # recipe
  indicator <- recipe$incidence == 1 # Was recipe$incidence != 0
  
  data_list <- map(
    data_list,
    .f = function(element) {
      
      # 1) copy the data to avoid
      # overwriting data in memory.
      # TODO: This might not necessary be an issue
      # test this at a later point in time.
      element <- copy(
        element
        )
      
      # 2) Fill NA values
      # in the type variables
      # NOTE: The function breaks if this
      # step is missed.
      # 
      # Has to be for the assignment variable
      # containing the matched_ prefix.
      element[
        str_detect(assignment, 'matched_'),
        type := recipe$incidence
        ]
      
      # 3) Follow the recipe
      # by chosing outcome measure, and
      # if needed be incidence.
      element <- element[
        outcome_type %chin% recipe$outcome_measure 
      ]
      
      if (indicator) {
        
        element <- element[
          type == recipe$incidence
        ]
        
      }
      
      
      # 4) Set year to x
      # for normalized approach
      setnames(
        element,
        old = 'year',
        new = 'x',
        skip_absent = TRUE
        )
      
      return(
        element
      )
      
    }
  )
  
  
  # return; ####
  return(
    data_list
  )
  
}



.model2_grind <- function(
    data_list,
    intervention = NULL,
    control = NULL,
    allocators = NULL,
    chars = NULL
) {
  
  
  # NOTE: This 
  
  
  # global options
  char_logical <- isTRUE(
    str_detect(
      string = paste(chars, collapse = "|"),
      pattern = "[:alpha:]"
    )
  )
  
  do_char <- fcase(
    char_logical, TRUE,
    default = FALSE
  )
  
  
  data_list <- map(
    data_list,
    .f = function(data) {
      
      if (nrow(data) == 0) {
        
        return(
          NULL
        )
        
      }
      
      if (do_char) {
        
        extract_ids <- extract_id(
          lookup = lookup[[2]],
          values = chars
        )
        
        
        data <- data[
          id %in% extract_ids
        ]
        
      }
      
      #1) Extract Allocators
      data <- data[
        #allocator %chin% allocators
        str_detect(allocator, pattern = paste(allocators, collapse = '|'))
      ]
      
      data <- data[
        str_detect(as.character(assignment), pattern = paste(intervention, collapse = '|'))
      ]
      
      
      # 2) classify accordingly
      data[
        ,
        assignment_factor := fcase(
          as.character(assignment) %chin% intervention, "intervention",
          as.character(assignment) %chin% control, "control"
        )
        ,
      ]
      
      
      
      
      
    }
  )
  
  
  
  
  
}



grind <- function(
    data_list,
    recipe = list(
      outcome_measure = 'qty', 
      incidence = 1
    ),
    intervention = NULL,
    control = NULL,
    allocators = NULL,
    chars = NULL,
    alternate = FALSE,
    export = FALSE,
    do_incidence = FALSE
) {
  
  #' Function Information
  #' 
  #' @param data_list a list of data.tables regardless of the model
  #' chosen
  #' 
  #' @param intervention a character vector of length 1.
  #' @param control a character vector of length 1.
  #' @param allocator a character vector of lenth N.
  #' @param chars a character vector of length N.
  #' @param alternate a logical value, indicating wether alternate
  #' versions of the data are to be calculated.
  #' @param export logical. Should the data be prepared for exporting.
  
  
  
  # global function options; #####
  message('Grinding data')
  
  
  get_char <- chars

  has_char <- str_detect(
    paste(get_char, collapse = ""),
    pattern = '[:alpha:]'
  )
  alternates <- fcase(
    isFALSE(alternate), "qty",
    default = "cost"
  )

  do_incidence <- fcase(
    isTRUE(do_incidence), 1,
    default = 0
  )
  
  
  # check_char <- length(chars) > 1
  
  
  master_class <- c(
    class(data_list)
    )
  
  # global data manipulation; ####
  
  if (inherits(data_list, 'model1')) {
    
    data_list <- .model1_grind(
      data_list,
      recipe = recipe
    )
    
    
  } else {
    
    data_list <- .model2_grind(
      data_list,
      intervention = intervention,
      control = control,
      chars = chars,
      allocators = allocators
    )
    
  }
  

  
  
  data_list <- structure(
    data_list,
    class = c(
      master_class
      # , fifelse(
      #   !has_char, 'aggregate', 'no_aggregate'
      #   )
    )
  )
    
    return(
      data_list
    )
}
   