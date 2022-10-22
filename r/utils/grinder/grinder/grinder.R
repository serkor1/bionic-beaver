# script: Grinder
# objective: Create a wrapper function for all related grinder
# functions.
# date: 2022-07-29
# author: Serkan Korkmaz


.model1_grind <- function(
    data_list,
    recipe = list(
      outcome_measure = 'qty', 
      incidence = 1
    )
    
    # ,
    # options = NULL,
    # allocators = NULL,
    # chars = NULL,
    # do_incidence = NULL
) {
  
  
  #' function information
  #' 
  #' @param data_list a list of data.tables from
  #' model 1.
  #' 
  #' @param recipe a named list of chosen
  #' filtering parameters
  
  
  # function logic; ####
  data_list <- map(
    data_list,
    .f = function(element) {
      
      # NOTE: Had class function innit
      element <- copy(element)
      
      # 1) Fill NA values
      # in the type variables
      element[str_detect(assignment, 'matched_'), type := recipe$incidence]
      
      # 2) Filter data according
      # to the recipe
      element <- element[
        outcome_type %chin% recipe$outcome_measure 
      ]
      
      if (recipe$incidence != 0) {
        
        element <- element[
          type == recipe$incidence
        ]
        
      }
      
      
      # 3) Set year to x
      # for normalized approach
      setnames(element, old = 'year', new = 'x', skip_absent = TRUE)
      
      return(
        element
      )
      
    }
  )
  
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
  
  
  #' function information
  #'
  #'
  #'
  #'
  
  
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
      master_class, fifelse(
        !has_char, 'aggregate', 'no_aggregate'
        )
    )
  )
    
    return(
      data_list
    )
}
   