# script: Grinder
# objective: Create a wrapper function for all related grinder
# functions.
# date: 2022-07-29
# author: Serkan Korkmaz


.grind_model1 <- function(
    data_list,
    intervention = NULL,
    control = NULL,
    allocators = NULL,
    chars = NULL,
    do_incidence = NULL
) {
  
  
  #' function information;
  #' 
  #' 
  #' @param data_list a list of data.tables
  #' from model1.
  
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
  
  
  
  chars_ <- chars
  iterator <- 0
  
  
  
  
  # function logic; ####
  data_list <- map(
    data_list,
    .f = function(data) {
      
      
      iterator <<- iterator + 1
      
      # WARNING: If not specified
      # this will break.
      data[
        #assignment %chin% 'matching',
        str_detect(assignment, 'matched'),
        type := do_incidence
      ]
      
      
      # filter according to incidence
      if (do_incidence != 0) {
        
        data <- data[
          type == do_incidence
        ]
        
      }
      
      
      
      # filter according to characteristics
      if (do_char) {
        
        
        extract_ids <- extract_id(
          lookup = lookup[[1]][[iterator]],
          values = chars
        )


        data <- data[
          id %in% extract_ids
        ]
        
        
        
        # data <- data[
        #   chars %chin% chars_
        # ]
        
      }
      
      
      # classify data
      data <- data[
        assignment %chin% c(intervention, control, paste0("matched_", intervention)) &
          allocator %chin% c(allocators) 
      ]
      
      
      # 2) classify accordingly
      data[
        ,
        assignment_factor := fcase(
          assignment %chin% paste0("matched_", intervention), "population",
          assignment %chin% intervention, "intervention",
          assignment %chin% control, "control"
        )
        ,
      ]
      
      
      setnames(
        data,
        old = c("year"),
        new = c("x"),
        skip_absent = TRUE
      )
      
      return(
        data
      )
      
    }
  )
  
}



.grind_model2 <- function(
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
    
   
    
    data_list <- .grind_model1(
      data_list,
      intervention = intervention,
      control = control,
      allocators = allocators,
      chars = chars,
      do_incidence = do_incidence
    )
    
    
  }
  
  if (inherits(data_list, 'model2')) {
    
    data_list <- .grind_model2(
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








