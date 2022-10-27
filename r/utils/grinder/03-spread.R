# script: scr_spread
# date: Sat Oct 22 21:01:45 2022
# author: Serkan Korkmaz
# objective: Generate a class of functions
# that filters data by characteristics and spreads
# it for further flavoring
# prelims; #####

.spread_model1 <- function(
  data_list,
  values = NULL
) {
  
  
  #' function information; 
  
  

  # }
 
  
  # function logic; ####

  i <- 0
  
  data_list <- map(
    data_list,
    .f = function(element) {
      
      if (nrow(element) == 0) {
        
        return(
          NULL
        )
        
      }
      
      i <<- i + 1
      
      
      
      # 1) Filter the data
      # according to chosen characteristics
      if (isTruthy(values)) {
        
        element <- element[
          id %in% extract_id(
            lookup = lookup[[1]][[i]],
            values = values
          )
        ]
        
      }
      
      # 2) Calculate the
      # weighed outcomes
      element <- element[
        ,
        .(
          outcome = sum(
            outcome * weight, na.rm = TRUE
          )/sum(weight, na.rm = TRUE)
          )
        ,
        by = .(
          x, allocator, assignment,assignment_factor
        )
      ]
      # 
      # 2) Cast the data with the 
      # id in the data.
      element <- dcast(
        element,
        formula = x + allocator ~ assignment_factor,
        value.var = 'outcome'
        #,fun.aggregate = mean
      )
      
      return(
        element
      )
      
    }
  )

  
  return(
    data_list
  )
  
}




.spread_model2 <- function(
    data_list,
    export = FALSE
) {
 
  
  #' function information;
  #' 
  #' 
  #' 
  #' 
  
  
  # function logic
  if (export) {
    
    data_list <- .export_model2(
      data_list
    )
    
  } else {
    
    data_list <- map(
      data_list,
      .f = function(data) {
        
        if (nrow(data) == 0) {
          
          return(
            NULL
          )
        }
        
        # extract class
        get_class <- class(data)
        
        
        # data <- data[
        #   ,
        #   .(
        #     outcome = sum(outcome * weight)
        #   )
        #   ,
        #   by = .(id, assignment_factor, allocator)
        # ][
        #   ,
        #   .(
        #     outcome = sum(outcome * weight)
        #   )
        #   ,
        #   by = .(assignment_factor, allocator)
        # ]
        
        
        
        data <- data[
          ,
          .(
            outcome = sum(
              outcome * weight, na.rm = TRUE
            )/sum(weight, na.rm = TRUE)
          )
          
          ,
          by = .(
            assignment, allocator
          )
        ]
        
        
        # data <- data[
        #   ,
        #   .(
        #     outcome = mean(
        #       outcome, na.rm = TRUE
        #     )
        #   )
        #   ,
        #   by = .(id, assignment,allocator)
        # ][
        #   ,
        #   .(
        #     outcome = mean(outcome)
        #   )
        #   ,
        #   by = .(
        #     assignment,allocator
        #   )
        # ]
        
        
        
        # data <- dcast(
        #   data,
        #   value.var = 'outcome',
        #   formula   = ... ~ assignment_factor
        # )
        
        
        # data[
        #   ,
        #   `:=`(
        #     difference = rowSums(
        #       cbind(-control, intervention)
        #     ),
        #     cdifference = NA,
        #     cintervention = NA
        #   )
        #   
        #   ,
        # ]
        
        
        class(data) <- c(class(data), get_class[length(get_class)])
        
        
        
        return(
          data
        )
        
        
      }
    )
    
    
  }
  
  
  
  
  
   
}




spread <- function(
    data_list,
    export    = FALSE,
    alternate = FALSE,
    values   = NULL,
    options   = list(
      outcome_measure = 'qty', 
      incidence = 1
    )
    ) {
  
  #' function information; #####
  #' 
  #' 
  #' This function takes a grinded @param data_list
  #' and spreads the values for further flavoring (Added Effects)
  #' 
  #' @param data_list a list of data.tables 
  #' 
  #' @returns a list of spreaded data tables.
  
  
  message('Spreading data')
  
  # function preliminaries; #####
  export <- fcase(
    isTRUE(export), TRUE,
    default = FALSE
  )
  
  alternate <- fcase(
    isTRUE(alternate), TRUE,
    default = FALSE
  )
  
  
  # extract the parent class of the list
  # this is either model1 or model2.
  master_class <- class(data_list)
  
  
  
  # function logic; #####
  

  if (inherits(data_list, 'model1')) {
    
    
    data_list <- .spread_model1(
      data_list = data_list,
      values = values
    )
    
    
  } else {
    
    data_list <- .spread_model2(
      data_list = data_list,
      export    = export
    )
    
  }
  
  export <- fcase(
    isTRUE(export), "export",
    default = NULL
  )
  
  
  class_string <- c(master_class, export)
  
  # return statement; #####
  
  return(
    structure(
      data_list,
      class = class_string[!sapply(class_string, is.na)]
    )
  )
  
  
}



