.spread_model1 <- function(
  data_list,
  export = FALSE,
  alternate = FALSE
) {
  
  
  #' function information; 
  
  
  # # global options
  # export <- fcase(
  #   isTRUE(export), TRUE,
  #   default = FALSE
  # )
  # 
  alternate <- fcase(
    isTRUE(alternate), 'qty',
    default = "cost"
  )
  
  
  # function logic; ####

  if (export) {
    
    data_list <- .export_model1(
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
        
        
        get_class <- class(data)
        
        
        data <- data[
          outcome_type %chin% alternate
        ]
        
        
        group_cols <- .find_cols(
          cols = colnames(data),
          pattern = c("x", "year", "assignment", "allocator", "allocator"),
          negate = FALSE
        )
        
        data <- data[
          ,
          list(
            outcome = mean(
              outcome, na.rm = TRUE
            )
          )
          ,
          by = c(group_cols)
        ]
        
        
        data <- dcast(data,
          x + allocator ~ assignment_factor,
          value.var = "outcome"
        )
        
        
        
        # check if control group is
        # chosen
        control <- str_detect(
          string  = 'control',
          pattern = paste(
            colnames(data),
            collapse = "|"
          )
        )


        if (!control) {

          data[
            ,
            control := population
            ,
          ]

        }

         data <- data[
          x >= 0
          ,
             `:=`(
               cintervention = 0,
               difference = rowSums(cbind(-control, intervention)),
               cdifference = 0
             )

             ,
         ]
        
        
        class(data) <- c(class(data), get_class[length(get_class)])

        return(
          data
        )
        
        
      }
    )
    
  }
  
  
  
  
  
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
        
        
        data <- data[
          ,
          .(
            outcome = mean(outcome)
          )
          ,
          by = .(id, assignment_factor, allocator)
        ][
          ,
          .(
            outcome = mean(outcome)
          )
          ,
          by = .(assignment_factor, allocator)
        ]
        
        
        
        data <- dcast(
          data,
          value.var = 'outcome',
          formula   = ... ~ assignment_factor
        )
        
        
        data[
          ,
          `:=`(
            difference = rowSums(
              cbind(-control, intervention)
            ),
            cdifference = NA,
            cintervention = NA
          )
          
          ,
        ]
        
        
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
    export = FALSE,
    alternate = FALSE
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
      export    = export,
      alternate = alternate
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
  
  # return statement; #####
  
  return(
    structure(
      data_list,
      class = c(master_class)
    )
  )
  
  
}