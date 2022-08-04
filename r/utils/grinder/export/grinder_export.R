# script: Grinder
# objective: Grinder for export ready data.
# date: 2022-07-29
# author: Serkan Korkmaz



# model 1; ####
.export_model1 <- function(
    data_list
) {
  
  
  
  #' function information;
  #' 
  #' @param data_list a nested of list of data.tables 
  #' from model1.
  
  data_list <- map(
    data_list,
    .f = function(data) {
      
      if (nrow(data) != 0) {
        
        
        # extract IDs
        fetch_id <<- unique(
          data$id
        )
        
        
        # Cast Outcome Type
        data <- dcast(
          data,
          value.var = 'outcome',
          formula = ... ~ outcome_type
        )
        
        
        data <- data[
          ,
          .(
            cost = mean(cost),
            qty = mean(qty)
          )
          ,
          by = .(x, assignment, allocator, id, price)
        ][
          ,
          .(
            cost = mean(cost),
            qty  = mean(qty)
          )
          ,
          by = .(x, assignment, allocator, price)
        ]
        
        return(
          data
        )
        
      }
      
      
    }
  )
  
  
  # generate list; ####
  data_list <- structure(
    list(
      data = data_list,
      id   = fetch_id
    ), class = c('model1', 'export')
  )
  
  
  
  # return; ####
  
  return(
    data_list
  )
  
}


# model 2; ####
.export_model2 <- function(
    data_list
) {
  
  #' function information;
  #' 
  #' @param data_list a nested of list of data.tables 
  #' from model1.
  
  data_list <- map(
    data_list,
    .f = function(data) {
      
      if (nrow(data) != 0) {
        

        # extract IDs
        fetch_id <<- unique(
          data$id
        )
        
        # extract class
        get_class <- class(data)
        
        
        data <- data[
          ,
          .(
            outcome = mean(outcome)
          )
          ,
          by = .(id, assignment, allocator)
        ][
          ,
          .(
            outcome = mean(outcome)
          )
          ,
          by = .(assignment, allocator)
        ]
        
        
        
        data <- dcast(
          data,
          value.var = 'outcome',
          formula   = ... ~ assignment
        )
        
        
        
        return(
          data
        )
        
        
        
      }
      
      
    }
  )
  
}







#' main function; ####
export_model <- function(
    data_list
) {
  
  
  
  #' function information
  #' 
  #' A wrapper function of exporters.
  
  if (inherits(data_list, 'model1')) {
    
    
    
    data_list <- .export_model1(
      data_list = data_list
    )
    
  }
  
  
  if (inherits(data_list, 'model2')) {
    
    data_list <- .export_model2(
      data_list = data_list
    )
    
  }
  
  # return; #####
  return(
    data_list
  )
  
}


