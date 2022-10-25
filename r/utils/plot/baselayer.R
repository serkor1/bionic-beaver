# script: baselayer
# objective: create functions that generates baseplots
# for the data
# author: Serkan Korkmaz
# date: 2022-06-16


.baselayer_model1 <- function(data_list) {
  
  #' function information
  #' 
  #' @param data_list
  
  
  
  # function logic; ####
  # Generate Empty Plot
  plot <- map(
    data_list,
    
   
    
    .f = function(data) {
      
     
      tryCatch(
        {
          # iterator <- 0
          
          
          data %>% 
            split(.$allocator) %>% 
            map(
              .f = function(unique_data) {
                
                # iterator <<- iterator + 1 
                
                
                plot_ly(
                  data   = unique_data,
                  type   = "scatter",
                  mode   = "lines+markers",
                  # showlegend = fifelse(
                  #   iterator == 1, TRUE, FALSE
                  # )
                  mode   = TRUE
                )
                
              }
            )
        },
        error = function(cond) {
          
          
          NULL
          
        }
      )
      
      
      
      
    }
  )
  
  
  
  
  # return statement; ####
  return(
    structure(plot, class = "model1")
  )
  
  
}



.baselayer_model2 <- function(data_list) {
  
  #' function information
  #' 
  #' @param data_list 
  
  
  
  
  
  iterator <- 0
  # function logic; ####
  # Generate Empty Plot
  plot <- map(
    data_list,
    
    
    
    .f = function(data) {
      
      
      iterator <<- iterator + 1 
      
      
      plot_ly(
        data   = data,
        type   = "bar",
        orientation = 'h',
        showlegend = FALSE
        # showlegend = fifelse(
        #   iterator == 1, TRUE, FALSE
        # )
      )
      
    }
  )
  
  # return statement; ####
  return(
    structure(plot, class = "model2")
  )
  
  
}




baselayer <- function(data_list) {
  
  #' function information
  #' 
  #' @param data_list a nested data_list of 
  #' length 1, ie. one list with nested lists
  #' 
  #' @returns a list of plots for each type
  #' of plot
  
  # function logic; ####
  # 1) Extract Classes of each dataset in the
  # nested list
  is_children <- unlist(sapply(
    data_list, class
  ))
  
  # 2) Check if it includes the children
  # class
  is_children <- sum(is_children %chin% "children") > 0
  
  if (is_children) {
    
    
    
    plot <- .baselayer_model2(
      data_list
    )
    
    
    
  } else {
    
    plot <- .baselayer_model1(
      data_list
    )
    
  }
  
  # return; ####
  return(
    plot
  )
  
}


