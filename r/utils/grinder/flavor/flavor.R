


.flavor_model1 <- function(data_list, effect, do_match = FALSE) {
  
  #' function information
  #' 
  #' This function adds effects to the spreaded data
  #' of model 1.
  #' 
  #' @param data_list a list of data.tables from the spread
  #' funtion.
  #' 
  #' @param effect a vector of effects as numerics of length
  #' 5.
  #' @param match a logical value. Defaults to FALSE. If TRUE
  #' then the comparision is the population based on matching.
  
  # Handle errors; ####
  if (length(effect) < 5) {
    
    warning('Effects are less than 5, extending effects with last value carried forward.', call. = FALSE)
    
    effect <- c(
      effect,
      rep(effect[length(effect)], 5 - length(effect))
    )
    
    
  }
  
  
  list_names <- names(data_list)
  
  
  
  # At app startup, the inputs
  # arent, necessarily, rendered
  # first. Using fcase mitigates this
  # potential issue.
  do_match <- fcase(
    isTRUE(do_match), TRUE,
    default = FALSE
  )
  
  # function logic; #####
  counter <- 0
  
  data_list <- map(
    seq_along(data_list),
    .f = function(i) {
      
      counter <<- counter + 1
      
      # Set counter to avoid
      # redundant verbosing
      if (counter == 1) {
        
        message(
          'Adding flavors!'
        )
        
      }
      
      # Copy data to avoid, overwriting
      # data in memory
      # TODO: Test with and without
      data <- copy(
        data_list[[i]]
      )
      
      if (is.null(data)) {
        return(NULL)
      }
      
      
      
      # Recalculate difference
      # if matching is chosen.
      if (do_match) {
        
        # 1) Replace Control
        # with matching population
        data[
          ,
          control := population
          ,
        ]
        
        
        data[
          ,
          difference := rowSums(
            cbind(-control, intervention)
          )
          ,
        ]
        
      }
      
      
      data[
        x > 0,
        effect := (effect/100)
        ,
        by = .(allocator)
      ][
        ,
        `:=`(
          cdifference   = difference * effect,
          cintervention = pmax(
            intervention - ((difference * effect))
            ,0
          )
        )
        ,
        by = .(allocator)
      ][
        x == 0,
        cintervention := intervention
      ]
      
      
      
      
      
      return(data)
      
      
      
    }
  )
  
  names(data_list) <- list_names
  # return; ####
  
  
  
  return(
    data_list
  )
  
  
}




.flavor_model2 <- function(data_list, effect) {
  
  #' function information
  #' 
  #' This function adds effects to the spreaded data
  #' of model 2.
  #' 
  #' @param data_list a list of data.tables from the spread
  #' funtion.
  #' 
  #' @param effect a vector of effects as numerics of length
  #' 5.
  #' @param match a logical value. Defaults to FALSE. If TRUE
  #' then the comparision is the population based on matching. 
  
  # Handle errors; ####
  if (length(effect) > 1) {
    
    warning('Effects are greater than 1. Using the first value.', call. = FALSE)
    
    effect <- c(
      effect[1]
    )
    
  }
  
  
  data_list <- map(
    seq_along(data_list),
    .f = function(i) {
      
      # Copy data to avoid, overwriting
      # data in memory
      # TODO: Test with and without
      data <- copy(
        data_list[[i]]
      )
      
      if (is.null(data)) {
        return(NULL)
      }
      
      
      # extract the class of the 
      # data
      #get_class <- class(data)[3]
      
      # Calculate effects
      data[
        ,
        `:=`(
          cdifference = difference * effect/100,
          cintervention = max(intervention - ((difference * (effect/100))),0)
        )
        ,
      ]
      
      
      #class(data) <- c(class(data), get_class)
      
      return(data)
      
    }
  )
  
}





flavor <- function(data_list, effect, do_match = FALSE) {
  
  #' function information
  #' 
  #' Wrapper function of added effects (flavors)
  #' depends on the class of the @param data_list
  
  is_export <- inherits(data_list, 'export')
  
  message(
    paste('Is Export:', is_export)
  )
  
  if (is_export) {
    
    message("in export")
    
    id_data   <- data_list[[2]]
    data_list <- data_list[[1]]
    
    
  }
  
  
  
  
  if (inherits(data_list, 'model1')) {
    
    data_list <- .flavor_model1(
      data_list,
      effect,
      do_match
    )
    
    class(data_list) <- c(class(data_list), 'model1')
    
  }
  
  if (inherits(data_list, 'model2')) {
    
    data_list <- .flavor_model2(
      data_list,
      effect
    )
    
    class(data_list) <- c(class(data_list), 'model2')
    
  }
  
  if (is_export) {
    
    
    
    data_list <- list(
      data_list,
      id_data
    )
    
  }
  
  
  
  
  return(
    data_list
  )
  
}

