# script: scr_flavor
# date: Sat Oct 22 21:02:23 2022
# author: Serkan Korkmaz
# objective: Generate a class of functions that
# flavers the data according according to the
# choice of the user.
# prelims; #####


.model1_flavor <- function(
    data_list, 
    effect,
    extrapolate = FALSE,
    allocators = NULL
    ) {
  
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
      
      
      
      data <- data[
        allocator %chin% allocators
      ]
      
      
      if (nrow(data) == 0) {
        
        return(
          NULL
        )
        
      }
      
      
      
      
      # 1) locate relevant 
      # columns
      idx <- which(
        sapply(
          colnames(data),
          str_detect,
          'control|intervention|population'
        )
      )
      
      data <- data[
        ,
  
          lapply(
            .SD,
            sum,
            na.rm =TRUE
          )
        
        
        ,
        by = .(
          x, allocator
        ),
        .SDcols = idx
      ]
      
      
      # If control exists
      counter_factual <- fifelse(
        c('control') %chin%  colnames(data),
        yes = 'control',
        no = 'population'
      )
      
      
      data[
        ,
        `:=`(
          difference = rowSums(
            cbind(
              -get(counter_factual), intervention
            )
          )
        )
        ,
      ]
      
      
      data[
        x > 0,
        `:=`(
          effect = (effect/100)
        )
        ,
        by = .(allocator)
      ][
        ,
        `:=`(
          
          cintervention = pmax(
            intervention - ((difference * effect))
            ,0
          )
        )
        ,
        by = .(allocator)
      ][
        ,
        cdifference := rowSums(
          cbind(-get(counter_factual), cintervention)
        )
        ,
        by = .(allocator)
      ][
        x == 0,
        cintervention := intervention
      ]


      if (extrapolate) {

        # Extrapolate the data
        # such that it extends T+10

        # 1) Extend data by
        # outcome
        data <- rbindlist(
          fill = TRUE,
          use.names = TRUE,
          list(data,

            data.table(
              allocator = unique(data$allocator)
            )[
              ,
              .(
                x = 6:15
              )
              ,
              by = allocator
            ]

          )

        )
        
        
        data[
          ,
          `:=`(
            cdifference = nafill(type = 'locf', x = cdifference) 
          )
          ,
        ]

        data[
          x > 5
          ,
          `:=`(
            cdifference = cdifference  * 1/(1+0.1)^(x-5)
          )
          ,
          by = allocator
        ]





      }
      
      
      
      return(data)
      
      
      
    }
  )
  
  names(data_list) <- list_names
  # return; ####
  
  
  
  return(
    data_list
  )
  
  
}




.model2_flavor <- function(data_list, effect, who) {
  
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
      
      
      
      
      data[
        str_detect(allocator, pattern = paste(who, collapse = '|'))
        ,
        `:=`(
         outcome = outcome * effect,
         effect  = effect
          
        )
        ,
      ]
      
      
      
      #class(data) <- c(class(data), get_class)
      
      return(data)
      
    }
  )
  
}





flavor <- function(
    data_list, 
    effect, 
    do_match = FALSE, 
    who = NULL, 
    extrapolate = FALSE,
    chars = NULL,
    allocators = NULL
    ) {
  
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
    
    data_list <- .model1_flavor(
      data_list,
      effect,
      extrapolate = extrapolate,
      allocators = allocators
    )
    
    class(data_list) <- c(class(data_list), 'model1')
    
  }
  
  if (inherits(data_list, 'model2')) {
    
    data_list <- .model2_flavor(
      data_list,
      effect,
      who = who
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

