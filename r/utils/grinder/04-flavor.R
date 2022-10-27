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
  #' @param data_list a list of data.tables from model 1.
  #' has to be spreaded data.
  #' 
  #' @param effect a vector of effects as numerics of length
  #' 5.
  #' @param extrapolate logical. FALSE by default. Set to TRUE
  #' if extrapolation is needed for T+10
  #' 
  #' @param allocators A vector of characters of lenght >= 1. The allocators
  #' are the outcomes desired for analysis.
  
  
  
  
  
  
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
  
  # 1) Elementwise operations
  # on the data_list.
  data_list <- map(
    data_list,
    .f = function(element) {
      
      # 1) Copy the data to avoid
      # overwriting data in the memory.
      element <- copy(
        element
      )
      
      
      
      # 2) Filter data according
      # to chosen allocators.
      element <- element[
        allocator %chin% allocators
      ]
      
      
      # 3) Check for existing
      # data.tables with nrow == 0
      # End the function if TRUE
      if (nrow(element) == 0) {
        
        return(
          NULL
        )
        
      }
      
      # 4) Locate columns based
      # on existance and sum
      # the values
      idx <- which(
        sapply(
          colnames(element),
          str_detect,
          'control|intervention|population'
        )
      )
      
      element <- element[
        ,
        lapply(
          X = .SD,
          FUN = sum,
          na.rm = TRUE
        )
        ,
        by = .(
          x, allocator
        ),
        .SDcols = idx
      ]
      
      
      # 5) Check the existance of
      # control colums - if it exists
      # then set counter_factual to control.
      # population otherwise.
      counter_factual <- fifelse(
        c('control') %chin%  colnames(element),
        yes = 'control',
        no = 'population'
      )
      
      element[
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
      
      # 6) Add effects to the data
      # by reference and calculate
      # counterfactual values
      # setting the max value to 0.
      element[
        x > 0
        ,
        `:=`(
          effect = (
            effect/100
            )
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
          cbind(
            -get(counter_factual),
            cintervention
            )
        )
        ,
        by = .(allocator)
      ][
        x == 0
        ,
        cintervention := intervention
      ]
      
      # 7) Determine wether extrapolation
      # is chosen and implement if TRUE
      if (extrapolate) {

        # 1) Extend data by
        # outcome and do rbindlist
        # as it is faster than rbind.
        element <- rbindlist(
          fill = TRUE,
          use.names = TRUE,
          list(
            # Full element
            element,
            
            # Generate data based on chosen
            # allocators and extend
            # x to 15.
            data.table(
              allocator = unique(element$allocator)
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
        
        # 2) Carry the last observed value
        # forward which is T = 5.
        element[
          ,
          `:=`(
            cdifference = nafill(
              type = 'locf',
              x = cdifference
            ) 
          )
          ,
        ]

        # 3) Discount all values
        # according to x setting T = 6 to 1.
        element[
          x > 5
          ,
          `:=`(
            cdifference = cdifference  * 1/(1+0.1)^(x-5)
          )
          ,
          by = allocator
        ]





      }
      
      
      
      return(element)
      
      
      
    }
  )
  
  # set names of the list
  # to retain the names for
  # packing and wrapping the data
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
  
  # is_export <- inherits(data_list, 'export')
  # 
  # message(
  #   paste('Is Export:', is_export)
  # )
  # 
  # if (is_export) {
  #   
  #   message("in export")
  #   
  #   id_data   <- data_list[[2]]
  #   data_list <- data_list[[1]]
  #   
  #   
  # }
  
  message('Flavoring data')
  
  
  
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
  
  # if (is_export) {
  #   
  #   
  #   
  #   data_list <- list(
  #     data_list,
  #     id_data
  #   )
  #   
  # }
  
  
  
  
  return(
    data_list
  )
  
}

