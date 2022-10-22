# script: scr_wrap
# date: Tue Sep 27 11:15:34 2022
# author: Serkan Korkmaz
# objective: Generate wrapping function that will wrap the data
# in a format that is readable outside the model




.wrap_model1 <- function(
    data_list
) {
  
  #' function information
  #' 
  #' This function wraps the data for
  #' model1 and makes it readable outside
  #' the model.
  #' 
  #' @param data_list a list of data.tables
  #' from the model1
  #' 
  #' 
  #' @returns 
  
  
  wrapped_data <- map(
    data_list,
    function(element) {
      
      # NOTE: Each element
      # is the data.table of each
      # sector.
      
      # 1) Split according to allocators
      # in the data set
      unique_allocator <- split(
        element,
        element$allocator
      )
      
      # 2) Split data by allocator
      # and store in a list
      data <- map(
        unique_allocator,
        function(data) {
          
          data <- copy(data)
          
          
          idx <- which(sapply(data, is.numeric))
          
          data[
            ,
            (idx) := lapply(.SD, round, 2)
            ,
            .SDcols = idx
          ]
          
          
          
          # Split allocator variable
          # and replace it:
          # group_subgroup
          data[
            ,
            allocator := str_split_fixed(
              string  = allocator,
              pattern = "_",
              n       = 2
            )[,2]
            ,
          ]
          
          
          return(
            data
          )
          
          
        }
      )
      
      # 3) Rename the list elements
      # for easy storage
      names(data) <- str_split_fixed(
        string = names(data),
        pattern = "_",
        n = 2
      )[,2]
      
      return(
        data
      )
      
    }
  )
  
  
  return(
    wrapped_data
  )
  
  
  
  
}



.wrap_model2 <- function(
    data_list
) {
  
  #' function information
  #' 
  #' @param data_list a list of data.tables
  #' from model2
  #' 
  #' @returns a wrapped data_list of data.tables
  #' ready for packing.
  
  
  map(
    data_list,
    function(element) {
      
      # 1) Copy data
      # to avoid overwriting
      element <- copy(element)
      
      element[
        ,
        `:=`(
          effect = fifelse(test = is.na(effect), yes = 0, no = effect),
          assignment = str_remove(assignment, '.+_'),
          allocator  = str_remove(allocator, '[:alpha:]+_')
        )
        ,
      ]
      
      setnames(
        element,
        old = c('assignment', 'allocator', 'outcome', 'effect'),
        new = c('Aldersgruppe', 'Fordeling', 'Produktivitetstab', 'Sygedage')
      )
      
      
      # 1) Copy the data;
      element <- copy(
        element
      )
      
      # 2) Round the data;
      element[
        ,
        `:=`(
          Produktivitetstab = round(Produktivitetstab, 2)
        )
        ,
      ]
      
      # 3) Cast data;
      # and set colorder
      element <- dcast(
        data = element,
        formula = Fordeling + Sygedage ~ factor(
          Aldersgruppe, 
          levels = c('0-2 år', '3-6 år', '7-11 år', '12-17 år')
        ),
        value.var = 'Produktivitetstab'
      )
      
      
      # 4) Set names
      setnames(
        element,
        old = c('Fordeling'),
        new = c('Hvem tager sygedagen?'),
        skip_absent = TRUE
      )
      
      
      
    }
  )
  
  
}





wrap <- function(data_list) {
  
  
  #' function information
  #' 
  #' @param data_list a list of data.tables
  #' of each model.
  
  # 1) Extract the class of the
  # model
  # This is necessary as discard
  # reclasses the object
  
  get_class <- class(
    data_list
  )
  
  # 2) Discard empty elements
  # from the list.
  data_list <- discard(
    data_list,
    is.null
  )
  
  # 3) Reclass the data_list
  # accordingly
  
  class(data_list) <- get_class
  
  if (inherits(data_list, 'model1')) {
    
    data_list <- .wrap_model1(
      data_list = data_list
    )
    
  }
  
  if (inherits(data_list, 'model2')) {
    
    data_list <- .wrap_model2(
      data_list = data_list
    )
    
  }
  
  # 4) Reclass the data_list
  # accordingly
  
  # TODO: Find a neat solution
  # to this.
  
  class(data_list) <- get_class
  
  return(
    data_list
  )
  
}
