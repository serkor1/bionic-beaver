# script: 
# objective: 
# date: 2022-07-25
# author: Serkan Korkmaz

















foo <- function(data_list) {
  
  message("Test")
  
  #' function information
  #' 
  #' 
  #' @param data_list a list data.tables that has
  #' been spread and flavoured.
  #' 
  #' 
  #' @returns a list of named data.tables
  
  # filter null list
  # to avoid clutter.
  data_list <- discard(
    data_list,
    is.null
  )
  
  
  # split list
  data_list <- map(
    data_list,
    .f = function(element) {
      
      # Split data
      tmp <- element %>% split(
        .$allocator
      )
      
      
      tmp <- map(
        tmp,
        .f = function(element) {
          
          
          element[
            ,
            allocator := str_split_fixed(
              allocator, pattern = "_",n = 2
            )[,2]
            ,
          ]
          
        }
      )
      
      
      names(tmp) <- str_split_fixed(
        names(tmp),
        pattern = "_",
        n = 2
      )[,1]
      
      
      return(
        tmp
      )
      
      
      
    }
  )
  
  
  # set names of parent list
  
  
  return(data_list)
  
  
  
}







baz <- function(data_list) {
  
  message("Test")
  
  
  
  map(
    data_list,
    .f = function(element) {
      
      get_path <- str_remove(
        unique(names(element)),
        pattern = " "
      )
      
      map(
        element,
        .f = function(element) {
          
          
          write_ods(
            element,
            update = TRUE,
            path = paste0(
              "output/",
              get_path,
              '.ods'
            ),
            sheet = unique(element$allocator),append = TRUE
          )
          
          
        }
      )
      
      
      
      
    }
  )
  
  
  
  
}