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
  #' been spread and not flavoured.
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





.prep_data_model1 <- function(data) {
  
  
  #' function information
  #' 
  #' @param data a data.table
  
  # local variables;
  unique_allocators <- split(
    data,
    data$allocator
  )
  
  
  data <- map(
    unique_allocators,
    .f = function(element) {

      element[
        ,
        allocator := str_split_fixed(
          allocator, pattern = "_",n = 2
        )[,2]
        ,
      ]
      
      
      
      setnames(
        element,
        old = c("x", 'assignment', 'price', 'cost', 'qty'),
        new = c('tid', 'sygdomsgruppe', 'pris(stk)', 'pris(i alt)', 'mængde')
      )
      
      
      return(element)
      
    }
  )
  
  
  names(data) <- str_split_fixed(
    names(data),
    pattern = "_",
    n = 2
  )[,2]
 
  
  return(data) 
}



.prep_id_model1 <- function(data) {
  
  
  #' function information
  #' 
  #' @data a vector of IDs.
  
  
  
  
  
  lookup[[1]][id %in% data]
  
  
  
}






prep_export <- function(data_list) {
  
  #' function information
  #' 
  #' 
  #' 
  
  data <- discard(data_list[[1]], is.null)[[1]]
  id   <- data_list[[2]]
  
  if (inherits(data_list, 'model1')) {
    
    
      data_list <- list(
        .prep_data_model1(data),
        .prep_id_model1(id)
      )
    
    
    
  }
  
  
  
  names(data_list) <- c("data", "id")
  
  
  return(data_list)
  
}






write_export <- function(prep_export, fname) {
  
  
  
  write_ods(
    dcast(
      prep_export$id,value.var = 'value',formula = id ~ variable
    ),
    path = fname,
    sheet = 'Demografi'
  )
  
  
  
  
  # unique(prep_export$id) %>% write_ods(
  #   path = fname,
  #   sheet = "Demografi"
  # )
  
  
  map(
    prep_export$data,
    .f = function(x) {
      
      write_ods(
        x,path = fname,append = TRUE,sheet = unique(x$allocator)
      )
      
      
    }
  )
  
  
  
  
}
