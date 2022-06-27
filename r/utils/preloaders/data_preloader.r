# script: Data Preloader
# objective: Load the data (Development/Live) mode. This script converts
# the data to long format
# author: Serkan Korkmaz
# date: 2022-06-11

# convert to longer; #####
# 
# 
# This function converts the data
# to longer based on existing hardcoded
# variables

.convert_long <- function(data) {
  
  #' Function Information
  #' 
  #' @param data a data.table in wide format
  #' to be converted
  #' 
  #' 
  #' @return data.table in long format
  
  # extract class; ####
  get_class <- class(data)[3]
  
  # helper function; ####
  # define a function that locates the relevant
  # colums to be converted
  
  find_idx <- function(cols, pattern, negate) {
    
    #' Funtion Information
    #' 
    #' @param cols a character vector of columns
    #' of the data.
    #' 
    #' @param pattern a character vector of columns
    #' that needs to be detected
    #' 
    #' @param negate should the detection be negated such
    #' that TRUE => !TRUE
    
    which(
      sapply(
        cols,
        function(name) {
          
          str_detect(
            name,
            pattern = paste(
              pattern, collapse = "|"
            ),
            negate = negate
          )
          
        }
      )
    )
    
    
  }
  
  
  # 1) Collect characteristics
  # from the data
  
  idx <- find_idx(
    colnames(data),
    pattern = c(
      "x",
      "year",
      "\\btype",
      "assignment",
      "allocator",
      "qty",
      "cost",
      "indk"
      ),
    negate = FALSE
  )
  
  data <- data %>% melt(
    id.vars = idx,
    variable.name = "variable",
    value.name = "value",
    variable.factor = FALSE
  )
  
  data[
    ,
    chars := paste(
      variable,
      value,
      sep = "_"
    )
    ,
  ][
    ,
    c("variable", "value") := NULL
    ,
  ]
  
  
  # 2) Collect outcomes
  # from the new data
  
  idx <- find_idx(
    colnames(data),
    pattern = c("qty", "cost","indk"),
    negate = TRUE
  )

  data <- data %>% melt(
    id.vars = idx,
    variable.name = "outcome_type",
    value.name    = "outcome",
    variable.factor = FALSE
  )
  
  
  #3) If the column names does not include
  # 'allocator' then outcome_type should be
  # allocator
  if (sum(colnames(data) %chin% c("allocator")) == 0) {
    
    setnames(
      data,
      old = "outcome_type",
      new = "allocator"
    )
    
  }
  
  
  class(data) <- c(class(data), get_class)
  
  # return statement
  
  return(
    setDT(data)
  )
  
  
}



# preload data; ####

preload_data <- function(developper_mode = FALSE) {
  
  
  
  if (developper_mode) {
    
    message("Preloaded Data (Developper Mode)")
    
    generate_data()
    
  } else {
    
    message("Preloaded Data (Live Mode)")
    
    get_names <- list.files(
      path = "input/data/"
    ) %>% str_remove(".csv")
    
    get_path <- list.files(
      path = "input/data/",
      full.names = TRUE
    ) 
    
    seq_along(get_path) %>% map(
      .f = function(i) {
        
        data <- fread(
          get_path[i],
          encoding = 'UTF-8',
          nThread = 4,na.strings = ""
        )
        
        class(data) <- c(
          class(data),
          get_names[i]
        )
        
        data
        
      }
    ) %>% set_names(
      get_names
    )
    
    
  }
  
  
  
  
  
  
  
  
}
