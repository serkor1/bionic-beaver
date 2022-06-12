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
  
  
  
  # return statement
  
  return(
    data
  )
  
  
}
