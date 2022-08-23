# script: helper
# objective: this script gathers helper function
# to reduce repeated codes
# author: Serkan Korkmaz
# date: 2022-06-12



.find_cols <- function(cols, pattern, negate) {
  
  #' function information
  #' 
  #' @param cols a character vector of column
  #' names.
  #' 
  #' @param pattern a character vector of patterns
  #' to detect
  #' 
  #' @param negate logical value. Should the pattern
  #' be negated
  #' 
  #' @returns a vector of column names that are identified
  #' as useful.
  
  idx <- which(
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
  
  
  cols[idx]
  
  
  
}




