# script: Reference Table
# objective: Create a Reference Table based off the data
# to keep data integrity intact.
# date: 2022-07-27
# author: Serkan Korkmaz



# lookuptable; #####
.gen_lookup <- function(
    data_list
) {
  
  
  #' function information
  #' 
  #' This function generate one(!)
  #' look-uptable per list. This assumes that
  #' the socioeconomic variables are equal in each nested list.
  
  
  
  lookup <- map(
    data_list,
    function(element) {
      
      counter <- 0
      
     tmp <- map(
        element,
        .f = function(element) {
          
          
          
          
          
          # 1) Extract characteristics
          # and ID
          data <- unique(
            element[
              ,
              .(
                id,
                chars
              )
              ,
            ]
          )
          
          # 2) split variables
          # and cast
          
          data <- unique(data[
            ,
            c("variable", "value") :=  tstrsplit(chars, "_", fixed=TRUE)
            ,
          ])

          # # 
          # data <- dcast(
          #   data,
          #   value.var = 'value',
          #   formula = id ~ .
          # )
          
          
          
          
          
          return(data)
          
          
          
         
         
          
          
        }
        )
     
       discard(
         tmp,
         is.null
         )
     
     
      
      
    }
  )
  
  
  
  
  return(lookup)
  
  
  
}



# ID extractor; #####

.extract_id <- function(
    lookup = lookup,
  values = NULL,
  vars = c("alder", 'arbejdsmarked', 'køn', 'uddannelse')
) {
  
  
  #' function information
  #' 
  #' @param chars can be NULL. A vector
  #' of socioeconomic characteristics
  
  if (is.null(chars)) {
    
    return(NULL)
    
  }
  
  
  
  values <- sort(values)
  prefixes <- unique(str_split_fixed(values, '_', 2)[,1])
  
  
  reduce(map(
    prefixes,
    .f = function(x) {
      
      reduce(map(
        values[str_detect(values, x)],
        .f = function(x) {
          
          
          lookup[
            chars %chin% x
          ]$id
          
          
        }
      ), union)
      
      
      
      
      
    }
  ), intersect)
  

  
}
