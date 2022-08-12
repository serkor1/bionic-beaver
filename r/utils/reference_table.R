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
          
          
          
         counter <<- counter + 1
         
         if (counter > 1) {
           
           return(NULL)
           
         } else {
           
           
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
         
         
         # dcast(
         #   data,
         #   value.var = 'value',
         #   formula = id ~ variable
         # )
          
          
          
          
          
          return(data)
           
           
         }
         
         
          
          
        }
        )
     
       discard(
         tmp,
         is.null
         )
     
     
      
      
    }
  )
  
  
  
  
  return(flatten(lookup))
  
  
  
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
  
  # lookup[
  #   ,
  #   lapply(
  #     .SD,
  #     function(x) {
  #       
  #       
  #       x %chin% str_remove_all(chars, "[:alpha:]+_")
  #       
  #     }
  #   )
  #   ,
  #   by = .(id),
  #   .SDcols = colnames(lookup)[-1]
  # ][
  #   ,
  #   .(
  #     id,
  #     fetch = apply(.SD, 1, sum)
  #   ),
  #   .SDcols = colnames(lookup)[-1]
  # ][
  #   fetch == length(chars)
  # ]$id
  
  
  sort(
    unique(lookup[
    chars %chin% values
  ]$id)
  )

  
}
