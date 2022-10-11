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
      
      get_class <- class(element)[length(class(element))]
      
      counter <- 0
      
     tmp <- map(
        element,
        .f = function(element) {
          
          
          
          
          idx <- which(
            sapply(
              colnames(element),
              function(x) {
                
                str_detect(
                  x, pattern = 'id|chars|total_n|total_N'
                )
                
              }
            )
          )
          
          
          # 1) Extract characteristics
          # and ID
          data <- unique(
            element[
              ,
              # .(
              #   id,
              #   chars,
              #   total_n,
              #   total_N
              # )
              # 
              ..idx
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
          
          
          class(data) <- c(class(data), get_class)
          
          
          
          return(data)
          
          
          
         
         
          
          
        }
        )
     
       discard(
         tmp,
         is.null
         )
       
       structure(
         tmp,
         class = c(class(tmp),get_class)
       )
     
     
      
      
    }
  )
  
  
  
  
  return(
    lookup
    
  )
  
  
  
}



# ID extractor; #####

.extract_id_model1 <- function(
  lookup = lookup,
  values = NULL
) {
  
  
  #' function information
  #' 
  #' @param values character vector of choices.
  #' 
  #' 
  #' Returns the Intersection of the union to avoid 
  #' duplicates.
  #' 
  #' NOTE: This function has been verified, and works 
  #' as intended.
  
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



.extract_id_model2 <- function(
    lookup = lookup,
    values = NULL
) {
  
  
  #' function information
  #' 
  #' @param values character vector of choices.
  #' 
  #' 
  #' Returns the Intersection of the union to avoid 
  #' duplicates.
  #' 
  #' NOTE: This function has been verified, and works 
  #' as intended.
  
  if (is.null(chars)) {
    
    return(NULL)
    
  }
  
  
  prefixes <- unique(str_split_fixed(values, '_', 2)[,1])
  
  reduce(map(
    lookup,
    function(element) {
      element[
        chars %chin% values
      ]$id
    }
  ), unlist)
  
  
  
}






extract_id <- function(
  lookup = lookup,
  values = NULL
) {
  
  if (inherits(lookup, 'model1')) {
    
    
    id <- .extract_id_model1(
      lookup = lookup,
      values = values
    )
    
  } else {
    
    
    id <- .extract_id_model2(
      lookup = lookup,
      values = values
    )
    
  }
  
  return(
    id
  )
  
}


