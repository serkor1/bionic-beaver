# script: Table functions
# objective: Generate baselayer for the table
# date: 2022-08-22
# author: Serkan Korkmaz




.model1_tablebaselayer <- function(data_list) {
  
  #' function information
  #' 
  #' @param data_list flavored of model1
  #' 
  #' @return a list of data.tables
  #' that adhere to the standards of 
  #' tables
  
  
  map(
    data_list,
    .f = function(element) {
      
      if (is.null(element)) {
        
        return(
          NULL
        )
        
      }
      element <- copy(element)
      
      element[
        ,
        `:=`(
          allocator = str_split(
            allocator,pattern = "_",simplify = TRUE
          )[,2]
        )
        
        ,
      ]
      
      
      idx <- which(
        sapply(element, is.numeric)
      )
      
      
      element[
        ,
        (idx) := lapply(
          .SD,
          round,
          2
        )
        ,
        .SDcols = idx
      ]
      
      
      setorder(
        element, 
        allocator
      )
      
      
      setcolorder(
        element, 
        c("x",
          "allocator",
          "population",
          'intervention',
          'control',
          'difference',
          'effect',
          'cintervention',
          'cdifference'
        )
      )
      
      
      
      setnames(
        element,
        old = c("x",
                "allocator",
                "population",
                'intervention',
                'control',
                'difference',
                'effect',
                'cintervention',
                'cdifference'
        ),
        new = c("Tid",
                "Outcome",
                "Befolkningen",
                'Sygdomsgruppe',
                'Sammenligningsgruppe',
                'Faktisk forskel',
                'Effekt',
                'Kontrafaktisk Sygdomsgruppe',
                'Forventet forskel'
        ),
        skip_absent = TRUE
      )
      
      
    }
  )
  
  
  
}




table_baselayer <- function(data_list) {
  
  #' function information
  #' 
  #' @param data_list of flavoured tables
  
  if(inherits(data_list, 'model1')) {
    
    data_list <- .model1_tablebaselayer(
      data_list
    )
    
  } else {
    
    data_list <- .model2_tablebaselayer(
      data_list
    )
    
  }
  
  
  return(
    data_list
  )
  
}