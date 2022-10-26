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




extract_name <- function(
    data_list,
    alternate = NULL,
    get_icon      = FALSE
){
  
  #' function information
  #' 
  #' @param data_list a list of data.table
  #' of either model1 or model2. Can be a single 
  #' data.table.
  #' 
  #' @param alternate a logical value of alternated outcomes
  #' can be NULL.
  #' 
  #' @returns a character vector of lenght 1.
  #' to use in the remainder of model
  
  if (inherits(data_list, 'list')) {
    
    
  } else {
    
    if (!is.null(alternate)) {
      
      alternate <- isTRUE(alternate)
      
      output <- fcase(
        inherits(data_list, c("primary_care")),
        fifelse(
          alternate,
          no = "Gennemsnitlig ydelser pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        ),
        inherits(data_list, c(
          "psychiatric_care", "somatic_care"
        )),
        fifelse(
          alternate,
          no = "Gennemsnitlig sengedage pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        )
        ,
        inherits(data_list, c("transfers")),
        fifelse(
          alternate,
          no = "Gennemsnitlig antal uger pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        ),
        default = fifelse(
          alternate,
          no = "Gennemsnitlig antal recepter pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        )
      )
      
      
      
      
      
    } else {
      
      if (get_icon) {
        
        output <- fcase(
          inherits(data_list, c("primary_care")), 'house-chimney-medical',
          inherits(data_list, c("psychiatric_care")), 'head-side-heart',
          inherits(data_list, c("somatic_care")), 'hospital',
          inherits(data_list, c("transfers")),'money-bill-transfer',
          default = 'prescription-bottle-medical'
        )
        
      } else {
        
        output <- fcase(
          inherits(data_list, c("primary_care")), 'Primær sundhedssektor',
          inherits(data_list, c("psychiatric_care")), 'Psykiatrisk hospitalskontakt',
          inherits(data_list, c("somatic_care")), 'Somatisk hospitalskontakt',
          inherits(data_list, c("transfers")), 'Indkomst',
          default = 'Receptpligtig medicin'
        )
      }
      
      
      
      
    }
    
  }
  
  
  return(
    output
  )
  
}


