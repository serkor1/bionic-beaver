# Setup Script;
# 
# 
# This script should be run everytime
# there is new data. The data should be uniform
# everytime data is extracted.

gather_data <- function(do_return = FALSE) {
  
  
  library(magrittr)
  library(data.table)
  library(stringr)
  library(purrr)
  
  # Generate List of Files;
  list_files <- list.files(
    path = "raw/",
    pattern = "*.txt",
    full.names = TRUE
  )
  
  # Generate Names
  list_name <- list.files(
    path = "raw/",
    pattern = "*.txt",
    full.names = FALSE
  )
  
  # Generate Vector of names
  list_name <- list_name %>% 
    stringr::str_extract(pattern = "primary_care|somatic_care|psychiatric_care|transfers") %>% 
    unique()
  
  # Load the data;
  list_name %>% map(
    .f = function(data_name) {
      
      data <- list_files[stringr::str_detect(list_files, data_name)] %>% map(
        .f = function(path) {
          
          data.table::fread(
            path,
            nThread = 4)
          
        }
      ) %>% data.table::rbindlist()
      
      
      get_char <- which(sapply(data, is.character))


      data[
        ,
        (get_char) := lapply(
          .SD,
          function (x) {
            
            x %>% 
              str_replace_all(pattern = "\\*oe\\*",replacement = "ø") %>% 
              str_replace_all(pattern = "\\*ae\\*",replacement = "æ") %>% 
              str_replace_all(pattern = "\\*aa\\*", replacement = "å")
            
            
            
          }
        ) 
        ,
        .SDcols = get_char
      ]
      
      
      data <- data[order(year)]
      
      
      colnames(data) <- colnames(data) %>% 
        str_replace_all(pattern = "oe",replacement = "ø") %>% 
        str_replace_all(pattern = "ae",replacement = "æ") %>% 
        str_replace_all(pattern = "aa", replacement = "å")
      
      
      if (do_return) {
        
        data
        
      } else {
        
        
        fwrite(
          x = data,
          file = paste0(
            "input/data/",
            data_name,
            ".csv"
          ),
          row.names = FALSE
        )
          
          
      }
      
      
    }
  )
  
 
  
  
}







gather_parameter <- function() {
  
  library(magrittr)
  library(data.table)
  library(stringr)
  library(purrr)
  
  # Gather Data
  data <- gather_data(do_return = TRUE)
  
  
  # wrtie demographics
  demographics <- unique(data[[1]][,-c("year", "type", "allocator", "outcome", "disease"),])
  
  # Add Mock variable
  demographics[,mock := 1,]
  
  demographics <- demographics %>% melt(id.vars = "mock") %>% unique()
  
  demographics <- demographics[,mock := NULL,][
    ,
    demographics := paste0(
      variable, "_", value
    )
    ,
  ][,list(x = demographics),]
  
  
  
  fwrite(
    demographics,sep = ";",
    file = "input/parameters/demographics.csv"
  )
  
  
  # Write diseases
  diseases <- unique(data[[1]][,list(x = disease),])
  
  
  fwrite(
    diseases,sep = ";",
    file = "input/parameters/diseases.csv"
  )
  
  
  
  # Allocator
  allocator <- data %>% map(
    .f = function(data) {
      
      
      unique(data[,list(x = allocator)])
      
      
    }
  ) %>% rbindlist()
  
  fwrite(
    allocator,sep = ";",
    file = "input/parameters/allocator.csv"
  )
  
  
  
}


gather_data()

gather_parameter()
