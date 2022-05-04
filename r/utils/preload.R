# Preloaders;


# NOTE: This Works!!!!

gen_option <- function(data) {
  
  
  
  # Split Disease
  data <- unique(data[
    ,
    c("class", "subclass") := tstrsplit(disease, "_", fixed = TRUE)
    ,
  ], by = c("disease", "class", "subclass"))
  
  
  get_option <- unique(data$class) %>% map(
    .f = function(unique_class) {
      
      
      # Extract the Data
      data <- data[class %chin% unique_class][]
      
      list(
        setNames(data$disease,as.vector(data$subclass))
      ) %>% set_names(unique_class)
      
      
      
      
    }
  ) %>% flatten()
  
  
  return(
    get_option
  )
  
}



gen_demographics <- function() {
  
  # Extract Demographics;
  data <- data_list[[1]][,-c("year", "disease", "allocator", "outcome", "type", "class", "subclass")]
  
  
  
  
  1:ncol(data) %>% map(
    
    .f = function(col) {
      
      unique(data[[col]])
      
      
    }
    
  ) %>% set_names(colnames(data))
}





get_options <- function() {
  
  path <- list.files(
    path = "input/parameters/",
    full.names = TRUE
  )
  
  get_names <- list.files(
    path = "input/parameters/",
    full.names = FALSE
  ) %>% str_extract(pattern = "[:alpha:]+")
  
  options <-  path %>% map(
    .f = function(path) {
      
      data <- fread(
        path,
        encoding = "UTF-8",
        sep = ";"
        )
      
      data <- data[
        ,
        
        c("class", "subclass") := tstrsplit(x, "_", fixed = TRUE)
        
        ,
        ]
      
      
      
      unique(data$class) %>% map(
        .f = function(unique_class) {
          
          data <- data[class %chin% unique_class][]
          
          list(
            setNames(
              # The Actual Values
              #data$class,
              paste0(data$class, "_", data$subclass),
              
              # The Shown Value
              as.vector(data$subclass)
              )
          ) %>% set_names(unique_class)
          
        }
      ) %>% flatten() 
      
      
      
      #%>% set_names(data$class)
      
      
      
      
      
    }
  ) %>% set_names(get_names)
  
  
  return(
    list(
      diseases = options$diseases,
      demographics = options$demographics,
      allocator    = options$allocator
    )
  )
  
}









