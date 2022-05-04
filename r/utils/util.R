# This is the utilities
# script.
# 
# Includes functions only related 
# to the backend.
# 
# Often only needed to run once.


convert_excel <- function() {
  
  # Generate List of Files;
  list_excel <- list.files(
    path = "raw/",
    pattern = "*.xls",
    full.names = TRUE
  )
  
  # Generate Names
  list_name <- list.files(
    path = "raw/",
    pattern = "*.xls",
    full.names = FALSE
  )
  
  for (i in 1:length(list_excel)) {
    
    tmp <- readxl::read_excel(
      list_excel[i]
    ) %>% mutate(
      across(where(is.character), as.factor)
    )
    
    
    data.table::fwrite(
      x = tmp,
      file = paste0(
        "input/",
        str_remove(list_name[i],pattern = "\\.[:alpha:]+"),
        ".csv"
      ),
      row.names = FALSE
    )
    
    
    
  }
  
  
  
}


convert_text <- function() {
  
  library(data.table)
  
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
          
          data.table::fread(path)
          
        }
      ) %>% data.table::rbindlist()
      
      
      get_char <- which(sapply(data, is.character))
      
      
      if (data_name == "transfers") {
        
        
        data[,
             allocator := paste0("Overførsel_", allocator)
             ,
             ]
        
        
      }
      
      
      
      
      # Replace Strings
      data[
        ,
        (get_char) := lapply(
          .SD,
          function(x) {
            
            
            str_replace(x, pattern = "\\*ae\\*",replacement = "æ")
            
            
            
          }
          
        )
        ,
        .SDcols = get_char
        ]
      
      
      data[
        ,
        (get_char) := lapply(
          .SD,
          function(x) {
            
            
            str_replace(x, pattern = "\\*oe\\*",replacement = "ø")
            
            
            
          }
          
        )
        ,
        .SDcols = get_char
      ]
      
      data[
        ,
        (get_char) := lapply(
          .SD,
          function(x) {
            
            
            str_replace(x, pattern = "\\*aa\\*",replacement = "å")
            
            
            # str_replace(x, pattern = "\\*ae\\*",replacement = "æ")
            # str_replace(x, pattern = "\\*oe\\*", replacement = "ø")
            # str_replace(x, pattern = "\\*aa\\*", replacement = "ø")
            
          }
          
        )
        ,
        .SDcols = get_char
      ]
      
      
      colnames(data) <- str_replace_all(colnames(data),pattern = "oe", replacement = "ø")
      
      
      
      data.table::fwrite(
        x = data,
        file = paste0(
          "input/",
          data_name,
          ".csv"
        ),
        row.names = FALSE
      )
      
      
    }
  )
  
  
}


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
          encoding = 'UTF-8',nThread = 4
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



generate_data <- function() {
  
  
  
  path <- list.files(
    path = "input/parameters",
    full.names = TRUE
  )
  
  get_names <- list.files(
    path = "input/parameters",
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
      
    }
  )
  
  
  
  unique_class <- unique(options[[1]]$class)
  
  disease   <- options[[3]]$x
  descriptives <- options[[2]][,list(class, subclass),] %>% split(.$class)
  
  
  
  
  
  
  unique_class %>% map(
    .f = function(unique_class) {
      
      
      allocator <- options[[1]][class %chin% unique_class]$x
      
      
      data <- data.table(
        CJ(
          year = -2:5,
          type = 0:1,
          disease = disease,
          allocator = allocator,
          køn = descriptives$køn$subclass,
          alder = descriptives$alder$subclass,
          arbejdsmarked = descriptives$arbejdsmarked$subclass,
          uddannelse = descriptives$uddannelse$subclass
          
          
        )
      )
      
      
      data[
        ,
        outcome := runif(.N, min =10, max = 100)
        ,
      ]
      
      
      
      
      
      
      
    }
  ) %>% set_names(c("primary_care","psychiatric_care", "somatic_care","transfers"))
  
  
  
  
}




