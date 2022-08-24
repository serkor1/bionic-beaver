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



generate_data <- function() {
  
  
  # Name Vector
  name_vector <- c("primary_care","psychiatric_care", "somatic_care","transfers")
  iterator <- 0
  
  
  allocators <- list(
    primary_sector = c(
      'Primær Sektor_Almen Praksis', 
      'Primær Sektor_Anden Speciale', 
      'Primær Sektor_Fysioterapi', 
      'Primær Sektor_Psykiater', 
      'Primær Sektor_Psykolog'
      ),
    psychiatric_care = c(
      'Psykiatrien_Ambulant',
      'Psykiatrien_Indlæggelse',
      'Psykiatrien_Skadestue'
    ),
    somatic_care = c(
      'Somatikken_Ambulant',
      'Somatikken_Indlæggelse',
      'Somatikken_Skadestue'
    ),
    transfers = c(
      'Overførsel_Førtidspension',
      'Overførsel_Fleksjob',
      'Overførsel_Midlertidig Overførselsindkomst',
      'Overførsel_Selvforsørgende'
    )
  )
  
  
  
  
  
  
  model_1 <- allocators %>% map(
    .f = function(unique_class) {
      
      iterator <<- iterator + 1
      
      
      
      data <- data.table(
        CJ(
          year = -2:5,
          type = 0:1,
          assignment = c(
            "matching",
            "Andre Lidelser_Hjertekar",
            "Andre Lidelser_KOL",
            "DiabetesI_Uden Komplikationer",
            "Cancer_Prostata",
            "Andre Lidelser_Ryg- og Nakkesmerter",
            "Psykiske Lidelser_Mild",
            "Andre Lidelser_Kronisk Leversygdom",
            "Cancer_Bryst",
            "Psykiske Lidelser_Svær",
            "Andre Lidelser_Leddegigt",
            "Diabetes II_Uden Komplikationer",
            "Diabetes II_Med Komplikationer",
            "Cancer_Lunge",
            "Psykiske Lidelser_Moderat",
            "Diabetes I_Med Komplikationer",
            "Cancer_Tarm"
            ),
          allocator = unique_class,
          køn = c('Mand','Kvinde'),
          alder = c('18-49 år', '50-64 år', '65+ år'),
          arbejdsmarked = c(
            'Aktiv',
            'Inaktiv',
            'Udenfor'
          ),
          uddannelse = c(
            'Faglært',
            'Ufaglært',
            'Videregående Uddannelse'
          )
          
          
        )
      )
      
      # add random prices
      data[
        ,
        price := runif(1, min = 100, max = 10000)
        ,
        by = .(allocator)
      ][
        ,
        qty := runif(.N, min =10, max = 100)
        ,
      ][
        ,
        cost := price * qty
        ,
      ]
      
      
      # data[
      #   ,
      #   `:=`(
      #     qty = runif(.N, min =10, max = 100),
      #     cost = runif(.N, min =1000, max = 10000)
      #   )
      #   ,
      # ]
      
      
      # Change Class
      class(data) <- c(class(data), name_vector[iterator])
      
      
      return(data)
      
      
      
    }
  ) %>% set_names(name_vector)
  
  
  # Model 2 Data;
  
  model_2 <- data.table(
    CJ(
      assignment = c(
        "0-2 år",
        "3-6 år",
        "7-11 år",
        "12-17 år"
      ),
      feduc = c(
        "Faglært",
        "Ufaglært",
        "Videregående Uddannelse"
      )
      
    )
  )
  
  
  model_2[
    ,
    outcome := runif(
      .N,
      min = 500,
      max = 900
    )
    ,
  ]
  
  class(model_2) <- c(class(model_2), "children")
  
  
  return(
    list(
      model_1,
      list(model_2)
      
    )
  )
  
  
}




