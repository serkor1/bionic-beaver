# script: Data Preloader
# objective: Load the data in a format readable by the rest of 
# the scripts.
# author: Serkan Korkmaz
# date: 2022-06-11


# load data; ####
# 
# Loads the input data in raw format. This data has to be converted to long
# before it can be feeded to the models.

.load_data <- function(path) {
  
  #' function information
  #' 
  #' 
  #' @param path character of path, either \input\data\model1 or \input\data\model2
  #' 
  #' @returns a named list of data.tables in wide format.
  
  get_names <- list.files(
    path = path
  ) %>% str_remove(".csv")
  
  get_path <- list.files(
    path = path,
    full.names = TRUE
  ) 
  
  
  if (str_detect(path,pattern = 'model1')) {
    
    # Message
    message(
      "Loading Model1 Costs!"
    )
    
    
    cost_data <- .load_model1cost()
    
  } else {
    
    # message
    message(
      "Loading Model2 Costs!"
    )
    
  }
  
  
  
  seq_along(get_path) %>% map(
    .f = function(i) {
      
      data <- fread(
        get_path[i],
        encoding = 'UTF-8',
        nThread = 4,
        na.strings = ""
      )
      
      
      if (str_detect(path, 'model1')) {


        data <- cost_data[data, on = .(allocator)][
          ,
          cost := qty * price
          ,
        ]

      }
      
      
      
      
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


# convert to longer; #####
# 
# This function converts the data
# to longer based on existing hardcoded
# variables

.convert_long <- function(
    data,
    char_vector = c('x','køn', 'alder', 'arbejdsmarked', 'uddannelse', 'feduc', 'ftype')
) {
  
  #' Function Information
  #' 
  #' @param data a data.table in wide format
  #' to be converted
  #' 
  #' 
  #' @return data.table in long format
  
  data <- copy(data)
  
  # add row ID
  # to protect data integrity.
  tryCatch(
    {
      data[
        ,
        id := seq_len(.N)
        ,
        by = .(
          year,
          type,
          allocator,
          assignment
        )
      ]
    },
    error = function(cond) {
      
      data[
        ,
        id := .I
        ,
      ]
      
      
      
    }
  )
  
  
  
  setcolorder(
    data, c("id")
  )
  
  
  
  
  
  
  # extract class; ####
  get_class <- class(data)[3]
  
  # helper function; ####
  # define a function that locates the relevant
  # colums to be converted
  
  find_idx <- function(cols, pattern, negate) {
    
    #' Funtion Information
    #' 
    #' @param cols a character vector of columns
    #' of the data.
    #' 
    #' @param pattern a character vector of columns
    #' that needs to be detected
    #' 
    #' @param negate should the detection be negated such
    #' that TRUE => !TRUE
    
    which(
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
    
    
  }
  
  
  # 1) Collect all socio-economic characteristics of the data. ####
  
  idx <- find_idx(
    colnames(data),
    pattern = char_vector,
    negate = TRUE
  )
  
  
  data <- data %>% melt(
    id.vars = idx,
    variable.name = "variable",
    value.name = "value",
    variable.factor = FALSE
  )
  
  
  
  data[
    ,
    chars := paste(
      variable,
      value,
      sep = "_"
    )
    ,
  ][
    ,
    c("variable", "value") := NULL
    ,
  ]
  
  
  # # # 2) Collect outcomes
  # # # from the new data
  # 
  # idx <- find_idx(
  #   colnames(data),
  #   pattern = base::setdiff(
  #     colnames(data),
  # 
  #     # Change these variables if they are named
  #     # differently in your data. This is, however, not
  #     # recomennded as the remaining scripts depends
  #     # on these variables.
  #     # DO IT AT OWN RISK.
  #     c('chars', 'year', 'type', 'assignment', 'allocator', 'id', 'price')
  #   ),
  #   negate = TRUE
  # )
  # 
  # 
  # 
  # 
  # data <- data %>% melt(
  #   id.vars = idx,
  #   variable.name = "outcome_type",
  #   value.name    = "outcome",
  #   variable.factor = FALSE
  # )



  #3) If the column names does not include
  # 'allocator' then outcome_type should be
  # allocator
  if (sum(colnames(data) %chin% c("allocator")) == 0) {

    setnames(
      data,
      old = "outcome_type",
      new = "allocator"
    )

  }

  
  class(data) <- c(
    class(data),
    get_class
  )
  
  
  # return statement
  
  return(
    setDT(data)
  )
  
  
}


# preload data; ####

preload_data <- function(
    developper_mode = FALSE, 
    path = c('input/data/model1/', 'input/data/model2/'),
    char_vector = c('x','køn', 'alder', 'arbejdsmarked', 'uddannelse', 'feduc', 'ftype')
) {
  
  #' function information
  #' 
  #' 
  #' @param developper_mode logical. FALSE by default. Set TRUE
  #' if there exists live data.
  #' @param char_vector a vector of characters. All socioeconomic variables that you
  #' wish to collect against outcomes and other fixed values.
  #' @param path a vector of length 2. Can be empty. Path to the data. The first
  #' element is for model1, second is for model2. If empty, this will be replaced
  #' by synthetic data.
  #' 
  #' 
  #' @returns a named list of data.tables in long format
  #' for the remaining app.
  
  
  # load data; #####
  if (developper_mode) {
    
    message("Preloaded Data (Developper Mode)")
    
    data_list <- generate_data()
    
  } else {
    
    
    # TODO: The data has to be loaded seperately
    # in lists.
    
    message("Preloaded Data (Live Mode)")
    
    data_list <- list(
      
      # model 1 data;
      model1 = .load_data(
        path = path[1]
      ),
      # model 2 data;
      model2 = .load_data(
        path = path[2]
      )
    )
    
    # check for empty lists
    # and replace with synthetic data
    increment <- 0
    data_list <- map(
      data_list,
      function(element) {
        
        increment <<- increment + 1
        
        
        if (is_empty(element)) {
          
          
          
          generate_data()[[increment]]
          
          
        } else {
          
          element
          
        }
        
        
      }
    )
    
    
    
  }
  
  
  
  
  
  # convert data to long;
  data_list <- data_list %>% map(
    .f = function(get_list) {
      
      # Each iteration is over the lists
      # so we need another map
      
      map(
        get_list,
        .f = function(data) {
          
          data %>%
            .convert_long(char_vector = char_vector)
          
        }
      )
      
    }
  )
  
  
  list(
    structure(data_list[[1]], class = 'model1'),
    structure(data_list[[2]], class = 'model2')
  )
  
  
}


