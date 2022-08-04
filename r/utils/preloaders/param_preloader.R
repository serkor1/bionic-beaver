# script: param_preloader
# objective: generate a function that loads
# all the possible parameters statically
# author: Serkan Korkmaz
# date: 2022-06-12



.load_model1cost <- function() {
  
  # load data
  data <- rbindlist(
    map(
      list.files(
        "input/parameters/model1/",
        full.names = TRUE
      ),
      fread
    )
  )
  
  
  data[
    ,
    .(
      allocator = paste0(
        class,
        "_",
        subclass
      ),
      price = price
    )
    
    ,
  ]
  
  
}



.get_outcomes <- function(data_list, variable = "allocator") {
  
  #' function information
  #' 
  #' @param data_list the entire
  #' list of data. This is of length 2, as
  #' there are two data models
  #' 
  #' @returns a named list of choice parameters
  #' for the model.
  
  # 1) Store the variable name
  # as.name otherwsise it cant be evaluated.
  variable <- as.name(
    variable
    )
  
  
  # 1) Iterate through the list
  # which has two elements, nesting
  # multiple datasets
  map(
    data_list,
    function(get_list) {
      
      
      # Create a data.table
      # based on the choices in each element
      # of the list
      choices <- get_list %>% map(
        function(data) {
          
          
          
        # Extract Unique Choices
        # from the data based on the variable
        # name.
         get_choice <-  unique(
           data[,.(tmp = eval(variable)),]
          )
         
         
         
         # Split in class subclass
         # for the named list
         get_choice <- unique(get_choice[
           ,
           c("class", "subclass") := tstrsplit(tmp, "_", fixed = TRUE)
           ,
         ][,.(class, subclass),])
         
         
         
         
         

         
         

          
          
        }
      ) %>% rbindlist()  
      
      # While the choices are unique by default
      # there are repeated measures in each set
      # this is true for characteristics across
      # the first model
      choices <- unique(
        choices
        )
      
      
      # The actual value of choice
      # is based on wether the class
      # subclass is repeated.
      actual_choice <- fifelse(
        
        choices$class %chin% choices$subclass
        ,
        yes = paste0(choices$class),
        no = paste0(
          choices$class,
          "_",
          choices$subclass
        )
      )
      
      # The named list internally
      # groups the choices if the
      # actual values includes _ then
      # it should be grouped. Otherwise
      # not.
      name_for_list <- 
        fifelse(
          
          str_detect(actual_choice,pattern = "_")
          ,
          yes = paste0(choices$class),
          no = fifelse(
            str_detect(actual_choice,pattern = "år"),
            yes = "Aldersgruppe",
            no  = paste0(choices$class)
          )
        )
      
      # Generate a named list of choices
      # for the model
      split(
        
        
        setNames(
          
          
          actual_choice
          
          
          ,
          
          # The shown value on the list
          # that appears to the user.
          choices$subclass
        ),
        
        
        name_for_list
        
    
        
      )
      
      
    }
  )
  
  
}





# A hidden wrapper of the oucomes
# visible to the user.
.gen_option <- function(data_list) {
  
  list(
    chars      = .get_outcomes(data_list,variable = "chars"),
    assignment = .get_outcomes(data_list, variable = "assignment"),
    outcome    = .get_outcomes(data_list, variable = "allocator")
  )
  
  
}

