# script: scr_checksums
# objective: Generate md5-checksums to
# verify data integrity
# date: 2022-09-19
# author: Serkan Korkmaz

.model1_checksum <- function(
    data_list,
    assignment_grid,
    outcome_grid,
    variables
) {
  
  #' function information
  #' 
  #' @param data_list a list of data processed
  #' by the model.
  #' 
  #' @param assignment_grid a data.table with possible
  #' combinations of the assignment and characteristics
  #' 
  #' @param outcome_grid a vector of outcomes in the model
  
  
  data <- rbindlist(
    spread(
      grind(
        data_list = data_list,
        intervention = assignment_grid$intervention,
        control      = assignment_grid$control,
        allocators   = outcome_grid,
        chars        = variables
      )
    )
  )[
    ,
    .(
      x,
      allocator,
      control,
      intervention
    )
    ,
  ]
  
  setorder(
    data, x, allocator
  )
  
  
  
  return(
    digest::digest(
      object = as.data.frame(
        data
      ),
      algo = 'md5'
    )
  )
  
  
  
}


.model1_rawchecksum <- function(
    assignment_grid,
    outcome_grid,
    variables
) {
  
  data <- .load_data(
    'input/data/model1'
  )
  
  
  data <- rbindlist(
    map(
      data,
      function(element) {
        
        
        
        
        element <- element[
          type == 0 & assignment %chin% c(assignment_grid$intervention, assignment_grid$control)
        ]
        
        if (!is.null(variables)) {
          
          get_col = str_split(variables, '_',simplify = TRUE)[,1]
          get_val = str_split(variables, '_', simplify = TRUE)[,2]
          
          
          element <- element[
            element[
              
              ,
              Reduce(
                `&`,
                lapply(
                  .SD,
                  `%chin%`,
                  get_val
                )
              ),
              .SDcols = get_col
            ]
          ]
        }
        
        
        setnames(
          element,
          old = 'year',
          new = 'x'
        )
        
        
        
        element <- element[
          ,
          .(
            outcome = mean(qty, na.rm = TRUE)
          )
          ,
          by = .(
            x,
            allocator,
            assignment
          )
          
        ]
        
        
        element[
          ,
          assignment := fcase(
            assignment %chin% assignment_grid$intervention, 'intervention',
            assignment %chin% assignment_grid$control, 'control'
          )
          ,
        ]
        
        
        element <- dcast(
          element,
          formula = x + allocator ~ assignment,
          value.var = 'outcome'
        )
        
        
        
        
        
        
      }
    )
  )
  
  
  setorder(
    data, x, allocator
  )
  
  
  return(
    digest::digest(
      object = as.data.frame(
        data
      ),
      algo = 'md5'
    )
  )
  
}



verify_checksums <- function(
    model = 1,
    data_list,
    assignment_grid,
    outcome_grid,
    variables
) {
  
  
  ok_status <- numeric()
  
  
  if (model == 1) {
    
    rbindlist(map(
      1:nrow(assignment_grid),
      function(i) {

        if (is.null(variables)) {

          variables <- NULL

        } else {

          variables <- variables[i]

        }

        suppressMessages(
          checksum_model <- .model1_checksum(
            data_list = data_list,
            assignment_grid = assignment_grid[i,],
            outcome_grid = outcome_grid,
            variables = variables
          )
        )


        suppressMessages(
          checksum_input <- .model1_rawchecksum(
            assignment_grid = assignment_grid[i,],
            outcome_grid = outcome_grid,
            variables = variables
          )
        )
        
        
        
        data.table(
          status_ok = as.numeric(
            checksum_input == checksum_model
          )
        )






      }
    ))[
      ,
      .(
        status_ok = mean(status_ok)
      )
      ,
    ]
    
    
    
  }
  
  
}