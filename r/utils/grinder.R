# Script: Grinder-class functions
# that prepares the data for further 
# analysis


# main grinder; #####
grinder <- function(
    data_list,
    intervention = NULL,
    control = NULL,
    allocators = NULL,
    chars = NULL,
    alternate = FALSE
) {
  
  
  #' Function Information
  #' 
  #' @param data_list a list of data.tables regardless of the model
  #' chosen
  #' 
  #' @param intervention a character vector of length 1.
  #' @param control a character vector of length 1.
  #' @param allocator a character vector of lenth N.
  #' @param chars a character vector of length N.
  #' @param alternate a logical value, indicating wether alternate
  #' versions of the data are to be calculated.
  
  
  
  get_char <- chars
  
  alternates <- fcase(
    isFALSE(alternate), "qty",
    default = "cost"
  )
  
  
  # TODO: Generate SD Cols which
  # corresponds to those columns that are not outcomes
  
  # TODO: Needs to be robust against
  # existing models such that it only operates on existing
  # data to avoid errors.
  
  data_list %>% map(
    .f = function(data) {
      
      # 1) Filter data according to chosen
      # parameters.
      #
      # TODO: This is disease, but should be called something
      # that is globally applicable. allocation is probably a 
      # good idea
      # We always want the matching group
      # as a comparision
      
      data <- tryCatch(
        expr = {
          data[
            outcome_type %chin% alternates & type == 1
          ]
        },
        error = function(cond) {
          
          
          data
          
        }
      )
      
      
      # Robust Check for Characteristics
      check_chars <- str_detect(
        paste(get_char,collapse = ""),pattern = '[:alpha:]'
      ) 
      
      
      
      
      if (check_chars) {
        
        data <- data[
          chars %chin% get_char
        ]
        
      }
      
      
      data <- data[
        assignment %chin% c(intervention, control, "matching") &
          allocator %chin% c(allocators) 
      ]
      # data <- tryCatch(
      #   
      #   # Treat Data as model_1
      #   expr = {
      #     
      #   },
      #   
      #   error = function(cond) {
      #     
      #     message(paste(cond))
      #     
      #     data[
      #       assignment %chin% c(intervention, control, "matching") &
      #         allocator %chin% c(allocators)
      #     ]
      #     
      #     
      #   }
      # )
      
      
      
      
      
      
      
      # 2) classify accordingly
      data[
        ,
        assignment_factor := fcase(
          assignment %chin% "matching", "population",
          assignment %chin% intervention, "intervention",
          assignment %chin% control, "control"
        )
        ,
      ]
      
      
      # 3) Find all columns that are used as
      # grouping
      idx <- which(
        sapply(
          colnames(data),
          function(name) {
            
            str_detect(
              name,
              pattern = paste(c("x","year", "assignment", "allocator", "allocator"), collapse = "|")
            )
            
          }
        )
      )
      
      
      # 3) aggregate data;
      # by group
      data[
        ,
        .(
          outcome = mean(
            outcome, na.rm = TRUE
          )
        )
        ,
        by = c(colnames(data)[idx])
      ]
      
      
      
    }
  )
  
  
}


# main grinder; ####
#' grinder <- function(data_list, intervention, control, allocator_vector = NULL, group_value = NULL, type = TRUE, cost = FALSE) {
#'   #' @param data_list input data in its raw
#'   #' format.
#'   #' @param intervention character. The disease
#'   #' chosen as the intervention
#'   #' @param control character. the disease
#'   #' chosen as the control
#'   #' 
#'   #' @param allocator_vector character vector. The chosen outcome
#'   #' has to max 3.Same as the Outcome
#'   #' 
#'   #' @returns a list of data.tables in wide format.
#'   
#'   
#'   # Extract Grouping variables; ####
#'   # group_variable <- .extract_grouping(
#'   #   group_value
#'   # )
#'   group_variable = NULL
#'   
#'   
#'   # Collapse Allocator; ####
#'   allocator_vector <- allocator_vector  %>% paste(
#'       collapse = "|"
#'     )
#'   
#'   # HAd remove strings
#'   
#'   type_char <- fcase(
#'     isTRUE(type), 1,
#'     default = 0
#'   )
#'   
#'   cost <- fcase(
#'     isTRUE(cost), TRUE,
#'     default = FALSE
#'   )
#'   
#'   
#'   
#'   
#'   
#'   data_list %>% map(
#'     .f = function(data) {
#'       
#'       get_char <- which(sapply(data, is.character))
#'       
#'       
#'       
#'       # NOTE: All matching obs have type as NA.
#'       # if we do not replace these it will vanish
#'       data[
#'         disease %chin% "matching",
#'         type := type_char
#'       ]
#'       
#'       
#'       
#'       # Step 1)
#'       # Filter the data to include 
#'       # only the desired diseases
#'       data <- data[
#'         type == type_char
#'       ]
#'       
#'       if (cost) {
#'         
#'         data[,`:=`(outcome = cost, qty = NULL),]
#'         
#'       } else {
#'         
#'         data[,`:=`(outcome = qty, cost = NULL),]
#'         
#'       }
#'       
#'       # Steo 2)
#'       # Classify the data according to
#'       # intevention and control based on the
#'       # chosen intervention and control values
#'       
#'       data <- data[,
#'                    allocation := fcase(
#'                      disease %chin% "matching", "population",
#'                      disease %chin% intervention, "intervention",
#'                      disease %chin% control, "control"
#'                    )
#'                    ,][!is.na(allocation)]
#'       
#'      
#'       
#'       
#'      
#'       
#'       
#'       
#'       
#'       
#'       
#'       # Step 3)
#'       # Filter Data based on the chosen parameters
#'       data <- data[
#'         data[
#'           ,
#'           apply(
#'             .SD, 
#'             1,
#'             function(x) {
#'               
#'               # The sum is always equal to the number of
#'               # possible combinations
#'               sum(x %chin% group_value) == length(group_variable)
#'               
#'               
#'             }
#'           )
#'           ,
#'           .SDcols = get_char
#'         ]
#'       ]
#'       
#'       
#'       
#'       
#'       
#'       
#'       
#'       # Step 4) 
#'       # Aggregate Results by year and group
#'       # accordingly
#'       # TODO: Is it per patient or total
#'       # this is important.
#'       # TODO: Normalize Outcomes variables
#'       # to do a more smooth transition.
#'       # This might be a good idea generate these
#'       # outcome via paramters
#'       
#'       data <- data[
#'         str_detect(
#'           string = allocator,
#'           pattern = allocator_vector
#'         )
#'         ,
#'         list(
#'           outcome = mean(outcome, na.rm = TRUE)
#' 
#'         )
#'         ,
#'         by = c(
#'           "year", "allocation", "disease", "allocator", group_variable
#'         )
#'       ]
#'       
#'       
#'       
#'       return(data)
#'       
#'     }
#'   )
#' }






# foo; #####
foo <- function(data) {
  
  #' This function converts the data to wide
  #' 
  #' TODO: This should replace info_grinder and
  #' plot_grinder and table_grinder
  
  
  # NOTE: This could be moved to 
  # baz
  
  
  
  
  data <- map(
    seq_along(data),
    .f = function(i) {
      
      data  <- data[[i]]
      
      if (nrow(data) == 0) {
        
        return(NULL)
        
      }
      
      
      # Extract the Class; 
      store_class <- class(data)[3]
      
      
      
      group_cols <- .find_cols(
        cols = colnames(data),
        pattern = c("x", "year", "assignment", "allocator", "allocator"),
        negate = FALSE
      )
      
      
      
      
      data <- data[
        ,
        list(
          outcome = mean(
            outcome, na.rm = TRUE
          )
        )
        ,
        by = c(group_cols)
      ]
      
      
      data <- tryCatch(
        {
          data %>% dcast(
            year + allocator ~ assignment_factor,
            value.var = "outcome"
          )
        },
        error = function(cond) {
          
          data %>% dcast(
            allocator ~ assignment_factor,
            value.var = "outcome"
          )
          
          
          
        }
      )
      
      
      
     



      tryCatch(
        expr = {




          data[,
               `:=`(
                 cintervention = 0,
                 difference = sum(-control, intervention),
                 cdifference = 0
               )

               ,
               by = 1:nrow(data)
          ]

        },

        error = function(cond){

          # Test for missing column name and add
          existing_columns <- colnames(data) %>%
            str_extract(pattern = "control|intervention") %>% na.omit()

          # Needs the colums
          needed_colums <- c("control", "intervention")

          # Extract missing COlums
          missing_column <- needed_colums[!(needed_colums %chin% existing_columns)]

          data[
            ,
            (missing_column) := NA
            ,
          ]


          data[,
               `:=`(
                 cintervention = 0,
                 difference = 0,
                 cdifference = 0
               )

               ,
               by = 1:nrow(data)
          ]


        }
      )




      tryCatch(
        {
          
          data[
            year <= 0,
            `:=`(
              cintervention = NA,
              cdifference   = NA,
              difference    = NA
            )
          ]
          
        },
        
        error = function(cond) {
          
          NULL
          
        }
      )
      
      
      # Rename Columns such
      # that it can be fed into
      # the plot functions without 
      # troubles
      setnames(
        data,
        old = c("year"),
        new = c("x"),
        skip_absent = TRUE
      )
      
      
      # This needs to be in foo
      if (store_class == "children") {
        
        
        setnames(
          data,
          old = c("allocator"),
          new = c("x"),
          skip_absent = TRUE
        )
        
        
      }
      


      # Reclass the data
      class(data) <- c(class(data), store_class)

      setDT(data)

      return(data)
      
      
    }
  )
  
  
  
  return(data)
  

}

# baz; ####
baz <- function(data, effect, do_match = FALSE) {
  
  #' Function Information
  #' 
  #' @param data, list. This is the data
  #' in wide format processed by foo().
  #' @param effect, numeric vector of effects.
  
  
  do_match <- fcase(
    isTRUE(do_match), TRUE,
    default = FALSE
  )
  
  
  counter <- 0
  
  map(
    seq_along(data),
    .f = function(i) {
      
      counter <<- counter + 1
      
      if (counter == 1) {
        
        message(
          "Calculating effects!"
        )
        
      }
      

      
      # It has to be a copy
      # otherwise it overwrites data
      # in memory!!!
      data <- copy(data[[i]])
      
      if (is.null(data)) {
        
        return(NULL)
        
      }
      
      
      if (do_match) {
        
        # Replace Population with
        # control values
        data[
          ,
          control := population
          ,
        ]
        
        data[
          ,
          difference := sum(-control, intervention)
          ,
          by = 1:nrow(data)
        ]
        
        data[
          x <= 0,
          `:=`(
            cintervention = NA,
            cdifference   = NA,
            difference    = NA
          )
        ]
        
      }
      
      # Extract the Class; 
      store_class <- class(data)[3]
      
      data[
        x > 0
        ,
        effect := (effect/100)
        ,
        by = .(allocator)
      ]
      
      data[
        !is.na(effect),
        `:=`(
          cdifference   = difference * effect,
          # Was abs - but not used.
          cintervention = max(intervention - ((difference * effect)),0)
        ),
        by = .(allocator, x)
      ]

      data[
        x == 0,
        `:=`(
          cintervention = intervention
        )

      ]
      
      class(data) <- c(class(data), store_class)
      
      
      return(data)
      
    }
  )
  

  
}










baz1 <- function(data, effect) {
  
  
  #' function information
  #' 
  #' @param data list of data from 
  #' the second model
  #' 
  #' @param effect an effect value
  #' that will calculate effects
  #' 
  #' @returns the same data with added effects
  
  map(
    seq_along(data),
    .f = function(i) {
      
      # Extract Data
      data <- copy(data[[i]])
      
      
      # Extract the Class; 
      store_class <- class(data)[3]
      
      # Skip NULLS
      if (is.null(data)) {
        
        return(NULL)
        
      }
      
      # Calculate effects
      data[
        ,
        `:=`(
          cdifference = difference * effect/100,
          cintervention = max(intervention - ((difference * (effect/100))),0)
        )
        ,
      ]
      
      
      class(data) <- c(class(data), store_class)
      
      return(data)
      
    }
  )
  
  
  
}





























info_grinder <- function(data, group_value = NULL, aggregrate = TRUE, effect =  rep(50,5)) {
  
  # Function Information; #####
  #' @param data data in wide format. Possibly extracted
  #' from the grinder
  #' 
  #' @param group_vale a character vector of actual
  #' group values. Ie. Male, Female etc.
  #' 
  #' @param group_variable a character vector of column
  #' names containing those variables. Programmatically 
  #' generated.
  #' 
  #' This function needs to to aggregate
  #' by year and and outcome only. 
  #' It will ouput the plotable data
  #' such that the plot in question, will show
  #' how much a female/male on Such and such uses, 
  #' for example, primary care.
  
  
  
  if (nrow(data) > 0) {
    
    
    # Remember that it messes up class
    # so we extract it
    class_holder <- class(data)[3]
    
    # If there is Chosen Any grouping
    # vaalues;
    if (!is.null(group_value)) {
      
      group_variable <- .extract_grouping(
        group_value
      )
      
      
      data <- data[
        eval(
          parse(
            text = paste(
              group_variable,
              sep = "%chin%",
              quote(group_value),
              collapse = "&"
            )
          )
        ),
        ,
      ]
      
      
      
    }
    
    
    data <- data[
      ,
      list(
        outcome = sum(
          outcome, na.rm = TRUE
        )
      )
      ,
      by = .(year, allocation, allocator)
    ]
    
    # Prepare Data; ####
    
    # Step 1) 
    # Cast to Wide Format
    data <- data %>% dcast(
      year + allocator ~ allocation,
      value.var = "outcome"
    )
    
    # Step 2)
    # Add Effects
    data[
      year > 0,
      effect := (1 - effect/100)
      ,
      by = .(allocator)
    ]
    
    
    # Step 2)
    # Calculate Differences
    data[
      ,
      `:=`(
        outcome = sum(
          -Control,
          Intervention
        ),
        counterfactual = sum(
          -Control,
          Intervention * effect
        )
      )
      
      ,
      by = 1:nrow(data)
    ]
    
    data[
      year == 0,
      counterfactual := outcome
    ]
    
    
    data <- data[year >= 0]
    
    
    
    class(data) <- c(class(data), class_holder)
    
    
    
    
    
    
    
    # Aggregate the data; 
    if (aggregrate) {
      
      
      data <- data[
        ,
        list(
          effect = mean(effect, na.rm = TRUE) * 100,
          outcome = round(mean(outcome, na.rm = TRUE)),
          counterfactual = round(mean(counterfactual, na.rm = TRUE))
        )
        ,
      ]
      
      
      
    } 
    
    
    
    # Prepare Data; 
    # for boxes
    data[,
         percent_change := abs(round(100 * (abs(counterfactual) - abs(outcome)) / abs(outcome)))
         ,
    ][,
      percent_change := fifelse(is.na(percent_change), "-", as.character(percent_change))
      ,
    ]
    
    
    
    outcome_label <- fcase(
      inherits(data, "primary_care"), "Besøg",
      inherits(data, c("psychiatric_care", "somatic_care")), "Sengedage",
      inherits(data, c("transfers")), "Uger"
    )
    
    
    data[,
         `:=`(
           outcome_label = fifelse(outcome < 0, paste("Færre", outcome_label), paste("Flere", outcome_label)),
           counter_label = fifelse(counterfactual < 0, paste("Færre", outcome_label), paste("Flere", outcome_label)),
           outcome_icon  = fifelse(outcome < 0, "caret-down", "caret-up"),
           counter_icon  = fifelse(counterfactual < 0, "caret-down", "caret-up"),
           outcome_status = fifelse(outcome < 0, "lime", "danger"),
           counter_status = fifelse(counterfactual < 0, "lime", "danger")
         )
         ,
    ]
    
    
    
    
    
    
    
  }
  
  
  return(
    data
  )
  
  
}


