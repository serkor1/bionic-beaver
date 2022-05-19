# Script: Grinder-class functions
# that prepares the data for further 
# analysis


# main grinder; ####
grinder <- function(data_list, intervention, control, allocator_vector = NULL, group_value = NULL, type = TRUE, cost = FALSE) {
  #' @param data_list input data in its raw
  #' format.
  #' @param intervention character. The disease
  #' chosen as the intervention
  #' @param control character. the disease
  #' chosen as the control
  #' 
  #' @param allocator_vector character vector. The chosen outcome
  #' has to max 3.Same as the Outcome
  #' 
  #' @returns a list of data.tables in wide format.
  
  
  # Extract Grouping variables; ####
  group_variable <- .extract_grouping(
    group_value
  )
  
  
  # Collapse Allocator; ####
  allocator_vector <- allocator_vector  %>% paste(
      collapse = "|"
    )
  
  # HAd remove strings
  
  type_char <- fcase(
    isTRUE(type), 1,
    default = 0
  )
  
  cost <- fcase(
    isTRUE(cost), TRUE,
    default = FALSE
  )
  
  
  
  
  
  data_list %>% map(
    .f = function(data) {
      
      get_char <- which(sapply(data, is.character))
      
      
      
      # NOTE: All matching obs have type as NA.
      # if we do not replace these it will vanish
      data[
        disease %chin% "matching",
        type := type_char
      ]
      
      
      
      # Step 1)
      # Filter the data to include 
      # only the desired diseases
      data <- data[
        type == type_char
      ]
      
      if (cost) {
        
        data[,`:=`(outcome = cost, qty = NULL),]
        
      } else {
        
        data[,`:=`(outcome = qty, cost = NULL),]
        
      }
      
      # Steo 2)
      # Classify the data according to
      # intevention and control based on the
      # chosen intervention and control values
      
      data <- data[,
                   allocation := fcase(
                     disease %chin% "matching", "Population",
                     disease %chin% intervention, "Intervention",
                     disease %chin% control, "Control"
                   )
                   ,][!is.na(allocation)]
      
     
      
      
      
      
      
      
      
      
      
      # Step 3)
      # Filter Data based on the chosen parameters
      data <- data[
        data[
          ,
          apply(
            .SD, 
            1,
            function(x) {
              
              # The sum is always equal to the number of
              # possible combinations
              sum(x %chin% group_value) == length(group_variable)
              
              
            }
          )
          ,
          .SDcols = get_char
        ]
      ]
      
      
      
      
      
      
      
      # Step 4) 
      # Aggregate Results by year and group
      # accordingly
      # TODO: Is it per patient or total
      # this is important.
      # TODO: Normalize Outcomes variables
      # to do a more smooth transition.
      # This might be a good idea generate these
      # outcome via paramters
      
      data <- data[
        str_detect(
          string = allocator,
          pattern = allocator_vector
        )
        ,
        list(
          outcome = mean(outcome, na.rm = TRUE)

        )
        ,
        by = c(
          "year", "allocation", "disease", "allocator", group_variable
        )
      ]
      
      
      
      return(data)
      
    }
  )
}



# plot_grinder; ####
plot_grinder <- function(data, group_value = NULL) {
  
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
  
  
  if (nrow(data) != 0) {
    
    # Store Class of the data;
    get_class <- class(data)[3]
    

    data <- data[
      ,
      list(
        outcome = mean(
          outcome, na.rm = TRUE
        )
      )
      ,
      by = .(year, allocation, allocator)
    ]
    
    
    data <- data %>% dcast(
      year + allocator ~ allocation,
      value.var = "outcome"
    )
    
    
    # Outcome is the difference between
    # Treatment and Control.
    
    tryCatch(
      expr = {
        
        data[,
             difference := sum(Intervention, -Control)
             ,
             by = 1:nrow(data)
        ]
        
      },
      
      error = function(cond){
        
        # Test for missing column name and add
        existing_columns <- colnames(data) %>%
          str_extract(pattern = "Control|Intervention") %>% na.omit()
        
        # Needs the colums
        needed_colums <- c("Control", "Intervention")
        
        # Extract missing COlums
        missing_column <- needed_colums[!(needed_colums %chin% existing_columns)]
        
        data[
          ,
          (missing_column) := NA
          ,
        ]
        
        
        data[,
             `:=`(
               difference = 0
             )
             
             ,
             by = 1:nrow(data)
        ]
        
    
      }
    )
    
    
    
    # Reclass the data
    class(data) <- c(class(data), get_class) 
    
  } else {
    
    data = NULL
    
  }
  
  
  return(
    data  
  )
  
  
}




# table grinder; #####

table_grinder <- function(data, group_value = NULL, do_aggregate = TRUE, intervention_cost = 1000, intervention_effect = 0.5) {
  
  #' @param data a data.table in long format. Should include only
  #' one care type.
  #' 
  #' @param do_aggregate Logical. If TRUEit will calculate the costs
  #' by group across all years
  #' 
  #' @param group_value Charcter Vector. These are the actual values
  #' that are to be calculated
  #' 
  #' @return data.table in wideformat.
  
  
  # Step 1) 
  # Locate Group variables
  
  group_variable <- .extract_grouping(
    group_value
  )
  
  
  # Step 2) 
  # dcast to long and filter
  data <- dcast(
    data,
    paste0(
      paste(c("year",group_variable), collapse = "+"), "~ allocation"
    ),
    value.var = "outcome"
  )
  
  if (!is.null(group_value)) {
    
    data <- data[eval(
      parse(
        text = paste(
          group_variable,
          sep = "%chin%",
          quote(group_value),
          collapse = "&"
        )
      )
    )]
  } 
  
  
  
  
  
  # step 3)
  # reorder columns
  # so Intervention is followed by Control
  setcolorder(
    data,
    neworder = c(
      setdiff(
        colnames(data), c("intervention", "control")
      ),
      c("intervention","control")
    )
  )
  
  # step 3)
  # Calculate differences
  data[,difference := intervention - control,]
  
  
  # Step 4)
  # Conditional aggregation
  if (do_aggregate) {
    
    # Calculate Differences by grop
    # on an aggregate level
    data <- data[,
                 list(
                   intervention = sum(intervention),
                   control      = sum(control),
                   difference   = sum(difference)
                 ),
                 by = c(group_variable)]
    
    # Calculate totals
    totals <- data[,
                   list(
                     intervention = sum(intervention),
                     control      = sum(control),
                     difference   = sum(difference)
                   )
    ]
    
    # Bind the totals
    data <- rbind(
      data,
      totals,
      fill = TRUE
    )
    
    
    # Calculate effects
    effect <- totals[,
                     
                     list(
                       intervention = sum(intervention) * (1 - intervention_effect),
                       control      = sum(control)
                     )
                     
                     ,][
                       ,
                       difference := intervention - control
                       ,
                     ]
    
    
    data <- rbind(
      data,
      effect,
      fill = TRUE
    )
    
    # Calculate the difference between
    # the diference after the intervention effect
    difference <- data.table(difference = totals$difference - effect$difference)
    
    data <- rbind(
      data,
      difference,
      fill = TRUE
    )
  } else {
    
    
    # Calculate Differences by grop
    # on an aggregate level
    data <- data[,
                 list(
                   intervention = sum(intervention),
                   control      = sum(control),
                   difference   = sum(difference)
                 ),
                 by = c("year",group_variable)]
    
    
    # At this stage you can add effects and costs
    # TODO: Do this at a later stage
    
    
    # Reformat to Long
    data <- melt(
      data,
      id.vars = c("year",group_variable),
      variable.name = "group"
    )
    
    # Reformat to Wide
    data <- dcast(
      data,
      ... ~ year + group
    )
  }
  
  
  return(
    data
  )
  
}






foo <- function(data) {
  
  #' This function converts the data to wide
  #' 
  #' TODO: This should replace info_grinder and
  #' plot_grinder and table_grinder
  
  
  
  if (nrow(data) == 0) {

    return(NULL)

  }
  
  
  # Extract the Class; 
  store_class <- class(data)[3]
  
  
  
  data <- data[
    ,
    list(
      outcome = mean(
        outcome, na.rm = TRUE
      )
    )
    ,
    by = .(year, allocation, allocator)
  ]
  
  
  data <- data %>% dcast(
    year + allocator ~ allocation,
    value.var = "outcome"
  )
  
  
  
  tryCatch(
    expr = {
      
      data[,
           difference := sum(Intervention, -Control)
           ,
           by = 1:nrow(data)
      ]
      
    },
    
    error = function(cond){
      
      # Test for missing column name and add
      existing_columns <- colnames(data) %>%
        str_extract(pattern = "Control|Intervention") %>% na.omit()
      
      # Needs the colums
      needed_colums <- c("Control", "Intervention")
      
      # Extract missing COlums
      missing_column <- needed_colums[!(needed_colums %chin% existing_columns)]
      
      data[
        ,
        (missing_column) := NA
        ,
      ]
      
      
      data[,
           `:=`(
             difference = 0
           )
           
           ,
           by = 1:nrow(data)
      ]
      
      
    }
  )
  
  
  # Reclass the data
  class(data) <- c(class(data), store_class) 
  
  
  return(data)
  

}


baz <- function(data, effect, aggregate = FALSE) {
  
  # I forgot what this was
  
  #' Get data
  
  if (aggregate) {
    
    by_vector <- c("allocator")
    
  } else {
    
    by_vector <- c("allocator", "year")
    
  }
  
  
  
  data[
    year > 0,
    effect := (effect/100),
    by = .(allocator)
  ]
  
  data[
    !is.na(effect),
    `:=`(
      cDifference   = difference * effect,
      # Was abs - but not used.
      cIntervention = max(Intervention - ((difference * effect)),0)
    ),
    by = by_vector
  ]
  
  data[
    year == 0,
    `:=`(
      effect = 0,
      cIntervention = Intervention
    )
    
  ]
  
  
  
  
  
  
  
  
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


