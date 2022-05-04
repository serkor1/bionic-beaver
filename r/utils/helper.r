# This script contains helper functions
# like Descriptive Tables with Borders
# and plotting functions.
# NOTE: This script follows the Roxygen Documentation
# standards.




# This function extracts the column names
# needs rework at some point.
# TODO: Make it an internal hidden function
.extract_grouping <- function(group_value) {
  
  
  #' @param group_value the individual values
  #' chosen from UI. Character vector.
  #' 
  #' @return Charcter Vector of column names that 
  #' the data are to be grouped by.
  
  #' TODO: These are hardcoded as of now
  #' but should be programmatically coded
  
  data <- data_list$primary_care
  
  # Get all the values
  # from the data
  køn <- data[,unique(køn),]
  uddannelse <- data[,unique(uddannelse),]
  arbejdsmarked <- data[,unique(arbejdsmarked),]
  alder <- data[,unique(alder),]
  
  
  
  get_col <- c(
    fcase(sum(køn %chin% group_value) > 0, "køn",default = NA),
    fcase(sum(uddannelse %chin% group_value) > 0, "uddannelse",default = NA),
    fcase(sum(arbejdsmarked %chin% group_value) > 0, "arbejdsmarked",default = NA),
    fcase(sum(alder %chin% group_value) > 0, "alder", default = NA)
  )
  
  
  get_col[!is.na(get_col)]
  
  
  
}


# Data Grinder; ####

.extract_data <- function(data_list,outcome = NULL) {
  
  #' @param outcome the chosen outcomes from UI
  #' used to extract the data needed for the grinder
  #' 
  #' @param data_list a preloaded list of data in raw
  #' format.
  #' 
  #' @return a list of the needed data in long format
  #' in its raw format with with classes
  
  # This returns a character vector
  # of either contacts, transfers and tax
  get_data <- fcase(
    outcome %chin% c("Almen Praksis", "Psykolog", "Psykiater", "Fysioterapi", "Anden Speciale"), "primary_care",
    str_detect(string = outcome, "psy"), "psychiatric_care",
    str_detect(string = outcome, "som"), "somatic_care",
    default = "transfers"
  )
  
  data_list[
    data_list %>% names() %chin% get_data
  ]
  
  
}


generate_table <- function(data) {
  
  #' @param data takes a data.table that has
  #' unique care values.
  #' 
  #' @returns Shiny Table
  
  # Step 1) Check column names
  # if it contains numbers then it is 
  # disaggregated yearly data
  # 
  # ie 2012_control etc
  
  col_name <- colnames(
    data
  )
  
  has_year <- str_detect(
    col_name,
    pattern = "[:digit:]"
  )
  
  if (sum(has_year) != 0) {
    
    
    year_vector <- colnames(data)[has_year] 
    
    
    # Count Grouping Columns
    # as these has to be skipped
    # in the header process
    grouping_col <- sum(!has_year) - 1
    
    container <- htmltools::withTags(table(
      class = 'display',
      thead(
        
        
        tr(
          th(rowspan = 2, ''),
          lapply(1:grouping_col, function(x){th(rowspan = 2, '')}),
          lapply(
            1:8,
            function(x) {
              
              th(
                colspan = 3,
                paste("Year", x + 2011)
              )
              
              
              
            }
          )
        ),
        tr(
          lapply(rep(c('Intervention', 'Control', "difference"), 8), th)
          
        )
      )
    ))
    
    
    
    
  }
  
  
  
  data %>% 
    DT::datatable(
      container = container,
      rownames = FALSE,
      options = list(
        scrollX = FALSE,
        scrollY = FALSE,
        dom = 't',
        searching = FALSE,
        columnDefs =list(
          list(
            className = "dt-head-center dt-center", targets = 1:(ncol(data) - 1)
          )
        )
      )
    ) %>% formatStyle(
      ncol(data), `border-right` = "solid 1px"
    ) %>% formatStyle(1, `border-left` = "solid 1px")
  
  
  
}




