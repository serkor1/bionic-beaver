# script: scr_pack
# date: Tue Sep 27 11:35:50 2022
# author: Serkan Korkmaz
# objective: Write packing function that stores the
# the data in ODS format locally.


.pack_model1 <- function(
    data_list,
    intervention_name,
    control_name,
    directory
) {
  
  
  #' function information
  #' 
  #' @param data_list a list of wrapped
  #' data.tables
  #' 
  #' @param intervention_name a character vector of length 1, used to name
  #' the intervention group.
  #' 
  #' @param control_name a character vector of length 1, used to name
  #' the control group.
  
  # 1) Extract names of the list
  # for storing data uniquely
  get_name <- names(data_list)
  
  
  name_iterator <- 0
  
  map(
    data_list,
    function(element) {
      
      # 1) Iterator
      # Set up a an iterator
      # that determines wether the ODS
      # should be appended or not.
      iterator <- 0
      name_iterator <<- name_iterator + 1
      # 2) Expport data
      # to folder
      
      
      
      
      map(
        element,
        function(data) {
          
          # 0) Copy data
          data <- copy(data)
          
          # 1) Increment iterator
          # by 1 for each loop.
          iterator <<- iterator + 1
          
          # 2) Generating appending
          # indicator for the exports
          
          do_append <- TRUE
          
          if (iterator == 1) {
            
            do_append <- FALSE
            
          } 
          
          
          # 3) Rename columns
          # by reference
          setnames(
            data,
            old = c('x', 'intervention', 'control', 'cintervention', 'difference', 'cdifference', 'effect'),
            new = c(
              'tid',
              intervention_name,
              fifelse(str_detect(control_name, pattern = '[:alpha:]+'), control_name, 'Population'),
              paste(intervention_name, '(Kontrafaktisk)'),
              'forskel',
              'forskel (Kontrafaktisk)',
              'Forventet Effekt'
              )
          )
          
          # 4) String to title
          colnames(data) <- str_to_title(
            colnames(data)
          )
          
          sheet <- unique(data$Allocator)
          
          write_ods(
            x = data[,Allocator := NULL,],
            path = paste0(
              directory,'/',
              get_name[name_iterator],
              '.ods'
            ),
            append = do_append,
            sheet = sheet,
            update = FALSE
          )
          
          
          
        }
      )
      
    }
  )
  
  
}


pack <- function(
    data_list,
    intervention_name,
    control_name,
    directory,
    char,
    filename = 'output.zip'
) {
  
  #' function information
  #' 
  #' @param data_list a list of wrapped
  #' data.tables
  #' 
  #' @param intervention_name a character vector of length 1, used to name
  #' the intervention group.
  #' 
  #' @param control_name a character vector of length 1, used to name
  #' the control group.
  
  # 1) Generate temporary directory
  # for storage
  
  directory <- 'output'
  
  if (!dir.exists(directory)) {
    
    
    dir.create(
      path = directory
    )
    
  } else {
    
    map(
      list.files(
        directory,pattern = '*.ods', full.names = TRUE
      ),
      file.remove
    )
    
    
  }
  
  
  
  if (!is.null(char)) {
    
    demographics <- data.table(
      placeholder = char
    )
    
    demographics[
      ,
      c('Variabel', 'Valgt') := tstrsplit(
        placeholder, "_"
      )
      ,
    ]
    
    write_ods(
      demographics[,.(Variabel, Valgt),],
      sheet = 'Demografi',
      paste0(directory, '/demografi.ods')
    )
    
  }
  
  

  if (inherits(data_list, 'model1')) {
    
    .pack_model1(
      data_list         = data_list,
      intervention_name = intervention_name,
      control_name      = control_name,
      directory         = directory
    )
    
  }
  
  
  
  zip(
    zipfile = filename,
    files = directory
  )
  
  
}