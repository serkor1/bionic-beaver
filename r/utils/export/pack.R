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
          setcolorder(
            data,
            c("x",
              "allocator",
              'intervention',
              'control',
              'difference',
              'effect',
              'cintervention',
              'cdifference'
            )
          )
          
          # set effect to percentage
          # data[
          #   x > 0
          #   ,
          #   `:=`(
          #     effect = paste(
          #       effect * 100, "%"
          #     )
          #   )
          #   ,
          # ]
          
          data[
            x > 0
            ,
            effect := effect * 100
            ,
          ]
          
          
          setnames(
            data,
            old = c("x",
                    "allocator",
                    'intervention',
                    'control',
                    'difference',
                    'effect',
                    'cintervention',
                    'cdifference'
            ),
            new = c(
              'År',
              'Kategori',
              intervention_name,
              fifelse(str_detect(control_name, pattern = '[:alpha:]+'), control_name, 'Den generelle befolkning'),
              'Faktisk forskel',
              'Forventet effekt (%)',
              paste(intervention_name, '(kontrafaktisk)'),
              'Forventet forskel'
              
              ),
            skip_absent = TRUE
          )
          
          # 4) String to title
          # colnames(data) <- str_to_title(
          #   colnames(data)
          # )
          
          sheet <- unique(data$Kategori)
          
          write_ods(
            x = data,
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


.pack_model2 <- function(
    data_list,
    directory
) {
  
  #' function information
  #' 
  #' @param data_list a list of wrapped data.tables
  #' ready for storing locally in a zip file
  #' 
  #' @returns NULL
  
  map(
    data_list,
    function(element) {
      
      # 1) Copy data to avoid
      # overwriting memory
      data <- copy(element)
      
      # # 2) Remove prefix
      # # from the data
      # idx <- which(sapply(data, is.character))
      # 
      # data[
      #   ,
      #   (idx) := lapply(
      #     .SD,
      #     function(x) {
      #       
      #       str_split(x, pattern = '_', simplify = TRUE)[,2]
      #       
      #     }
      #   )
      #   ,
      #   .SDcols = idx
      # ]
      # 
      # 
      # setnames(
      #   data,
      #   old = c('assignment', 'allocator', 'outcome', 'effect'),
      #   new = c('Aldersgruppe', 'Hvem tager sygedagen?', 'Produktivitetstab', 'Sygedage')
      # )
      
      
      write_ods(
        x = data,
        path = paste0(
          directory,'/',
          'børnemodel',
          '.ods'
        ),
        append = TRUE,
        sheet = 'Børnemodel',
        update = FALSE
      )
      
    }
  )
  
}





pack <- function(
    data_list,
    intervention_name = NULL,
    control_name = NULL,
    directory = NULL,
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
    
    data_list <- .pack_model1(
      data_list         = data_list,
      intervention_name = intervention_name,
      control_name      = control_name,
      directory         = directory
    )
    
  }
  
  
  if (inherits(data_list, 'model2')) {
    
    data_list <- .pack_model2(
      data_list         = data_list,
      directory         = directory
    )
    
  }
  
  
  
  
  
  zip(
    zipfile = filename,
    files = directory
  )
  
  
  return(data_list)
  
}
