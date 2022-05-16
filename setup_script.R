gather_parameter <- function() {

  library(magrittr)
  library(data.table)
  library(stringr)
  library(purrr)

  # Gather Data
  data <- lapply(
    list.files("input/data",full.names = TRUE),
    fread
  )


  # wrtie demographics
  demographics <- unique(data[[1]][,-c("year", "type", "allocator", "cost", "qty", "disease"),])

  # Add Mock variable
  demographics[,mock := 1,]

  demographics <- demographics %>% melt(id.vars = "mock") %>% unique()

  demographics <- demographics[,mock := NULL,][
    ,
    demographics := paste0(
      variable, "_", value
    )
    ,
  ][,list(x = demographics),]



  fwrite(
    demographics,sep = ";",
    file = "input/parameters/demographics.csv"
  )


  # Write diseases
  diseases <-  unique(data[[1]][,list(x = disease),])[
    str_detect(x, pattern = "_")
  ]


  fwrite(
    diseases,sep = ";",
    file = "input/parameters/diseases.csv"
  )



  # Allocator
  allocator <- data %>% map(
    .f = function(data) {
      
      # We got a empty for some reason. No clue why.

      unique(data[,list(x = allocator)])[str_detect(x, "[:alpha:]+")]


    }
  ) %>% rbindlist()

  fwrite(
    allocator,sep = ";",
    file = "input/parameters/allocator.csv"
  )



}



gather_parameter()
