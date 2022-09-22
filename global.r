# Clear Workspace and Setup; ####
rm(list = ls()); gc()

# packages;
suppressPackageStartupMessages(
  {
    # Essential Shiny libraries
    library(shiny)
    library(bs4Dash)
    library(shinyjs)
    library(shinyWidgets)
    library(shinycssloaders)
    library(shinyFeedback)
    library(fresh)
    library(waiter)
    library(DT)
    
    # Essential Backend libraries
    library(stringr)
    library(data.table)
    library(plotly)
    library(purrr)
    library(rhandsontable)
    library(readODS)
    library(formattable)
  }
)


# Developper Mode;
developper_mode = FALSE

# Setup script; ####
# 
# If the parameters have not been loaded
# execute the scripts
check_parameter <- list.files(
  path = "input/parameters/",
  recursive = TRUE
) %>% length()

if (check_parameter == 0) {
  
  message("Running Price-parameter scripts!")
  
  map(
    list.files(
    path = "r/setup/",
    full.names = TRUE
  ),
  source,
  encoding = 'UTF-8'
  )
  
}




# Load Modules and Utilities; #####
list.files(
  path = "r",
  recursive = TRUE,
  full.names = TRUE
) %>% map(
  source,
  encoding = 'UTF-8'
  
)


# preload data;
set.seed(1903)
system.time(
  data_list <- preload_data(
    developper_mode = developper_mode
  )
)


# version;
version <- c("v0.3.1")



# Preloading Options; #####

load_parameters <- .gen_option(
  data_list = data_list
)


chars      <- load_parameters$chars
assignment <- load_parameters$assignment
outcome    <- load_parameters$outcome
lookup     <- .gen_lookup(data_list)

