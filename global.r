# Clear Workspace and Setup; ####
rm(list = ls()); gc()


# packages;
library(tidyverse)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(data.table)
library(waiter)
library(plotly)
library(waiter)
library(DT)


# Shiny BS

# Developper Mode;
developper_mode = FALSE



# Load Modules and Utilities; #####
list.files(
  path = "r",
  recursive = TRUE,
  full.names = TRUE
) %>% map(
  source,
  encoding = 'UTF-8'
  
)




# Preload data;
system.time(
  data_list <- preload_data(
    developper_mode = developper_mode
  )
)


data_list[[1]]

# version;
version <- c("v0.2")



# Preloading Options; #####

# All options are created
# and are on the form [class]_[subclass]
options <- get_options()

diseases <- options$diseases

outcomes <- options$allocator

# demographics are constructed as
# variable_value
demographics <-  options$demographics %>% 
  map(
    .f = function(element) {
      str_remove(
        element,
        pattern = "[:alpha:]+_"
      )
      
      }
    
    )


