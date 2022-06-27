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
library(fresh)

# Shiny BS

# Developper Mode;
developper_mode = TRUE



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
  data_list <-  preload_data(
    developper_mode = developper_mode
  ) %>% map(
    .f = function(get_list) {

      # Each iteration is over the lists
      # so we need another map

      map(
        get_list,
        .f = function(data) {

          data %>%
            .convert_long()

        }
      )

    }
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

