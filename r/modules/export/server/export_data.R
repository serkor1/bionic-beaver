# script: server logic of the data exporter module
# objective: Create the backend that enables
# user to add and remove elements from the
# tables
# date: 2022-07-25
# author: Serkan Korkmaz





.data_downloader <- function(id) {
  moduleServer(id,
               function(input,
                        output,
                        session)
               {
                 
                 
                 
                 req(input$pt_target_e)
                 req(input$pt_control_e)
                 req(input$pt_outcome_e)

                 # Generate Data
                 #
                 # placeholder
                 data <- reactive(
                   {

                     data_list[[1]] %>% grind(
                       intervention     = paste(input$pt_target_e),
                       control          = paste(input$pt_control_e),
                       allocators       = paste(input$pt_outcome_e),
                       chars            = paste(input$pt_demographic_e),
                       do_incidence = input$do_incident_e,
                       alternate = FALSE
                     ) %>% spread(export = TRUE)

                   }
                 )
                 
                 
                 
                 output$export_download <- downloadHandler(

                   filename = function() {
                     # WARNING: paste0 breaks the
                     # download process.
                     paste(
                       "output",
                       "zip",
                       sep="."
                       )
                     
                   },

                   content = function(fname) {
                     

                     data()[[1]] %>%
                       foo() %>%
                       baz()

                       zip(
                         zipfile = fname,
                         files = "output"
                       )
                     
                     
                     

                   },

                   contentType = "application/zip"

                 )

                 
               })
}

