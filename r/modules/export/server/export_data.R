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
                 
                 
                 
                 req(input$pt_target)
                 req(input$pt_control)
                 req(input$pt_outcome)
                 
                 # Generate Data
                 # 
                 # placeholder
                 data <- reactive(
                   {
                     
                     data_list[[1]] %>% grind(
                       intervention     = paste(input$pt_target),
                       control          = paste(input$pt_control),
                       allocators       = paste(input$pt_outcome),
                       chars            = paste(input$pt_demographic),
                       alternate = FALSE
                     ) %>% spread(export = TRUE)
                     
                   }
                 )
                 
                 
                 
                 output$export_download <- downloadHandler(

                   filename = function() {
                     paste("output", "zip", sep=".")
                   },

                   content = function(fname) {
                     
                     
                     
                     
                     data()[[1]] %>%
                       foo() %>%
                       baz()

                     

                     zip(
                       zipfile = fname,
                       files = "output/"
                       )

                   },

                   contentType = "application/zip"

                 )

                 
               })
}

