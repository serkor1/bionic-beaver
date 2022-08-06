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
                 
                 
                 # Observer for Logging: ######
                  observe({
                    
                    req(input$download_model)
                    
                    message(
                      paste(
                        "Exporter Module:", input$download_model
                      )
                      
                    )
                    
                  })
                  
                 
                  
                  {
                    
                    
                    # Observer: updatePicker + logging; ####
                    observe({
                      
                      req(input$pt_sector_e)
                      
                      message("Updated Picker")
                      
                      updatePickerInput(
                        inputId = 'pt_outcome_e',
                        session = session,
                        choices = outcome[[1]][paste(input$pt_sector_e)],
                        
                      )
                    })
                    
                    
                  }
                  
                  

                  
                  # Generate Data
                  #
                  # placeholder
                  data <- reactive(
                    {
                      
                      req(input$pt_target_e)
                      req(input$pt_control_e)
                      req(input$pt_outcome_e)
                      
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
                        "data",
                        "ods",
                        sep="."
                      )
                      
                    },
                    
                    content = function(fname) {
                      
                      
                      data() %>% 
                        prep_export() %>% write_export(fname = fname)
                      
                      
                      
                      
                      
                      
                    }
                    
                    # ,
                    # 
                    # contentType = "application/zip"
                    
                  )
                  
                
                 

                 
                 
                 
                 # End of Server
               }
               )
}

