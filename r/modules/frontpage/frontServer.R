




mainDownload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    message("Downloading Maindata")
    
    output$mainDownload <- downloadHandler(
      
      filename = function() {
        paste("data", "zip", sep = ".")
      },
      
      content = function(fname) {
        
        zip(zipfile = fname, files = "input/data/")
      },
      
      contentType = "application/zip"
      
    )
    
    
    
  }
  
  
  )
  
  
  
  
}

