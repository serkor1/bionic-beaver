frontUI <- function(id) {
  
  # Generate Namespace; ####
  ns <- NS(id)
  
  
  ui_body <- tagList(
    
    
    
    fluidRow(
      column(
        width = 6,
        
        bs4Card(
          title = "Velkommen",
          icon = icon("home", verify_fa = FALSE),
          width = 12,
          status = "warning",height = "500px",
          collapsible = FALSE,
          
          
            
            h3("Den Sunhedsøkonomiske Investeringsmodel"),
            hr(),
            p(
              "Modellen er udviklet af VIVEs Sundhedsafdeling i samarbejde med LIF. Modellen er open-source og er derfor til fri afbenyttelse."
            )
            
            
            
          
          
          
        )
        
        
      ),
      
      
      column(
        width = 6,
        bs4Card(
          title = "Guide",
          width = 12,
          icon = icon("info", verify_fa = FALSE),
          status = "info",height = "500px",
          collapsible = FALSE,
          
          h3("Hvordan Starter jeg?"),
          
          p(
            "Der er to modeller. Model1 og Model2."
          )
          
          
        )
        
      )
      
      
      
    ),
    
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title = "Disclaimer",
          status = "danger",
          width = 12,
          collapsible = FALSE,
          icon = icon("exclamation", verify_fa = FALSE),
          
          p(
            "Brugen af modellen er på eget ansvar. 
            Modellen er ubegrænset ift. sammenligninger og det er derfor hensigtsmæssigt at bruge den med sund fornuft.
            Hvis man er interesseret i data kan det downloades her."
            ),
          
          
          
          downloadButton(
            outputId = ns("mainDownload"),
            label = "Download Data"
          )
          
        )
      )
    )
    
    
    
    
    
    
    
    
    
    
    
    
  )
  
  
  return(
    list(
      #sidebar    = ui_sidebar,
      body       = ui_body
      #header     = ui_header
    )
    
  )
  
  
  
  
  
}