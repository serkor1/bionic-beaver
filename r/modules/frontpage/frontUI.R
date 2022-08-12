frontUI <- function(id) {
  
  # Generate Namespace; ####
  ns <- NS(id)
  
  
  ui_body <- tagList(
    
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title = "Disclaimer",
          status = "danger",
          solidHeader = TRUE,
          background = 'danger',
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          icon = icon("exclamation", verify_fa = FALSE),
          
          p(
            "Brugen af modellen sker efter den enkelte brugers valg af beregningsgrupper og antagelser. 
VIVE har udtrukket data for alle individer med de pågældende diagnoser og opstillet modellens beregningsramme. 
Der er ingen tekniske begrænsninger på, hvilke grupper man kan sammenligne, så det er brugeren af modellen, der skal stå inde for, at beregningen giver mening."
          )

        )
      )
    ),
    
    
    fluidRow(
      
      column(
        width = 6,
        
        bs4Card(
          title = "Velkommen",
          icon = icon("home", verify_fa = FALSE),
          width = 12,
          status = "warning",height = "500px",
          collapsible = FALSE,
          
          
            
            h3("Den Sundhedsøkonomiske Investeringsmodel"),
            hr(),
            p(
              "Modellen giver mulighed for at foretage økonomiske sammenligninger af en række patientgrupper. For hver patientgruppe er opgjort deres gennemsnitlige forbrug af en række offentlige ydelser samt deres tilknytning til arbejdsmarkedet. 
Tallene kan bruges til at sammenligne forskellige patientgrupper eller beregne det økonomiske potentiale ved tilbud til patientgrupperne.
Modellen er udviklet af VIVE finansieret af Lægemiddelindustriforeningen. Data til modellen er dokumenteret her (der skal komme et link, når tid er)."
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
          
          h3("Hvordan starter jeg?"),
          
          p(
            "Der er to modeller. Model1 og Model2."
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