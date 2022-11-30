frontUI <- function(
) {
  
  # generate modal
  front_modal <- modalDialog(
    title = span(icon('home', verify_fa = FALSE), 'Velkommen til Beregner for Investeringer i Sundhed (BIS)'),
    size = 'xl',
    easyClose = TRUE,
    footer = modalButton(
      'Start',
      icon = icon('play', verify_fa = FALSE)
    ),
    fluidRow(
      hr(),
      accordion(
        id = 'front_accordion',
        accordionItem(
          title = span(icon('biohazard', verify_fa = FALSE),'Disclaimer'),
          status = 'danger',
          collapsed = FALSE,
          
          includeMarkdown(
            'desc/disclaimer.Rmd'
          )
          
        ),
        accordionItem(
          title = span(icon(name = 'circle-info', verify_fa = FALSE), 'Hvad er BIS?'),
          status = 'primary',
          collapsed = TRUE,
          
          
          includeMarkdown(
            'desc/description.Rmd'
          )
          
        ),
        accordionItem(
          title = span(icon(name = 'code-compare', verify_fa = FALSE), 'Versionshistorik'),
          collapsed = TRUE,
          status = 'info',
          column(
            width = 12,
            
            h4(
              paste(
                'Version', version
              )
            ),
            
            p(
              paste(
                'Opdateret', Sys.Date()
              )
            ),
            
            column(
              12,
              includeMarkdown(
                'desc/version_hist.Rmd'
              )
            )
            
            
            
          )
        ),
        accordionItem(
          title = span(icon(name = 'heart', verify_fa = FALSE), 'Open source'),
          collapsed = TRUE,
          status = 'warning',
          includeMarkdown(
            'desc/opensource.Rmd'
          )
        )
        
      )
      
      
    )
    
  )
  
  
  
  showModal(
    front_modal
  )
  
}