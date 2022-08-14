# script: UI of Model2
# objective: Create Information Boxes of chosen parameters
# date: 2022-08-14
# author: Serkan Korkmaz

.model2UI_choices <- function(
    id,
    output
) {
  
  #' function information
  #' 
  #' This function creates a UI 
  #' with chosen paraneters
  ns <- NS(id)
  
  
  fluidRow(
    
    # First Colum;
    column(
      width = 6,
      bs4InfoBox(
        width = 12,
        title = strong('Valgt gruppe'),
        subtitle = textOutput(ns("chosen_intervention"),inline = TRUE),
        color = 'primary',
        fill = TRUE,
        gradient = TRUE
      ),
      
      bs4InfoBox(
        width = 12,
        title = strong('Valgt sammenligningsgruppe'),
        subtitle = textOutput(ns("chosen_control"),inline = TRUE),
        color = 'primary',
        fill = TRUE,
        gradient = TRUE
      )
      ),
    
    # second column
    column(
      width = 6,
      bs4InfoBox(
        width = 12,
        title = strong('Valgt uddannelsesniveau'),
        subtitle = textOutput(ns("chosen_educ"),inline = TRUE),
        color = 'primary',
        icon = icon("user-graduate"),
        fill = TRUE,
        gradient = TRUE
      ),
      
      bs4InfoBox(
        width = 12,
        title = strong('Valgt familietype'),
        subtitle = textOutput(ns("chosen_type"),inline = TRUE),
        color = 'primary',
        icon = icon('address-book'),
        fill = TRUE,
        gradient = TRUE
      )
      
    )
      
      
    )
  
    
  
  
  
}






.options_plot_model2 <- function(
    id,
    input,
    output
) {
  
  ns <- NS(id)
  
  column(
    width = 12,
    h5("Grafindstillinger"),
    
    
    colorPickr(
      inputId = ns("col_intervention"),
      preview = TRUE,
      label = 'Intevention',
      selected = '#4682B4',
      update = 'changestop'
    ),
    
    colorPickr(
      inputId = ns("col_control"),
      preview = TRUE,
      label = 'Control',
      selected = '#FFA500',
      update = 'changestop'
    ),
    
    colorPickr(
      inputId = ns("col_background"),
      preview = TRUE,
      label = 'Baggrundsfarve',
      selected = '#FFFFFF',
      update = 'changestop'
    ),
    
    
    actionButton(
      inputId = ns("col_reset"),
      label = "Nulstil Farver"
    )
    
    
  )
  
  
}