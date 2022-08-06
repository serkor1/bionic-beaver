# script: ui body elements
# objective: Create a UI interface for 
# exporting data because boomers doesnt 
# know how to use this mopdel
# date: 2022-07-25
# author: Serkan Korkmaz



# generate choices for model1; #####


.eparam_model1 <- function(id, input, output) {
  
  #' function information
  #' 
  #' Disclaimer: This function will eventually 
  #' be discarded, as it is currently not compatible
  #' with modules with same inputIds. 
  ns <- NS(id)
  
  fluidRow(
    
    
    column(
      width = 6,
      
      vive_picker(
        id = ns("pt_target_e"),
        title = "Gruppe",
        header = "Vælg Gruppe",
        choices = assignment[[1]],
        multiple = FALSE
      ),
      
      vive_picker(
        id = ns("pt_outcome_e"),
        title = "Outcomes",
        header = "Vælg Outcomes",
        choices = outcome[[1]],
        multiple = TRUE,
        selectAllText = "Vælg Alle",
        deselectAllText = "Nulstil"
      )
      
      
    ),
    
    column(
      width = 6,
      vive_picker(
        id = ns("pt_control_e"),
        title = "Sammenligningsgruppe",
        header = "Vælg Sammenligningsgruppe",
        choices = assignment[[1]],
        multiple = FALSE
      ) ,
      
      vive_picker(
        id = ns("pt_demographic_e"),
        title = "Demografi",
        header = "Vælg Demografiske",
        choices = chars[[1]],
        multiple = TRUE,
        selectAllText = "Vælg Alle",
        deselectAllText = "Nulstil"
      )
    ),
    
    
    column(
      width = 12,
      materialSwitch(
        inputId = ns("do_incident_e"),
        label = "Incidente Patienter?",inline = TRUE,
        value = FALSE,
        status = "primary"
      )
    )
    
    
    
    
    
    
  )
  
  
  
}





# collect UI body
.exportUI_body <- function(
    input,
    output,
    id
) {
  
  #' function information
  #' 
  #' 
  #' 
  
  ns <- NS(id)
  
  
  tagList(
    modalDialog(
      title = "Download Data",
      size = 'xl',
      footer = span(
        
        downloadButton(
          ns("export_download"),
          label = "Download"
        ),
        modalButton("Luk")
      ),
      easyClose = TRUE,
      
      fluidRow(
        column(
          width = 12,
          radioGroupButtons(
            inputId = ns('download_model'),
            width = "100%",
            justified = TRUE,
            choiceNames  = c("Model 1", "Model 2"),
            choiceValues = c("model1", "model2")
          )
        )
        
      ),
      
      
      hr(),
      
      conditionalPanel(
        condition = "input.download_model == 'model1'",
        ns = ns,
        .eparam_model1(
          id = id,
          input = input,
          output = output
        )
        
      ),
      
      conditionalPanel(
        condition = "input.download_model == 'model2'",
        ns = ns,
        fluidRow(
          h3("Kommer ved næste opdatering!")
        )
      )
      
      
      
      
      
      
      
      
    )
  )
  
  
  
  
  
  
  
  
}