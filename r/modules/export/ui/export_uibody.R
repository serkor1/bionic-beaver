# script: ui body elements
# objective: Create a UI interface for 
# exporting data because boomers doesnt 
# know how to use this mopdel
# date: 2022-07-25
# author: Serkan Korkmaz



# generate choices for model1; #####


.exportUI_model1 <- function(
    id,
    input,
    output
) {
  
  
  ns <- NS(id)
  
  
    .param_model1(
      id = id,
      input = input,
      output = output
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
  
  # bs4Card(
  #   # title = span(
  #   #   
  #   #   radioGroupButtons(
  #   #     inputId = ns(paste0("change_views")),
  #   #     width = "500px",
  #   #     label = NULL,
  #   #     choices = c(`Model 1 <i class='fa fa-bar-chart'></i>` = "model1", ` Model 2 <i class='fa fa-line-chart'></i>` = "model2"),
  #   #     justified = TRUE, size = "sm"
  #   #   )
  #   #   
  #   #   
  #   #   
  #   # ),
  #   title = "Dataeksporteringsmodul",
  #   status = 'primary',
  #   collapsible = FALSE,
  #   solidHeader = FALSE,
  #   width = 12,
  #   maximizable = FALSE,
  #   
  #   fluidRow(
  #     p(
  #       strong("Bemærk!")
  #     )
  #   )
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  # )
  
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
      fluidRow(
        .exportUI_model1(
          id = id,
          input = input,
          output = output
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.download_model == 'model2'",
      ns = ns,
      fluidRow(
        h3("s")
      )
    )
    
    
    
    
    
    
    
    
  )
  
  
  
  
  
}