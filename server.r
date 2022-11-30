# script: server logic
# date: Thu Oct 27 11:28:31 2022
# author: Serkan Korkmaz
# objective: Generate a unified server
# for the applikation


server <- function(
    input,
    output,
    session
){
  
  # The Controlbar is disabled and hidden
  # as bs4dash is bugged once changin skin color
  # without skinselector()
  disable(
    'controlbar-toggle'
  )
  hide(
    'controlbar-toggle'
  )
  
  # Remove circle from the
  # icon to force a flat
  # image.
  removeCssClass(
    selector = '.brand-image',
    class = 'img-circle elevation-3'
  )
  
  
  # Click help_switch otherwsie 
  # its bugged.
  # NOTE: This does not do anything
  # as of now.
  shinyjs::click(
    id = 'help_switch'
    )
  
  # On server start; ####
  
  # Load the front page as a modal
  frontUI()
  
  # Load the main model
  # for the user such that 
  # it is not dependent on
  # buttons pressed.
  ui <- model1UI(
    id = "model1"
  )
  
  output$gen_body <- renderUI(
    {
      ui$body
    }
  )
  

  # Start of Server Logic; ####
  
  shinyjs::onclick(
    id = 'tab-model_2',
    function() {
      ui <- model2UI(
        id = "model2"
      )
      showModal(
        ui = ui$body
      )
    }
  )
  
  # The in-app documentation
  # is removed for now. Will be
  # implemented at a later time.
  shinyjs::onclick(
    id = 'tab-report',
    function() {

      runjs(
        'window.open(
        "https://www.vive.dk/da/udgivelser/teknisk-dokumentation-bis-beregner-for-investeringer-i-sundhed-18419/"
        )'
      )


    }

  )
  
  
  # The in-app version history
  shinyjs::onclick(
    id = 'tab-version_history',
    function() {
      
      showModal(
        modalDialog(
          title     = span(icon('code-compare', verify_fa = FALSE),'Versionshistorik'),
          size      = 'l',
          easyClose = TRUE,footer = modalButton("Luk"),
          
          
          column(
            width = 12,
            
           version_text,
            
            column(
              12,
              includeMarkdown(
                'desc/version_hist.Rmd'
              )
            )
              
              
            
          )
          
          
          
        )
      )
      
      
    }
    
  )
  
  # NOTE: This section
  # is disabled ant the id is currently
  # a boomer-proof button
  # shinyjs::onclick(
  #   id = 'tab-model_1',
  #   function() {
  # 
  #     ui <- model1UI(
  #       id = "model1"
  #     )
  # 
  #     output$gen_body <- renderUI({
  #       ui$body
  #     })
  # 
  #   }
  # )
  
  # Model 1 module:
  .model1server_output(
    id = "model1",
    data_list = data_list[[1]],
    get_switch = reactive(
      input$customSwitch1
    )
  )
  
  
  .model1server_choices(
    id = 'model1'
  )
  
  
  .model1server_warnings(
    id = 'model1'
  )
  
  # Model2 module:
  .model2server(
    id = 'model2',
    data_list = data_list[[2]],
    get_switch = reactive(input$customSwitch1)
  )
  
  
  
  
  
  
  waiter::waiter_hide()
  
  # end of server logic; ####
  
}

