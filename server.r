server <- function(input, output, session) {
  
  
  # Start of Server Logic;
  
  #
  observe({
    
    message(
      paste(
        "Chosen Tab:", paste(input$tab)
      )
    )
    
  })
  
  
  shinyjs::onclick(
    id = 'tab-export_data',
    function() {
      
      temp <- exportUI(
        id = 'exporter'
      )
      
      
      showModal(
        ui = temp$body
      )
      
      
    }
    
  )
  
  
  shinyjs::onclick(
    id = 'tab-documentation',
    function() {
      
      runjs(
        'window.open(
        "documentation/_book/index.html"
        )'
      )
      
      
    }
    
  )
  
  
  
  
  
  
  # UI rendering; #####
  observeEvent(
    input$tab,
    ignoreInit = FALSE,
    ignoreNULL = FALSE,
    {
      
      if (input$tab %chin% c('front_page', 'model_1','model_2')){
        
        ui <- frontUI(
          id = "front"
        )
        
        if (input$tab != 'front_page') {
          
          if (input$tab == 'model_1'){
            
            ui <- model1UI(
              id = "model1"
            )
            
          }
          
          if (input$tab == 'model_2'){
            
            ui <- model2UI(
              id = "model2"
            )
            
          }
          
        }
        
        output$gen_body <- renderUI({
          ui$body
        })
        
      }
      
      
      
      
      
    }
  )
  
  
  
  
  # server modules; #####
  .data_downloader(
    'exporter'
  )
  
  
  .model1server_plot(
    id = "model1",
    data_list = data_list[[1]]
  )
  
  
  .model1server_choices(
    id = 'model1'
  )

  
    .model1server_warnings(
      id = 'model1'
    )
  
    
  .model2server_choices(id = 'model2')


  .model2server_plot(
    id = 'model2',
    data_list = data_list[[2]]
  )
  
  
  
  
  
  
  
  
  
  
  waiter::waiter_hide()
  
  # end of server logic;
  
}

