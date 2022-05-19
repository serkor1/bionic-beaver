server <- function(input, output, session) {
  
  message("Frontpage Rendered")
  
  
  front_ui <- frontUI(
    id = "front"
  )
  
  mainDownload_server(
    "front"
  )
  output$gen_body <- renderUI(
    {
      front_ui$body
    }
  )
  
  
  output$sidebar_ui <- renderUI(
    NULL
    
  )
  
  output$gen_header <- renderUI(
    {
      NULL
    }
  )
  
  # Frontpage; #####
  observeEvent(
    input$front_page,
    ignoreInit = FALSE,
    ignoreNULL = FALSE,
    {
      message("Frontpage Rendered")
      
      
      front_ui <- frontUI(
        id = "front"
      )
      
      mainDownload_server(
        "front"
      )
      output$gen_body <- renderUI(
        {
          front_ui$body
        }
      )
      
      
      output$sidebar_ui <- renderUI(
        NULL
        
      )
      
      output$gen_header <- renderUI(
        {
          NULL
        }
      )
      
      
      
      
      
    }
    
    
    
  )
  
  
  
  
  
  # Model 1; ####
  
  observeEvent(
    input$model_1,
    {
      
        
        message("Model 1 Chosen")
        
        
        model1_ui <- model1UI(
          id = "model1"
        )
        
        
        
        output$sidebar_ui <- renderUI(
          model1_ui$sidebar
          
        )
        
        
        output$gen_body <- renderUI(
          {
            model1_ui$body
          }
        )
        
        output$gen_header <- renderUI(
          {
            model1_ui$header
          }
        )
        
        
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    }
  )
  
  
  
  
  # Generate Data for Model 1
  data <- isolate(
    {
      reactive(
        main_dataserver(id = "model1")
      )
    }
  )
  
  observe({
    
    effect <- main_effectserver(id = "model1")
    
    
    main_warnings(
      id = "model1"
    )



    # Generate Chosen Parameters
    main_choiceserver(
      id = "model1",
      data = data()
    )

    # Generate Plots
    main_plotserver(
      id = "model1",
      data = data(),
      intervention_effect = effect,
      light_mode = reactive(input$customSwitch1)
    )
    
    # Generate
    
    main_tableserver(
      id = "model1",
      data = data()
    )
    # main_infobox(
    #   id = "model1",
    #   data = data()
    # )
    
    
    
    
  }
  
  )
  
  
  
  
  
  
  
  waiter::waiter_hide()
  
  
  
}

