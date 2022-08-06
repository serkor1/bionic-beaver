server <- function(input, output, session) {
  
  
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
  # UI - Rendering; #####
  # 
  # It is based on input$tab
  
  observeEvent(
    input$tab,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      
      # Frontpage
      if (input$tab == "front_page") {
        
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
        

        
      }
      
      if (input$tab == "model_1") {
        
        
        message("In Model 1")

        model1_ui <- model1UI(
          id = "model1"
        )




        output$gen_body <- renderUI(
          {
            model1_ui$body
          }
        )
        
        
      }
      
      if (input$tab == "model_2") {
        
        message("in model 2")
        
        
        model2_ui <- model2UI(
          id = "model2"
        )
        
        
        output$sidebar_ui <- renderUI(
          model2_ui$sidebar
          
        )
        
        
        output$gen_body <- renderUI(
          {
            model2_ui$body
          }
        )
        
        output$gen_header <- renderUI(
          {
            model2_ui$header
          }
        )
        
        output$performance <- renderUI(
          {
            model2_ui$performance
          }
        )
        
        
        
        
        
      }
      
     
      
    }
  )
  
  
  
  shinyjs::onclick(
    id = 'tab-export_data',
    function() {

      export_ui <- exportUI(
        id = 'exporter'
      )


      showModal(
        ui = export_ui$body
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
  
  
  
  
  
 
  
  observeEvent(
    input$tab == "export_data",
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    
    {
      
      .data_downloader(
        'exporter'
      )
      
    }
  )
  
  
  # observe(
  #   {
  #   
  # }
  # )
  
  
  
  observe({
    
    data <- main_dataserver(
      id = "model1",
      data_list = data_list[[1]]
    )
    
    effect <- main_effectserver(
      id = "model1",
      data = data() # Was isolated
    )
    
    main_warnings(
      'model1'
    )

    main_choiceserver(
      id = 'model1',
      data = data()
    )

    main_plotserver(
      id = "model1",
      data = data(),
      intervention_effect = effect(),
      light_mode = reactive(input$customSwitch1)
    )
    
    



    
  })
  
  
  
  observe({
    
    data2 <- second_dataserver(
      id = "model2",
      data_list = data_list[[2]]
    )
    
    
      
      second_plotserver(
        id   = 'model2',
        data = data2()
      )
      
    

    

  })
  
  
  
  
  
  
  waiter::waiter_hide()
  
  
  
}

