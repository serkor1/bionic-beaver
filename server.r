server <- function(input, output, session) {
  
  # UI - Rendering; #####
  # 
  # It is based on input$tab
  
  observeEvent(
    input$tab,
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
        
        output$performance <- renderUI(
          {
            model1_ui$performance
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
  

  # Generate Data for Model 1
  
  
  observe({
    if (input$tab == "model_1") {
      
      data <- isolate(
        {
          reactive(
            main_dataserver(id = "model1",data_list = data_list[[1]])
          )
        }
      )
      
      
      
      
      effect <- reactive({
        main_effectserver(
          id = "model1",
          data = data()
        )
      })
      
      
      
      
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
        intervention_effect = effect(),
        light_mode = reactive(input$customSwitch1)
      )
      
      main_tableserver(
        id = "model1",
        data = data(),
        intervention_effect = effect()
      )
      
    }
  }
  
  )
  
  
  
  
  
  
  
  waiter::waiter_hide()
  
  
  
}

