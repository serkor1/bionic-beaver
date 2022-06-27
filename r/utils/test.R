
temp_box <- function(input, output, id) {
  
  ns <- NS(id)
  
  # Generate Tabset Names
  tabset_names <- data_list[[1]] %>% names()
  
  
  
  
  bs4TabCard(
    width = 12,status = "primary",
    collapsible = FALSE,
    sidebar = tmpsidebar(id, input,output),
    title = span(
      
      radioGroupButtons(
        inputId = paste0("change_views"),
        width = "500px",
        label = NULL,
        choices = c(`Grafik <i class='fa fa-bar-chart'></i>` = "see_plot", ` Tabel <i class='fa fa-line-chart'></i>` = "see_table"),
        justified = TRUE, size = "sm"
      )
      
      
      
    ),
    .list = map(
      seq_along(tabset_names),
      .f = function(i){
        
        icon_name <- fcase(
          tabset_names[i] %chin% c("primary_care"), "stethoscope",
          tabset_names[i] %chin% c("psychiatric_care"), "couch",
          tabset_names[i] %chin% c("somatic_care"), "hospital",
          tabset_names[i] %chin% c("transfers"), "briefcase"
          
        )
        
        
        tabPanel(
          title = paste(
            fcase(
              tabset_names[i] %chin% c("primary_care"), "Almen praksis",
              tabset_names[i] %chin% c("psychiatric_care"), "Psykiatri",
              tabset_names[i] %chin% c("somatic_care"), "Somatik",
              tabset_names[i] %chin% c("transfers"), "Arbejdsmarkedet"
              
            )
          ),
          value = ns(paste0("tab",i)),
          icon = icon(icon_name),
          
          
          conditionalPanel(
            condition = "input.change_views == 'see_plot'",
            {
              
              column(
                width = 12,
                fluidRow(
                  h3("Plot"),
                  
                  hr()
                )
              )
              
              
              fluidRow(
                plotlyOutput(
                  outputId = ns(paste0("plot",i))
                )
              )

              


              # model1UI_performance(
              #   id_value = ns(paste0("effect",i)),
              #   id,
              #   output
              # )
            }
          ),
          
          conditionalPanel(
            condition = "input.change_views == 'see_table'",
            {
              h3("Table")
            }
          )
          
        )
        
      }
    )
    
    
  )
  
  
  
}
