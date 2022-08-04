# script: secondary_body
# objective: Generate the UI body of the secondary model
# author: Serkan Korkmaz
# date: 2022-06-12



.box_main <- function(id, output,...) {
  
  # Generate Namespace
  ns <- NS(id)
  
  #' function information
  #' 
  #' @param ... these are the additional 
  #' content of the main box of the model
  
  # genereate sidebar; #####
  gen_sidebar <- bs4CardSidebar(
    width       = 25,
    icon        = span("Indstillinger", icon("cog")),
    background  = "#bfc9d1",
    startOpen   = TRUE,
    easyClose   = TRUE,
    id          = "sidebar_id",
    tabsetPanel(
      id = "tabcards",
      type = "pills",
      tabPanel(
        title = "Parametre",icon = icon("cog"),
        
        column(
          width = 12,
          
          fluidRow(
            
            vive_picker(
              id = ns("pt_target"),
              title = "Gruppe",
              header = "Vælg Gruppe",
              choices = assignment[[2]]
            ),
            
            vive_picker(
              id = ns("pt_control"),
              title = "Sammenligningsgruppe",
              header = "Vælg Sammenligningsgruppe",
              choices = assignment[[2]],
              multiple = FALSE
            ),
            
            vive_picker(
              id = ns("pt_outcome"),
              title = "Outcomes",
              header = "Vælg Outcomes",
              choices = outcome[[2]],
              multiple = TRUE,
              selectAllText = "Vælg Alle",
              deselectAllText = "Nulstil"
            ),
            
            
            
            vive_picker(
              id = ns("pt_demographic"),
              title = "Demografi",
              header = "Vælg Demografiske",
              choices = chars[[2]],
              multiple = TRUE,
              selectAllText = "Vælg Alle",
              deselectAllText = "Nulstil"
            ),
            
          )
          
        )
        
        
        
        
        
        
      ),
      tabPanel(
        title = "Tab2"
      ),
      tabPanel(
        title = "Tab3"
      )
    )
    
    
  )
  
  
  # card; #####
  bs4Card(
    title = "Temporary Title",
    width = 12,
    height = "500px",
    collapsible = FALSE,
    closable = FALSE,
    headerBorder = FALSE,
    status = "primary",
    sidebar = gen_sidebar,
    ...
  )
  
  
}





model2UI_body <- function(id, output) {
  
  # Generate Namespace
  ns <- NS(id)
  
  
  tagList(
    
    # Force Alerts to be at the bottom
    div(id = ns("myalert"), style = "position: absolute; bottom: 0; right: 0;"),
    
    fluidRow(
      column(
        width = 12,
        radioGroupButtons(
          inputId = paste0("change_view"),width = "100%",
          label = NULL,
          choices = c(`Grafik <i class='fa fa-bar-chart'></i>` = "see_plot", ` Tabel <i class='fa fa-line-chart'></i>` = "see_table"),
          justified = TRUE,
          size = "sm"
        )
      )
      
      
      
    ),
    fluidRow(
      .box_main(
        id,
        output,
        
        
        plotlyOutput(
          {
            
            outputId = ns("plot")
            
          }
        )
        
        
        )
    )
    
    
  )
  
  
  
}