# script: UI - Body
# objective: Create UI for model 1 (Body)
# date: 2022-07-22
# author: Serkan Korkmaz


.mainbox_model1 <- function(
  input,
  output,
  id
) {
  
  #' function information
  #' 
  #' 
  #' Collect all UI elements for the main
  #' box.
  
  # logic; #####
  
  
  ns <- NS(id)
  
  # Generate Tabset Names
  tabset_names <- data_list[[1]] %>% names()
  
  
  
  
  bs4TabCard(
    width = 12,
    id = 'main_card', 
    status = "primary",
    height = 'auto', # Was 1000px
    collapsible = FALSE,
    sidebar = .sidebar(id, input, output),
    # This has been moved to sidebar
    # title = span(
    #   
    #   radioGroupButtons(
    #     inputId = ns(paste0("change_views")),
    #     width = "500px",
    #     label = NULL,
    #     choices = c(`Grafik <i class='fa fa-bar-chart'></i>` = "see_plot", ` Tabel <i class='fa fa-line-chart'></i>` = "see_table"),
    #     justified = TRUE, size = "sm"
    #   )
    #   
    #   
    #   
    # ),
    .list = map(
      seq_along(tabset_names),
      .f = function(i){
        
        # 1) Generate Tabset title
        tab_title <- extract_name(data_list = data_list[[1]][[i]])
        tab_icon  <- extract_name(data_list = data_list[[1]][[i]], get_icon = TRUE)
          
          
        #   fcase(
        #   tabset_names[i] %chin% c("primary_care"), "house-chimney-medical",
        #   tabset_names[i] %chin% c("psychiatric_care"), "head-side-heart",
        #   tabset_names[i] %chin% c("somatic_care"), "hospital",
        #   tabset_names[i] %chin% c("transfers"), "money-bill-transfer",
        #   default = 'prescription-bottle-medical'
        #   
        # )
        
        
        tabPanel(
          # title = paste(
          #   fcase(
          #     tabset_names[i] %chin% c("primary_care"), "Primær Sundhedssektor",
          #     tabset_names[i] %chin% c("psychiatric_care"), "Psykiatrisk Hospitalskontakt",
          #     tabset_names[i] %chin% c("somatic_care"), "Somatisk Hospitalskontakt",
          #     tabset_names[i] %chin% c("transfers"), "Overførsler",
          #     default = 'Præparatforbrug'
          #     
          #   )
          # ),
          title = tab_title,
          value = ns(paste0("tab",i)),
          icon = icon(tab_icon, verify_fa = FALSE),
          
          
          conditionalPanel(
            condition = "input.change_views == 'see_plot'",ns = ns,
            {
              
              
              column(
                width = 12,
                withSpinner(
                  plotlyOutput(
                    outputId = ns(paste0("plot",i)),
                    height = '800px',
                    width = '100%',
                    inline = FALSE
                  ),
                  type = 7,
                  size = 2,
                  hide.ui = FALSE
                )
              )
              
              
              

            }
          ),
          
          conditionalPanel(
            condition = "input.change_views == 'see_table'",ns = ns,
            {
              
                column(
                  width = 12,
                  withSpinner(
                    div(style = "height: 800px;",
                        uiOutput(
                          ns(paste0("table",i))
                        )
                        )
                      
                    ,
                    type = 7,
                    size = 2,
                    hide.ui = FALSE
                  )
                )
                
              
            }
          )
          
        )
        
      }
    )
    
    
  )
  
  
  
  
  
  
}



# information boxes; #####
# 
# This UI element collects all choices
# in the information boxes for the
# user to see what is chosen
.model1UI_choices <- function(id, output,input) {
  
  ns <- NS(id)
  
  #' function information;
  #' 
  #' There are no @params as all
  #' ids are hardcoded
  
  
  fluidRow(
    column(
      width = 4,
        bs4InfoBox(
          title = strong("Valgt gruppe"),
          subtitle =  textOutput(ns("chosen_target"),inline = TRUE),
          color = "primary",
          width = 12,
          fill = TRUE,
          gradient = TRUE,
          icon = icon("lungs-virus",verify_fa = FALSE)
        ),
        bs4InfoBox(
          title = strong("Valgt sammenligningsgruppe"),
          subtitle =  textOutput(ns("chosen_control"),inline = TRUE),
          color = "primary",
          width = 12,
          fill = TRUE,
          gradient = TRUE,
          icon = icon("lungs-virus",verify_fa = FALSE)
        )
      
    ),
    
    column(
      width = 4,
      bs4InfoBox(
        title = strong("Uddannelse"),
        subtitle =  textOutput(ns("chosen_educ"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("school")
      ),
      bs4InfoBox(
        title = strong("Arbejdsmarkedsstatus"),
        subtitle =  textOutput(ns("chosen_labor"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("car-building", verify_fa = FALSE)
      )
    ),
    
    column(
      width = 4,
      bs4InfoBox(
        title = strong("Køn"),
        subtitle =  textOutput(ns("chosen_gender"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("venus-mars",verify_fa = FALSE)
      ),
      bs4InfoBox(
        title = strong("Alder"),
        subtitle =  textOutput(ns("chosen_age"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("id-card",verify_fa = FALSE)
      )
    )
    
    
    
  )
  
  
  
}


# performance indicator; #####
# 
# This UI element collects the key-performance
# indicators of the final results.

model1UI_performance <- function(id, output, input, id_value) {
  
  
  #' function information;
  #' 
  #' @param id_value character of length 1.
  
  ns <- NS(id)
  
  fluidRow(
    
    # Left side;
    column(
      width = 6,
      bs4ListGroup(
        width = 12,
        type = "heading",
        
        bs4ListGroupItem(
          
          
          progressBar(
            id = ns(paste0("e_", id_value)),
            value = 0,
            total = 100,
            display_pct = TRUE,
            title = NULL
          ),
          title = "Procentvise Effekter",
          subtitle = "Gennemsnitlig forventet effekt",
          disabled = TRUE
        ),
        
        bs4ListGroupItem(
          title = NULL,
          subtitle = "Gennemsnitlig faktisk effekt",
          disabled = TRUE,
          progressBar(
            value = 0,
            total = 100,
            display_pct =TRUE,
            id = ns(paste0("r_", id_value))
          )
        )
        
        
        
        
      )
    ),
    
    # right side;
    
    column(
      width = 6,
      bs4ListGroup(
        width = 12,
        type = "heading",
        
        bs4ListGroupItem(
          
          progressBar(
            id = ns(paste0("re_", id_value)),
            value = 0,
            display_pct = FALSE,
            title = NULL
          ),
          title = "Reale Effekter",
          subtitle = "Gennemsnitlig forventet effekt",
          disabled = TRUE
        ),
        
        bs4ListGroupItem(
          title = NULL,
          subtitle = "Gennemsnitlig faktisk effekt",
          disabled = TRUE,
          progressBar(
            value = 50,
            total = 100,
            display_pct =TRUE,
            id = ns(paste0("rr_", id_value))
          )
        )
        
        
        
        
      )
    )
    
  )
  
}



# Ouput box; #######
# 
# This box is where all the main results
# go.

.model1UI_output <- function(id, output, input) {
  
  #' function information
  #' 
  #' This function has no @params as
  #' it is hard coded.
  
  
  ns <- NS(id)
  
  
  # Generate Tabsets;
  tabset_names <- data_list[[1]] %>% names()
  
  fluidRow(
    column(
      width = 12,
      
      
      .mainbox_model1(
        input = input,
        output = output,
        id = id
        )
    )
  )
  
}


# Main body output;
# 
# This function collects all body inputs
# and collects them


.model1UI_body <- function(id,output,input) {
  
  ns <- NS(id)
  
  
  # Generate Tabsets;
  tabset_names <- data_list[[1]] %>% names()
  
  
  tagList(
    
    
    div(id = ns("myalert"), style = "position: absolute; bottom: 0; right: 0;"),
    # Infobox
    
    
      
      fluidRow(
        style = "height:10%;",
        column(
          width = 12,
          .model1UI_choices(id,output,input)
        ),
        br()

      ),

      fluidRow(
        style = "height:90%; position:relative;",
        column(
          width = 12,
          .model1UI_output(
            id,
            output,
            input
          )
        )

      )
  )
}
    
    
