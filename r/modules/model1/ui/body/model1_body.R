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
    status = "primary",
    collapsible = FALSE,height = '825px',
    sidebar = .sidebar(id, input, output),
    title = span(
      
      radioGroupButtons(
        inputId = ns(paste0("change_views")),
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
            condition = "input.change_views == 'see_plot'",ns = ns,
            {
              h3("Plot")
              
              column(
                width = 12,
                fluidRow(
                  h3("Plot"),
                  
                  hr()
                )
              )
              
              
              fluidRow(
                column(
                  width = 12,
                  shinycssloaders::withSpinner(
                     plotlyOutput(
                      outputId = ns(paste0("plot",i)),height = '800px'
                    ),
                    type = 7,
                    size = 2,hide.ui = FALSE
                  )
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
            condition = "input.change_views == 'see_table'",ns = ns,
            {
              h3("Tablez")
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
        title = strong("Valgt Gruppe"),
        subtitle =  textOutput(ns("chosen_target"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("lungs-virus")
      ),
      bs4InfoBox(
        title = strong("Valgt Sammenligningsgruppe"),
        subtitle =  textOutput(ns("chosen_control"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("lungs-virus")
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
        icon = icon("user-graduate")
      ),
      bs4InfoBox(
        title = strong("Arbejdsmarkedsstatus"),
        subtitle =  textOutput(ns("chosen_labor"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("briefcase", verify_fa = TRUE)
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
        icon = icon("venus-mars")
      ),
      bs4InfoBox(
        title = strong("Alder"),
        subtitle =  textOutput(ns("chosen_age"),inline = TRUE),
        color = "primary",
        width = 12,
        fill = TRUE,
        gradient = TRUE,
        icon = icon("id-card")
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
    
    .model1UI_choices(id,output,input),
    
    
    
    # .box_resultmain(
    #   id = id,
    #   output = output
    # 
    # 
    # 
    # ),
    
    .model1UI_output(
      id,
      output,
      input
      )
    
    
    
    
    
    
    
    
  )
  
  
  
  
  
}