# This script collects all UI-elements
# in the body of the application.

# functions; #####
# 
# This script uses functions that are
# unique to this specific UI and are therefore hidden
# and not appart of utils

.box_resultmain <- function(id,output, ...) {
  
  ns <- NS(id)
  
  
  #' function information
  #' 
  #' @param is_table logical. FALSE by default. If TRUE
  #' then the conainer includes a table and therefore
  #' the sidebar  should be different.
  

  tmpsidebar <- bs4CardSidebar(
    width = 25,icon = span("Indstillinger", icon("cog")),
    background = "#bfc9d1",
    startOpen = FALSE,
    id = "id_placeholder",
    
    conditionalPanel(
      condition = "input.change_view == 'see_plot'",
      {
        column(
          width = 12,
          h5("Grafindstillinger"),
          materialSwitch(
            inputId = ns("show_baseline"),
            value = TRUE,
            status = "info",
            label = "Vis Befolkningsværdi"
          ) %>% popover(
            placement = "bottom",
            title = "Skal den generelle befolkning vises?",
            content = "Skal den generelle befolkning vises?"
          ),
          
          pickerInput(
            inputId = ns("col_intervention"),
            label = "Intevention",
            selected = "steelblue",
            choices = colors(),
            options = list(
              `live-search` = TRUE,
              `size` = 5)
          ),
          
          pickerInput(
            inputId = ns("col_control"),
            label = "Control",
            selected = "orange",
            choices = colors(),
            options = list(
              `live-search` = TRUE,
              `size` = 5)
          ),
          
          pickerInput(
            inputId = ns("col_background"),
            label = "Population",
            selected = "white",
            choices = colors(),
            options = list(
              `live-search` = TRUE,
              `size` = 5)
          ),
          
          actionButton(
            inputId = ns("col_reset"),
            label = "Nulstil Farver"
          )
          
          
        )
      }
    ),
    
    conditionalPanel(
      condition = "input.change_view == 'see_table'",
      
      {
        
        column(
          width = 12,
          h5("Tabelindstillinger"),
          materialSwitch(
            inputId = ns("show_basseline"),
            value = TRUE,
            status = "info",
            label = "Vis Noget andet"
          ) %>% popover(
            placement = "bottom",
            title = "Skal den generelle befolkning vises?",
            content = "Skal den generelle befolkning vises?"
          ),
          
          pickerInput(
            inputId = ns("col_intersvention"),
            label = "Intevention",
            selected = "steelblue",
            choices = colors(),
            options = list(
              `live-search` = TRUE,
              `size` = 5)
          ),
          
          pickerInput(
            inputId = ns("col_consrol"),
            label = "Control",
            selected = "orange",
            choices = colors(),
            options = list(
              `live-search` = TRUE,
              `size` = 5)
          ),
          
          pickerInput(
            inputId = ns("col_bacdkground"),
            label = "Population",
            selected = "white",
            choices = colors(),
            options = list(
              `live-search` = TRUE,
              `size` = 5)
          ),
          
          actionButton(
            inputId = ns("col_resset"),
            label = "Nulstil Farver"
          )
          
          
        )
        
      }
    )
    
    

    
    
    
    
  )
  
  
  

  
  # bs4card;
  
  bs4Card(
    width = 12,
    headerBorder = FALSE,
    sidebar = tmpsidebar,
    collapsible = FALSE,
    closable = FALSE,
    # title = "Visuelt Overblik",
    # icon = icon("chart-line", verify_fa = FALSE),
    status = "primary",
    title = span(
      
      radioGroupButtons(
        inputId = paste0("change_view"),width = "500px",
        label = NULL,
        choices = c(`Grafik <i class='fa fa-bar-chart'></i>` = "see_plot", ` Tabel <i class='fa fa-line-chart'></i>` = "see_table"),
        justified = TRUE, size = "sm"
      )
      
      
      
    ),
    
    ...
    
  )
  
}



# information boxes; #####
# 
# This UI element collects all choices
# in the information boxes for the
# user to see what is chosen
model1UI_choices <- function(id, output) {
  
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

model1UI_performance <- function(id, output, id_value) {
  
  
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
            total = 100000,
            display_pct =FALSE,
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
            display_pct = TRUE,
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

model1UI_output <- function(id, output) {
  
  #' function information
  #' 
  #' This function has no @params as
  #' it is hard coded.
  
  
  ns <- NS(id)
  
  
  # Generate Tabsets;
  tabset_names <- data_list %>% names()

  fluidRow(
    column(
      width = 12,
      
      
    
    conditionalPanel(
      condition = "input.change_view == 'see_plot'",
      {
        column(
          width = 12,
          do.call(
            tabsetPanel,
            c(
              type = "pills",
              
              map(
                seq_along(tabset_names),
                .f = function(i) {
                  
                  
                  
                  
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
                    
                    
                    icon = icon(icon_name),
                    value = ns(paste0("tab",i)),
                    br(),
                    
                    
                    # Add Conditional Panel
                    # to view graph or tables
                    
                    
                    plotlyOutput(
                      outputId = ns(paste0("plot",i))
                    ),
                    
                    hr(),
                    
                    model1UI_performance(
                      id_value = paste0("effect",i),
                      id,
                      output
                    )
                    
                    
                    
                    
                    
                    
                  )
                  
                  
                  
                }
                
              )
            )
          )
        )
      }
    ),
    
    conditionalPanel(
      condition = "input.change_view == 'see_table'",
      {
        column(
          width = 12,
          do.call(
            tabsetPanel,
            c(
              type = "pills",
              
              map(
                seq_along(tabset_names),
                .f = function(i) {
                  
                  
                  
                  
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
                    
                    
                    icon = icon(icon_name),
                    value = ns(paste0("tabs",i)),
                    br(),
                    DT::dataTableOutput(
                      outputId = ns(paste0("table",i))
                    ),
                    hr(),
                    
                    model1UI_performance(
                      id_value = paste0("effect",i),
                      id,
                      output
                    )
                    
                    
                  )
                  
                  
                  
                }
                
              )
            )
          )
        )
      }
    )
  )
  )
  
}


# Main body output;
# 
# This function collects all body inputs
# and collects them


model1UI_body <- function(id,output) {
  
  ns <- NS(id)
  
  
  # Generate Tabsets;
  tabset_names <- data_list %>% names()
  
  
  tagList(
    
    
    div(id = ns("myalert"), style = "position: absolute; bottom: 0; right: 0;"),
    # Infobox
    model1UI_choices(id,output),
    
    
    
    .box_resultmain(
      id = id,
      output = output,
      model1UI_output(id,output)
      
      
    )
    
    
    
    
    
    
    
    
  )
  
  
  
  
  
}