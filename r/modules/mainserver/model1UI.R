# script; UI of
# the main model.

# sidebar; #####
model1UI_sidebar <- function(id, output){
  
  ns <- NS(id)
 
  
  tagList(
    
    sidebarHeader(h5(("Vælg gruppe"))),
    
    vive_picker(
      id = ns("pt_target"),
      title = "Gruppe",
      header = "Vælg Gruppe",
      choices = diseases,
      multiple = FALSE
    ),
    
    vive_picker(
      id = ns("pt_control"),
      title = "Sammenligningsgruppe",
      header = "Vælg Sammenligningsgruppe",
      choices = diseases,
      multiple = FALSE
    ),
    
    sidebarHeader(h5(("Vælg parametre"))),
    
    vive_picker(
      id = ns("pt_outcome"),
      title = "Outcomes",
      header = "Vælg Outcomes",
      choices = outcomes,
      multiple = TRUE,
      selectAllText = "Vælg Alle",
      deselectAllText = "Nulstil"
    ),
    
    
    
    vive_picker(
      id = ns("pt_demographic"),
      title = "Demografi",
      header = "Vælg Demografiske",
      choices = demographics,
      multiple = TRUE,
      selectAllText = "Vælg Alle",
      deselectAllText = "Nulstil"
    ),
    
    sidebarHeader(h5("Vælg beregningsgrundlag")),
    
    materialSwitch(
      inputId = ns("do_incident"),
      label = "Incidente Patienter?",
      value = FALSE,
      status = "primary"
    ) %>% popover(
      title = "Klik for at vælge Incidente patienter.",
      placement = "right",
      content = "Klik for at vælge Incidente patienter."
    ),
    
    
    materialSwitch(
      inputId = ns("do_cost"),
      label = "Omkostninger?",
      value = FALSE,
      status = "primary"
    ) %>% popover(
      title = "Klik for at vælge Omkostning pr. patient.",
      placement = "right",
      content = "Klik for at vælge Omkostning pr. patient."
    )

    
  )
  
}

# header; ####
model1UI_header <- function(id, output){
  
  ns <- NS(id)
  
  tagList(
    
    tags$li(
      (
        dropdownButton(
          label = "Effektforventinger",circle = FALSE,
          tooltip = "Vælg Årlig Effekt",
          map(
            1:5,
            .f = function(i) {
              
              sliderInput(
                inputId = ns(paste0("effect_", i)),
                label = paste("Tid", i),
                min = 0,
                max = 100,
                value = round(runif(1, min = 0, max = 100))
              )
            }
          )
        )
        
      )
      ,
      class = "dropdown"
    )
    
  )
  
}


# body; ####

model1UI_body <- function(id,output) {
  
  ns <- NS(id)
  
  box_placeholder <- function(...) {
    
    bs4Card(
      width = 12,headerBorder = FALSE,
      sidebar = bs4CardSidebar(
        width = 25,icon = span("Indstillinger", icon("cog")),
        startOpen = TRUE,
        id = "id_placeholder",
        p("Grafindstillinger"),
        column(
          width = 4,
          
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
      ),
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
  
  
  
  # Generate Tabsets;
  tabset_names <- data_list %>% names()
  
  
  tagList(
    
    
    div(id = ns("myalert"), style = "position: absolute; bottom: 0; right: 0;"),
    # Infobox
    column(
      width = 12,
      # uiOutput(
      #   outputId = ns(paste0("infobox",i))
      # )
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
        
        
        
        
      ),
      
      
      progressBar(
        id = ns("mean_pbar"),
        value = 0,
        display_pct = TRUE,
        title = "Gennemsnitlig Effekt"
      ) 
      
      
      
      
    ),
    
    
    
    box_placeholder(
      
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
                      
                      
                      
                      # Add Conditional Panel
                      # to view graph or tables
                      
                      
                      plotlyOutput(
                        outputId = ns(paste0("plot",i))
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
                      
                      DT::dataTableOutput(
                        outputId = ns(paste0("table",i))
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
    ,
    
    
    
    
    
    
    
  )
  
  
  
  
  
}

model1UI <- function(id, output) {
  
  # Generate Namespace
  ns <- NS(id)
  
 
  # Generate Sidebar; ####
  ui_sidebar <- model1UI_sidebar(id, output)
  
  # Generate Hader; ####
  ui_header <- model1UI_header(id, output)
  
  ui_body <- model1UI_body(id, output)
    
    
  
  
  
  
  return(
    list(
      sidebar    = ui_sidebar,
      body       = ui_body,
      header     = ui_header
    )
    
  )
  
  
}