model1UI <- function(id) {
  
  # Generate Namespace
  ns <- NS(id)
  
  
  # Generate Sidebar; ####
  ui_sidebar <- tagList(
    
    sidebarHeader(h5(("Vælg gruppe"))),
    
    vive_picker(
      id = ns("pt_target"),
      title = "Gruppe",
      header = "Vælg Gruppe",
      choices = diseases,
      # selected = sample(
      #   c("Psykiske Lidelser_Svær", "Psykiske Lidelser_Moderat"),
      #   size = 1
      #   ),
      multiple = FALSE
    ),
    
    vive_picker(
      id = ns("pt_control"),
      title = "Sammenligningsgruppe",
      header = "Vælg Sammenligningsgruppe",
      choices = diseases,
      multiple = TRUE,
      #selected = c("Psykiske Lidelser_Mild"),
      max = 1,
      selectAllText = "Matching",
      deselectAllText = "Nulstil"
    ),
    
    sidebarHeader(h5(("Vælg parametre"))),
    
    vive_picker(
      id = ns("pt_outcome"),
      title = "Outcomes",
      header = "Vælg Outcomes",
      choices = outcomes,
      # selected = c(
      #   "Primær Sektor_Almen Praksis",
      #   "Primær Sektor_Psykolog",
      #   "Psykiatrien_Ambulant",
      #   "Somatikken_Ambulant",
      #   "Overførsel_Midlertidig Overførselsindkomst"
      #   ),
      multiple = TRUE,
      selectAllText = "Vælg Alle",
      deselectAllText = "Nulstil"
    ),
    
    
    
    vive_picker(
      id = ns("pt_demographic"),
      title = "Demografi",
      header = "Vælg Demografiske",
      choices = demographics,
      # selected = c(
      #   sample(demographics$alder, 1),
      #   sample(demographics$køn, 1),
      #   sample(demographics$uddannelse, 1)
      #   ),
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
      )
    
    
    
    
    
    
    
    
  )
  
  ui_header <- tagList(
    
    tags$li(
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
      ),
      class = "dropdown"
    )
    
  )
  
  
  
  # Generate Tabsets;
  tabset_names <- data_list %>% names()
  
  
  ui_body <- tagList(
    
    
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
        
        
        
        
      )
      
      
    ),
    
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
                
                
                fluidRow(
                  column(
                    width = 12,
                    bs4Card(
                      width = 12,
                      id = "test",
                      collapsible = TRUE,
                      closable = FALSE,
                      title = "Visuelt Overblik",
                      icon = icon("chart-line", verify_fa = FALSE),
                      footer = fluidRow(
                        materialSwitch(
                          inputId = ns("do_difference"),
                          label = "Vis Forskelle",
                          value = FALSE,
                          status = "primary"
                        )
                        
                        
                        
                      ),
                      status = "primary",
                      
                      
                      column(
                        width = 12,
                        br(),
                        plotlyOutput(
                          outputId = ns(paste0("plot",i))
                        )
                      )
                      
                    ),
                    uiOutput(ns(paste0("infobox",i)))
                    
                    
                    
                    
                  )
                )
                
                
                
                
                
                
                
                
                
                
                
                
              )
              
            }
            
          )
        )
      )
    )
    
  )
  
  
  
  return(
    list(
      sidebar    = ui_sidebar,
      body       = ui_body,
      header     = ui_header
    )
    
  )
  
  
  
  
  
}