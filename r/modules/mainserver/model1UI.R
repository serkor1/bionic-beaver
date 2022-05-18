model1UI <- function(id, output) {
  
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
      multiple = FALSE,
      #selected = c("Psykiske Lidelser_Mild"),
      # max = 1,
      # selectAllText = "Matching",
      # deselectAllText = "Nulstil"
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
  
  ui_header <- tagList(
    
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
  
  
  ui_controlbar <- tagList(
    
      # materialSwitch(
      #   inputId = ns("show_baseline"),
      #   value = TRUE,
      #   status = "info",
      #   label = "Vis Befolkningsværdi"
      # ),
      # 
      # 
      # # column(
      # #   width = 12,
      # 
      #   # fluidRow(
      #   #   pickerInput(
      #   #     inputId = ns("col_intervention"),
      #   #     label = "Intevention",
      #   #     selected = "steelblue",
      #   #     choices = colors(),
      #   #     options = list(
      #   #       `live-search` = TRUE,
      #   #       `size` = 5)
      #   #   ),
      #   #   pickerInput(
      #   #     inputId = ns("col_control"),
      #   #     label = "Control",
      #   #     selected = "orange",
      #   #     choices = colors(),
      #   #     options = list(
      #   #       `live-search` = TRUE,
      #   #       `size` = 5)
      #   #   ),
      #   #   pickerInput(
      #   #     inputId = ns("col_background"),
      #   #     label = "Population",
      #   #     selected = "white",
      #   #     choices = colors(),
      #   #     options = list(
      #   #       `live-search` = TRUE,
      #   #       `size` = 5)
      #   #   )
      # 
      #  # )
      #   # ,
      #   #
      # 
      # 
      #   column(
      #     width = 12,
      #     pickerInput(
      #       inputId = ns("col_intervention"),
      #       label = "Intevention",
      #       selected = "steelblue",
      #       choices = colors(),
      #       options = list(
      #         `live-search` = TRUE,
      #         `size` = 5)
      #     ),
      # 
      #     pickerInput(
      #       inputId = ns("col_control"),
      #       label = "Control",
      #       selected = "orange",
      #       choices = colors(),
      #       options = list(
      #         `live-search` = TRUE,
      #         `size` = 5)
      #     ),
      # 
      #     pickerInput(
      #       inputId = ns("col_background"),
      #       label = "Population",
      #       selected = "white",
      #       choices = colors(),
      #       options = list(
      #         `live-search` = TRUE,
      #         `size` = 5)
      #     ),
      # 
      #     actionButton(
      #       inputId = ns("col_reset"),
      #       label = "Nulstil Farver"
      #     )
      # 
      # 
      #   )
    
  )
  
  
  
  
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
  
  
  
  return(
    list(
      sidebar    = ui_sidebar,
      body       = ui_body,
      header     = ui_header,
      control    = ui_controlbar
    )
    
  )
  
  
  
  
  
}





















#' 
#' 
#' 
#' model1_valuebox <- function(id, data, session = getDefaultReactiveDomain()) {
#'   
#'   #' Function Information;
#'   #' 
#'   #' 
#'   
#'   # Generate Namespace
#'   ns <- NS(id)
#'   
#'   tagList(
#'       bs4Card(
#'         solidHeader = FALSE,
#'         id = "test",
#'         gradient = TRUE,
#'         collapsible = TRUE,
#'         background = "primary",
#'         width = 12,
#'         headerBorder = FALSE,
#'         title = NULL,
#'         fluidRow(
#'           
#'           column(
#'             width = 3,
#'             descriptionBlock(
#'               number = sum(data()[[1]]$outcome),
#'               numberColor = "lime",
#'               numberIcon = icon("caret-up"),
#'               header = HTML("&nbsp"),
#'               text = "Årlig Forventet Effekt",
#'               rightBorder = TRUE,
#'               marginBottom = FALSE
#'             )
#'           )
#'           
#'         )
#'         
#'         
#'       )
#'       
#'       
#'     
#'     
#'     
#'   )
#'     
#' 
#'  
#'   
#'   
#'   
#' }