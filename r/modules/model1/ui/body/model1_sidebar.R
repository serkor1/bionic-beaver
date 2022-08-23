# script: main_sidebar
# objective: Generate Sidebar for boxes
# date: 2022-07-22
# author: Serkan Korkmaz

# parameters; #####
.param_model1 <- function(
    id,
    input,
    output
) {
  
  #' function information
  #' 
  #' Parameters for Model 1.
  
  # function logic; #####
  ns <- NS(id)
  
  
  column(
    width = 12,
    
    
    vive_picker(
      id = ns("pt_target"),
      title = "Gruppe",
      header = "Vælg Gruppe",
      choices = assignment[[1]],
      multiple = FALSE
    ),
    
    
    radioGroupButtons(
      inputId = ns("do_match"),
      label = NULL,
      selected = "TRUE",
      choices = c(
        "Befolkningen" = "TRUE", 
        "Sygdomsgruppe" = "FALSE"
      ),
      justified = TRUE
    ) %>% popover(
      content = "Skal sammenligningen være den generelle befolkning, eller en specifik sygdomsgruppe?",
      title = "Skal sammenligningen være den generelle befolkning, eller en specifik sygdomsgruppe?",
      placement = "bottom"
    ),
    
    
    conditionalPanel(
      condition = "input.do_match != 'TRUE'",ns = ns,
      {
        vive_picker(
          id = ns("pt_control"),
          title = "Sammenligningsgruppe",
          header = "Vælg Sammenligningsgruppe",
          choices = assignment[[1]],
          multiple = FALSE
        ) 
      }
    ),
    
    
    
    
    
    
    vive_picker(
      id = ns("pt_outcome"),
      title = "Outcomes",
      header = "Vælg Outcomes",
      choices = outcome[[1]],
      multiple = TRUE,
      selectAllText = "Vælg Alle",
      deselectAllText = "Nulstil"
    ),
    
    
    
    vive_picker(
      id = ns("pt_demographic"),
      title = "Demografi",
      header = "Vælg Demografiske",
      choices = chars[[1]],
      multiple = TRUE,
      selectAllText = "Vælg Alle",
      deselectAllText = "Nulstil"
    ),
    
    
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


# effects; #####

.effect_model1 <- function(
    id,
    input,
    output
) {
  
  #' function information; 
  #' 
  #' 
  #' Generate effect sliders
  #' for the sidebar
  
  ns <- NS(id)
  
  column(
    width = 12,
    map(
      1:5,
      .f = function(i) {
        
        sliderInput(
          inputId = ns(paste0("effect_", i)),
          label = paste("Tid", i),
          min = 0,
          max = 100,
          value = 0
        )
      }
    )
  )
}


.options_plot_model1 <- function(
    id,
    input,
    output
) {
  
  ns <- NS(id)
  
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
    
    # pickerInput(
    #   inputId = ns("col_intervention"),
    #   label = "Intevention",
    #   selected = "steelblue",
    #   choices = colors(),
    #   options = list(
    #     `live-search` = TRUE,
    #     `size` = 5)
    # ),
    # 
    # pickerInput(
    #   inputId = ns("col_control"),
    #   label = "Control",
    #   selected = "orange",
    #   choices = colors(),
    #   options = list(
    #     `live-search` = TRUE,
    #     `size` = 5)
    # ),
    
    colorPickr(
      inputId = ns("col_intervention"),
      preview = TRUE,
      label = 'Intevention',
      selected = '#4682B4',
      update = 'changestop'
    ),
    
    colorPickr(
      inputId = ns("col_control"),
      preview = TRUE,
      label = 'Control',
      selected = '#FFA500',
      update = 'changestop'
    ),
    
    colorPickr(
      inputId = ns("col_background"),
      preview = TRUE,
      label = 'Baggrundsfarve',
      selected = '#FFFFFF',
      update = 'changestop'
    ),
   
    
    actionButton(
      inputId = ns("col_reset"),
      label = "Nulstil Farver"
    )
    
    
  )
  
  
}


.options_table_model1 <- function(
    id,
    input,
    output
) {
  
  ns <- NS(id)
  
  column(
    width = 12,
    h5("Tabelindstillinger"),
    materialSwitch(
      inputId = ns("show_baseline"),
      value = TRUE,
      status = "info",
      label = "Vis Noget andet"
    ) %>% popover(
      placement = "bottom",
      title = "Skal den generelle befolkning vises?",
      content = "Skal den generelle befolkning vises?"
    ),
    
    pickerInput(
      inputId = ns("m1col_intervention"),
      label = "Intevention",
      selected = "steelblue",
      choices = colors(),
      options = list(
        `live-search` = TRUE,
        `size` = 5)
    ),
    
    pickerInput(
      inputId = ns("m1col_control"),
      label = "Control",
      selected = "orange",
      choices = colors(),
      options = list(
        `live-search` = TRUE,
        `size` = 5)
    ),
    
    pickerInput(
      inputId = ns("m1col_background"),
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

# box-sidebar; ######

.sidebar <- function(
    id,
    input,
    output
) {
  
  #' function information
  #' 
  #' Create a sidebar for model 1 with
  #' user input. 
  #' 
  #' Uses @tmp_opts from another script
  #' TODO: Find the source of this script.
  
  ns <- NS(id)
  
  # tabsetpanel; #####
  
  gen_panel <- tabsetPanel(
    id = 'tabcard',
    type = 'pills',
    
    # first tab:
    tabPanel(
      title = 'Parametre',
      
      .param_model1(
        input = input,
        output = output,
        id = id
      )
    ),
    
    
    
    # second tab:
    tabPanel(
      title = 'Effektforventninger',
      .effect_model1(
        input = input,
        output = output,
        id = id
      )
    ),
    
    
    # third tab:
    tabPanel(
      title = "Visuelle Indstillinger",
      conditionalPanel(
        condition = "input.change_views == 'see_plot'",ns = ns,
        {
          
          .options_plot_model1(
            input = input,
            output = output,
            id = id
          )
          
        }
      ),
      
      conditionalPanel(
        condition = "input.change_views == 'see_table'",ns = ns,
        
        {
          
          .options_table_model1(
            input = input,
            output = output,
            id = id
          )
          
          
        }
      )
      
      
    )
    
  )
  
  
  # generate sidebar
  
  bs4CardSidebar(
    width = 30,
    icon = span("Parametre og Indstillinger", icon("cog")),
    startOpen = TRUE,
    id = "id_placeholder",
    gen_panel
  )
  
  
  
}