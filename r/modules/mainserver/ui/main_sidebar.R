# This scrip collects all side
# elements of the UI.
# 
# NOTE: This not necessarily
# confinded to the sidebar

model1UI_options <- function(id, output){
  
  # ns <- NS(id)
  # 
  # 
  # tagList(
  #   
  #   sidebarHeader(h5(("Vælg målgruppe"))),
  #   vive_picker(
  #     id = ns("pt_target"),
  #     title = "Gruppe",
  #     header = "Vælg Gruppe",
  #     choices = diseases,
  #     multiple = FALSE
  #   ),
  #   
  #   
  #   sidebarHeader(h5(("Vælg sammenligningsgruppe"))),
  #   radioGroupButtons(
  #     inputId = ns("do_match"),
  #     label = NULL,
  #     selected = "TRUE",
  #     choices = c(
  #       "Befolkningen" = "TRUE", 
  #       "Sygdomsgruppe" = "FALSE"
  #     ),
  #     justified = TRUE
  #   ) %>% popover(
  #     content = "Skal sammenligningen være den generelle befolkning, eller en specifik sygdomsgruppe?",
  #     title = "Skal sammenligningen være den generelle befolkning, eller en specifik sygdomsgruppe?",
  #     placement = "bottom"
  #   ),
  #   
  #   
  #   conditionalPanel(
  #     condition = "input.do_match != 'TRUE'",ns = ns,
  #     {
  #       vive_picker(
  #         id = ns("pt_control"),
  #         title = "Sammenligningsgruppe",
  #         header = "Vælg Sammenligningsgruppe",
  #         choices = diseases,
  #         multiple = FALSE
  #       ) 
  #     }
  #   ),
  #   
  #   
  #   
  #   
  #   
  #   
  #   sidebarHeader(h5(("Vælg parametre"))),
  #   
  #   vive_picker(
  #     id = ns("pt_outcome"),
  #     title = "Outcomes",
  #     header = "Vælg Outcomes",
  #     choices = outcomes,
  #     multiple = TRUE,
  #     selectAllText = "Vælg Alle",
  #     deselectAllText = "Nulstil"
  #   ),
  #   
  #   
  #   
  #   vive_picker(
  #     id = ns("pt_demographic"),
  #     title = "Demografi",
  #     header = "Vælg Demografiske",
  #     choices = demographics,
  #     multiple = TRUE,
  #     selectAllText = "Vælg Alle",
  #     deselectAllText = "Nulstil"
  #   ),
  #   
  #   sidebarHeader(h5("Vælg beregningsgrundlag")),
  #   
  #   materialSwitch(
  #     inputId = ns("do_incident"),
  #     label = "Incidente Patienter?",
  #     value = FALSE,
  #     status = "primary"
  #   ) %>% popover(
  #     title = "Klik for at vælge Incidente patienter.",
  #     placement = "right",
  #     content = "Klik for at vælge Incidente patienter."
  #   ),
  #   
  #   
  #   materialSwitch(
  #     inputId = ns("do_cost"),
  #     label = "Omkostninger?",
  #     value = FALSE,
  #     status = "primary"
  #   ) %>% popover(
  #     title = "Klik for at vælge Omkostning pr. patient.",
  #     placement = "right",
  #     content = "Klik for at vælge Omkostning pr. patient."
  #   )
  #   
  #   
  # )
  
}



tmp_opts <- function(id, output) {
  
  ns <- NS(id)
  
  
  column(
    width = 12,
    
  
    vive_picker(
      id = ns("pt_target"),
      title = "Gruppe",
      header = "Vælg Gruppe",
      choices = diseases,
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
          choices = diseases,
          multiple = FALSE
        ) 
      }
    ),
    
    
    
    
    
    
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