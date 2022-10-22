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
    width       = 30,
    icon        = span("Parametre og Indstillinger", icon("cog")),
    #background  = "#bfc9d1",
    startOpen   = TRUE,
    easyClose   = TRUE,
    id          = "sidebar_id",
    tabsetPanel(
      id = "tabcards",
      type = "pills",
      tabPanel(
        title = "Parametre",
        #icon = icon("cog"),

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
            )

          ),

          sliderInput(
            inputId = ns('effect'),
            label   = 'Sygedage',
            min = 1,
            max = 14,
            value = 1,width = '100%'
          )

        )






      ),
      tabPanel(
        title = "Visuelt",

        .options_plot_model2(
          id = id,
          input = input,
          output = output
        )


      )
    )


  )


  # card; #####
  bs4TabCard(
    title = NULL,
    width = 12,
    height = "500px",
    collapsible = FALSE,
    closable = FALSE,
    headerBorder = FALSE,
    status = "primary",
    sidebar = gen_sidebar,
    tabPanel(
      title = 'Forældremodellen',
      icon = icon('building',verify_fa = FALSE),
      ...
    )

  )


}
#' 
#' 
#' 
#' 
#' 
#' model2UI_body <- function(id, output) {
#'   
#'   # Generate Namespace
#'   ns <- NS(id)
#'   
#'   
#'   tagList(
#'     
#'     # Force Alerts to be at the bottom
#'     div(id = ns("myalert"), style = "position: absolute; bottom: 0; right: 0;"),
#'     .model2UI_choices(
#'       id = id,
#'       output = output
#'     ),
#'     
#'     
#'     # fluidRow(
#'     #   column(
#'     #     width = 12,
#'     #     radioGroupButtons(
#'     #       inputId = paste0("change_view"),width = "100%",
#'     #       label = NULL,
#'     #       choices = c(`Grafik <i class='fa fa-bar-chart'></i>` = "see_plot", ` Tabel <i class='fa fa-line-chart'></i>` = "see_table"),
#'     #       justified = TRUE,
#'     #       size = "sm"
#'     #     )
#'     #   )
#'     #   
#'     #   
#'     #   
#'     # ),
#'     
#'       .box_main(
#'         id,
#'         output,
#'         
#'         
#'         plotlyOutput(
#'           {
#'             
#'             outputId = ns("plot")
#'             
#'           }
#'         )
#'         
#'         
#'         )
#'     
#'     
#'     
#'   )
#'   
#'   
#'   
#' }

model2UI_body <- function(id, output) {
  
  ns <- NS(id)
  
  
  choices <- unlist(chars[[2]])
  
  names(choices) <- str_remove(names(choices),pattern = '.+\\.')
  
  
  tagList(
    modalDialog(
      title = span( icon('family', lib = 'font-awesome', verify_fa = FALSE), 'Forældremodellen'),
      size = 'xl',
      fade = TRUE,
      easyClose = TRUE,
      footer = span(
        
            downloadButton(
              outputId = ns('download_files'),
              label = 'Download Materiale'
            ),
            modalButton(label = 'Luk', icon = icon('close'))
          
          
          
        
        
      ),
      
      # Options
      bs4Card(
        title = 'Parametre',icon = icon('cog', lib = 'font-awesome', verify_fa = FALSE),status = 'primary',solidHeader = TRUE,
        width = 12,
        
        column(
          width = 12,

          fluidRow(

            # vive_picker(
            #   id = ns("pt_target"),
            #   title = "Gruppe",
            #   header = "Vælg Gruppe",
            #   multiple = TRUE,
            #   choices = assignment[[2]]
            # ),
            #
            #
            # vive_picker(
            #   id = ns("pt_outcome"),
            #   title = "Outcomes",
            #   header = "Vælg Outcomes",
            #   choices = outcome[[2]],
            #   multiple = TRUE,
            #   selectAllText = "Vælg Alle",
            #   deselectAllText = "Nulstil"
            # ),
            #
            #
            #
            # vive_picker(
            #   id = ns("pt_demographic"),
            #   title = "Demografi",
            #   header = "Vælg Demografiske",
            #   choices = chars[[2]],
            #   multiple = TRUE,
            #   selectAllText = "Vælg Alle",
            #   deselectAllText = "Nulstil"
            # ),



            checkboxGroupButtons(
              inputId = ns('pt_demographic'),
              label = "Uddannelse",
              choices = choices,
              justified = TRUE,
              size = 'sm',
              width = '100%',
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"))
            ),

            checkboxGroupButtons(
              inputId = ns('pt_who'),
              label = "Hvem tager Sygedagen?",
              choices = c("Delte sygedage" = 'Delt', "Højest uddannede" = 'Højest', "Lavest uddannede" = 'Lavest'),
              justified = TRUE,
              size = 'sm',
              width = '100%',
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"))
            ),
            
            checkboxGroupButtons(
              inputId = ns('pt_group'),
              label = "Aldersgruppen?",
              choices = str_remove(names(unlist(assignment[[2]])),pattern = '.+\\.'),
              justified = TRUE,
              size = 'sm',
              width = '100%',
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"))
            )



          ),

          sliderInput(
            inputId = ns('effect'),
            label   = 'Sygedage',
            min = 1,
            max = 14,
            value = 1,width = '100%'
          )

        )
      ),
      
      accordion(
        id = ns('accordion_model2'),
        
        accordionItem(
          title = span(
            icon(name = 'chart-simple', lib = 'font-awesome', verify_fa = FALSE),
            'Graf'
            ),
          collapsed = FALSE,
          status = 'primary',
          solidHeader = TRUE,
          plotlyOutput(
            ns('plot')
          )
        ),
        accordionItem(
          title = span(
            icon(name = 'table', lib = 'font-awesome', verify_fa = FALSE),
            'Tabel'
          ),
          solidHeader = TRUE,
          collapsed = TRUE,
          status = 'primary',
          uiOutput(
            ns('table')
          ),
          markdown(
            '__Tabel:__ Ugentlig produktionstab pr. `forældre` i kroner.'
          )
          
        )
      )
      
      
      
      
      
    )
  )
  
}