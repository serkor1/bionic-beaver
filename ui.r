
bs4DashPage(
  help = TRUE,
  freshTheme = create_theme(
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    bs4dash_layout(
      main_bg = "#353c42",sidebar_width = "350px"
    ),
    bs4dash_sidebar_light(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
    ),
    bs4dash_color(
      gray_900 = "#FFF", white = "#272c30"
    )
  ),
  # Loaader; ####
  preloader = list(
    html = tagList(spin_1(),"Indlæser ...")
  ),
  
  # Global Options; ###
  fullscreen = TRUE,
  dark = TRUE,
  
  # Title; ####
  # title on Navigation bar
  # on browser
  title = "Den Sunhedsøkonomiske Investeringsmodel",
  
  # header; ####
  header = bs4DashNavbar(border = FALSE,
                         fixed = TRUE,
                         titleWidth = "350px",
                         
                         title = bs4DashBrand(
                           color = "primary",
                           opacity = 0,image = NULL,
                           # title = div(
                           #   align = 'center',
                           # span(
                           #   icon("dashboard", verify_fa = FALSE),
                           #   strong("Dashboard")
                           # )
                           # )
                           title = column(
                             width = 6,offset = 4,
                             span(
                               icon("dashboard", verify_fa = FALSE),
                               strong("Dashboard")
                             )
                           )
                         ),
                         rightUi = tags$li(
                           uiOutput("gen_header"),
                           class = "dropdown"
                         )
  ),
  
  
  # sidebar; ####
  sidebar = bs4DashSidebar(
    fixed = TRUE,
    minified = FALSE,
    width = "350px",
    
    
    
    
    sidebarMenu(
      id = "tab",
      
      fluidRow(
        column(
          width = 12,
          descriptionBlock(
            number = NULL,
            numberColor = "primary",
            text = NULL,
            header = actionLink(
              inputId = "front_page",
              label = "Forsiden",
              icon = icon("home",verify_fa = FALSE)
            ),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      ),
      
      
      
      sidebarHeader(h5(("Vælg Model"))),
      
      fluidRow(
        column(
          width = 6,
          descriptionBlock(
            number = NULL,
            numberColor = "primary",
            text = NULL,
            header = actionLink(
              inputId = "model_1",
              label = "Målgruppe",
              icon = icon("project-diagram",
                          verify_fa = FALSE)
            ),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        
        column(
          width = 6,
          descriptionBlock(
            number = NULL,
            numberColor = "primary",
            text = NULL,
            header = actionLink(
              inputId = "model_2",
              label = "Forældre",
              icon = icon("users", verify_fa = FALSE)
            ),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      ),
      
      uiOutput("sidebar_ui")
      
      
      
      
      
      
    )
    
  ),
  
  # body; ####
  body = bs4DashBody(
    # tags$head(
    #   includeCSS("www/theme.css")
    # ),
    includeCSS("www/theme.css"),
    # CSS Code;
    # TODO: Migrate to External file
    # tags$head(tags$style('#test .card-header{ display: none}')),
    # tags$head(
    #   tags$style(HTML("
    #   .shiny-output-error-validation {
    #     color: white;
    #   }
    # "))
    # ),
    
    useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    
    uiOutput("gen_body"),
    uiOutput("performance")
    
  ),
  
  # footer; #####
  footer = bs4DashFooter(
    fixed = TRUE,
    left = span("Den Sundhedsøkonomiske Investeringsmodel",
                
                bs4Badge(
                  color = "info",
                  rounded = TRUE,
                  fifelse(isTRUE(developper_mode), "Developper Mode", "Live Mode")
                ),
                bs4Badge(
                  color = "info",
                  rounded = TRUE,
                  paste("Version", version)
                  
                )
                
    ),
    right = span(
      a(
        icon("r-project"),
        "Backend",
        href = "https://cran.r-project.org/",
        target = "_blank"
      ),
      HTML("&nbsp"),
      a(icon("github"),
        "Repository",
        href = "https://github.com/serkor1/bionic-beaver",
        target = "_blank"),
      # add horizontal space between
      # the links.
      HTML("&nbsp"),
      a(
        icon("book"),
        "Dokumentation",
        href = "documentation/_book/index.html",
        target = "_blank"
      ),
      
      a(
        img(src = "vive.png",height = "20px", width = "50px"),
        href = "https://www.vive.dk/da/",
        target = "_blank"
      ),
      a(
        img(src = "logo-lif-white.svg",height = "20px", width = "50px"),
        href = "https://www.lif.dk/",
        target = "_blank"
      )
    )
  )
)
