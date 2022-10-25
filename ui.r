# script: Userinterface
# objective: Generate a dynamic UI
# date: 2022-08-12
# author: Serkan Korkmaz


bs4DashPage(
  help = TRUE,
  freshTheme = create_theme(
    # bs4dash_vars(
    #   navbar_light_color = '#343a40'
    # ),
    # bs4dash_vars(
    #   navbar_light_bgcolor = '#e90c0c'
    # ),
    # bs4dash_yiq(
    #   contrasted_threshold = 10,
    #   text_dark = "#FFF",
    #   text_light = "#272c30"
    # ),
    bs4dash_layout(
      #main_bg = "#353c42",
      sidebar_width = "200px",
    ),
    bs4dash_status(
      primary = "#5E81AC",
      danger = "#BF616A",
      dark = "#343a40"
    )
  ),
  
  # Loaader; ####
  preloader = list(
    html = tagList(
      spin_1(),"Indlæser ..."
      )
  ),
  
  # Global Options; ###
  fullscreen = TRUE,
  dark = TRUE,
  
  # Title; ####
  # title on Navigation bar
  # on browser
  title = "Beregner til Investeringer i Sundhed",
  
  # header; ####
  header = bs4DashNavbar(
    border = FALSE,
    controlbarIcon = icon('house'),
    fixed = TRUE,
    title = bs4DashBrand(
      color = "primary",
      opacity = 1,
      image = 'heart-pulse-thin.svg',
      title = 'Dashboard'
      # title = column(
      #   width = 12,
      #   offset = 2,
      #   tags$div(
      #     
      #     span(
      #       icon("dashboard", verify_fa = FALSE),
      #       strong("Dashboard")
      #     )
      #   )
      #   
      #   )
      )
  ),
  
  controlbar = bs4DashControlbar(
    disable = FALSE,
    skinSelector()
  ),
  
  
  # sidebar; ####
  sidebar = bs4DashSidebar(
    # Sidebar Start:
    fixed = TRUE,
    minified = TRUE,
    collapsed = TRUE,
    # Sidebar Content Start
    sidebarMenu(
      id = "tab",
      compact = FALSE,
      flat = TRUE,
      childIndent = TRUE,

      menuItem(
        text = "Vælg model",
        startExpanded = TRUE,
        icon = icon("calculator", verify_fa = FALSE,lib = 'font-awesome'),
        menuSubItem(
          text = "Målgruppemodel",
          tabName = "model_1",
          icon = icon('angles-right', verify_fa = FALSE)
        ),
        menuSubItem(
          text = "Forældremodellen",
          tabName = "model_2",
          icon = icon('angles-right', verify_fa = FALSE)
        )
      ),
      
      bs4SidebarMenuItem(
        text = "Data",
        startExpanded = TRUE,
        icon = icon("sitemap", verify_fa = FALSE,lib = 'font-awesome'),
        menuSubItem(
          text = "Model dokumentation",
          tabName = 'documentation',
          icon = icon('book-heart', verify_fa = FALSE)
        ),
        menuSubItem(
          text = "VIVEs dokumentation",
          tabName = "report",
          icon    = icon('books', verify_fa = FALSE)
        )
        
        # ,
        # menuSubItem(
        #   text = "Download Data",
        #   tabName = "export_data",
        #   icon = icon('download', verify_fa = FALSE)
        # )
      )
    )
    
    # Sidebar End
  ),
  
  # body; ####
  body = bs4DashBody(
    
    # Font Awesome pro Kit
    tags$script(
      src ="https://kit.fontawesome.com/226906fa85.js"
    ),
    
    # Body Start;
    includeCSS("www/customtheme.css"),
    useShinyjs(),
    useShinyFeedback(),
    
    
    
    
    
    use_waiter(),
        uiOutput("gen_body")
      
      
    
    
    
    # Body End
    
  ),
  
  # footer; #####
  footer = bs4DashFooter(
    # Footer Start;
    fixed = TRUE,
    left = span(
      "Beregner til Investeringer i Sundhed (BIS)",
      bs4Badge(
        color = "info",rounded = TRUE,fifelse(isTRUE(developper_mode), "Developper Mode", "Live Mode")),
      bs4Badge(
        color = "info",
        rounded = TRUE,
        paste("Version", version)
        )
    ),
    right = span(
      a(
        icon("r-project"),
        "R",
        href = "https://cran.r-project.org/",
        target = "_blank"
      ),
      HTML("&nbsp"),
      a(icon("github"),
        "Github",
        href = "https://github.com/serkor1/bionic-beaver",
        target = "_blank",
        id     = 'github'),
      # add horizontal space between
      # the links.
      HTML("&nbsp"),
      a(
        icon('book-heart', verify_fa = FALSE),
        "Dokumentation",
        href = "documentation/_book/index.html",
        target = "_blank"
      ),
      a(
        img(src = "vive.png",height = "25px", width = "50px"),
        href = "https://www.vive.dk/da/",
        target = "_blank"
      )
    )
    
    # Footer End;
  )
)
