# script: Userinterface
# objective: Generate a dynamic UI
# date: 2022-08-12
# author: Serkan Korkmaz

yellow <- '#f7dc6f'

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
      main_bg = '#ffffff', #"#353c42",
      sidebar_width = "200px"
    ),
    bs4dash_sidebar_light(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#73808c",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(
      primary = "#5E81AC",
      danger = "#BF616A",
      dark = "#343a40",
      light = "#73808c",
    ),
    bs4dash_color(
      gray_900 = "#FFF",
      white = "#272c30"
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
  title = "Den Sundhedsøkonomiske Investeringsmodel",
  
  # header; ####
  header = bs4DashNavbar(
    border = FALSE,
    fixed = TRUE,
    #titleWidth = "200p",
    title = bs4DashBrand(
      color = "primary",
      opacity = 0,
      image = NULL,
      title = column(
        width = 12,
        offset = 2,
        tags$div(
          
          span(
            icon("dashboard", verify_fa = FALSE),
            strong("Dashboard")
          )
        )
        
        )
      )
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
        text = "Forsiden",
        icon = icon('home', verify_fa = FALSE,lib = 'font-awesome'),
        startExpanded = TRUE,
        tabName = "front_page",
        selected = TRUE
      ),
      menuItem(
        text = "Vælg Model",
        startExpanded = TRUE,
        icon = icon("calculator", verify_fa = FALSE,lib = 'font-awesome'),
        menuSubItem(
          text = "Målgruppemodel",
          tabName = "model_1"
        ),
        menuSubItem(
          text = "Forældremodellen",
          tabName = "model_2"
        )
      ),
      
      bs4SidebarMenuItem(
        text = "Data",
        startExpanded = TRUE,
        icon = icon("sitemap", verify_fa = FALSE,lib = 'font-awesome'),
        menuSubItem(
          text = "Model Dokumentation",
          tabName = 'documentation'
        ),
        menuSubItem(
          text = "VIVEs Dokumentation",
          tabName = "report"
        ),
        menuSubItem(
          text = "Download Data",
          tabName = "export_data"
        )
      )
    )
    
    # Sidebar End
  ),
  
  # body; ####
  body = bs4DashBody(
    # Body Start;
    includeCSS("www/customtheme.css"),
    useShinyjs(),
    useShinyFeedback(),
    use_waiter(),
    uiOutput("gen_body"),
    uiOutput("performance")
    
    # Body End
    
  ),
  
  # footer; #####
  footer = bs4DashFooter(
    # Footer Start;
    fixed = TRUE,
    left = span(
      "Den Sundhedsøkonomiske Investeringsmodel",
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
        img(src = "vive.png",height = "25px", width = "50px"),
        href = "https://www.vive.dk/da/",
        target = "_blank"
      )
    )
    
    # Footer End;
  )
)
