ui <- function(request) {
  #
  tagList(
    useShinyjs(),
    use_prompt(),
    useShinydashboardPlus(),
    introjsUI(),
    
    extendShinyjs(script = "./Helpers/shinyui.js", functions = c(
      "isTabdisabled", # For testing purposes
      "isIconhidden", # For testing purposes
      "disableTab", # Disables a tab
      "enableTab", # Enables a tab
      "disableBtn", # Disables a button
      "setFileInput", # Change appearance of fileInput
      "toggleTabInputs" # Toggles state of inputs on a page
    )), # Custom JS code
    
    tags$head(
      # tags$link(rel = "stylesheet", type = "text/css", href = "./Helpers/SLOPER.css"),
      
      # Hack to replicate the addTooltip functionality with prompter
      tags$script(HTML(addTooltip_handler_script)),
      
      ## Css
      includeCSS("./Helpers/SLOPER.css")
    ),
    
    list(tags$head(HTML('<link rel="icon", href="slope_icon.png", 
                        type="image/png" />'))),
    
    div(style = "display:none", titlePanel(title = "", windowTitle = "")),
    
    # Loading mask, can hide/show/edit html when wanting to show "loading" but a
    # progress indicator doesn't make sense (like at startup, where a bunch of
    # disjoint operations have to run)
    div(
      id = "loading-gray-overlay",
      class = "loading-mask",
      div(class = "fadein-out busy relative-centered",
          style = "font-size:xx-large",
          "Loading app resources...")
    ), 
    
    inlineCSS('.navbar-default .navbar-brand {padding: 2px;}'),
    #
    navbarPage(
      title = tags$span(tags$img(src = "slope_icon.png", style = "max-height:100%"), ""),
      id = "top_page",
      ##### LANDING TAB ######
      tabPanel("Welcome", welcome_tab()),
      tabPanel("Upload", upload_tab_overlord()),
      tabPanel("Quality Control", QC_tab_overlord()),
      tabPanel("Model Set-Up", Model_setup_tab_overlord()),
      tabPanel("Pre-processing", preprocessing_tab_overlord()),
      
      # #### EDA Tab #####


      # #### EDA Tab #####
      # EDA_tab(),

      #### Stats Integration Tab ####
      # integration_tab(),

      ### Run model ###
      tabPanel("Run Model", RM_tab_overlord()),
      
      # #### Download Tab #####
      download_tab(),

      tabPanel(uiOutput("notify_which_data"), value = "nav_which_datatype")
      # tabPanel(uiOutput('plots_summary'), value = 'nav_plot_review'),
      # tabPanel(uiOutput('current_data_summary'), value = 'nav_data_review')
    ),
    # end Navbarpage

    hidden(actionButton("omicsData_reset", 
                        "Reset to before this tab", 
                        class = "btn-warning", 
                        style = "position:fixed;left:15px;bottom:15px")),
    
    hidden(actionButton("model_reccomendations", 
                 "Model Requirements",
                 width = "170px",
                 style = "position:absolute;top:8px;right:325px;z-index:1100;")),
    
    actionButton("glossary_button", 
                 "Glossary",
                 width = "80px",
                 style = "position:absolute;top:8px;right:235px;z-index:1100;"),
    
    actionButton("help_button", 
                 "Help",
                 width = "55px",
                 style = "position:absolute;top:8px;right:170px;z-index:1100;"),
    
    actionButton("contact",
                 "Contact Maintainer",
                 width = "150px",
                 style = "position:absolute;top:8px;right:10px;z-index:1100;"),
    

    
    # uiOutput("developer_buttons"),
    
    ## ADDED FOR MAP
    div(
      style = "position:absolute;top:3px;right:16px;z-index:1100;",
      div(
        uiOutput("MAP_button")
      )
    ),
    
    
    uiOutput("developer_buttons")
    
    
  )
} # shinyUI + shinyjs tagList
