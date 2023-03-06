ui <- function(request) {
  #
  tagList(
    useShinyjs(),
    use_prompt(),
    useShinydashboardPlus(),
    
    extendShinyjs(script = "../Helper_functions/shinyui.js", functions = c(
      "isTabdisabled", # For testing purposes
      "isIconhidden", # For testing purposes
      "disableTab", # Disables a tab
      "enableTab", # Enables a tab
      "disableBtn", # Disables a button
      "setFileInput", # Change appearance of fileInput
      "toggleTabInputs" # Toggles state of inputs on a page
    )), # Custom JS code
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "../Helpers/SLOPER.css"),
      
      # Hack to replicate the addTooltip functionality with prompter
      tags$script(HTML(addTooltip_handler_script))
    ),
    
    list(tags$head(HTML('<link rel="icon", href="pmartlogo.png", type="image/png" />'))),
    div(style = "display:none", titlePanel(title = "", windowTitle = "SLOPE")),
    
    # Loading mask, can hide/show/edit html when wanting to show "loading" but a
    # progress indicator doesn't make sense (like at startup, where a bunch of
    # disjoint operations have to run)
    div(
      id = "loading-gray-overlay",
      class = "loading-mask",
      div(class = "fadein-out busy relative-centered", style = "font-size:xx-large", "Loading app resources...")
    ), 
    #
    navbarPage(
      title = tags$span(tags$img(src = "pmartlogo.png", style = "max-height:100%"), "SLOPE"),
      id = "top_page",
      ##### LANDING TAB ######
      navbarMenu(
        "Welcome",
        tabPanel(
          "Instructions",
          includeMarkdown("Static_UI/datareqs.md")
        ),
        tabPanel("Example Data"),
        tabPanel("Glossary")
      ),

      ##### GOALS TAB ######
      # goals_tab(),

      ##### UPLOAD TAB ######
      upload_tab(),

      ##### REFERENCE TAB ######
      # reference_tab(),

      ###### Groups TAB #######
      groups_tab(),
      
      ###### Variable Selection #######
      tabPanel("Variable Specifications"),
      
      ###### Variable Selection #######
      tabPanel("Model Selection"),
      
      navbarMenu("Pre-processing",
                 tabPanel("Scaling and Transformation"),
                 
                 #### Filter Tab ####
                 filter_tab(),
                 
                 ###### Normalization TAB #######
                 norm_tab(),
                 
                 # #### Statistics Tab #####
                 # pepstats_tab(),
                 
                 ###### Roll-up Tab ######
                 rollup_tab("")
                 ),
      
      # #### EDA Tab #####


      # #### EDA Tab #####
      # EDA_tab(),

      # #### Statistics Tab #####
      # stats_tab(),

      #### Stats Integration Tab ####
      # integration_tab(),

      ### Run model ###
      tabPanel("Run Model"),
      
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
    
    actionButton("model_reccomendations", 
                        "Model Suggestions",
                        style = "position:absolute;top:3px;right:16px;z-index:1100;"),
    
    uiOutput("developer_buttons"),
    
    ## ADDED FOR MAP
    div(
      style = "position:absolute;top:3px;right:16px;z-index:1100;",
      div(
        uiOutput("MAP_button")
      )
    )
    
    
  )
} # shinyUI + shinyjs tagList
