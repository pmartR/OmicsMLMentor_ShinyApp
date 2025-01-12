ui <- div(
  #
  tagList(
    useShinyjs(),
    use_prompt(),
    useShinydashboardPlus(), ## This causes the hovers for picker input, pages
    introjsUI(),
    
    extendShinyjs(script = "./Predict_app/Helpers/shinyui.js", functions = c(
      "isTabdisabled", # For testing purposes
      "isIconhidden", # For testing purposes
      "disableTab", # Disables a tab
      "enableTab", # Enables a tab
      "disableBtn", # Disables a button
      "setFileInput", # Change appearance of fileInput
      "toggleTabInputs" # Toggles state of inputs on a page
    )), # Custom JS code
    
    tags$script(HTML(updateCollapse_script)),
    
    tags$script(HTML(addTooltip_handler_script)),
    
    includeCSS("./Predict_app/Helpers/SLOPER.css"),
    
    list(tags$head(HTML('<link rel="icon", href="slope_icon.png", 
                        type="image/png" />'))),
    
    div(style = "display:none", titlePanel(title = "", windowTitle = "")),
    
    inlineCSS('.navbar-default .navbar-brand {padding: 2px;}'),
    
    # start navbarPage
    navbarPage(
      # need to update this to be slope_icon.png
      title = tags$span(tags$img(src = "slope_icon.png", style = "max-height:100%"), ""),
      id = "top_page",
      
      # tabPanel("Welcome",welcome_tab()),
      tabPanel("Upload",upload_tab()),
      tabPanel("Pre-processing", preprocess_UI()),
      tabPanel("Run Model", RM_tab()),
      tabPanel("Downloads", downloads_tab_overlord())

    ),
    # end Navbarpage
    
    div(style="position:fixed;z-index:9999;bottom:10px;right:10px;",
        actionButton("Browser","whats wrong")
    ),
    
    shinybusy::add_busy_spinner(position = "bottom-left")
    
  )
) # shinyUI + shinyjs tagList
